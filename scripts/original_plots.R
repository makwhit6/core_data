library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(here)

data_raw <- read_csv(here::here("nopublish", "year4_consq_valid_dta.csv"))

data <- data_raw %>% 
  select(student_id, grade, 
         starts_with("sbacela"),
         starts_with("session"),
         starts_with("date_core"),
         starts_with("wcpm_trad"), 
         starts_with("wcpm_core_scale_wave"),
         starts_with("wcpm_core_scale_se_wave")) %>% 
  drop_na(wcpm_traditional_wave1:wcpm_traditional_wave4, 
          wcpm_core_scale_wave1:wcpm_core_scale_wave4) %>% 
  rename_all(~str_replace(., "core_scale", "corescale")) %>%
  rename_all(~str_replace(., "wcpm_corescale_se", "se-corescale")) %>% 
  rename_all(~str_replace(., "wcpm_", "wcpm-")) %>%
  rename_all(~str_replace(., "date_core", "date")) %>% 
  mutate_at(vars(starts_with("date")), as.character) %>%
  distinct()

data_long <- data %>%
  pivot_longer(
    cols = date_wave1:'se-corescale_wave4',
    names_to = c("test", "wave"),
    values_to = "value",
    names_sep = "_",
    values_transform = list(value = as.character)
  ) %>% 
  pivot_wider(
    names_from = test,
    values_from = value
  ) %>% 
  mutate(
    `se-traditional` = case_when(
      grade == 2 ~ 8,
      grade == 3 ~ 9.73,
      grade == 4 ~ 10),
    month = fct_relevel(factor(lubridate::month(date)), "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")
  ) %>% 
  pivot_longer(
    cols = `wcpm-traditional`:`se-traditional`,
    names_to = "test",
    values_to = "score",
    values_transform = list(score = as.numeric)
  )  %>% 
  separate(test, c("foo", "test"), sep = "-") %>% 
  pivot_wider(
    names_from = foo,
    values_from = score
  )  %>% 
  mutate(test_id = recode(test,
                          traditional = "Traditional ORF", #easyCBM / yellow
                          corescale = "CORE")) %>% #CORE / black
  mutate(date = lubridate::ymd(date))


# Select randomly sampled students
samps_pre_grade <- 4

#set.seed(210)
set.seed(8)
id_samples <- data_raw %>% 
  drop_na(wcpm_traditional_wave1:wcpm_traditional_wave4, 
          wcpm_core_scale_wave1:wcpm_core_scale_wave4) %>% 
  select(student_id, read_prf_easycbm_fall, grade) %>% 
  group_by(grade) %>% 
  mutate(cut_25 = unname(quantile(read_prf_easycbm_fall, probs = .25, na.rm = TRUE)),
         cut_10 = unname(quantile(read_prf_easycbm_fall, probs = .10, na.rm = TRUE))) %>% 
  ungroup() %>% 
  filter(read_prf_easycbm_fall <= cut_25,
         read_prf_easycbm_fall >= cut_10) %>% 
  select(grade, student_id) %>% 
  nest(data = -grade) %>% 
  mutate(n = samps_pre_grade) %>% 
  mutate(samp_ids = map2(data, n, sample_n)) %>% 
  select(samp_ids, grade) %>% 
  unnest(cols = c(samp_ids)) 

data_samples <- data_long %>% 
  semi_join(id_samples) 

## -- summarize perf levels, and wight for lower readers
data_samples %>% 
  distinct(student_id, .keep_all = TRUE) %>% 
  group_by(grade) %>% 
  count(sbacela_performance) %>% 
  mutate(p = n/sum(n))

plots <- data_samples %>% 
  rename(ELA = sbacela_performance) %>% 
  nest(data = -student_id) %>% 
  mutate(
    grade = map_dbl(data,
                    ~unique(.$grade)),
    date_breaks = map(data,
                      ~select(., date) %>% unique() %>% pull()),
    plot1 = map2(data, date_breaks,
                 ~ggplot(data = .x, aes(date, wcpm, group = test, color = test_id)) +
                   geom_point(size = 3) +
                   geom_line() +
                   geom_text_repel(aes(label = round(wcpm, 0)), size = 5, show.legend = FALSE) +
                   theme_minimal() +
                   theme(legend.title = element_blank(),
                         legend.position = "bottom",
                         axis.text.x = element_text(angle = 45, vjust = 0.5)) +
                   scale_color_colorblind() +
                   scale_x_date(breaks = .y, date_labels = "%b %d") +
                   facet_wrap(~grade + ELA, 
                              labeller = labeller(grade=label_both, ELA=label_both))
    ),
    ploterror = map2(data, date_breaks,
                     ~ggplot(data = .x, aes(date, wcpm, group = test, color = test_id)) +
                       geom_ribbon(aes(ymin = wcpm - 1*se, ymax = wcpm + 1*se), color = NA, alpha = .3) +
                       geom_point(size = 3) +
                       geom_line() +
                       geom_text_repel(aes(label = round(wcpm, 0)), size = 5, show.legend = FALSE) +
                       theme_minimal() +
                       theme(legend.title = element_blank(),
                             legend.position = "bottom",
                             axis.text.x = element_text(angle = 75, vjust = 0.5)) +
                       scale_color_colorblind() +
                       scale_x_date(breaks = .y, date_labels = "%b %d") +
                       facet_wrap(~test_id + grade + ELA, 
                                  labeller = labeller(grade=label_both, ELA=label_both))),
    
  )

map2(paste0("plot1_gr", plots$grade, "_", plots$student_id, ".jpg"), plots$plot1, 
     ~ggsave(.x, .y, path = here::here("plots", "original")))

map2(paste0("ploterror_gr", plots$grade, "_", plots$student_id, ".jpg"), plots$ploterror, 
     ~ggsave(.x, .y, path = here::here("plots", "original")))


data_samples %>%
  select(student_id, grade) %>% 
  distinct(.keep_all = TRUE) %>% 
  left_join(select(data_raw, student_id, read_prf_easycbm_fall)) %>% 
  arrange(grade, student_id)


############################################












