library(tidyverse)
library(qualtRics)
library(here)
library(readxl)

qualtrics_api_credentials(api_key = "RLPk714vkWXUHfT5uv5fm0hSyk0a3PBO5t8ouoAR", 
                          base_url = "oregon.qualtrics.com",
                          install = TRUE,
                          overwrite = TRUE)

readRenviron("~/.Renviron")

surveys <- all_surveys() 

qual_raw_g3 <- fetch_survey(surveyID = pull(filter(surveys, name == "Grade3_Focus-Group-Survey"), id), 
                            force_request = TRUE)


names(qual_raw_g3)


rename_fx <- function(x){
  df <- x %>% 
    slice(1:2)
  as_tibble(cbind(qual = names(df), t(df))) %>% 
    select(qual, new = V2, original = V3)
}

lookup <- tibble(
  sheet = paste("Figure", c(1:4)),
  data = map(sheet, ~read_xlsx(here("data", "Grade3_Focus-Group-Survey_January 19 2021_15.26.xlsx"),
                              sheet = .)),
  dta = map(data, ~rename_fx(.))
) %>% 
  select(dta) %>% 
  unnest(cols = c(dta)) %>% 
  mutate(new = str_to_lower(new))

qual_raw_g3 %>% 
  select_if(names(qual_raw_g3) %in% lookup$qual) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "qual",
    values_to = "values"
  ) %>%
  left_join(lookup) %>% 
  select(new, values) %>% 
  pivot_wider(
    names_from = new,
    values_from = values
  ) %>% 
  unnest(cols = everything())


