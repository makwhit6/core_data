library(tidyverse)
library(qualtRics)

qualtrics_api_credentials(api_key = "RLPk714vkWXUHfT5uv5fm0hSyk0a3PBO5t8ouoAR", 
                          base_url = "oregon.qualtrics.com",
                          install = TRUE,
                          overwrite = TRUE)

readRenviron("~/.Renviron")

surveys <- all_surveys() 

qual_raw_g2 <- fetch_survey(surveyID = pull(filter(surveys, name == "Grade2_Focus-Group-Survey"), id), 
                            force_request = TRUE)

