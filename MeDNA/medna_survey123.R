###############################
# medna survey123
# General functions reused within medna_survey123 project
# CREATED BY: mkimble
###############################

###############################
# Install or load Libraries
###############################
install_or_load_pack <- function(pack){
  # https://nhsrcommunity.com/blog/a-simple-function-to-install-and-load-packages-in-r/
  # Swapped to this code, works better
  # install_or_load_pack(pack)
  # pack: expects list of libraries, e.g., pack<-c("tidyverse","tm","wordcloud","ggwordcloud","topicmodels")
  create_pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create_pkg))
    install.packages(create_pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

load_pack <- function(pack){
  # load_pack(pack)
  # pack: expects list of libraries, e.g., pack<-c("tidyverse","tm","wordcloud","ggwordcloud","topicmodels")
  sapply(pack, require, character.only = TRUE)
}

###############################
# s123
###############################
add_seasons_df <- function(df) {
  df<-df %>%
    mutate(season = case_when(survey_month==12 | survey_month==1 | survey_month==2 ~ "Winter", 
                              survey_month==3 | survey_month==4 | survey_month==5 ~ "Spring",
                              survey_month==6 | survey_month==7 | survey_month==8 ~ "Summer",
                              survey_month==9 | survey_month==10 | survey_month==11 ~ "Fall")) %>%
    mutate(season_year = paste0(season," ",survey_year))
  return(df)
}
