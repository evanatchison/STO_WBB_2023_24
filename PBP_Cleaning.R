
# File to Clean PBP Data --------------------------------------------------



# Load in libraries -------------------------------------------------------


knitr::opts_chunk$set(echo = TRUE)
library(readxl)
library(readr)
library(lubridate)
library(tidyverse)





# Function to Clean PBP Data File -----------------------------------------

clean_pbp <- function(filename){
  new_tibble <- read_excel(str_c("data/PBP/", filename, ".xlsx"))
  new_data <- new_tibble%>%
    select(number, 114:124, date, visname, homename)%>%
    filter(!is.na(action))%>%
    rename(quarter = number)%>%
    mutate(date = mdy(date))
  new_data
}



# Game by game PBP data ---------------------------------------------------

pbp <- clean_pbp("11_11_Alma_PBP") %>% 
  bind_rows(clean_pbp("11_12_North_Park_PBP"),
            clean_pbp("11_16_Minnesota_Morris_PBP"),
            clean_pbp("11_19_St_Bens_PBP"),
            clean_pbp("11_30_St_Marys_PBP"),
            clean_pbp("12_3_Hamline_PBP"),
            clean_pbp("12_7_Gustavus_PBP"))

