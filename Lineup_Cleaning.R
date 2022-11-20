
# File to Clean/Combine Lineup Data ---------------------------------------




# Load in libraries -------------------------------------------------------

library(readxl)
library(readr)
library(lubridate)
library(tidyverse)




# Function for loading in lineup data -------------------------------------


clean_lineups <- function(filename, dt, opponent){
  new_tibble <- read_excel(str_c("data/", filename, ".xlsx"))
  new_data <- new_tibble%>%
    rename(Lineup = 1,
           Time = 2,
           Score = 3, 
           Score_Diff = 4,
           Pts_Min = 5,
           Reb = 6,
           Stl = 7,
           Tov = 8, 
           Ast = 9,
           PPP = 10)%>%
    mutate(date = mdy(dt),
           opp = opponent,
           # Roundabout way of grabbing points allowed
           Pts_Allowed = abs(parse_number(str_extract(Score, "-[0-9]+"))),
           Score = parse_number(Score), # Points scored,
           Score_Diff = parse_number(Score_Diff),
           Pts_Min = parse_number(Pts_Min),
           Reb = parse_number(Reb),
           Stl = parse_number(Stl),
           Tov = parse_number(Tov),
           Ast = parse_number(Ast),
           min = parse_number(Time),
           sec = parse_number(str_extract(Time, ":\\d\\d")),
           total_sec = min*60 + sec)
  new_data
}





# Game by game lineup data ------------------------------------------------

lineups <- clean_lineups("11_11_Alma_Lineups", "11/11/2022", "Alma College") %>% 
  bind_rows(clean_lineups("11_12_North_Park_Lineups", "11/12/2022", "North Park College"))


