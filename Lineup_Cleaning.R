
# File to Clean/Combine Lineup Data ---------------------------------------




# Load in libraries -------------------------------------------------------

library(readxl)
library(readr)
library(lubridate)
library(tidyverse)




# Function for loading in lineup data -------------------------------------


clean_lineups <- function(filename, dt, opponent){
  new_tibble <- read_excel(str_c("data/lineups/", filename, ".xlsx"),
                           col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
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
  bind_rows(clean_lineups("11_12_North_Park_Lineups", "11/12/2022", "North Park College"),
            clean_lineups("11_16_Minnesota_Morris", "11/16/2022", "Minnesota Morris"),
            clean_lineups("11_19_St_Bens", "11/19/2022", "College of St. Benedict"),
            clean_lineups("11_30_St_Marys", "11/30/2022", "St. Mary's University"),
            clean_lineups("12_3_Hamline", "12/3/2022", "Hamline University"),
            clean_lineups("12_7_Gustavus", "12/7/2022", "Gustavus Adolphus College"),
            clean_lineups("12_10_St_Kates_Lineups", "12/10/2022", "St. Catherine Univeristy"))


miac_lineups <- bind_rows(clean_lineups("11_19_St_Bens", "11/19/2022", "College of St. Benedict"),
                          clean_lineups("11_30_St_Marys", "11/30/2022", "St. Mary's University"),
                          clean_lineups("12_3_Hamline", "12/3/2022", "Hamline University"),
                          clean_lineups("12_7_Gustavus", "12/7/2022", "Gustavus Adolphus College"),
                          clean_lineups("12_10_St_Kates_Lineups", "12/10/2022", "St. Catherine Univeristy"))
