#devtools::install_github("abresler/nbastatR", force=T)

library(nbastatR)
library(tidyverse)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

#Get NBA Teams
TEAM=nba_teams(league="NBA") %>% 
  filter(yearPlayedLast==2024,idLeague==2) %>%
  select(nameTeam,idTeam,slugTeam)

#Game Data for 2023
GAME2024=game_logs(
  seasons = 2025,
  league = "NBA",
  result_types = "team",
  season_types = "Regular Season",
  nest_data = F,
  assign_to_environment = TRUE,
  return_message = TRUE
)

#Selecting One Game to Illustrate Cleaning
GAME=filter(GAME2024,idGame==22400062) %>%
              select(idGame,nameTeam,locationGame,orebTeam,ptsTeam)

#Split Data Up Into Home and Away
HOME=GAME %>% filter(locationGame=="H") %>% select(-locationGame)
AWAY=GAME %>% filter(locationGame=="A") %>% select(-locationGame)

#Rename Variables
HOME2 = HOME %>% rename(Home=nameTeam,OREB_H=orebTeam,PTS_H=ptsTeam)
AWAY2 = AWAY %>% rename(Away=nameTeam,OREB_A=orebTeam,PTS_A=ptsTeam)

#Merge Datasets and Create Spread, Total, and OREB

COMBINED = full_join(HOME2,AWAY2, by=c("idGame")) %>%
            mutate(Spread=PTS_H-PTS_A,
                   Total=PTS_H+PTS_A,
                   OREB=OREB_H+OREB_A) %>%
            select(idGame,Away,Home,Spread,Total,OREB,everything())



#Box Score Data for Individual Game
BOX2025=unnest(box_scores(game_ids=c(22400062),
                      box_score_types="Advanced",
                      result_types="team"
))

