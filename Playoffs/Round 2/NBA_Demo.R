#devtools::install_github("abresler/nbastatR", force=T)

library(nbastatR)
library(tidyverse)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

#Get NBA Teams
TEAM=nba_teams(league="NBA") %>% 
  filter(yearPlayedLast==2022,idLeague==2) %>%
  select(nameTeam,idTeam,slugTeam)

#Game Data for 2023
GAME2023=game_logs(
  seasons = 2023,
  league = "NBA",
  result_types = "team",
  season_types = "Regular Season",
  nest_data = F,
  assign_to_environment = TRUE,
  return_message = TRUE
)

#Box Score Data for Individual Game
BOX2023=unnest(box_scores(game_ids=c(22200033),
                      box_score_types="Traditional",
                      result_types="team"
))


