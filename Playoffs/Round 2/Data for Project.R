#Install and Load RCurl Package
library(curl)
library(tidyverse)

#Read Data Directly from Github
GAMES=read.csv(url("https://raw.githubusercontent.com/mattymo18/STOR-538-Project2-2021/master/Source-Data/games.csv"))
GAMES_DETAILS=read.csv(url("http://raw.githubusercontent.com/mattymo18/STOR-538-Project2-2021/master/Source-Data/games_details.csv"))
TEAMS=read.csv(url("https://raw.githubusercontent.com/mattymo18/STOR-538-Project2-2021/master/Source-Data/teams.csv"))

#Preview Datasets
head(filter(GAMES,GAME_ID==12000047))
head(filter(GAMES_DETAILS,GAME_ID==12000047))

#Simplify Games Data
ONE_GAME=GAMES %>% filter(GAME_ID==12000047) %>% 
            select(GAME_DATE_EST,GAME_ID,HOME_TEAM_ID,VISITOR_TEAM_ID,PTS_home,PTS_away) %>%
            mutate(Spread=PTS_home-PTS_away,Total=PTS_home+PTS_away)
head(ONE_GAME)

#Obtain Aggregated OREB from Player Level Statistics
OREB = GAMES_DETAILS %>%
          select (TEAM_ABBREVIATION,GAME_ID,TEAM_ID,OREB) %>%
          group_by(TEAM_ABBREVIATION,GAME_ID,TEAM_ID) %>%
          summarize(OREB=sum(OREB,na.rm=T)) %>%
          ungroup()

head(filter(OREB,GAME_ID==12000047))

#Merging Offensive Rebounds Into Game Data
ONE_GAME_DONE = left_join(ONE_GAME,select(OREB,-TEAM_ABBREVIATION),by=c("GAME_ID","HOME_TEAM_ID"="TEAM_ID")) %>%
                    rename(OREB_home=OREB) %>%
                    left_join(select(OREB,-TEAM_ABBREVIATION),by=c("GAME_ID","VISITOR_TEAM_ID"="TEAM_ID")) %>%
                    rename(OREB_away=OREB) %>%
                    mutate(OREB=OREB_home+OREB_away)
head(ONE_GAME_DONE)

#Creating Home Team and Away Team Variables
TWO_TEAMS=filter(TEAMS,TEAM_ID %in% c(1610612753,1610612766)) %>%
            select(TEAM_ID,CITY,NICKNAME) %>%
            unite(NAME,CITY,NICKNAME,sep=" ")

head(TWO_TEAMS)

#Merging Team Name into original data
ONE_GAME_DONE_AGAIN=left_join(ONE_GAME_DONE,TWO_TEAMS,by=c("HOME_TEAM_ID"="TEAM_ID")) %>%
                        rename("Home Team"=NAME) %>%
                        left_join(TWO_TEAMS,by=c("VISITOR_TEAM_ID"="TEAM_ID")) %>%
                        rename("Away Team"=NAME) %>%
                        select(GAME_DATE_EST,"Home Team","Away Team",everything()) %>%
                        select(-GAME_ID,-HOME_TEAM_ID,-VISITOR_TEAM_ID)





