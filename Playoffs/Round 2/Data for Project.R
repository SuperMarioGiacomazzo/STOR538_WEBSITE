#Install and Load RCurl Package
library(curl)

#Convert Text to Tables
GAMES=read.csv(url("https://raw.githubusercontent.com/mattymo18/STOR-538-Project2-2021/master/Source-Data/games.csv"))
GAMES_DETAILS=read.csv(url("http://raw.githubusercontent.com/mattymo18/STOR-538-Project2-2021/master/Source-Data/games_details.csv"))
TEAMS=read.csv(url("https://raw.githubusercontent.com/mattymo18/STOR-538-Project2-2021/master/Source-Data/teams.csv"))


