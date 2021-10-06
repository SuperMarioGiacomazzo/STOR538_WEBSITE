#install.packages("nflfastR")

library(nflfastR)
library(tidyverse)

#Data from Matt's repository

results = read_csv(url("https://raw.githubusercontent.com/mattymo18/STOR-538-P2-2021-Spring/master/Source_Data/game_results.csv"))

#Play-by-Play Data in R

data2010 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2010.rds'))
data2011 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2011.rds'))
data2012 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2012.rds'))
data2013 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2013.rds'))
data2014 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2014.rds'))
data2015 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2015.rds'))
data2016 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2016.rds'))
data2017 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2017.rds'))
data2018 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))
data2019 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
data2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
data2021 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2021.rds'))

#Aggregate to Game Level

GAME2010=data2010 %>%
            group_by(game_id,posteam,posteam_type) %>%
            summarize(Total_Yds=sum(yards_gained,na.rm=T), 
                      TD_Pass=sum(pass_touchdown,na.rm=T),
                      TD_Rush=sum(rush_touchdown,na.rm=T),
                      FGA=sum(field_goal_attempt,na.rm=T),
                      FGM=sum(field_goal_result=="made",na.rm=T),
                      EPA=sum(extra_point_attempt,na.rm=T),
                      EPM=sum(extra_point_result=="good",na.rm=T),
                      TPCA=sum(two_point_attempt,na.rm=T),
                      TPCM=sum(two_point_conv_result=="success",na.rm=T),
                      SAFETY=sum(safety,na.rm=T),
                      RUSH_yds=sum(rush_attempt*yards_gained,na.rm=T),
                      PASS_yds=sum(pass_attempt*yards_gained,na.rm=T),
                      RA=sum(rush_attempt,na.rm=T),
                      PA=sum(pass_attempt,na.rm=T)) %>%
            mutate(posteam_type=ifelse(posteam_type=="away","Away","Home")) %>%
            filter(posteam!="") %>%
            ungroup() %>%
            mutate(Points=6*TD_Pass+6*TD_Rush+3*FGM+1*EPM+2*TPCM+2*SAFETY,
                   P_to_R=RA/PA) %>%
            separate(game_id,into=c("YEAR","WEEK","AWAYCITY","HOMECITY"),sep="_") %>%
            mutate(posteam=ifelse(posteam=="LA","LAR",posteam)) %>%
            select(YEAR,WEEK,AWAYCITY,HOMECITY,everything())


