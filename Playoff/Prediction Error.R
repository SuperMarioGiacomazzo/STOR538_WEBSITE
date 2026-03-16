library(DT)
library(tidyverse)
library(readxl)
library(plyr)

setwd("D:/Mario Documents/UNC/STOR 538/STOR538_WEBSITE/Playoff")

Actual=read.csv(file="Actual.csv")

P1=read.csv(file="Prediction_1.csv")
P2=read.csv(file="Prediction_2.csv")
P3=read.csv(file="Prediction_3.csv")
P4=read.csv(file="Prediction_4.csv")
P5=read.csv(file="Prediction_5.csv")
P6=read.csv(file="Prediction_6.csv")
P8=read.csv(file="Prediction_8.csv")
P9=read.csv(file="Prediction_9.csv")[,-1]
P10=read.csv(file="Prediction_10.csv")

SPREAD=join_all(list(dplyr::rename(P1,Spread_1=Spread)[,1:4],
                     dplyr::rename(P2,Spread_2=Spread)[,1:4],
                     dplyr::rename(P3,Spread_3=Spread)[,1:4],
                     dplyr::rename(P4,Spread_4=Spread)[,1:4],
                     dplyr::rename(P5,Spread_5=Spread)[,1:4],
                     dplyr::rename(P6,Spread_6=Spread)[,1:4],
                     dplyr::rename(P8,Spread_8=Spread)[,1:4],
                     dplyr::rename(P9,Spread_9=Spread)[,1:4],
                     dplyr::rename(P10,Spread_10=Spread)[,1:4]),
                by=c("Date","Away","Home"),type="left")
                 

TOTAL=join_all(list(dplyr::rename(P1,Total_1=Total)[,c(1:3,5)],
                    dplyr::rename(P2,Total_2=Total)[,c(1:3,5)],
                    dplyr::rename(P3,Total_3=Total)[,c(1:3,5)],
                    dplyr::rename(P4,Total_4=Total)[,c(1:3,5)],
                    dplyr::rename(P5,Total_5=Total)[,c(1:3,5)],
                    dplyr::rename(P6,Total_6=Total)[,c(1:3,5)],
                    dplyr::rename(P8,Total_8=Total)[,c(1:3,5)],
                    dplyr::rename(P9,Total_9=Total)[,c(1:3,5)],
                    dplyr::rename(P10,Total_10=Total)[,c(1:3,5)]),
               by=c("Date","Away","Home"),type="left")


OREB=join_all(list(dplyr::rename(P1,OREB_1=OREB)[,c(1:3,6)],
                   dplyr::rename(P2,OREB_2=OREB)[,c(1:3,6)],
                   dplyr::rename(P3,OREB_3=OREB)[,c(1:3,6)],
                   dplyr::rename(P4,OREB_4=OREB)[,c(1:3,6)],
                   dplyr::rename(P5,OREB_5=OREB)[,c(1:3,6)],
                   dplyr::rename(P6,OREB_6=OREB)[,c(1:3,6)],
                   dplyr::rename(P8,OREB_8=OREB)[,c(1:3,6)],
                   dplyr::rename(P9,OREB_9=OREB)[,c(1:3,6)],
                   dplyr::rename(P10,OREB_10=OREB)[,c(1:3,6)]),
              by=c("Date","Away","Home"),type="left")

write.csv(dplyr::select(mutate(SPREAD,Actual=Actual$Spread),"Date","Away","Home","Actual",everything()),"SPREAD_class.csv")
write.csv(dplyr::select(mutate(TOTAL,Actual=Actual$Total),"Date","Away","Home","Actual",everything()),"TOTAL_class.csv")
write.csv(dplyr::select(mutate(OREB,Actual=Actual$OREB),"Date","Away","Home","Actual",everything()),"OREB_class.csv")

mae.func=function(prediction,actual){
  mean(abs(actual-prediction),na.rm=T)
}

SPREAD.RESULT=apply(SPREAD[,-(1:3)],2,mae.func,actual=Actual$Spread)
SPREAD.RANK=rank(SPREAD.RESULT)
TOTAL.RESULT=apply(TOTAL[,-(1:3)],2,mae.func,actual=Actual$Total)
TOTAL.RANK=rank(TOTAL.RESULT)
OREB.RESULT=apply(OREB[,-(1:3)],2,mae.func,actual=Actual$OREB)
OREB.RANK=rank(OREB.RESULT)

Group = c(1:6,8:10)
Class_Results = tibble(Group=Group,
                       `Spread MAE`=SPREAD.RESULT,
                       `Spread Rank`=SPREAD.RANK,
                       `Total MAE`=TOTAL.RESULT,
                       `Total Rank`=TOTAL.RANK,
                       `OREB MAE`=OREB.RESULT,
                       `OREB Rank`=OREB.RANK)

write.csv(Class_Results,"Class_Results.csv",row.names=F)
