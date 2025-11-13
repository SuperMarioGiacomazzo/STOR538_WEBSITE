library(DT)
library(tidyverse)
library(readxl)
library(plyr)

setwd("D:/DoctorMario/UNC/STOR 538/STOR538_WEBSITE/Playoff")

Actual=read.csv(file="Actual.csv",fileEncoding="latin1")
Actual$Home=gsub('^.', '', Actual$Home)
Actual$Away=gsub('^.', '', Actual$Away)
Actual[58,2]="Miami Dolphins"
Actual=Actual[,c(1,2,3,5,4,6)]
Actual=dplyr::rename(Actual,Total=Total.Points,Pass=Total.Passing.Yards)

P1=read.csv(file="Predictions_1.csv")
P2=read.csv(file="Predictions_2.csv")
P2[58,2]="Miami Dolphins"
P3=read.csv(file="Predictions_3.csv",fileEncoding="latin1")
P3$Home=gsub('^.', '', P3$Home)
P3$Away=gsub('^.', '', P3$Away)
P3[58,2]="Miami Dolphins"
P4=read.csv(file="Predictions_4.csv")
P5=read.csv(file="Predictions_5.csv")
P5$Home=gsub('^.', '', P5$Home)
P5$Away=gsub('^.', '', P5$Away)
P5[58,2]="Miami Dolphins"
P6=read.csv(file="Predictions_6.csv",fileEncoding="latin1")
P6$Home=gsub('^.', '', P6$Home)
P6$Away=gsub('^.', '', P6$Away)
P6[58,2]="Miami Dolphins"
P7=read.csv(file="Predictions_7.csv")
P8=read.csv(file="Predictions_8.csv")
P8$Home=gsub('^.', '', P8$Home)
P8$Away=gsub('^.', '', P8$Away)
P9=read.csv(file="Predictions_9.csv")
P9$Home=gsub('^.', '', P9$Home)
P9$Away=gsub('^.', '', P9$Away)
P9[58,2]="Miami Dolphins"

SPREAD=join_all(list(dplyr::rename(P1,Spread_1=Spread)[,1:4],
                     dplyr::rename(P2,Spread_2=Spread)[,1:4],
                     dplyr::rename(P3,Spread_3=Spread)[,1:4],
                     dplyr::rename(P4,Spread_4=Spread)[,1:4],
                     dplyr::rename(P5,Spread_5=Spread)[,1:4],
                     dplyr::rename(P6,Spread_6=Spread)[,1:4],
                     dplyr::rename(P7,Spread_7=Spread)[,1:4],
                     dplyr::rename(P8,Spread_8=Spread)[,1:4],
                     dplyr::rename(P9,Spread_9=Spread)[,1:4]),
                by=c("Week","Home","Away"),type="left")
                 

TOTAL=join_all(list(dplyr::rename(P1,Total_1=Total)[,c(1:3,5)],
                     dplyr::rename(P2,Total_2=Total)[,c(1:3,5)],
                     dplyr::rename(P3,Total_3=Total)[,c(1:3,5)],
                     dplyr::rename(P4,Total_4=Total)[,c(1:3,5)],
                     dplyr::rename(P5,Total_5=Total)[,c(1:3,5)],
                     dplyr::rename(P6,Total_6=Total)[,c(1:3,5)],
                     dplyr::rename(P7,Total_7=Total)[,c(1:3,5)],
                     dplyr::rename(P8,Total_8=Total)[,c(1:3,5)],
                     dplyr::rename(P9,Total_9=Total)[,c(1:3,5)]),
               by=c("Week","Home","Away"),type="left")

PASS=join_all(list(dplyr::rename(P1,Pass_1=Pass)[,c(1:3,6)],
                    dplyr::rename(P2,Pass_2=Pass)[,c(1:3,6)],
                    dplyr::rename(P3,Pass_3=Pass)[,c(1:3,6)],
                    dplyr::rename(P4,Pass_4=Pass)[,c(1:3,6)],
                    dplyr::rename(P5,Pass_5=Pass)[,c(1:3,6)],
                    dplyr::rename(P6,Pass_6=Pass)[,c(1:3,6)],
                    dplyr::rename(P7,Pass_7=Pass)[,c(1:3,6)],
                    dplyr::rename(P8,Pass_8=Pass)[,c(1:3,6)],
                    dplyr::rename(P9,Pass_9=Pass)[,c(1:3,6)]),
              by=c("Week","Home","Away"),type="left")

write.csv(dplyr::select(mutate(SPREAD,Actual=Actual$Spread),"Week","Home","Away","Actual",everything()),"SPREAD_class.csv")
write.csv(dplyr::select(mutate(TOTAL,Actual=Actual$Total),"Week","Home","Away","Actual",everything()),"TOTAL_class.csv")
write.csv(dplyr::select(mutate(PASS,Actual=Actual$Pass),"Week","Home","Away","Actual",everything()),"PASS_class.csv")

mae.func=function(prediction,actual){
  mean(abs(actual-prediction),na.rm=T)
}

SPREAD.RESULT=apply(SPREAD[,-(1:3)],2,mae.func,actual=Actual$Spread)
SPREAD.RANK=rank(SPREAD.RESULT)
TOTAL.RESULT=apply(TOTAL[,-(1:3)],2,mae.func,actual=Actual$Total)
TOTAL.RANK=rank(TOTAL.RESULT)
PASS.RESULT=apply(PASS[,-(1:3)],2,mae.func,actual=Actual$Pass)
PASS.RANK=rank(PASS.RESULT)

Group = c(1:9)
Class_Results = tibble(Group=Group,
                       `Spread MAE`=SPREAD.RESULT,
                       `Spread Rank`=SPREAD.RANK,
                       `Total MAE`=TOTAL.RESULT,
                       `Total Rank`=TOTAL.RANK,
                       `Pass MAE`=PASS.RESULT,
                       `Pass Rank`=PASS.RANK)

write.csv(Class_Results,"Class_Results.csv",row.names=F)




