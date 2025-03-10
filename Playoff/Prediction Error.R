library(DT)
library(tidyverse)
library(readxl)
library(plyr)

setwd("D:/Mario Documents/UNC/STOR 538/STOR538_WEBSITE/Playoffs/Round 2")

#Actual=read.csv(file="Actual_Wrong.csv")
Actual=read.csv(file="Actual.csv")

P1=read.csv(file="Prediction_1.csv")
P2=read.csv(file="Prediction_2.csv")[1:55,1:6]
P4=read.csv(file="Prediction_4.csv")
P5a=read.csv(file="Prediction_5a.csv")
P5b=read.csv(file="Prediction_5b.csv")
P5c=read.csv(file="Prediction_5c.csv")
P6=read.csv(file="Prediction_6.csv")
P7=read.csv(file="Prediction_7.csv")
P8=read.csv(file="Prediction_8.csv")
P9=read.csv(file="Prediction_9.csv")
P11=read.csv(file="Prediction_11.csv")
P12=read.csv(file="Prediction_12.csv")
P13=read.csv(file="Prediction_13.csv")[,1:6]
P14=read.csv(file="Prediction_14.csv")
P15=read.csv(file="Prediction_15.csv")
P16=read.csv(file="Prediction_16.csv")
P17=read.csv(file="Prediction_17.csv")

SPREAD=join_all(list(dplyr::rename(P1,Spread_1=Spread)[,1:4],
                     dplyr::rename(P2,Spread_2=Spread)[,1:4],
                     dplyr::rename(P4,Spread_4=Spread)[,1:4],
                     dplyr::rename(P5a,Spread_5a=Spread)[,1:4],
                     dplyr::rename(P5b,Spread_5b=Spread)[,1:4],
                     dplyr::rename(P5c,Spread_5c=Spread)[,1:4],
                     dplyr::rename(P6,Spread_6=Spread)[,1:4],
                     dplyr::rename(P7,Spread_7=Spread)[,1:4],
                     dplyr::rename(P8,Spread_8=Spread)[,1:4],
                     dplyr::rename(P9,Spread_9=Spread)[,1:4],
                     dplyr::rename(P11,Spread_11=Spread)[,1:4],
                     dplyr::rename(P12,Spread_12=Spread)[,1:4],
                     dplyr::rename(P13,Spread_13=Spread)[,1:4],
                     dplyr::rename(P14,Spread_14=Spread)[,1:4],
                     dplyr::rename(P15,Spread_15=Spread)[,1:4],
                     dplyr::rename(P16,Spread_16=Spread)[,1:4],
                     dplyr::rename(P17,Spread_17=Spread)[,1:4]),
                by=c("Date","Away","Home"),type="left")
                 

TOTAL=join_all(list(dplyr::rename(P1,Total_1=Total)[,c(1:3, 5)],
                     dplyr::rename(P2,Total_2=Total)[,c(1:3, 5)],
                     dplyr::rename(P4,Total_4=Total)[,c(1:3, 5)],
                     dplyr::rename(P5a,Total_5a=Total)[,c(1:3, 5)],
                     dplyr::rename(P5b,Total_5b=Total)[,c(1:3, 5)],
                     dplyr::rename(P5c,Total_5c=Total)[,c(1:3, 5)],
                     dplyr::rename(P6,Total_6=Total)[,c(1:3, 5)],
                     dplyr::rename(P7,Total_7=Total)[,c(1:3, 5)],
                     dplyr::rename(P8,Total_8=Total)[,c(1:3, 5)],
                     dplyr::rename(P9,Total_9=Total)[,c(1:3, 5)],
                     dplyr::rename(P11,Total_11=Total)[,c(1:3, 5)],
                     dplyr::rename(P12,Total_12=Total)[,c(1:3, 5)],
                     dplyr::rename(P13,Total_13=Total)[,c(1:3, 5)],
                     dplyr::rename(P14,Total_14=Total)[,c(1:3, 5)],
                     dplyr::rename(P15,Total_15=Total)[,c(1:3, 5)],
                     dplyr::rename(P16,Total_16=Total)[,c(1:3, 5)],
                     dplyr::rename(P17,Total_17=Total)[,c(1:3, 5)]),
                by=c("Date","Away","Home"),type="left")

OREB=join_all(list(dplyr::rename(P1,OREB_1=OREB)[,c(1:3, 6)],
                    dplyr::rename(P2,OREB_2=OREB)[,c(1:3, 6)],
                    dplyr::rename(P4,OREB_4=OREB)[,c(1:3, 6)],
                    dplyr::rename(P5a,OREB_5a=OREB)[,c(1:3, 6)],
                    dplyr::rename(P5b,OREB_5b=OREB)[,c(1:3, 6)],
                    dplyr::rename(P5c,OREB_5c=OREB)[,c(1:3, 6)],
                    dplyr::rename(P6,OREB_6=OREB)[,c(1:3, 6)],
                    dplyr::rename(P7,OREB_7=OREB)[,c(1:3, 6)],
                    dplyr::rename(P8,OREB_8=OREB)[,c(1:3, 6)],
                    dplyr::rename(P9,OREB_9=OREB)[,c(1:3, 6)],
                    dplyr::rename(P11,OREB_11=OREB)[,c(1:3, 6)],
                    dplyr::rename(P12,OREB_12=OREB)[,c(1:3, 6)],
                    dplyr::rename(P13,OREB_13=OREB)[,c(1:3, 6)],
                    dplyr::rename(P14,OREB_14=OREB)[,c(1:3, 6)],
                    dplyr::rename(P15,OREB_15=OREB)[,c(1:3, 6)],
                    dplyr::rename(P16,OREB_16=OREB)[,c(1:3, 6)],
                    dplyr::rename(P17,OREB_17=OREB)[,c(1:3, 6)]),
               by=c("Date","Away","Home"),type="left")

write.csv(dplyr::select(mutate(SPREAD,Actual=Actual$Spread),"Date","Away","Home","Actual",everything()),"SPREAD_class.csv")
write.csv(dplyr::select(mutate(TOTAL,Actual=Actual$Total),"Date","Away","Home","Actual",everything()),"TOTAL_class.csv")
write.csv(dplyr::select(mutate(OREB,Actual=Actual$OREB),"Date","Away","Home","Actual",everything()),"OREB_class.csv")

mae.func=function(prediction,actual){
  mean(abs(actual-prediction))
}

SPREAD.RESULT=apply(SPREAD[,-(1:3)],2,mae.func,actual=Actual$Spread)
TOTAL.RESULT=apply(TOTAL[,-(1:3)],2,mae.func,actual=Actual$Total)
OREB.RESULT=apply(OREB[,-(1:3)],2,mae.func,actual=Actual$OREB)
