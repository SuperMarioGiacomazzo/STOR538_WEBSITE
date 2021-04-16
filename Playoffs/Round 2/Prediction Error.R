setwd("D:/Mario Documents/UNC/STOR 538/STOR538_WEBSITE/Playoffs/Round 2")

Actual=read.csv(file="Actual.csv")[-(1:25),]

P1=read.csv(file="Predictions_1.csv")[-(1:25),]
P2=read.csv(file="Predictions_2.csv")[-(1:25),-1]
P3=read.csv(file="Predictions_3.csv")[-(1:25),-1]
P4=read.csv(file="Predictions_4.csv")[-(1:25),-1]
P5=read.csv(file="Predictions_5.csv")[-(1:25),]
P6=read.csv(file="Predictions_6.csv")[-(1:25),]
P7=read.csv(file="Predictions_7.csv")[-(1:25),]
P8=read.csv(file="Predictions_8.csv")[-(1:25),]
P9=read.csv(file="Predictions_9.csv")[-(1:25),]
P10=read.csv(file="Predictions_10.csv")[-(1:25),]
P11=read.csv(file="Predictions_11.csv")[-(1:25),]
P12=read.csv(file="Predictions_12.csv")[-(1:25),]
P13=read.csv(file="Predictions_13.csv")[-(1:25),]
P13$Spread=as.numeric(P13$Spread)
P13$Total=as.numeric(P13$Total)
P13$OREB=as.numeric(P13$OREB)
P14=read.csv(file="Predictions_14.csv")[-(1:25),]
P15=read.csv(file="Predictions_15.csv")[-(1:25),]
P16=read.csv(file="Predictions_16.csv")[-(1:25),]
P17=read.csv(file="Predictions_17.csv")[-(1:25),]

SPREAD=cbind(P1$Spread,P2$Spread,P3$Spread,P4$Spread,
             P5$Spread,P6$Spread,P7$Spread,P8$Spread,
             P9$Spread,P10$Spread,P11$Spread,P12$Spread,
             P13$Spread,P14$Spread,P15$Spread,P16$Spread,P17$Spread)

TOTAL=cbind(P1$Total,P2$Total,P3$Total,P4$Total,
             P5$Total,P6$Total,P7$Total,P8$Total,
             P9$Total,P10$Total,P11$Total,P12$Total,
             P13$Total,P14$Total,P15$Total,P16$Total,P17$Total)

OREB=cbind(P1$OREB,P2$OREB,P3$OREB,P4$OREB,
             P5$OREB,P6$OREB,P7$OREB,P8$OREB,
             P9$OREB,P10$OREB,P11$OREB,P12$OREB,
             P13$OREB,P14$OREB,P15$OREB,P16$OREB,P17$OREB)

rmse.func=function(prediction,actual){
  sqrt(mean((actual-prediction)^2,na.rm=T))
}

SPREAD.RESULT=apply(SPREAD,2,rmse.func,actual=Actual$Spread)
TOTAL.RESULT=apply(TOTAL,2,rmse.func,actual=Actual$Total)
OREB.RESULT=apply(OREB,2,rmse.func,actual=Actual$OREB)

