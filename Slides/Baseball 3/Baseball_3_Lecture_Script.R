#Predicted Runs for Season
pred.runs.season=function(x){
  y=x%*%c(-422.3215,0.4624,0.8090,1.0566,1.4321,0.3284,0.2045)
  return(y)
}

pred.runs.season_book=function(x){
  y=x%*%c(-422,0.462,0.809,1.056,1.432,0.328,0.204)
  return(y)
}

pred.runs.season(c(1,948,285,56,190,513,137)) #Arizona Diamondbacks 2016

#Percent of Outs for Average Players
teammult=3912.49/4328.64
teammult

#Bryant 2016 Stats (S,D,T,HR,BB+HBP,SB)
bryant16=c(99,35,3,39,93,8)

#Average Team Stats (S,D,T,HR,BB+HBP,SB)
avg.team=c(939.8286,276.2,29.161905,159.3571,544.5905,95.07619)

#Bryant+Average Team Stats
bryant.avg=avg.team*teammult+bryant16
bryant.avg

#Predicted Runs Scored
avg_predict=pred.runs.season(c(1,avg.team))
bryant.avg_predict=pred.runs.season(c(1,bryant.avg))
bryant.avg_predict-avg_predict

#Predicted Runs Scored Book
avg_predict=pred.runs.season_book(c(1,avg.team))
bryant.avg_predict=pred.runs.season_book(c(1,bryant.avg))
bryant.avg_predict-avg_predict

#Slide Check
0.904*939.83+99
