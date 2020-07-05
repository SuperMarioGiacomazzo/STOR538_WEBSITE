options(scipen=9999)
library(tidyverse)

#Import Game Data
GameData=read_csv("D:/Mario Documents/UNC/STOR 390/STOR390_WEBSITE/Slides/Basketball 2/GameData.csv")
head(GameData)

#Modifed Data
GameData2 = cbind(GameData[,1:2],matrix(NA,20,18))
names(GameData2)[3:20]=paste("Player",1:18,sep="")

for(j in 1:20){
  for(k in 1:18)
  GameData2[j,k+2]= as.numeric(k %in% GameData[j,3:12])
}

GameData2[,12:20]=-GameData2[,12:20]
Games.Played=colSums(GameData2[,3:20])


#Added Constraint to Data (Sum of Effects = 0)
GameData2[21,]=c(NA,0,rep(1/18,18))

#Create Matrix A
A=as.matrix(GameData2[,3:20])

#Create Vector y
y=as.matrix(GameData2[,2])

#Solve Linear Equations
b=solve(t(A)%*%A)%*%t(A)%*%y

#Print for Slides
colnames(A)=NULL
print(A)
print(y)
print(b)
colSums(A[-21,-c(1,2)]!=0)

#Approximation of Score
Approx.Score=rep(NA,20)
for(k in 1:20){
  Team1Total=sum(as.numeric(b)[as.numeric(GameData[k,3:7])])
  Team2Total=sum(as.numeric(b)[as.numeric(GameData[k,8:12])])
  Approx.Score[k]=Team1Total-Team2Total
}
plot(x=y[-21],y=Approx.Score,col="red",pch=16,
     xlab="Actual Result",ylab="Estimated Result")
abline(a=0,b=1,lty=2)


#Looking at a Specific Player
Games.Played.15=GameData2[GameData2$Player15==-1,]
Opponent.Points=rep(NA,dim(Games.Played.15)[1])
Team.Points=rep(NA,dim(Games.Played.15)[1])
for(k in 1:dim(Games.Played.15)[1]){
  Opponent.Points[k]=sum(b[which(Games.Played.15[k,3:20]==1)])
  Team.Points[k]=sum(b[which(Games.Played.15[k,3:20]==-1)])
}
x1=mean(Team.Points)-mean(Opponent.Points)

Team.Points2=rep(NA,dim(Games.Played.15)[1])
for(k in 1:dim(Games.Played.15)[1]){
  Team.Points2[k]=sum(b[which(Games.Played.15[k,3:20]==-1)])-9.742491
}
x2=mean(Team.Points2)-mean(Opponent.Points)

x1-x2
