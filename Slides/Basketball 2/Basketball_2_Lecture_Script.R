options(scipen=9999)
library(tidyverse)

#Import Game Data
GameData=read_csv("D:/Mario Documents/UNC/STOR 538/STOR538_WEBSITE/Slides/Basketball 2/GameData.csv")
head(GameData)

#Modifed Data
GameData2 = cbind(GameData[,1:2],matrix(NA,20,18))
names(GameData2)[3:20]=paste("Player",1:18,sep="")

for(j in 1:20){
  for(k in 1:18)
  GameData2[j,k+2]= as.numeric(k %in% GameData[j,3:12])
}

GameData2[,12:20]=-GameData2[,12:20]
Games.Played=abs(colSums(GameData2[,3:20]))

GameData2b=GameData2

#Added Constraint to Data (Sum of Effects = 0)
GameData2[21,]=c(NA,0,rep(1,18))
GameData2b[21,]=c(NA,0,Games.Played)


#Create Matrix A
A=as.matrix(GameData2[,3:20])
Ab=as.matrix(GameData2b[,3:20])

#Create Vector y
y=as.matrix(GameData2[,2])
yb=as.matrix(GameData2b[,2])

#Solve Linear Equations
b=solve(t(A)%*%A)%*%t(A)%*%y
mean(b)
bb=solve(t(Ab)%*%Ab)%*%t(Ab)%*%y
mean(bb)

OUT=cbind(Games.Played,b,bb)

plot(x=OUT[,1],y=(OUT[,2]-OUT[,3]))

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

#Test Results from Website
y=c(-40,-25,60,0,-30,-12.5,-12.5,-30,50,30,-50)
A=matrix(c(1,1,1,0,0,-1,-1,-1,0,0,
           1,1,1,0,0,-1,-1,0,-1,0,
           1,0,0,1,1,-1,0,0,-1,-1,
           1,0,0,1,1,0,0,-1,-1,-1,
           0,1,1,1,0,0,-1,-1,-1,0,
           0,1,1,0,1,0,-1,-1,0,-1,
           1,1,1,0,0,0,-1,0,-1,-1,
           1,1,1,0,0,-1,0,0,-1,-1,
           0,0,1,1,1,-1,0,0,-1,-1,
           1,0,1,1,0,-1,-1,-1,0,0,
           1,0,0,1,1,0,-1,-1,-1,0),nrow=11,ncol=10,byrow=T)

t(A)%*%A
colSums(A)
b=solve(t(A)%*%A)%*%t(A)%*%y
mean(b)

A2=rbind(A,rep(1,10))
y2=c(y,0)
b2=solve(t(A2)%*%A2)%*%t(A2)%*%y2
mean(b2)

mean(c(-30.5,0,-56,36.5,-31.75,-65.5,-38.25,8.25,40.5,-60.75))

lm(y~.-1,data=as.data.frame(cbind(A,y)))

lm(y2~.-1,data=as.data.frame(cbind(A2,y2)))

#Constraint
y=c(30,40,20,15,60,14,17,25)

X=matrix(c(1,0,0,0,
           1,0,0,0,
           0,1,0,0,
           0,1,0,0,
           0,0,1,0,
           0,0,1,0,
           0,0,0,1,
           0,0,0,1),nrow=8,ncol=4,byrow=T)
data=as.data.frame(cbind(y,X))

lm(y~.,data=data)
lm(y~.-1,data=data)

y2=c(30,40,20,15,60,14,17,25,0)

X2=matrix(c(1,1,0,0,0,
           1,1,0,0,0,
           1,0,1,0,0,
           1,0,1,0,0,
           1,0,0,1,0,
           1,0,0,1,0,
           1,0,0,0,1,
           1,0,0,0,1,
           0,1,1,1,1),nrow=9,ncol=5,byrow=T)
data2=as.data.frame(cbind(y2,X2))

lm(y2~.-1,data2)

solve(t(X2)%*%X2)%*%t(X2)%*%y2
35-27.625
