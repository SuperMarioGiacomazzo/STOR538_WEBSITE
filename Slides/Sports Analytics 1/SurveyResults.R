setwd("D:/Mario Documents/UNC/STOR 538/STOR538_WEBSITE/Slides/Sports Analytics 1")

library(tidyverse)
library(forcats)

DATA=read.csv("Sports Analytics Survey.csv")[,-(1:4)]
names(DATA)=c("Year","MajorMinor","Place","R Experience","Graduation","Watch","Participated","Other")

DATA2=separate(data=DATA,col=Watch,into=paste("Watch",1:24),sep=";")
DATA3=separate(data=DATA2,col=Participated,into=paste("Part",1:25),sep=";")

table(DATA3$Year)
barplot(table(DATA3$Place))

png('RExp.png',width=1200,height=800)
par(mar=c(3,4,3,2),cex=1.5,cex.main=2.5)
barplot(table(DATA3$`R Experience`),ylim=c(0,40),main="R Experience")
dev.off()

DATA4=DATA3 %>% mutate(Graduation=as.factor(Graduation)) 
levels(DATA4$Graduation)=c("Job Outside Sports Analytics","Job In Sports Analytics","Master's","Doctoral","Professional")

png('Graduation.png',width=1200,height=800)
par(mar=c(12,2,3,1),cex=1.5,cex.main=2.5)
barplot(table(DATA4$`Graduation`),las=2,ylim=c(0,40),main="Graduation Goal")
dev.off()

sportswatched =  na.omit(gather(data=DATA4,6:29,key="Watch",value="Sport")$Sport)

unique(sportswatched)

sportswatched2 = ifelse(sportswatched==unique(sportswatched)[7],"Arcade",sportswatched)
sportswatched3 = ifelse(sportswatched2==unique(sportswatched)[15],"Combat Sports",sportswatched2)
sportswatched4 = ifelse(sportswatched3==unique(sportswatched)[12],"Auto/Bike Racing",sportswatched3)
sportswatched5 = ifelse(sportswatched4==unique(sportswatched)[54],"Yard Games",sportswatched4)
sportswatched6 = ifelse(sportswatched5==unique(sportswatched)[22],"Board Games",sportswatched5)
sportswatched7 = ifelse(sportswatched6==unique(sportswatched)[4],"Air Sports",sportswatched6)


png('Watched.png',width=1200,height=800)
par(mar=c(14,2,3,1),cex=1.5,cex.main=2.5)
barplot(table(fct_reorder(as.factor(sportswatched7),sportswatched7,.fun=table)),las=2,ylim=c(0,42),main="Sports Watched")
dev.off()

sportsplayed =  na.omit(gather(data=DATA4,30:54,key="Play",value="Sport")$Sport)

unique(sportsplayed)

sportsplayed2 = ifelse(sportsplayed==unique(sportsplayed)[6],"Arcade",sportsplayed)
sportsplayed3 = ifelse(sportsplayed2==unique(sportsplayed)[7],"Competitive Board Games",sportsplayed2)
sportsplayed4 = ifelse(sportsplayed3==unique(sportsplayed)[26],"Combat Sports",sportsplayed3)
sportsplayed5 = ifelse(sportsplayed4==unique(sportsplayed)[32],"Yard Games",sportsplayed4)

png('Played.png',width=1200,height=800)
par(mar=c(14,2,3,1),cex=1.5,cex.main=2.5)
barplot(table(fct_reorder(as.factor(sportsplayed5),sportsplayed5,.fun=table)),las=2,ylim=c(0,35),main="Sports Played")
dev.off()

check=intersect(unique(sportswatched7),unique(sportsplayed5))
sportswatched8=na.omit(ifelse(sportswatched7 %in% check,sportswatched7,NA))
sportsplayed6=na.omit(ifelse(sportsplayed5 %in% check,sportsplayed5,NA))

png('Together.png',width=1400,height=700)
par(mar=c(12,2.2,1,1),cex=2)
barplot(table(sportswatched8),las=2,ylim=c(-40,40),col="red")
barplot(-table(sportsplayed6),las=2,ylim=c(-40,40),add=T,col="blue")
dev.off()


