setwd("D:/Mario Documents/UNC/STOR 538/STOR538_WEBSITE/Slides/Sports Analytics 1")

library(tidyverse)
library(forcats)

DATA=read.csv("Sports Analytics Survey.csv")[,-(1:4)]
names(DATA)=c("Year","MajorMinor","R Experience","Graduation","Watch","Participated","Other")

DATA2=separate(data=DATA,col=Watch,into=paste("Watch",1:33),sep=";")
DATA3=separate(data=DATA2,col=Participated,into=paste("Part",1:27),sep=";")

table(DATA3$Year)

png('RExp.png',width=1200,height=800)
par(mar=c(3,4,3,2),cex=1.5,cex.main=2.5)
barplot(table(DATA3$`R Experience`),ylim=c(0,35),main="R Experience")
dev.off()

DATA4=DATA3 %>% mutate(Graduation=factor(Graduation)) 
levels(DATA4$Graduation)=c("Doctoral","Not Sports Analytics","Sports Analytics","Master's","Professional")

DATA4=DATA4 %>% mutate(Graduation = fct_infreq(Graduation))

png('Graduation.png',width=1200,height=800)
par(mar=c(12,3,3,1),cex=1.5,cex.main=2.5)
barplot(table(DATA4$`Graduation`)/96,las=2,ylim=c(0,1),main="Graduation Goal")
dev.off()

sportswatched =  na.omit(gather(data=DATA4,5:42,key="Watch",value="Sport")$Sport)

unique(sportswatched)

sportswatched2 = ifelse(sportswatched==unique(sportswatched)[9],"Arcade",sportswatched)
sportswatched3 = ifelse(sportswatched2==unique(sportswatched)[13],"Combat Sports",sportswatched2)
sportswatched4 = ifelse(sportswatched3==unique(sportswatched)[6],"Auto/Bike Racing",sportswatched3)
sportswatched5 = ifelse(sportswatched4==unique(sportswatched)[47],"Yard Games",sportswatched4)
sportswatched6 = ifelse(sportswatched5==unique(sportswatched)[11],"Board Games",sportswatched5)
sportswatched7 = ifelse(sportswatched6==unique(sportswatched)[5],"Air Sports",sportswatched6)

unique(sportswatched7)

png('Watched.png',width=1200,height=800)
par(mar=c(14,2,3,1),cex=1.5,cex.main=2.5)
barplot(table(fct_reorder(as.factor(sportswatched7),sportswatched7,.fun=table)),las=2,ylim=c(0,42),main="Sports Watched")
dev.off()

sportsplayed =  na.omit(gather(data=DATA4,43:61,key="Play",value="Sport")$Sport)

unique(sportsplayed)

sportsplayed2 = ifelse(sportsplayed==unique(sportsplayed)[4],"Arcade",sportsplayed)
sportsplayed3 = ifelse(sportsplayed2==unique(sportsplayed)[13],"Competitive Board Games",sportsplayed2)
sportsplayed4 = ifelse(sportsplayed3==unique(sportsplayed)[8],"Combat Sports",sportsplayed3)
sportsplayed5 = ifelse(sportsplayed4==unique(sportsplayed)[38],"Yard Games",sportsplayed4)
sportsplayed6 = ifelse(sportsplayed5==unique(sportsplayed)[6],"Air Sports",sportsplayed5)

unique(sportsplayed6)

png('Played.png',width=1200,height=800)
par(mar=c(14,2,3,1),cex=1.5,cex.main=2.5)
barplot(table(fct_reorder(as.factor(sportsplayed6),sportsplayed,.fun=table)),las=2,ylim=c(0,35),main="Sports Played")
dev.off()

check=intersect(unique(sportswatched7),unique(sportsplayed6))
sportswatched8=na.omit(ifelse(sportswatched7 %in% check,sportswatched7,NA))
sportsplayed7=na.omit(ifelse(sportsplayed6 %in% check,sportsplayed6,NA))

png('Together.png',width=1400,height=700)
par(mar=c(12,2.2,1,1),cex=2)
barplot(table(sportswatched8),las=2,ylim=c(-40,40),col="red")
barplot(-table(sportsplayed7),las=2,ylim=c(-40,40),add=T,col="blue")
dev.off()


