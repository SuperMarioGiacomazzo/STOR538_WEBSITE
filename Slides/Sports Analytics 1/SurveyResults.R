setwd("D:/Mario Documents/UNC/STOR 538/STOR538_WEBSITE/Slides/Sports Analytics 1")

library(tidyverse)
library(forcats)

DATA=read.csv("Sports Analytics Survey.csv")[,-(1:4)]
names(DATA)=c("Year","MajorMinor","R Experience","Graduation","Watch","Participated","Other")

DATA2=separate(data=DATA,col=Watch,into=paste("Watch",1:27),sep=";")
DATA3=separate(data=DATA2,col=Participated,into=paste("Part",1:30),sep=";")

table(DATA3$Year)

png('RExp.png',width=1200,height=800)
par(mar=c(3,4,3,2),cex=2,cex.main=2.5)
barplot(table(DATA3$`R Experience`)/nrow(DATA3),ylim=c(0,1),main="R Experience")
dev.off()

DATA4=DATA3 %>% mutate(Graduation=factor(Graduation)) 
levels(DATA4$Graduation)=c("Doctoral","Not Sports Analytics","Sports Analytics","Master's","Professional")

DATA4=DATA4 %>% mutate(Graduation = fct_infreq(Graduation))

png('Graduation.png',width=1200,height=800)
par(mar=c(12,3,3,1),cex=2,cex.main=2.5)
barplot(table(DATA4$`Graduation`)/nrow(DATA3),las=2,ylim=c(0,1),main="Graduation Goal")
dev.off()

sportswatched =  na.omit(gather(data=DATA4,5:31,key="Watch",value="Sport")$Sport)

unique(sportswatched)

sportswatched2 = ifelse(sportswatched==unique(sportswatched)[10],"Arcade",sportswatched)
sportswatched3 = ifelse(sportswatched2==unique(sportswatched)[9],"Combat Sports",sportswatched2)
sportswatched4 = ifelse(sportswatched3==unique(sportswatched)[3],"Auto/Bike Racing",sportswatched3)
sportswatched5 = ifelse(sportswatched4==unique(sportswatched)[38],"Yard Games",sportswatched4)
sportswatched6 = ifelse(sportswatched5==unique(sportswatched)[11],"Board Games",sportswatched5)
sportswatched7 = ifelse(sportswatched6==unique(sportswatched)[4],"Air Sports",sportswatched6)
sportswatched8 = ifelse(sportswatched7==unique(sportswatched)[5],NA,sportswatched7)
sportswatched9 = na.omit(sportswatched8)

unique(sportswatched9)
length(unique(sportswatched9))

png('Watched.png',width=1200,height=800)
par(mar=c(14,3,3,1),cex=1.5,cex.main=2.5)
barplot(table(fct_reorder(as.factor(sportswatched9),sportswatched9,.fun=table)),las=2,ylim=c(0,100),main="Sports Watched")
dev.off()

sportsplayed =  na.omit(gather(data=DATA4,32:61,key="Play",value="Sport")$Sport)

unique(sportsplayed)

sportsplayed2 = ifelse(sportsplayed==unique(sportsplayed)[5],"Arcade",sportsplayed)
sportsplayed3 = ifelse(sportsplayed2==unique(sportsplayed)[19],"Competitive Board Games",sportsplayed2)
sportsplayed4 = ifelse(sportsplayed3==unique(sportsplayed)[38],"Combat Sports",sportsplayed3)
sportsplayed5 = ifelse(sportsplayed4==unique(sportsplayed)[34],"Yard Games",sportsplayed4)
sportsplayed6 = ifelse(sportsplayed5==unique(sportsplayed)[32],"Auto/Bike Racing",sportsplayed5)
sportsplayed7 = ifelse(sportsplayed6==unique(sportsplayed)[12],NA,sportsplayed6)
sportsplayed8 = na.omit(sportsplayed7)

unique(sportsplayed8)
length(unique(sportsplayed8))

png('Played.png',width=1200,height=800)
par(mar=c(14,3,3,1),cex=1.5,cex.main=2.5)
barplot(table(fct_reorder(as.factor(sportsplayed8),sportsplayed8,.fun=table)),las=2,ylim=c(0,60),main="Sports Played")
dev.off()

table(sportsplayed8)

check=intersect(unique(sportswatched9),unique(sportsplayed8))
sportswatched10=na.omit(ifelse(sportswatched9 %in% check,sportswatched9,NA))
sportsplayed9=na.omit(ifelse(sportsplayed8 %in% check,sportsplayed8,NA))

png('Together.png',width=1400,height=700)
par(mar=c(13,3,1,1),cex=2)
barplot(table(sportswatched10),las=2,ylim=c(-70,70),col="red")
barplot(-table(sportsplayed9),las=2,ylim=c(-70,70),add=T,col="blue")
dev.off()


