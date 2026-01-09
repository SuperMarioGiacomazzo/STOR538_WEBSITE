setwd("D:/DoctorMario/UNC/STOR 538/STOR538_WEBSITE/Slides/Sports Analytics 1")

library(tidyverse)
library(forcats)

DATA=read.csv("Sports Analytics Survey.csv")[,-(1:4)]
names(DATA)=c("Year","MajorMinor","R Experience","Graduation","Watch","Participated","Other")

DATA2=separate(data=DATA,col=Watch,into=paste("Watch",1:37),sep=";")
DATA3=separate(data=DATA2,col=Participated,into=paste("Part",1:26),sep=";")

table(DATA3$Year)

png('RExp.png',width=1200,height=800)
par(mar=c(3,4,3,2),cex=2,cex.main=2.5)
barplot(table(DATA3$`R Experience`)/nrow(DATA3),ylim=c(0,1),main="R Experience")
dev.off()

DATA4=DATA3 %>% mutate(Graduation=factor(Graduation)) 
levels(DATA4$Graduation)=c("Doctorate","Outside Sports Analytics","Sports Analytics","Master's","Professional")
table(DATA3$Graduation)

DATA4=DATA4 %>% mutate(Graduation = fct_infreq(Graduation))

png('Graduation.png',width=1200,height=800)
par(mar=c(12,3,3,1),cex=2,cex.main=2.5)
barplot(table(DATA4$`Graduation`)/nrow(DATA3),las=2,ylim=c(0,1),main="Graduation Goal")
dev.off()

sportswatched =  na.omit(gather(data=DATA4,5:41,key="Watch",value="Sport")$Sport)
unique(sportswatched)

sportswatched2 = ifelse(sportswatched==unique(sportswatched)[1],"Arcade",sportswatched)
sportswatched3 = ifelse(sportswatched2==unique(sportswatched)[2],"Auto/Bike Racing",sportswatched2)
sportswatched4 = ifelse(sportswatched3==unique(sportswatched)[9],"Air Sports",sportswatched3)
sportswatched5 = ifelse(sportswatched4==unique(sportswatched)[14],"Board Games",sportswatched4)
sportswatched6 = ifelse(sportswatched5==unique(sportswatched)[16],"Combat Sports",sportswatched5)
sportswatched7 = ifelse(sportswatched6==unique(sportswatched)[51],"Yard Games",sportswatched6)
sportswatched8 = na.omit(sportswatched7)

unique(sportswatched8)
length(unique(sportswatched8))

png('Watched.png',width=1200,height=800)
par(mar=c(14,3,3,1),cex=1.5,cex.main=2.5)
barplot(table(fct_reorder(as.factor(sportswatched8),sportswatched8,.fun=table)),las=2,ylim=c(0,42),main="Sports Watched")
dev.off()


sportsplayed =  na.omit(gather(data=DATA4,42:67,key="Play",value="Sport")$Sport)

unique(sportsplayed)

sportsplayed2 = ifelse(sportsplayed==unique(sportsplayed)[1],"Arcade",sportsplayed)
sportsplayed3 = ifelse(sportsplayed2==unique(sportsplayed)[11],"Air Sports",sportsplayed2)
sportsplayed4 = ifelse(sportsplayed3==unique(sportsplayed)[15],"Board Games",sportsplayed3)
sportsplayed5 = ifelse(sportsplayed4==unique(sportsplayed)[24],"Combat Sports",sportsplayed4)
sportsplayed6 = ifelse(sportsplayed5==unique(sportsplayed)[40],"Yard Games",sportsplayed5)
sportsplayed7 = ifelse(sportsplayed6==unique(sportsplayed)[2],NA,sportsplayed6)
sportsplayed8 = na.omit(sportsplayed7)

unique(sportsplayed8)
length(unique(sportsplayed8))

png('Played.png',width=1200,height=800)
par(mar=c(14,3,3,1),cex=1.5,cex.main=2.5)
barplot(table(fct_reorder(as.factor(sportsplayed8),sportsplayed8,.fun=table)),las=2,ylim=c(0,30),main="Sports Played")
dev.off()

table(sportsplayed8)

check=intersect(unique(sportswatched8),unique(sportsplayed8))
sportswatched9=na.omit(ifelse(sportswatched8 %in% check,sportswatched8,NA))
sportsplayed8=na.omit(ifelse(sportsplayed7 %in% check,sportsplayed7,NA))

png('Together.png',width=1400,height=700)
par(mar=c(13,3,1,1),cex=2)
barplot(table(sportswatched9),las=2,ylim=c(-30,40),col="red")
barplot(-table(sportsplayed8),las=2,ylim=c(-30,40),add=T,col="blue")
dev.off()


