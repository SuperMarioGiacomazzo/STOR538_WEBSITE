setwd("D:/Mario Documents/UNC/STOR 390/STOR390_WEBSITE/Slides/Sports Analytics 1")

library(tidyverse)
library(forcats)

DATA=read.csv("Sports Analytics Survey.csv")
names(DATA)=c("Country","State","Race","Watch","Participated","Other")

DATA2=separate(data=DATA,col=Watch,into=paste("Watch",1:23),sep=";")
DATA3=separate(data=DATA2,col=Participated,into=paste("Part",1:21),sep=";")

DATA4=DATA3 %>% 
        mutate(Country=fct_collapse(DATA3$Country,
                       USA=levels(DATA3$Country)[2:7]),
               State=fct_collapse(DATA3$State,
                       `North Carolina`=levels(DATA3$State)[c(7,10,11)]))

table(DATA4$Country)
table(DATA4$State)
table(DATA4$Race)

sportswatched =  na.omit(gather(data=DATA4,4:26,key="Watch",value="Sport")$Sport)
sportswatched2 = ifelse(sportswatched==unique(sportswatched)[4],"Arcade",sportswatched)
sportswatched3 = ifelse(sportswatched2==unique(sportswatched2)[10],"Combat Sports",sportswatched2)
sportswatched4 = ifelse(sportswatched3==unique(sportswatched3)[11],"Auto/Bike Racing",sportswatched3)
sportswatched5 = ifelse(sportswatched4==unique(sportswatched4)[47],"Yard Games",sportswatched4)

png('Watched.png',width=1200,height=800)
par(mar=c(14,2,3,1),cex=1.5,cex.main=2.5)
barplot(table(sportswatched5),las=2,ylim=c(0,30),main="Sports Watched")
dev.off()

sportsplayed =  na.omit(gather(data=DATA4,27:47,key="Watch",value="Sport")$Sport)
sportsplayed2 = ifelse(sportsplayed==unique(sportsplayed)[7],"Arcade",sportsplayed)
sportsplayed3 = ifelse(sportsplayed2==unique(sportsplayed2)[30],"Competitive Board Games",sportsplayed2)
sportsplayed4 = ifelse(sportsplayed3==unique(sportsplayed3)[32],"Combat Sports",sportsplayed3)
sportsplayed5 = ifelse(sportsplayed4==unique(sportsplayed4)[39],"Yard Games",sportsplayed4)

png('Played.png',width=1200,height=800)
par(mar=c(14,2,3,1),cex=1.5,cex.main=2.5)
barplot(table(sportsplayed5),las=2,ylim=c(0,30),main="Sports Played")
dev.off()

check=intersect(unique(sportswatched5),unique(sportsplayed5))
sportswatched6=na.omit(ifelse(sportswatched5 %in% check,sportswatched5,NA))
sportsplayed6=na.omit(ifelse(sportsplayed5 %in% check,sportsplayed5,NA))

png('Together.png',width=1400,height=700)
par(mar=c(12,2.2,1,1),cex=2)
barplot(table(sportswatched6),las=2,ylim=c(-30,30),col="red")
barplot(-table(sportsplayed6),las=2,ylim=c(-30,30),add=T,col="blue")
dev.off()

table(sportswatched5)[3]
