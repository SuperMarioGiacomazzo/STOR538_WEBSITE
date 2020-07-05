options(scipen=9999)
library(tidyverse)
library(rvest)

#Import Game Data
Player=read_csv("D:/Mario Documents/UNC/STOR 390/STOR390_WEBSITE/Slides/Basketball 4/players.csv")
Salary=read_csv("D:/Mario Documents/UNC/STOR 390/STOR390_WEBSITE/Slides/Basketball 4/salaries_1985to2018.csv")
Games=read_csv("D:/Mario Documents/UNC/STOR 390/STOR390_WEBSITE/Slides/Basketball 4/nba.games.stats.csv")

#Investigation of Salary
head(Salary)
Salary.Data=Salary %>%
  group_by(team,season_start) %>%
  summarize(total.salary=sum(salary)/1000000) %>%
  ungroup() %>%
  group_by(season_start) %>%
  summarize(n=n(),mean.salary=mean(total.salary),
            sd.salary=sd(total.salary))
head(Salary.Data)


ggplot(Salary.Data) +
  geom_line(aes(x=season_start,y=mean.salary),size=2,color="deepskyblue1") +
  ylab("Average Team Salary (in Millions)") +
  xlab("NBA Season") +
  scale_x_continuous(breaks=seq(1984,2017,4)) +
  geom_hline(yintercept=mean(Salary.Data$mean.salary),linetype=2) +
  geom_errorbar(aes(x=season_start,ymin=mean.salary-sd.salary,ymax=mean.salary+sd.salary)) +
  ggtitle("Change in NBA Team Salary (Interval Indicates Single Standard Deviation)") +
  theme_classic()

#Pythagorean Theorem for Wins
head(Games[,2:9])
Games2 = Games %>%
            mutate(Season=rep(c(2014,2015,2016,2017),each=82*30)) %>%
            group_by(Team,Season) %>%
            summarize(Win.Per=mean(WINorLOSS=="W"),
                      Scored=mean(TeamPoints),
                      Allowed=mean(OpponentPoints))
head(Games2)

pythag.func=function(data,par){
  R=data$Scored/data$Allowed
  y=data$Win.Per
  resid=y-(R^(par[1]))/(R^(par[1])+1)
  return(sum(resid^2))
}

result=optim(par=c(13),fn=pythag.func,data=Games2,method="BFGS")
print(result$par[1])

#Application to Replacement Players
alpha=result$par[1]
R=68.7/98.7
ReplWinPer=(R^(alpha))/(R^(alpha)+1)
print(ReplWinPer)
ReplWinCount=ReplWinPer*82

#Calculating Salary Amount Per Win
Salary06 = Salary %>%
  filter(season_start==2006) %>%
  group_by(team) %>%
  summarize(total.salary=sum(salary)/1000000) %>%
  arrange(desc(total.salary))
head(Salary06)

ggplot(Salary06) +
  geom_histogram(aes(x=total.salary),fill="deepskyblue1") +
  ylab("Frequency") + xlab("Total Payroll (in Millions)")+
  theme_classic()
print(Salary06$team)
print(Salary06$total.salary)

#Relationship with Salary and Wins
wikipedia="https://en.wikipedia.org/wiki/2006%E2%80%9307_NBA_season"
wins = wikipedia %>%
        read_html() %>%
        html_table(fill=T)

wins2=as.data.frame(rbind(as.matrix(wins[[4]]),as.matrix(wins[[5]]),
                    as.matrix(wins[[6]]),as.matrix(wins[[7]]),
                    as.matrix(wins[[8]]),as.matrix(wins[[9]])))[,1:2]
names(wins2)=c("team","wins")
str_detect(wins2$team,".-")
wins3=mutate(wins2,team=str_replace(team,".-",""))
head(wins3)

salarywins06=inner_join(Salary06,wins3)
salarywins06$wins=as.numeric(as.character(salarywins06$wins))
head(salarywins06)

ggplot(salarywins06) + 
  geom_smooth(aes(x=wins,y=total.salary),color="deepskyblue1",
              method="lm",size=1,linetype="dashed") +
  geom_point(aes(x=wins,y=total.salary),color="deepskyblue1",size=2) +
  xlab("Number of Wins (2006-2007)") + ylab("Team Payroll") +
  theme_classic()

linearwts=lm(total.salary~wins,data=salarywins06)
summary(linearwts)

predict(linearwts,newdata=data.frame(wins=41))

median(salarywins06$total.salary)


