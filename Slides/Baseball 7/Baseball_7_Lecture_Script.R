options(scipen=9999)

#WWRT
mu=2*100*62/162+1
sd=sqrt((mu-1)*(mu-2)/(162-1))
z=(15-mu)/sd
print(c(mu,sd,z))

#Random Simulation of Hitting Streaks of Good Batter
Batting.Average=0.333
hit.sim=sample(x=c(0,1),size=1000000,replace=T,
               prob=c(1-Batting.Average,Batting.Average))
hitting.streak=1:15
streak.count=1:15

for(i in hitting.streak){
  n.streak=0
  count=0
  for(j in 1:(length(hit.sim)-i+1)){
    count=count+1
    if(sum(hit.sim[j:(j+i-1)]==1)==i){
      n.streak=n.streak+1
    }else{
      n.streak=n.streak+0
    } 
  }
  hitting.streak[i]=n.streak
  streak.count[i]=count
}

#Random Simulation of Hitting Slumps of Good Batter
Batting.Average=0.333
hit.sim=sample(x=c(0,1),size=1000000,replace=T,
               prob=c(1-Batting.Average,Batting.Average))
hitting.slump=1:15
slump.count=1:15

for(i in hitting.slump){
  n.slump=0
  count=0
  for(j in 1:(length(hit.sim)-i+1)){
    count=count+1
    if(sum(hit.sim[j:(j+i-1)]==0)==i){
      n.slump=n.slump+1
    }else{
      n.slump=n.slump+0
    } 
  }
  hitting.slump[i]=n.slump
  slump.count[i]=count
}

#Show Results
print(hitting.streak/streak.count)
print(hitting.slump/slump.count)

#Combine Results for Graphic
library(tidyverse)
sim.data=tibble(length=1:15,
                hitting.streak=hitting.streak/streak.count,
                hitting.slump=hitting.slump/slump.count)

ggplot(data=sim.data)+
  geom_bar(aes(x=length,y=hitting.streak),stat="identity")+
  geom_text(aes(x=length,y=hitting.streak,
                label=round(hitting.streak,2)), vjust=-0.2)+
  xlab("Possible Length of Hitting Streak")+
  ylab("Relative Frequency")+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(data=sim.data)+
  geom_bar(aes(x=length,y=hitting.slump),stat="identity")+
  geom_text(aes(x=length,y=hitting.slump,
                label=round(hitting.slump,2)), vjust=-0.2)+
  xlab("Possible Length of Hitting Slump")+
  ylab("Relative Frequency")+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#Poisson Distribution Example
dpois(7,lambda=5,log=F)
dpois(0,lambda=5,log=F)^5

#Probability Calculation
A3=1-(1-.333)^3
A4=1-(1-.333)^4
A=(A3^28)*(A4^28)
E=0.5*(1-A3)+0.5*(1-A4)

2006-1900
0.0000055*106
1+98*.247

25.21*A

0.024/.0000055
.000055*106*X=.024
.024/106
.024/(106*.0000055)
library(Lahman)
Data=Batting %>% 
  filter(yearID>=1900 & yearID<=2006) %>% 
  filter(AB>=500) %>% 
  summarize(n=n())
Data$n

1-dpois(0,0.024)
1-.4^0*exp(-0.4)/1

Data2=Pitching %>% filter(G==35) %>% 
  filter(yearID>=1900 & yearID<=2006)

