library(tidyverse)

#Monte Carlo Simulation
HR.OUT.MC=function(home.run.percent,n.Sim){
  runs.result = rep(NA,n.Sim)
  for(i in 1:n.Sim){
    runs=0
    outs=0
    while(outs<3){
      sample=runif(1)
      if(sample>home.run.percent){
        outs=outs+1
      }else{
        runs=runs+1
      }
    }
    runs.result[i]=runs
  }
  return(runs.result)
}

Player.5=HR.OUT.MC(0.5,10000)
Player.5=tibble(R.per.I=Player.5,
                R.per.G=Player.5*9)
head(Player.5)

ggplot(Player.5) +
  geom_histogram(aes(x=R.per.I),fill="deepskyblue2") +
  geom_vline(xintercept=mean(Player.5$R.per.I),size=2)+
  ylab("Frequency") + xlab("Runs Per Inning")+
  annotate("text", x = 15, y = 1500,size=4,
    label = paste("Average Runs/Inning=",mean(Player.5$R.per.I))) +
  theme_classic()

ggplot(Player.5) +
  geom_histogram(aes(x=R.per.G),fill="deepskyblue2") +
  geom_vline(xintercept=mean(Player.5$R.per.G),size=2) +
  ylab("Frequency") + xlab("Runs Per Game")+
  annotate("text", x = 9*15, y = 1500,size=4,
    label = paste("Average Runs/Game=",mean(Player.5$R.per.G))) +
  theme_classic()

Player.75=HR.OUT.MC(0.75,10000)
Player.75=tibble(R.per.I=Player.75,
                R.per.G=Player.75*9)
head(Player.75)

ggplot(Player.75) +
  geom_histogram(aes(x=R.per.G),fill="deepskyblue2") +
  geom_vline(xintercept=mean(Player.75$R.per.G),size=2) +
  ylab("Frequency") + xlab("Runs Per Game")+
  annotate("text", x = 350, y = 1200,size=4,
    label = paste("Average Runs/Game=",mean(Player.75$R.per.G))) +
  theme_classic()