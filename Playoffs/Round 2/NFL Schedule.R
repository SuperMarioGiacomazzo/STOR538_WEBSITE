library(tidyverse)
library(rvest)
library(XML)

url="https://www.sportingnews.com/us/nfl/news/nfl-schedule-2020-dates-times-channels-week-by-week/112tca2d0jcy91t47yhzemrrhp"

out = url %>%
        read_html() %>%
        html_nodes("table") %>%
        html_table(fill=TRUE,header=T)

predictions=out[[1]]

for(k in 2:51){
  predictions=predictions %>% add_row(out[[k]])
}

predictions2=tibble(Game=predictions[109:147,-c(2,3)]) %>%
              separate(Game, into=c("Away Team","Home Team"),sep=" at ") %>%
              mutate(Spread=NA,Total=NA,Result=NA) %>%
              mutate(Date=c(rep("11/1/2020",11),"11/2/2020","11/5/2020",
                            rep("11/8/2020",12),"11/9/2020","11/12/2020",
                            rep("11/15/2020",12))) %>%
              select(Date,everything())

predictions3=predictions2[1:7,] %>% 
              add_row(Date="11/1/2020",
                        `Away Team`="Pittsburgh Steelers",
                        `Home Team`="Baltimore Ravens",
                        Spread=NA,Total=NA,Result=NA) %>%
              add_row(predictions2[8:39,])

str(predictions3)

write_csv(predictions3,"Predictions.csv")
