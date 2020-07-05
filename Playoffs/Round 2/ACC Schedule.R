library(tidyverse)
library(rvest)

url="https://en.wikipedia.org/wiki/2019_Atlantic_Coast_Conference_football_season"

out = url %>%
        read_html() %>%
        html_nodes(css=".wikitable") %>%
        html_table(fill=T)

predictions = rbind(out[[30]][-c(1,7),c(1,4,3)],out[[32]][-9,c(1,4,3)],out[[34]][-10,c(1,4,3)]) %>%
                rename("Home Team"="Home team","Visitor Team"="Visiting team") %>%
                mutate(Date=as.Date(Date, format="%B %d")) %>%
                mutate(Spread=NA,Total=NA,Result=NA)

write_csv(predictions,"Predictions.csv")
