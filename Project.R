#bring in library dplyr
library(dplyr)

#read csv files and bring out columns which will get in the way of the rbind function
NBA1 <- read.csv("NBA_Player_Stats.csv")
NBA2 <- read.csv("NBA_Player_Stats_2.csv")
WS <- read.csv("win_share.csv")
NBA2$MVP <- NULL
names(NBA2)[names(NBA2) == "Season"] <- "Year"

#rbind and concatenate to only necessary stats (NBA)
rawNBA <- rbind(NBA1, NBA2)
NBAcon <- rawNBA[,-c(0, 1, 4, 5, 6, 7, 8, 9, 10,12,13, 15, 16, 17, 18, 19, 20, 22, 23, 31)]

#averaging the stats into one column by player name
NBAstat <- NBAcon %>%
  group_by(Player) %>%
  summarize(
    Pos = last(Pos),
    across(where(is.numeric), ~ round(mean(., na.rm = TRUE), 3)),
    .groups = "drop"
  )



#read in csv for college stats

colcon <- read.csv('COLLEGEstat.csv')


#Getting rid of players not in both of datasets
common_players <- inner_join(NBAstat, colcon, by = "Player") %>% 
  select(Player)

# Filter both datasets to include only common players
NBAstat <- NBAstat %>% 
  semi_join(common_players, by = "Player")

colcon <- colcon %>% 
  semi_join(common_players, by = "Player")

NBAstat<- cbind(NBAstat,WS)

