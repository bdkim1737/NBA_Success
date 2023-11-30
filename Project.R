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
NBAcon <- rawNBA[,-c(0, 1, 4, 5, 6, 7, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 31)]

#averaging the stats into one column by player name
NBAstat <- NBAcon %>%
  group_by(Player) %>%
  summarize(
    Pos = last(Pos),
    across(where(is.numeric), ~ round(mean(., na.rm = TRUE), 3)),
    .groups = "drop"
  )



#rbind and concatenate to only necessary stats (college)
college2016 <- read.csv("sportsref_download.csv")
college2016 <- college2016[-1, ]
college2014 <- read.csv("sportsref_download2014.csv")
college2014 <- college2014[-1, ]
college2015 <- read.csv("sportsref_download2015.csv")
college2015 <- college2015[-1, ]
college2017 <- read.csv("sportsref_download2017.csv")
college2017 <- college2017[-1, ]
college2018 <- read.csv("sportsref_download2018.csv")
college2018 <- college2018[-1, ]

college2014_2018 <- rbind(college2014, college2015, college2016, college2017, college2018)

colcon <- college2014_2018[,-c(0,1,2,3,5,6,7,8,9,10,11,20,22)]


#Changing column names and getting rid of first row that was acting like a column title
names(colcon)[names(colcon) == "Round.1"] <- "Player"
names(colcon)[names(colcon) == "Shooting"] <- "FG"
names(colcon)[names(colcon) == "X.9"] <- "ThreeP"
names(colcon)[names(colcon) == "Per.Game"] <- "MP"
names(colcon)[names(colcon) == "X.11"] <- "PTS"
names(colcon)[names(colcon) == "X.12"] <- "TRB"
names(colcon)[names(colcon) == "X.13"] <- "AST"
names(colcon)[names(colcon) == "Advanced"] <- "WS"
names(colcon)[names(colcon) == "X.15"] <- "BPM"
names(colcon)[names(colcon) == "X.10"] <- "FT"

#Changing character to number
colcon$FG <- as.numeric(colcon$FG)
colcon$ThreeP <- as.numeric(colcon$ThreeP)
colcon$MP <- as.numeric(colcon$MP)
colcon$PTS <- as.numeric(colcon$PTS)
colcon$TRB <- as.numeric(colcon$TRB)
colcon$AST <- as.numeric(colcon$AST)
colcon$WS <- as.numeric(colcon$WS)
colcon$BPM <- as.numeric(colcon$BPM)

#Getting rid of players not in both of datasets
common_players <- inner_join(NBAstat, colcon, by = "Player") %>% 
  select(Player)

# Filter both datasets to include only common players
NBAstat <- NBAstat %>% 
  semi_join(common_players, by = "Player")

colcon <- colcon %>% 
  semi_join(common_players, by = "Player")

NBAstat<- cbind(NBAstat,WS)

