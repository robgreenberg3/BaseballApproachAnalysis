#20-21     Visiting and home line scores.  For example:
# "010000(10)0x"
#Would indicate a game where the home team scored a run in
#the second inning, ten in the seventh and didn't bat in the
#bottom of the ninth.


# Robert Greenberg
# 12/15/2018

#remove all existing objects from workspace
rm(list=ls())

#cleaner way to create column:
library(tidyverse)
library(ggplot2)
library(stringi)

#Reading in data:
getwd()

root_path <- "C:/Users/RobertGreenberg/"
setwd(root_path)
getwd()

# Read in Data
allgp_2017 <- read_csv("Documents/Sabermetrics/SaberProject/all2017.csv")
Eventheaders <- read_csv("Documents/Sabermetrics/SaberProject/all2017_headers.csv")
master_id <- read_csv("Documents/Sabermetrics/SaberProject/master_id.csv")
fangraphs_data <- read_csv("Documents/Sabermetrics/SaberProject/FanGraphs_Leaderboard_2017.csv")


as.vector(Eventheaders)

colnames(allgp_2017) <- colnames(Eventheaders)

allgp_2017 <- allgp_2017[!is.na(names(allgp_2017))]

glimpse(allgp_2017, 10)

glimpse(allgp_2017$HomeScore)

#Data Cleaning

allgp_2017$HomeScore <- as.numeric(allgp_2017$HomeScore)
allgp_2017$AwayScore <- as.numeric(allgp_2017$AwayScore)

#run.dif will be positive if home team is winning (will need to check what team the player is)
allgp_2017 <- allgp_2017 %>% mutate(Pitch_Count = nchar(allgp_2017$Sequence))
allgp_2017 <- allgp_2017 %>% mutate(Run.Diff = (allgp_2017$HomeScore - allgp_2017$AwayScore))
allgp_2017$Run.Diff


(allgp_2017$Pitch_Count)
glimpse(allgp_2017$AwayScore)

gb <- allgp_2017 %>% group_by(allgp_2017$Run.Diff, allgp_2017$Pitch_Count)


#I want the average pitch count for each different run diff.

#plot(allgp_2017$Run.Diff, allgp_2017$Pitch_Count)

#leveraged index 

data(allgp_2017, package="ggplot2")

glimpse(gb$Run.Diff)
glimpse(gb$Pitch_Count)


# joining data
mstr <- allgp_2017 %>% full_join(master_id, by = c("Batter" = "retro_id")) %>% full_join(fangraphs_data, by = c("retro_name" = "Player"))


z <- ggplot(mstr, aes(abs(mstr$Run.Diff), mstr$Pitch_Count))
z +   geom_jitter() + geom_smooth(method=lm) + scale_y_continuous(breaks=seq(0,15,1)) + scale_x_continuous(breaks=seq(1,10,1), limits = c(1, 10)) +
  labs(title="Pitch Count vs. Abs(Run Differential) 1" )



# Getting Elite Batters
mstr <- mstr %>% filter(mstr$wOBA > 0.250)

str(mstr$Pitch_Count)

avg <- mean(mstr$Pitch_Count, na.rm = TRUE)
avg # average pitch count per PA in 2018

up_5 <- mstr %>% subset(mstr$Run.Diff == 5)

up_1_first <- mstr %>% subset(mstr$Run.Diff == 1 & mstr$Inning == c(1,2,3))

up_1_last <- mstr %>% subset(mstr$Run.Diff == 1 & mstr$Inning == c(7,8,9))

down_1_first <- mstr %>% subset(mstr$Run.Diff < 0 & mstr$Inning == c(1,2,3))

down_1_last <- mstr %>% subset(mstr$Run.Diff < 0 & mstr$Inning == c(7,8,9))

down_5 <- mstr %>% subset(mstr$Run.Diff == -5)

glimpse(mstr)



#elite guys -> Run scenarios 



box_w <- mstr %>% mutate(time_in_game = ifelse(mstr$Run.Diff < -5 & mstr$Inning == c(1,2,3), "Down Runs Early",
                                                    ifelse(mstr$Run.Diff < -5 & mstr$Inning == c(7,8,9), "Down Runs Late",
                                                           ifelse(mstr$Run.Diff > 5 & mstr$Inning == c(1,2,3), "Up Runs Early",
                                                                  ifelse(mstr$Run.Diff > 5 & mstr$Inning == c(7,8,9), "Up Runs Late", "Other"))))) %>%  drop_na(time_in_game)


p <- ggplot(box_w, aes(box_w$time_in_game, box_w$Pitch_Count, fill = box_w$time_in_game), na.rm = TRUE)
p + geom_violin() + geom_boxplot(width=0.1, fill="#FFFFCC") + xlab("Game Situation") + ylab("Pitch Count") + scale_fill_brewer(palette="Set1") + theme(plot.title = element_text(face='bold',size=18,hjust=0.5),
                                                                                                                                                              axis.title.x = element_text(face='bold',size=16,hjust=0.5),
                                                                                                                                                               axis.title.y = element_text(face='bold',size=16,vjust=1),
                                                                                                                                                               axis.text.x = element_text(size=13,color='black'),
                                                                                                                                                               axis.text.y = element_text(size=12,color='black')) +
  theme(legend.position="none") +  scale_y_continuous(breaks=seq(0,24,2), limits = c(1, 24)) + labs(title="Pitch Counts in Different Game Scenarios among 2017 wOBA Leaders")


# everybody else
box_a <- allgp_2017 %>% mutate(time_in_game = ifelse(allgp_2017$Run.Diff < -5 & allgp_2017$Inning == c(1,2,3), "Down Runs Early",
                                              ifelse(allgp_2017$Run.Diff < -5 & allgp_2017$Inning == c(7,8,9), "Down Runs Late",
                                                     ifelse(allgp_2017$Run.Diff > 5 & allgp_2017$Inning == c(1,2,3), "Up Runs Early",
                                                            ifelse(allgp_2017$Run.Diff > 5 & allgp_2017$Inning == c(7,8,9), "Up Runs Late", "Other"))))) %>%  drop_na(time_in_game)

p <- ggplot(box_a, aes(box_a$time_in_game, box_a$Pitch_Count, fill = box_a$time_in_game), na.rm = TRUE)
p + geom_violin() + geom_boxplot(width=0.1, fill="#FFFFCC") + xlab("Game Situation") + ylab("Pitch Count") + scale_fill_brewer(palette="Set2") + theme(plot.title = element_text(face='bold',size=18,hjust=0.5),
                                                                                                                                                       axis.title.x = element_text(face='bold',size=16,hjust=0.5),
                                                                                                                                                       axis.title.y = element_text(face='bold',size=16,vjust=1),
                                                                                                                                                       axis.text.x = element_text(size=13,color='black'),
                                                                                                                                                       axis.text.y = element_text(size=12,color='black')) +
  theme(legend.position="none") +  scale_y_continuous(breaks=seq(0,24,2), limits = c(1, 24)) + labs(title="Pitch Counts in Different Game Scenarios among all 2017 Players")


# Statistical Significance analysis 
summary(box_a$Pitch_Count)
t.test(box_a$Pitch_Count)

summary(box_w$Pitch_Count)
t.test(box_w$Pitch_Count, box_a$Pitch_Count)


# CLOSE GAME VS. Plate appearence 
# When you are a homerun away from tie or lead, do you take less pitches later in games 

#down 1, as game goes on
#down 2 man on second



#everyone else -> different scenarios where a homerun ties or takes lead in game
allgp_2017 <- allgp_2017 %>% mutate(men_on_base = ifelse((!stri_isempty(allgp_2017$Runner1) & stri_isempty(allgp_2017$Runner2) & stri_isempty(allgp_2017$Runner3)) | 
                                                          (stri_isempty(allgp_2017$Runner1) & !stri_isempty(allgp_2017$Runner2) & stri_isempty(allgp_2017$Runner3)) |
                                                          (stri_isempty(allgp_2017$Runner1) & stri_isempty(allgp_2017$Runner2) & !stri_isempty(allgp_2017$Runner3)), 1,
                                                          ifelse((!stri_isempty(allgp_2017$Runner1) & !stri_isempty(allgp_2017$Runner2) & stri_isempty(allgp_2017$Runner3)) |
                                                                   (!stri_isempty(allgp_2017$Runner1) & stri_isempty(allgp_2017$Runner2) & !stri_isempty(allgp_2017$Runner3)) |
                                                                   (stri_isempty(allgp_2017$Runner1) & !stri_isempty(allgp_2017$Runner2) & !stri_isempty(allgp_2017$Runner3)), 2,
                                                                 ifelse((!stri_isempty(allgp_2017$Runner1) & !stri_isempty(allgp_2017$Runner2) & !stri_isempty(allgp_2017$Runner3)), 3, 0))))


bigimp_e <- allgp_2017 %>% subset((abs(allgp_2017$Run.Diff) == 1 & allgp_2017$men_on_base >= 0) | (abs(allgp_2017$Run.Diff) == 2 & allgp_2017$men_on_base >= 1) | 
                                      (abs(allgp_2017$Run.Diff) == 3 & allgp_2017$men_on_base >= 2) | (abs(allgp_2017$Run.Diff) == 4 & allgp_2017$men_on_base == 3))

m <- ggplot(bigimp_e, aes(as.factor(bigimp_e$Inning), bigimp_e$Pitch_Count, fill = as.factor(bigimp_e$Inning)), na.rm = TRUE)
m + geom_violin() + geom_boxplot(width=0.1, fill="#FFFFCC") + xlab("Inning") + ylab("Pitch Count") + scale_y_continuous(breaks=seq(0,20,1)) + theme(plot.title = element_text(face='bold',size=18,hjust=0.5),
                                                                                                                                                    axis.title.x = element_text(face='bold',size=16,hjust=0.5),
                                                                                                                                                    axis.title.y = element_text(face='bold',size=16,vjust=1),
                                                                                                                                                    axis.text.x = element_text(size=13,color='black'),
                                                                                                                                                    axis.text.y = element_text(size=12,color='black')) +
  labs(title="Pitch Count when a Homerun ties or gives lead vs. Inning(among all 2017 Players)") + xlab("Inning") + ylab("Pitch Count") +  theme(legend.position="none")


#Stat Analysis
summary(bigimp_w$Inning, bigimp_w$Pitch_Count)
t.test(bigimp_e$Inning, bigimp_e$Pitch_Count)
cor(bigimp_e$Inning, bigimp_e$Pitch_Count)

#ELITE PLAYERS
mstr <- mstr %>% mutate(men_on_base = ifelse((!stri_isempty(mstr$Runner1) & stri_isempty(mstr$Runner2) & stri_isempty(mstr$Runner3)) | 
                                                           (stri_isempty(mstr$Runner1) & !stri_isempty(mstr$Runner2) & stri_isempty(mstr$Runner3)) |
                                                           (stri_isempty(mstr$Runner1) & stri_isempty(mstr$Runner2) & !stri_isempty(mstr$Runner3)), 1,
                                                         ifelse((!stri_isempty(mstr$Runner1) & !stri_isempty(mstr$Runner2) & stri_isempty(mstr$Runner3)) |
                                                                  (!stri_isempty(mstr$Runner1) & stri_isempty(mstr$Runner2) & !stri_isempty(mstr$Runner3)) |
                                                                  (stri_isempty(mstr$Runner1) & !stri_isempty(mstr$Runner2) & !stri_isempty(mstr$Runner3)), 2,
                                                                ifelse((!stri_isempty(mstr$Runner1) & !stri_isempty(mstr$Runner2) & !stri_isempty(mstr$Runner3)), 3, 0))))


bigimp_w <- mstr %>% subset((abs(mstr$Run.Diff) == 1 & mstr$men_on_base >= 0) | (abs(mstr$Run.Diff) == 2 & mstr$men_on_base >= 1) | 
                                       (abs(mstr$Run.Diff) == 3 & mstr$men_on_base >= 2) | (abs(mstr$Run.Diff) == 4 & mstr$men_on_base == 3))

d <- ggplot(bigimp_w, aes(as.factor(bigimp_w$Inning), bigimp_w$Pitch_Count, fill = as.factor(bigimp_w$Inning)), na.rm = TRUE)
d + geom_violin() + geom_boxplot(width=0.1, fill="#FFFFCC") + xlab("Inning") + ylab("Pitch Count") + scale_y_continuous(breaks=seq(0,20,1)) + theme(plot.title = element_text(face='bold',size=18,hjust=0.5),
                                                                                                                                                    axis.title.x = element_text(face='bold',size=16,hjust=0.5),
                                                                                                                                                    axis.title.y = element_text(face='bold',size=16,vjust=1),
                                                                                                                                                    axis.text.x = element_text(size=13,color='black'),
                                                                                                                                                    axis.text.y = element_text(size=12,color='black')) +
  labs(title="Pitch Count when a Homerun ties or gives lead vs. Inning(among all 2017 wOBA Leaders)") + xlab("Inning") + ylab("Pitch Count") +  theme(legend.position="none")



#Stat Analysis
t.test(bigimp_w$Pitch_Count, bigimp_e$Pitch_Count)

summary(bigimp_w$Inning, bigimp_w$Pitch_Count)
t.test(bigimp_w$Inning, bigimp_w$Pitch_Count)
cor(bigimp_w$Inning, bigimp_w$Pitch_Count)


################  More you are down or up, less pitches you take (let me go home already...) #####
x <- ggplot(box_a, aes(as.factor(abs(box_a$Run.Diff)), box_a$Pitch_Count, fill = as.factor(abs(box_a$Run.Diff))), na.rm = TRUE)
x + geom_violin() + geom_boxplot(width=0.1, fill="#FFFFCC") + xlab("Abs(Run Differential)") + ylab("Pitch Count") + scale_y_continuous(breaks=seq(0,26,1)) + theme(plot.title = element_text(face='bold',size=18,hjust=0.5),
                                                                                                                                                                   axis.title.x = element_text(face='bold',size=16,hjust=0.5),
                                                                                                                                                                   axis.title.y = element_text(face='bold',size=16,vjust=1),
                                                                                                                                                                   axis.text.x = element_text(size=13,color='black'),
                                                                                                                                                                   axis.text.y = element_text(size=12,color='black')) +
  labs(title="Pitch Count vs. Abs(Run Differential) (among all 2017 Players)") + xlab("Abs(Run Differential)") + ylab("Pitch Count") +  theme(legend.position="none")

#Stat Analysis
summary(abs(box_a$Run.Diff), box_a$Pitch_Count)
t.test(abs(box_a$Run.Diff), box_a$Pitch_Count)
cor(abs(box_a$Run.Diff), box_a$Pitch_Count)


#ELITE 
x <- ggplot(box_w, aes(as.factor(abs(box_w$Run.Diff)), box_w$Pitch_Count, fill = as.factor(abs(box_w$Run.Diff))), na.rm = TRUE)
x + geom_violin() + geom_boxplot(width=0.1, fill="#FFFFCC") + xlab("Abs(Run Differential)") + ylab("Pitch Count") + scale_y_continuous(breaks=seq(0,26,1)) + theme(plot.title = element_text(face='bold',size=18,hjust=0.5),
                                                                                                                                                                   axis.title.x = element_text(face='bold',size=16,hjust=0.5),
                                                                                                                                                                   axis.title.y = element_text(face='bold',size=16,vjust=1),
                                                                                                                                                                   axis.text.x = element_text(size=13,color='black'),
                                                                                                                                                                   axis.text.y = element_text(size=12,color='black')) +
  labs(title="Pitch Count vs. Abs(Run Differential) (among all 2017 wOBA Leaders)") + xlab("Abs(Run Differential)") + ylab("Pitch Count") +  theme(legend.position="none")

#Stat Analysis
summary(abs(box_w$Run.Diff), box_w$Pitch_Count)
t.test(abs(box_a$Pitch_Count), box_w$Pitch_Count)
#cor(box_a$Pitch_Count, box_w$Pitch_Count)








##))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

