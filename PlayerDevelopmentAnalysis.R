######clear your working directory
rm(list=ls())

#####Installing packages and libraries
install.packages("lubridate")
install.packages("baseballDBR")
install.packages("gtsummary")
library(baseballDBR)
library(lubridate)
library(ggplot2)
library(sandwich)
library(readr)
library(readxl)
library(tidyr)
library(gtsummary)



##Import baseball data sets for Batting, Pitching, and Fielding
get_bbdb(table = c("Batting", "Pitching", "Fielding"))

##Import historical player data
playerHistory <- read_excel("Players.xlsx")

##Import data on minor league batters from 2006 to 2012
minorLeagueBatter2006to2012 <- read_excel("2006-2012_Minor_League_Batters.xlsx")

##Import data on minor league pitchers from 2006 to 2012
minorLeaguePitchers2006to2012 <- read_excel("2006-2012_Minor_League_Pitchers.xlsx") 

##Import opening day payroll information for 2022 season
openingDayPayroll2022 <- read_excel("MLB_Opening_Day_Payroll_2022.xlsx")

##Import  list of players called up in 2022
playersCalledUp2022 <- read_excel("Players_Called_Up_2022.xlsx")



############### Data Management on Batting Data Set ###############
############### Data Management on Batting Data Set ###############
############### Data Management on Batting Data Set ###############


##Add weighted runs above average statistic as new column in data set
Batting$wRAA <- wRAA(Batting, Pitching, Fielding, Fangraphs=FALSE,
                     NA_to_zero=TRUE, Sep.Leagues=FALSE)


##Merge playerHistory and Batting so stats can be searched by player name
##instead of player ID
playerBattingStats <- merge(playerHistory, Batting, by = "playerID")


##Creat new column in playerBattingStats called FullName which combines
##nameFirst and nameLast to simplify analysis on dataset
playerBattingStats$FullName <- paste(playerBattingStats$nameFirst, 
                                        playerBattingStats$nameLast)


##Combine birthYear, birthMonth, and birthDay to create BirthDate 
playerBattingStats$BirthDate <- paste(playerBattingStats$birthYear,
                                      playerBattingStats$birthMonth,
                                      playerBattingStats$birthDay, sep = "-")

##Convert BirthDate to date format
playerBattingStats$BirthDate <- as.Date(playerBattingStats$BirthDate, 
                                          format = "%Y-%m-%d")

##Calculate age at which players debuted in the Majors
playerBattingStats$debutAge <- lubridate::time_length(difftime
         (playerBattingStats$debut, playerBattingStats$BirthDate), "years")


##Calculate number of singles. Needed to calculate slugging
playerBattingStats$X1B <- as.numeric(playerBattingStats$H-
  (playerBattingStats$X2B+playerBattingStats$X3B+playerBattingStats$HR))


##Calculate slugging, needed for OPS
playerBattingStats$SLG <- (playerBattingStats$X1B+ 
                2*playerBattingStats$X2B + 3*playerBattingStats$X3B + 
                4*playerBattingStats$HR)/ playerBattingStats$AB


##Calculate on base percentage, needed for OPS
playerBattingStats$OBP <- ((playerBattingStats$H+playerBattingStats$BB+
                                                   playerBattingStats$HBP)/
                         (playerBattingStats$AB + playerBattingStats$BB + 
                         playerBattingStats$HBP + playerBattingStats$SF))


##Calculate On base plus slugging (OPS)
playerBattingStats$OPS <- playerBattingStats$OBP+playerBattingStats$SLG


##Calculate weighted on base average (wOBA)
playerBattingStats$wOBA <- (0.690*playerBattingStats$BB + 
              0.722*playerBattingStats$HBP + 0.888*playerBattingStats$X1B + 
              1.271*playerBattingStats$X2B + 1.616*playerBattingStats$X3B +
                                               2.101*playerBattingStats$HR) / 
(playerBattingStats$AB + (playerBattingStats$BB - playerBattingStats$IBB) 
                          + playerBattingStats$SF + playerBattingStats$HBP)


##Sort data frame by yearID in descending order.
playerBattingStats <-playerBattingStats[order(- playerBattingStats$yearID),]



##Change finalGame and debut to date objects so difftime function
##can be run on them to get career length of players 
playerBattingStats$finalGame <- as.Date(playerBattingStats$finalGame, 
                                                     format = "%Y-%m-%d")
playerBattingStats$debut <- as.Date(playerBattingStats$debut, 
                                                     format = "%Y-%m-%d")


##Calculate career length and round
CareerLength <- lubridate::time_length(difftime(playerBattingStats$finalGame, 
                                          playerBattingStats$debut), "years")
CareerLength <-round(CareerLength, 1)


##Append CareerLength vector to playerBattingStats dataset
playerBattingStats <- cbind(playerBattingStats, CareerLength)


##Set indicator if player had an above average OPS (on base plus slugging)
## 
##
##"Average" obtained from Fangraphs  
playerBattingStats$aboveAverageOPS <- (ifelse(playerBattingStats$OPS 
                                                 > .710, 1, 0))


##Set indicator if player had an above average wOBA
## 
##
##"Average" obtained from Fangraphs
playerBattingStats$aboveAveragewOBA <- (ifelse(playerBattingStats$wOBA 
                                               > .320, 1, 0))

##Set indicator if player had an above average wRAA
## 
##
##"Average" obtained from Fangraphs
playerBattingStats$aboveAveragewRAA <- (ifelse(playerBattingStats$wRAA 
                                               > 0, 1, 0))

##Create data set that contains Major League stats for players that exist in 
##both playerBattingStats and minorLeagueBatter2006to2012 
PromotedPlayers2006to2012 <- playerBattingStats[playerBattingStats$FullName 
                                 %in% c(minorLeagueBatter2006to2012$FullName),]


##Create data set that contains minor league stats for players that also 
##exist in both playerBattingStats and minorLeagueBatter2006to2012  
PromotedPlayersMiLBstats <- minorLeagueBatter2006to2012[minorLeagueBatter2006to2012$
                              FullName %in% c(playerBattingStats$FullName), ]



##Combine Minor League and Major League stats for Batters
MinorAndMajorLeagueBatterStats <- merge(PromotedPlayers2006to2012, 
              PromotedPlayersMiLBstats, by="FullName", all.x=TRUE, all.y=FALSE)



##Get count of years players were above average in wRAA, wOBA, and OPS
BattersYrsAboveAVG <- aggregate(cbind(aboveAveragewRAA, aboveAveragewOBA, 
 aboveAverageOPS) ~ FullName, data = MinorAndMajorLeagueBatterStats, FUN = sum)
  

##Change column names to avoid duplicates when merging datasets 
colnames(BattersYrsAboveAVG) <- c('FullName','yrswRAAaboveAvg',
                                  'yrswOBAboveAVG','yrsOPSaboveAVG')


##Merge MinorAndMajorLeagueBatterStats and BattersYrsAboveAVG
MinorAndMajorLeagueBatterStats <- merge(MinorAndMajorLeagueBatterStats, 
                    BattersYrsAboveAVG, by="FullName", all.x=TRUE, all.y=FALSE)


##Convert specified major league stats from int to num 
MinorAndMajorLeagueBatterStats[30:46] = 
lapply(MinorAndMajorLeagueBatterStats[30:46], FUN = function(y){as.numeric(y)})



############### Data Management on Pitching Data Sets ###############
############### Data Management on Pitching Data Sets ###############
############### Data Management on Pitching Data Sets ###############



##Merge playerHistory and Pitching
playerPitchingStats <- merge(playerHistory, Pitching, by = "playerID")


##Create a new column in playerPitchingStats called FullName 
playerPitchingStats$FullName <- paste(playerPitchingStats$nameFirst, 
                                       playerPitchingStats$nameLast)


##Combine birthYear, birthMonth, and birthDay to BirthDate 
playerPitchingStats$BirthDate <- paste(playerPitchingStats$birthYear,
                                     playerPitchingStats$birthMonth,
                                     playerPitchingStats$birthDay, sep = "-")


##Change BirthDate to date format
playerPitchingStats$BirthDate <- as.Date(playerPitchingStats$BirthDate, 
                                                      format = "%Y-%m-%d")

##Calculate age at which players debuted in the Majors
playerPitchingStats$debutAge <- lubridate::time_length(difftime
        (playerPitchingStats$debut, playerPitchingStats$BirthDate), "years")


##Create new column called FIP (fielding independent pitching).
##Measures pitchers performance independent of the defense around them. 
playerPitchingStats$FIP <- FIP(Pitching, Fangraphs=FALSE, NA_to_zero=FALSE, 
                                                           Sep.Leagues=FALSE)


##Calculate the number of innings pitched, needed to calculate WHIP.
##
##IPouts is the number of outs pitched, divide by 3 to get innings pitched 
playerPitchingStats$IP <- playerPitchingStats$IPouts/3



##Calculate WHIP (walks plus hits per innings pitched). 
playerPitchingStats$WHIP <- (playerPitchingStats$BB+ 
                               playerPitchingStats$H)/playerPitchingStats$IP


##Sorting data frame by yearID in descending order.
playerPitchingStats <-playerPitchingStats[order(- playerPitchingStats$yearID),]


##Change finalGame and debut to date objects so difftime function
##can be run to get career length of players 
playerPitchingStats$finalGame <- as.Date(playerPitchingStats$finalGame, 
                                          format = "%Y-%m-%d")


playerPitchingStats$debut <- as.Date(playerPitchingStats$debut, 
                                      format = "%Y-%m-%d")


##Calculate career length and round
PitcherCareerLength <- lubridate::time_length(difftime(playerPitchingStats$finalGame, 
                                          playerPitchingStats$debut), "years")

PitcherCareerLength <-round(PitcherCareerLength, 1)

##Append PitcherCareerLength vector to playerPitchingStats dataset
playerPitchingStats <- cbind(playerPitchingStats, PitcherCareerLength)


##Set indicator if player had an above average ERA (earned run average)
##in a year. "Average" determined by Fangraphs chart. 
playerPitchingStats$aboveAverageERA <- ifelse(playerPitchingStats$ERA 
                                                < 3.75, 1, 0)

##Set indicator if player had an above average FIP in a year.
playerPitchingStats$aboveAverageFIP <- ifelse(playerPitchingStats$FIP 
                                                < 4.20, 1, 0)

##Set indicator if player had an above average WHIP in a year. 
playerPitchingStats$aboveAverageWHIP <- ifelse(playerPitchingStats$WHIP 
                                                < 1.30, 1, 0)

##Get a count of NA values in aboveAverageERA
sum(is.na(playerPitchingStats$aboveAverageERA))


##Create data set that contains Major League stats for players that exist in 
##both playerPitchingStats and minorLeaguePitchers2006to2012 
PromotedPitchers2006to2012 <- playerPitchingStats[playerPitchingStats$FullName 
                              %in% c(minorLeaguePitchers2006to2012$FullName),]


##Create data set that contains minor league stats for players that also 
##exist in both playerPitchingStats and minorLeaguePitchers2006to2012  
PromotedPitchersMiLBstats <- minorLeaguePitchers2006to2012[minorLeaguePitchers2006to2012$FullName 
                                    %in% c(playerPitchingStats$FullName), ]


##Combine Minor League and Major League stats for Pitchers
MinorAndMajorLeaguePitchersStats <- merge(PromotedPitchers2006to2012, 
   PromotedPitchersMiLBstats, by="FullName", all.x=TRUE, all.y=FALSE)


##Get count of years players were above average in ERA, FIP, and WHIP
PitchersYrsAboveAVG <- aggregate(cbind(aboveAverageERA, aboveAverageFIP, 
aboveAverageWHIP) ~ FullName, data = MinorAndMajorLeaguePitchersStats, FUN = sum)


##Change column names to avoid duplicates when merging datasets. 
colnames(PitchersYrsAboveAVG) <- c('FullName','yrsERAaboveAvg',
                                  'yrsFIPAboveAvg','yrsWHIPaboveAvg')


##Merge MinorAndMajorLeaguePitchersStats and PitchersYrsAboveAVG
MinorAndMajorLeaguePitchersStats <- merge(MinorAndMajorLeaguePitchersStats, 
                PitchersYrsAboveAVG, by="FullName", all.x=TRUE, all.y=FALSE)


 


############### Applied Probability ###############
############### Applied Probability ###############
############### Applied Probability ###############



##Calculate percent of players in the MLB who have above average wRAA
PlayersAboveAVG <- nrow(MinorAndMajorLeagueBatterStats
    [MinorAndMajorLeagueBatterStats$aboveAveragewRAA == 1, ])

percentPlayersAboveAVG <- round((PlayersAboveAVG/9786), 2)


##Calculate the probability that 1 or more out of 5 players, called up in
##a year, will have positive contribution (above average wRAA) to the team
##
##How does it change for 2 or more players
pbinom(1,5,percentPlayersAboveAVG, lower.tail = FALSE)
pbinom(2,5,percentPlayersAboveAVG, lower.tail = FALSE)


##Check for missingness for aboveAverageERA/WHIP/FIP
sum(is.na(MinorAndMajorLeaguePitchersStats$aboveAverageERA))
sum(is.na(MinorAndMajorLeaguePitchersStats$aboveAverageWHIP))
sum(is.na(MinorAndMajorLeaguePitchersStats$aboveAverageFIP))


##Calculate percent of players in league with above average ERA
PitchersWithAboveAvgERA <- nrow(MinorAndMajorLeaguePitchersStats
[MinorAndMajorLeaguePitchersStats$aboveAverageERA == 1, ])

percentPitchersWaboveAvgERA <- round((PitchersWithAboveAvgERA/8169), 2)


##Calculate percent of players in league with above average WHIP

PitchersWithAboveAvgWHIP <- nrow(MinorAndMajorLeaguePitchersStats
        [MinorAndMajorLeaguePitchersStats$aboveAverageWHIP == 1, ])

percentPitchersWaboveAvgWHIP <- round((PitchersWithAboveAvgWHIP/8169), 2)


##Calculating the probability that 1 or more out of 5 pitchers called up in
##a year will have positive contribution (above average ERA or WHIP) to the
##team
##
##How does it change for 2 or more players
pbinom(1,5,percentPitchersWaboveAvgERA, lower.tail = FALSE)
pbinom(2,5,percentPitchersWaboveAvgERA, lower.tail = FALSE)



############### Statistical Inference ###############
############### Statistical Inference ###############
############### Statistical Inference ###############



##Check for normal distribution of wRAA
hist(MinorAndMajorLeagueBatterStats$wRAA, main ="wRAA Distribution", 
     xlab = "wRAA")


##Check how many observations will be left out when removing NA values
sum(is.na(MinorAndMajorLeagueBatterStats$wRAA))


##Find mean and standard deviation of wRAA (weighted runs above average)
sd(MinorAndMajorLeagueBatterStats$wRAA, na.rm = TRUE)
mean(MinorAndMajorLeagueBatterStats$wRAA, na.rm = TRUE)


##Find probability a player will have wRAA above various thresholds 
##0:Average, 10:Above Average, and 20:Great
pnorm(0, mean = -0.926944, sd = 9.483237, lower.tail = FALSE)
pnorm(10, mean = -0.926944, sd = 9.483237, lower.tail = FALSE)
pnorm(20, mean = -0.926944, sd = 9.483237, lower.tail = FALSE)




##Distribution graph with prbobaility of wRAA above 10
wRAAx <- seq(-5, 8, length = 100) * 9.483237 - (-0.926944) 
wRAAf <- dnorm(wRAAx, -0.926944, 9.483237)

wRAAlb <- 10 # Lower bound
wRAAub <- max(MinorAndMajorLeagueBatterStats$wRAA, na.rm = TRUE)   # Upper bound

x2 <- seq(wRAAlb, wRAAub, length = 100) # New Grid
y <- dnorm(x2, -0.926944, 9.483237) # Density

plot(wRAAx, wRAAf, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "wRAA",
     ylim = c(0, .05), main = "Probability Position Player
     Will Have wRAA above 10")

     segments(wRAAlb, 0, wRAAlb, y)

polygon(c(wRAAlb, x2, wRAAub), c(0, y, 0), col = rgb(0, 0, 1, alpha = 0.5))

text(21, 0.01, "13%")



##Add variable for minor league years played by dividing minor league games
##by 140 (average games played per year by the various minor league levels)
MinorAndMajorLeagueBatterStats$mnrYrs <- 
(MinorAndMajorLeagueBatterStats$mnrG/140)


##Check the range of data is for mnrYrs 
summary(MinorAndMajorLeagueBatterStats$mnrYrs)


##Create subsets of data for players who spent less than 2.25 years in the
##minors and those who spent greater than or equal to 2.25 years. 
GrtrThn2.25yrs <- subset(MinorAndMajorLeagueBatterStats, mnrYrs >= 2.25)
LessThn2.25yrs <- subset(MinorAndMajorLeagueBatterStats, mnrYrs < 2.25)


##Check distribution of data
boxplot(GrtrThn2.25yrs$CareerLength)
boxplot(LessThn2.25yrs$CareerLength)



##Ho: avg career length of players who play greater than or equal to
##2.25 years in the minors is equal to avg career length of players who 
##play less than 2.25 years in the minors
##
##Ha:avg career length of players who play greater than or equal to
##2.25 years in the minors is not equal to avg career length of players who 
##play less than 2.25 years in the minors

t.test(GrtrThn2.25yrs$CareerLength, LessThn2.25yrs$CareerLength, 
                                              conf.level = .95)


##Ho: avg wRAA of players who play greater than or equal to
##2.25 years in the minors is equal to avg wRAA of players who 
##play less than 2.25 years in the minors
##
##Ha:avg wRAA of players who play greater than or equal to
##2.25 years in the minors is not equal to avg wRAA of players who 
##play less than 2.25 years in the minors

t.test(GrtrThn2.25yrs$wRAA, LessThn2.25yrs$wRAA, 
                               conf.level = .95)




##Check how many observations will be left out when removing NA values
sum(is.na(MinorAndMajorLeaguePitchersStats$ERA))


##Find mean and standard deviation of wRAA (weighted runs above average)
sd(MinorAndMajorLeaguePitchersStats$ERA, na.rm = TRUE)
mean(MinorAndMajorLeaguePitchersStats$ERA, na.rm = TRUE)


##Find probability that player will have ERA below various thresholds 
##3.75:Average, 3.40:Above Average, and 3.00:Great
pnorm(3.75, mean = 5.578657, sd = 6.342064, lower.tail = TRUE)
pnorm(3.40, mean = 5.578657, sd = 6.342064, lower.tail = TRUE)
pnorm(3.00, mean = 5.578657, sd = 6.342064, lower.tail = TRUE)



##Distribution graph with prbobaility of ERA above 10
ERAx <- seq(0, 7, length = 100) * 6.342064 - 5.578657 
ERAf <- dnorm(ERAx, 5.578657, 6.342064)

ERAlb <- 0 # Lower bound
ERAub <- 3.40   # Upper bound

ERAx2 <- seq(ERAlb, ERAub, length = 100) # New Grid
ERAy <- dnorm(ERAx2, 5.578657, 6.342064) # Density

plot(ERAx, ERAf, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "ERA",
     ylim = c(0, .07), main = "Probability Pitcher
     Will Have ERA below 3.40")

segments(ERAub, 0, ERAub, ERAy)

polygon(c(-8, ERAx2, ERAub), c(0, ERAy, 0), col = rgb(0, 0, 1, alpha = 0.5))

text(-2, 0.01, "37%")


##Check range of data for mnrIP
summary(MinorAndMajorLeaguePitchersStats$mnrIP)


##Create subsets of data for players who spent less than 2.25 years in the
##minors and those who spent greater than or equal to 2.25 years. 
GrtrThn269IP <- subset(MinorAndMajorLeaguePitchersStats, mnrIP >= 269)
LessThn269IP <- subset(MinorAndMajorLeaguePitchersStats, mnrIP < 269)



##Ho: avg Major League Innings pitched of pitchers who pitch greater than 
##or equal to 269 innings in the minors is equal to avg Major League Innings 
##pitched of pitchers who  pitch less than 269 innings in the minors
##
##Ha: avg Major League Innings pitched of pitchers who pitch greater than 
##or equal to 269 innings in the minors is not equal to avg Major League Innings 
##pitched of pitchers who  pitch less than 269 innings in the minors

t.test(GrtrThn269IP$IP, LessThn269IP$IP, 
       conf.level = .95)


##Ho: avg ERA of pitchers who pitch greater than or equal to
##269 innings in the minors is equal to avg ERA of pitchers who 
##pitch less than 269 innings in the minors
##
##Ha: avg ERA of pitchers who pitch greater than or equal to
##269 innings in the minors is not equal to avg ERA of pitchers who 
##pitch less than 269 innings in the minors

t.test(GrtrThn269IP$ERA, LessThn269IP$ERA, 
                         conf.level = .95)




############### Batters Regression Analysis ###############
############### Batters Regression Analysis ###############
############### Batters Regression Analysis ###############




##Check if wRAA data is normalized for linear regression 
hist(MinorAndMajorLeagueBatterStats$wRAA, main ="wRAA Distribution", 
     xlab = "wRAA")



##Turn off scientific notation
options(scipen = 999)

##Linear Model 1
linearModel1 <- lm(yrswRAAaboveAvg ~ mnrG + mnrPA +
                mnrH + mnr1B + mnr2B + mnr3B + mnrR + mnrRBI + mnrBB +
                  mnrSO + mnrSB + mnrAVG, data=MinorAndMajorLeagueBatterStats)
summary(linearModel1)


##Linear Model 2
linearModel2 <- lm(yrswRAAaboveAvg ~ debutAge + mnrG + mnrPA, 
                   data=MinorAndMajorLeagueBatterStats)
summary(linearModel2)


##Linear Model 3
linearModel3 <- lm(yrswRAAaboveAvg ~ debutAge + mnrG + mnrH + mnrHR, 
                   data=MinorAndMajorLeagueBatterStats)
summary(linearModel3)

tbl_regression(linearModel3, intercept = TRUE)

##Linear Model 4
linearModel4 <- lm(yrswRAAaboveAvg ~ mnrG + mnrH + mnrHR, 
                   data=MinorAndMajorLeagueBatterStats)
summary(linearModel4)


##Linear Model 5
linearModel5 <- lm(yrswRAAaboveAvg ~ debutAge + mnrG + mnrH, 
                   data=MinorAndMajorLeagueBatterStats)
summary(linearModel5)


##Linear Model 6
linearModel6 <- lm(yrswRAAaboveAvg ~ debutAge + mnrAVG + mnrRBI, 
                   data=MinorAndMajorLeagueBatterStats)
summary(linearModel6)


##Linear Model 7
linearModel7 <- lm(yrswRAAaboveAvg ~ mnrG + mnrH + mnrG*mnrH, 
                    data=MinorAndMajorLeagueBatterStats)
summary(linearModel7)


##produce residual vs. fitted plot and add horizontal line at 0
tbl_regression(linearModel7, intercept = TRUE)

res <- resid(linearModel7)

plot(fitted(linearModel7), res)

abline(0,0)


##Linear Model 8
linearModel8 <- lm(CareerLength ~ mnrG + mnrPA, +
               mnrH + mnr1B + mnr2B + mnr3B + mnrHR + mnrR + mnrRBI + mnrBB +
                 mnrSO + mnrSB + mnrAVG, data=MinorAndMajorLeagueBatterStats)

summary(linearModel8)

##Linear Model 9
linearModel9 <- lm(CareerLength ~ debutAge + mnrG + mnrPA, 
                      data=MinorAndMajorLeagueBatterStats)

summary(linearModel9)

##Linear Model 10
linearModel10 <- lm(wRAA ~ debutAge + mnrG + mnrPA,
              data=MinorAndMajorLeagueBatterStats)
summary(linearModel10)


res <- resid(linearModel10)
#produce residual vs. fitted plot
plot(fitted(linearModel10), res)

#add a horizontal line at 0 
abline(0,0)



############### Pitcher Regression Analysis ###############
############### Pitcher Regression Analysis ###############
############### Pitcher Regression Analysis ###############



##Linear model 1 for pitchers
PitcherLinearModel1 <- lm(yrsERAaboveAvg ~ mnrIP + mnrBABIP + mnrWHIP + 
              mnrERA + mnrFIP, data = MinorAndMajorLeaguePitchersStats)
summary(PitcherLinearModel1)


##Linear model 2 for pitchers
PitcherLinearModel2 <- lm(yrsERAaboveAvg ~ mnrIP + mnrERA + mnrWHIP, 
                            data = MinorAndMajorLeaguePitchersStats)
summary(PitcherLinearModel2)


##Linear model 3 for pitchers
PitcherLinearModel3 <- lm(yrsERAaboveAvg ~ mnrIP + mnrERA*mnrWHIP, 
                          data = MinorAndMajorLeaguePitchersStats)
summary(PitcherLinearModel3)


##Linear model 4 for pitchers
PitcherLinearModel4 <- lm(yrsWHIPaboveAvg ~ mnrIP + mnrBABIP + mnrWHIP +
             mnrERA + mnrFIP, data = MinorAndMajorLeaguePitchersStats)

summary(PitcherLinearModel4)



##Turn scientific notation back on
options(scipen = 0)



############### Data Visualizations ###############
############### Data Visualizations ###############
############### Data Visualizations ###############



##Separate players called up into pitchers and position players
pitchersCalledUp <- subset(playersCalledUp2022, Pos == "1" | Pos == "/1")

positionPlayersCalledUp <- subset(playersCalledUp2022, Pos != "1" & Pos !="/1")


##Calculate the average number of pitchers and position players called 
##per team in a year
pitchersCalledUp$avgPerTeam <- nrow(pitchersCalledUp)/30

positionPlayersCalledUp$avgPerTeam <- nrow(pitchersCalledUp)/30



##Number of position players called up by team
ggplot(positionPlayersCalledUp, aes(x=reorder(Tm, Tm, function(x)-length(x)))) +
  geom_bar(fill='lightblue') +  
  geom_hline(yintercept = positionPlayersCalledUp$avgPerTeam)+
  labs(title = "Number of Position Players \n Called Up By team", x="Team")+
  scale_y_continuous(breaks = seq(1, 10, by = 2))



##Number of pitchers called up by team
ggplot(pitchersCalledUp, aes(x=reorder(Tm, Tm, function(x)-length(x)))) +
  geom_bar(fill='lightblue') + 
  geom_hline(yintercept = pitchersCalledUp$avgPerTeam)+
  labs(title = "Number of Pitchers \n Called Up By team", x="Team")+
  scale_y_continuous(breaks = seq(1, 10, by = 2))



##Average career length of position player
ggplot(MinorAndMajorLeagueBatterStats, aes(x = CareerLength, y = factor(0),
                                           na.rm=TRUE,)) + 
  labs(title = "Average Career Length\n of Position Player", 
       x = "Career Length in Years", y=element_blank()) +
  geom_boxplot(fill = "light blue") + 
  theme_classic() +
  scale_x_continuous(breaks = seq(1, 20, by = 2))



##Average career length of pitchers 
ggplot(MinorAndMajorLeaguePitchersStats, aes(x = PitcherCareerLength, 
                                             y = factor(0), na.rm=TRUE,)) + 
  labs(title = "Average Career Length\n of Pitchers", 
       x = "Career Length in Years", y=element_blank()) +
  geom_boxplot(fill = "light blue") + 
  theme_classic() +
  scale_x_continuous(breaks = seq(1, 20, by = 2))



##Number of years players produced above average in wRAA
ggplot(MinorAndMajorLeagueBatterStats, aes(x=yrswRAAaboveAvg)) + 
  labs(title = "Number of Years Players\n Produced Above Average wRAA", 
       x = "Years wRAA Above Average", y = "Number of Players") +
  geom_histogram(binwidth = .5, fill="lightblue", color="black") +
  geom_vline(aes(xintercept=mean(yrswRAAaboveAvg, na.rm=TRUE)),
             color="red", linetype="dashed", size=1) +
  annotate("text", x=3, y=1500, label="Mean", angle=90) +
  scale_x_continuous(breaks = seq(1, 20, by = 2))



##Number of years pitchers produced above average in ERA
ggplot(MinorAndMajorLeaguePitchersStats, aes(x=yrsERAaboveAvg)) + 
  labs(title = "Number of Years Pitchers\n Produced Above Average ERA", 
       x = "Years ERA Above Average", y = "Number of Players") +
  geom_histogram(binwidth = .5, fill="lightblue", color="black") +
  geom_vline(aes(xintercept=mean(yrsERAaboveAvg, na.rm=TRUE)),
             color="red", linetype="dashed", size=1) +
  annotate("text", x=3, y=850, label="Mean", angle=90) +
  scale_x_continuous(breaks = seq(1, 20, by = 2))



##Average position player career length by debut age
ggplot(MinorAndMajorLeagueBatterStats, aes(cut(debutAge, seq(17,34,4)), 
                                           CareerLength), na.rm=TRUE,) + 
  labs(title = "Average Position Player Career Length\n By Debut Age", 
       x = "Debut Age", y = "Career Length") +
  geom_boxplot(fill = "light blue", outlier.color = "azure 4") + 
  theme_classic()



##Average pitcher career length by debut age 
ggplot(MinorAndMajorLeaguePitchersStats, aes(cut(debutAge, seq(17,34,4)), 
                                             PitcherCareerLength), na.rm=TRUE,) + 
  labs(title = "Average Pitcher Career Length\n By Debut Age", x = "Debut Age", 
       y = "Career Length") +
  geom_boxplot(fill = "light blue", outlier.color = "azure 4") + 
  theme_classic()



##2022 Opening Day Payroll
ggplot(data = openingDayPayroll2022, aes(x = reorder(Team, Payroll),
                                         y = Payroll)) + 
  labs(title = "2022 Opening Day Payroll", x = "Team",
       y = "Payroll (in Millioins)") +
  geom_bar(stat = "identity", width=.9, fill="lightblue") +
  coord_flip( )+
  geom_text(aes(label=paste("$", (format(Payroll, nsmall=1)))), hjust=1, 
            color="black")
