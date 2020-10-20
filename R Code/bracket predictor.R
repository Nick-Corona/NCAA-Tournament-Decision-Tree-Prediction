cbb_clean <- read.csv("cbb_clean.csv")
bracket_teams <- read.csv("cbb2020_bracket_teams.csv")
library(rpart)
tree64f = rpart(POSTSEASON ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD +
                  ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D +
                  ADJ_T, data=cbb_clean, method='class')

data64train = cbb_clean[which(cbb_clean$YEAR!=2019),]

data64 = cbb_clean[which(cbb_clean$POSTSEASON != '0' & cbb_clean$POSTSEASON != 'R68'),]

library(maptree)
draw.tree(tree64f)

predict(tree64f, bracket_teams, type = "class")


############################################################################################
#Model to predict who makes round 32, given round of 64
round64 <- read.csv("round64.csv")
round64$R32[is.na(round64$R32)] <- 0
predictR32 <- rpart(R32 ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD +
                      ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D +
                      ADJ_T, data=round64, method='class')
R32_model <- predict(predictR32, round64, type = 'class')
table(round64$R32, R32_model)


###########################################################################################
#Model to predict who makes sweet 16, given round 32
round32 <- read.csv("round32.csv")
round32$S16[is.na(round32$S16)] <- 0
predictS16 <- rpart(S16 ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD +
                      ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D +
                      ADJ_T, data=round32, method='class')
S16_model <- predict(predictS16, round32, type = 'class')
table(round32$S16, S16_model)


##########################################################################################
#Model to predict who makes elite 8, given sweet 16
sweet16 <- read.csv("sweet16.csv")
sweet16$E8[is.na(sweet16$E8)] <- 0
predictE8 <- rpart(E8 ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD +
                      ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D +
                      ADJ_T, data=sweet16, method='class')
E8_model <- predict(predictE8, sweet16, type = 'class')
table(sweet16$E8, E8_model)

#########################################################################################
#Model to predict who makes final 4, given elite 8
elite8 <- read.csv("elite8.csv")
elite8$F4[is.na(elite8$F4)] <- 0
predictF4 <- rpart(F4 ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD +
                     ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D +
                     ADJ_T, data=elite8, method='class')
F4_model <- predict(predictF4, elite8, type = 'class')
table(elite8$F4, F4_model)

########################################################################################
#Model to predict who makes championship, given final 4
final4 <- read.csv("final4.csv")
final4$x2ND[is.na(final4$x2ND)] <- 0
predictx2ND <- rpart(x2ND ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD +
                     ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D +
                     ADJ_T, data=final4, method='class')
x2ND_model <- predict(predictx2ND, final4, type = 'class')
table(final4$x2ND, x2ND_model)

########################################################################################
#Model to predict champions, given appears in championship
x2ND <- read.csv("x2ND.csv")
x2ND$Champions[is.na(x2ND$Champions)] <- 0
predictChampions <- rpart(Champions ~ ADJOE + ADJDE + EFG_O + EFG_D + TOR + TORD +
                       ORB + DRB + FTR + FTRD + X2P_O + X2P_D + X3P_O + X3P_D +
                       ADJ_T, data=x2ND, method='class')
Champions_model <- predict(predictChampions, x2ND, type = 'class')
table(x2ND$Champions, Champions_model)

########################################################################################

#Now using 2020 data to predict a bracket

#Predicting who makes round of 32, given the round of 64

predict(predictR32, bracket_teams, type = 'class')

draw.tree(predictR32)

########################################################################################

#Predicting sweet 16 based off round 32
x2020round32 <- read.csv("2020round32.csv")
predict(predictS16, x2020round32, type = 'class')
draw.tree(predictS16)

########################################################################################

#Predicting elite 8 based off sweet 16
x2020sweet16 <- read.csv("2020sweet16.csv")
predict(predictE8, x2020sweet16, type = 'class')
testmod <- predict(predictE8, x2020sweet16, type = 'class')
library(maptree)
draw.tree(predictE8)


#######################################################################################

#Predicting final 4 based off elite 8

x2020elite8 <- read.csv("2020elite8.csv")
predict(predictF4, x2020elite8, type = 'class')
draw.tree(predictF4)

#######################################################################################

#Predicting championship game based off final 4

x2020final4 <- read.csv("2020final4.csv")
predict(predictx2ND, x2020final4, type = 'class')
draw.tree(predictx2ND)

#########################################################################################

#Predicting champions based off championship game

x2020x2ND <- read.csv("2020x2ND.csv")
predict(predictChampions, x2020x2ND, type = 'class')
draw.tree(predictChampions)

