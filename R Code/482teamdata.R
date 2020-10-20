cbb_clean <- read.csv("cbb_CLEAN_TeamsGroup.csv")
cbb_clean$TEAM = paste(cbb_clean$YEAR,".",cbb_clean$TEAM, sep = "")

cbb_clean$R64 = NA
cbb_clean$R32 = NA
cbb_clean$S16 = NA
cbb_clean$E8 = NA
cbb_clean$F4 = NA
cbb_clean$x2ND = NA
cbb_clean$Champions = NA

cbb_clean$POSTSEASON = factor(cbb_clean$POSTSEASON, order=TRUE, levels=c('0', 'R68', 'R64', 'R32',
                                                                         'S16', 'E8', 'F4', '2ND', 'Champions'))
 
cbb_clean$R64[which(cbb_clean$POSTSEASON >= 'R64')] = 1
cbb_clean$R32[which(cbb_clean$POSTSEASON >= 'R32')] = 1
cbb_clean$S16[which(cbb_clean$POSTSEASON >= 'S16')] = 1
cbb_clean$E8[which(cbb_clean$POSTSEASON >= 'E8')] = 1
cbb_clean$F4[which(cbb_clean$POSTSEASON >= 'F4')] = 1
cbb_clean$x2ND[which(cbb_clean$POSTSEASON >= '2ND')] = 1
cbb_clean$Champions[which(cbb_clean$POSTSEASON >= 'Champions')] = 1

write.csv(cbb_clean,"C:\\Users\\nickc\\Documents\\Senior year\\STAT 482\\cbb_clean.csv", row.names = FALSE)

conf_list <- unique(cbb_clean$CONF)
conf_barthag_means <- aggregate(cbb_clean[, 7], list(cbb_clean$CONF), mean)
names(conf_barthag_means)[1]<-"CONF"
names(conf_barthag_means)[2]<-"MEAN_BARTHAG"
conf_barthag_means <- conf_barthag_means[with(conf_barthag_means, order(-MEAN_BARTHAG)), ]
par(mfrow=c(2,1))
barplot(height = conf_barthag_means$MEAN_BARTHAG[1:10], names.arg = conf_barthag_means$CONF[1:10], ylim = 0:1, col = "green", main = "Top 10 Conferences by Mean BARTHAG", ylab = "BARTHAG")
barplot(height = conf_barthag_means$MEAN_BARTHAG[23:33], names.arg = conf_barthag_means$CONF[23:33], ylim = 0:1, col = "red", main = "Bottom 10 Conferences by Mean BARTHAG", ylab = "BARTHAG")

#====================================================================================================

cbb_clean$POST_WINS
cbb_clean$POST_WINS[is.na(cbb_clean$POST_WINS)] <- 0
cbb_clean$POST_WINS[cbb_clean$POSTSEASON == "R68"] <- 0
cbb_clean$POST_WINS[cbb_clean$POSTSEASON == "R64"] <- 0
cbb_clean$POST_WINS[cbb_clean$POSTSEASON == "R32"] <- 1
cbb_clean$POST_WINS[cbb_clean$POSTSEASON == "S16"] <- 2
cbb_clean$POST_WINS[cbb_clean$POSTSEASON == "E8"] <- 3
cbb_clean$POST_WINS[cbb_clean$POSTSEASON == "F4"] <- 4
cbb_clean$POST_WINS[cbb_clean$POSTSEASON == "2ND"] <- 5
cbb_clean$POST_WINS[cbb_clean$POSTSEASON == "Champions"] <- 6

wins_lm <- lm(cbb_clean$POST_WINS ~ cbb_clean$ADJOE + cbb_clean$ADJDE + cbb_clean$ADJDE + cbb_clean$BARTHAG + cbb_clean$EFG_O + cbb_clean$EFG_D + cbb_clean$TOR +cbb_clean$TORD + cbb_clean$ORB + cbb_clean$DRB + cbb_clean$FTR + cbb_clean$FTRD + cbb_clean$SEED)
wins_lm2 <- lm(cbb_clean$POST_WINS ~ cbb_clean$ADJOE + cbb_clean$ADJDE + cbb_clean$DRB)
summary(wins_lm)
summary(wins_lm2)
plot(wins_lm2$residuals)
