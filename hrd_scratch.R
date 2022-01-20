library(stats)
setwd("/Users/paulhatini/Desktop/Baseball")

hrd21 <- read.csv("hrd21.csv", header = TRUE)
hrd19 <- read.csv("hrd19.csv", header = TRUE)
hrd18 <- read.csv("hrd18.csv", header = TRUE)
hrd17 <- read.csv("hrd17.csv", header = TRUE)
hrd16 <- read.csv("hrd16.csv", header = TRUE)
hrd15 <- read.csv("hrd15.csv", header = TRUE)

hrd21$Year <- 2021
hrd19$Year <- 2019
hrd18$Year <- 2018
hrd17$Year <- 2017
hrd16$Year <- 2016
hrd15$Year <- 2015

'''hrd21 <- read.csv("hrd2021.csv", header = TRUE)
hrd19 <- read.csv("hrd1819.csv", header = TRUE)
hrd18 <- read.csv("hrd1718.csv", header = TRUE)
hrd17 <- read.csv("hrd1617.csv", header = TRUE)
hrd16 <- read.csv("hrd1516.csv", header = TRUE)
hrd15 <- read.csv("hrd15.csv", header = TRUE)

hrd21$Year <- 2021
hrd19$Year <- 2019
hrd18$Year <- 2018
hrd17$Year <- 2017
hrd16$Year <- 2016
hrd15$Year <- 2015'''

hrd_stats <- rbind(hrd21, hrd19, hrd18, hrd17, hrd16, hrd15)
hrd_results <- read.csv("hrd_results.csv", header = TRUE)
hrd <- merge(hrd_results, hrd_stats, by=c("Year", "Name"))

hrd$Semifinals[is.na(hrd$Semifinals)] <- 0
hrd$Final[is.na(hrd$Final)] <- 0
hrd$average <- hrd$Total/(rowSums(hrd[,c(3,4,5)] !=0))

hrd <- hrd[,colSums(is.na(hrd)) == 0] # remove NA
hrd$Age.Rng <- NULL # remove age range

clean <- function(x) {
  gsub("\\%", "", x)
} # function to remove %

hrd <- data.frame(apply(hrd, 2, clean)) # apply clean function by column

hrd$Dol <- as.numeric(gsub("\\$", "", hrd$Dol))
hrd$Dol[40] <- 0.5

hrd[,9:259] <- data.frame(apply(hrd[,9:259], 2, as.numeric))
hrd$Total <- as.numeric(hrd$Total)

write.csv(hrd, file="hrd.csv")

hrd$playerid <- NULL
hrd$Events <- NULL
hrd$HardHit <- NULL
hrd$Barrels <- NULL

##

hist(hrd$Total)

## Regression

LR_fit_full <- lm(average ~ ., data = hrd[,9:259])
summary(LR_fit_full)
# this doesn't work on the full dataset because of singularities

#hrd_small <- data.frame(cbind(hrd$average, hrd$EV, hrd$maxEV, hrd$LA, hrd$Barrel., hrd$HardHit., hrd$wOBA, hrd$ISO., hrd$HR.FB.., hrd$Pull.., hrd$BB..))
#colnames(hrd_small) <- c("average", "EV", "maxEV", "LA", "Barrel.", "HardHit.", "wOBA", "ISO.", "HR.FB..", "Pull..", "BB..")

hrd_small <- data.frame(cbind(hrd$average, hrd$Age, hrd$IBB, hrd$HBP, hrd$BB.K, hrd$ISO, hrd$GB.FB, hrd$wOBA, hrd$WAR, hrd$Dol, hrd$Clutch, hrd$FB..1, hrd$wFB, hrd$O.Swing., hrd$Z.Swing.,
                              hrd$Swing., hrd$O.Contact., hrd$Z.Contact., hrd$Contact., hrd$Zone., hrd$Pull., hrd$Hard., hrd$TTO., hrd$EV, hrd$LA, hrd$Barrel., hrd$maxEV))
colnames(hrd_small) <- c("average", "Age", "IBB","HBP", "BB.K", "ISO", "GB.FB", "wOBA", "WAR", "Dol", "Clutch", "FB..1", "wFB", "O.Swing.", "Z.Swing.",
                         "Swing.", "O.Contact.", "Z.Contact.", "Contact.", "Zone.", "Pull.", "Hard.", "TTO.", "EV", "LA", "Barrel.", "maxEV")

hrd_small <- data.frame(cbind(hrd$average, hrd$Age, hrd$IBB, hrd$HBP, hrd$BB.K, hrd$ISO, hrd$GB.FB, hrd$wOBA, hrd$WAR, hrd$Dol, hrd$Clutch, hrd$FB..1, hrd$wFB, 
                              hrd$Swing., hrd$Contact., hrd$Zone., hrd$Pull., hrd$Hard., hrd$TTO., hrd$EV, hrd$LA, hrd$Barrel., hrd$maxEV))
colnames(hrd_small) <- c("average", "Age", "IBB","HBP", "BB.K", "ISO", "GB.FB", "wOBA", "WAR", "Dol", "Clutch", "FB..1", "wFB", "Swing.", 
                         "Contact.", "Zone.", "Pull.", "Hard.", "TTO.", "EV", "LA", "Barrel.", "maxEV")

LR_fit_full_small <- lm(average ~ ., data = hrd_small[0:40,], )
summary(LR_fit_full_small)

LR_fit_full_small_select <- step(LR_fit_full_small,direction="backward",test="F")
summary(LR_fit_full_small_select)

predictions <- predict(LR_fit_full_small_select, newdata = hrd_small[41:48,], type = "response")

cbind(hrd$Name[41:48], predictions)
(summary(LR_fit_full_small_select)$sigma)**2

## predictions

pred_2021 <- as.data.frame(cbind(hrd$Name[41:48], hrd$Odds[41:48], c(2,8,3,5,4,1,7,6), predictions))
colnames(pred_2021) <- c("names", "odds", "seeds", "pred")
pred_2021$pred <- as.numeric(pred_2021$pred)
pred_2021$odds <- as.numeric(pred_2021$odds)
pred_2021 <- pred_2021[order(pred_2021$seeds),]

var <- (summary(LR_fit_full_small_select)$sigma)**2

hrd$Quarterfinals <- as.numeric(hrd$Quarterfinals)
hrd$Semifinals <- as.numeric(hrd$Semifinals)
hrd$Finals <- as.numeric(hrd$Finals)

vec <- c(hrd$Quarterfinals[which(hrd$Quarterfinals != 0)], hrd$Semifinals[which(hrd$Semifinals != 0)], hrd$Finals[which(hrd$Finals != 0)])
var(vec)

hrd[,which()]

sim_matchup <- function(pred_1, pred_2, var_1, var_2) {
  prob <- pnorm(0, mean=(pred_1-pred_2), sd=var_1+var_2, lower.tail=FALSE)
  sim <- rbinom(1,1,prob)
  if (sim==1) {return(pred_1)} else {return(pred_2)}
}

log5 <- function(pred_1, pred_2) {
  Home = pred_1/(pred_1+pred_2)
  Away = pred_2/(pred_1+pred_2)
  numerator = Home - (Home * Away)
  denominator = Home + Away - 2 * Home * Away
  prob = numerator / denominator
  sim <- rbinom(1,1,prob)
  if (sim==1) {return(pred_1)} else {return(pred_2)}
}



sim_matchup(15, 18, var, var)
log5(15, 18)

pred_2021[order(pred_2021$seeds),]



pred <- as.numeric(pred_2021$pred[order(pred_2021$seeds)])

sim_derby <- function(pred){
  return(sim_matchup(sim_matchup(sim_matchup(pred[1], pred[8], var, var), sim_matchup(pred[4], pred[5], var, var), var, var),
              sim_matchup(sim_matchup(pred[2], pred[7], var, var), sim_matchup(pred[3], pred[6], var, var), var, var), var, var))
}

n <- 1000000
sims <- replicate(n, sim_derby(pred), simplify=FALSE)

probs <- c(length(sims[which(sims==pred[1])])/n,
length(sims[which(sims==pred[2])])/n,
length(sims[which(sims==pred[3])])/n,
length(sims[which(sims==pred[4])])/n,
length(sims[which(sims==pred[5])])/n,
length(sims[which(sims==pred[6])])/n,
length(sims[which(sims==pred[7])])/n,
length(sims[which(sims==pred[8])])/n)

pred_2021 <- cbind(pred_2021, probs)

odds_to_probs <- function(odds) {
  return(100/(odds+100))
}

pred_2021 <- cbind(pred_2021, sapply(pred_2021$odds, odds_to_probs))

#your data...
d <- data.frame(row.names=c("1-2","2-3","3-4"), abc = c(10,80, 30), 
                def = c(15, 95, 55), ghi = c(20, 10, 80))
#but you make a matrix out of it to create bar chart
d <- do.call(rbind, d)

rbind(sapply(pred_2021$odds, odds_to_probs), probs)


#...and you are sorted
barplot(rbind(sapply(pred_2021$odds, odds_to_probs), probs), beside = TRUE, ylim=c(0,0.5), names.arg=pred_2021$names, las=1, legend.text = c("consensus", "model"))


