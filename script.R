## How has no crowd effected the Premier League this Season?
library(ggplot2)
library(dplyr)
library(moments)
library(reshape2)
# read in results data from Seasons 1993/94 - now
results <- read.csv("data/external/results/results.csv")
results <- results %>% filter(Season != "1993-94")
results <- results %>% filter(Season != "1994-95")
results <- results %>% select(Season, DateTime, HomeTeam, AwayTeam, FTHG, FTAG, FTR, Referee)
# Create statistical summaries from this season
HomeSummary <- results %>% group_by(Season) %>% summarise(mean = mean(FTHG), median = median(FTHG), sd = sd(FTHG), skew = skewness(FTHG), min = range(FTHG)[1], max = range(FTHG)[2], Q1 = quantile(FTHG, probs = c(0.25)), Q3 = quantile(FTHG, probs = c(0.75)))
AwaySummary <- results %>% group_by(Season) %>% summarise(mean = mean(FTAG), median = median(FTAG), sd = sd(FTAG), skew = skewness(FTAG), min = range(FTAG)[1], max = range(FTAG)[2], Q1 = quantile(FTAG, probs = c(0.25)), Q3 = quantile(FTAG, probs = c(0.75)))
HomeSummary$HoA <- "Home"
AwaySummary$HoA <- "Away"
GoalSummary <- rbind(HomeSummary, AwaySummary)
GoalSummary$HoA <- factor(GoalSummary$HoA, levels = c("Home", "Away"))
GoalSummary <- arrange(GoalSummary, Season)
GoalSummary <- mutate(GoalSummary, ID = "")
GoalSummary$ID[GoalSummary$Season != "2020-21"] <- "Past"
GoalSummary$ID[GoalSummary$Season == "2020-21"] <- "Present"
GoalSummary$ID <- factor(GoalSummary$ID, levels = c("Past", "Present"))
# create a plot to compare the mean goals this season against previous seasons
g <- ggplot(GoalSummary, aes(x = HoA, y = mean, group = Season))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size = 5, pch = 21, fill = "salmon", alpha = 0.5, data = filter(GoalSummary, ID == "Past"))
g <- g + geom_point(size = 10, pch = 21, fill = "black", data = filter(GoalSummary, ID == "Present"))

# Now back to our results df
# hypothesise that the population mean is 0, with the alternative that it is greater than 0 
with(filter(results, Season != "2020-21"), t.test(FTHG, FTAG, paired = TRUE, alternative = "greater"))
# we can be very confident that the true population mean of FTHG is greater than FTAG
# now lets hypotheisise that the population mean is mu = 0.3996842
ttest <- results %>% group_by(Season) %>% 
  summarise(pValue = t.test(FTHG, FTAG, paired = TRUE, mu = 0.369789 )$p.value)
# Make FDR adjustment to p-values accounting for the chance that comes with multiple testing
ttest <- mutate(ttest, pAdj = p.adjust(pValue, "BH"))
arrange(ttest, pValue)
# we see that whilst with 5% confidence we could not reject then Null Hypothesis with 6% we can. Since, this 
# seems significantly more valid than the other seasons, this seems a clear signal of an outlier. Given the
# strength of the limited data we have available.

# Now lets look at the change in distribution
# home ... 
HomeDistr <- results %>% group_by(Season) %>% count(FTHG)
HomeDistrWide <- dcast(HomeDistr, Season ~ FTHG, value.var = "n")
HomeDistrWide[is.na(HomeDistrWide)] <- 0
HomeDistr <- melt(HomeDistrWide, id.vars = "Season", value.name = "n", variable.name = "FTHG")
HomeDistr <- HomeDistr %>% arrange(Season, FTHG)
# away ... 
AwayDistr <- results %>% group_by(Season) %>% count(FTAG)
AwayDistrWide <- dcast(AwayDistr, Season ~ FTAG, value.var = "n")
AwayDistrWide[is.na(AwayDistrWide)] <- 0
AwayDistr <- melt(AwayDistrWide, id.vars = "Season", value.name = "n", variable.name = "FTAG")
AwayDistr <- AwayDistr %>% arrange(Season, FTAG)
#
names(HomeDistr) <- c("Season", "FTG", "n"); names(AwayDistr) <- c("Season", "FTG", "n")
HomeDistr$HoA <- "Home"; AwayDistr$HoA <- "Away"
GoalDistr <- rbind(HomeDistr, AwayDistr)
GoalDistr$HoA <- factor(GoalDistr$HoA, levels = c("Home", "Away"))
# add in a probability density for each goal
GoalDistr <- GoalDistr %>% group_by(Season, HoA) %>% mutate(p = n / sum(n))
# cluster analysis of the dsitribution of goals
# home ... 
HomeDistrWide <- dcast(subset(GoalDistr, HoA == "Home"), Season ~ FTG, value.var = "p")
Season <- HomeDistrWide$Season
HomeDistrWide <- as.matrix(HomeDistrWide)
rownames(HomeDistrWide) <- Season
homeDist <- dist(HomeDistrWide)
hCluster1 <- hclust(homeDist)
plot(hCluster1)
# away ...
AwayDistrWide <- dcast(subset(GoalDistr, HoA == "Away"), Season ~ FTG, value.var = "p")
Season <- AwayDistrWide$Season
AwayDistrWide <- as.matrix(AwayDistrWide)
rownames(AwayDistrWide) <- Season
AwayDist <- dist(AwayDistrWide)
hCluster2 <- hclust(AwayDist)
plot(hCluster2)

# Assuming the other seasons in the Premeir League and 2020-21 is an outlier
# lets fit a distribution to all of the other seasons and compare to 2020-21
# split home and away
GoalDistr$ID <- ""
GoalDistr$ID[GoalDistr$Season != "2020-21"] <- "Past Seasons"
GoalDistr$ID[GoalDistr$Season == "2020-21"] <- "This Season"
GoalDistr$ID <- factor(GoalDistr$ID, levels = c("Past Seasons", "This Season"))
# group by past and present and find a general distribution for the past seasons
GeneralDistr <- GoalDistr %>% group_by(ID, HoA, FTG) %>% summarise(p = mean(p)) 
GeneralDistr$FTG <- as.numeric(as.character(GeneralDistr$FTG))
# plot the distributions 
g1 <- ggplot(GeneralDistr, aes(FTG, p, fill = ID)) + 
  geom_point(size = 5, pch = 21, alpha = 0.5) + 
  geom_line(aes(linetype = ID, colour = ID), size = 1) +
  facet_wrap(~HoA, nrow = 2) +
  scale_x_continuous(breaks = 0:9)
g1
# home distribution
HomeDistr <- filter(GeneralDistr, HoA == "Home")
AwayDistr <- filter(GeneralDistr, HoA == "Away")

seqn <- seq(1, 1.6, 0.2)
x <- 0:9
df = data.frame(x = numeric(), d = numeric(), lambda = numeric())
for (i in seqn) {
  df <- rbind(df, data.frame(x=x, d = dpois(x, lambda = i), lambda = i))
}

g2 <- ggplot(HomeDistr, aes(FTG, p, group = ID)) + 
  geom_point() + 
  geom_line(aes(colour = ID), size = 1.5) + 
  geom_line(data = df, aes(x = x, y = d, group = lambda), alpha = 0.4) +
  scale_x_continuous(breaks = 0:9)

g2

g3 <- ggplot(AwayDistr, aes(FTG, p, group = ID)) + 
  geom_point() + 
  geom_line(aes(colour = ID), size = 1.5) + 
  geom_line(data = df, aes(x = x, y = d, group = lambda), alpha = 0.4) +
  scale_x_continuous(breaks = 0:9)

g3

## Who is scoring these away goals??
res21 <- filter(results, Season == "2020-21")
length(unique(select(filter(res21, FTAG >= 3), HomeTeam, AwayTeam, FTHG, FTAG)$AwayTeam))
unique(select(filter(res21, FTAG >= 3), HomeTeam, AwayTeam, FTHG, FTAG)$HomeTeam)
unique(select(filter(res21, FTHG == 0), HomeTeam, AwayTeam, FTHG, FTAG)$HomeTeam)
unique(select(filter(res21, FTHG == 0), HomeTeam, AwayTeam, FTHG, FTAG)$AwayTeam)






