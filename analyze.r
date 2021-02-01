# Load libraries.
library(ggplot2)
library(dplyr)
library(lmtest)

# Read data and add columns.
t <- read.table("foosball.tsv", header=T)
t$Date <- as.Date(t$Date, format= "%d.%m.%y")
winner <- ifelse(t$Blues > t$Reds, "Blues", "Reds")
t$Winner <- winner
t$TotalGoals <- t$Blues + t$Reds
t$GoalsDifference <- t$Blues - t$Reds

# Report the total number of matches and the total number of goals.
nrow(t)
sum(t$TotalGoals)
sum(t$Blues)
sum(t$Reds)
sum(t$Winner=="Blues")
sum(t$Winner=="Reds")

# Test for increase/decrease in numbers of matches per day over time.
t_counted <- t %>% count(Date, name = 'count')
res <- lm(t_counted$count ~ t_counted$Date)
summary(res)
intercept <- as.numeric(coef(res)["(Intercept)"])
slope <- as.numeric(coef(res)["t_counted$Date"])
f <- summary(res)$fstatistic
p <- pf(f[1],f[2],f[3],lower.tail=F)
str <- paste("P =", formatC(p, format = "e", digits = 2))

# Plot the number of matches per day.
pdf("matches_per_day.pdf", 7, 4)
ggplot(t) + geom_bar(aes(x = Date), width = 3) +
	geom_abline(slope = slope, intercept = intercept) +
	labs(x = "Date", y = "Matches per day") +
	annotate("text", x = as.Date("01.07.2020", format= "%d.%m.%y"), y=4.5, label = str) +
	theme(axis.title = element_text(size = 12, face="bold"), panel.background = element_rect(fill = alpha('#eee8d5', 0.5)))
dev.off()

# Test for increase/decrease in the difference in goals per match.
res <- lm(t$GoalsDifference ~ as.numeric(row.names(t)))
summary(res)

# Test for autocorrelation in the difference in goals per match.
dwtest(res)

# Plot the goals per match.
pdf("difference_in_goals.pdf", 7, 4)
ggplot(t) + geom_bar(aes(x = as.numeric(row.names(t)), y = GoalsDifference, fill = Winner), stat = 'identity', width = 1) +
	scale_fill_manual(values=c("#268bd2", "#dc322f")) +
	labs(x = "Match number", y = "Difference in goals") +
	theme(axis.title = element_text(size = 12, face="bold"), panel.background = element_rect(fill = alpha('#eee8d5', 0.5)))
dev.off()