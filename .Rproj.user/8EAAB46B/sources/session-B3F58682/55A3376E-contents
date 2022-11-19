# ---------------------------------------------------------------------------
# IMPORT DATA AND SETUP
# ---------------------------------------------------------------------------
data <- read.csv("./data/customer-data")

str(data)
summary(data)

# ---------------------------------------------------------------------------
# CREATE PLOTS AND SEARCH FOR CORRELATIONS
# ---------------------------------------------------------------------------

library(ggplot2)

# Is there a correlation between Time on Website & Yearly Amount Spent?
ggplot(data, aes(x=Time.on.Website, y=Yearly.Amount.Spent)) + 
  geom_point(colour="orange") + 
  ggtitle("Time on website against vs Yearly amount spent") + 
  xlab("Time on Website") +
  ylab("Yearly Amount Spent")

# Is there a correlation between Avg Session Length & Yearly Amount Spent?
ggplot(data, aes(x=Avg..Session.Length, y=Yearly.Amount.Spent)) + 
  geom_point(colour="orange") +
  ggtitle("Average session length against vs Yearly amount spent") + 
  xlab("Time on Website") +
  ylab("Yearly Amount Spent")

# pairplot of all continuous variables --> 
#### length of membership seems the most correlated
pairs(data[c("Avg..Session.Length", 
             "Time.on.App", 
             "Time.on.Website", 
             "Length.of.Membership",
             "Yearly.Amount.Spent")],
      col = "orange",
      pch = 16,
      labels = c("Avg Session Length", 
                 "Time on App", 
                 "Time on website",
                 "Length of Membership",
                 "Yearly spent"),
      main = "Pairplot of variables")

# pairplot of all continuous variables