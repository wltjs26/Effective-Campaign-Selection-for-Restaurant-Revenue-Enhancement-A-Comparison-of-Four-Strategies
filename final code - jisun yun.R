
#* Final coding portion
# * Design and Analysis of Experiments
# * Course MATH 483
# * Professor: Despina Stasi
# * Students: Jisun Yun
# * Date: December.5.2023
# */

# Load libraries
library(dplyr)
library(rstatix)

#--------------------------------------------------------------
# FIRST Tests (2.2.1) - randomized block design, multiple comparison

# Read and inspect the data
# The sales data for 30 restaurants targeted for the campaigns.
first <- read.csv("data1.csv")
head(first)
summary(first)

# Item-wise mean sales
# Comparison of average sales values by campaign
first %>%
  group_by(campaign) %>%
  summarize(mean_sales = mean(sales))

# Pre-preparation steps for graphing
# Specify the order of detailed items.
first$campaign <- factor(first$campaign, levels = c("A", "B", "C", "D"))

# Set up the plotting environment.
par(mfrow = c(1, 1))

# draw a boxplot graph
# Comparison of average sales values by campaign (graph)
boxplot <- boxplot(sales ~ campaign , col = c("pink","yellowgreen","yellow","violet"),
                   data = first,
                   ylab = "Sales",
                   xlab = "Campaign",
                   main = "Comparison of Sales by Campaigns",
                   ylim = c(0, 400))

means <- tapply(first$sales, first$campaign, mean)
text(1:4, means + 2, labels = round(means, 2), pos = 3, col = "black")

# There are 4 campaign methods, to see if each campaign has the same or different effects
# block: restaurants (30)
# At each restaurant, conduct a one-week campaign and then collect and test the sales results.

# ANOVA without interaction terms
aov_campaign <- aov(sales ~ restaurant + campaign, data = first)
summary(aov_campaign)

# Calculate effect size (partial eta-squared)
effect_size1 <- eta_squared(aov_result)

# Print the effect size
print(effect_size1)

# H0: all means of methods are same
# H1: H0 is not correct
# H1 accept -> multiple comparison
# result: reject H0

## Multiple comparisons
tukey_result <- TukeyHSD(aov_campaign, "campaign")
tukey_result

#---------------------------------------------------------------
# Second Tests (2.2.2) - multiple way layout, multiple comparison

## Check if "digital" method is effective using multiple ANOVA
# import data
# Factors that may have an impact: campaign, weekdays/weekends, location, time of day.
four <- read.csv("data3.csv")
head(four)
summary(four)

# Grouped mean sales for campaign, week, location, and time
four %>%
  group_by(campaign, week, location, time) %>%
  summarize(mean_revenue = mean(revenue))


# Three-way ANOVA
model_four <- aov(revenue ~ week * location * campaign * time, data=four)
summary(model_four)

# Calculate effect size (partial eta-squared)
effect_size2 <- eta_squared(model_four)

# Print the effect size
print(effect_size2)

# multiple comparison 
tukey_result_location <- TukeyHSD(model_four, "location")
tukey_result_location

## Order levels for factors
four$campaign <- factor(four$campaign, levels = c("before", "after"))
four$week <- factor(four$week, levels = c("day", "weekend"))
four$time <- factor(four$time, levels = c("lunch", "dinner"))

# Split data by location
subset_data_pro <- subset(four, location == "Pro")
subset_data_seoul <- subset(four, location == "Seoul")
subset_data_sub <- subset(four, location == "Sub")

# Set up plotting environment
par(mfrow = c(1, 1))

# Draw box plots 
## Province
# Boxplot
bp1 <- boxplot(revenue ~ campaign * week * time, col = c("orange", "skyblue"),
               data = subset_data_pro,
               ylab = "Sales",
               xlab = "Campaign.Day.Time",
               main = "Subset of data: Sales Change (Province)",
               ylim = c(0, 400))
# Legend
legend("topright", legend = c("Before", "After"), fill = c("orange", "skyblue"), title = "Campaign")

# Calculate the mean values for multiple factors. 
means <- tapply(subset_data_pro$revenue, list(subset_data_pro$campaign, subset_data_pro$week, subset_data_pro$time), mean)

# Display the mean values above each box.
text(1:8, means + 3, labels = round(means, 2), pos = 3, col = "black")


## Seoul
# Boxplot
bp2 <- boxplot(revenue ~ campaign * week * time, col = c("orange","skyblue"),
               data = subset_data_seoul,
               ylab = "Sales",
               xlab = "Campaign.day.time",
               main = "Subset of data: Sales Change (Seoul)",
               ylim = c(0, 400))
# Legend
legend("topright", legend = c("Before", "After"), fill = c("orange", "skyblue"), title = "Campaign")

# Calculate the mean values for multiple factors. 
means <- tapply(subset_data_seoul$revenue, list(subset_data_seoul$campaign, subset_data_seoul$week, subset_data_seoul$time), mean)

# Display the mean values above each box.
text(1:8, means + 0, labels = round(means, 2), pos = 3, col = "black")

## Suburb
# Draw boxplot
bp3 <- boxplot(revenue ~ campaign * week * time, col = c("orange", "skyblue"),
               data = subset_data_sub,
               ylab = "Sales",
               xlab = "Campaign.Day.Time",
               main = "Subset of data: Sales Change (Suburb)",
               ylim = c(0, 400))
# Add legend
legend("topright", legend = c("Before", "After"), fill = c("orange", "skyblue"), title = "Campaign")

# Calculate means for multiple factors
means <- tapply(subset_data_sub$revenue, list(subset_data_sub$campaign, subset_data_sub$week, subset_data_sub$time), mean)

# Display mean values above each box
text(1:8, means + 0, labels = round(means, 2), pos = 3, col = "black")

#-----------------------------------------------------
# LAST TESTS - multiple way layout, multiple comparison

# Extract only lunch and dinner data from the data
lunch_data <- subset(four, time == "lunch")
dinner_data <- subset(four, time == "dinner")

# Three-way ANOVA for lunch
model_lunch <- aov(revenue ~ week * location * campaign, data = lunch_data)
summary(model_lunch)

# Tukey HSD for location in lunch
tukey_result_location_lunch <- TukeyHSD(model_lunch, "location")
print(tukey_result_location_lunch)

# Three-way ANOVA for dinner
model_dinner <- aov(revenue ~ week * location * campaign, data = dinner_data)
summary(model_dinner)

# Tukey HSD for location in dinner
tukey_result_location_dinner <- TukeyHSD(model_dinner, "location")
print(tukey_result_location_dinner)


