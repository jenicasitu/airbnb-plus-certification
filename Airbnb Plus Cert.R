# BANA 277
# Midterm Exam: Airbnb Plus Certification

setwd("/Users/jenicasitu/Desktop/uci msba summer 2022 - summer 2023/winter 2023/bana 277/R")
# load libraries
library(summarytools)
library(Hmisc)
library(psych)
library(tidyverse)
library(sqldf)

# import data
data <- read.csv("Imps.csv", header = TRUE)
describe(data)
head(data)

# descriptive statistics (2)
summary(data)
descr(data, stats = "common")
describe(data)

freq(data$Program, report.nas = FALSE)
freq(data$Plus, report.nas = FALSE)

describeBy(data, data$Plus) # grouped by Plus certification

# data visualizations, showing useful patterns in the data (3)
# box whisker of prices
boxplot(data$Price, data=data,
        main="Listing Prices", ylab="Price")

# price and booked
Pr_B_meantable <- sqldf("select Booked, avg(Price) as mean_Price 
                    from data group by Booked")
Pr_B_meantable
barplot(Pr_B_meantable[,2], names.arg=c("Not Booked", "Booked"),
        main="Mean Price of Listing, Not Booked vs Booked")

# Plus and booking
Pl_B_table <- sqldf("select Plus, avg(Booked) as mean_Booked
                    from data group by Plus")
Pl_B_table
barplot(Pl_B_table[,2], names.arg=c("Not Plus", "Plus"),
        main="Mean Booking Rate of Listing, Not Plus vs Plus")
        
# logistic regression analysis (4)
# log calculation of log(Rank) and log(Price)
data$Rank_log <- log(data$Rank)
data$Price_log <- log(data$Price)
View(data)

logit <- glm(Booked ~ Plus + Rank_log + Price_log, data = data, family = "binomial")
summary(logit)

# exponentiated form of the coefficients (5)
# exponentiate coefficients and interpret them as odds-ratios
exp(coef(logit))



