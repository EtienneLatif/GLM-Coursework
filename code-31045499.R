## R template from STAT6123 Assignment 2022-23 ##


############
## Task 1 ##
############

# Clear R workspace
rm(list=ls())

# Set the working directory to read the data for Task 1
setwd("data")

# Read and attach the data for Task 1
task1.data <- read.table("expenditure.txt", header=T)
attach(task1.data)

# Below is where you should write your R code for Task 1
# Begin each segment of code with a comment identifying the question being answered

#########
## 0.1 ##
#########

# Install and import ggplot2
install.packages("ggplot2")
library("ggplot2")

# Set a custom theme for ggplot2 to avoid repetitive lines of code
theme <- theme(
  axis.text.x = element_text(size = 12), 
  axis.text.y = element_text(size = 12),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  plot.title = element_text(hjust = 0.5, size = 17))

#########
## 1.1 ##
#########

# Begin with an assessment of the distribution of the expenditure variable

# View a basic summary of the expenditure variable
summary(expenditure)

# Produce a frequency histogram to display the distribution of the variable
ggplot(task1.data, aes(x = expenditure)) + 
  geom_histogram(bins = 15, col = "#475492", fill = "#a9c9dd") +
  geom_vline(aes(xintercept = mean(expenditure)), col = "#03045e", 
             linetype = "dashed", size = 1) +
  theme

# Produce a boxplot to display the quantiles of the variable
ggplot(task1.data, aes(x = expenditure)) + 
  geom_boxplot(col = "#475492", fill = "#a9c9dd") +
  geom_vline(aes(xintercept = mean(expenditure)), col = "#03045e", 
             linetype = "dashed", size = 1) +
  theme

# Investigate relationship between expenditure and income
ggplot(task1.data, aes(x = income, y = expenditure)) + 
  geom_point(size = 1, col = "#475492") +
  geom_smooth(method = lm, se = FALSE, col = "#03045e") +
  theme

# Investigate relationship between expenditure and categorical variables

# Convert the variables to factors
# Had to include task1.data$ at the beginning of these lines as it seems R was
# creating new variables outside of the data frame when ommitting this, even 
# despite the data frame being attached
task1.data$house.ten <- factor(house.ten, levels = c("1", "2", "3"),
                               labels = c("Public rented", "Private rented",
                                          "Owned"))
task1.data$sex.hh <- factor(sex.hh, levels = c("1", "2"), labels = c("Male", 
                                                                     "Female"))
task1.data$lab.force <- factor(lab.force, levels = c("1", "2", "3", "4"),
                               labels = c("Full time working",
                                          "Part time working",
                                          "Unemployed", 
                                          "Economically inactive"))
task1.data$hh.size <- factor(hh.size, levels = c("1", "2", "3", "4", "5"),
                             labels = c("1 person", "2 persons", "3 persons",
                                        "4 persons", "5 persons or more"))
task1.data$hh.adults <- factor(hh.adults, levels = c("1", "2", "3", "4"),
                               labels = c("1 adult", "2 adults", "3 adults",
                                          "4 adults or more"))

head(task1.data$sex.hh)

# Generate boxplots of expenditure by house.ten
ggplot(task1.data, aes(x = house.ten, y = expenditure)) + 
  geom_boxplot(col = "#475492", fill = "#a9c9dd") +
  ggtitle("Figure 3.1") +
  coord_flip() +
  theme 

# Generate boxplots of expenditure by sex.hh
ggplot(task1.data, aes(x = sex.hh, y = expenditure)) + 
  geom_boxplot(col = "#475492", fill = "#a9c9dd") +
  ggtitle("Figure 3.2") +
  coord_flip() +
  theme

# Generate boxplots of expenditure by lab.force
ggplot(task1.data, aes(x = lab.force, y = expenditure)) + 
  geom_boxplot(col = "#475492", fill = "#a9c9dd") +
  ggtitle("Figure 3.3") +
  coord_flip() +
  theme

# Generate boxplots of expenditure by hh.size
ggplot(task1.data, aes(x = hh.size, y = expenditure)) + 
  geom_boxplot(col = "#475492", fill = "#a9c9dd") +
  ggtitle("Figure 3.4") +
  coord_flip() +
  theme

# Generate boxplots of expenditure by hh.adults
ggplot(task1.data, aes(x = hh.adults, y = expenditure)) + 
  geom_boxplot(col = "#475492", fill = "#a9c9dd") +
  ggtitle("Figure 3.5") +
  coord_flip() +
  theme

# Income vs lab.force
# This lends credence to a suggestion of the reason for outliers in economically
# inactive category, but has been omitted from the report as it is speculative.
ggplot(task1.data, aes(x = lab.force, y = income)) + 
  geom_boxplot(col = "#475492", fill = "#a9c9dd") +
  coord_flip() +
  theme

#########
## 1.2 ##
#########

# Regressing expenditure on income
lm1 <- lm(expenditure ~ income)

# Summary of model
summary(lm1)

# Plot the studentised residuals against fitted values from the model
ggplot(lm1, aes(x = .fitted, y = rstandard(lm1))) + 
  geom_point(size = 1, col = "#475492") +
  geom_hline(yintercept = 0, col = "#03045e", size = 1) +
  xlab("Fitted values") + ylab("Studentised residuals") +
  theme

## Generate a QQ-plot of the studentised residuals 
ggplot(lm1, aes(sample = rstandard(lm1))) +
  stat_qq(size = 2, col = "#475492") +
  stat_qq_line(size = 1, col = "#03045e") +
  xlab("Theoretical quantiles") + ylab("Sample quantiles")
  theme

#########
## 1.3 ##
#########

# Creating income squared variable
task1.data$income_quadr <- income ^ 2

# Regressing expenditure on income and income squared
lm2 <- lm(expenditure ~ income + income_quadr, data = task1.data)

# Summary of model
summary(lm2)

# Plot the studentised residuals against fitted values from the model
ggplot(lm2, aes(x = .fitted, y = rstandard(lm2))) +
  geom_point(size = 1, col = "#475492") +
  geom_hline(yintercept = 0, col = "#03045e", size = 1) +
  xlab("Fitted values") + ylab("Studentised residuals") +
  theme

## Generate a QQ-plot of the studentised residuals 
ggplot(lm2, aes(sample = rstandard(lm2))) +
  stat_qq(size = 2, col = "#475492") +
  stat_qq_line(size = 1, col = "#03045e") +
  xlab("Theoretical quantiles") + ylab("Sample quantiles")
theme

#########
## 1.4 ##
#########

# Creating natural log of expenditure variable
task1.data$log_expenditure <- log(expenditure)

# Plot log expenditure against income to visualise this relationship
ggplot(task1.data, aes(x = income, y = log_expenditure)) + 
  geom_point(size = 1, col = "#475492") +
  geom_smooth(method = lm, se = FALSE, col = "#03045e") +
  theme

# The relationship appears slightly (negative) quadratic
# We would expect the quadratic term (in the next question) to be a useful
# predictor.

# Regressing log expenditure on income
lm3 <- lm(log_expenditure ~ income, data = task1.data)

# Summary of model
summary(lm3)

# Plot the studentised residuals against fitted values from the model
ggplot(lm3, aes(x = .fitted, y = rstandard(lm3))) + 
  geom_point(size = 1, colour = "#475492") +
  geom_hline(yintercept = 0, col = "#03045e", size = 1) +
  xlab("Fitted values") + ylab("Studentised residuals") +
  theme

## Generate a QQ-plot of the studentised residuals 
ggplot(lm3, aes(sample = rstandard(lm3))) +
  stat_qq(size = 2, col = "#475492") +
  stat_qq_line(size = 1, col = "#03045e") +
  xlab("Theoretical quantiles") + ylab("Sample quantiles") +
  theme

#########
## 1.5 ##
#########

# Regressing log expenditure on income and income squared
lm4 <- lm(log_expenditure ~ income + income_quadr, data=task1.data)

# Summary of model
summary(lm4)

# Plot the studentised residuals against fitted values from the model
ggplot(lm4, aes(x = .fitted, y = rstandard(lm4))) + 
  geom_point(size = 1, colour = "#475492") +
  geom_hline(yintercept = 0, col = "#03045e", size = 1) +
  xlab("Fitted values") + ylab("Studentised residuals") +
  theme

## Generate a QQ-plot of the studentised residuals 
ggplot(lm4, aes(sample = rstandard(lm4))) +
  stat_qq(size = 2, col = "#475492") +
  stat_qq_line(size = 1, col = "#03045e") +
  xlab("Theoretical quantiles") + ylab("Sample quantiles") +
  theme

#########
## 1.6 ##
#########

# Compare lm3 and lm4. Disregard lm1 and lm2 as modelling assumptions violated.

# Simple comparison of the two models. Note whether there is a difference in 
# R^2 and whether the t-test suggests income^2 provides useful information.
summary(lm3)
summary(lm4)

# ANOVA to compare the goodness-of-fit of the two models
anova(lm3, lm4)

# Check minimum and maximum income
min(income)
max(income)

#########
## 1.7 ##
#########

best_mod <- lm4

# Begin by trying to add each main effect
lm5 <- lm(log_expenditure ~ income + income2 + house.ten, data = task1.data)
anova(lm4, lm5) # Significant, keep house.ten
best_mod <- lm5

lm6 <- lm(log_expenditure ~ income + income2 + house.ten + sex.hh,
          data = task1.data)
anova(best_mod, lm6) # Insignificant, do not keep sex.hh

lm7 <- lm(log_expenditure ~ income + income2 + house.ten + lab.force,
          data = task1.data)
anova(best_mod, lm7) # Significant, keep lab.force
best_mod <- lm7

lm8 <- lm(log_expenditure ~ income + income2 + house.ten + lab.force + hh.size,
          data = task1.data)
anova(lm7, lm8) # Significant, keep hh.size
best_mod <- lm8

lm9 <- lm(log_expenditure ~ income + income2 + house.ten + lab.force + hh.size +
            hh.adults, data = task1.data)
anova(best_mod, lm9) # Insignificant, do not keep hh.adults

# Current best model contains main effects of house.ten, lab.force, hh.size, 
# and hh.adults

# Try to add interactions with income
lm10 <- lm(log_expenditure ~ income + income2 + house.ten + lab.force + hh.size
           + hh.adults + house.ten:income, data = task1.data)
anova(best_mod, lm10) # Insignificant, do not include this interaction

lm11 <- lm(log_expenditure ~ income + income2 + house.ten + lab.force + hh.size 
           + hh.adults + lab.force:income, data = task1.data)
anova(best_mod, lm11) # Significant, include this interaction
best_mod <- lm11

lm12 <- lm(log_expenditure ~ income + income2 + house.ten + lab.force + hh.size 
           + hh.adults + lab.force:income + hh.size:income, data = task1.data)
anova(best_mod, lm12) # Significant, include this interaction
best_mod <- lm12

lm13 <- lm(log_expenditure ~ income + income2 + house.ten + lab.force + hh.size 
           + hh.adults + lab.force:income + hh.size:income + hh.adults:income,
           data = task1.data)
anova(best_mod, lm13) # Insignificant, do not include this interaction

# Best model includes interactions between income and lab.force and income and
# hh.size

# 

## Task 2 ##

# Clear R workspace
rm(list=ls())


# Set the working directory to read the data for Task 2
setwd("...")

# Read and attach the data for Task 2
task2.data <- read.table("airline.txt", header=T)
attach(task2.data)

# Below is where you should write your R code for Task 2
# Begin each segment of code with a comment identifying the question being answered
