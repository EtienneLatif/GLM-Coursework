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
  axis.text.x  = element_text(size = 12), 
  axis.text.y  = element_text(size = 12),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  plot.title   = element_text(hjust = 0.5, size = 17))

#########
## 1.1 ##
#########

# Begin with an assessment of the distribution of the expenditure variable

# View a basic summary of the expenditure variable
summary(expenditure)

# Produce a frequency histogram to display the distribution of the expenditure 
# variable
ggplot(task1.data, aes(x = expenditure)) + 
  geom_histogram(bins = 15, col = "#475492", fill = "#a9c9dd") +
  geom_vline(aes(xintercept = mean(expenditure)), col = "#03045e", 
             linetype = "dashed", linewidth = 1) +
  theme

# Produce a boxplot to display the quantiles of the expenditure variable
ggplot(task1.data, aes(x = expenditure)) + 
  geom_boxplot(col = "#475492", fill = "#a9c9dd") +
  geom_vline(aes(xintercept = mean(expenditure)), col = "#03045e", 
             linetype = "dashed", linewidth = 1) +
  theme

# Investigate the relationship between expenditure and income with a scatter 
# plot and line of best fit
ggplot(task1.data, aes(x = income, y = expenditure)) + 
  geom_point(size = 1, col = "#475492") +
  geom_smooth(method = lm, se = FALSE, col = "#03045e") +
  theme

# Investigate relationship between expenditure and categorical variables

# Convert the variables to factors
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

# Investigate the relationship between expenditure and categorical variables
# with box plots of expenditure against each variable
# Figure titles are for use in the report

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
# This lends credence to a suggestion for the reason that outliers are present 
# in the economically inactive category given in the report, but has been 
# omitted from the report as it is purely speculative.
ggplot(task1.data, aes(x = lab.force, y = income)) + 
  geom_boxplot(col = "#475492", fill = "#a9c9dd") +
  coord_flip() +
  theme

#########
## 1.2 ##
#########

# Regress expenditure on income
lm1 <- lm(expenditure ~ income)

# Summary of model
summary(lm1)

# Check the regression assumptions

# Plot the studentised residuals against fitted values from the model
ggplot(lm1, aes(x = .fitted, y = rstandard(lm1))) + 
  geom_point(size = 1, col = "#475492") +
  geom_hline(linewidth = 1, yintercept = 0, col = "#03045e") +
  xlab("Fitted values") + ylab("Studentised residuals") +
  theme

## Generate a QQ-plot of the studentised residuals 
ggplot(lm1, aes(sample = rstandard(lm1))) +
  stat_qq(size = 2, col = "#475492") +
  stat_qq_line(linewidth = 1, col = "#03045e") +
  xlab("Theoretical quantiles") + ylab("Sample quantiles") +
  theme

#########
## 1.3 ##
#########

# Create the income squared variable
task1.data$income_quadr <- income ^ 2

# Regress expenditure on income and income squared
lm2 <- lm(expenditure ~ income + income_quadr, data = task1.data)

# Summary of model
summary(lm2)

# Check the regression assumptions

# Plot the studentised residuals against fitted values from the model
ggplot(lm2, aes(x = .fitted, y = rstandard(lm2))) +
  geom_point(size = 1, col = "#475492") +
  geom_hline(linewidth = 1, yintercept = 0, col = "#03045e") +
  xlab("Fitted values") + ylab("Studentised residuals") +
  theme

## Generate a QQ-plot of the studentised residuals 
ggplot(lm2, aes(sample = rstandard(lm2))) +
  stat_qq(size = 2, col = "#475492") +
  stat_qq_line(linewidth = 1, col = "#03045e") +
  xlab("Theoretical quantiles") + ylab("Sample quantiles") +
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
# predictor

# Regress log expenditure on income
lm3 <- lm(log_expenditure ~ income, data = task1.data)

# Summary of model
summary(lm3)

# Check the regression assumptions

# Plot the studentised residuals against fitted values from the model
ggplot(lm3, aes(x = .fitted, y = rstandard(lm3))) + 
  geom_point(size = 1, colour = "#475492") +
  geom_hline(linewidth = 1, yintercept = 0, col = "#03045e") +
  xlab("Fitted values") + ylab("Studentised residuals") +
  theme

## Generate a QQ-plot of the studentised residuals 
ggplot(lm3, aes(sample = rstandard(lm3))) +
  stat_qq(size = 2, col = "#475492") +
  stat_qq_line(linewidth = 1, col = "#03045e") +
  xlab("Theoretical quantiles") + ylab("Sample quantiles") +
  theme

#########
## 1.5 ##
#########

# Regress log expenditure on income and income squared
lm4 <- lm(log_expenditure ~ income + income_quadr, data=task1.data)

# Summary of model
summary(lm4)

# Check the regression assumptions

# Plot the studentised residuals against fitted values from the model
ggplot(lm4, aes(x = .fitted, y = rstandard(lm4))) + 
  geom_point(size = 1, colour = "#475492") +
  geom_hline(linewidth = 1, yintercept = 0, col = "#03045e" ) +
  xlab("Fitted values") + ylab("Studentised residuals") +
  theme

## Generate a QQ-plot of the studentised residuals 
ggplot(lm4, aes(sample = rstandard(lm4))) +
  stat_qq(size = 2, col = "#475492") +
  stat_qq_line(linewidth = 1, col = "#03045e") +
  xlab("Theoretical quantiles") + ylab("Sample quantiles") +
  theme

#########
## 1.6 ##
#########

# Compare lm3 and lm4. Disregard lm1 and lm2 as modelling assumptions violated.

# Simple comparison of the two models. Note informally whether there is a 
# difference in R^2 and formally use a t-test to evaluate the significance of 
# the income^2 predictor

# View both model summaries
summary(lm3)
summary(lm4)

# ANOVA to compare the goodness-of-fit of the two models
anova(lm3, lm4)

# Check minimum and maximum income to comment on the range of income values for
# which we can interpret the relationship between the two variables in the 
# report (extrapolating the relationship is poor practice)
min(income)
max(income)

#########
## 1.7 ##
#########

# Store the preferred model. Update this object based on significance of F tests
best_mod <- lm4

# Begin by trying to add each main effect

# Try to add house.ten
lm5 <- lm(log_expenditure ~ income + income_quadr + house.ten, 
          data = task1.data)
anova(lm4, lm5) # Significant, keep house.ten
best_mod <- lm5

# Try to add sex.hh
lm6 <- lm(log_expenditure ~ income + income_quadr + house.ten + sex.hh,
          data = task1.data)
anova(best_mod, lm6) # Insignificant, do not keep sex.hh

# Try to add lab.force
lm7 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force,
          data = task1.data)
anova(best_mod, lm7) # Significant, keep lab.force
best_mod <- lm7

# Try to add hh.size
lm8 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
          + hh.size, data = task1.data)
anova(lm7, lm8) # Significant, keep hh.size
best_mod <- lm8

# Try to add hh.adults
lm9 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
          + hh.size + hh.adults, data = task1.data)
anova(best_mod, lm9) # Insignificant, do not keep hh.adults

# Current best model contains main effects of house.ten, lab.force and hh.size
summary(best_mod)

# Try to add 2-way interactions with income

# Try to add the interaction between income and house.ten
lm10 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
           + hh.size + income:house.ten, data = task1.data)
anova(best_mod, lm10) # Significant, include this interaction
best_mod <- lm10

# Try to add the interaction between income and lab.force
lm11 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
           + hh.size + income:house.ten + income:lab.force, data = task1.data)
anova(best_mod, lm11) # Significant, include this interaction
best_mod <- lm11

# Try to add the interaction between income and hh.size
lm12 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
           + hh.size + income:house.ten + income:lab.force + income:hh.size,
           data = task1.data)
anova(best_mod, lm12) # Significant, include this interaction
best_mod <- lm12

# Consider the models obtained by dropping house.ten or lab.force from the
# current best model (not necessary to do for hh.size as this was the last
# test performed)

# Drop lab.force from the model
lm10_2 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
             + hh.size + income:house.ten + income:hh.size,
             data = task1.data)
anova(lm10_2, best_mod) # The model including lab.force is significantly better
                        # fit than that without

lm11_2 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
             + hh.size + income:lab.force + income:hh.size,
             data = task1.data)
anova(lm11_2, best_mod) # This model is not a significantly better fit, hence we
                        # we can discard the income and house.ten interaction

# Change the best model to the model without the interaction between income and
# house.ten
best_mod <- lm11_2

# Best model includes interactions between income and lab.force and income and
# hh.size
summary(best_mod)

# Try to add 2-way interactions between categorical variables

# Try to add the interaction between house.ten and lab.force
lm13 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
           + hh.size  + income:lab.force + income:hh.size 
           + house.ten:lab.force, data = task1.data)
anova(best_mod, lm13) # Insignificant, do not include this interaction

# Try to add the interaction between house.ten and hh.size
lm14 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
           + hh.size + income:lab.force + income:hh.size
           + house.ten:hh.size, data = task1.data)
anova(best_mod, lm14) # Insignificant, do not include this interaction

# Try to add the interaction between lab.force and hh.size
lm15 <- lm(log_expenditure ~ income + income_quadr + house.ten + lab.force 
           + hh.size + income:lab.force + income:hh.size
           + lab.force:hh.size, data = task1.data)
anova(best_mod, lm15) # Insignificant, do not include this interaction

# Final selected model
summary(best_mod)

# Plot the studentised residuals against fitted values from the model
ggplot(best_mod, aes(x = .fitted, y = rstandard(best_mod))) + 
  geom_point(size = 1, colour = "#475492") +
  geom_hline(linewidth = 1, yintercept = 0, col = "#03045e") +
  xlab("Fitted values") + ylab("Studentised residuals") +
  theme 

## Generate a QQ-plot of the studentised residuals 
ggplot(best_mod, aes(sample = rstandard(best_mod))) +
  stat_qq(size = 2, col = "#475492") +
  stat_qq_line(linewidth = 1, col = "#03045e") +
  xlab("Theoretical quantiles") + ylab("Sample quantiles") +
  theme
  

## Task 2 ##

# Clear R workspace
rm(list=ls())


# Set the working directory to read the data for Task 2
setwd("data")

# Read and attach the data for Task 2
task2.data <- read.table("airline.txt", header=T)
attach(task2.data)

# Below is where you should write your R code for Task 2
# Begin each segment of code with a comment identifying the question being answered

#########
## 2.1 ##
#########

# We can explicitly calculate the information matrix due to it's lack of
# dependence on unknown variables

# Recall from the report that I = X^TX where X is the 
# design matrix

# We need to define the design matrix X = [[1, x_1], [1, x_2], ... , [1, x_n]]^T

# Dummy variable of 1000 ones
dummy_var <- rep(1, 1000)

# Create the design matrix
X_data <- c(dummy_var, x) # Create a list of all the design matrix data
X <- matrix(data = X_data, nrow = 1000, ncol = 2, byrow = FALSE) # design matrix

# Create the Fisher information matrix
I <- t(X) %*% X
I # View the matrix for presentation in the report

#########
## 2.2 ##
#########

# We will implement the Fisher scoring algorithm: 
# beta(m+1) <- beta(m) + I^-1 u(m)
# Note that the Fisher information does not depend on beta 

# Define the maximum number of iterations before termination of algorithm
max_iter <- 1000

# Track the number of iterations
iter <- 1

# Define the tolerance that determines when an estimate is said to have 
# converged
tolerance <- 0.001

# Create a boolean variable to trigger if the estimate converges
converged <- FALSE # initialise at FALSE

# Create a matrix to store our estimates of beta_0 and beta_1
beta_hat      <- matrix(NA, max_iter, dim(X)[2]) # initialise the matrix with NA
                                                 # values
beta_hat[1, ] <- c(0.1, 0.1) # initialise the first estimate beta(0)

# Use a while loop to iterate the algorithm while the convergence boolean has 
# not been triggered and the maximum number of iterations has not been reached
while(!converged & iter < max_iter) {
  
  # Calculate the systematic component
  eta <- X %*% beta_hat[iter, ]
  
  # Calculate the mean vector by inverting the link function
  mu <- exp(eta)
  
  # Calculate the vector z as defined in the report
  z <- (y - mu) / mu
  
  # Calculate the score
  u <- t(X) %*% z
  
  # Calculate the next estimate of beta
  beta_hat[(iter+1), ] <- beta_hat[iter, ] + solve(I) %*% u
  
  # Check for convergence
  if(abs(beta_hat[iter, 1] - beta_hat[(iter+1), 1] + 
         beta_hat[iter, 2] - beta_hat[(iter+1), 2]) < tolerance) {
    converged <- TRUE # trigger the boolean if the algorithm has converged
  }
  
  # Update the iteration
  iter <- iter + 1
}

# Get the final estimate of beta
beta_hat <- beta_hat[iter, ]
beta_hat

#########
## 2.3 ##
#########

# Variance covariance matrix of the mle of beta is given by the inverse of the
# Fisher information matrix
cov_mat <- solve(I)
cov_mat # view the result for presentation in the report

#########
## 2.4 ##
#########

# The t-statistic for significance of beta_1 is given by the mle of beta_1 
# divided by the standard deviation of the mle of beta_1
t_stat_1 <- beta_hat[2] / sqrt(cov_mat[2, 2])
t_stat_1

# Find the p-value of the t-test for significance of the variable by using
# P(t > t_stat_1) = 1 - P(t < t_stat_1)
1 - pt(q = t_stat_1, df = (1000 - 2 - 1))
