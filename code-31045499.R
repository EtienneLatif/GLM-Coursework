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
detach(task1.data)

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
  axis.text.x=element_text(size=12), 
  axis.text.y=element_text(size=12),
  axis.title.x=element_text(size=15),
  axis.title.y=element_text(size=15))


#########
## 1.1 ##
#########


# Begin with an assessment of the distribution of the expenditure variable

# View a basic summary of the expenditure variable
summary(expenditure)

# Produce a frequency histogram to display the distribution of the variable
ggplot(task1.data, aes(x=expenditure)) + 
  geom_histogram(bins=15, colour="#475492", fill="#a9c9dd") +
  geom_vline(aes(xintercept=mean(expenditure)), color="#03045e", 
             linetype="dashed", size=1) +
  theme

# Produce a boxplot to display the quantiles of the variable
ggplot(task1.data, aes(x=expenditure)) + 
  geom_boxplot(color="#475492", fill="#a9c9dd") +
  geom_vline(aes(xintercept=mean(expenditure)), color="#03045e", 
             linetype="dashed", size=1) +
  theme

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
