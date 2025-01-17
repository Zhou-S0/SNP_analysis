# Installing and loading package that I need to use:
# To tidy and change the dataset (tidyverse and dplyr)
# Creating density plots and change the text of the graphs (ggplot2)
# To use colours that could help with visualisation (RColorBrewer)
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("RColorBrewer") 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer) 

# Using the read.csv() function to read the assignment.1.csv file
data <- read.csv("assignment.1.csv")

# Using str() and summary() to see the strucutre/summary of the dataset
str(data)
summary(data)

# Changing ancestry variable from categorical to factor using the as.factor() function
data$ancestry <- as.factor(data$ancestry)

# Summarising the risk scores and each SNPs
summary(data$risk_score)
summary(data$SNP1)
summary(data$SNP2)
summary(data$SNP3)
summary(data$SNP4)
summary(data$SNP5)

# To save time, set the SNP names into a variable
snps <- c("SNP1", "SNP2", "SNP3", "SNP4", "SNP5")

# Graph One - Histogram
# Making a histogram of the risk scores to see the distribution of the disease susceptibility
# This is done by using the hist() function
hist(data$risk_score,
     main = "The Distribution of Risk Scores",
     xlab = "Risk Score",
     ylab = "Frequency",
     col = brewer.pal(3, "Set1")[1])

# Graph Two - Density Plot 
# Making a density plot of the risk scores by ancestry to see what ancestry is most affected
# This done by using ggplot() and the geom_density() function
ggplot(data, aes(x = risk_score, fill = ancestry)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Density Plot of Risk Scores by Ancestry",
       x = "Risk Score",
       y = "Density",
       fill = "Ancestry") +
  theme_minimal()+
  theme(axis.title.x = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
        axis.title.y = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
        plot.title = element_text(hjust = 0.5, colour = "black", size = 15, face = 'bold', family = 'Arial'),
        legend.title = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
        legend.position = 'top') # Help organise the title, lables and legend on the plot

# Graph Three - Boxplot
# Creating a boxplot to see the risk scores grouped by ancestry for better visualisation
# This is done by using the ggplot2 packages with the geom_boxplot()
ggplot(data, aes(x = ancestry, y = risk_score, fill = ancestry)) +
  geom_boxplot() +
  labs (title = "Risk Score classified by Ancestry",
        x = "Ancestry",
        y = "Risk Score") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
    axis.title.y = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
    plot.title = element_text(hjust = 0.5, colour = "black", size = 15, face = 'bold', family = 'Arial'),
    legend.position = "top") # Help organise the title, lables and legend on the plot

# Using pivot_longer %>%, reshaping the data from wide to long using the pivot_longer() function
# This makes it easier to plot the boxplot
data_longer <- data %>%
  pivot_longer(cols = snps,
               names_to = "SNP",
               values_to = "SNP_value")
print(data_longer)

# Graph Four - Box plot 
# Making another box plot showing the risk scores for each individual carrying the risk allele or not
# This was done with ggplot2 package, with geom_boxplot()
ggplot(data_longer, aes(x = factor(SNP_value), y = risk_score, fill = factor(SNP_value))) +
  geom_boxplot() +
  labs(title = "Risk Score by SNP",
       x = "Allele (0 vs 1)",
       y = "Risk Score",) +
  scale_fill_manual(values = c("mediumorchid", "mediumblue")) +
  facet_wrap(~ SNP) +
  theme_minimal() +
  theme(axis.title.x = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
        axis.title.y = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
        plot.title = element_text(hjust = 0.5, colour = "black", size = 15, face = 'bold', family = 'Arial'),
        legend.title = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
        legend.position = "top")

# To determine the association between the risk score and SNPs, while taking ancestry into account
# There is a need to conduct a linear regression/model since it will quantify the relationship between the variables
lm_snp <- lm(risk_score ~ SNP1 + SNP2 + SNP3 + SNP4 + SNP5 + ancestry, data = data)
lm_summary <- summary(lm_snp)
lm_summary

# After looking through the summary, extract the p-values from the report/output
snp_p_values <- lm_summary$coefficients[, "Pr(>|t|)"]

# Taking those P-Values and performing for Bonferroni Correction
# Done by using p.adjust() and selecting the method as 'bonferroni'
bonferroni_p_values <- p.adjust(snp_p_values, method = 'bonferroni')
print(bonferroni_p_values)

# Creating a dataframe for the P-Values before and after the Bonferroni Correction
snp_data <- data.frame(
  snp = snps, 
  P_Value = snp_p_values[2:6],
  Bonferroni_P_Value = bonferroni_p_values [2:6]
)
print(snp_data)

# Reshaping the data from wide to long using the pivot_longer function again
# Helps with plotting the bar plot with P-Values before and after the Bonferroni Correction
snp_data_long <- snp_data %>%
  pivot_longer(cols = c("P_Value", "Bonferroni_P_Value"), 
               names_to = "P_Type", 
               values_to = "P_Value")
print(snp_data_long)

# Graph Five - Bar Plot
# Coding a boxplot to visualise the p-values before and after the Bonferroni Correction
# This is done by geom_bar() and the dataset created above
ggplot(snp_data_long, aes(x = snp, y = P_Value, fill = P_Type)) +
  geom_bar(stat = "identity",  position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "SNP P-Values before and after Bonferroni Correction",
       x = "SNP",
       y = "P-Value",
       fill = "Type") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
    axis.title.y = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
    plot.title = element_text(hjust = 0.5, colour = "black", size = 15, face = 'bold', family = 'Arial'),
    legend.title = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
    legend.position = 'right')