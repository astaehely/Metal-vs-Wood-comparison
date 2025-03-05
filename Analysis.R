library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)

data <- read.csv("HardHitData.csv")

## Remove NAs to clean the data
data <- data %>%
  filter(
    !is.na(ama_exit_speed)
  )

## Drop players with limited pro and amateur data
data <- data %>%
  drop_na(pro_hard_hit)

data <- data %>%
  filter(
    ama_pa >= 50
  )


## Separate Metal and Wood bat data to run statistical tests
metal <- data %>% filter(ama_bat_type == "Metal")
wood <- data %>% filter(ama_bat_type == "Wood")


## Plot Distributions & QQ Plot to determine normality
ggplot(data, aes(x = ama_hard_hit, fill = ama_bat_type)) +
  geom_histogram(aes(y = ..count..), position = "identity", bins = 30, alpha = 0.6, color = "black") +
  scale_fill_manual(values = c("blue", "green")) +
  labs(title = "Distribution of Amateur Hard-Hit Rate by Bat Type",
       x = "Amateur HH Rate",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = 'none')


ggplot(data, aes(sample = ama_hard_hit, color = ama_bat_type)) +
  scale_color_manual(values = c("blue", "green")) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ama_bat_type) +
  labs(title = "Q-Q Plot by Bat Type", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() + theme(legend.position = 'none')

## Check Variability with boxplots and BFL test

leveneTest(ama_hard_hit ~ ama_bat_type, data = data)

boxplot <- data %>%
  ggplot(aes(x = ama_hard_hit, y = ama_exit_speed, fill = ama_bat_type)) + geom_boxplot() + 
  scale_fill_manual(values=c("blue", "green"))+
  theme_minimal()
boxplot

## Wilcoxon Rank Sum Test to test if differences are significant
wilcox.test(ama_hard_hit~ama_bat_type, data = data)


## Create, apply, and visualize adjustment factor
summary_wood <- summary(data$ama_hard_hit[data$ama_bat_type == "Wood"])
summary_metal <- summary(data$ama_hard_hit[royals$ama_bat_type == "Metal"])

adjustment_factor <- mean(data$ama_hard_hit[data$ama_bat_type == "Wood"]) / 
  mean(data$ama_hard_hit[data$ama_bat_type == "Metal"])

data$adjusted_hard_hit <- ifelse(data$ama_bat_type == "Metal", 
                                 data$ama_hard_hit * adjustment_factor, 
                                 data$ama_hard_hit)


ggplot(data, aes(x = adjusted_hard_hit, fill = ama_bat_type)) +
  geom_histogram(aes(y = ..count..), position = "identity", bins = 30, alpha = 0.6, color = "black") +
  scale_fill_manual(values = c("blue", "green")) +
  labs(title = "Adjusted Distribution of Amateur Hard-Hit Rates by Bat Type",
       x = "Adjusted Amateur Hard-Hit Rate",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Correlation analysis before and after adjustment
cor_original <- cor(data$ama_hard_hit, data$pro_hard_hit, method = "pearson")
cor_adjusted <- cor(data$adjusted_hard_hit, data$pro_hard_hit, method = "pearson")

list(cor_original = cor_original, cor_adjusted = cor_adjusted)

ggplot(data = data, aes(x = ama_hard_hit, y = pro_hard_hit)) +
  geom_point(alpha = 0.6, color = "blue") + # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") + # Regression line with confidence interval
  labs(
    title = "Scatter Plot of Amateur vs Professional Hard-Hit Rates",
    x = "Amateur Hard-Hit Rate (%)",
    y = "Professional Hard-Hit Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12)
  )

ggplot(data = data, aes(x = adjusted_hard_hit, y = pro_hard_hit)) +
  geom_point(alpha = 0.6, color = "blue") + # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") + # Regression line with confidence interval
  labs(
    title = "Scatter Plot of Amateur vs Professional Hard-Hit Rates",
    x = "Adjusted Hard-Hit Rate (%)",
    y = "Professional Hard-Hit Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12)
  )

