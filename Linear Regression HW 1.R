set.seed(1989) #if you are using R 3.5 or earlier
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

?GaltonFamilies

# Load necessary libraries
library(HistData)
library(tidyverse)

# Load the GaltonFamilies dataset
data("GaltonFamilies")

# Filter for daughters only
daughters <- GaltonFamilies %>%
  filter(gender == "female") %>%
  mutate(
    mother = mother,
    daughter = childHeight
  )

# Calculate means and standard deviations
mean_mother <- mean(daughters$mother)
sd_mother <- sd(daughters$mother)

mean_daughter <- mean(daughters$daughter)
sd_daughter <- sd(daughters$daughter)

# Calculate correlation coefficient
cor_mother_daughter <- cor(daughters$mother, daughters$daughter)

# Print results
cat("Mean of mothers' heights:", round(mean_mother, 2), "\n")
cat("Standard deviation of mothers' heights:", round(sd_mother, 2), "\n")
cat("Mean of daughters' heights:", round(mean_daughter, 2), "\n")
cat("Standard deviation of daughters' heights:", round(sd_daughter, 2), "\n")
cat("Correlation coefficient between mother and daughter heights:", round(cor_mother_daughter, 2), "\n")


# Calculate means and standard deviations
mean_mother <- mean(daughters$mother)
sd_mother <- sd(daughters$mother)

# Calculate summary statistics
mean_mother <- mean(daughters$mother)
sd_mother <- sd(daughters$mother)

mean_daughter <- mean(daughters$daughter)
sd_daughter <- sd(daughters$daughter)

r <- cor(daughters$mother, daughters$daughter)

# Mother's height
x <- 60

# Regression prediction
expected_daughter <- mean_daughter + r * (sd_daughter / sd_mother) * (x - mean_mother)

# Show result
round(expected_daughter, 2)


# Correlation between mother and daughter heights
r <- cor(daughters$mother, daughters$daughter)

# Percent of variability explained
r_squared_percent <- round(r^2 * 100, 2)
r_squared_percent


# Load libraries
library(HistData)
library(tidyverse)

# Load and filter for daughters
data("GaltonFamilies")
daughters <- GaltonFamilies %>%
  filter(gender == "female") %>%
  mutate(
    mother = mother,
    daughter = childHeight
  )

# Summary statistics
mean_x <- mean(daughters$mother)
mean_y <- mean(daughters$daughter)
sd_x <- sd(daughters$mother)
sd_y <- sd(daughters$daughter)
r <- cor(daughters$mother, daughters$daughter)

# Slope
beta1 <- r * (sd_y / sd_x)

# Intercept
beta0 <- mean_y - beta1 * mean_x

# Output results
cat("Slope (beta1):", round(beta1, 3), "\n")
cat("Intercept (beta0):", round(beta0, 3), "\n")
cat("Change in daughter's height for 1 inch increase in mother's height:", round(beta1, 3), "inches\n")


set.seed(1989, sample.kind = "Rounding")  # Use this line if using R 3.6 or later
library(HistData)
library(tidyverse)

data("GaltonFamilies")

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Now compute the standard deviation of mothers' heights
sd(female_heights$mother)

mean_daughters <- mean(female_heights$daughter)
mean_daughters

sd_daughters <- sd(female_heights$daughter)
sd_daughters

model <- lm(daughter ~ mother, data = female_heights)
r_squared <- summary(model)$r.squared
percent_variability <- r_squared * 100
percent_variability

mother_height <- 60
predicted_daughter_height <- intercept + slope * mother_height
predicted_daughter_height


