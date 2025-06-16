library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

library(caret)


length(y_hat)
length(y)
library(dplyr)

# Determine majority sex by class type
majority_sex <- dat %>%
  group_by(type) %>%
  summarize(predicted_sex = ifelse(mean(sex == "female") > 0.5, "female", "male"))

# Join predicted values back to original dataset
dat_predicted <- dat %>%
  left_join(majority_sex, by = "type")

# Now extract y and y_hat, and ensure same length
y     <- dat_predicted$sex
y_hat <- dat_predicted$predicted_sex

# Check again
length(y)
length(y_hat)

library(caret)

# Convert to factors with same levels
y     <- factor(y, levels = c("male", "female"))
y_hat <- factor(y_hat, levels = c("male", "female"))

# Confusion matrix (optional)
table(y_hat, y)

# Metrics
sensitivity(y_hat, y)
specificity(y_hat, y)
mean(y == "female")  # prevalence

summary(y)
summary(y_hat)

any(is.na(y))      # Should return TRUE if there are missing values
any(is.na(y_hat))  # Same

library(dplyr)
library(caret)

# Recreate dataset
data(dat)  # Only if dat is not already loaded

# Step 1: Create the rule — majority sex by type
majority_sex <- dat %>%
  group_by(type) %>%
  summarize(predicted_sex = ifelse(mean(sex == "Female") > 0.5, "Female", "Male"))

# Step 2: Join prediction with original data
dat_predicted <- dat %>%
  left_join(majority_sex, by = "type") %>%
  filter(!is.na(sex) & !is.na(predicted_sex))  # Drop any accidental NAs

# Step 3: Extract y and y_hat
y     <- factor(dat_predicted$sex, levels = c("Male", "Female"))
y_hat <- factor(dat_predicted$predicted_sex, levels = c("Male", "Female"))

# Step 4: Metrics
confusion <- table(y_hat, y)
confusion

sensitivity(y_hat, y)
specificity(y_hat, y)
mean(y == "Female")  # prevalence

head(dat)

# Recreate dataset
data(dat)  # Only if dat is not already loaded

# Step 1: Create the rule — majority sex by type
majority_sex <- dat %>%
  group_by(type) %>%
  summarize(predicted_sex = ifelse(mean(sex == "Female") > 0.5, "Female", "Male"))

# Step 2: Join prediction with original data
dat_predicted <- dat %>%
  left_join(majority_sex, by = "type") %>%
  filter(!is.na(sex) & !is.na(predicted_sex))  # Drop any accidental NAs

# Step 3: Extract y and y_hat
y     <- factor(dat_predicted$sex, levels = c("Male", "Female"))
y_hat <- factor(dat_predicted$predicted_sex, levels = c("Male", "Female"))

# Step 4: Metrics
confusion <- table(y_hat, y)
confusion

sensitivity(y_hat, y)
specificity(y_hat, y)
mean(y == "Female")  # prevalence

