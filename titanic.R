library(tidyverse)
library(titanic)

# Prepare the data
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

# Calculate mean and standard deviation
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

# Create the QQ plot with the identity line
p <- ggplot(titanic, aes(sample = Age)) +
  geom_qq() +
  geom_abline(slope = 1, intercept = 0, color = "red") # Add identity line

# Add the dparams to the QQ plot
p + geom_qq(dparams = params)

library(tidyverse)
library(titanic)

# Prepare the data
titanic <- titanic_train %>%
  select(Survived, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Sex = factor(Sex))

# Default bar plot for 'Survived'
ggplot(titanic, aes(x = Survived)) + 
  geom_bar() +
  ggtitle("Bar plot of Survival")

# Bar plot for 'Survived' grouped by 'Sex' using position_dodge
ggplot(titanic, aes(x = Survived, fill = Sex)) + 
  geom_bar(position = position_dodge()) +
  ggtitle("Bar plot of Survival by Sex")


library(tidyverse)
library(titanic)

# Prepare the data
titanic <- titanic_train %>%
  select(Survived, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Sex = factor(Sex))

# Density plot of age filled by survival status
ggplot(titanic, aes(x = Age, fill = Survived)) + 
  geom_density(alpha = 0.2) + 
  scale_y_continuous(name = "Count") + 
  ggtitle("Density Plot of Age by Survival Status") +
  theme_minimal()

library(tidyverse)
library(titanic)

# Prepare the data
titanic <- titanic_train %>%
  select(Survived, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Sex = factor(Sex))

# Default bar plot for 'Survived'
ggplot(titanic, aes(x = Survived)) + 
  geom_bar() +
  ggtitle("Bar plot of Survival")

# Bar plot for 'Survived' grouped by 'Sex' using position_dodge
ggplot(titanic, aes(x = Survived, fill = Sex)) + 
  geom_bar(position = position_dodge()) +
  ggtitle("Bar plot of Survival by Sex")


library(tidyverse)
library(titanic)

# Prepare the data
titanic <- titanic_train %>%
  select(Survived, Fare, Age, Sex, SibSp, Parch) %>%
  mutate(Survived = factor(Survived)) %>%
  filter(Fare > 0)  # Filter out rows where Fare is 0

# Boxplot of fare grouped by survival status with log2 transformation and jittered data points
ggplot(titanic, aes(x = Survived, y = Fare, color = Survived)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width = 0.1) + 
  scale_y_continuous(trans = "log2") +  # Apply log2 transformation to Fare
  ggtitle("Boxplot of Fare by Survival Status") +
  ylab("Fare (Log2 Transformed)") +
  theme_minimal()

library(tidyverse)
library(titanic)

# Prepare the data
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived), Pclass = factor(Pclass))

# 1. Basic barplot of passenger class filled by survival
ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
  geom_bar() + 
  ggtitle("Passenger Class Filled by Survival") +
  ylab("Count") +
  theme_minimal()

# 2. Barplot of passenger class filled by survival using position_fill() for relative proportions
ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
  geom_bar(position = "fill") + 
  ggtitle("Passenger Class Filled by Survival (Relative Proportions)") +
  ylab("Proportion") +
  theme_minimal()

# 3. Barplot of survival filled by passenger class using position_fill() for relative proportions
ggplot(titanic, aes(x = Survived, fill = Pclass)) + 
  geom_bar(position = "fill") + 
  ggtitle("Survival Filled by Passenger Class (Relative Proportions)") +
  ylab("Proportion") +
  theme_minimal()

library(tidyverse)
library(titanic)

# Prepare the data
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived), Pclass = factor(Pclass), Sex = factor(Sex))

# Create the density plot grid
ggplot(titanic, aes(x = Age, fill = Survived)) + 
  geom_density(alpha = 0.2) + 
  scale_y_continuous("Count") + 
  facet_grid(Sex ~ Pclass) + 
  theme_minimal() + 
  ggtitle("Density of Age by Survival Status, Faceted by Sex and Passenger Class") 
