# Titanic Part Two

# What is the accuracy on the Loess model?

library(caret)
library(tidyverse)

# Make sure Survived is a factor
train_set <- train_set %>% mutate(Survived = factor(Survived))
test_set <- test_set %>% mutate(Survived = factor(Survived))

# Train Loess model
set.seed(1)
fit_loess <- train(Survived ~ Fare,
                   method = "gamLoess",
                   data = train_set)

# Predict on test set
predictions <- predict(fit_loess, newdata = test_set)

# Accuracy
confusionMatrix(predictions, test_set$Survived)$overall["Accuracy"]

# What is the accuracy of your model (using age as the only predictor) on the test set ?
fit_logreg_a <- glm(Survived ~ Age, data = train_set, family = 'binomial')
survived_hat_a <- ifelse(predict(fit_logreg_a, test_set) >= 0, 1, 0)
mean(survived_hat_a == test_set$Survived)

# What is the accuracy of your model (using these four predictors) on the test set?

fit_logreg_b <- glm(Survived ~ Sex + Pclass + Fare + Age, data = train_set, family = 'binomial')
survived_hat_b <- ifelse(predict(fit_logreg_b, test_set) >= 0, 1, 0)
mean(survived_hat_b == test_set$Survived)

# What is the accuracy of your model (using all predictors) on the test set?

str(train_set)
fit_logreg_c <- glm(Survived ~ ., data = train_set, family = 'binomial')
survived_hat_c <- ifelse(predict(fit_logreg_c, test_set) >= 0, 1, 0)
mean(survived_hat_c == test_set$Survived)

# what is the optimal value of the number of neighbors k?

library(caret)
library(tidyverse)

# Ensure Survived is a factor
train_set <- train_set %>% mutate(Survived = factor(Survived))

# Set the seed
set.seed(6)

# Train kNN model
fit_knn <- train(Survived ~ .,
                 method = "knn",
                 data = train_set,
                 tuneGrid = data.frame(k = seq(3, 51, 2)),
                 trControl = trainControl(method = "cv", number = 10))

# View the optimal k
fit_knn$bestTune
k <- seq(3,51,2)
fit_knn9a <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
fit_knn9a$bestTune

# Plot the kNN model

ggplot(fit_knn9a)


set.seed(6)
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune