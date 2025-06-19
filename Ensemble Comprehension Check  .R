#Ensemble Comprehension Check 

# Load required libraries
library(caret)
library(dslabs)
library(tidyverse)

# Load the data
set.seed(1)
data("mnist_27")

# Define model list
models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

# Install any missing packages (optional safety step)
required_pkgs <- c("naivebayes", "gam", "MASS", "randomForest", "klaR")
installed <- rownames(installed.packages())
for (pkg in required_pkgs) {
  if (!(pkg %in% installed)) install.packages(pkg)
}

# Train all models
fits <- lapply(models, function(model){ 
  print(paste("Training:", model))
  train(y ~ ., method = model, data = mnist_27$train)
})

# Assign model names to the fits list
names(fits) <- models

# Get matrix of predictions (each column = one model)
pred_matrix <- sapply(fits, function(fit) {
  predict(fit, newdata = mnist_27$test)
})
dim(pred_matrix)

# Compute accuracy for each model on the test set. 
# True labels
y_true <- mnist_27$test$y

# Compute accuracy for each model
accuracies <- sapply(fits, function(fit) {
  y_pred <- predict(fit, newdata = mnist_27$test)
  mean(y_pred == y_true)
})
mean_accuracy <- mean(accuracies)
mean_accuracy

# build an ensemble prediction by majority vote and compute the accuracy of the ensemble.
# Matrix of predictions from all models
pred_matrix <- sapply(fits, function(fit) {
  predict(fit, newdata = mnist_27$test)
})
# For each row (test sample), count how many models voted "7"
ensemble_preds <- ifelse(rowMeans(pred_matrix == "7") > 0.5, "7", "2")
# Compare to true labels
ensemble_accuracy <- mean(ensemble_preds == mnist_27$test$y)
ensemble_accuracy

# How many of the individual methods do better than the ensemble?
# Count how many individual models did better than the ensemble
sum(accuracies > ensemble_accuracy)

print(accuracies)

# what is the mean of these training set accuracy estimates?
# Get best training set accuracy (from CV) for each model
train_accuracies <- sapply(fits, function(fit) {
  max(fit$results$Accuracy)
})
mean(train_accuracies)

# Now let's only consider the methods with a minimum accuracy estimate of greater than or equal to 0.8 when constructing the ensemble.
# CV-based accuracy for each model
train_accuracies <- sapply(fits, function(fit) {
  max(fit$results$Accuracy)
})
high_perf_models <- names(train_accuracies[train_accuracies >= 0.8])
# Subset fits to only high-performing models
selected_fits <- fits[high_perf_models]

# Generate predictions
selected_preds <- sapply(selected_fits, function(fit) {
  predict(fit, newdata = mnist_27$test)
})
# Vote 7 if >= 50% of selected models predict 7
ensemble_selected <- ifelse(rowMeans(selected_preds == "7") >= 0.5, "7", "2")
accuracy_selected_ensemble <- mean(ensemble_selected == mnist_27$test$y)
accuracy_selected_ensemble

