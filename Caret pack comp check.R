library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat) 

plot(fit)

text(fit)

# Load required packages
library(tidyverse)
library(caret)
library(rpart)  # for classification trees
library(dslabs)

# Load the dataset
data("tissue_gene_expression")

# Set seed for reproducibility
set.seed(1991)

# Define tuning grid for cp values
cp_grid <- data.frame(cp = seq(0, 0.1, 0.01))

# Fit classification tree using caret and rpart
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y,
             method = "rpart",
             tuneGrid = cp_grid)

# Plot accuracy vs cp
ggplot(fit)

fit$bestTune

#rerun with method = 'rpart'

library(tidyverse)
library(caret)
library(rpart)
library(dslabs)

data("tissue_gene_expression")

# Set seed
set.seed(1991)

# Define cp grid
cp_grid <- data.frame(cp = seq(0, 0.1, 0.01))

# Train the model with custom control for minsplit = 0
fit <- train(tissue_gene_expression$x, 
             tissue_gene_expression$y,
             method = "rpart",
             tuneGrid = cp_grid,
             control = rpart.control(minsplit = 0))

# Get best model
fit$bestTune

# Predict on training data (or use resamples if using CV)
y_hat <- predict(fit, tissue_gene_expression$x)

# Confusion matrix
confusion <- confusionMatrix(y_hat, tissue_gene_expression$y)

# View accuracy
confusion$overall["Accuracy"]

set.seed(1991)
data("tissue_gene_expression")

fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)

confusionMatrix(fit_rpart)

plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

# What value of mtry maximizes accuracy?

set.seed(1991)
library(randomForest)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

library(tidyverse)
library(caret)
library(dslabs)
library(randomForest)

data("tissue_gene_expression")

# Set seed
set.seed(1991)

# Define mtry tuning grid
mtry_grid <- data.frame(mtry = seq(50, 200, 25))

# Train Random Forest model
fit <- train(tissue_gene_expression$x, 
             tissue_gene_expression$y,
             method = "rf",
             tuneGrid = mtry_grid,
             nodesize = 1)
fit$bestTune



