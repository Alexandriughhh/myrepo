set.seed(16)
act_scores <- rnorm(10000, mean = 20.9, sd = 5.7)
sd(act_scores)

num_perfect_scores <- sum(act_scores >= 36)
print(num_perfect_scores)
prob_over_30 <- mean(act_scores > 30)
print(prob_over_30)

prob_less_10 <- mean(act_scores <= 10)
print(prob_less_10)

# Step 1: Create a sequence from 1 to 36
x <- 1:36

# Step 2: Calculate the values of the normal distribution PDF
f_x <- dnorm(x, mean = 20.9, sd = 5.7)

# Step 3: Plot the values
plot(x, f_x, type = "l", main = "Normal Distribution PDF",
     xlab = "ACT Score", ylab = "Density", col = "blue", lwd = 2)

# Calculate the probability that Z > 2
prob_z_greater_2 <- 1 - pnorm(2)

# Print the result
print(prob_z_greater_2)

mean_act <- 20.9
sd_act <- 5.7
z <- 2

score <- mean_act + z * sd_act
print(score)

# Calculate mean and standard deviation from act_scores
mean_act <- mean(act_scores)
sd_act <- sd(act_scores)

# Calculate the 97.5th percentile
percentile_975 <- qnorm(0.975, mean = mean_act, sd = sd_act)

# Print the result
print(percentile_975)

# Calculate mean and SD from act_scores
mean_act <- mean(act_scores)
sd_act <- sd(act_scores)

# Define the CDF function for ACT scores
cdf_act <- function(x) {
  pnorm(x, mean = mean_act, sd = sd_act)
}

# Apply the function to the range 1 to 36
x_values <- 1:36
cdf_values <- sapply(x_values, cdf_act)

# Print the results
print(cdf_values)

# Calculate mean and SD from act_scores
mean_act <- mean(act_scores)
sd_act <- sd(act_scores)

# Define CDF function
cdf_act <- function(x) pnorm(x, mean = mean_act, sd = sd_act)

# Sequence of possible scores
x_values <- 1:36

# Calculate CDF values
cdf_values <- sapply(x_values, cdf_act)

# Find the minimum integer score where CDF >= 0.95
min_score <- x_values[which(cdf_values >= 0.95)[1]]

print(min_score)

mean_act <- 20.9
sd_act <- 5.7

percentile_95 <- qnorm(0.95, mean = mean_act, sd = sd_act)
print(percentile_95)

# Create a sequence of probabilities from 0.01 to 0.99
p <- seq(0.01, 0.99, 0.01)

# Calculate sample quantiles for act_scores
sample_quantiles <- quantile(act_scores, probs = p)

# Print the quantiles (optional)
print(sample_quantiles)

# To find the percentile of a score 26, 
# find the proportion of scores less than or equal to 26
percentile_26 <- mean(act_scores <= 26)

# Convert to percentage for readability
percentile_26_percent <- percentile_26 * 100

print(percentile_26_percent)

# Define the probability sequence
p <- seq(0.01, 0.99, 0.01)

# Calculate sample quantiles from act_scores
sample_quantiles <- quantile(act_scores, probs = p)

# Calculate theoretical quantiles from normal distribution
theoretical_quantiles <- qnorm(p, mean = 20.9, sd = 5.7)

# Make the QQ-plot
plot(theoretical_quantiles, sample_quantiles,
     main = "QQ-Plot of ACT Scores vs Normal Distribution",
     xlab = "Theoretical Quantiles",
     ylab = "Sample Quantiles",
     pch = 19, col = "blue")

# Add a reference line y = x
abline(a = 0, b = 1, col = "red", lwd = 2)



