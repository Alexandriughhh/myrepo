choices <- 5
Correct <- 1
probability <- Correct/choices
print(probability)

# Define probabilities
prob_correct <- 1 / 5
prob_incorrect <- 4 / 5

# Define points
points_correct <- 1
points_incorrect <- -0.25

# Calculate expected value
expected_value <- (prob_correct * points_correct) + (prob_incorrect * points_incorrect)

# Display result
print(expected_value)

# Expected value per question (from previous calculation)
expected_value_per_question <- (1/5 * 1) + (4/5 * -0.25)

# Total number of questions
total_questions <- 44

# Expected total score
expected_total_score <- expected_value_per_question * total_questions

# Display result
print(expected_total_score)

# Probabilities
p_correct <- 1/5
p_incorrect <- 4/5

# Outcomes
x_correct <- 1
x_incorrect <- -0.25

# Expected value (mean)
expected_value <- p_correct * x_correct + p_incorrect * x_incorrect

# Expected value of X squared
expected_value_sq <- (x_correct^2) * p_correct + (x_incorrect^2) * p_incorrect

# Variance and standard deviation for one question
variance_single <- expected_value_sq - expected_value^2
sd_single <- sqrt(variance_single)

# Standard error for 44 questions
n_questions <- 44
standard_error <- sd_single * sqrt(n_questions)

# Display result
print(standard_error)

# Step 1: Set known values
p_correct <- 1/5
p_incorrect <- 4/5
x_correct <- 1
x_incorrect <- -0.25
n <- 44

# Expected value per question
mu <- p_correct * x_correct + p_incorrect * x_incorrect

# Variance and SD per question
E_x2 <- (x_correct^2) * p_correct + (x_incorrect^2) * p_incorrect
variance <- E_x2 - mu^2
sd <- sqrt(variance)

# Total mean and standard error
total_mean <- mu * n
total_se <- sd * sqrt(n)

# Step 2: Calculate z-score for score >= 8
z <- (8 - total_mean) / total_se

# Step 3: Calculate probability using normal distribution
prob_8_or_more <- 1 - pnorm(z)

# Display result
print(prob_8_or_more)

set.seed(21)  # For reproducibility

# Parameters
n_students <- 10000
n_questions <- 44
p_correct <- 1/5
p_incorrect <- 4/5

# Simulate scores for 10,000 students
scores <- replicate(n_students, {
  # Simulate 44 guesses: TRUE if correct, FALSE if incorrect
  correct_guesses <- runif(n_questions) < p_correct
  # Assign scores: +1 for correct, -0.25 for incorrect
  sum(ifelse(correct_guesses, 1, -0.25))
})

# Estimate probability of scoring 8 or more
prob_8_or_more_sim <- mean(scores >= 8)

# Display result
print(prob_8_or_more_sim)

# Probabilities and point values
p_correct <- 1/4
p_incorrect <- 3/4
points_correct <- 1
points_incorrect <- 0

# Expected value
expected_value <- p_correct * points_correct + p_incorrect * points_incorrect

# Display result
print(expected_value)

p <- seq(0.25, 0.95, 0.05)
# Range of student skill probabilities
p_vals <- seq(0.25, 0.95, 0.05)

# Number of questions
n_questions <- 44

# Find the lowest p such that P(score > 35) > 0.80
for (p in p_vals) {
  prob_over_35 <- 1 - pbinom(35, size = n_questions, prob = p)
  if (prob_over_35 > 0.80) {
    cat("Lowest p where P(score > 35) > 80%:", p, "\n")
    break
  }
}
print(p)

# Probabilities
p_win <- 5 / 38
p_lose <- 33 / 38

# Payouts
payout_win <- 6
payout_lose <- -1

# Expected value
expected_value <- (p_win * payout_win) + (p_lose * payout_lose)

# Display result
print(expected_value)

# Probabilities
p_win <- 5 / 38
p_lose <- 33 / 38

# Payouts
payout_win <- 6
payout_lose <- -1

# Expected value
expected_value <- (p_win * payout_win) + (p_lose * payout_lose)

# Expected value of payout squared
expected_value_sq <- (payout_win^2) * p_win + (payout_lose^2) * p_lose

# Variance
variance <- expected_value_sq - expected_value^2

# Standard deviation (standard error for one bet)
standard_error <- sqrt(variance)

# Display result
print(standard_error)

# Expected value of one bet (from previous)
expected_value_one_bet <- (5 / 38) * 6 + (33 / 38) * (-1)

# Expected value of average payout over 500 bets
expected_value_avg_500 <- expected_value_one_bet

print(expected_value_avg_500)

# Probabilities
p_win <- 5 / 38
p_lose <- 33 / 38

# Payouts
payout_win <- 6
payout_lose <- -1

# Expected value of one bet
expected_value <- (p_win * payout_win) + (p_lose * payout_lose)

# Expected value of payout squared
expected_value_sq <- (payout_win^2) * p_win + (payout_lose^2) * p_lose

# Variance of one bet
variance <- expected_value_sq - expected_value^2

# Standard deviation of one bet
sd_one_bet <- sqrt(variance)

# Number of bets
n_bets <- 500

# Standard error of average payout over 500 bets
se_avg_500 <- sd_one_bet / sqrt(n_bets)

# Display result
print(se_avg_500)

# Probabilities
p_win <- 5 / 38
p_lose <- 33 / 38

# Payouts
payout_win <- 6
payout_lose <- -1

# Expected value of one bet
expected_value_one_bet <- (p_win * payout_win) + (p_lose * payout_lose)

# Expected value of sum of 500 bets
expected_sum_500 <- 500 * expected_value_one_bet

# Display result
print(expected_sum_500)

# Probabilities
p_win <- 5 / 38
p_lose <- 33 / 38

# Payouts
payout_win <- 6
payout_lose <- -1

# Expected value of one bet
expected_value <- (p_win * payout_win) + (p_lose * payout_lose)

# Expected value of payout squared
expected_value_sq <- (payout_win^2) * p_win + (payout_lose^2) * p_lose

# Variance of one bet
variance <- expected_value_sq - expected_value^2

# Standard deviation of one bet
sd_one_bet <- sqrt(variance)

# Number of bets
n_bets <- 500

# Standard error of sum of 500 bets
se_sum_500 <- sd_one_bet * sqrt(n_bets)

# Display result
print(se_sum_500)

# Probabilities
p_win <- 5 / 38
p_lose <- 33 / 38

# Payouts
payout_win <- 6
payout_lose <- -1

# Expected value of one bet
expected_value <- (p_win * payout_win) + (p_lose * payout_lose)

# Expected value of payout squared
expected_value_sq <- (payout_win^2) * p_win + (payout_lose^2) * p_lose

# Variance and SD of one bet
variance <- expected_value_sq - expected_value^2
sd_one_bet <- sqrt(variance)

# Number of bets
n_bets <- 500

# Expected value and standard error of sum of 500 bets
mean_sum <- expected_value * n_bets
se_sum <- sd_one_bet * sqrt(n_bets)

# Probability of losing money (sum ≤ 0)
prob_lose_money <- pnorm(0, mean = mean_sum, sd = se_sum)

# Display result
print(prob_lose_money)

