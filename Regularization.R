# Regularization

set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# ID of top school and average score of the 10th school?
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% dplyr::select(id, size, score)

# Compare the median school size to the median school size of the top 10 schools based on score.

median(schools$size)

schools %>% top_n(10, score) %>% .$size %>% median()

# what is the median school size of the bottom 10 schools based on the score?

schools %>% top_n(-10, score) %>% .$size %>% median()

# What is the ID of the top school with regularization?
#What is the regularized score of the 10th school?

overall <- mean(sapply(scores, mean))
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


# What value of X gives the minimum RMSE?

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)

# Rank the schools based on the average obtained with the best  from Q6. Note that no small school is incorrectly included.
# What is the ID of the top school now?

alpha <- alphas[which.min(rmse)]  
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# Rerun what value of gives the minimum RMSE without removing the overall mean.

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)