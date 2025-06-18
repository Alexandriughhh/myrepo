# Cross validation comprehension check Q2

pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}

ind <- which(pvals <= 0.01)
length(ind)