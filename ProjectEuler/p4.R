# Largest palindrome product
# Problem 4
# A palindromic number reads the same both ways.
# The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
#
# Find the largest palindrome made from the product of two 3-digit numbers.

revable <- function(n) {
  n <- noquote(strsplit(as.character(n), NULL)[[1]])
  l <- length(n)
  c <- floor(l / 2)
  n1 <- n[1:c]
  n1r <- rev(n1)
  n1r <- as.numeric(paste(n1r, collapse=''))
  n2 <- n[(l-c+1):l]
  n2 <- as.numeric(paste(n2, collapse=''))
  return (n1r == n2)
}
# revable(323456456)

x <- 100:999
y <- 100:999
z <- unique(unlist(lapply(x, function(x) x*y)))
z <- z[order(z)]

for (i in length(z):1) {
  print(z[i])
  if (revable(z[i])) {
    print(z[i])
    break
  }
}
