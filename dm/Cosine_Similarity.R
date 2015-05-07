s1 <- "one sees clearly only with the heart anything essential is invisible to the eyes"
s2 <- "let my soul smile through my heart and my heart smile through my eyes that I may scatter rich smiles in sad hearts"

s1l <- unlist(strsplit(s1, " "))
s2l <- unlist(strsplit(s2, " "))

sl <- unique(c(s1l, s2l))

n1 <- NULL
n2 <- NULL
for (c in sl) {
  cp <- paste0("^", c, "$")
  n1 <- c(n1, sum(grepl(cp, s1l)))
  n2 <- c(n2, sum(grepl(cp, s2l)))
}

sum(n1 * n2) / (sqrt(sum(n1^2)) * sqrt(sum(n2^2)))
