# Load the constraint data
con_age <- read.csv("data/SimpleWorld/age.csv")
con_sex <- read.csv("data/SimpleWorld/sex.csv")
cons <- cbind(con_age, con_sex)

cons # print the constraint data to screen

## To load the individual level data, enter the following:
ind <- read.csv("data/SimpleWorld/ind-full.csv")

target <- list(as.matrix(con_age[1,]), as.matrix(con_sex[1,]))
descript <- list(1, 2)
ind$age_cat <- cut(ind$age, breaks = c(0, 50, 100))
seed <- table(ind[c("age_cat", "sex")])

summary(ind$age_cat)



library(mipfp) # install.packages("mipfp") is a prerequisite
res <- Ipfp(seed, descript, target)
res$x.hat # the result for the first zone

###Alternative approach

A <- t(cbind(model.matrix(~ ind$age_cat - 1),
             model.matrix(~ ind$sex - 1)[, c(2, 1)]))

cons <- apply(cons, 2, as.numeric) # convert to numeric data

library(ipfp) # install.packages("ipfp") is a prerequisite
weights <- apply(cons, 1, function(x) ipfp(x, A, x0 = rep(1, 5)))
weights[,1] # result for the first zone
