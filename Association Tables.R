# Purpose: Testing for association in 2 or 3 way tables


# Testing independence in 2-way tables ----------------------------------

data.cars <- mtcars
help(mtcars)

x1 <- mtcars$cyl
x2 <- mtcars$vs

table(x1)
table(x2)

# Test for association: use the chi-square test
data.table = table(x1,x2)
data.table


# Testing independence in 2-way tables ------------------------------------

chisq.test(data.table)
# strictly speaking, assumptions for the test are violated here 
# as cell entries are less than 5 in several cases

# When the sample size is low, we can apply the Fisher's exact test 
# instead of Chi-Square test.
fisher.test(data.table)

# Three categorical variables
data.ucb = UCBAdmissions
help("UCBAdmissions")

# let's ignore dept for a second
table.ucb <- margin.table(data.ucb, c(1, 2))
table.ucb

# for male applicants
1198 / (1198 + 1493)

# for female applicants
557 / (557 + 1278)

# let's run the chi-square test
chisq.test(table.ucb)


# Looking at tables more closely ------------------------------------------

# Department A
data.ucb[, , 1]

# for male applicants
512 / (512 + 313)

# for female applicants
89 / (89 + 19)

# For dept A, females applicants actually have a higher success rate

# Department F
data.ucb[, , 6]

# for male applicants
22 / (22 + 351)

# for female applicants
24 / (24 + 317)

dept <- c("A", "B", "C", "D", "E", "F")
admit <- matrix(0, nrow = 6, ncol = 2)

for (i in 1:6){
  foo <- data.ucb[,,i]
  admit[i, 1] = foo[1, 1] / (foo[1, 1] + foo[2, 1])
  admit[i, 2] = foo[1, 2] / (foo[1, 2] + foo[2, 2])
}

admit
row.names(admit) <- dept
colnames(admit) <- c("M", "F")
round(admit, 3)


# test for 3-way association
mantelhaen.test(UCBAdmissions)