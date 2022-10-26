# Purpose: Using Market Basket Analysis


# Importing data ----------------------------------------------------------

library("arules")

# Epub data
data("Epub") # load the transaction database
Epub  # N and p
summary(Epub) # note that density is just 0.18%, which means 99.82% of the entries are zero ---  that is how sparse the database is
# summary also gives a lot of information: N and p, most frequent items,
# item size distribution of transactions, average number of transactions, etc.


# Exploring the data ------------------------------------------------------

# We see that the data set contains transaction IDs and in addition time stamps (using class POSIXct) for the transactions. This additional information can be used for analyzing the data set
year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y")
table(year)

Epub2003 <- Epub[year == "2003"]
length(Epub2003)
image(Epub2003)
# The plot is a direct visualization of the binary incidence matrix where the the dark dots represent the ones in the matrix. From the plot we see that the items in the data set are not evenly distributed.

transactionInfo(Epub2003[size(Epub2003) > 20])
inspect(Epub2003[1:5,]) # to view transactions, use the inspect function
as(Epub2003[1:5], "list")

EpubTidLists <- as(Epub, "tidLists")
as(EpubTidLists[1:3], "list")

foo <- table(size(Epub)) # size distribution of transactions
length(foo) # number of different sizes
sum(foo[2:36])/sum(foo) # fraction of transactions with 2+ items
hist(size(Epub))
summary(size(Epub))

# apriori algorithm
rules <- apriori(Epub, parameter = list(supp=0.01, conf=0.01))
# find all rules with support>1% and confidence >1%
length(rules)  # how many such association rules are there?
inspect(rules) # the rules are actually meaningless as lhs is empty
# to remove meaningless rules, we take subset
# with the requirement that the lhs has partial match with "doc"
# this makes sense since all items begin with "doc"
rules1 <- subset(rules, subset = lhs %pin% "doc")
length(rules1) 

# which item sets lead to doc_11d? (most frequent item)
rules <- apriori(Epub,appearance = list(default = "lhs", rhs = "doc_11d"),
                 parameter = list(supp = 0.001, conf = 0.001))
# we specify under appearance that rhs must be doc_11d
# note that we have reduced support and confidence threshold 
length(rules)  # how many such association rules are there?
inspect(rules) # views the rules
# to remove meaningless rules, we take subset with the requirement that the lhs has partial match with "doc" this makes sense since all items begin with "doc"
rules1 <- subset(rules, subset = lhs %pin% "doc")
length(rules1) 
inspect(rules1) # these rules make sense
# note that we have support, confidence, and lift for each rules


# which item sets do doc_11d lead to?
rules <- apriori(Epub,appearance = list(default = "rhs", lhs = "doc_11d"),
                parameter = list(supp = 0.001, conf = 0.001))
# we specify under appearance that lhs must be doc_11d
# note that we have reduced support and confidence threshold 

rules2 <- subset(rules, subset = lhs %pin% "doc")
length(rules2) 
inspect(rules2) # these rules make sense
# note that we have support, confidence, and lift for each rules