#### Exercise 14.2

# loading the data
course.df <- read.csv("Coursetopics.csv")

# create a binary incidence matrix
count.course.df <- course.df
incid.course.df <- ifelse(count.course.df > 0, 1, 0)
incid.course.mat <- as.matrix(incid.course.df[, -1])


library(arules)
#  convert the binary incidence matrix into a transactions database
course.trans <- as(incid.course.mat, "transactions")
inspect(course.trans)

# plot data
itemFrequencyPlot(course.trans)

# run apriori function, you can use the barchart you ust plot to have an idea of the support(item frequency, so you dont ust raise the support unnecessarily)
rules <- apriori(course.trans, 
                 parameter = list(supp= 10/365, conf = 0.3, target = "rules"))

# inspect rules
inspect(sort(rules, by = "lift"))

