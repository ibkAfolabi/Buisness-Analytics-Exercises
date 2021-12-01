
#Read in the data
auction.df <- read.csv("eBayAuctions.csv")


# convert numberical variable(Duration) to categorical variable in the same data frame
?cut
auction.df$Duration <- cut(auction.df$Duration, breaks = c(0,2.5,5,7.5,10),labels = c("A","B","C","D"))
head(auction.df)

# partition
set.seed(1)  
train.index <- sample(c(1:dim(auction.df)[1]), dim(auction.df)[1]*0.6)  
train.df <- auction.df[train.index, ]
valid.df <- auction.df[-train.index, ]

# To get the best BEST PRUNED TREE
library(rpart)
library(rpart.plot)
# ???	First create several trees using cross validation approach on the training data(this method 
cv.ct <- rpart(Competitive ~ ., data = train.df, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5, minbucket = 50,maxdepth = 7)
# use printcp() to print the table. 
printcp(cv.ct)


# 	Then display a table showing the shortlisted best trees and their complexity parameters, cross validation error(xerror) etc 

# 	You can then write code to prune the tree gotten using the lowest complexity parameter.this is not yet the best pruned tree its just the tree with the lowerest cross validation error
# prune by lower cp
pruned.ct <- prune(cv.ct,
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)
# 	Then to get the best pruned tree, we use the estimated standard error of the cross validation error(xstd) to prune the tree further
     # Go to the row with the lowest xerror, pick the lowest xerror and add its xstd to it 
     # Whatever you get, will then determine the tree you choose, how?
     # Go up the  to the row above the row in question(ie one with lowest xerror) and pick  a smaller tree with the lowest xerror.
#this code now helps me to pick and display the tree in row 4 above because it has xerror closest to 0.41328+0.024862=.0.438142 and is therefore the best pruned tree
set.seed(1)
cv.ct <- rpart(Competitive ~ ., data = train.df, method = "class", cp = 0.00001, minsplit = 1, xval = 5,minbucket = 50,maxdepth = 7)  # minsplit is the minimum number of observations in a node for a split to be attempted. xval is number K of folds in a K-fold cross-validation.
printcp(cv.ct)  # Print out the cp table of cross-validation errors. The R-squared for a regression tree is 1 minus rel error. xerror (or relative cross-validation error where "x" stands for "cross") is a scaled version of overall average of the 5 out-of-sample errors across the 5 folds.
pruned.ct <- prune(cv.ct, cp = 0.000010)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 




# 	You can run the default tree in R and see if it gives the best pruned-just to compare

default.ct <- rpart(Competitive ~ ., data = train.df, method = "class")
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

#Evaluating the best Prune Tree and the default tree using confusion matrix
library(caret)
# best prune tree: validation
pruned.ct.point.pred.valid <- predict(pruned.ct,valid.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(pruned.ct.point.pred.valid, as.factor(valid.df$Competitive))

# default tree: training
default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Competitive))



