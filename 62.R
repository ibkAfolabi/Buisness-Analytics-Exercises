#6.2a
sw.df <- read.csv("Tayko2.csv")
 table(sw.df$US)
 table(sw.df$Web_order)
 table(sw.df$GenderMale)
 table(sw.df$Address_is_res)

 # compute the average of spending by  US       ,,GenderMale,Address_is_res
   aggregate(sw.df[, 9], list(sw.df$US), mean)
   aggregate(sw.df[, 9], list(sw.df$US), sd)
 a
   # compute the average of spending by  Web_order      ,,GenderMle,Address_is_res
   aggregate(sw.df[, 9], list(sw.df$Web_order), mean)
   aggregate(sw.df[, 9], list(sw.df$Web_order), sd) 
  
   # compute the average of spending by  GenderMale      
   aggregate(sw.df[, 9], list(sw.df$GenderMale), mean)
   aggregate(sw.df[, 9], list(sw.df$GenderMale), sd)
   
   
   # compute the average of spending by  Address_is_res     
   aggregate(sw.df[, 9], list(sw.df$Address_is_res), mean)
   aggregate(sw.df[, 9], list(sw.df$Address_is_res), sd)
  
  # 6.2b
  
  plot(sw.df$Spending ~ sw.df$Freq, xlab = "Spending", ylab = "Freq")
  plot(sw.df$Spending ~ sw.df$last_update_days_ago, xlab = "Spending", ylab = "last_update_days_ago")
  
  # 6.2c
  #FITTING A PREDICTIVE MODEL FOR SPENDING 
  SW.df <- read.csv("Tayko2.csv")
  # use first 863 rows of data
  SW.df <- SW.df[1:2000, ]
  # select variables for regression
  selected.var <- c(9, 1, 2, 3, 5, 6, 7)
  # partition data
  set.seed(1) # set seed for reproducing the partition
  train.index <- sample(c(1:2000), 1400)
  train.df <- SW.df[train.index, selected.var]
  valid.df <- SW.df[-train.index, selected.var]
  # use lm() to run a linear regression of Price on all 11 predictors in the
  # training set.
  # use . after ~ to include all the remaining columns in train.df as predictors.
  SW.lm <- lm(Spending ~ ., data = train.df)
  # use options() to ensure numbers are not displayed in scientific notation.
  options(scipen = 999)
  summary(SW.lm)
  
  
  #6.2 iv. in backward, we start from all predictors and eliminate the lease useful predictor
  # use step() to run stepwise regression.
  # set directions = to either "backward", "forward", or "both".
  SW.lm.step <- step(SW.lm, direction = "backward")
  summary(SW.lm.step) # Which variables did it drop?
  SW.lm.step.pred <- predict(SW.lm.step, valid.df)
  accuracy(SW.lm.step.pred, valid.df$Spending)

  #6.2 v. show are predictions and prediction are calaculated for the first purchace in the validation set
  library(forecast)
  # use predict() to make predictions on a new set.
  SW.lm.pred <- predict(SW.lm, valid.df)
  
  some.residuals <- valid.df$Spending[1:1] - SW.lm.pred[1:1]
  data.frame("Predicted" = SW.lm.pred[1:1], "Actual" = valid.df$Spending[1:1],
             "Residual" = some.residuals)
  
  #6.2 vi. evaluate performance of model on validation set
  library(forecast)
  # use predict() to make predictions on a new set.
  SW.lm.pred <- predict(SW.lm, valid.df)
  
  some.residuals <- valid.df$Spending - SW.lm.pred
  data.frame("Predicted" = SW.lm.pred, "Actual" = valid.df$Spending,
             "Residual" = some.residuals)
  # use accuracy() to compute common accuracy measures.
  accuracy(SW.lm.pred, valid.df$Spending)
  
  
  #6.2 vii. histogram of  performance of model on validation set
  library(forecast)
  SW.lm.pred <- predict(SW.lm, valid.df)
  all.residuals <- valid.df$Spending- SW.lm.pred
   hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
  