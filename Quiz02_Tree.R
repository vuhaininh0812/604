# import the data
df <- read.csv ("eBayAuctions.csv")
dim (df)
head(df)
colnames (df) <- c("category", "currency", "sellerrating", "duration", "enday", "closeprice", "openprice", "competitive")
str(df)
summary (df)

# factoring the categorical variables
df$category <- as.factor (df$category)
df$currency <- as.factor (df$currency)
df$duration <- as.factor (df$duration)
df$enday <- as.factor (df$enday)
df$competitive <- factor (df$competitive, levels = c(0,1), labels = c("not_competitive","competitive"))


# split the data
set.seed (2509)
train.index <- sample (1:dim(df)[1], dim(df)[1] * 0.8)
length(train.index)
train.df <- df [train.index,]
test.df <- df[-train.index,]

# decision tree
library(rpart)
library(rpart.plot)
tree1 <- rpart (competitive ~ . , data=train.df, method = "class", minbucket = 50, maxdepth = 7,
                cp =0, minsplit = 1, xval=5)
length(tree1$frame$var[tree1$frame$var == "<leaf>"])
prp(tree1,type = 2, extra = 1, split.font = 1, varlen = -10, box.palette=c("red", "green"))
printcp (tree1)
plotcp(tree1)
cp1 <- tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"]

# prune tree
tree2 <- prune (tree1, cp = cp1)
length(tree2$frame$var[tree2$frame$var == "<leaf>"])
prp(tree2,type = 2, extra = 1, split.font = 1, varlen = -10, box.palette=c("red", "green"))

# decision tree without close price
tree3 <- rpart (competitive ~ . - closeprice , data=train.df, method = "class", minbucket = 50, maxdepth = 7,
                cp =0, minsplit = 1, xval=5)
length(tree3$frame$var[tree3$frame$var == "<leaf>"])
prp(tree3,type = 2, extra = 1, split.font = 1, varlen = -10, box.palette=c("red", "green"))
printcp (tree3)
plotcp(tree3)
cp2 <- tree3$cptable[which.min(tree3$cptable[,"xerror"]),"CP"]

# prune tree
tree4 <- prune (tree3, cp = cp2)
length(tree4$frame$var[tree4$frame$var == "<leaf>"])
prp(tree4,type = 2, extra = 1, split.font = 1, varlen = -10, box.palette=c("red", "green"))
printcp(tree4)

# scatter plot
library (ggplot2)
ggplot () + geom_point (data = train.df[train.df$competitive=="competitive",],aes(x=log(openprice), y=log(sellerrating)),color="red") + 
  geom_point (data = train.df[train.df$competitive=="not_competitive",],aes(x=log(openprice), y=log(sellerrating)),color="blue") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_vline(aes(xintercept=log(1.8)),color="orange", size=1.5) +
  geom_segment(aes(x=log(1.8), y=log(2370),xend=7, yend=log(2370)),color="purple", size=1.5) 

# confusion matrix decision tree
library(caret)
confusionMatrix(predict(tree4, train.df, type ="class"), train.df$competitive)
confusionMatrix(predict(tree4, test.df, type ="class"), test.df$competitive)

# random forest
library(randomForest)
forest1 <- randomForest(competitive ~ . -closeprice, data=train.df, ntree = 500, 
                        mtry = 4, nodesize = 5, importance = TRUE, parms = list(loss = lossmatrix)) 
varImpPlot(forest1)

# confusion matrix random forest
confusionMatrix(predict(forest1, train.df), train.df$competitive)
confusionMatrix(predict(forest1, test.df), test.df$competitive)

