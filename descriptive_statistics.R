# Packed data
c <- read.csv(file.choose())
str(c)
summary(c)

table(c)
range(c[,1])
low_val <- 148.5
high_val <- 151
step_val = 0.5
c_break <- seq(low_val, high_val, by=step_val)
class(c_break)
class(c$Weight)
names(c)
x <- cut(c$Weight, c_break)
table(x)
is.na(x)

Freq_table <- data.frame(table(x))
Freq_table$percentage <- round(Freq_table$Freq,1) / sum(Freq_table$Freq)*100
Freq_table

Freq_table$cumml <- cumsum(Freq_table$percentage)

seg_data <- read.csv(file.choose())

seg_data <- seg_data[,-c(1)]
head(seg_data)

set.seed(12345)
train_cases <- sample(1:nrow(seg_data), nrow(seg_data) * 0.7)
train_cases

train <- seg_data[train_cases,]
test <- seg_data[-train_cases,]

dim(train)
dim(test)

library("e1071")

seg_nb <- naiveBayes(segment ~ ., data = train)
seg_nb

test$predict <- predict(seg_nb, test)


install.packages("rpart")
library(rpart)
# Heart data
h_data <- read.csv(file.choose())
names(h_data)[1] <- "age"

sample_index <- sample(1:nrow(h_data), nrow(h_data)/3, replace = FALSE)

train1 <- h_data[-sample_index, ]
test1 <- h_data[sample_index, ]


fit1 <- rpart(c ~ ., method="class", data = train1)

install.packages("rpart.plot")
# To visualize the tree
library(rpart.plot)
rpart.plot(fit1, type=4, extra=2, cex=0.8)
pred <- predict(fit1, test1, type="class")

pred

test1$prediction <- pred


# Simply use table
table(test1$prediction, test1$c)
mean(test1$prediction == test1$c)

#####################################    KNN   #################################
# Heart data
heart_data <- read.csv(file.choose())
names(heart_data)[1] <- "age"

install.packages("caret", dependencies = TRUE)
library(caret)
install.packages("stringi")


heart_data$c <- as.factor(heart_data$c)
heart_data$chest.pain <- as.factor(heart_data$chest.pain)
heart_data$sex <-as.factor(heart_data$sex)


heart_data <- na.omit(heart_data)

set.seed(1234)
sm1 <- sample(1:nrow(heart_data), nrow(heart_data)/3, replace =FALSE)
test1 <- heart_data[sm1,]
train1 <- heart_data[-sm1, ]

knnmd <- knn3(c~.,data = train1, k=15)
pred1 <- predict(knnmd, test1, type = "class")
table(pred1, test1$c)

list1 <- list()
ind <- 1
for(i in seq(1,100, 5)){
  knnmd <- knn3(c~., train1, k=i)
  pred1 <- predict(knnmd, test1, type="class")
  list1[ind] <- mean(pred1 == test1$c)
  ind <- ind + 1
}
plot(seq(1,100,5) , unlist(list1), type="l")
# the Bigger the accuracy tge better, but beware not too big of a k issuitable

library(e1071)

x <- train1[,-14]
y <- train1[,14]

obj1 <- tune.knn(x,y,k=seq(40, 60, 2))

summary(obj1)


############################## Association Rules #######################
install.packages("arules")
library(arules)

# Data groceries
data("Groceries")
inspect(head(Groceries))
summary(Groceries)
arules_model <- apriori(Groceries, parameter = list(sup=0.01, conf=0.3))
arules_model
inspect(arules_model[1:20])
itemFrequencyPlot(Groceries, topN = 15,
                  main = "Item Distribution",
                  type = "absolute", ylab = "Frequency")

inspect(subset(arules_model, support > 0.05))
inspect(subset(arules_model, lift > 2))


## Most and least popular items ----------------

# Most popular items
itemFrequencyPlot(Groceries,type="relative",
                  topN=10,horiz=TRUE,col='steelblue3')


# Least popular items
par(mar=c(2,10,2,2), mfrow=c(1,1))

barplot(sort(table(unlist(LIST(Groceries))))[1:10],
        horiz = TRUE,las = 1,col='orange')


### Cross tables by index -------------------------

# Contingency table
tbl <-crossTable(Groceries)
tbl[1:4,1:4]

# Sorted  table
tbl = crossTable(Groceries, sort = TRUE)
tbl[1:4,1:4]

### Cross tables by item names -------------------------

## Contingency tables

# Counts
tbl['whole milk','flour']

##### Choose parameters arules ---------------------------------------------------------


# Set of confidence levels 
confidenceLevels <- seq(from=0.1, to=0.9, by =0.1)

# Create empty vector
rules_sup0005 <- NULL

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  rules_sup0005[i] =
    length(apriori(Groceries,
                   parameter=list(supp=0.005,
                                  conf=confidenceLevels[i],
                                  target="rules")))
}
## Plotting  -----------------------------------
library(ggplot2)
# Number of rules found with a support level of 0.5%
qplot(confidenceLevels, rules_sup0005,
      geom=c("point", "line"),xlab="Confidence level",
      ylab="Number of rules found") +
  theme_bw()

# Subsetting rules
inspect(subset(rules, subset =
                 items %in% c("soft cheese","whole milk") &
                 confidence >.95))

#### Interactive inspection --------------------------------------------
rules = apriori(Groceries,parameter = list(supp=.001,conf=.5,minlen=2,target='rules'))


# DT : Datatable inspection

install.packages("arulesViz")
library(arulesViz)

inspectDT(rules)


# Plot rules as scatterplot -----------------------------
plot(rules, method = "scatterplot",engine = "html")


# Plot rules as graph
plot(rules, method = "graph",engine = "html")


# Other types of plots using method :
plot(rules, method = "two-key plot",engine = "html")  # two-key plot

plot(rules, method = "matrix",engine = "html")

## Interactive graphs ------------------------

# Plot rules as graph "
# The engine and the method
plot(rules, method = "graph",engine = "html")


## Interactive subgraphs --------------------------------------

#Sorting extracted rules
# Top 10 rules with highest confidence
top10_rules_Groceries =head(sort(rules,by = "confidence"), 10)

inspect(top10_rules_Groceries)

# Plot the top 10 rules
plot(top10_rules_Groceries,method = "graph", engine = "html")


# RuleExploring Groceries :
ruleExplorer(rules)


