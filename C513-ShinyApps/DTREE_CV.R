# Daniel F Diaz
# Final Project Data Mining
# Performing Decision Tree Methods 
# Cardiovascular Disease 


library(tree)


rm(list=ls())
cardio <-read.csv("heart-c.csv", header = TRUE)

cardio$diam_narrowing <- as.character(cardio$diam_narrowing)
cardio$diam_narrowing[cardio$diam_narrowing == '<50'] <- 'NO'
cardio$diam_narrowing[cardio$diam_narrowing == '>50_1'] <- 'YES'

cardio$diam_narrowing <- as.factor(cardio$diam_narrowing)


#use cross validation to find optimal pruning level
set.seed(input$DT_SEED)

train <- sample(1:nrow(cardio),152)

cardio.test <- cardio[-train,]

cardio1 <<- cardio
tree.disease2 <- tree(diam_narrowing~.,cardio1)
#--------------------------TREE Desease two

cv.disease2 <- cv.tree(tree.disease2, FUN = prune.misclass)
lvars$TREE_CV <- cv.disease2
# dev list the error rate for node sizes 
# smallest error rate for terminal node size = 6

