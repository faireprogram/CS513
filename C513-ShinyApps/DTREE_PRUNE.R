# Daniel F Diaz
# Final Project Data Mining
# Performing Decision Tree Methods 
# Cardiovascular Disease 


library(tree)


rm(list=ls())
cardio <-read.csv("heart-c.csv", header = TRUE)
#attach(cardio)

cardio$diam_narrowing <- as.character(cardio$diam_narrowing)
cardio$diam_narrowing[cardio$diam_narrowing == '<50'] <- 'NO'
cardio$diam_narrowing[cardio$diam_narrowing == '>50_1'] <- 'YES'

cardio$diam_narrowing <- as.factor(cardio$diam_narrowing)


set.seed(input$DT_SEED)
train <- sample(1:nrow(cardio),152)
cardio.test <- cardio[-train,]

tree.disease2 <- tree(diam_narrowing~.-diam_narrowing,cardio,subset=train)


#Prune the tree using 6 terminal nodes
prune.disease2 <- prune.misclass(tree.disease2,best=input$PRUNE_K)

lvars$TREE_PRUNE <- prune.disease2

