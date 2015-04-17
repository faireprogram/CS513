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


set.seed(input$DT_SEED)
train <- sample(1:nrow(cardio),152)
cardio.test <- cardio[-train,]
tree.disease <- tree(diam_narrowing~.-diam_narrowing,cardio,subset=train)
#make text look nicer
plot(tree.disease)
text(tree.disease,pretty=0)


lvars$TREE_DESEASE<-tree.disease

