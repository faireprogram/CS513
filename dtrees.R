# Daniel F Diaz
# Final Project Data Mining
# Performing Decision Tree Methods 
# Cardiovascular Disease 

install.packages("tree")
library(tree)
library('DMwR')
install.packages("partykit")
library("partykit")
library("rpart")


rm(list=ls())
cardio <-read.csv("heart-c.csv", header = TRUE)
View(cardio)
attach(cardio)

#replace na's in column vessel_color_flourosopy with 0 through 3
#Let's do knn imputation instead
cardio <- knnImputation(cardio, k=4)
View(cardio)

#make sure numbers are integers
cardio$vessel_color_flourosopy[167] <- 0
cardio$vessel_color_flourosopy[192] <- 2
View(cardio)
summary(cardio)

cardio$diam_narrowing <- as.character(cardio$diam_narrowing)
cardio$diam_narrowing[cardio$diam_narrowing == '<50'] <- 'NO'
cardio$diam_narrowing[cardio$diam_narrowing == '>50_1'] <- 'YES'

#need to convert back to factor from char
cardio$diam_narrowing <- as.factor(cardio$diam_narrowing)

View( cardio)

#shortening row name in chestpain
#colnames(cardio) <- c()
# Critical that changed to char as.character

cardio$chestpaintype <- as.character(cardio$chestpaintype)
cardio$chestpaintype[cardio$chestpaintype == "typ_angina"] <- "typ"
cardio$chestpaintype[cardio$chestpaintype == "atyp_angina"] <- "atyp"
cardio$chestpaintype[cardio$chestpaintype == "non_anginal"] <- "non"

#need to convert back to factor from char
cardio$chestpaintype <- as.factor(cardio$chestpaintype)
View(cardio)


#change colname
colnames(cardio)[8] <- "maxHrtRate"
colnames(cardio)[5] <- "chol"
colnames(cardio)[3] <- "chestPain"
colnames(cardio)[12] <- "vessel_color"


View(cardio)
summary(cardio)

tree.disease <- tree(diam_narrowing~.-diam_narrowing,cardio)
summary(tree.disease)

#try to do the same with rpart
#part.tree <- rpart(diam_narrowing~.-diam_narrowing,cardio,method = "class")
#plot(part.tree)
#text(part.tree,pretty=0)

#make text look nicer
plot(tree.disease)
text(tree.disease,pretty=0)
tree.disease
#nicer
#plot(as.party(tree.disease),tp_args=list(id =FALSE))


#See if we can improve/ simplify tree
#Computing test error ( not just training error)
set.seed(2)
train <- sample(1:nrow(cardio),152)
cardio.test <- cardio[-train,]

tree.disease2 <- tree(diam_narrowing~.-diam_narrowing,cardio,subset=train)
tree.pred <- predict(tree.disease2, cardio.test,type = "class")
table(tree.pred, cardio.test$diam_narrowing)

#Test error using test dataset 151 instances
# accuracy rate (71+43)/151 = .7549

#use cross validation to find optimal pruning level
set.seed(3)
cv.disease2 <- cv.tree(tree.disease2, FUN = prune.misclass)
names(cv.disease2)
cv.disease2

# dev list the error rate (deviance) for node sizes 
# smallest error rate for terminal node size = 6

#par(mfrow=c(1,2))
#Pictures of error rate using different number of terminal nodes

#use these pictures on slides
plot(cv.disease2$size,cv.disease2$dev, type ="b",ylab="error rate",xlab="number of nodes",main="Reduced error pruning")
#plot(cv.disease2$k,cv.disease2$dev, type ="b")

#Prune the tree using 6 terminal nodes
prune.disease2 <- prune.misclass(tree.disease2,best=6)
plot(prune.disease2)
text(prune.disease2,pretty=0 )

#How does pruned tree perform on test
tree.pred <- predict(prune.disease2, cardio.test,type = "class")
table(tree.pred,cardio.test$diam_narrowing)
#accuracy = 120/151 = .794
# 4% improvement pruning tree down to 6 terminal nodes

# Conclusion:

# Turns out the most important attribute in predicting heart disease is
# (Root Node) Thal: Thalassemia: improper oxygen transport 
# http://en.wikipedia.org/wiki/Thalassemia
# Depth 2: Chest pain ( severity of pain)
# Depth 2: Slope up (slope of peak exercise: measures exertion/ physical fitness)
# Depth 3: Vessel color
# Depth 3: Cholesterol level


