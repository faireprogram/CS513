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




#How does pruned tree perform on test

#accuracy = 120/151 = .794
# 4% improvement pruning tree down to 6 terminal nodes

#--------------------------------ERRORATE
loop_predict_dtree_prune <- function() {
  list_n <- {}
  num_k <- 1
  for (i in 2:input$PRUNE_K) {
    

    set.seed(input$DT_SEED)
    train <- sample(1:nrow(cardio),152)
    cardio.test <- cardio[-train,]
    
    tree.disease2 <- tree(diam_narrowing~.-diam_narrowing,cardio,subset=train)
    
    #Prune the tree using 6 terminal nodes
    prune.disease2 <- prune.misclass(tree.disease2,best= i)
    
    
    tree.pred <- predict(prune.disease2, cardio.test,type = "class")
    r<-table(tree.pred,cardio.test$diam_narrowing)
    err_rate<-1-( r[1,1] + r[2,2]) / sum(r)
    
    list_n <- c(list_n,err_rate)
    num_k <- num_k + 1
  }
  error_frame <- data.frame(cbind(K=1:(num_k-1), ERROR_RATE = list_n))
  error_frame
}

min_k <- function(data) {
  min_error<- min(data$ERROR_RATE)
  min_k_value <- NULL
  for(i in 1:nrow(data)) {
    if(min_error == data[i,"ERROR_RATE"]){
      min_k_value <- data[i,"K"]
      break
    } 
  }
  
  min_k_value
}

lvars$TREE_PRUNE_ERRORATE <- loop_predict_dtree_prune()

