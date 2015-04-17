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




#--------------------------------ERRORATE
loop_predict_dtree <- function() {
  
  list_n <- {}
  num_k <- 1
  for (i in 1:input$DT_SEED) {
    
    set.seed(i) 
    train <- sample(1:nrow(cardio),152)
    cardio.test <- cardio[-train,]
    tree.disease2 <- tree(diam_narrowing~.-diam_narrowing,cardio,subset=train)
    tree.pred <- predict(tree.disease2, cardio.test,type = "class")
    r<-table(tree.pred, cardio.test$diam_narrowing)
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

lvars$TREE_PREDICT_ERRORATE <- loop_predict_dtree()



