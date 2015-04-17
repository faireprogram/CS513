library(shiny)
vars<-reactiveValues(RE_HN_KNN = NULL,
                     RE_ECG_KNN=NULL)



# Define server logic for random distribution application
shinyServer(function(input, output) {

  lvars<-reactiveValues(KMEAN_CLUSTER = NULL,
                       KMEAN_DATASET=NULL,
                       K_SAMPLE_INDEX = NULL,
                       KKNN_ERROR_RATE = NULL,
                       TREE_DESEASE = NULL,
                       TREE_PREDICT_ERRORATE = NULL,
                       TREE_CV=NULL,
                       TREE_PRUNE = NULL,
                       TREE_PRUNE_ERRORATE = NULL,
                       TREE_CV_DATASET = NULL)
  
  DATASET<-NULL
  ###
  ### Local data
  local_original_data <- function() {
    DATASET<-read.csv("heart-c.csv")
    DATASET
  }
  
  local_normalize_data <- function() {
    if(is.null(DATASET)) {
      DATASET <- local_original_data()
    }
    source("normalize_data.R", local=TRUE)
    DATASET_NORM
  }
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output renderers defined 
  # below then all used the value computed from this expression
  data <- reactive({  
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  data2 <- reactive({  
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  csv_data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    data
  })
  
  
  selectedData <- reactive({
    source("KMEANS.R", local=TRUE)
  })

  
  output$Plot_Cluster <- renderPlot({
    selectedData()
    par(mar = c(5.1, 4.1, 0, 1))
    plot(lvars$KMEAN_DATASET,
         col = lvars$KMEAN_CLUSTER$cluster,
         pch = 20, cex = 3)
    points(lvars$KMEAN_CLUSTER$centers, pch = 4, cex = 4, lwd = 4)
  })
  

  
  output$ECG_KNN <- renderPlot({
    source("KNN.R", local=TRUE)
    min_ecg_n <- min_k(vars$RE_ECG_KNN)
    min_hn_n <- min_k(vars$RE_HN_KNN)
    
    ticks <- seq(1,as.integer(input$KMAX),by=2)
    kmax <- as.integer(input$KMAX)
    
    ######---------------------------------
    plot(vars$RE_ECG_KNN, type="o", col="blue",ylim=0:1 ,
         xaxt="n", ann=FALSE)
    axis(1,at=ticks, lab=ticks*2-1)
    lines(vars$RE_HN_KNN, type="o", pch=22, lty=2, col="black")
    title(xlab="K", col.lab=rgb(0,0.5,0))
    title(ylab="ERROR RATE", col.lab=rgb(0,0.5,0))
    
    legend(1, c(paste("Knn:[ErrorRate=",vars$RE_ECG_KNN[min_ecg_n,2],"]",seq=""),
                paste("Kknn :[ErrorRate=",vars$RE_HN_KNN[min_hn_n,2],"]",seq="")), 
           cex=0.8, 
           col=c("blue","black"), pch=21:22, lty=1:2);
    
    points(vars$RE_ECG_KNN[min_ecg_n,], pch = 4, cex = 4, lwd = 4,col="red")
    points(vars$RE_HN_KNN[min_hn_n,], pch = 4, cex = 4, lwd = 4,col="green")
#     
#     text(vars$RE_ECG_KNN[min_ecg_n,] +c(-2,0.015),cex=0.8, label=paste("ErrorRate_ECG=",vars$RE_ECG_KNN[min_ecg_n,2],seq=""))
#     text(vars$RE_HN_KNN[min_hn_n,] +c(-2,0.015),cex=0.8, label=paste("ErrorRate_HN=",vars$RE_HN_KNN[min_hn_n,2],seq=""))
    
  })

  
  # Generate a summary of the data
  output$KKNN_PLOT <- renderPlot({
    source("KNN_PLOT.R", local=TRUE)
    
    min_ecg_n <- min_k(lvars$KKNN_ERROR_RATE)
    plot(lvars$KKNN_ERROR_RATE, type="o", col="blue",ylim=0:1 ,
         xaxt="n", ann=FALSE)
    axis(1,at=1:input$KKNN_D, lab=1:input$KKNN_D)
    title(xlab="Distance/vote", col.lab=rgb(0,0.5,0))
    title(ylab="ERROR RATE", col.lab=rgb(0,0.5,0))
    
    legend(1, paste("k=",input$KKNN_K, " D= ", input$KKNN_D ,":[ErrorRate=",lvars$KKNN_ERROR_RATE[min_ecg_n,2],"]",seq=""), 
           cex=0.8, 
           col=c("blue"), pch=21:22, lty=1:2);
    
    points(lvars$KKNN_ERROR_RATE[min_ecg_n,], pch = 4, cex = 4, lwd = 4,col="red")
    
  })

  # Generate a summary of the data
  output$TREE_PLOT <- renderPlot({
    source("DTREE_PLOT.R",local =TRUE)
    plot(lvars$TREE_DESEASE)
    text(lvars$TREE_DESEASE,pretty=0)
  })


  # Generate a summary of the data
  output$TREE_Errorate <- renderPlot({
  
    source("DTREE_PLOT_ERRORATE.R",local =TRUE)
    min_ecg_n <- min_k( lvars$TREE_PREDICT_ERRORATE)
    plot( lvars$TREE_PREDICT_ERRORATE, type="o", col="blue",ylim=0:1 ,
          xaxt="n", ann=FALSE)
    axis(1,at=1:input$DT_SEED, lab=1:input$DT_SEED)
    title(xlab="K", col.lab=rgb(0,0.5,0))
    title(ylab="ERROR RATE", col.lab=rgb(0,0.5,0))
    
    legend(1, paste("Seed=",input$DT_SEED ,":[ErrorRate=",lvars$TREE_PREDICT_ERRORATE[min_ecg_n,2],"]",seq=""), 
           cex=0.8, 
           col=c("blue"), pch=21:22, lty=1:2);
    
    points(lvars$TREE_PREDICT_ERRORATE[min_ecg_n,], pch = 4, cex = 4, lwd = 4,col="red")
  
  })
  

  # Generate a summary of the data
  output$TREE_CV <- renderPlot({
    source("DTREE_CV.R",local =TRUE)
    
    par(mfrow=c(1,2))
    plot(lvars$TREE_CV$size,cv.disease2$dev, type ="b")
    plot(lvars$TREE_CV$k,cv.disease2$dev, type ="b")
    
  })

  # Generate a summary of the data
  output$TREE_Prune_Error_Plot <- renderPlot({
    source("DTREE_CV.R",local =TRUE)
    par(mfrow=c(1,2))
    plot(lvars$TREE_CV$size,lvars$TREE_CV$dev, type ="b",ylab="error rate",xlab="number of nodes",main="Reduced error pruning")
    
})

  # Generate a summary of the data
  output$TREE_PRUNE_PLOT <- renderPlot({
    source("DTREE_PRUNE.R",local =TRUE)
    plot(lvars$TREE_PRUNE)
    text(lvars$TREE_PRUNE,pretty=0 )
    
  })

  # Generate a summary of the data
  output$TREE_PRUNE_ErrorRate <- renderPlot({
    source("DTREE_PRUNE_Error.R",local =TRUE)
    min_ecg_n <- min_k( lvars$TREE_PRUNE_ERRORATE)
    plot( lvars$TREE_PRUNE_ERRORATE, type="o", col="blue",ylim=0:1 ,
          xaxt="n", ann=FALSE)
    axis(1,at=1:input$PRUNE_K , lab=1:input$PRUNE_K + 1)
    title(xlab="K", col.lab=rgb(0,0.5,0))
    title(ylab="ERROR RATE", col.lab=rgb(0,0.5,0))
    
    legend(1, paste("Seed=",input$DT_SEED, "Prune_K = ", input$PRUNE_K ,":[ErrorRate=",lvars$TREE_PRUNE_ERRORATE[min_ecg_n,2],"]",seq=""), 
           cex=0.8, 
           col=c("blue"), pch=21:22, lty=1:2);
    
    points(lvars$TREE_PRUNE_ERRORATE[min_ecg_n,], pch = 4, cex = 4, lwd = 4,col="red")
    
    
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$original_data <- renderDataTable({
   # data.frame(x=data())
    data<- local_original_data()
    data
  })
  
  output$normalized_data <- renderDataTable({
    # data.frame(x=data())
    data<- local_normalize_data()
    data
  })

  # Generate a summary of the data
  output$KNN_PLOT_1 <- renderPlot({
    source("Knn_Prediction.R",local =TRUE)
    plot(Predict_a,Test[,1])
    
  })


  # Generate a summary of the data
  output$KNN_PLOT_2 <- renderPlot({
    source("Knn_Prediction.R",local =TRUE)
    plot(Predict_a,Test[,2])
  })

  # Generate a summary of the data
  output$KNN_PLOT_3 <- renderPlot({
    source("Knn_Prediction.R",local =TRUE)
    plot(Predict_a,Test[,4])
  })

  # Generate a summary of the data
  output$KNN_PLOT_4 <- renderPlot({
    source("Knn_Prediction.R",local =TRUE)
    plot(Predict_a,Test[,6]) 
  })
  
  output$KNN_PLOT_Table_1 <- renderPrint({
    source("Knn_Prediction.R",local =TRUE)
    d<-table(Predict_a,Test[,13]) 
    d
  })

  output$KNN_PLOT_Table_2 <- renderPrint({
    source("Knn_Prediction.R",local =TRUE)
   d<- table(Predict_a,Test[,14])
   d
  })

  output$KMEAN_PLOT_Table_1 <- renderPrint({
    source("Kmeans_Prediction.R",local =TRUE)
    d<- table(Training1[,12],Predict_clus$cluster)
    d
  })

  output$KMEAN_PLOT_Table_2 <- renderPrint({
    source("Kmeans_Prediction.R",local =TRUE)
    d<- table(Training1[,14],Predict_clus$cluster)
    d
  })

  output$KMEAN_PLOT_Table_3 <- renderPrint({
    source("Kmeans_Prediction.R",local =TRUE)
    d<-table(Training1[,13],Predict_clus$cluster)
    d
  })

  output$KMEAN_PLOT_Table_4 <- renderPrint({
    source("Kmeans_Prediction.R",local =TRUE)
    d<-  table(Training1[,11],Predict_clus$cluster)
    d
  })

  # Generate a summary of the data
  output$KMEAN_PLOT_1 <- renderPlot({
    source("Kmeans_Prediction.R",local =TRUE)
    plot(max_heart_rate, age, col = Predict_clus$cluster)
    points(Predict_clus$centers,col = 1:3, pch = 17, cex = 2)
  })

  # Generate a summary of the data
  output$KMEAN_PLOT_2 <- renderPlot({
    source("Kmeans_Prediction.R",local =TRUE)
    plot(resting_bp, age, col = Predict_clus$cluster)
    points(Predict_clus$centers,col = 1:3, pch = 17, cex = 2)
  })

  # Generate a summary of the data
  output$KMEAN_PLOT_3 <- renderPlot({
    source("Kmeans_Prediction.R",local =TRUE)
    plot(age, cholestrol,type='p', col = Predict_clus$cluster)
    points(Predict_clus$centers,col = 1:3, pch = 17, cex = 2)
  })
})



