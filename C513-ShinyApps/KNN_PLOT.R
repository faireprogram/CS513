
##### FINAL PROJECT #####
### CLASSIFICATION OF CARDIOVASCULAR DISEASE FOR CLEAVELAND MEDICAL DATA ###

### DONE BY:               ### 
###          Ram Kharawala ###
###          Mohit Bhasin  ###
###          Danny Diaz    ###
###          Wen Zhang     ###

rm(list=ls())

############################################
### Defining the normalization function  ###
############################################
norm <-function(x,minx,maxx) #Normalization
{
  z<-((x-minx)/(maxx-minx))
  return(z) 
}

###########################################
### Reading the Heart Disorder Dataset  ###
###########################################
Dataset <- read.csv("heart-c.csv") #Loading csv file


###################################################
### Creating New Datasets:                      ###
### 1: For classification based on ECG          ###
### 2: Classification based on heart narrowing  ###
###################################################
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Boolean values for Gender  >>>
# Female=1 Male=0            >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
gender_female <- ifelse(Dataset$gender=='female',1,0)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Numeric values for CHEST PAIN TYPE                                    >>>
# TYPICAL ANGINA=1 ATYPICAL ANGINA=2 ASYMPTOMATIC=3 NON-ANGINAL PAIN=0  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cpt_num <- ifelse(Dataset$chestpaintype=='typ_angina',1,
                  ifelse(Dataset$chestpaintype=='atyp_angina',2,
                         ifelse(Dataset$chestpaintype=='asympt',3,0)))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Numeric values for FASTING BLOOD SUGAR  >>>
# >120=1 <120=0                           >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fbs_num <- ifelse(Dataset$fasting_bloodsugar=='t',1,0)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Numeric values for EXERCISE INDUCED ANGINA  >>>
# Yes=1 No=0                                  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
exia_num <- ifelse(Dataset$exercise_induced_ang=='yes',1,0)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Numeric values for HEART STATUS (DIAMETER NARROWING)  >>>
# >50%=1 <50%=0                                         >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
diam_num <- ifelse(Dataset$diam_narrowing=='>50_1',1,0)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Replacing 'NA' values with their mean for Flourosopy  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
nm <- na.omit(Dataset$vessel_color_flourosopy)
Dataset$vessel_color_flourosopy <- ifelse(is.na(Dataset$vessel_color_flourosopy)==TRUE,
                                          mean(nm),Dataset$vessel_color_flourosopy)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Slope of excercise during ST depression testing  >>>
# 1 = up; 2 = flat; 3 = down                       >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
slope_st <- ifelse(Dataset$slope=='up',1,
                   ifelse(Dataset$slope=='flat',2,3))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Defect type                                          >>>
# 3 = normal; 6 = fixed defect; 7 = reversable defect  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
defect_type <- ifelse(Dataset$thal=='normal',3,
                      ifelse(Dataset$thal=='fixed_defect',6,7))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Creating a new dataset with required attributes  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
New_Dataset <- cbind(age=norm(Dataset[,1],min(Dataset[,1]),max(Dataset[,1]))
                     ,resting_bp=norm(Dataset[,4],min(Dataset[,4]),max(Dataset[,4]))
                     ,cholestrol=norm(Dataset[,5],min(Dataset[,5]),max(Dataset[,5]))
                     ,fasting_blood_sugar=fbs_num
                     ,max_heartrate=norm(Dataset[,8],min(Dataset[,8]),max(Dataset[,8]))
                     ,exercise_induced_ang=exia_num
                     ,ST_dep_rate=norm(Dataset[,10],min(Dataset[,10]),max(Dataset[,10]))
                     ,ST_slope=norm(slope_st,min(slope_st),max(slope_st))
                     ,defect=norm(defect_type,min(defect_type),max(defect_type))
                     ,flourosopy_result=Dataset$vessel_color_flourosopy
                     ,ecg_result=as.character(Dataset$rest_ecg)
                     ,male_female=as.character(Dataset$gender)
                     ,chest_pain_type=as.character(Dataset$chestpaintype)
                     ,heart_result=as.character(Dataset$diam_narrowing))

################################################################################
# Storing 70% of the new datsets as TRAINING DATA and the rest as TEST DATA  ###
################################################################################
if(is.null(lvars$K_SAMPLE_INDEX)) {
  temp <- sample(nrow(New_Dataset),as.integer(0.70 * nrow(New_Dataset)))
  lvars$K_SAMPLE_INDEX <-temp
}
temp <-lvars$K_SAMPLE_INDEX

New_Dataset<-data.frame(New_Dataset)

#########-------------------------------------------------------------

Training <- New_Dataset[temp,]

Test <- New_Dataset[-temp,]


##########################
# Error rate function  ###------------------------------------------------
##########################
error <- function(col) #col - column to compare with
{
  e_result <- cbind(Test, as.character(Predict))
  false <- e_result[,col]!=e_result[,15]
  err_rate <- sum(false)/length(false)
  err_rate
}

#####################
# Performing KNN  ###
#####################
library(class)

# Classifying based on gender
Predict_g <- knn(Training[,1:9], Test[,1:9], Training[,14], k=15)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Error value for kNN when classifying with gender  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
e_result <- cbind(Test, as.character(Predict_g))
false <- e_result[,14]!=e_result[,15]
err_rate <- sum(false)/length(false)
err_rate

###-------------------------------------------------------------------
library(kknn)
predict_kknn<-kknn(heart_result~.-ecg_result-male_female-chest_pain_type,
        train=Training,
        test=Test,
        k=1,
#         kernel = c("triangular", "rectangular", "epanechnikov", "optimal"),
        distance=1)  
fitted(predict_kknn)






loop_predict_knn <- function(SEQ_K) {
  list_n <- {}
  num_k <- 1
  
  for (i in SEQ_K) {
    
    # Classifying based on gender
    Predict_g <- knn(Training[,1:9], Test[,1:9], Training[,14], k=i)
    e_result <- cbind(Test, as.character(Predict_g))
    false <- e_result[,14]!=e_result[,15]
    err_rate <- sum(false)/length(false)
    
    list_n <- c(list_n,err_rate)
    num_k <- num_k + 1
  }
  error_frame <- data.frame(cbind(K=1:(num_k-1), ERROR_RATE = list_n))
  error_frame
}


loop_predict_kknn <- function(SEQ_K) {
  list_n <- {}
  num_k <- 1
  for (i in SEQ_K) {
    
    predict_kknn<-kknn(heart_result~.-ecg_result-male_female-chest_pain_type,
                       train=Training,
                       test=Test,
                       k=i,
#                        kernel = c("triangular", "rectangular", "epanechnikov", "optimal"),
                       distance=1)  
    Predict_g<-fitted(predict_kknn)
    e_result <- cbind(Test, as.character(Predict_g))
    false <- e_result[,14]!=e_result[,15]
    err_rate <- sum(false)/length(false)
   
    list_n <- c(list_n,err_rate)
    num_k <- num_k + 1
  }

  error_frame <- data.frame(cbind(K=1:(num_k-1), ERROR_RATE = list_n))
  error_frame
}

loop_predict_kknnd <- function(SEQ_K) {
  list_n <- {}
  num_k <- 1
  for (i in SEQ_K) {
    predict_kknn<-kknn(heart_result~.-ecg_result-male_female-chest_pain_type,
                       train=Training,
                       test=Test,
                       k=input$KKNN_K,
                       #                        kernel = c("triangular", "rectangular", "epanechnikov", "optimal"),
                       distance=i)  
    Predict_g<-fitted(predict_kknn)
    e_result <- cbind(Test, as.character(Predict_g))
    false <- e_result[,14]!=e_result[,15]
    err_rate <- sum(false)/length(false)
    
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

KBEGIN<-1
KEND<-input$KKNN_D
SEQ_K <- 1:input$KKNN_D

  re<-loop_predict_kknnd(SEQ_K)
  lvars$KKNN_ERROR_RATE <- re
