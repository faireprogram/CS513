
##### FINAL PROJECT #####

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
# Boolean values for Gender
# Female=1 Male=0
gender_female <- ifelse(Dataset$gender=='female',1,0)

# Numeric values for CHEST PAIN TYPE
# TYPICAL ANGINA=1 ATYPICAL ANGINA=2 ASYMPTOMATIC=3 NON-ANGINAL PAIN=0 
cpt_num <- ifelse(Dataset$chestpaintype=='typ_angina',1,
           ifelse(Dataset$chestpaintype=='atyp_angina',2,
           ifelse(Dataset$chestpaintype=='asympt',3,0)))

# Numeric values for FASTING BLOOD SUGAR
# >120=1 <120=0 
fbs_num <- ifelse(Dataset$fasting_bloodsugar=='t',1,0)

# Numeric values for EXERCISE INDUCED ANGINA 
# Yes=1 No=0 
exia_num <- ifelse(Dataset$exercise_induced_ang=='yes',1,0)

# Numeric values for HEART STATUS (DIAMETER NARROWING) 
# >50%=1 <50%=0 
diam_num <- ifelse(Dataset$diam_narrowing=='>50_1',1,0)

New_Dataset_ECG <- cbind(age=norm(Dataset[,1],min(Dataset[,1]),max(Dataset[,1]))
                      ,male_female=gender_female
                      ,cpt_num
                      ,resting_bp=norm(Dataset[,4],min(Dataset[,4]),max(Dataset[,4]))
                      ,cholestrol=norm(Dataset[,5],min(Dataset[,5]),max(Dataset[,5]))
                      ,fbs_num
                      ,max_heartrate=norm(Dataset[,8],min(Dataset[,8]),max(Dataset[,8]))
                      ,exercise_induced_ang=exia_num
                      ,ecg_result=as.character(Dataset$rest_ecg))


nm <- na.omit(Dataset$vessel_color_flourosopy)
Dataset$vessel_color_flourosopy <- ifelse(is.na(Dataset$vessel_color_flourosopy)==TRUE,
                                          mean(nm),Dataset$vessel_color_flourosopy)
New_Dataset_HN <- cbind(age=norm(Dataset[,1],min(Dataset[,1]),max(Dataset[,1]))
                        ,male_female=gender_female
                        ,cpt_num
                        ,resting_bp=norm(Dataset[,4],min(Dataset[,4]),max(Dataset[,4]))
                        ,cholestrol=norm(Dataset[,5],min(Dataset[,5]),max(Dataset[,5]))
                        ,fbs_num
                        ,max_heartrate=norm(Dataset[,8],min(Dataset[,8]),max(Dataset[,8]))
                        ,exercise_induced_ang=exia_num
                        ,flourosopy=Dataset$vessel_color_flourosopy
                        ,heart_result=as.character(Dataset$diam_narrowing))


### Storing 70% of the new datsets as TRAINING DATA and the rest as TEST DATA
temp <- sample(nrow(New_Dataset_ECG),as.integer(0.70 * nrow(New_Dataset_ECG)))
Training_ECG <- New_Dataset_ECG[temp,]

Test_ECG <- New_Dataset_ECG[-temp,]


temp <- sample(nrow(New_Dataset_HN),as.integer(0.70 * nrow(New_Dataset_HN)))
Training_HN <- New_Dataset_HN[temp,]

Test_HN <- New_Dataset_HN[-temp,]


# Performing KNN 
library(class)

Predict_1_ECG <- knn(Training_ECG[,-9], Test_ECG[,-9], Training_ECG[,9], k=2)
Predict_1_HN <- knn(Training_HN[,-10], Test_HN[,-10], Training_HN[,10], k=10)

# Getting the error rate for KNN pertaining to ElectroCardioGram
Result_1_Gender <- cbind(Test_ECG,as.character(Predict_1_ECG))

false_results <- Result_1_Gender[,9]!=Result_1_Gender[,10]
Error_rate_1_ECG <- sum(false_results)/length(false_results)
Error_rate_1_ECG

# Getting the error rate for KNN pertaining to Heart Narrowing%
HN_Result_1_Gender <- cbind(Test_HN,as.character(Predict_1_HN))

false_results <- HN_Result_1_Gender[,10]!=HN_Result_1_Gender[,11]
Error_rate_1_HN <- sum(false_results)/length(false_results)
Error_rate_1_HN

#table(Predict_1_ECG, Test_ECG[,2])
#table(Predict_1_HN, Test_HN[,2])

#table(Predict_1_ECG, Test_ECG[,3])
#table(Predict_1_HN, Test_HN[,3])

########################  K MEANS  ####################
#Applying K MEANS algorithm and classifyng data in clusters.

browser()
KMEAN_DATASET<-Training_ECG[,c(XOL,YOL)]

KMEAN_CLUSTER <- kmeans(KMEAN_DATASET, KMEAN_K)



