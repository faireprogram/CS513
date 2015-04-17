
##### FINAL PROJECT #####
### CLASSIFICATION OF CARDIOVASCULAR PROBLEMS FOR CLEAVELAND MEDICAL DATA ###

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


##############################
### Creating New Datasets  ###
##############################
#>>>>>>>>>>>>>>>>
# Age Groups  >>>
#>>>>>>>>>>>>>>>>
age_group <- ifelse(Dataset$age>0 & Dataset$age<20,'A', 
                    ifelse(Dataset$age>20 & Dataset$age<30,'B', 
                           ifelse(Dataset$age>30 & Dataset$age<40,'C',
                                  ifelse(Dataset$age>40 & Dataset$age<50,'D',
                                         ifelse(Dataset$age>50 & Dataset$age<60,'E','F')))))

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
New_Dataset1 <- cbind(age=norm(Dataset[,1],min(Dataset[,1]),max(Dataset[,1]))
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

temp <- sample(nrow(New_Dataset1),as.integer(0.70 * nrow(New_Dataset1)))
Training1 <- New_Dataset1[temp,]
Test1 <- New_Dataset1[-temp,]

#Applying K MEANS algorithm and classifyng data in clusters.
age <- Training1[,1]
cholestrol <- Training1[,3]
resting_bp <- Training1[,2]
fasting_blood_sugar <- Training1[,4]
max_heart_rate <- Training1[,5]
gender <- Training1[,12]


Predict_clus <- kmeans(Training1[,1:10], input$Kmeans_CURRENT) # Kmeans
Predict_clus
# table(Training1[,12],Predict_clus$cluster) # Frequency Table age cluster
# table(Training1[,14],Predict_clus$cluster) # Frequency Table heart_narrowing cluster
# table(Training1[,13],Predict_clus$cluster) # Frequency Table chest_pain_type cluster
# table(Training1[,11],Predict_clus$cluster) # Frequency Table ECG_result cluster

# Plotting on max_heart_rate vs age
# plot(max_heart_rate, age, col = Predict_clus$cluster)
# points(Predict_clus$centers,col = 1:3, pch = 17, cex = 2)

# Plotting on resting_bp vs age
# plot(resting_bp, age, col = Predict_clus$cluster)
# points(Predict_clus$centers,col = 1:3, pch = 17, cex = 2)

# Plotting on age vs cholestrol
# plot(age, cholestrol,type='p', col = Predict_clus$cluster)
# points(Predict_clus$centers,col = 1:3, pch = 17, cex = 2)
