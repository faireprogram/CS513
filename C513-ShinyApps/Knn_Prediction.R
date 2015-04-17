
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
New_Dataset <- cbind(#age=norm(Dataset[,1],min(Dataset[,1]),max(Dataset[,1]))
                      resting_bp=norm(Dataset[,4],min(Dataset[,4]),max(Dataset[,4]))
                      ,cholestrol=norm(Dataset[,5],min(Dataset[,5]),max(Dataset[,5]))
                      ,fasting_blood_sugar=fbs_num
                      ,max_heartrate=norm(Dataset[,8],min(Dataset[,8]),max(Dataset[,8]))
                      ,exercise_induced_ang=exia_num
                      ,ST_dep_rate=norm(Dataset[,10],min(Dataset[,10]),max(Dataset[,10]))
                      ,ST_slope=norm(slope_st,min(slope_st),max(slope_st))
                      ,defect=norm(defect_type,min(defect_type),max(defect_type))
                      ,flourosopy_result=Dataset$vessel_color_flourosopy                      
                      ,age=as.character(age_group)
                      ,ecg_result=as.character(Dataset$rest_ecg)
                      ,male_female=as.character(Dataset$gender)
                      ,chest_pain_type=as.character(Dataset$chestpaintype)
                      ,heart_result=as.character(Dataset$diam_narrowing))



################################################################################
# Storing 70% of the new datsets as TRAINING DATA and the rest as TEST DATA  ###
################################################################################
temp <- sample(nrow(New_Dataset),as.integer(0.70 * nrow(New_Dataset)))
Training <- New_Dataset[temp,]

Test <- New_Dataset[-temp,]


#####################
# Performing KNN  ###
#####################
library(class)

# Classifying based on gender
Predict_g <- knn(Training[,1:9], Test[,1:9], Training[,12], k=29)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Error value for kNN when classifying with gender  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
e_result <- cbind(Test, as.character(Predict_g))
false <- e_result[,12]!=e_result[,15]
err_rate <- sum(false)/length(false)
err_rate
#>>>>>>>>>>>>>>>>>>>>>>
# Frequency tables  >>>
#>>>>>>>>>>>>>>>>>>>>>>
table(Predict_g, Test[,14]) # Frequency Table gender vs heart narrowing
table(Predict_g, Test[,13]) # Frequency Table gender vs chest pain type
table(Predict_g, Test[,11]) # Frequency Table gender vs ECG_result

# Classifying based on Age groups
Predict_a <- knn(Training[,1:9], Test[,1:9], Training[,10], k=input$K_CURRENT)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Error value for kNN when classifying with Age groups  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
e_result <- cbind(Test, as.character(Predict_a))
false <- e_result[,10]!=e_result[,15]
err_rate <- sum(false)/length(false)
err_rate
#>>>>>>>>>>>>>>>>>>>>>>>>>>
# Plotting Comparisons  >>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>
# plot(Predict_a,Test[,1]) # Graph1: age group vs resting blood pressure
# # Conclusion 1: Blood pressure level is high between the age 40 to 50 years
# plot(Predict_a,Test[,2]) # Graph2: age group vs cholestrol level
# # Conclusion 2: Cholestrol level high between the age 40 to 60 years
# plot(Predict_a,Test[,4]) # Graph3: age group vs max heart rate achieved
# # Conclusion 3: Higher heart rate between the ages 30 to 40 years
# plot(Predict_a,Test[,6]) # Graph4:age group vs ST depression rate
# Conclusion 4: Higher ST depression rate is found between the ages 50 to 60 years
table(Predict_a,Test[,13]) 
# Conclusion 5: More asymptomatic chest pain type found between ages 50 to 60 years
# and non anginal chest pain found mainly between 40 to 50 years of age
table(Predict_a,Test[,14])
# Conclusion 6: Heart diameter narrowing >50% mainly found between the ages 50 to 60 years
# more patients between 40 to 60 years of age

# FINAL CONCLUSION: Patients more prone to cardiovascular problems are 
#                   between the ages 40 to 60 years
# Patients more prone to heart attacks are between the ages 50 to 60 years

