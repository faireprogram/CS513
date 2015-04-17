
##### FINAL PROJECT #####

### DONE BY:               ### 
###          Ram Kharawala ###
###          Mohit Bhasin  ###
###          Danny Diaz    ###
###          Wen Zhang     ###

#rm(list=ls())

############################################
### Defining the normalization function  ###
############################################
norm <-function(x,minx,maxx) #Normalization
{
  z<-((x-minx)/(maxx-minx))
  return(z) 
}

###########################################
### Reading the Heart Disorder DATASET  ###
###########################################

table(DATASET$slope) 

thal_normal <- ifelse(DATASET$thal=='normal',1,0)
thal_reversable_defect <- ifelse(DATASET$thal=='reversable_defect',1,0)
thal_fixed_defect <- ifelse(DATASET$thal=='fixed_defect',1,0)

slop_flat <- ifelse(DATASET$slope=='flat',1,0)
slop_down <- ifelse(DATASET$slope=='down',1,0)

###################################################
### Creating New DATASETs:                      ###
### 1: For classification based on ECG          ###
### 2: Classification based on heart narrowing  ###
###################################################
# Boolean values for Gender
# Female=1 Male=0
gender_female <- ifelse(DATASET$gender=='female',1,0)

# Numeric values for CHEST PAIN TYPE
# TYPICAL ANGINA=1 ATYPICAL ANGINA=2 ASYMPTOMATIC=3 NON-ANGINAL PAIN=0 
cpt_typ_num <- ifelse(DATASET$chestpaintype=='typ_angina',1,0)
cpt_atyp_num <- ifelse(DATASET$chestpaintype=='atyp_angina',1,0)

# Numeric values for FASTING BLOOD SUGAR
# >120=1 <120=0 
fbs_num <- ifelse(DATASET$fasting_bloodsugar=='t',1,0)

# Numeric values for EXERCISE INDUCED ANGINA 
# Yes=1 No=0 
exia_num <- ifelse(DATASET$exercise_induced_ang=='yes',1,0)

# Numeric values for HEART STATUS (DIAMETER NARROWING) 
# >50%=1 <50%=0 
diam_num <- ifelse(DATASET$diam_narrowing=='>50_1',1,0)


nm <- na.omit(DATASET$vessel_color_flourosopy)
DATASET$vessel_color_flourosopy <- ifelse(is.na(DATASET$vessel_color_flourosopy)==TRUE,
                                          mean(nm),DATASET$vessel_color_flourosopy)
New_DATASET_HN <- cbind(age=norm(DATASET[,1],min(DATASET[,1]),max(DATASET[,1]))
                        ,male_female=gender_female
                        ,cpt_typ_num = cpt_typ_num
                        ,cpt_atyp_num = cpt_atyp_num
                        ,resting_bp=norm(DATASET[,4],min(DATASET[,4]),max(DATASET[,4]))
                        ,cholestrol=norm(DATASET[,5],min(DATASET[,5]),max(DATASET[,5]))
                        ,fbs_num
                        ,max_heartrate=norm(DATASET[,8],min(DATASET[,8]),max(DATASET[,8]))
                        ,exercise_induced_ang=exia_num
                        ,flourosopy=DATASET$vessel_color_flourosopy
                        ,heart_result=as.character(DATASET$diam_narrowing))


DATASET_NORM <- data.frame(New_DATASET_HN)
DATASET_NORM$thal_normal  <- thal_normal  
DATASET_NORM$thal_reversable_defect   <- thal_reversable_defect   
DATASET_NORM$thal_fixed_defect   <- thal_fixed_defect   
DATASET_NORM$slop_flat   <- slop_flat   
DATASET_NORM$slop_down   <- slop_down    
