#########################################
###################
################### Approach 1 - Random Forest & Logistic Regression
###################
#########################################

# Importing the Files

df1 = read.csv("C:/DataSet/Project2/application_train.csv", stringsAsFactors = TRUE, header = TRUE)

df2 = read.csv("C:/DataSet/Project2/previous_application.csv", stringsAsFactors = TRUE, header = TRUE)
df3 = read.csv("C:/DataSet/Project2/credit_card_balance.csv", stringsAsFactors = TRUE, header = TRUE)

df4 = read.csv("C:/DataSet/Project2/applications_to_score.csv", stringsAsFactors = TRUE, header = TRUE)

# Installing Required Packages
# This is only needed once so the code is commented out.

#install.packages("ROSE")
#install.packages("corrplot")
#install.packages("dataQualityR")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("mice")
#install.packages("plyr")
#install.packages("dplyr")

# Adding Required Libraries

library(ROSE)
library(corrplot)
library(dataQualityR)
library(randomForest)
library(caret)
library(mice)
library(dplyr)

##############################################################
########## Previous Application
##############################################################

View(df2)

checkDataQuality(data = df1, 
                 out.file.num = "C:/DataSet/Project2/df2_qr_num.csv", 
                 out.file.cat = "C:/DataSet/Project2/df2_qr_cat.csv")

df2_qr_num = read.csv("C:/DataSet/Project2/df2_qr_num.csv")
df2_qr_cat = read.csv("C:/DataSet/Project2/df2_qr_cat.csv")

View(df2_qr_num)
View(df2_qr_cat)

# Subsetting the Previous Application Data Frame
df2A = subset(df2, select = c(SK_ID_CURR, NAME_CONTRACT_STATUS))

# Creating Dummy Variables
df2B = fastDummies::dummy_cols(df2A)

summary(df2B)

# Aggregating Using Sum
df2C = aggregate(cbind(previous_approved = df2B$NAME_CONTRACT_STATUS_Approved, previous_canceled = df2B$NAME_CONTRACT_STATUS_Canceled, previous_refused = df2B$NAME_CONTRACT_STATUS_Refused, previous_unused = df2B$'NAME_CONTRACT_STATUS_Unused offer'),by=list(SK_ID_CURR = df2B$SK_ID_CURR),FUN = sum)

df2D = df2 %>% dplyr::count(SK_ID_CURR)
df2D = df2D %>% dplyr::rename(previous_count = n)
df2E = plyr::join(df2C,df2D,type='left')

View(df2E)

##############################################################
###### Credit Card
##############################################################

View(df3)

df3A = df3 %>% dplyr::count(SK_ID_CURR)
df3A = df3A %>% dplyr::rename(Credit_Months = n)

df3B = subset(df3, select = c(SK_ID_CURR, AMT_CREDIT_LIMIT_ACTUAL, SK_DPD, SK_DPD_DEF))
df3B$AMT_CREDIT_LIMIT_ACTUAL[df3B$AMT_CREDIT_LIMIT_ACTUAL == 0] <- NA

df3C<-aggregate(cbind(Credit_LIMIT_MEAN = df3B$AMT_CREDIT_LIMIT_ACTUAL, SK_DPD_MEAN = df3B$SK_DPD, SK_DPD_DEF_MEAN = df3B$SK_DPD_DEF), by=list(SK_ID_CURR = df3B$SK_ID_CURR),FUN = mean, na.rm = TRUE)

df3D = plyr::join(df3A,df3C,type='left')

View(df3D)

##############################################################
###### Left Join
##############################################################

oriA = plyr::join(df1,df2E,type='left')
ori = plyr::join(oriA,df3C,type='left')

View(ori)

# Checking the Data Quality

checkDataQuality(data = ori, 
                 out.file.num ="C:/DataSet/Project2/ori_qr_num.csv", 
                 out.file.cat= "C:/DataSet/Project2/ori_qr_cat.csv")

ori_qr_num = read.csv("C:/DataSet/Project2/ori_qr_num.csv")
ori_qr_cat = read.csv("C:/DataSet/Project2/ori_qr_cat.csv")

View(ori_qr_num)
View(ori_qr_cat)

# Removing unnecessary columns that either has large number of null values or are not intuitively required for this case.

df = subset(ori, select = -c(NAME_TYPE_SUITE,
                                         APARTMENTS_AVG,
                                         BASEMENTAREA_AVG, 
                                         YEARS_BEGINEXPLUATATION_AVG, 
                                         YEARS_BUILD_AVG,
                                         COMMONAREA_AVG,
                                         ELEVATORS_AVG,
                                         ENTRANCES_AVG,
                                         FLOORSMAX_AVG,
                                         FLOORSMIN_AVG,
                                         LANDAREA_AVG,
                                         LIVINGAPARTMENTS_AVG,
                                         LIVINGAREA_AVG,
                                         NONLIVINGAPARTMENTS_AVG,
                                         NONLIVINGAREA_AVG,
                                         APARTMENTS_MODE,
                                         BASEMENTAREA_MODE,
                                         YEARS_BEGINEXPLUATATION_MODE,
                                         YEARS_BUILD_MODE,
                                         COMMONAREA_MODE,
                                         ELEVATORS_MODE,
                                         ENTRANCES_MODE,
                                         FLOORSMAX_MODE,
                                         FLOORSMIN_MODE,
                                         LANDAREA_MODE,
                                         LIVINGAPARTMENTS_MODE,
                                         LIVINGAREA_MODE,
                                         NONLIVINGAPARTMENTS_MODE,
                                         NONLIVINGAREA_MODE,
                                         FONDKAPREMONT_MODE,
                                         HOUSETYPE_MODE,
                                         TOTALAREA_MODE,
                                         WALLSMATERIAL_MODE,
                                         EMERGENCYSTATE_MODE,
                                         APARTMENTS_MEDI,
                                         BASEMENTAREA_MEDI,
                                         YEARS_BEGINEXPLUATATION_MEDI,
                                         YEARS_BUILD_MEDI,
                                         COMMONAREA_MEDI,
                                         ELEVATORS_MEDI,
                                         ENTRANCES_MEDI,
                                         FLOORSMAX_MEDI,
                                         FLOORSMIN_MEDI,LANDAREA_MEDI,
                                         LIVINGAPARTMENTS_MEDI,
                                         LIVINGAREA_MEDI,
                                         NONLIVINGAPARTMENTS_MEDI,
                                         NONLIVINGAREA_MEDI,
                                         FLAG_DOCUMENT_4,
                                         FLAG_DOCUMENT_6,
                                         FLAG_DOCUMENT_7,
                                         FLAG_DOCUMENT_9,
                                         FLAG_DOCUMENT_10,
                                         FLAG_DOCUMENT_11,
                                         FLAG_DOCUMENT_12,
                                         FLAG_DOCUMENT_15,
                                         FLAG_DOCUMENT_17,
                                         FLAG_DOCUMENT_19,
                                         FLAG_DOCUMENT_20,
                                         FLAG_DOCUMENT_21,
                                         FLAG_MOBIL,
                                         LIVE_CITY_NOT_WORK_CITY,
                                         REG_CITY_NOT_WORK_CITY,
                                         LIVE_REGION_NOT_WORK_REGION,
                                         REG_REGION_NOT_WORK_REGION,
                                         FLAG_PHONE,
                                         FLAG_WORK_PHONE,
                                         ORGANIZATION_TYPE,
                                         FLAG_EMP_PHONE))

View(df)

# Adding Mannual Features - Financial Ratios

df$goods_covered = df$AMT_CREDIT/df$AMT_GOODS_PRICE
df$interest_rate = df$AMT_ANNUITY/df$AMT_CREDIT
df$debt_to_income = df$AMT_INCOME_TOTAL/df$AMT_CREDIT

# Car Value

df$car_value = ifelse(df$FLAG_OWN_CAR == "Y", 36843*(1-0.175)**(df$OWN_CAR_AGE), 0)

df = subset(df, select = -c(FLAG_OWN_CAR, OWN_CAR_AGE))

# Any applicant that is unemplyed either because of pension status or otherwise
# was listed as having not worked for 1000 years, this code takes care of that

df$DAYS_EMPLOYED[df$DAYS_EMPLOYED==365243] = 0

# There are a number of outliers and N/A's in OBS_30_CNT_SOCIAL_CIRCLE, and 
# OBS_60_CNT_SOCIAL_CIRCLE datasets that we need to take care of

df <- df %>% mutate(OBS_30_CNT_SOCIAL_CIRCLE = ifelse(OBS_30_CNT_SOCIAL_CIRCLE > 100, 
                                                                  NA, 
                                                                  OBS_30_CNT_SOCIAL_CIRCLE),  
                                OBS_60_CNT_SOCIAL_CIRCLE = ifelse(OBS_60_CNT_SOCIAL_CIRCLE > 100, 
                                                                  NA, 
                                                                  OBS_60_CNT_SOCIAL_CIRCLE))


#As a check, the N/A values are removed for AMT_ANNUITY and OBS_30_CNT_SOCIAL_CIRCLE


df = subset(df, !is.na(df$AMT_ANNUITY))
df = subset(df, !is.na(df$OBS_30_CNT_SOCIAL_CIRCLE))

# This part of the code involves imputing certain values for variables
# The first set of variables are EXT_SOURCE, as they are measures of reliability

score = subset(df, select = c(EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3, Credit_LIMIT_MEAN ))

# Using MICE, missing values are imputed through a linear regression prediction based on the existing values

imp_values = mice(data = score, m = 5, method = "norm", maxit = 5)
score_complete = complete(imp_values, 1)

df = subset(df, select = -c(EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3, Credit_LIMIT_MEAN))
df = cbind(df, score_complete)

# Imputing Using Sample

credit_bureau = subset(df, select = c(AMT_REQ_CREDIT_BUREAU_HOUR,
                                            AMT_REQ_CREDIT_BUREAU_DAY,
                                            AMT_REQ_CREDIT_BUREAU_WEEK,
                                            AMT_REQ_CREDIT_BUREAU_MON,
                                            AMT_REQ_CREDIT_BUREAU_QRT,
                                            AMT_REQ_CREDIT_BUREAU_YEAR))

imp_values = mice(data = credit_bureau, m = 5, method = "sample", maxit = 5)
credit_bureau_complete = complete(imp_values, 1)

df = subset(df, select = -c(AMT_REQ_CREDIT_BUREAU_HOUR,
                                        AMT_REQ_CREDIT_BUREAU_DAY,
                                        AMT_REQ_CREDIT_BUREAU_WEEK,
                                        AMT_REQ_CREDIT_BUREAU_MON,
                                        AMT_REQ_CREDIT_BUREAU_QRT,
                                        AMT_REQ_CREDIT_BUREAU_YEAR))
df = cbind(df, credit_bureau_complete)

# For age, it would be more efficient to group the ages into bands as opposed to having a continuous variable of days

age_df = data.frame(target = df$TARGET, age = as.integer(df$DAYS_BIRTH/-365))
summary(age_df$age)

age_df$age_group<-cut(age_df$age, breaks = seq(20, 70, by=5), right = TRUE)

age_df_interval<-age_df%>%
  group_by(age_group,
           target)%>%
  summarise(count = n(),
            count_percentage = n()/nrow(df)*100)

df = cbind(df, age_df$age_group)

# Finally, we omit any and all remaining N/A values

df = na.omit(df)

nums = select_if(df, is.numeric)

# Correlation plot for the numeric variables created to get a sense of the relationships between the features

corrplot(cor(nums), 
         method = "circle",
         order = "hclust",
         addgrid.col = F,
         addCoefasPercent = T,
         tl.pos = "lt",
         tl.cex = 0.4,
         tl.col = "black",
         sig.level = 0.000005)

df = subset(df, select = -c(SK_ID_CURR))

names(df) = make.names(names(df))

set.seed(100)

split = 0.8

# After cleaning the data, predictions can now be made on the feature importance
# First thing we do is create testing and training sets

index = createDataPartition(df$TARGET, p=split, list=FALSE)

train_data = df[index, ]
test_data  = df[-index, ]

train_data$TARGET = as.factor(train_data$TARGET)
str(train_data)

# In order to trim down the number of features to be used, a Random Forest model is used with bagging to find the most important ones using the randomForest model with 300 trees and 9 features per bag 9 features was chosen as it is close to the square root of the feature size.

rfModel = randomForest(TARGET ~ ., 
                       data = train_data,
                       ntree = 300, 
                       mtry = 9, 
                       replace = T)

varImpPlot(rfModel)

length(rfModel$predicted)

var_importance = cbind(names(subset(df, select = -c(TARGET))), varImp(rfModel))
var_importance = arrange(var_importance, desc(Overall))

var_importance$Features = var_importance$`names(subset(df, select = -c(TARGET)))`

col_names = as.data.frame(var_importance$Features[1:25])

names(df) %in% col_names$`var_importance$Features[1:25]`

important_data = subset(df, select = names(df) %in% col_names$`var_importance$Features[1:25]`)
important_data$TARGET = df$TARGET


# Once we have a dataset of only 25 of the most important features
# we can balance the dataset to an 50:50 ratio using the ROSE package

important_data_balanced = ovun.sample(TARGET~., data = important_data, method = "both",  p=0.6)$data
prop.table(table(important_data_balanced$TARGET))

train_data = important_data_balanced[index, ]
test_data  = important_data_balanced[-index, ]
train_data$TARGET = as.factor(train_data$TARGET)

# Once we have the most important features, and a balanced dataset, 
# the first model to run is Logistic Regression using lgm

logModel = glm(TARGET ~ ., data = train_data, family = "binomial", maxit = 100)

predictions_log = as.data.frame(as.factor(ifelse(predict(logModel, 
                                                         test_data, 
                                                         type = "response")>0.5, 1, 0)))


#Using caret and ROSE, a confusion matrix and ROC curve are
#created respectively

test_data$TARGET = as.factor(test_data$TARGET)

confusionMatrix(predictions_log$`as.factor(ifelse(predict(logModel, test_data, type = "response") > 0.5, 1, 0))`, 
                test_data$TARGET)

roc.curve(test_data$TARGET, 
          predictions_log$`as.factor(ifelse(predict(logModel, test_data, type = "response") > 0.5, 1, 0))`,
          plotit = T,
          main = "Logistic Regression Model")


#The second model to run is a repeat of the one used when selecting features

rfModel = randomForest(TARGET ~ ., 
                       data = train_data,
                       ntree = 300, 
                       mtry = 9, 
                       replace = T) 
predictions_rf = as.data.frame(predict(rfModel, 
                                       test_data, 
                                       type = "response"))

confusionMatrix(predictions_rf$`predict(rfModel, test_data, type = "response")`,
                test_data$TARGET)

roc.curve(test_data$TARGET, 
          predictions_rf$`predict(rfModel, test_data, type = "response")`,
          plotit = T,
          main = "ROC Curve for Random Forest w/ Bagging Model")


########################################
######### applications_to_score
########################################

# Repeating above steps for applications_to_score

App.ScoreA = plyr::join(df4,df2E,type='left')
App.Score = plyr::join(App.ScoreA,df3C,type='left')

df.Score = subset(App.Score, select = -c(NAME_TYPE_SUITE,
                             APARTMENTS_AVG,
                             BASEMENTAREA_AVG, 
                             YEARS_BEGINEXPLUATATION_AVG, 
                             YEARS_BUILD_AVG,
                             COMMONAREA_AVG,
                             ELEVATORS_AVG,
                             ENTRANCES_AVG,
                             FLOORSMAX_AVG,
                             FLOORSMIN_AVG,
                             LANDAREA_AVG,
                             LIVINGAPARTMENTS_AVG,
                             LIVINGAREA_AVG,
                             NONLIVINGAPARTMENTS_AVG,
                             NONLIVINGAREA_AVG,
                             APARTMENTS_MODE,
                             BASEMENTAREA_MODE,
                             YEARS_BEGINEXPLUATATION_MODE,
                             YEARS_BUILD_MODE,
                             COMMONAREA_MODE,
                             ELEVATORS_MODE,
                             ENTRANCES_MODE,
                             FLOORSMAX_MODE,
                             FLOORSMIN_MODE,
                             LANDAREA_MODE,
                             LIVINGAPARTMENTS_MODE,
                             LIVINGAREA_MODE,
                             NONLIVINGAPARTMENTS_MODE,
                             NONLIVINGAREA_MODE,
                             FONDKAPREMONT_MODE,
                             HOUSETYPE_MODE,
                             TOTALAREA_MODE,
                             WALLSMATERIAL_MODE,
                             EMERGENCYSTATE_MODE,
                             APARTMENTS_MEDI,
                             BASEMENTAREA_MEDI,
                             YEARS_BEGINEXPLUATATION_MEDI,
                             YEARS_BUILD_MEDI,
                             COMMONAREA_MEDI,
                             ELEVATORS_MEDI,
                             ENTRANCES_MEDI,
                             FLOORSMAX_MEDI,
                             FLOORSMIN_MEDI,LANDAREA_MEDI,
                             LIVINGAPARTMENTS_MEDI,
                             LIVINGAREA_MEDI,
                             NONLIVINGAPARTMENTS_MEDI,
                             NONLIVINGAREA_MEDI,
                             FLAG_DOCUMENT_4,
                             FLAG_DOCUMENT_6,
                             FLAG_DOCUMENT_7,
                             FLAG_DOCUMENT_9,
                             FLAG_DOCUMENT_10,
                             FLAG_DOCUMENT_11,
                             FLAG_DOCUMENT_12,
                             FLAG_DOCUMENT_15,
                             FLAG_DOCUMENT_17,
                             FLAG_DOCUMENT_19,
                             FLAG_DOCUMENT_20,
                             FLAG_DOCUMENT_21,
                             FLAG_MOBIL,
                             LIVE_CITY_NOT_WORK_CITY,
                             REG_CITY_NOT_WORK_CITY,
                             LIVE_REGION_NOT_WORK_REGION,
                             REG_REGION_NOT_WORK_REGION,
                             FLAG_PHONE,
                             FLAG_WORK_PHONE,
                             ORGANIZATION_TYPE,
                             FLAG_EMP_PHONE))

# Financial Ratio
df.Score$goods_covered = df.Score$AMT_CREDIT/df.Score$AMT_GOODS_PRICE
df.Score$interest_rate = df.Score$AMT_ANNUITY/df.Score$AMT_CREDIT
df.Score$debt_to_income = df.Score$AMT_INCOME_TOTAL/df.Score$AMT_CREDIT

# Car Value
df.Score$car_value = ifelse(df.Score$FLAG_OWN_CAR == "Y", 36843*(1-0.175)**(df.Score$OWN_CAR_AGE), 0)
df.Score = subset(df.Score, select = -c(FLAG_OWN_CAR, OWN_CAR_AGE))

# Days Employed
df.Score$DAYS_EMPLOYED[df.Score$DAYS_EMPLOYED==365243] = 0

df.Score <- df.Score %>% mutate(OBS_30_CNT_SOCIAL_CIRCLE = ifelse(OBS_30_CNT_SOCIAL_CIRCLE > 100, 
                                                      NA, 
                                                      OBS_30_CNT_SOCIAL_CIRCLE),  
                    OBS_60_CNT_SOCIAL_CIRCLE = ifelse(OBS_60_CNT_SOCIAL_CIRCLE > 100, 
                                                      NA, 
                                                      OBS_60_CNT_SOCIAL_CIRCLE))



#df.Score = subset(df.Score, !is.na(df.Score$AMT_ANNUITY))

score = subset(df.Score, select = c(EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3 ))

imp_values = mice(data = score, m = 5, method = "norm", maxit = 5)
score_complete = complete(imp_values, 1)

df.Score = subset(df.Score, select = -c(EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3))
df.Score = cbind(df.Score, score_complete)

credit_bureau = subset(df.Score, select = c(AMT_REQ_CREDIT_BUREAU_HOUR,
                                      AMT_REQ_CREDIT_BUREAU_DAY,
                                      AMT_REQ_CREDIT_BUREAU_WEEK,
                                      AMT_REQ_CREDIT_BUREAU_MON,
                                      AMT_REQ_CREDIT_BUREAU_QRT,
                                      AMT_REQ_CREDIT_BUREAU_YEAR))

imp_values = mice(data = credit_bureau, m = 5, method = "sample", maxit = 5)
credit_bureau_complete = complete(imp_values, 1)

df.Score = subset(df.Score, select = -c(AMT_REQ_CREDIT_BUREAU_HOUR,
                            AMT_REQ_CREDIT_BUREAU_DAY,
                            AMT_REQ_CREDIT_BUREAU_WEEK,
                            AMT_REQ_CREDIT_BUREAU_MON,
                            AMT_REQ_CREDIT_BUREAU_QRT,
                            AMT_REQ_CREDIT_BUREAU_YEAR))
df.Score = cbind(df.Score, credit_bureau_complete)

age_df = data.frame(df.Score$SK_ID_CURR, age = as.integer(df.Score$DAYS_BIRTH/-365))
summary(age_df$age)

age_df$age_group<-cut(age_df$age, breaks = seq(20, 70, by=5), right = TRUE)

df.Score = cbind(df.Score, age_df$age_group)

df.Score <- df.Score %>% rename(age_df.age_group = 'age_df$age_group')

View(df.Score)

important_data_score = subset(df.Score, select = names(df.Score) %in% col_names$`var_importance$Features[1:25]`)
important_data_score$TARGET = df.Score$TARGET

View(important_data_score)

nrow(important_data_score)

checkDataQuality(data = important_data_score, 
                 out.file.num ="C:/DataSet/Project2/important_data_score_num.csv", 
                 out.file.cat= "C:/DataSet/Project2/important_data_score_cat.csv")

important_data_score_num = read.csv("C:/DataSet/Project2/important_data_score_num.csv")
important_data_score_cat = read.csv("C:/DataSet/Project2/important_data_score_cat.csv")

View(important_data_score_num)
View(important_data_score_cat)

##############################################

previous = subset(important_data_score, select = c(AMT_ANNUITY, AMT_GOODS_PRICE, previous_approved, previous_refused, previous_count, goods_covered, interest_rate, Credit_LIMIT_MEAN))

imp_values = mice(data = previous, m = 5, method = "norm", maxit = 5)
score_complete = complete(imp_values, 1)

important_data_score = subset(important_data_score, select = -c(AMT_ANNUITY, AMT_GOODS_PRICE, previous_approved, previous_refused, previous_count, goods_covered, interest_rate, Credit_LIMIT_MEAN))

important_data_score = cbind(important_data_score, score_complete)

#######################

checkDataQuality(data = important_data_score, 
                 out.file.num ="C:/DataSet/Project2/important_data_score_num.csv", 
                 out.file.cat= "C:/DataSet/Project2/important_data_score_cat.csv")

important_data_score_num = read.csv("C:/DataSet/Project2/important_data_score_num.csv")
important_data_score_cat = read.csv("C:/DataSet/Project2/important_data_score_cat.csv")

View(important_data_score_num)
View(important_data_score_cat)


#####################################

important_data_score$TARGET = as.factor(important_data_score$TARGET)

View(important_data_score)

predictions_rf = predict(rfModel, important_data_score, type = "response")

final = data.frame(SK_ID_CURR = App.Score$SK_ID_CURR)

final$Target = predictions_rf

View(final)

write.csv(final,"C:/DataSet/Project2/predictions_rf.csv")


#########################################
###################
################### Approach 2 - XGBoost & CatBoost
###################
#########################################



##################################
######Merge External Data#########
##################################
df1<-read.csv("/Users/yiming/Desktop/project2/application_train (1).csv",header = TRUE, stringsAsFactors = TRUE)
df2<-read.csv("/Users/yiming/Desktop/project2/previous_application (1).csv",header = TRUE, stringsAsFactors = TRUE)
df3<-read.csv("/Users/yiming/Desktop/project2/credit_card_balance (1).csv",header = TRUE, stringsAsFactors = TRUE)
df4<-read.csv("/Users/yiming/Desktop/project2/applications_to_score.csv",header = TRUE, stringsAsFactors = TRUE)
#aggregate data
df22<-lapply(df2,as.numeric)#transfer all dataset to numerical
df22<-aggregate(df22,by=list(df22$SK_ID_CURR),FUN = sum)#aggregate
df22<-df22[,-c(2,3)]
colnames(df22)<-paste(colnames(df22),'previous',sep="_")
names(df22)[1]<-'SK_ID_CURR'
df33<-lapply(df3,as.numeric)
df33<-aggregate(df33,by=list(df33$SK_ID_CURR),FUN = sum)
df33<-df33[,-c(2,3)]
colnames(df33)<-paste(colnames(df33),'credit_info',sep="_")
names(df33)[1]<-'SK_ID_CURR'
#merge data
#left outer join: return all rows from the left table, and any rows with matching keys from the right table
library(plyr)
oriA<-join(df1,df22,type='left')
ori<-join(oriA,df33,type='left')
scoredataA<-join(df4,df22,type='left')
scoredata<-join(scoredataA,df33,type='left')
write.csv(ori,"/Users/yiming/Desktop/project2/mergedOriginalData.csv")
write.csv(scoredata,"/Users/yiming/Desktop/project2/mergedSubmissionData.csv")

###############################################
#############CLEAN THE MERGING DATA############
###############################################
ori <- read.csv("/Users/yiming/Desktop/project2/mergedOriginalData.csv", header = TRUE, stringsAsFactors = TRUE)

### FEATURE ENGINEERING
library(dataQualityR)
checkDataQuality(data = ori, 
                 out.file.num ="dq_num.csv", 
                 out.file.cat= "dq_cat.csv")
dq_num<-read.csv("~/Downloads/dq_num.csv")
dq_cat<-read.csv("~/Downloads/dq_cat.csv")
View(dq_num)   
View(dq_cat)   



# Now, we dummified all categorical variables
library(dplyr)
ori.df <- select(ori,
                 SK_ID_CURR,
                 NAME_CONTRACT_TYPE,
                 CODE_GENDER,
                 FLAG_OWN_CAR,
                 FLAG_OWN_REALTY, 
                 NAME_TYPE_SUITE,
                 NAME_INCOME_TYPE,
                 NAME_EDUCATION_TYPE,
                 NAME_FAMILY_STATUS,
                 NAME_HOUSING_TYPE,
                 OCCUPATION_TYPE,
                 WEEKDAY_APPR_PROCESS_START,
                 ORGANIZATION_TYPE,
                 FONDKAPREMONT_MODE,
                 HOUSETYPE_MODE,
                 WALLSMATERIAL_MODE,
                 EMERGENCYSTATE_MODE)

str(ori.df)
dataDummy <- dummyVars("~.",data=ori.df, fullRank=F)
data.dummified <- as.data.frame(predict(dataDummy,ori.df))
sum(is.na(data.dummified))
oriAN<-ori[,-c(1,4,5,6,7,13,14,15,16,17,30,34,42,88,89,91,92)]
#Replace missing values in each column with its column mean
for(i in 1:ncol(oriAN)){
  oriAN[is.na(oriAN[,i]), i] <- mean(oriAN[,i], na.rm = TRUE)
}
#check NA 
sum(is.na(oriAN))

library(plyr)
ori.clean<-join(oriAN,data.dummified,type='left')


score <- read.csv("/Users/yiming/Desktop/project2/mergedSubmissionData.csv", header = TRUE, stringsAsFactors = TRUE)
library(dataQualityR)
checkDataQuality(data = score, 
                 out.file.num ="dq_num.csv", 
                 out.file.cat= "dq_cat.csv")
dq_num<-read.csv("~/Downloads/dq_num.csv")
dq_cat<-read.csv("~/Downloads/dq_cat.csv")
View(dq_num)   
View(dq_cat)   
score.df <- select(score,
                   SK_ID_CURR,
                   NAME_CONTRACT_TYPE,
                   CODE_GENDER,
                   FLAG_OWN_CAR,
                   FLAG_OWN_REALTY, 
                   NAME_TYPE_SUITE,
                   NAME_INCOME_TYPE,
                   NAME_EDUCATION_TYPE,
                   NAME_FAMILY_STATUS,
                   NAME_HOUSING_TYPE,
                   OCCUPATION_TYPE,
                   WEEKDAY_APPR_PROCESS_START,
                   ORGANIZATION_TYPE,
                   FONDKAPREMONT_MODE,
                   HOUSETYPE_MODE,
                   WALLSMATERIAL_MODE,
                   EMERGENCYSTATE_MODE)
str(score.df)
score.dataDummy <- dummyVars("~.",data=score.df, fullRank=F)
Score_data.dummified <- as.data.frame(predict(score.dataDummy,score.df))
sum(is.na(Score_data.dummified))
scoreAN<-score[,-c(1,3,4,5,6,7,13,14,15,16,17,30,34,42,88,89,91,92)]
#Replace missing values in each column with its column mean
for(i in 1:ncol(scoreAN)){
  scoreAN[is.na(scoreAN[,i]), i] <- mean(scoreAN[,i], na.rm = TRUE)
}
#check NA 
sum(is.na(scoreAN))
summary(score$CODE_GENDER)
summary(score$NAME_INCOME_TYPE)
summary(score$ORGANIZATION_TYPE)
# correlationMatrix <- cor(scoreAN[,3:161])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)
# #remove attributes that are highly correlated with each other
# scoreAN<-scoreAN[,-c(highlyCorrelated)]
library(plyr)
score.clean<-join(scoreAN,Score_data.dummified,type='left')
library(dplyr)
setdiff(colnames(data.dummified),  colnames(Score_data.dummified))
score.clean$CODE_GENDER.XNA<-0
score.clean$NAME_INCOME_TYPE.Businessman<-0
score.clean$NAME_INCOME_TYPE.Student<-0
score.clean$TARGET<-NA
dfname<-'score.clean'
Variable<-'NAME_INCOME_TYPE.Maternity leave'
score.clean<-within ( assign(dfname  , get(dfname) ),
                      assign(Variable, 0         ))
dfname<-'score.clean'
Variable<-'ORGANIZATION_TYPE.Industry: type 8'
score.clean<-within ( assign(dfname  , get(dfname) ),
                      assign(Variable, 0         ))
colnames(ori.clean) %in% colnames(score.clean) %>% table
write.csv(ori.clean,"/Users/yiming/Desktop/project2/oriClean.csv")
write.csv(score.clean,"/Users/yiming/Desktop/project2/submissionClean.csv")

######################
#####Balance Data#####
######################
#get the data
ori<-read.csv('/Users/yiming/Desktop/project2/oriClean.csv')
index <- sample(1:nrow(ori),(.1)*nrow(ori))  # technique to reduce dataset
ori <- ori [index, ]
str(ori)
table(ori$TARGET)   
prop.table(table(ori$TARGET)) #check target 0 & 1 prop


library(caret)  #function createDataPartition()
#Create our training dataset with a 70/30 split
set.seed(88)
indexSplit <- createDataPartition(ori$TARGET, p=0.7, list=FALSE)
train.imbalanced <- ori[indexSplit,] 
test<- ori[-indexSplit,] 
table(train.imbalanced$TARGET)
prop.table(table(train.imbalanced$TARGET)) #imbalance prop

library(ROSE)
train.under<-ovun.sample(TARGET~., data = train.imbalanced, method = "under", N= 5000)$data  # this runs!
table(train.under$TARGET)
prop.table(table(train.under$TARGET))#undersampling prop

train.over<-ovun.sample(TARGET~., data = train.imbalanced, method = "over", N=30000 )$data
prop.table(table(train.over$TARGET))#oversampling prop

train.both<-ovun.sample(TARGET~., data = train.imbalanced, method = "both", N= 30000)$data
prop.table(table(train.both$TARGET))#both prop        

###########################
########CATBOOST###########
###########################

######imbalance######
library(catboost)
library(dplyr)
y_train.imbalance <- unlist(train.imbalanced[c('TARGET')])
X_train.imbalance <- train.imbalanced %>% select(-TARGET)
train_pool.imbalance <- catboost.load_pool(data = X_train.imbalance, label = y_train.imbalance)
y_test <- unlist(test[c('TARGET')])
X_test<- test %>% select(-TARGET)
test_pool <- catboost.load_pool(data = X_test, label = y_test)
imbalanceModel.catboost<- catboost.train(learn_pool = train_pool.imbalance,params = list(loss_function = 'Logloss',iterations = 100))
y_pred.imbalance=catboost.predict(imbalanceModel.catboost,test_pool)
roc_test <- roc( y_test, y_pred.imbalance, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_pred.imbalance=catboost.predict(imbalanceModel.catboost,test_pool,prediction_type = 'Class')
Table_App <- table(data.frame(Prediction = y_pred.imbalance, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)
confusionMatrix(table(y_pred.imbalance,test$TARGET), positive = "1")

#Feature selection
catboostFeatureimportance<-catboost.get_feature_importance(imbalanceModel.catboost, 
                                                           pool = train_pool.imbalance, 
                                                           type = 'FeatureImportance',
                                                           thread_count = -1)
summary(catboostFeatureimportance)
featureLargerthanMean <- which(catboostFeatureimportance > 0.32467) 

X_train.imbalanceImp <- train.imbalanced %>% select(c(featureLargerthanMean))
X_train.imbalanceImp<-X_train.imbalanceImp %>% select(-X)

X_test.imbalanceImp<- test %>% select(c(featureLargerthanMean))
X_test.imbalanceImp<-X_test.imbalanceImp %>% select(-X)

train_poolImp.imbalance <- catboost.load_pool(data = X_train.imbalanceImp, label = y_train.imbalance)
test_poolImp.imbalance <- catboost.load_pool(data = X_test.imbalanceImp, label = y_test)

imbalanceModelImp.catboost<- catboost.train(learn_pool = train_poolImp.imbalance,params = list(loss_function = 'Logloss',iterations = 100))
y_predImp.imbalance=catboost.predict(imbalanceModelImp.catboost,test_poolImp.imbalance)
roc_test <- roc( y_test, y_predImp.imbalance, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_predImp.imbalance=catboost.predict(imbalanceModelImp.catboost,test_poolImp.imbalance,prediction_type = 'Class')
Table_App <- table(data.frame(Prediction = y_predImp.imbalance, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)
confusionMatrix(table(y_predImp.imbalance,test$TARGET), positive = "1")

######over sampling########
y_train.over <- unlist(train.over[c('TARGET')])
X_train.over <- train.over %>% select(-TARGET)
train_pool.over <- catboost.load_pool(data = X_train.over, label = y_train.over)
y_test <- unlist(test[c('TARGET')])
X_test<- test %>% select(-TARGET)
test_pool <- catboost.load_pool(data = X_test, label = y_test)
overModel.catboost<- catboost.train(learn_pool = train_pool.over,params = list(loss_function = 'Logloss',iterations = 100))
y_pred.over=catboost.predict(overModel.catboost,test_pool)
roc_test <- roc( y_test, y_pred.over, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_pred.over=catboost.predict(overModel.catboost,test_pool,prediction_type = 'Class')
confusionMatrix(table(y_pred.over,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_pred.over, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)
#Feature selection
catboostFeatureimportance<-catboost.get_feature_importance(overModel.catboost, 
                                                           pool = train_pool.over, 
                                                           type = 'FeatureImportance',
                                                           thread_count = -1)
summary(catboostFeatureimportance)
featureLargerthanMean <- which(catboostFeatureimportance > 0.32467) 

X_train.overImp <- train.over %>% select(c(featureLargerthanMean))
X_train.overImp<-X_train.overImp %>% select(-X)

X_test.overImp<- test %>% select(c(featureLargerthanMean))
X_test.overImp<-X_test.overImp %>% select(-X)

train_poolImp.over <- catboost.load_pool(data = X_train.overImp, label = y_train.over)
test_poolImp.over <- catboost.load_pool(data = X_test.overImp, label = y_test)

overModelImp.catboost<- catboost.train(learn_pool = train_poolImp.over,params = list(loss_function = 'Logloss',iterations = 100))
y_predImp.over=catboost.predict(overModelImp.catboost,test_poolImp.over)
roc_test <- roc( y_test, y_predImp.over, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_predImp.over=catboost.predict(overModelImp.catboost,test_poolImp.over,prediction_type = 'Class')
confusionMatrix(table(y_predImp.over,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_predImp.over, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)
#####under sampling#######
y_train.under <- unlist(train.under[c('TARGET')])
X_train.under <- train.under %>% select(-TARGET)
train_pool.under <- catboost.load_pool(data = X_train.under, label = y_train.under)
y_test <- unlist(test[c('TARGET')])
X_test<- test %>% select(-TARGET)
test_pool <- catboost.load_pool(data = X_test, label = y_test)
underModel.catboost<- catboost.train(learn_pool = train_pool.under,params = list(loss_function = 'Logloss',iterations = 100))
y_pred.under=catboost.predict(underModel.catboost,test_pool)
roc_test <- roc( y_test, y_pred.under, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_pred.under=catboost.predict(underModel.catboost,test_pool,prediction_type = 'Class')
Table_App <- table(data.frame(Prediction = y_pred.under, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)
confusionMatrix(table(y_pred.under,test$TARGET), positive = "1")
#Feature selection
catboostFeatureimportance<-catboost.get_feature_importance(underModel.catboost, 
                                                           pool = train_pool.under, 
                                                           type = 'FeatureImportance',
                                                           thread_count = -1)
summary(catboostFeatureimportance)
featureLargerthanMean <- which(catboostFeatureimportance > 0.32467) 

X_train.underImp <- train.under %>% select(c(featureLargerthanMean))
X_train.underImp<-X_train.underImp %>% select(-X)

X_test.underImp<- test %>% select(c(featureLargerthanMean))
X_test.underImp<-X_test.underImp %>% select(-X)

train_poolImp.under <- catboost.load_pool(data = X_train.underImp, label = y_train.under)
test_poolImp.under <- catboost.load_pool(data = X_test.underImp, label = y_test)
underModelImp.catboost<- catboost.train(learn_pool = train_poolImp.under,params = list(loss_function = 'Logloss',iterations = 100))
y_predImp.under=catboost.predict(underModelImp.catboost,test_poolImp.under)
roc_test <- roc( y_test, y_predImp.under, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_predImp.under=catboost.predict(underModelImp.catboost,test_poolImp.under,prediction_type = 'Class')
Table_App <- table(data.frame(Prediction = y_predImp.under, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)
confusionMatrix(table(y_predImp.under,test$TARGET), positive = "1")

#######using both sampling######
y_train.both <- unlist(train.both[c('TARGET')])
X_train.both <- train.both %>% select(-TARGET)
train_pool.both <- catboost.load_pool(data = X_train.both, label = y_train.both)
y_test <- unlist(test[c('TARGET')])
X_test<- test %>% select(-TARGET)
test_pool <- catboost.load_pool(data = X_test, label = y_test)
bothModel.catboost<- catboost.train(learn_pool = train_pool.both,params = list(loss_function = 'Logloss',iterations = 100))
y_pred.both=catboost.predict(bothModel.catboost,test_pool)
roc_test <- roc( y_test, y_pred.both, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_pred.both=catboost.predict(bothModel.catboost,test_pool,prediction_type = 'Class')
Table_App <- table(data.frame(Prediction = y_pred.both, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)
confusionMatrix(table(y_pred.both,test$TARGET), positive = "1")
#Feature selection
catboostFeatureimportance<-catboost.get_feature_importance(overModel.catboost, 
                                                           pool = train_pool.over, 
                                                           type = 'FeatureImportance',
                                                           thread_count = -1)
summary(catboostFeatureimportance)
featureLargerthanMean <- which(catboostFeatureimportance > 0.32467) 

X_train.bothImp <- train.both %>% select(c(featureLargerthanMean))
X_train.bothImp<-X_train.bothImp %>% select(-X)

X_test.bothImp<- test %>% select(c(featureLargerthanMean))
X_test.bothImp<-X_test.bothImp %>% select(-X)

train_poolImp.both <- catboost.load_pool(data = X_train.bothImp, label = y_train.both)
test_poolImp.both <- catboost.load_pool(data = X_test.bothImp, label = y_test)

bothModelImp.catboost<- catboost.train(learn_pool = train_poolImp.both,params = list(loss_function = 'Logloss',iterations = 100))
y_predImp.both=catboost.predict(bothModelImp.catboost,test_poolImp.both)
roc_test <- roc( y_test, y_predImp.both, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_predImp.both=catboost.predict(bothModelImp.catboost,test_poolImp.both,prediction_type = 'Class')
Table_App <- table(data.frame(Prediction = y_predImp.both, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)
confusionMatrix(table(y_predImp.both,test$TARGET), positive = "1")

#######################
######XG BOOST#########
#######################
library(xgboost)
library(dplyr)
library(pROC)
####imbalance data#####
y_train.imbalance <- unlist(train.imbalanced[c('TARGET')])
X_train.imbalance <- train.imbalanced %>% select(-TARGET)
y_test <- unlist(test[c('TARGET')])
X_test<- test %>% select(-TARGET)
imbalanceModel.xgboost<- xgboost(data = as.matrix(X_train.imbalance), label = as.matrix(y_train.imbalance), max.depth = 8, eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
y_pred.imbalance<-predict(imbalanceModel.xgboost, as.matrix(X_test))
roc_test <- roc( y_test, y_pred.imbalance, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_pred.imbalance <- as.numeric(y_pred.imbalance > 0.5)
confusionMatrix(table(y_pred.imbalance,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_pred.imbalance, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)


#Feature selection
xgImportance_matrix <- xgb.importance(model = imbalanceModel.xgboost)
xgb.plot.importance(importance_matrix = xgImportance_matrix)
summary(xgImportance_matrix)
featureLargerthanMean <- xgImportance_matrix[which(xgImportance_matrix$Importance > 0.0049751)] 

X_train.imbalanceImp <- train.imbalanced %>% select(c(featureLargerthanMean$Feature))
X_train.imbalanceImp<-X_train.imbalanceImp %>% select(-X)

X_test.imbalanceImp<- test %>% select(c(featureLargerthanMean$Feature))
X_test.imbalanceImp<-X_test.imbalanceImp %>% select(-X)

imbalanceModelImp.xgboost<-xgboost(data = as.matrix(X_train.imbalanceImp), label = as.matrix(y_train.imbalance), max.depth = 8, eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
y_predImp.imbalance<-predict(imbalanceModelImp.xgboost, as.matrix(X_test.imbalanceImp))
roc_test <- roc( y_test, y_predImp.imbalance, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_predImp.imbalance <- as.numeric(y_predImp.imbalance > 0.5)
confusionMatrix(table(y_predImp.imbalance,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_predImp.imbalance, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)


######over sampling#####
y_train.over <- unlist(train.over[c('TARGET')])
X_train.over <- train.over %>% select(-TARGET)
y_test <- unlist(test[c('TARGET')])
X_test<- test %>% select(-TARGET)
overModel.xgboost<- xgboost(data = as.matrix(X_train.over), label = as.matrix(y_train.over), max.depth = 8, eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
y_pred.over<-predict(overModel.xgboost, as.matrix(X_test))
roc_test <- roc( y_test, y_pred.over, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_pred.over <- as.numeric(y_pred.over > 0.5)
confusionMatrix(table(y_pred.over,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_pred.over, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)


#Feature selection
xgImportance_matrix <- xgb.importance(model = overModel.xgboost)
xgb.plot.importance(importance_matrix = xgImportance_matrix)
summary(xgImportance_matrix)
featureLargerthanMean <- xgImportance_matrix[which(xgImportance_matrix$Importance > 0.00462963)] 

X_train.overImp <- train.over %>% select(c(featureLargerthanMean$Feature))
X_train.overImp<-X_train.overImp %>% select(-X)

X_test.overImp<- test %>% select(c(featureLargerthanMean$Feature))
X_test.overImp<-X_test.overImp %>% select(-X)

overModelImp.xgboost<-xgboost(data = as.matrix(X_train.overImp), label = as.matrix(y_train.over), max.depth = 8, eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
y_predImp.over<-predict(overModelImp.xgboost, as.matrix(X_test.overImp))
roc_test <- roc( y_test, y_predImp.over, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_predImp.over <- as.numeric(y_predImp.over > 0.5)
confusionMatrix(table(y_predImp.over,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_predImp.over, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)

#####under sampling#####
y_train.under <- unlist(train.under[c('TARGET')])
X_train.under <- train.under %>% select(-TARGET)
y_test <- unlist(test[c('TARGET')])
X_test<- test %>% select(-TARGET)
underModel.xgboost<- xgboost(data = as.matrix(X_train.under), label = as.matrix(y_train.under), max.depth = 8, eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
y_pred.under<-predict(underModel.xgboost, as.matrix(X_test))
roc_test <- roc( y_test, y_pred.under, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_pred.under <- as.numeric(y_pred.under > 0.5)
confusionMatrix(table(y_pred.under,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_pred.under, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)

#Feature selection
xgImportance_matrix <- xgb.importance(model = underModel.xgboost)
xgb.plot.importance(importance_matrix = xgImportance_matrix)
summary(xgImportance_matrix)
featureLargerthanMean <- xgImportance_matrix[which(xgImportance_matrix$Importance > 0.0055556)] 

X_train.underImp <- train.under %>% select(c(featureLargerthanMean$Feature))
X_train.underImp<-X_train.underImp %>% select(-X)

X_test.underImp<- test %>% select(c(featureLargerthanMean$Feature))
X_test.underImp<-X_test.underImp %>% select(-X)

underModelImp.xgboost<-xgboost(data = as.matrix(X_train.underImp), label = as.matrix(y_train.under), max.depth = 8, eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
y_predImp.under<-predict(underModelImp.xgboost, as.matrix(X_test.underImp))
roc_test <- roc( y_test, y_predImp.under, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_predImp.under <- as.numeric(y_predImp.under > 0.5)
confusionMatrix(table(y_predImp.under,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_predImp.under, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)

#######both sampling######
y_train.both <- unlist(train.both[c('TARGET')])
X_train.both <- train.both %>% select(-TARGET)
y_test <- unlist(test[c('TARGET')])
X_test<- test %>% select(-TARGET)
bothModel.xgboost<- xgboost(data = as.matrix(X_train.both), label = as.matrix(y_train.both), max.depth = 8, eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
y_pred.both<-predict(bothModel.xgboost, as.matrix(X_test))
roc_test <- roc( y_test, y_pred.both, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_pred.both <- as.numeric(y_pred.both > 0.5)
confusionMatrix(table(y_pred.both,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_pred.both, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)
#Feature selection
xgImportance_matrix <- xgb.importance(model = bothModel.xgboost)
xgb.plot.importance(importance_matrix = xgImportance_matrix)
summary(xgImportance_matrix)
featureLargerthanMean <- xgImportance_matrix[which(xgImportance_matrix$Importance > 0.00483092)] 

X_train.bothImp <- train.both %>% select(c(featureLargerthanMean$Feature))
X_train.bothImp<-X_train.bothImp %>% select(-X)

X_test.bothImp<- test %>% select(c(featureLargerthanMean$Feature))
X_test.bothImp<-X_test.bothImp %>% select(-X)

bothModelImp.xgboost<-xgboost(data = as.matrix(X_train.bothImp), label = as.matrix(y_train.both), max.depth = 8, eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
y_predImp.both<-predict(bothModelImp.xgboost, as.matrix(X_test.bothImp))
roc_test <- roc( y_test, y_predImp.both, algorithm = 2) 
plot(roc_test)
auc(roc_test)
y_predImp.both <- as.numeric(y_predImp.over > 0.5)
confusionMatrix(table(y_predImp.both,test$TARGET), positive = "1")
Table_App <- table(data.frame(Prediction = y_predImp.both, Actual = test$TARGET))
accuracy <- (Table_App[1,1]+Table_App[2,2])/sum(Table_App)
precision <- (Table_App[1,1])/sum(Table_App[,1])
recall <- Table_App[1,1]/sum(Table_App[1,])
fscore <- (2*precision*recall)/(precision+recall)
gscore <- sqrt(precision*recall)

#########################################
###################
################### Approach 3 - GBM Approach
###################
#########################################

library(dplyr)
#get the data
ori<-read.csv('~/Desktop/Business Analytics/BA Project 2/oriClean.csv', header = TRUE, stringsAsFactors = TRUE)
index <- sample(1:nrow(ori),(.1)*nrow(ori))  # technique to reduce dataset
ori <- ori [index, ]
str(ori)
table(ori$TARGET)   
prop.table(table(ori$TARGET)) #check target 0 & 1 prop

outcomeName <- 'TARGET'
predictorNames <- names(ori)[names(ori) != outcomeName]  # creating a list of features 

ori$TARGET<-as.factor(ori$TARGET)

library(caret)  #function createDataPartition()
#Create our training dataset with a 70/30 split
set.seed(88)
indexSplit <- createDataPartition(ori$TARGET, p=0.7, list=FALSE)
imbalanced.train <- ori[indexSplit,] 
test<- ori[-indexSplit,] 
table(imbalanced.train$TARGET)
prop.table(table(imbalanced.train$TARGET)) #imbalance prop

library(ROSE)
under.train<-ovun.sample(TARGET~., data = imbalanced.train, method = "under", N= 5000)$data  # this runs!
table(under.train$TARGET)
prop.table(table(under.train$TARGET))#undersampling prop

over.train<-ovun.sample(TARGET~., data = imbalanced.train, method = "over", N=30000 )$data
prop.table(table(over.train$TARGET))#oversampling prop

both.train<-ovun.sample(TARGET~., data = imbalanced.train, method = "both", N= 30000)$data
prop.table(table(both.train$TARGET))#both prop  


##############################################################
########## IMBALANCED
##############################################################
fitControl <- trainControl(method = "none")
imbalanced.model <- train(TARGET ~ ., data = imbalanced.train, 
                          method = "gbm", 
                          trControl = fitControl, 
                          verbose = FALSE, 
                          tuneGrid = data.frame(interaction.depth = 8,
                                                n.trees = 100,
                                                shrinkage = .1,
                                                n.minobsinnode = 20))

Imp.imbalanced<-varImp(imbalanced.model) #generates error.  run with line below
Imp.imbalanced<-summary(imbalanced.model)
Imp.imbalanced

imbalanced.predict<-predict(imbalanced.model,test[,predictorNames],type="raw")
confusionMatrix(imbalanced.predict,test[,outcomeName],positive = "1")
#FEATURE SELECTION
Imp.imbalanced1 <- as.data.frame(Imp.imbalanced)
summary(Imp.imbalanced1$rel.inf)
imbalanced.m <- as.numeric(mean(Imp.imbalanced1$rel.inf)) #mean
Imp.imbalanced1 <- Imp.imbalanced1[(Imp.imbalanced1$rel.inf <= imbalanced.m),,drop = FALSE]
imbalanced_to_drop <- row.names(Imp.imbalanced1)
imbalanced.train.new<- imbalanced.train[!names(imbalanced.train) %in% imbalanced_to_drop] #drop the features that are less than mean
imbalanced.test.new<- test[!names(test) %in% imbalanced_to_drop]

imbalanced.outcome <- 'TARGET'
imbalanced.predictors <- names(imbalanced.train.new)[names(imbalanced.train.new) != imbalanced.outcome]
#RE-RUN IMBALANCED
imbalanced.model.new <- train(TARGET ~ ., data = imbalanced.train.new, 
                              method = "gbm", 
                              trControl = fitControl, 
                              verbose = FALSE, 
                              tuneGrid = data.frame(interaction.depth = 8,
                                                    n.trees = 100,
                                                    shrinkage = .1,
                                                    n.minobsinnode = 20))

imbalanced.predict.new<-predict(imbalanced.model.new,imbalanced.test.new[,imbalanced.predictors],type="raw")
confusionMatrix(imbalanced.predict.new,imbalanced.test.new[,imbalanced.outcome],positive = "1")

##############################################################
########## UNDER SAMPLING
##############################################################
fitControl <- trainControl(method = "none")
under.model <- train(TARGET ~ ., data = under.train, 
                     method = "gbm", 
                     trControl = fitControl, 
                     verbose = FALSE, 
                     ## Only a single model can be passed to the
                     ## function when no resampling is used:
                     tuneGrid = data.frame(interaction.depth = 8,
                                           n.trees = 100,
                                           shrinkage = .1,
                                           n.minobsinnode = 20))

Imp.under<-varImp(under.model) #generates error.  run with line below
Imp.under<-summary(under.model)
Imp.under

under.predict<-predict(under.model,test[,predictorNames],type="raw")
confusionMatrix(under.predict,test[,outcomeName],positive = "1")
#FEATURE SELECTION
Imp.under1 <- as.data.frame(Imp.under)
summary(Imp.under1$rel.inf)
under.m <- as.numeric(mean(Imp.under1$rel.inf)) #mean
Imp.under1 <- Imp.under1[(Imp.under1$rel.inf <= under.m),,drop = FALSE] 
under_to_drop <- row.names(Imp.under1)
under.train.new<- under.train[!names(under.train) %in% under_to_drop] #drop the features that are less than mean
under.test.new<- test[!names(test) %in% under_to_drop]

under.outcome <- 'TARGET'
under.predictors <- names(under.train.new)[names(under.train.new) != under.outcome]
#RE-RUN UNDER SAMPLING
under.model.new <- train(TARGET ~ ., data = under.train.new, 
                         method = "gbm", 
                         trControl = fitControl, 
                         verbose = FALSE, 
                         ## Only a single model can be passed to the
                         ## function when no resampling is used:
                         tuneGrid = data.frame(interaction.depth = 8,
                                               n.trees = 100,
                                               shrinkage = .1,
                                               n.minobsinnode = 20))

under.predict.new<-predict(under.model.new,under.test.new[,under.predictors],type="raw")
confusionMatrix(under.predict.new,under.test.new[,under.outcome],positive = "1")


##############################################################
########## OVER SAMPLING
##############################################################
fitControl <- trainControl(method = "none")
over.model <- train(TARGET ~ ., data = over.train, 
                    method = "gbm", 
                    trControl = fitControl, 
                    verbose = FALSE, 
                    ## Only a single model can be passed to the
                    ## function when no resampling is used:
                    tuneGrid = data.frame(interaction.depth = 8,
                                          n.trees = 100,
                                          shrinkage = .1,
                                          n.minobsinnode = 20))

Imp.over<-varImp(over.model) #generates error.  run with line below
Imp.over<-summary(over.model)
Imp.over

over.predict<-predict(over.model,test[,predictorNames],type="raw")
confusionMatrix(over.predict,test[,outcomeName],positive = "1")
#FEATURE SELECTION
Imp.over1 <- as.data.frame(Imp.over)
summary(Imp.over1$rel.inf)
over.m <- as.numeric(mean(Imp.over1$rel.inf)) #mean
Imp.over1 <- Imp.over1[(Imp.over1$rel.inf <= over.m),,drop = FALSE]
over_to_drop <- row.names(Imp.over1)
over.train.new<- over.train[!names(over.train) %in% over_to_drop] #drop the features that are less than mean
over.test.new<- test[!names(test) %in% over_to_drop]

over.outcome <- 'TARGET'
over.predictors <- names(over.train.new)[names(over.train.new) != over.outcome]
#RE-RUN OVER SAMPLING
over.model.new <- train(TARGET ~ ., data = over.train.new, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        ## Only a single model can be passed to the
                        ## function when no resampling is used:
                        tuneGrid = data.frame(interaction.depth = 8,
                                              n.trees = 100,
                                              shrinkage = .1,
                                              n.minobsinnode = 20))

over.predict.new<-predict(over.model.new,over.test.new[,over.predictors],type="raw")
confusionMatrix(over.predict.new,over.test.new[,over.outcome],positive = "1")


##############################################################
########## BOTH SAMPLING
##############################################################
fitControl <- trainControl(method = "none")
both.model <- train(TARGET ~ ., data = both.train, 
                    method = "gbm", 
                    trControl = fitControl, 
                    verbose = FALSE, 
                    ## Only a single model can be passed to the
                    ## function when no resampling is used:
                    tuneGrid = data.frame(interaction.depth = 8,
                                          n.trees = 100,
                                          shrinkage = .1,
                                          n.minobsinnode = 20))

Imp.both<-varImp(both.model) #generates error.  run with line below
Imp.both<-summary(both.model)
Imp.both

both.predict<-predict(both.model,test[,predictorNames],type="raw")
confusionMatrix(both.predict,test[,outcomeName],positive = "1")
#FEATURE SELECTION
Imp.both1 <- as.data.frame(Imp.both)
summary(Imp.both1$rel.inf)
both.m <- as.numeric(mean(Imp.both1$rel.inf)) #mean
Imp.both1 <- Imp.both1[(Imp.both1$rel.inf <= both.m),,drop = FALSE]
both_to_drop <- row.names(Imp.both1)
both.train.new<- both.train[!names(both.train) %in% both_to_drop]#drop the features that are less than mean
both.test.new<- test[!names(test) %in% both_to_drop]

both.outcome <- 'TARGET'
both.predictors <- names(both.train.new)[names(both.train.new) != both.outcome]
#RE-RUN BOTH SAMPLING
both.model.new <- train(TARGET ~ ., data = both.train.new, 
                        method = "gbm", 
                        trControl = fitControl, 
                        verbose = FALSE, 
                        ## Only a single model can be passed to the
                        ## function when no resampling is used:
                        tuneGrid = data.frame(interaction.depth = 8,
                                              n.trees = 100,
                                              shrinkage = .1,
                                              n.minobsinnode = 20))

both.predict.new<-predict(both.model.new,both.test.new[,both.predictors],type="raw")
confusionMatrix(both.predict.new,both.test.new[,both.outcome],positive = "1")

##############################################################
########## PRECISION, RECALL, F-SCORE, G-SCORE
##############################################################
#imbalanced
imb.pre <- predict(imbalanced.model, test[,predictorNames])
table.imb.pre <- table(data.frame(Prediction = imb.pre, Actual = test[,outcomeName]))
table.imb.pre
imb.accuracy <- (table.imb.pre[1,1]+table.imb.pre[2,2])/sum(table.imb.pre)
imb.accuracy
imb.precision <- (table.imb.pre[1,1])/sum(table.imb.pre[,1])
imb.precision
imb.recall <- table.imb.pre[1,1]/sum(table.imb.pre[1,])
imb.recall
imb.fscore <- 2*((imb.precision*imb.recall)/(imb.precision+imb.recall))
imb.fscore
imb.gscore <- sqrt(imb.precision*imb.recall)
imb.gscore
#imbalanced FS
imb.pre.new <- predict(imbalanced.model.new, imbalanced.test.new[,imbalanced.predictors])
table.imb.pre.new <- table(data.frame(Prediction = imb.pre.new, Actual = imbalanced.test.new[,imbalanced.outcome]))
table.imb.pre.new
imb.accuracy.new <- (table.imb.pre.new[1,1]+table.imb.pre.new[2,2])/sum(table.imb.pre.new)
imb.accuracy.new
imb.precision.new <- (table.imb.pre.new[1,1])/sum(table.imb.pre.new[,1])
imb.precision.new
imb.recall.new <- table.imb.pre.new[1,1]/sum(table.imb.pre.new[1,])
imb.recall.new
imb.fscore.new <- 2*((imb.precision.new*imb.recall.new)/(imb.precision.new+imb.recall.new))
imb.fscore.new
imb.gscore.new <- sqrt(imb.precision.new*imb.recall.new)
imb.gscore.new
#over
over.pre <- predict(over.model, test[,predictorNames])
table.over.pre <- table(data.frame(Prediction = over.pre, Actual = test[,outcomeName]))
table.over.pre
over.accuracy <- (table.over.pre[1,1]+table.over.pre[2,2])/sum(table.over.pre)
over.accuracy
over.precision <- (table.over.pre[1,1])/sum(table.over.pre[,1])
over.precision
over.recall <- table.over.pre[1,1]/sum(table.over.pre[1,])
over.recall
over.fscore <- 2*((over.precision*over.recall)/(over.precision+over.recall))
over.fscore
over.gscore <- sqrt(over.precision*over.recall)
over.gscore
#over FS
over.pre.new <- predict(over.model.new, over.test.new[,over.predictors])
table.over.pre.new <- table(data.frame(Prediction = over.pre.new, Actual = over.test.new[,over.outcome]))
table.over.pre.new
over.accuracy.new <- (table.over.pre.new[1,1]+table.over.pre.new[2,2])/sum(table.over.pre.new)
over.accuracy.new
over.precision.new <- (table.over.pre.new[1,1])/sum(table.over.pre.new[,1])
over.precision.new
over.recall.new <- table.over.pre.new[1,1]/sum(table.over.pre.new[1,])
over.recall.new
over.fscore.new <- 2*((over.precision.new*over.recall.new)/(over.precision.new+over.recall.new))
over.fscore.new
over.gscore.new <- sqrt(over.precision.new*over.recall.new)
over.gscore.new
#under
under.pre <- predict(under.model, test[,predictorNames])
table.under.pre <- table(data.frame(Prediction = under.pre, Actual = test[,outcomeName]))
table.under.pre
under.accuracy <- (table.under.pre[1,1]+table.under.pre[2,2])/sum(table.under.pre)
under.accuracy
under.precision <- (table.under.pre[1,1])/sum(table.under.pre[,1])
under.precision
under.recall <- table.under.pre[1,1]/sum(table.under.pre[1,])
under.recall
under.fscore <- 2*((under.precision*under.recall)/(under.precision+under.recall))
under.fscore
under.gscore <- sqrt(under.precision*under.recall)
under.gscore
#under FS
under.pre.new <- predict(under.model.new, under.test.new[,under.predictors])
table.under.pre.new <- table(data.frame(Prediction = under.pre.new, Actual = under.test.new[,under.outcome]))
table.under.pre.new
under.accuracy.new <- (table.under.pre.new[1,1]+table.under.pre.new[2,2])/sum(table.under.pre.new)
under.accuracy.new
under.precision.new <- (table.under.pre.new[1,1])/sum(table.under.pre.new[,1])
under.precision.new
under.recall.new <- table.under.pre.new[1,1]/sum(table.under.pre.new[1,])
under.recall.new
under.fscore.new <- 2*((under.precision.new*under.recall.new)/(under.precision.new+under.recall.new))
under.fscore.new
under.gscore.new <- sqrt(under.precision.new*under.recall.new)
under.gscore.new
#both
both.pre <- predict(both.model, test[,predictorNames])
table.both.pre <- table(data.frame(Prediction = both.pre, Actual = test[,outcomeName]))
table.both.pre
both.accuracy <- (table.both.pre[1,1]+table.both.pre[2,2])/sum(table.both.pre)
both.accuracy
both.precision <- (table.both.pre[1,1])/sum(table.both.pre[,1])
both.precision
both.recall <- table.both.pre[1,1]/sum(table.both.pre[1,])
both.recall
both.fscore <- 2*((both.precision*both.recall)/(both.precision+both.recall))
both.fscore
both.gscore <- sqrt(both.precision*both.recall)
both.gscore
#both FS
both.pre.new <- predict(both.model.new, both.test.new[,both.predictors])
table.both.pre.new <- table(data.frame(Prediction = both.pre.new, Actual = both.test.new[,both.outcome]))
table.both.pre.new
both.accuracy.new <- (table.both.pre.new[1,1]+table.both.pre.new[2,2])/sum(table.both.pre.new)
both.accuracy.new
both.precision.new <- (table.both.pre.new[1,1])/sum(table.both.pre.new[,1])
both.precision.new
both.recall.new <- table.both.pre.new[1,1]/sum(table.both.pre.new[1,])
both.recall.new
both.fscore.new <- 2*((both.precision.new*both.recall.new)/(both.precision.new+both.recall.new))
both.fscore.new
both.gscore.new <- sqrt(both.precision.new*both.recall.new)
both.gscore.new

##############################################################
########## AUC
##############################################################
library(pROC)
#imbalanced
imbalanced.probs <- predict(imbalanced.model,test[,predictorNames],type="prob")
imbalanced.roc<-roc(test$TARGET, imbalanced.probs[, 2]) 
plot(imbalanced.roc)
auc(imbalanced.roc)
imbalanced.new.probs <- predict(imbalanced.model.new,imbalanced.test.new[,imbalanced.predictors],type="prob")   
imbalanced.roc.new<-roc(test$TARGET, imbalanced.probs[, 2]) 
plot(imbalanced.roc.new)
auc(imbalanced.roc.new)
#under
under.probs <- predict(under.model,test[,predictorNames],type="prob")   
under.roc<-roc(test$TARGET, under.probs[, 2]) 
plot(under.roc)
auc(under.roc)
under.new.probs <- predict(under.model.new,under.test.new[,under.predictors],type="prob")   
under.roc.new<-roc(test$TARGET, under.probs[, 2]) 
plot(under.roc.new)
auc(under.roc.new)
#over
over.probs <- predict(over.model,test[,predictorNames],type="prob")   
over.roc<-roc(test$TARGET, over.probs[, 2]) 
plot(over.roc)
auc(over.roc)
over.new.probs <- predict(over.model.new,over.test.new[,over.predictors],type="prob")   
over.roc.new<-roc(test$TARGET, over.probs[, 2]) 
plot(over.roc.new)
auc(over.roc.new)
#both
both.probs <- predict(both.model,test[,predictorNames],type="prob")   
both.roc<-roc(test$TARGET, both.probs[, 2]) 
plot(both.roc)
auc(both.roc)
both.new.probs <- predict(both.model.new,both.test.new[,both.predictors],type="prob")   
both.roc.new<-roc(test$TARGET, both.probs[, 2]) 
plot(both.roc.new)
auc(both.roc.new)


##############################################################
########## TUNING
##############################################################
#IMBALANCED
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  ## repeated ten times
  repeats = 3)

imbalanced.tuned <- train(TARGET ~ ., data = imbalanced.train.new,
                          method = "gbm", 
                          trControl = fitControl, 
                          verbose = FALSE, 
                          tuneGrid = data.frame(interaction.depth = 8,
                                                n.trees = 100,
                                                shrinkage = .1,
                                                n.minobsinnode = 20))

imbalanced.tuned.predict<-predict(imbalanced.tuned,imbalanced.test.new[,imbalanced.predictors],type="raw")
confusionMatrix(imbalanced.tuned.predict,imbalanced.test.new[,imbalanced.outcome],positive = "1")

##UNDER SAMPLING
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  ## repeated ten times
  repeats = 3)

under.tuned <- train(TARGET ~ ., data = under.train.new,
                     method = "gbm", 
                     trControl = fitControl, 
                     verbose = FALSE, 
                     tuneGrid = data.frame(interaction.depth = 8,
                                           n.trees = 100,
                                           shrinkage = .1,
                                           n.minobsinnode = 20))

under.tuned.predict<-predict(under.tuned,under.test.new[,under.predictors],type="raw")
confusionMatrix(under.tuned.predict,under.test.new[,under.outcome],positive = "1")

#OVER SAMPLING
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  ## repeated ten times
  repeats = 3)

over.tuned <- train(TARGET ~ ., data = over.train.new,
                    method = "gbm", 
                    trControl = fitControl, 
                    verbose = FALSE, 
                    tuneGrid = data.frame(interaction.depth = 8,
                                          n.trees = 100,
                                          shrinkage = .1,
                                          n.minobsinnode = 20))

over.tuned.predict<-predict(over.tuned,over.test.new[,over.predictors],type="raw")
confusionMatrix(over.tuned.predict,over.test.new[,over.outcome],positive = "1")

#BOTH SAMPLING
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  ## repeated ten times
  repeats = 3)

both.tuned <- train(TARGET ~ ., data = both.train.new,
                    method = "gbm", 
                    trControl = fitControl, 
                    verbose = FALSE, 
                    tuneGrid = data.frame(interaction.depth = 8,
                                          n.trees = 100,
                                          shrinkage = .1,
                                          n.minobsinnode = 20))

both.tuned.predict<-predict(both.tuned,both.test.new[,both.predictors],type="raw")
confusionMatrix(both.tuned.predict,both.test.new[,both.outcome],positive = "1")

