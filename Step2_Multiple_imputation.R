#####################Multiple Imputation################################
library(dplyr)

Merged_Demo<-read.csv("/Processed data/Merged_Demo.csv")
IAT<-read.csv("/Processed data/IAT.csv")

Merged_Reward<-read.csv("/Processed data/Merged_Reward.csv")
Merged_Negative_emotion<-read.csv("/Processed data/Merged_Negative_emotion.csv")
Merged_Executive_function<-read.csv("/Processed data/Merged_Executive_function.csv")


Demo<-dplyr::select(Merged_Demo,EID, 
                    Age, Sex, Study_Site,
                    Handedness,
                    Child_Ethnicity,Child_Race,
                    annual_household_income,
                    Barratt_Total,
                    Total_diagnosis)

           
IAT<-dplyr::select(IAT,EID,  
                   IAT_01,IAT_02,IAT_03,IAT_04,IAT_05,
                   IAT_06,IAT_07,IAT_08,IAT_09,IAT_10,
                   IAT_11,IAT_12,IAT_13,IAT_14,IAT_15,
                   IAT_16,IAT_17,IAT_18,IAT_19,IAT_20)


Reward<-dplyr::select(Merged_Reward,EID, 
                      Temp_Disc_run1_k,Temp_Disc_run2_k,
                      Temp_Disc_run3_k,Temp_Disc_run4_k,
                      Temp_Disc_run5_k,Temp_Disc_run6_k)

Negative_emotion<-dplyr::select(Merged_Negative_emotion,EID, 
                                CBCL_AD_T,CBCL_WD_T,
                                MFQ_SR_Total,
                                PANAS_NegativeAffect,
                                SDQ_Emotional_Problems)


Executive_function<-dplyr::select(Merged_Executive_function,EID, 
                                  NIH7_Card,NIH7_Flanker,
                                  NIH7_List,
                                  WMI_COMP,
                                  PSI_COMP)



Data_imp<-merge(IAT,Demo,by=c("EID"), all.x=TRUE) 
Data_imp<-merge(Data_imp,Reward,by=c("EID"), all.x=TRUE)                                  
Data_imp<-merge(Data_imp,Negative_emotion,by=c("EID"), all.x=TRUE)
Data_imp<-merge(Data_imp,Executive_function,by=c("EID"), all.x=TRUE)


Data_imp$Sex <- factor(Data_imp$Sex, 
                           levels = c(0, 1), 
                           labels = c("Male", "Female"))


Data_imp$Child_Ethnicity[Data_imp$Child_Ethnicity == 3] <- 2


Data_imp$Child_Ethnicity <- factor(Data_imp$Child_Ethnicity, 
                                       levels = c(0, 1, 2), 
                                       labels = c("Not Hispanic or Latino", "Hispanic or Latino","Other"))


Data_imp$Child_Race[Data_imp$Child_Race %in% c(4, 5, 6, 7, 8, 9, 10, 11)] <- 2

Data_imp$Child_Race <- factor(Data_imp$Child_Race, 
                                  levels = c(0, 1, 2, 3), 
                                  labels = c("White", "Black","Mixed/Other","Asian"
                                  ))




##########################################################################################

library(mice)

Data_imp$Handedness <- as.factor(Data_imp$Handedness)
Data_imp$Child_Ethnicity <- as.factor(Data_imp$Child_Ethnicity)
Data_imp$Child_Race <- as.factor(Data_imp$Child_Race)


methods <- make.method(Data_imp)
methods[c("Handedness", "Child_Ethnicity", "Child_Race")] <- "polyreg" 

Data_imped<- mice(Data_imp,m=100,method=methods,seed = 123)                         


summary(Data_imped)
plot(Data_imped_IAT)
densityplot(Data_imped_IAT) 

long_format_data <- complete(Data_imped, action = "long", include = TRUE)

long_format_data <- long_format_data %>%
  filter(.imp != 0)


mode_func <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


category_vars <- c("Handedness","Child_Ethnicity","Child_Race") 


																			
continuous_vars <- c("IAT_01", "IAT_02","IAT_03","IAT_04","IAT_05",
                     "IAT_06","IAT_07","IAT_08","IAT_09","IAT_10",
                     "IAT_11","IAT_12","IAT_13","IAT_14","IAT_15",
                     "IAT_16","IAT_17","IAT_18","IAT_19","IAT_20",
                     "Age","annual_household_income",
                     "Barratt_Total",
                     "Temp_Disc_run1_k","Temp_Disc_run2_k",
                     "Temp_Disc_run3_k","Temp_Disc_run4_k",
                     "Temp_Disc_run5_k","Temp_Disc_run6_k",
                     "CBCL_AD_T","CBCL_WD_T","MFQ_SR_Total",
                     "PANAS_NegativeAffect","SDQ_Emotional_Problems",
                     "NIH7_Card","NIH7_Flanker","NIH7_List","WMI_COMP","PSI_COMP")



Data_imped_com <- long_format_data %>%
  group_by(EID) %>%
  summarise(across(all_of(category_vars), ~ mode_func(.), .names = "{.col}_mode"),
            across(all_of(continuous_vars), ~ median(.), .names = "{.col}_median"))




write.csv(Data_imped_com, file = "/Analysis data/Data_imped_com.csv")
