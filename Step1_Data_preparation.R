
library(reshape)
library(dplyr)

###############################Demographic information#####################################

Basic_Demos<-read.csv("/Raw data/Demo/Basic_Demos.csv")
Basic_Demos<-dplyr::select(Basic_Demos,EID,Release_Number,Age,Sex,Study_Site)
summary(Basic_Demos)
write.csv(Basic_Demos, file = "/Processed data/Basic_Demos.csv")

#...............................................................................

PreInt_Demos_Fam<-read.csv("D:/Raw data/Demo/PreInt_Demos_Fam.csv")
PreInt_Demos_Fam<-dplyr::select(PreInt_Demos_Fam,EID,Child_Ethnicity,Child_Race,Child_Race_Other)
write.csv(PreInt_Demos_Fam, file = "/Processed data/PreInt_Demos_Fam.csv")

#.........................................................................................

EHQ<-read.csv("/Raw data/Demo/EHQ.csv")
EHQ$Handedness <- ifelse(EHQ$EHQ_Total < -28, "left",
               ifelse(EHQ$EHQ_Total >= -28 & EHQ$EHQ_Total < 48, "Middle",
                      ifelse(EHQ$EHQ_Total >= 48, "right", NA)))


Handedness<-dplyr::select(EHQ,EID,EHQ_Total,Handedness)
write.csv(Handedness, file = "/Processed data/Handedness.csv")

#annual_household_income
FSQ<-read.csv("/Raw data/Demo/FSQ.csv")
FSQ<-dplyr::select(FSQ,EID,FSQ_04,FSQ_08)
FSQ <- dplyr::rename(FSQ, annual_household_income= FSQ_04,household_size=FSQ_08)
write.csv(FSQ, file = "/Processed data/FSQ.csv")

#Barratt

Barratt<-read.csv("/Raw data/Demo/Barratt.csv")
Barratt<-dplyr::select(Barratt,EID,Barratt_Total,Barratt_Total_Edu,Barratt_Total_Occ)
write.csv(Barratt, file = "/Processed data/Barratt.csv")


#Diagnosis_ClinicianConsensus

Diagnosis_ClinicianConsensus<-read.csv("/Raw data/Demo/Diagnosis_ClinicianConsensus.csv")
Diagnosis_ClinicianConsensus<-dplyr::select(Diagnosis_ClinicianConsensus,
                                            EID,
                                            NoDX,
                                            DX_01,DX_01_Time,
                                            DX_02,DX_02_Time,
                                            DX_03,DX_03_Time,
                                            DX_04,DX_04_Time,
                                            DX_05,DX_05_Time,
                                            DX_06,DX_06_Time,
                                            DX_07,DX_07_Time,
                                            DX_08,DX_08_Time,
                                            DX_09,DX_09_Time,
                                            DX_10,DX_10_Time
                                         )

#Replace blanck with "NA"
replace_blank <- function(x) {
  x[x == ""] <- NA
  return(x)
}

Diagnosis_ClinicianConsensus <- data.frame(lapply(Diagnosis_ClinicianConsensus, replace_blank))


#Calculate the number of diagnosis

attach(Diagnosis_ClinicianConsensus)
Diagnosis_ClinicianConsensus$No_01[DX_01=="No Diagnosis Given"] <- 0
Diagnosis_ClinicianConsensus$No_01[NoDX==2] <- 1
Diagnosis_ClinicianConsensus$No_01[NoDX==3] <- NA
detach(Diagnosis_ClinicianConsensus)


Diagnosis_ClinicianConsensus$No_02 <- ifelse(is.na(Diagnosis_ClinicianConsensus$DX_02), 0, 1)
Diagnosis_ClinicianConsensus$No_03 <- ifelse(is.na(Diagnosis_ClinicianConsensus$DX_03), 0, 1)
Diagnosis_ClinicianConsensus$No_04 <- ifelse(is.na(Diagnosis_ClinicianConsensus$DX_04), 0, 1)
Diagnosis_ClinicianConsensus$No_05 <- ifelse(is.na(Diagnosis_ClinicianConsensus$DX_05), 0, 1)
Diagnosis_ClinicianConsensus$No_06 <- ifelse(is.na(Diagnosis_ClinicianConsensus$DX_06), 0, 1)
Diagnosis_ClinicianConsensus$No_07 <- ifelse(is.na(Diagnosis_ClinicianConsensus$DX_07), 0, 1)
Diagnosis_ClinicianConsensus$No_08 <- ifelse(is.na(Diagnosis_ClinicianConsensus$DX_08), 0, 1)
Diagnosis_ClinicianConsensus$No_09 <- ifelse(is.na(Diagnosis_ClinicianConsensus$DX_09), 0, 1)
Diagnosis_ClinicianConsensus$No_10 <- ifelse(is.na(Diagnosis_ClinicianConsensus$DX_10), 0, 1)



Diagnosis_ClinicianConsensus$Total_diagnosis <- apply(Diagnosis_ClinicianConsensus[, 23:32], 1, function(x) if(any(is.na(x))) NA else sum(x, na.rm = TRUE))

write.csv(Diagnosis_ClinicianConsensus, file = "/Processed data/Diagnosis_ClinicianConsensus.csv")


#..................Merge demo files.....................................

Basic_Demos<-read.csv("/Processed data/Basic_Demos.csv")
PreInt_Demos_Fam<-read.csv("/Processed data/PreInt_Demos_Fam.csv")
Handedness<-read.csv("/Processed data/Handedness.csv")
FSQ<-read.csv("/Processed data/FSQ.csv")
Barratt<-read.csv("/Processed data//Barratt.csv")
Diagnosis<-read.csv("/Processed data/Diagnosis_ClinicianConsensus.csv")


Demo<-merge(Basic_Demos,PreInt_Demos_Fam,by=c("EID"), all.x=TRUE)
Demo<-merge(Demo,BMI,by=c("EID"), all.x=TRUE)
Demo<-merge(Demo,Handedness,by=c("EID"), all.x=TRUE)
Demo<-merge(Demo,FSQ,by=c("EID"), all.x=TRUE)
Demo<-merge(Demo,Barratt,by=c("EID"), all.x=TRUE)
Demo<-merge(Demo,Diagnosis,by=c("EID"), all.x=TRUE)


write.csv(Demo, file = "/Processed data/Merged_Demo.csv")


########################## Internet Addiction Test #############################

IAT<-read.csv("/Raw data/IAT.csv")
IAT<-dplyr::select(IAT,EID,  
                      IAT_01,IAT_02,IAT_03,IAT_04,IAT_05,
                      IAT_06,IAT_07,IAT_08,IAT_09,IAT_10,
                      IAT_11,IAT_12,IAT_13,IAT_14,IAT_15,
                      IAT_16,IAT_17,IAT_18,IAT_19,IAT_20)

write.csv(IAT, file = "/Processed data/IAT.csv")

########################## Reward processing #############################
#Delay Discounting Task

DDT<-read.csv("/Raw data/Reward/TEMP_DISC.csv")

DDT<-dplyr::select(DDT,EID,
                   Temp_Disc_run1_ed50,Temp_Disc_run1_k,
                   Temp_Disc_run2_ed50,Temp_Disc_run2_k,
                   Temp_Disc_run3_ed50,Temp_Disc_run3_k,
                   Temp_Disc_run4_ed50,Temp_Disc_run4_k,
                   Temp_Disc_run5_ed50,Temp_Disc_run5_k,
                   Temp_Disc_run6_ed50,Temp_Disc_run6_k
                   )


write.csv(DDT, file = "/Processed data/Merged_Reward.csv")

########################## Executive Function #############################

NIH_Scores<-read.csv("/Raw data/Executive function/NIH_Scores.csv")

NIH_Scores<-dplyr::select(NIH_Scores, EID,
                          NIH7_Card,NIH7_Flanker,
                          NIH7_List, NIH7_Pattern)

write.csv(NIH_Scores, file = "/Processed data/NIH_Scores.csv")


#Wechsler Intelligence Scale for Children-V (WISC-V) 

WISC<-read.csv("/Raw data/Executive function/WISC.csv")

#WISC<-dplyr::select(WISC,EID,WISC_FSIQ_Sum,WISC_FSIQ)

#colnames(WISC) <- c("EID", "FSIQ_Sum","FSIQ_Com")     

WISC<-dplyr::select(WISC,EID,
                    WISC_VCI_Sum,WISC_VCI,
                    WISC_WMI_Sum,WISC_WMI,
                    WISC_PSI_Sum,WISC_PSI,
                    WISC_FSIQ_Sum,WISC_FSIQ,
                    )


colnames(WISC) <- c("EID", 
                    "VCI_SCALE","VCI_COMP",
                    "WMI_SCALE","WMI_COMP",
                    "PSI_SCALE","PSI_COMP",
                    "FSIQ_Sum","FSIQ_Com") 

#Wechsler Adult Intelligence Scale-IV (WAIS-IV)

WAIS<-read.csv("/Raw data/Executive function/WAIS.csv")

WAIS<-dplyr::select(WAIS, EID,
                    WAIS_VCI_SCALE,WAIS_VCI_COMP,
                    WAIS_WMI_SCALE,WAIS_WMI_COMP,
                    WAIS_PSI_SCALE,WAIS_PSI_COMP,
                    WAIS_FSIQ_SCALE,WAIS_FSIQ_COMP)

colnames(WAIS) <- c("EID", 
                    "VCI_SCALE","VCI_COMP",
                    "WMI_SCALE","WMI_COMP",
                    "PSI_SCALE","PSI_COMP",
                    "FSIQ_Sum","FSIQ_Com") 

IQ<-rbind(WISC,WAIS) 
write.csv(IQ, file = "/Processed data/IQ.csv")

#.................................Merge files................................

ID<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/ID.csv")
NIH_Scores<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/NIH_Scores.csv")
IQ<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/IQ.csv")
WIAT<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/WIAT.csv")

Executive_function<-merge(ID,NIH_Scores,by=c("EID"), all.x=TRUE)
Executive_function<-merge(Executive_function,IQ,by=c("EID"), all.x=TRUE)
Executive_function<-merge(Executive_function,WIAT,by=c("EID"), all.x=TRUE)
write.csv(Executive_function, file = "D:/个人文件夹/HBN_PUI_Subtype/Processed data/Merged_Executive_function.csv")



############################### Negative affect ###############################

#.......The Positive and Negative Affect Schedule....................
PANAS<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Raw data/Negative emotion/PANAS.csv")

PANAS<-dplyr::select(PANAS,EID,PANAS_PositiveAffect,PANAS_NegativeAffect)
write.csv(PANAS, file = "D:/个人文件夹/HBN_PUI_Subtype/Processed data/PANAS.csv")

#..............................Mood & Feelings Questionnaire.................
MFQ_SR<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Raw data/Negative emotion/MFQ_SR.csv")

MFQ_SR<-dplyr::select(MFQ_SR, EID, MFQ_SR_Total)

write.csv(MFQ_SR, file = "D:/个人文件夹/HBN_PUI_Subtype/Processed data/MFQ_SR.csv")


#....................CBCL..............................

CBCL<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Raw data/Negative emotion/CBCL.csv")


CBCL<-dplyr::select(CBCL,EID,
                    CBCL_AD,CBCL_AD_T,
                    CBCL_WD,CBCL_WD_T,
                    CBCL_SC,CBCL_SC_T,
                    CBCL_SP,CBCL_SP_T,
                    #CBCL_TP,CBCL_TP_T,
                    CBCL_AP,CBCL_AP_T,
                    #CBCL_RBB,CBCL_RBB_T,
                    CBCL_AB,CBCL_AB_T, 
                    CBCL_Int,CBCL_Int_T,
                    CBCL_Ext,CBCL_Ext_T,
                    CBCL_Total,CBCL_Total_T
                    )


CBCL_Pre<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Raw data/Negative emotion/CBCL_Pre.csv")

CBCL_Pre<-dplyr::select(CBCL_Pre,EID,
                        CBCLPre_AD,CBCLPre_AD_T,
                        CBCLPre_WD,CBCLPre_WD_T,
                        CBCLPre_SC,CBCLPre_SC_T,
                        CBCLPre_SP,CBCLPre_SP_T,
                        CBCLPre_AP,CBCLPre_AP_T,
                        CBCLPre_AB,CBCLPre_AB_T,
                        CBCLPre_Int,CBCLPre_Int_T,
                        CBCLPre_Ext,CBCLPre_Ext_T,
                        CBCLPre_Total,CBCLPre_Total_T
)

colnames(CBCL_Pre) <- c("EID", 
                    "CBCL_AD","CBCL_AD_T",
                    "CBCL_WD","CBCL_WD_T",
                    "CBCL_SC","CBCL_SC_T",
                    "CBCL_SP","CBCL_SP_T",
                    "CBCL_AP","CBCL_AP_T",
                    "CBCL_AB","CBCL_AB_T", 
                    "CBCL_Int","CBCL_Int_T",
                    "CBCL_Ext","CBCL_Ext_T",
                    "CBCL_Total","CBCL_Total_T") 


write.csv(CBCL_com, file = "D:/个人文件夹/HBN_PUI_Subtype/Processed data/CBCL_com.csv")


#............................SDQ......................................
SDQ<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Raw data/Negative emotion/SDQ.csv")

SDQ<-dplyr::select(SDQ, EID, 
                   SDQ_Conduct_Problems,SDQ_Difficulties_Total,
                   SDQ_Emotional_Problems,SDQ_Externalizing,
                   SDQ_Generating_Impact,SDQ_Hyperactivity,
                   SDQ_Internalizing,SDQ_Peer_Problems,SDQ_Prosocial)

write.csv(SDQ, file = "D:/个人文件夹/HBN_PUI_Subtype/Processed data/SDQ.csv")


#.................Merge files.................................

ID<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/ID.csv")
PANAS<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/PANAS.csv")
MFQ_SR<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/MFQ_SR.csv")
ARI_SR<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/ARI_SR.csv")
CBCL_com<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/CBCL_com.csv")
SDQ<-read.csv("D:/个人文件夹/HBN_PUI_Subtype/Processed data/SDQ.csv")


Negative_emotion<-merge(ID,CBCL_com,by=c("EID"), all.x=TRUE)
Negative_emotion<-merge(Negative_emotion,ARI_SR,by=c("EID"), all.x=TRUE)
Negative_emotion<-merge(Negative_emotion,MFQ_SR,by=c("EID"), all.x=TRUE)
Negative_emotion<-merge(Negative_emotion,PANAS,by=c("EID"), all.x=TRUE)
Negative_emotion<-merge(Negative_emotion,SDQ,by=c("EID"), all.x=TRUE)



write.csv(Negative_emotion, file = "D:/个人文件夹/HBN_PUI_Subtype/Processed data/Merged_Negative_emotion.csv")

