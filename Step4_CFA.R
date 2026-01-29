########################################  confirmatory factor analysis ###########################################
library(lavaan)
library(lavaanPlot)
library(kutils)
library(semPlot)

CFA_data <- read.csv( "/Analysis data/PSM_sample.csv")

#Transform K function in DDT
CFA_data$run1_ed50 <- log10(1/CFA_data$Temp_Disc_run1_k)
CFA_data$run2_ed50 <- log10(1/CFA_data$Temp_Disc_run2_k)
CFA_data$run3_ed50 <- log10(1/CFA_data$Temp_Disc_run3_k)
CFA_data$run4_ed50 <- log10(1/CFA_data$Temp_Disc_run4_k)
CFA_data$run5_ed50 <- log10(1/CFA_data$Temp_Disc_run5_k)
CFA_data$run6_ed50 <- log10(1/CFA_data$Temp_Disc_run6_k)

#Correlation analysis

colnames(CFA_data)
corr_data<-dplyr::select(CFA_data,
                         run1_ed50, run2_ed50, run3_ed50,
                         run4_ed50, run5_ed50, run6_ed50,
                         CBCL_AD_T, CBCL_WD_T, MFQ_SR_Total, 
                         SDQ_Emotional_Problems, PANAS_NegativeAffect,
                         NIH7_Card, NIH7_Flanker, NIH7_List, WMI_COMP, PSI_COMP
                         )


#Define the CFA model
CFA_model2 <- 'Reward =~ run1_ed50 + run2_ed50+ run3_ed50 + run4_ed50 + run5_ed50 + run6_ed50
               Negative_emotion =~ CBCL_AD_T	+ CBCL_WD_T + MFQ_SR_Total + SDQ_Emotional_Problems + PANAS_NegativeAffect
               Executive_function =~ NIH7_Card + NIH7_Flanker + NIH7_List + WMI_COMP + PSI_COMP'

fit <- cfa(CFA_model, data = CFA_data,estimator = "MLM", std.lv = TRUE,mimic = "Mplus")                                     
summary(fit, standardized=TRUE, ci=TRUE, fit.measures=TRUE, rsquare = TRUE) 

modindices(fit, minimum.value = 50, sort = TRUE)

fscores <- lavPredict(fit)
CFA_data <- cbind(CFA_data, fscores)


write.csv(CFA_data,"/Analysis data/CFA_factor_score.csv")
