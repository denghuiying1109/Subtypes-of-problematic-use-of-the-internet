################################ Propensity Score Matching #####################################
library(dplyr)

Data_imped_com<-read.csv("/Analysis data/Data_imped_com.csv")

Data_imped_com<-mutate(Data_imped_com, IAT_total = rowSums(Data_imped_com[, 6:25]))

Data_imped_com <- subset(Data_imped_com, !(IAT_total > 30 & IAT_total < 50))

Data_imped_com <- Data_imped_com %>%
  mutate(treat = ifelse(IAT_total >= 50, 1, 0))

Data_imped_PSM <- Data_imped_com %>%
  filter(!(IAT_total <= 30 & Total_diagnosis > 0))

#.......................................................................................................................

Data_PSM_cov<-select(Data_imped_PSM, Age, Sex, Handedness, Child_Ethnicity, Child_Race, annual_household_income, Barratt_Total,treat)

#Preliminary	Analysis
library("RItools")
xBalance(treat	~ Age + Sex + Handedness + Child_Ethnicity + Child_Race + annual_household_income + Barratt_Total,data=Data_PSM_cov, report	= c("chisquare.test"))

#Calculates	the	propensity	score
ps	<- glm(treat	~ Age + Sex + Handedness + Child_Ethnicity + Child_Race + annual_household_income + Barratt_Total,data=Data_PSM_cov, family	= binomial())
summary(ps)

#Attach	the	predicted	propensity	score	to	the	datafile
Data_PSM_cov$psvalue	<- predict(ps,	type	= "response")

#install.packages("Hmisc")
library(Hmisc)
histbackback(split(Data_PSM_cov$psvalue,	Data_PSM_cov$treat),	main= "Propensity score before matching",	xlab=c("HC",	"PUI"))

########################################################################################################################################

#Match using	near-neighbor

library(MatchIt)
library(cobalt)
set.seed(123)


m2 <- matchit(treat ~ Age + Sex + Handedness + Child_Ethnicity + Child_Race + annual_household_income + Barratt_Total,
              data = Data_imped_PSM,method = "nearest",caliper = 0.4)


bal.tab(m2, stats = c("m", "ks"), binary = "std")
summary(m2)

plot(m2,type	= "jitter")

love.plot(m2, stats = c("m", "ks"), binary = "std",
          drop.distance = TRUE, abs = TRUE)

##################################################################################################################################
md2 <- match.data(m2)

names(md2)

md2_cov<-select(md2, Age, Sex, Handedness, Child_Ethnicity, Child_Race, annual_household_income, Barratt_Total,treat)

xBalance(treat	~ Age + Sex + Handedness + Child_Ethnicity + Child_Race + annual_household_income + Barratt_Total,data=md2_cov, report	= c("chisquare.test"))

ps_afer	<- glm(treat	~ Age + Sex + Handedness + Child_Ethnicity + Child_Race + annual_household_income + Barratt_Total,data=md2_cov, family	= binomial())

md2_cov$psvalue	<- predict(ps_afer,	type	= "response")

histbackback(split(md2_cov$psvalue,	md2_cov$treat),	main= "Propensity score after matching",	xlab=c("HC",	"PIU"))


#Examining distributional balance with plots:
bal.plot(m2, var.name = "Age")
bal.plot(m2, var.name = "Sex")
bal.plot(m2, var.name = "Handedness")
bal.plot(m2, var.name = "Child_Ethnicity")
bal.plot(m2, var.name = "Child_Race")
bal.plot(m2, var.name = "annual_household_income")
bal.plot(m2, var.name = "Barratt_Total")


write.csv(md2,file = "/Analysis data/PSM_sample.csv")
