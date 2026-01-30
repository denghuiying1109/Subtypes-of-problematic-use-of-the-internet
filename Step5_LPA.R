################################## Latent Profile Analysis ###############################
library(dplyr)
library(tidyverse)
library(tidyLPA)
data<-read.csv("/Analysis data/CFA_factor_score.csv")
PUI_data <- subset(data, treat == 1)

mydata<-dplyr::select(PUI_data, Reward, Negative_emotion, Executive_function)
mydata <- scale(mydata)

cluster <- estimate_profiles(mydata,n_profiles=1:6,models=5,package='MplusAutomation')
cluster

prob_data <- get_data(cluster[[3]], what = "posterior_probabilities")

##########################################################################################
head(prob_data)

library(dplyr)

average_probs <- prob_data %>%
  mutate(
    assigned_prob = case_when(
      Class == 1 ~ CPROB1,
      Class == 2 ~ CPROB2,
      Class == 3 ~ CPROB3
    )
  ) %>%
  group_by(Class) %>%
  summarise(
    n = n(),
    mean_posterior_prob = mean(assigned_prob),
    min_prob = min(assigned_prob)
  )

average_probs

ambiguous_cases <- prob_data %>%
  mutate(
    assigned_prob = case_when(
      Class == 1 ~ CPROB1,
      Class == 2 ~ CPROB2,
      Class == 3 ~ CPROB3
    )
  ) %>%
  filter(assigned_prob < 0.70)

nrow(ambiguous_cases)

###########################################################################################
str(cluster)
str(cluster[[1]]$fit)
loglik_values <- sapply(cluster, function(model) model$fit["LogLik"])
loglik_values

lpa_result <- get_data(cluster[[3]])
LPA_data <- LPA_data %>%
  mutate(Group_LPA = lpa_result$Class)

write.csv(LPA_data, "/Analysis data/LPA_CFA.csv")
