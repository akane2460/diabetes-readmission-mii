## Load Packages ----
library(tidyverse)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)
library(pROC)

# load cleaned data
load(here("data/diabetic_mii.rda"))

# inspect data
skim_without_charts(diabetic_mii)

# distribution of risk/instability indices----
## single med
fivenum(diabetic_mii$single_med_risk_score)
diabetic_mii |> 
  ggplot(aes(x = single_med_risk_score)) +
  geom_boxplot()


## double med
fivenum(diabetic_mii$double_med_risk_score)
diabetic_mii |> 
  ggplot(aes(x = double_med_risk_score)) +
  geom_boxplot()


## mii
fivenum(diabetic_mii$medication_instability_index)
diabetic_mii |> 
  ggplot(aes(x = medication_instability_index)) +
  geom_boxplot()


# readmit across MII---
diabetic_mii |> 
  group_by(readmitted) |> 
  summarize(
    mean_mii = mean(medication_instability_index),
    mean_double_risk = mean(double_med_risk_score),
    mean_single_risk = mean(single_med_risk_score),
    n = n(),
    .groups = "drop"
  )

diabetic_mii |> 
  mutate(
    mii_exists = case_when(
      medication_instability_index != 0 ~ "Flagged",
      medication_instability_index == 0 ~ "Not Flagged",
    )) |> 
  group_by(mii_exists) |> 
  summarize(
    readmit_rate = mean(readmitted == "YES", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

diabetic_mii |> 
  mutate(
    mii_exists = case_when(
      medication_instability_index != 0 ~ "Flagged",
      medication_instability_index == 0 ~ "Not Flagged",
    )) |> 
  group_by(mii_exists, readmitted) |> 
  summarize(
    n = n(),
    .groups = "drop"
  )

## testing mii univariate accuracy-----
mii_model <- glm(
  readmitted ~ medication_instability_index,
  data = diabetic_mii,
  family = binomial(link = "logit")
)

diabetic_mii$pred_prob <- predict(mii_model, type = "response")

# diabetic_mii and readmission
roc_mii <- diabetic_mii |>
  roc(diabetic_mii$readmitted, diabetic_mii$pred_prob, levels = c("NO", "YES"))


auc(roc_mii)
  # does not discriminate well between readmitted and nonreadmitted


# multi-variate mii accuracy-----
mii_model_full <- glm(
  readmitted ~ medication_instability_index + 
    age + 
    number_diagnoses + 
    time_in_hospital +
    num_medications +
    number_inpatient,
  data = diabetic_mii,
  family = binomial(link = "logit")
)

summary(mii_model_full)

roc_full <- roc(
  diabetic_mii$readmitted,
  predict(mii_model_full, type = "response"),
  levels = c("NO", "YES")
)
auc(roc_full)

# controlling for patient age, case complexity and number of previous visits, higher
# mii independently predicts higher readmission

