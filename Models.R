########################################################################################
########################################################################################
########################## MODELS: MASLD AND OFFSPRING NDD #############################
########################################################################################
########################################################################################

# Date: 1.10.2026

# R version 4.3.1 (R Foundation for Statistical Computing, Vienna, Austria)

# Name of dataset: df


########################################################################################
## Model 1: Unadjusted Cox regression model: Start follow-up at birth (main analysis) ##
########################################################################################

coxunadjusted <- function(event, time2event, exposure){
  cox_unadjusted <- coxph(Surv(time2event, event) ~ as.factor(exposure)
                          
                          # Main analysis
                          + strata(matched_id),
                          
                          # # Cousin-controlled analysis
                          # + strata(LopNr_case),
                          
                          cluster = LopNr, # LopNr = mother ID (accounts for non-independence among offspring)
                          robust = TRUE,
                          data = df)
  
  dat <- summary(cox_unadjusted)
  
  HR  <- dat$conf.int[1, "exp(coef)"]
  LCL <- dat$conf.int[1, "lower .95"]
  UCL <- dat$conf.int[1, "upper .95"]
  
  table_unadj <- data.frame(
    `Unadjusted HR (95% CI) (model 1, fup=birth)` =
      sprintf("%.2f (%.2f-%.2f)", HR, LCL, UCL),
    check.names = FALSE
  )
  
  return(table_unadj)
}

# Outcomes
cox_table_unadjusted <- rbind("", "", "", "1 (Reference)", coxunadjusted(df$outc, df$time2outc, df$group_binary))



########################################################################################
## Model 1: Unadjusted Cox regression model: Start follow-up at 5 years (sensitivity) ##
########################################################################################

# Prepare new variables (follow-up at 5 years)
df$outc5 <- ifelse(df$outc == 1 & df$time2outc < 5*365.25, 0, df$outc) # Censor offspring diagnosed before age 5
df$time2outc5 <- pmax(0, df$time2outc - 5*365.25) # Create a delayed-entry follow-up time

coxunadjusted <- function(event, time2event, exposure){
  cox_unadjusted <- coxph(Surv(time2event, event) ~ as.factor(exposure)
                          
                          # Main analysis
                          + strata(matched_id),
                          
                          # # Cousin-controlled analysis
                          # + strata(LopNr_case),
                          
                          cluster = LopNr, # LopNr = mother ID (accounts for non-independence among offspring)
                          robust = TRUE,
                          data = df)
  
  dat <- summary(cox_unadjusted) # outputs coefficients of unadjusted Cox model and HR's with confidence intervals
  
  HR  <- dat$conf.int[1, "exp(coef)"]
  LCL <- dat$conf.int[1, "lower .95"]
  UCL <- dat$conf.int[1, "upper .95"]
  
  table_unadj <- data.frame(
    `Unadjusted HR (95% CI) (model 1, fup=5yrs)` =
      sprintf("%.2f (%.2f-%.2f)", HR, LCL, UCL),
    check.names = FALSE
  )
  
  return(table_unadj)
}

cox_table_unadjusted_2 <- rbind("", "", "", "1 (Reference)", coxunadjusted(df$outc5, df$time2outc5, df$group_binary))



#######################################################################################
## Model 2: Adjusted Cox regression model:  Start follow-up at birth (main analysis) ##
#######################################################################################

coxadjusted <- function(event, time2event, exposure, cov1, cov2, cov3, cov4, cov5, cov6, cov7, cov8) {
  cox_model <- coxph(Surv(time2event, event) ~ as.factor(exposure) + cov1 + cov2 + cov3 + cov4 + cov5 + cov6 + cov7 + cov8
                     
                     # Main analysis
                     + strata(matched_id),
                     
                     # # Cousin-controlled analysis
                     # + strata(LopNr_case),
                     
                     cluster = LopNr, # LopNr = mother ID (accounts for non-independence among offspring)
                     robust = TRUE,
                     data = df)
  
  dat <- summary(cox_model)
  
  HR  <- dat$conf.int[1, "exp(coef)"]
  LCL <- dat$conf.int[1, "lower .95"]
  UCL <- dat$conf.int[1, "upper .95"]
  
  table_cox <- data.frame(
    `Adjusted HR (95% CI) (model 2: fup=birth)` =
      sprintf("%.2f (%.2f-%.2f)", HR, LCL, UCL),
    check.names = FALSE
  )
  
  table_cox <- rbind("", "", "", "1 (Reference)", table_cox)
  
  return(table_cox)
}

cox_adjusted_1 <- coxadjusted(df$outc, df$time2outc, df$group_binary,
                              df$education, # cov1
                              df$country, # cov2
                              df$smoking, # cov3
                              df$bmi, # cov4
                              df$comorb_HYPERTENSION_dx_meds, # cov5
                              df$comorb_DYSLIPIDEMIA_dx_meds, # cov6
                              df$comorb_DIABETES_dx_meds, # cov7
                              df$comorb_ndd_psych # cov8
)


##################################################################################
## Model 2: Adjusted Cox regression model: Start follow-up at birth, BMI as NCS ##
##################################################################################

coxadjusted <- function(event, time2event, exposure, cov1, cov2, cov3, cov4, cov5, cov6, cov7, cov8) {
  cox_model <- coxph(Surv(time2event, event) ~ as.factor(exposure) + cov1 + cov2 + cov3 + ns(cov4, df = 4) + cov5 + cov6 + cov7 + cov8
                     
                     # Main analysis
                     + strata(matched_id),
                     
                     # # Cousin-controlled analysis
                     # + strata(LopNr_case),
                     
                     cluster = LopNr, # LopNr = mother ID (accounts for non-independence among offspring)
                     robust = TRUE,
                     data = df)
  
  dat <- summary(cox_model)
  
  HR  <- dat$conf.int[1, "exp(coef)"]
  LCL <- dat$conf.int[1, "lower .95"]
  UCL <- dat$conf.int[1, "upper .95"]
  
  table_cox <- data.frame(
    `Adjusted HR (95% CI) (model 2: fup=birth, BMI ns)` =
      sprintf("%.2f (%.2f-%.2f)", HR, LCL, UCL),
    check.names = FALSE
  )
  
  table_cox <- rbind("", "", "", "1 (Reference)", table_cox)
  
  return(table_cox)
  
}

cox_adjusted_2 <- coxadjusted(df$outc, df$time2outc, df$group_binary,
                              df$education, # cov1
                              df$country, # cov2
                              df$smoking, # cov3
                              df$bmi, # cov4
                              df$comorb_HYPERTENSION_dx_meds, # cov5
                              df$comorb_DYSLIPIDEMIA_dx_meds, # cov6
                              df$comorb_DIABETES_dx_meds, # cov7
                              df$comorb_ndd_psych # cov8
)



##############################################################################################
## Model 2: Adjusted Cox regression model model 2: Start follow-up at 5 years (sensitivity) ##
##############################################################################################

# Prepare new variables (follow-up at 5 years)
df$outc5 <- ifelse(df$outc == 1 & df$time2outc < 5*365.25, 0, df$outc) # Censor offspring diagnosed before age 5
df$time2outc5 <- pmax(0, df$time2outc - 5*365.25) # Create a delayed-entry follow-up time

coxadjusted <- function(event, time2event, exposure, cov1, cov2, cov3, cov4, cov5, cov6, cov7, cov8) {
  cox_model <- coxph(Surv(time2event, event) ~ as.factor(exposure) + cov1 + cov2 + cov3 + cov4 + cov5 + cov6 + cov7 + cov8
                     
                     # Main analysis
                     + strata(matched_id),
                     
                     # # Cousin-controlled analysis
                     # + strata(LopNr_case),
                     
                     cluster = LopNr, # LopNr = mother ID (accounts for non-independence among offspring)
                     robust = TRUE,
                     data = df)
  
  dat <- summary(cox_model)
  
  HR  <- dat$conf.int[1, "exp(coef)"]
  LCL <- dat$conf.int[1, "lower .95"]
  UCL <- dat$conf.int[1, "upper .95"]
  
  table_cox <- data.frame(
    `Adjusted HR (95% CI) (model 2: fup=5yrs)` =
      sprintf("%.2f (%.2f-%.2f)", HR, LCL, UCL),
    check.names = FALSE
  )
  
  table_cox <- rbind("", "", "", "1 (Reference)", table_cox)
  
  return(table_cox)
  
}

cox_adjusted_3 <- coxadjusted(df$outc5, df$time2outc5, df$group_binary,
                              df$education, # cov1
                              df$country, # cov2
                              df$smoking, # cov3
                              df$bmi, # cov4
                              df$comorb_HYPERTENSION_dx_meds, # cov5
                              df$comorb_DYSLIPIDEMIA_dx_meds, # cov6
                              df$comorb_DIABETES_dx_meds, # cov7
                              df$comorb_ndd_psych # cov8
)



########################################################################################################
## Model 2: Adjusted Cox regression model model 2: Start follow-up at 5 years + BMI NCS (sensitivity) ##
########################################################################################################

# Prepare new variables (follow-up at 5 years)
df$outc5 <- ifelse(df$outc == 1 & df$time2outc < 5*365.25, 0, df$outc) # Censor offspring diagnosed before age 5
df$time2outc5 <- pmax(0, df$time2outc - 5*365.25) # Create a delayed-entry follow-up time

coxadjusted <- function(event, time2event, exposure, cov1, cov2, cov3, cov4, cov5, cov6, cov7, cov8) {
  cox_model <- coxph(Surv(time2event, event) ~ as.factor(exposure) + cov1 + cov2 + cov3 + ns(cov4, df = 4) + cov5 + cov6 + cov7 + cov8
                     
                     # Main analysis
                     + strata(matched_id),
                     
                     # # Cousin-controlled analysis
                     # + strata(LopNr_case),
                     
                     cluster = LopNr, # LopNr = mother ID (accounts for non-independence among offspring)
                     robust = TRUE,
                     data = df)
  
  dat <- summary(cox_model)
  
  HR  <- dat$conf.int[1, "exp(coef)"]
  LCL <- dat$conf.int[1, "lower .95"]
  UCL <- dat$conf.int[1, "upper .95"]
  
  table_cox <- data.frame(
    `Adjusted HR (95% CI) (model 2: fup=5yrs, BMI=ns)` =
      sprintf("%.2f (%.2f-%.2f)", HR, LCL, UCL),
    check.names = FALSE
  )
  
  table_cox <- rbind("", "", "", "1 (Reference)", table_cox)
  
  return(table_cox)
  
}

cox_adjusted_4 <- coxadjusted(df$outc5, df$time2outc5, df$group_binary,
                              df$education, # cov1
                              df$country, # cov2
                              df$smoking, # cov3
                              df$bmi, # cov4
                              df$comorb_HYPERTENSION_dx_meds, # cov5
                              df$comorb_DYSLIPIDEMIA_dx_meds, # cov6
                              df$comorb_DIABETES_dx_meds, # cov7
                              df$comorb_ndd_psych # cov8
)


######################################
## Model 3: Adjusted Cox regression ##
######################################

coxadjusted <- function(event, time2event, exposure, cov1, cov2, cov3, cov4, cov5, cov6, cov7, cov8, cov9, cov10, cov11){
  cox_model <- coxph(Surv(time2event, event) ~ as.factor(exposure) + cov1 + cov2 + cov3 + ns(cov4, df = 4) + cov5 + cov6 + cov7 + cov8 + cov9 + cov10 + cov11
                     
                     # Main analysis
                     + strata(matched_id),
                     
                     # # Cousin-controlled analysis
                     # + strata(LopNr_case),
                     
                     cluster = LopNr, # LopNr = mother ID (accounts for non-independence among offspring)
                     robust = TRUE,
                     data = df)
  
  dat <- summary(cox_model)
  
  HR  <- dat$conf.int[1, "exp(coef)"]
  LCL <- dat$conf.int[1, "lower .95"]
  UCL <- dat$conf.int[1, "upper .95"]
  
  table_cox <- data.frame(
    `Adjusted HR (95% CI) (Model 3: meds, BMI ns)` =
      sprintf("%.2f (%.2f-%.2f)", HR, LCL, UCL),
    check.names = FALSE
  )
  
  table_cox <- rbind("", "", "", "1 (Reference)", table_cox)
  
  return(table_cox)
}

cox_adjusted_5 <- coxadjusted(df$outc, df$time2outc, df$group_binary,
                              df$education, # cov1
                              df$country, # cov2
                              df$smoking, # cov3
                              df$bmi, # cov4
                              df$comorb_HYPERTENSION_dx_meds, # cov5
                              df$comorb_DYSLIPIDEMIA_dx_meds, # cov6
                              df$comorb_DIABETES_dx_meds, # cov7
                              df$comorb_ndd_psych, # cov8
                              df$med_psychol, # cov9
                              df$med_antidep, # cov10
                              df$med_asm # cov11
)