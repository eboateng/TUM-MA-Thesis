#############################################################################################################################

# Statistical analysis for anomaly returns and sentiment indexes

# Use Stock Index Buzz, Stock Index Sentiment, and Stock Index Fear as sentiment index 
# and categorize the high sentiment (HATTEN;HSENT;HFEAR) and low sentiment (LATTEN;LSENT;LFEAR)

# The code is designed to be executed separately for each country.

########################################################### Set Up ##########################################################
#install.packages(c("sandwich", "lmtest"))
library(sandwich)
library(lmtest)

data <- read.csv("C:/Data/Merged Data/Merged_US_Data.csv", header = TRUE, sep = ",")  

# Categorize the anomaly excess return based on the strategies

long_cols <- names(data[,c(15,18,21,24,27,30,33,36,39,42,45)])  # Anomaly excess returns on the long leg
short_cols <- names(data[,c(16,19,22,25,28,31,34,37,40,43,46)])   # Anomaly excess returns on the short leg
spread_cols <- names(data[,c(17,20,23,26,29,32,35,38,41,44,47)])  # Anomaly excess returns spreads 

colx <- names(data[,15:47])  # Anomaly excess returns

################################################# Statistical Analysis ######################################################
# From Table 1 to Table 5
# All t-statistics based on White standard error/robust standard error               

###################################################### Table 1 ##############################################################
## Table 1 Panel A: Correlation matrix among the long-short benchmark-adjusted returns for 11 anomalies

residual_matrix <- matrix(NA, nrow = nrow(data), ncol = length(spread_cols))  # Create an empty matrix to store residuals

  for (i in seq_along(spread_cols)) {
    model1A <- as.formula(paste0((spread_cols)[i], " ~ Mkt.RF + SMB + HML"))  # Fit a linear regression model
    fit1A <- lm(model1A, data = data[complete.cases(data[, c((spread_cols)[i], "Mkt.RF", "SMB", "HML")]), ])
    residual_matrix[complete.cases(data[, c((spread_cols)[i], "Mkt.RF", "SMB", "HML")]), i] <- residuals(fit1A)
  }

cor_matrix <- round(cor(residual_matrix, use = "complete.obs"), 2) # Correlation matrix for the residuals

# Make the correlation matrix tidy
lower_triangular <- cor_matrix
lower_triangular[upper.tri(lower_triangular)] <- NA
print(lower_triangular, na.print="")


## Table 1 Panel B: Average excess returns for 11 anomalies under long-short strategies

perform_t_test <- function(colx) {
  result1B <- data.frame(Anomaly = character(length(colx)), 
                         Mean = numeric(length(colx)), 
                         t_statistic = numeric(length(colx)), 
                         stringsAsFactors = FALSE)  # ensures that character strings are not converted to factors
  
  for (i in seq_along(colx)) {
    anomaly <- colx[i]
    data_subset <- data[[anomaly]][complete.cases(data[[anomaly]])]
    model1B <- lm(data_subset ~ 1)  # Model with only the intercept
    robust_se <- sqrt(diag(vcovHC(model1B, type = "HC1")))  # robust SE (the heteroskedasticity-consistent covariance matrix)
    
    result1B[i, "Anomaly"] <- anomaly
    result1B[i, "Mean"] <- round(coef(model1B)[1], 2)  # Intercept is the mean
    t_value <- coef(model1B)[1] / robust_se  # t-statistic
    result1B[i, "t_statistic"] <- round(t_value, 2)
  }
  
  return(result1B)
}

result1B_long <- perform_t_test(long_cols)  # under long leg
print(result1B_long)
result1B_short <- perform_t_test(short_cols)   # under short leg
print(result1B_short)
result1B_spread <- perform_t_test(spread_cols)   # long minus short
print(result1B_spread)


## Table 1 Panel C: Average benchmark-adjusted returns for 11 anomalies under long-short strategies

perform_lm <- function(colx) {
  result1C <- matrix(NA, nrow = length(colx), ncol = 2)
  colnames(result1C) <- c("Estimate", "t-statistic")
  rownames(result1C) <- colx
  
  for (i in seq_along(colx)) {
    model1C <- as.formula(paste0(colx[i], " ~ Mkt.RF + SMB + HML"))
    fit1C <- lm(model1C, data = data[complete.cases(data[, c(colx[i], "Mkt.RF", "SMB", "HML")]), ])
    
    robust_se <- sqrt(diag(vcovHC(fit1C, type = "HC1")))
    
    cat("\nSummary for", colx[i], ":\n")
    print(coeftest(fit1C, vcov = vcovHC(fit1C, type = "HC1")))
    
    result1C[i, "Estimate"] <- round(coef(fit1C)[1], 2)   # the intercept's estimate
    result1C[i, "t-statistic"] <- round(coef(fit1C)[1] / robust_se[1], 2)  # the intercept's t-statistic
  }
  
  return(result1C)
}

result1C_long <- perform_lm(long_cols)  # under long leg
print(result1C_long)
result1C_short <- perform_lm(short_cols)  # under short leg
print(result1C_short)
result1C_spread <- perform_lm(spread_cols)  # long minus short
print(result1C_spread)


###################################################### Table 2 ##############################################################
# Average excess returns for 11 anomalies under long-short strategies based on two sentiment categories           

perform_t_test <- function(colx) {  
  result2 <- data.frame(Anomaly = character(length(colx)), 
                        HSENT_Mean = numeric(length(colx)), 
                        HSENT_t = numeric(length(colx)), 
                        LSENT_Mean = numeric(length(colx)), 
                        LSENT_t = numeric(length(colx)),
                        Diff_Mean_SENT = numeric(length(colx)), 
                        Diff_t_SENT = numeric(length(colx)), 
                        HATTEN_Mean = numeric(length(colx)), 
                        HATTEN_t = numeric(length(colx)), 
                        LATTEN_Mean = numeric(length(colx)), 
                        LATTEN_t = numeric(length(colx)), 
                        Diff_Mean_ATTEN = numeric(length(colx)), 
                        Diff_t_ATTEN = numeric(length(colx)), 
                        HFEAR_Mean = numeric(length(colx)), 
                        HFEAR_t = numeric(length(colx)), 
                        LFEAR_Mean = numeric(length(colx)), 
                        LFEAR_t = numeric(length(colx)), 
                        Diff_Mean_FEAR = numeric(length(colx)), 
                        Diff_t_FEAR = numeric(length(colx)), 
                        stringsAsFactors = FALSE)
  
  for (i in seq_along(colx)) {
    
    # Calculate for HSENT
    tmp_hsent <- na.omit(data[data$HSENT == 1, colx[i]])
    mean_hsent <- mean(tmp_hsent)
    model_hsent <- lm(tmp_hsent ~ 1)
    robust_se_hsent <- sqrt(diag(vcovHC(model_hsent, type = "HC1")))
    t_statistic_hsent <- coef(model_hsent) / robust_se_hsent
    
    # Calculate for LSENT
    tmp_lsent <- na.omit(data[data$LSENT == 1, colx[i]])
    mean_lsent <- mean(tmp_lsent)
    model_lsent <- lm(tmp_lsent ~ 1)
    robust_se_lsent <- sqrt(diag(vcovHC(model_lsent, type = "HC1")))
    t_statistic_lsent <- coef(model_lsent) / robust_se_lsent
    
    # Calculate for HATTEN
    tmp_hatten <- na.omit(data[data$HATTEN == 1, colx[i]])
    mean_hatten <- mean(tmp_hatten)
    model_hatten <- lm(tmp_hatten ~ 1)
    robust_se_hatten <- sqrt(diag(vcovHC(model_hatten, type = "HC1")))
    t_statistic_hatten <- coef(model_hatten) / robust_se_hatten
    
    # Calculate for LATTEN
    tmp_latten <- na.omit(data[data$LATTEN == 1, colx[i]])
    mean_latten <- mean(tmp_latten)
    model_latten <- lm(tmp_latten ~ 1)
    robust_se_latten <- sqrt(diag(vcovHC(model_latten, type = "HC1")))
    t_statistic_latten <- coef(model_latten) / robust_se_latten
    
    # Calculate for HFEAR
    tmp_hfear <- na.omit(data[data$HFEAR == 1, colx[i]])
    mean_hfear <- mean(tmp_hfear)
    model_hfear <- lm(tmp_hfear ~ 1)
    robust_se_hfear <- sqrt(diag(vcovHC(model_hfear, type = "HC1")))
    t_statistic_hfear <- coef(model_hfear) / robust_se_hfear
    
    # Calculate for LFEAR
    tmp_lfear <- na.omit(data[data$LFEAR == 1, colx[i]])
    mean_lfear <- mean(tmp_lfear)
    model_lfear <- lm(tmp_lfear ~ 1)
    robust_se_lfear <- sqrt(diag(vcovHC(model_lfear, type = "HC1")))
    t_statistic_lfear <- coef(model_lfear) / robust_se_lfear
    
    # Assuming sample independence for sentiment comparisons
    # Stock Index Sentiment
    diff_mean_sent <- mean_hsent - mean_lsent
    combined_se_sent <- sqrt(robust_se_hsent^2 + robust_se_lsent^2)
    t_statistic_diff_sent <- diff_mean_sent / combined_se_sent
    
    # Stock Index Buzz
    diff_mean_atten <- mean_hatten - mean_latten
    combined_se_atten <- sqrt(robust_se_hatten^2 + robust_se_latten^2)
    t_statistic_diff_atten <- diff_mean_atten / combined_se_atten
    
    # Stock Index Fear
    diff_mean_fear <- mean_hfear - mean_lfear
    combined_se_fear <- sqrt(robust_se_hfear^2 + robust_se_lfear^2)
    t_statistic_diff_fear <- diff_mean_fear / combined_se_fear
    
    result2[i,] <- c(colx[i],
                     round(mean_hsent, 2),         # HSENT mean 
                     round(t_statistic_hsent, 2),  # HSENT t-statistic
                     round(mean_lsent, 2),         # LSENT mean
                     round(t_statistic_lsent, 2),  # LSENT t-statistic
                     round(diff_mean_sent, 2),         # Difference in means for Stock Index Sentiment
                     round(t_statistic_diff_sent, 2),  # Difference in t-statistic for Stock Index Sentiment
                     round(mean_hatten, 2),        # HATTEN mean
                     round(t_statistic_hatten, 2), # HATTEN t-statistic
                     round(mean_latten, 2),        # LATTEN mean
                     round(t_statistic_latten, 2), # LATTEN t-statistic
                     round(diff_mean_atten, 2),        # Difference in means for Stock Index Buzz
                     round(t_statistic_diff_atten, 2), # Difference in t-statistic for Stock Index Buzz
                     round(mean_hfear, 2),         # HFEAR mean
                     round(t_statistic_hfear, 2),  # HFEAR t-statistic
                     round(mean_lfear, 2),         # LFEAR mean
                     round(t_statistic_lfear, 2),  # LFEAR t-statistic
                     round(diff_mean_fear, 2),         # Difference in means for Stock Index Fear
                     round(t_statistic_diff_fear, 2))  # Difference in t-statistic for Stock Index Fear
  }
  return(result2)
}

result2_long <- perform_t_test(long_cols)  # under long leg
print(result2_long)
result2_short <- perform_t_test(short_cols)   # under short leg
print(result2_short)
result2_spread <- perform_t_test(spread_cols)   # long minus short
print(result2_spread)


###################################################### Table 3 ##############################################################
## Average benchmark-adjusted returns for 11 anomalies under long-short strategies based on two sentiment categories

perform_lm <- function(colx) {
  result3 <- data.frame(Anomaly = character(length(colx)), 
                        Estimate_HSENT = numeric(length(colx)),
                        t_statistic_HSENT = numeric(length(colx)),
                        Estimate_LSENT = numeric(length(colx)),
                        t_statistic_LSENT = numeric(length(colx)),
                        Diff_Estimate_SENT = numeric(length(colx)),
                        Diff_t_statistic_SENT = numeric(length(colx)),
                        Estimate_HATTEN = numeric(length(colx)),
                        t_statistic_HATTEN = numeric(length(colx)),
                        Estimate_LATTEN = numeric(length(colx)),
                        t_statistic_LATTEN = numeric(length(colx)),
                        Diff_Estimate_ATTEN = numeric(length(colx)),
                        Diff_t_statistic_ATTEN = numeric(length(colx)),
                        Estimate_HFEAR = numeric(length(colx)),
                        t_statistic_HFEAR = numeric(length(colx)),
                        Estimate_LFEAR = numeric(length(colx)),
                        t_statistic_LFEAR = numeric(length(colx)),
                        Diff_Estimate_FEAR = numeric(length(colx)),
                        Diff_t_statistic_FEAR = numeric(length(colx)),
                        stringsAsFactors = FALSE)
  
  for (i in seq_along(colx)) {
    anomaly <- colx[i]
    
    # Stock Index Sentiment
    
    model_SENT <- as.formula(paste0(anomaly, " ~ HSENT + LSENT + Mkt.RF + SMB + HML - 1"))    
    
    fit_SENT <- lm(model_SENT, data = data[complete.cases(data[, c(anomaly, "HSENT", "LSENT", "Mkt.RF", "SMB", "HML")]), ])
    
    robust_se_SENT <- sqrt(diag(vcovHC(fit_SENT, type = "HC1")))
    
    estimate_HSENT <- coef(fit_SENT)["HSENT"]
    t_statistic_HSENT <- coef(fit_SENT)["HSENT"] / robust_se_SENT["HSENT"]
    
    estimate_LSENT <- coef(fit_SENT)["LSENT"]
    t_statistic_LSENT <- coef(fit_SENT)["LSENT"] / robust_se_SENT["LSENT"]
    
    # High - low
    diff_estimate_SENT <- coef(fit_SENT)["HSENT"] - coef(fit_SENT)["LSENT"] 
    combined_se_SENT <- sqrt(robust_se_SENT["HSENT"]^2 + robust_se_SENT["LSENT"]^2)
    diff_t_statistic_SENT <- diff_estimate_SENT / combined_se_SENT
    
     # Stock Index Buzz
    
    model_ATTEN <- as.formula(paste0(anomaly, " ~ HATTEN + LATTEN + Mkt.RF + SMB + HML - 1")) 
    
    fit_ATTEN <- lm(model_ATTEN, data = data[complete.cases(data[, c(anomaly, "HATTEN", "LATTEN", "Mkt.RF", "SMB", "HML")]), ])
    
    robust_se_ATTEN <- sqrt(diag(vcovHC(fit_ATTEN, type = "HC1")))
    
    estimate_HATTEN <- coef(fit_ATTEN)["HATTEN"]
    t_statistic_HATTEN <- coef(fit_ATTEN)["HATTEN"] / robust_se_ATTEN["HATTEN"]
    
    estimate_LATTEN <- coef(fit_ATTEN)["LATTEN"]
    t_statistic_LATTEN <- coef(fit_ATTEN)["LATTEN"] / robust_se_ATTEN["LATTEN"]
    
    # High - low
    diff_estimate_ATTEN <- coef(fit_ATTEN)["HATTEN"] - coef(fit_ATTEN)["LATTEN"] 
    combined_se_ATTEN <- sqrt(robust_se_ATTEN["HATTEN"]^2 + robust_se_ATTEN["LATTEN"]^2)
    diff_t_statistic_ATTEN <- diff_estimate_ATTEN / combined_se_ATTEN
    
    # Stock Index Fear
    
    model_FEAR <- as.formula(paste0(anomaly, " ~ HFEAR + LFEAR + Mkt.RF + SMB + HML - 1")) 
    
    fit_FEAR <- lm(model_FEAR, data = data[complete.cases(data[, c(anomaly, "HFEAR", "LFEAR", "Mkt.RF", "SMB", "HML")]), ])
    
    robust_se_FEAR <- sqrt(diag(vcovHC(fit_FEAR, type = "HC1")))
    
    estimate_HFEAR <- coef(fit_FEAR)["HFEAR"]
    t_statistic_HFEAR <- coef(fit_FEAR)["HFEAR"] / robust_se_FEAR["HFEAR"]
    
    estimate_LFEAR <- coef(fit_FEAR)["LFEAR"]
    t_statistic_LFEAR <- coef(fit_FEAR)["LFEAR"] / robust_se_FEAR["LFEAR"]
    
    # High - low
    diff_estimate_FEAR <- coef(fit_FEAR)["HFEAR"] - coef(fit_FEAR)["LFEAR"] 
    combined_se_FEAR <- sqrt(robust_se_FEAR["HFEAR"]^2 + robust_se_FEAR["LFEAR"]^2)
    diff_t_statistic_FEAR <- diff_estimate_FEAR / combined_se_FEAR
   
    result3[i,] <- c(colx[i],
                     round(estimate_HSENT, 2),         
                     round(t_statistic_HSENT, 2),  
                     round(estimate_LSENT, 2),        
                     round(t_statistic_LSENT, 2),  
                     round(diff_estimate_SENT, 2),        
                     round(diff_t_statistic_SENT, 2), 
                     round(estimate_HATTEN, 2),         
                     round(t_statistic_HATTEN, 2),  
                     round(estimate_LATTEN, 2),        
                     round(t_statistic_LATTEN, 2),  
                     round(diff_estimate_ATTEN, 2),        
                     round(diff_t_statistic_ATTEN, 2), 
                     round(estimate_HFEAR, 2),         
                     round(t_statistic_HFEAR, 2),  
                     round(estimate_LFEAR, 2),        
                     round(t_statistic_LFEAR, 2),  
                     round(diff_estimate_FEAR, 2),        
                     round(diff_t_statistic_FEAR, 2))
  }
  return(result3)
}

result3_long <- perform_lm(long_cols)  # under long leg
print(result3_long)
result3_short <- perform_lm(short_cols)  # under short leg
print(result3_short)
result3_spread <- perform_lm(spread_cols)  # long minus short
print(result3_spread)

###################################################### Table 4 ##############################################################
# Regressing excess returns under long-short strategies on the lagged sentiment index

# Add lagged sentiment indexes
data$LAG_SENT <- lag(data$Stock_Index_Sentiment, 1)
data$LAG_ATTEN <- lag(data$Stock_Index_Buzz, 1)
data$LAG_FEAR <- lag(data$Stock_Index_Fear, 1)

perform_lm <- function(colx) {
  result4 <- matrix(NA, nrow = length(colx), ncol = 6)
  colnames(result4) <- c("Estimate_LAGSENT", "t-statistic_LAGSENT", 
                         "Estimate_LAGATTEN", "t-statistic_LAGATTEN", 
                         "Estimate_LAGFEAR", "t-statistic_LAGFEAR")
  rownames(result4) <- colx

  for (i in seq_along(colx)) {
    
    # Stock Index Sentiment
    
    model_LAGSENT <- as.formula(paste0(colx[i], " ~ LAG_SENT"))
    fit_LAGSENT <- lm(model_LAGSENT, data = data[complete.cases(data[, c(colx[i], "LAG_SENT")]), ])

    robust_se_LAGSENT <- sqrt(diag(vcovHC(fit_LAGSENT, type = "HC1")))
    
    cat("\nSummary for", colx[i], ":\n")
    print(coeftest(fit_LAGSENT, vcov = vcovHC(fit_LAGSENT, type = "HC1")))

    result4[i, "Estimate_LAGSENT"] <- round(coef(fit_LAGSENT)["LAG_SENT"], 2)
    result4[i, "t-statistic_LAGSENT"] <- round(coef(fit_LAGSENT)["LAG_SENT"] / robust_se_LAGSENT["LAG_SENT"], 2) 
    
    # Stock Index Buzz
    
    model_LAGATTEN <- as.formula(paste0(colx[i], " ~ LAG_ATTEN"))
    fit_LAGATTEN <- lm(model_LAGATTEN, data = data[complete.cases(data[, c(colx[i], "LAG_ATTEN")]), ])
    
    robust_se_LAGATTEN <- sqrt(diag(vcovHC(fit_LAGATTEN, type = "HC1")))
    
    cat("\nSummary for", colx[i], ":\n")
    print(coeftest(fit_LAGATTEN, vcov = vcovHC(fit_LAGATTEN, type = "HC1")))
    
    result4[i, "Estimate_LAGATTEN"] <- round(coef(fit_LAGATTEN)["LAG_ATTEN"], 2)
    result4[i, "t-statistic_LAGATTEN"] <- round(coef(fit_LAGATTEN)["LAG_ATTEN"] / robust_se_LAGATTEN["LAG_ATTEN"], 2) 
    
    # Stock Index Fear
    
    model_LAGFEAR <- as.formula(paste0(colx[i], " ~ LAG_FEAR"))
    fit_LAGFEAR <- lm(model_LAGFEAR, data = data[complete.cases(data[, c(colx[i], "LAG_FEAR")]), ])
    
    robust_se_LAGFEAR <- sqrt(diag(vcovHC(fit_LAGFEAR, type = "HC1")))
    
    cat("\nSummary for", colx[i], ":\n")
    print(coeftest(fit_LAGFEAR, vcov = vcovHC(fit_LAGFEAR, type = "HC1")))
    
    result4[i, "Estimate_LAGFEAR"] <- round(coef(fit_LAGFEAR)["LAG_FEAR"], 2)
    result4[i, "t-statistic_LAGFEAR"] <- round(coef(fit_LAGFEAR)["LAG_FEAR"] / robust_se_LAGFEAR["LAG_FEAR"], 2) 
  }
  
  return(result4)
}

result4_long <- perform_lm(long_cols)  # under long leg
print(result4_long)
result4_short <- perform_lm(short_cols)  # under short leg
print(result4_short)
result4_spread <- perform_lm(spread_cols)  # long minus short
print(result4_spread)


###################################################### Table 5 ##############################################################
# Regressing benchmark-adjusted returns under long-short strategies on the lagged sentiment index

perform_lm <- function(colx) {
  
  result5 <- matrix(NA, nrow = length(colx), ncol = 6)
  colnames(result5) <- c("Estimate_LAGSENT", "t-statistic_LAGSENT", 
                         "Estimate_LAGATTEN", "t-statistic_LAGATTEN", 
                         "Estimate_LAGFEAR", "t-statistic_LAGFEAR")
  rownames(result5) <- colx
  
  for (i in seq_along(colx)) {
    
    # Stock Index Sentiment
    
    model_LAGSENT <- as.formula(paste0(colx[i], " ~ LAG_SENT + Mkt.RF + SMB + HML"))
    fit_LAGSENT <- lm(model_LAGSENT, data = data[complete.cases(data[, c(colx[i], "LAG_SENT", "Mkt.RF" , "SMB" , "HML")]), ])
    
    robust_se_LAGSENT <- sqrt(diag(vcovHC(fit_LAGSENT, type = "HC1")))
    
    cat("\nSummary for", colx[i], ":\n")
    print(coeftest(fit_LAGSENT, vcov = vcovHC(fit_LAGSENT, type = "HC1")))
    
    result5[i, "Estimate_LAGSENT"] <- round(coef(fit_LAGSENT)["LAG_SENT"], 2) 
    result5[i, "t-statistic_LAGSENT"] <- round(coef(fit_LAGSENT)["LAG_SENT"] / robust_se_LAGSENT["LAG_SENT"], 2)
    
    # Stock Index Buzz
    
    model_LAGATTEN <- as.formula(paste0(colx[i], " ~ LAG_ATTEN + Mkt.RF + SMB + HML"))
    fit_LAGATTEN <- lm(model_LAGATTEN, data = data[complete.cases(data[, c(colx[i], "LAG_ATTEN", "Mkt.RF" , "SMB" , "HML")]), ])
    
    robust_se_LAGATTEN <- sqrt(diag(vcovHC(fit_LAGATTEN, type = "HC1")))
    
    cat("\nSummary for", colx[i], ":\n")
    print(coeftest(fit_LAGATTEN, vcov = vcovHC(fit_LAGATTEN, type = "HC1")))
    
    result5[i, "Estimate_LAGATTEN"] <- round(coef(fit_LAGATTEN)["LAG_ATTEN"], 2) 
    result5[i, "t-statistic_LAGATTEN"] <- round(coef(fit_LAGATTEN)["LAG_ATTEN"] / robust_se_LAGATTEN["LAG_ATTEN"], 2)
    
    # Stock Index Fear
    
    model_LAGFEAR <- as.formula(paste0(colx[i], " ~ LAG_FEAR + Mkt.RF + SMB + HML"))
    fit_LAGFEAR <- lm(model_LAGFEAR, data = data[complete.cases(data[, c(colx[i], "LAG_FEAR", "Mkt.RF" , "SMB" , "HML")]), ])
    
    robust_se_LAGFEAR <- sqrt(diag(vcovHC(fit_LAGFEAR, type = "HC1")))
    
    cat("\nSummary for", colx[i], ":\n")
    print(coeftest(fit_LAGFEAR, vcov = vcovHC(fit_LAGFEAR, type = "HC1")))
    
    result5[i, "Estimate_LAGFEAR"] <- round(coef(fit_LAGFEAR)["LAG_FEAR"], 2) 
    result5[i, "t-statistic_LAGFEAR"] <- round(coef(fit_LAGFEAR)["LAG_FEAR"] / robust_se_LAGFEAR["LAG_FEAR"], 2)
  }
  
  return(result5)
}

result5_long <- perform_lm(long_cols)  # under long leg
print(result5_long)
result5_short <- perform_lm(short_cols)  # under short leg
print(result5_short)
result5_spread <- perform_lm(spread_cols)  # long minus short
print(result5_spread)

