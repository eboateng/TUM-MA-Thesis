
library(dplyr)
library(readxl)
library(data.table)

########## - Import the merged FF3M-factors, Sentiment Index and anomaly excess returns under long-short strategies - ##########
ff3m_bw_data <- read_excel("C:/Data/Strategies_SI_FF3M/FF3M_BW_98_20.xlsx")
merged_US_data <- fread("C:/Data/Mispricing/merged_US_data.csv")

# Selecting the relevant columns
selected_data <- merged_US_data %>%
  select(date, Mkt.RF, SMB, HML, RF, starts_with("ex_"))

# Set the consistent format of date
selected_data$date <- as.character(selected_data$date)
ff3m_bw_data$date <- as.character(ff3m_bw_data$date)

merged_data <- left_join(selected_data, ff3m_bw_data, by = "date")

merged_US_data_BW_98_20 <- merged_data[,.(date, Mkt.RF, SMB, HML, RF,
                                    SENT_ORTH, HSENT,	LSENT,	
                                    ex_Long_GPtoAsset_L1, ex_Short_GPtoAsset_L1, ex_Spread_GPtoAsset_L1, 
                                    ex_Long_ROA_L1, ex_Short_ROA_L1, ex_Spread_ROA_L1,
                                    ex_Long_AssetGrowth_L1, ex_Short_AssetGrowth_L1, ex_Spread_AssetGrowth_L1,
                                    ex_Long_NetOperatingAssets_L1, ex_Short_NetOperatingAssets_L1, ex_Spread_NetOperatingAssets_L1,
                                    ex_Long_Accruals_L1, ex_Short_Accruals_L1, ex_Spread_Accruals_L1,
                                    ex_Long_InvestmentToAsset_L1, ex_Short_InvestmentToAsset_L1, ex_Spread_InvestmentToAsset_L1,
                                    ex_Long_NetStockIssues_L1, ex_Short_NetStockIssues_L1, ex_Spread_NetStockIssues_L1,
                                    ex_Long_CompositeEquity_Log_L1, ex_Short_CompositeEquity_Log_L1, ex_Spread_CompositeEquity_Log_L1,
                                    ex_Long_MomentumReturn_L1, ex_Short_MomentumReturn_L1, ex_Spread_MomentumReturn_L1,
                                    ex_Long_O_Score_L1, ex_Short_O_Score_L1, ex_Spread_O_Score_L1,
                                    ex_Long_COMBI, ex_Short_COMBI, ex_Spread_COMBI)]

fwrite(merged_US_data_BW_98_20, "C:/Data/Mispricing/merged_US_data_BW_98_20.csv")

########## - Import the merged FF3M-factors, Sentiment Index and anomaly excess returns under long-short strategies - ##########
library(sandwich)
library(lmtest)

data <- read.csv("C:/Data/Mispricing/merged_US_data_BW_98_20.csv", header = TRUE, sep = ",")  

# Transfer all units for benchmark factors and anomaly excess returns to percent 

data[, 2:5] <- data[, 2:5] * 100
data[, 9:41] <- data[, 9:41] * 100

# Categorize the anomaly excess return based on the strategies

long_cols <- names(data[,c(9,12,15,18,21,24,27,30,33,36,39)])  # Anomaly excess returns on the long leg
short_cols <- names(data[,c(10,13,16,19,22,25,28,31,34,37,40)])   # Anomaly excess returns on the short leg
spread_cols <- names(data[,c(11,14,17,20,23,26,29,32,35,38,41)])  # Anomaly excess returns spreads 

colx <- names(data[,9:41])  # Anomaly excess returns


########### - From Table 1 to Table 5 - ###########
# All t-statistics based on White standard error/robust standard error               

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


## Table 2: Average excess returns for 11 anomalies under long-short strategies based on two sentiment categories:              

perform_t_test <- function(colx) {  
  result2 <- data.frame(Anomaly = character(length(colx)), 
                        HSENT_Mean = numeric(length(colx)), 
                        HSENT_t = numeric(length(colx)), 
                        LSENT_Mean = numeric(length(colx)), 
                        LSENT_t = numeric(length(colx)), 
                        Diff_Mean = numeric(length(colx)), 
                        Diff_t = numeric(length(colx)), 
                        stringsAsFactors = FALSE)
  
  for (i in seq_along(colx)) {
    
    # High sentiment
    tmp_h <- na.omit(data[data$HSENT == 1, colx[i]])
    mean_h <- mean(tmp_h)
    model2_h <- lm(tmp_h ~ 1)
    robust_se_h <- sqrt(diag(vcovHC(model2_h, type = "HC1")))
    t_statistic_h <- coef(model2_h) / robust_se_h
    
    # Low sentiment
    tmp_l <- na.omit(data[data$LSENT == 1, colx[i]])
    mean_l <- mean(tmp_l)
    model2_l <- lm(tmp_l ~ 1)
    robust_se_l <- sqrt(diag(vcovHC(model2_l, type = "HC1")))
    t_statistic_l <- coef(model2_l) / robust_se_l
    
    # Difference in means
    diff_mean <- mean_h - mean_l
    combined_se <- sqrt(robust_se_h^2 + robust_se_l^2)  # Assuming sample independence for high and low sentiment
    t_statistic_diff <- diff_mean / combined_se
    
    result2[i,] <- c(colx[i],
                     round(mean_h, 2),  # mean based on high sentiment 
                     round(t_statistic_h, 2),    # t-statistic
                     round(mean_l, 2),  # mean based on low sentiment
                     round(t_statistic_l, 2),    # t-statistic
                     round(diff_mean, 2),  # difference in means
                     round(t_statistic_diff, 2))    # t-statistic for difference
  }                                                                                                                     
  return(result2)
}

result2_long <- perform_t_test(long_cols)  # under long leg
print(result2_long)
result2_short <- perform_t_test(short_cols)   # under short leg
print(result2_short)
result2_spread <- perform_t_test(spread_cols)   # long minus short
print(result2_spread)


## Table 3: Average benchmark-adjusted returns for 11 anomalies under long-short strategies based on two sentiment categories

perform_lm <- function(colx) {
  result3 <- data.frame(Anomaly = character(length(colx)), 
                        Estimate_HSENT = numeric(length(colx)),
                        t_statistic_HSENT = numeric(length(colx)),
                        Estimate_LSENT = numeric(length(colx)),
                        t_statistic_LSENT = numeric(length(colx)),
                        Diff_Estimate = numeric(length(colx)),
                        Diff_t_statistic = numeric(length(colx)),
                        stringsAsFactors = FALSE)
  
  for (i in seq_along(colx)) {
    anomaly <- colx[i]
    model3 <- as.formula(paste0(anomaly, " ~ HSENT + LSENT + Mkt.RF + SMB + HML - 1"))  # Omit the intercept: direct effect of sentiment variable
    fit3 <- lm(model3, data = data[complete.cases(data[, c(anomaly, "HSENT", "LSENT", "Mkt.RF", "SMB", "HML")]), ])
    
    robust_se <- sqrt(diag(vcovHC(fit3, type = "HC1")))
    
    result3[i, "Anomaly"] <- anomaly
    result3[i, c("Estimate_HSENT", "t_statistic_HSENT")] <- (c(round(coef(fit3)["HSENT"], 2), round(coef(fit3)["HSENT"] / robust_se["HSENT"], 2)))  # High
    result3[i, c("Estimate_LSENT", "t_statistic_LSENT")] <- (c(round(coef(fit3)["LSENT"], 2), round(coef(fit3)["LSENT"] / robust_se["LSENT"], 2)))  # Low
    
    # High- low
    diff_estimate <- coef(fit3)["HSENT"] - coef(fit3)["LSENT"]
    combined_se <- sqrt(robust_se["HSENT"]^2 + robust_se["LSENT"]^2)
    diff_t_statistic <- diff_estimate / combined_se
    
    result3[i, "Diff_Estimate"] <- round(diff_estimate, 2)
    result3[i, "Diff_t_statistic"] <- round(diff_t_statistic, 2)
  }
  
  return(result3)
}

result3_long <- perform_lm(long_cols)  # under long leg
print(result3_long)
result3_short <- perform_lm(short_cols)  # under short leg
print(result3_short)
result3_spread <- perform_lm(spread_cols)  # long minus short
print(result3_spread)


## Table 4: regressing excess returns under long-short strategies on the lagged sentiment index

data$LAG_SENT <- lag(data$SENT_ORTH, 1) # Add the lagged sentiment index into the data
 
perform_lm <- function(colx) {
  result4 <- matrix(NA, nrow = length(colx), ncol = 2)
  colnames(result4) <- c("Estimate", "t-statistic")
  rownames(result4) <- colx
  
  for (i in seq_along(colx)) {
    model4 <- as.formula(paste0(colx[i], " ~ LAG_SENT"))
    fit4 <- lm(model4, data = data[complete.cases(data[, c(colx[i], "LAG_SENT")]), ])
    
    robust_se <- sqrt(diag(vcovHC(fit4, type = "HC1")))
    
    cat("\nSummary for", colx[i], ":\n")
    print(coeftest(fit4, vcov = vcovHC(fit4, type = "HC1")))

    result4[i, "Estimate"] <- round(coef(fit4)["LAG_SENT"], 2)   # the lagged sentiment index's estimate
    result4[i, "t-statistic"] <- round(coef(fit4)["LAG_SENT"] / robust_se["LAG_SENT"], 2)   # the lagged sentiment index's t-statistic
  }
  
  return(result4)
}

result4_long <- perform_lm(long_cols)  # under long leg
print(result4_long)
result4_short <- perform_lm(short_cols)  # under short leg
print(result4_short)
result4_spread <- perform_lm(spread_cols)  # long minus short
print(result4_spread)


## Table 5: regressing benchmark-adjusted returns under long-short strategies on the lagged sentiment index

perform_lm <- function(colx) {
  result5 <- matrix(NA, nrow = length(colx), ncol = 2)
  colnames(result5) <- c("Estimate", "t-statistic")
  rownames(result5) <- colx
  
  for (i in seq_along(colx)) {
    model5 <- as.formula(paste0(colx[i], " ~ LAG_SENT + Mkt.RF + SMB + HML"))
    fit5 <- lm(model5, data = data[complete.cases(data[, c(colx[i], "LAG_SENT", "Mkt.RF" , "SMB" , "HML")]), ])
    
    robust_se <- sqrt(diag(vcovHC(fit5, type = "HC1")))
    
    cat("\nSummary for", colx[i], ":\n")
    print(coeftest(fit5, vcov = vcovHC(fit5, type = "HC1")))
    
    result5[i, "Estimate"] <- round(coef(fit5)["LAG_SENT"], 2)   # the lagged sentiment index's estimate
    result5[i, "t-statistic"] <- round(coef(fit5)["LAG_SENT"] / robust_se["LAG_SENT"], 2)   # the lagged sentiment index's t-statistic
  }
  
  return(result5)
}

result5_long <- perform_lm(long_cols)  # under long leg
print(result5_long)
result5_short <- perform_lm(short_cols)  # under short leg
print(result5_short)
result5_spread <- perform_lm(spread_cols)  # long minus short
print(result5_spread)

################################################ Correlation Test ###################################################
# Test the correlation between the ST anomaly return and the anomaly return calculated by myself within the overlapping period
# Load data
data1 <- read.csv("C:/Data/Strategies_SI_FF3M/AnomalyRet_10+1_Long_Short_Spread_USA.csv")
data2 <- read.csv("C:/Data/Strategies_SI_FF3M/ST_AnomalyRet_10+1_Long_Short_Spread_87_08.csv") # Stambaugh 

# Filter the overlapping period
data1 <- data1[data1$date <= 200801, ]
data2 <- data2[data2$date >= 199801, ]

# Rename columns in data2 to match data1
names(data2)[2:ncol(data2)] <- names(data1)[2:ncol(data1)]

# Merge the datasets by 'date' to align them
data_combined <- merge(data1, data2, by = "date", suffixes = c(".1", ".2"))

# Remove 'date' column after merging
data_combined <- data_combined[,-1]

# Calculate correlations for each pair of corresponding columns
correlations <- sapply(1:(ncol(data_combined)/2), function(i) { # adjust index range
  col1 <- data_combined[, i]             # Columns from data1
  col2 <- data_combined[, i + (ncol(data_combined)/2)]  # Corresponding columns from data2
  cor(col1, col2, use = "complete.obs")  # Ensure numeric columns, exclude NA pairs
})

# Set names to the correlations for clarity
names(correlations) <- names(data1)[-1] 

print(correlations) 
