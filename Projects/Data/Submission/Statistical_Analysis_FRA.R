#############################################################################################################################

# Construct long-short strategies and calculate the anomaly excess returns

# Output: tidy data [XX_Data_long_ST]; 10+1 anomaly returns under strategies [XX_strategies]; 10+1 anomaly excess returns under strategies [merged_XX_data]

########################################################### Set Up ##########################################################
library(dplyr)
library(data.table)
library(lubridate)
library(zoo) 

FRA_mispricing <- fread("C:/Data/Mispricing/fra_mispricing.csv")

setnames(FRA_mispricing, old = c( "me_lag1",   "ret",    "o_score",   "ret_12_1",      "gp_at",  "niq_at",  "chcsho_12m",        "eqnpo_12m",    "oaccruals_at",     "noa_at",          "at_gr1",      "ppeinv_gr1a"),
         new = c("LagMV_USD", "RET_USD", "O_Score", "MomentumReturn", "GPtoAsset", "ROA", "NetStockIssues", "CompositeEquity_Log", "Accruals", "NetOperatingAssets",  "AssetGrowth", "InvestmentToAsset"))

FRA_mispricing_relevant <- FRA_mispricing[,.(id, eom, size_grp, LagMV_USD, RET_USD,
                                             GPtoAsset, ROA, AssetGrowth, NetOperatingAssets, Accruals, InvestmentToAsset, 
                                             NetStockIssues, CompositeEquity_Log, MomentumReturn, O_Score)]

# Convert eom to a 'yearmon' object
FRA_mispricing_relevant[, date := as.yearmon(as.Date(as.character(eom), "%Y%m%d"), "%Y-%m")]

# Create a lagged dataset
FRA_mispricing_lagged <- FRA_mispricing_relevant %>%
  mutate(date = as.yearmon(format(date + 1/12, "%Y-%m"))) %>%
  rename_with(~paste0(., "_L1"), .cols = c(size_grp, GPtoAsset, ROA, AssetGrowth, NetOperatingAssets, Accruals, InvestmentToAsset, NetStockIssues, CompositeEquity_Log, MomentumReturn, O_Score))

# Join the original data set (all entries remain) with the lagged data set
FRA_mispricing_with_lags <- FRA_mispricing_relevant %>%
  left_join(FRA_mispricing_lagged, by = c("date", "id"))

FRA_mispricing_with_lags_relevant <- FRA_mispricing_with_lags[,.(id, date, size_grp, size_grp_L1, LagMV_USD.x, RET_USD.x,
                                                                 GPtoAsset, GPtoAsset_L1, ROA, ROA_L1, AssetGrowth, AssetGrowth_L1, NetOperatingAssets, NetOperatingAssets_L1, 
                                                                 Accruals, Accruals_L1, InvestmentToAsset, InvestmentToAsset_L1, 
                                                                 NetStockIssues, NetStockIssues_L1, CompositeEquity_Log, CompositeEquity_Log_L1, MomentumReturn, MomentumReturn_L1, O_Score, O_Score_L1)]

# Filter out the penny stocks and select the relevant columns
FR_Data_long_ST <- FRA_mispricing_with_lags_relevant %>%
  # Filter out rows where size_grp_L1 is "micro" or "nano"
  filter(!size_grp_L1 %in% c("micro", "nano")) %>%
  # Rename columns LagMV_USD.x and RET_USD.x
  rename(LagMV_USD = LagMV_USD.x, RET_USD = RET_USD.x) %>%
  # Remove rows where LagMV_USD is NA
  filter(!is.na(LagMV_USD)) %>%
  select(id, size_grp_L1, date, LagMV_USD, RET_USD, 
         GPtoAsset_L1, ROA_L1, AssetGrowth_L1, NetOperatingAssets_L1, Accruals_L1, InvestmentToAsset_L1, 
         NetStockIssues_L1, CompositeEquity_Log_L1, MomentumReturn_L1, O_Score_L1)

FR_Data_long_ST <- FR_Data_long_ST[order(date)]

# Set the required sample period (All anomalies from 199801 to 202012)
FR_Data_long_ST[, date := as.character(format(as.yearmon(date, "%Y-%m"), "%Y%m"))]
FR_Data_long_ST <- FR_Data_long_ST[date >= "199801" & date <= "202012"]
fwrite(FR_Data_long_ST, "C:/Data/Anomalies/FR_Data_long_ST.csv")

################################################# Create Long and Short strategies ###########################################
FR_Data_long_ST <- fread("C:/Data/Anomalies/FR_Data_long_ST.csv")

# List of all anomalies and those to inverse (negative relationship between anomaly value and future returns)
allAnomalies <- c("GPtoAsset_L1", "ROA_L1", "AssetGrowth_L1", "NetOperatingAssets_L1", "Accruals_L1", "InvestmentToAsset_L1", 
                  "NetStockIssues_L1", "CompositeEquity_Log_L1", "MomentumReturn_L1", "O_Score_L1")
inverseAnomalies <- c("AssetGrowth_L1", "NetOperatingAssets_L1", "Accruals_L1", "InvestmentToAsset_L1", "NetStockIssues_L1", "O_Score_L1")

createLongShortStrategies <- function(data_long, anomalyVariables, inverseVariables) {
  setDT(data_long)
  
  # Initialize the data table to store the a list for individual anomaly strategies and combined results
  strategies <- data.table(date = unique(data_long$date))
  combiList <- list()
  
  for (anomalyVariable in anomalyVariables) {
    
    ## the normal anomaly variable: "GPtoAsset_L1", "ROA_L1", "CompositeEquity_Log_L1"(definition deviates), "MomentumReturn_L1"
    # the mispricing degree (anomaly value) higher -> the anomaly return higher [currently: underpriced]
    # Long Position: Should be taken in stocks with higher anoamaly value (assign higher ranks), 
    # as they are expected to be underpriced and to yield higher future returns.
    ## the inverse anomaly variable: "AssetGrowth_L1", "NetOperatingAssets_L1", "Accruals_L1", "InvestmentToAsset_L1", "NetStockIssues_L1", "O_Score_L1"
    # the mispricing degree (anomaly value) higher -> the anomaly return lower [currently: overpriced]
    # Long Position: Should be taken in stocks with lower anoamaly value (assign higher ranks) -> value inverse
    # as they are expected to be underpriced and to yield higher future returns.
    
    data_long[, (anomalyVariable) := .SD[[anomalyVariable]] * ifelse(anomalyVariable %in% inverseVariables, -1, 1), .SDcols = anomalyVariable]
    
    # Calculates the 10th and 90th percentiles within each date for each anomaly and assign the 1st and 10th deciles accordingly
    
    data_long[, c("decile", "p10", "p90") := {
      p10 <- quantile(get(anomalyVariable), probs = 0.1, na.rm = TRUE)
      p90 <- quantile(get(anomalyVariable), probs = 0.9, na.rm = TRUE)
      
      decile <- fifelse(get(anomalyVariable) <= p10, 1L,  # assign the 1st decile
                        fifelse(get(anomalyVariable) >= p90, 10L, # assign the 10th decile
                                NA_integer_)) # assign NA to those that do not fall into either of the deciles
      .(decile, p10, p90)
    }, by = date, .SDcols = anomalyVariable]
    
    # Calculate the value-weighted returns for extreme deciles under long-short strategies
    
    spread_returns <- data_long[!is.na(decile), .(
      Long_Return = weighted.mean(RET_USD[decile == 10], LagMV_USD[decile == 10], na.rm = TRUE),
      Short_Return = weighted.mean(RET_USD[decile == 1], LagMV_USD[decile == 1], na.rm = TRUE)
    ), by = date]
    
    spread_returns[, Spread := Long_Return - Short_Return] # Calculate Spread of 10 anomalies
    
    # Store the results with the tidy names
    colNames <- c(sprintf("Long_%s", anomalyVariable), sprintf("Short_%s", anomalyVariable), sprintf("Spread_%s", anomalyVariable))
    spread_returns[, (colNames) := .(Long_Return, Short_Return, Spread)]
    strategies <- merge(strategies, spread_returns[, .(date, Long_Return, Short_Return, Spread)], by = "date", 
                        all.x = TRUE, suffixes = c("", paste0("_", anomalyVariable)))
    setnames(strategies, old = c("Long_Return", "Short_Return", "Spread"), new = colNames)
    
    # Add to combiList for combination strategy calculation
    spread_returns_normalized <- spread_returns[, .(date, Long_Return, Short_Return, Spread)]
    combiList[[anomalyVariable]] <- spread_returns_normalized
  }
  
  # Merge combination strategy into data table
  combinedStrategies <- rbindlist(combiList, idcol = "anomalyID")
  
  # Calculate the mean of Long and Short across all anomalies for each date as COMBI returns 
  combination <- combinedStrategies[, .(
    Long_COMBI = mean(Long_Return, na.rm = TRUE),
    Short_COMBI = mean(Short_Return, na.rm = TRUE)
  ), by = .(date)]
  
  # Calculate Spread_COMBI as the difference of Long_COMBI and Short_COMBI
  combination[, Spread_COMBI := Long_COMBI - Short_COMBI]
  
  strategies <- merge(strategies, combination, by = "date", all.x = TRUE)
  
  return(strategies)
}

FR_strategies <- createLongShortStrategies(FR_Data_long_ST, allAnomalies, inverseAnomalies)
fwrite(FR_strategies, "C:/Data/Strategies_SI_FF3M/AnomalyRet_10+1_Long_Short_Spread_FRA.csv")

################################################# Calculate the Excess Return ###########################################
FR_strategies <- fread("C:/Data/Strategies_SI_FF3M/AnomalyRet_10+1_Long_Short_Spread_FRA.csv")
setDT(FR_strategies)
FR_strategies[, date := as.character(date)]

FR_Merged_Data <- fread("C:/Data/Merged Data/Merged_FR_Data.csv") # include FF3M-factors and categorized Sentiment Index
FR_Merged_Data[, date := as.character(date)]

merged_FR_data <- merge(FR_Merged_Data, FR_strategies, by = "date", all = TRUE)
merged_FR_data <- merged_FR_data[!is.na(date)]

# Calculate excess returns under long strategies and short strategies

cols_to_adjust <- grep("^(Long_|Short_)", names(merged_FR_data), value = TRUE)

for (col in cols_to_adjust) {
  new_col_name <- paste0("ex_", col)
  merged_FR_data[, (new_col_name) := get(col) - RF]
}

# Calculate Spread of excess return under long-short strategies

long_cols <- grep("^ex_Long_", names(merged_FR_data), value = TRUE)
short_cols <- grep("^ex_Short_", names(merged_FR_data), value = TRUE)

for (i in seq_along(long_cols)) {
  spread_name <- sub("^ex_Long_", "ex_Spread_", long_cols[i])
  merged_FR_data[, (spread_name) := get(long_cols[i]) - get(short_cols[i])]
}

cols_to_remove <- grep("^(Long_|Short_|Spread_)", names(merged_FR_data), value = TRUE)
merged_FR_data[, (cols_to_remove) := NULL]

merged_FR_data <- merged_FR_data[,.(date, Mkt.RF, SMB, HML, RF,
                                    Stock_Index_Sentiment, HSENT,	LSENT,	
                                    Stock_Index_Buzz,	HATTEN,	LATTEN,	
                                    Stock_Index_Fear,	HFEAR, LFEAR,
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

fwrite(merged_FR_data, "C:/Data/Mispricing/merged_FR_data.csv")


###################################################### Statistical Analysis ####################################################
# Import the merged FF3M-factors, Sentiment Index and anomaly excess returns under long-short strategies
library(sandwich)
library(lmtest)
library(openxlsx)
library(tidyr)
library(dplyr)

data <- read.csv("C:/Data/Mispricing/merged_FR_data.csv", header = TRUE, sep = ",")  

# Transfer all units for benchmark factors and anomaly excess returns to percent 

data[, 2:5] <- data[, 2:5] * 100
data[, 15:47] <- data[, 15:47] * 100

# Categorize the anomaly excess return based on the strategies

long_cols <- names(data[,c(15,18,21,24,27,30,33,36,39,42,45)])  # Anomaly excess returns on the long leg
short_cols <- names(data[,c(16,19,22,25,28,31,34,37,40,43,46)])   # Anomaly excess returns on the short leg
spread_cols <- names(data[,c(17,20,23,26,29,32,35,38,41,44,47)])  # Anomaly excess returns spreads 

colx <- names(data[,15:47])  # Anomaly excess returns

################################################# Statistical Analysis ######################################################
# From Table 1 to Table 5
# All t-statistics based on White standard error/robust standard error               

###################################################### Table 1 ##############################################################
## Table 1 Panel A: Correlation matrix among the long-short benchmark-adjusted returns for 10 anomalies

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
formatted_lower_triangular <- matrix(as.character(lower_triangular), nrow = nrow(lower_triangular), byrow = TRUE)
formatted_lower_triangular[!is.na(lower_triangular)] <- sprintf("%0.2f", lower_triangular[!is.na(lower_triangular)])
formatted_lower_triangular[upper.tri(formatted_lower_triangular)] <- NA

## Table 1 Panel B: Average excess returns for 10 anomalies under long-short strategies

perform_t_test <- function(colx, label) {
  result1B <- data.frame(
    Anomaly = colx, 
    Mean = numeric(length(colx)), 
    t_statistic = numeric(length(colx)), 
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(colx)) {
    anomaly <- colx[i]
    data_subset <- data[[anomaly]][complete.cases(data[[anomaly]])]
    model1B <- lm(data_subset ~ 1)  # Only intercept
    robust_se <- sqrt(diag(vcovHC(model1B, type = "HC1")))  # Heteroscedasticity-consistent SE
    
    result1B[i, "Mean"] <- round(coef(model1B)[1], 2)
    t_value <- coef(model1B)[1] / robust_se
    result1B[i, "t_statistic"] <- round(t_value, 2)
  }
  
  return(result1B)
}

result1B_long <- perform_t_test(long_cols)
result1B_short <- perform_t_test(short_cols)
result1B_spread <- perform_t_test(spread_cols)

# Transpose each dataframe and convert to data frame
result1B_long_transposed <- as.data.frame(t(result1B_long))
result1B_short_transposed <- as.data.frame(t(result1B_short))
result1B_spread_transposed <- as.data.frame(t(result1B_spread))

# Combine and make the results tidy
result1B <- rbind(result1B_long_transposed, result1B_short_transposed, result1B_spread_transposed)
RowNames <- row.names(result1B)
result1B_cleaned <- result1B[!RowNames %in% c("Anomaly", "Anomaly1", "Anomaly2"), ]

# Define the desired order of the rows
desired_order <- c("Mean", "Mean1", "Mean2", "t_statistic", "t_statistic1", "t_statistic2")
result1B_cleaned$RowNames <- factor(row.names(result1B_cleaned), levels = desired_order)
result1B_cleaned <- result1B_cleaned[order(result1B_cleaned$RowNames), ]

## Table 1 Panel C: Average benchmark-adjusted returns for 11 anomalies under long-short strategies

perform_lm <- function(colx, label) {
  result1C <- data.frame(
    Anomaly = colx, 
    Estimate = numeric(length(colx)), 
    t_statistic = numeric(length(colx)), 
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(colx)) {
    model1C <- as.formula(paste0(colx[i], " ~ Mkt.RF + SMB + HML"))
    fit1C <- lm(model1C, data = data[complete.cases(data[, c(colx[i], "Mkt.RF", "SMB", "HML")]), ])
    
    robust_se <- sqrt(diag(vcovHC(fit1C, type = "HC1")))
    
    #cat("\nSummary for", colx[i], ":\n")
    #print(coeftest(fit1C, vcov = vcovHC(fit1C, type = "HC1")))
    
    result1C[i, "Estimate"] <- round(coef(fit1C)[1], 2)   # the intercept's estimate
    result1C[i, "t_statistic"] <- round(coef(fit1C)[1] / robust_se[1], 2)  # the intercept's t-statistic
  }
  
  return(result1C)
}

result1C_long <- perform_lm(long_cols)  # under long leg
result1C_short <- perform_lm(short_cols)  # under short leg
result1C_spread <- perform_lm(spread_cols)  # long minus short

# Transpose each dataframe and convert to data frame
result1C_long_transposed <- as.data.frame(t(result1C_long))
result1C_short_transposed <- as.data.frame(t(result1C_short))
result1C_spread_transposed <- as.data.frame(t(result1C_spread))

# Combine and make the results tidy
result1C <- rbind(result1C_long_transposed, result1C_short_transposed, result1C_spread_transposed)
RowNames <- row.names(result1C)
result1C_cleaned <- result1C[!RowNames %in% c("Anomaly", "Anomaly1", "Anomaly2"), ]

# Define the desired order of the rows
desired_order <- c("Estimate", "Estimate1", "Estimate2", "t_statistic", "t_statistic1", "t_statistic2")
result1C_cleaned$RowNames <- factor(row.names(result1C_cleaned), levels = desired_order)
result1C_cleaned <- result1C_cleaned[order(result1C_cleaned$RowNames), ]

# Export the Table 1 into an excel-file

wb <- createWorkbook()

addWorksheet(wb, "result1PanelA_FR")
addWorksheet(wb, "result1PanelB_FR")
addWorksheet(wb, "result1PanelC_FR")

writeData(wb, "result1PanelA_FR", formatted_lower_triangular, withFilter = FALSE)
writeData(wb, "result1PanelB_FR", result1B_cleaned, withFilter = FALSE)
writeData(wb, "result1PanelC_FR", result1C_cleaned, withFilter = FALSE)

saveWorkbook(wb, "C:/Data/Result_Excel/result1_FR.xlsx", overwrite = TRUE)

###################################################### Table 2 ##############################################################
# Average excess returns for 10 anomalies under long-short strategies based on two sentiment categories           

perform_t_test <- function(colx) {  
  result2 <- matrix(NA, nrow = 2 * length(colx), ncol = 9)
  colnames(result2) <- c("HSENT_Mean", "LSENT_Mean", "Diff_Mean_SENT", 
                         "HATTEN_Mean", "LATTEN_Mean", "Diff_Mean_ATTEN", 
                         "HFEAR_Mean", "LFEAR_Mean", "Diff_Mean_FEAR")
  
  for (i in seq_along(colx)) {
    
    # Calculate for HSENT
    tmp_hsent <- na.omit(data[data$HSENT == 1, colx[i]])
    mean_hsent <- mean(tmp_hsent)
    model_hsent <- lm(tmp_hsent ~ 1)
    robust_se_hsent <- sqrt(diag(vcovHC(model_hsent, type = "HC1")))
    t_statistic_hsent <- sprintf("(%0.2f)", round(coef(model_hsent) / robust_se_hsent, 2))
    
    # Calculate for LSENT
    tmp_lsent <- na.omit(data[data$LSENT == 1, colx[i]])
    mean_lsent <- mean(tmp_lsent)
    model_lsent <- lm(tmp_lsent ~ 1)
    robust_se_lsent <- sqrt(diag(vcovHC(model_lsent, type = "HC1")))
    t_statistic_lsent <- sprintf("(%0.2f)", round(coef(model_lsent) / robust_se_lsent, 2))
    
    # Assuming sample independence for sentiment comparisons
    # High - Low
    diff_mean_sent <- mean_hsent - mean_lsent
    combined_se_sent <- sqrt(robust_se_hsent^2 + robust_se_lsent^2)
    t_statistic_diff_sent <- sprintf("(%0.2f)", round(diff_mean_sent / combined_se_sent, 2))
    mean_hsent <- sprintf("%0.2f", round(mean_hsent, 2))
    mean_lsent <- sprintf("%0.2f", round(mean_lsent, 2))
    diff_mean_sent <- sprintf("%0.2f", round(diff_mean_sent, 2))
    
    # Calculate for HATTEN
    tmp_hatten <- na.omit(data[data$HATTEN == 1, colx[i]])
    mean_hatten <- mean(tmp_hatten)
    model_hatten <- lm(tmp_hatten ~ 1)
    robust_se_hatten <- sqrt(diag(vcovHC(model_hatten, type = "HC1")))
    t_statistic_hatten <- sprintf("(%0.2f)", round(coef(model_hatten) / robust_se_hatten, 2))
    
    # Calculate for LATTEN
    tmp_latten <- na.omit(data[data$LATTEN == 1, colx[i]])
    mean_latten <- mean(tmp_latten)
    model_latten <- lm(tmp_latten ~ 1)
    robust_se_latten <- sqrt(diag(vcovHC(model_latten, type = "HC1")))
    t_statistic_latten <- sprintf("(%0.2f)", round(coef(model_latten) / robust_se_latten, 2))
    
    # High - Low
    diff_mean_atten <- mean_hatten - mean_latten
    combined_se_atten <- sqrt(robust_se_hatten^2 + robust_se_latten^2)
    t_statistic_diff_atten <- sprintf("(%0.2f)", round(diff_mean_atten / combined_se_atten, 2))
    mean_hatten <- sprintf("%0.2f", round(mean_hatten, 2))
    mean_latten <- sprintf("%0.2f", round(mean_latten, 2))
    diff_mean_atten <- sprintf("%0.2f", round(diff_mean_atten, 2))
    
    # Calculate for HFEAR
    tmp_hfear <- na.omit(data[data$HFEAR == 1, colx[i]])
    mean_hfear <- mean(tmp_hfear)
    model_hfear <- lm(tmp_hfear ~ 1)
    robust_se_hfear <- sqrt(diag(vcovHC(model_hfear, type = "HC1")))
    t_statistic_hfear <- sprintf("(%0.2f)", round(coef(model_hfear) / robust_se_hfear, 2))
    
    # Calculate for LFEAR
    tmp_lfear <- na.omit(data[data$LFEAR == 1, colx[i]])
    mean_lfear <- mean(tmp_lfear)
    model_lfear <- lm(tmp_lfear ~ 1)
    robust_se_lfear <- sqrt(diag(vcovHC(model_lfear, type = "HC1")))
    t_statistic_lfear <- sprintf("(%0.2f)", round(coef(model_lfear) / robust_se_lfear, 2))
    
    # High - low
    diff_mean_fear <- mean_hfear - mean_lfear
    combined_se_fear <- sqrt(robust_se_hfear^2 + robust_se_lfear^2)
    t_statistic_diff_fear <- sprintf("(%0.2f)", round(diff_mean_fear / combined_se_fear, 2))
    mean_hfear <- sprintf("%0.2f", round(mean_hfear, 2))
    mean_lfear <- sprintf("%0.2f", round(mean_lfear, 2))
    diff_mean_fear <- sprintf("%0.2f", round(diff_mean_fear, 2))
    
    # Add estimates to the matrix (odd rows)
    result2[2 * i - 1, ] <- c(mean_hsent, mean_lsent, diff_mean_sent, mean_hatten, mean_latten, diff_mean_atten, mean_hfear, mean_lfear, diff_mean_fear)
    # Add t-statistics to the matrix (even rows)
    result2[2 * i, ] <- c(t_statistic_hsent, t_statistic_lsent, t_statistic_diff_sent, t_statistic_hatten, t_statistic_latten, t_statistic_diff_atten, t_statistic_hfear, t_statistic_lfear, t_statistic_diff_fear)
    
    rownames(result2) <- rep(colx, each = 2) # Assign row names for each estimate/t-statistic combination
    
  }
  return(result2)
}

result2_long <- perform_t_test(long_cols)  # under long leg
result2_short <- perform_t_test(short_cols)   # under short leg
result2_spread <- perform_t_test(spread_cols)   # long minus short

wb <- createWorkbook()

addWorksheet(wb, "result2_long_FR")
addWorksheet(wb, "result2_short_FR")
addWorksheet(wb, "result2_spread_FR")

writeData(wb, "result2_long_FR", result2_long, withFilter = FALSE)
writeData(wb, "result2_short_FR", result2_short, withFilter = FALSE)
writeData(wb, "result2_spread_FR", result2_spread, withFilter = FALSE)

saveWorkbook(wb, "C:/Data/Result_Excel/result2_FR.xlsx", overwrite = TRUE)


###################################################### Table 3 ##############################################################
## Average benchmark-adjusted returns for 10 anomalies under long-short strategies based on two sentiment categories

perform_lm <- function(colx) {
  result3 <- matrix(NA, nrow = 2 * length(colx), ncol = 9)
  colnames(result3) <- c("Estimate_HSENT", "Estimate_LSENT", "Diff_Estimate_SENT", 
                         "Estimate_HATTEN", "Estimate_LATTEN", "Diff_Estimate_ATTEN", 
                         "Estimate_HFEAR", "Estimate_LFEAR", "Diff_Estimate_FEAR")
  
  for (i in seq_along(colx)) {
    anomaly <- colx[i]
    
    # Stock Index Sentiment
    
    model_SENT <- as.formula(paste0(anomaly, " ~ HSENT + LSENT + Mkt.RF + SMB + HML - 1")) # Omit the intercept: direct effect of sentiment variable
    
    fit_SENT <- lm(model_SENT, data = data[complete.cases(data[, c(anomaly, "HSENT", "LSENT", "Mkt.RF", "SMB", "HML")]), ])
    
    robust_se_SENT <- sqrt(diag(vcovHC(fit_SENT, type = "HC1")))
    
    # Critical t-values calculation
    #n_sent = nrow(data[complete.cases(data[, c(anomaly, "HSENT", "LSENT", "Mkt.RF", "SMB", "HML")]), ])
    #df_sent = n_sent - 5  # Number of predictors including HSENT, LSENT, Mkt.RF, SMB, HML, minus intercept
    #critical_t_sent = qt(0.95, df_sent)  # One-sided test
    
    estimate_HSENT <- sprintf("%0.2f", round(coef(fit_SENT)["HSENT"], 2))
    t_statistic_HSENT <- sprintf("(%0.2f)", round(coef(fit_SENT)["HSENT"] / robust_se_SENT["HSENT"], 2))
    
    estimate_LSENT <- sprintf("%0.2f", round(coef(fit_SENT)["LSENT"], 2))
    t_statistic_LSENT <- sprintf("(%0.2f)", round(coef(fit_SENT)["LSENT"] / robust_se_SENT["LSENT"], 2))
    
    # High - low
    diff_estimate_SENT <- coef(fit_SENT)["HSENT"] - coef(fit_SENT)["LSENT"]
    combined_se_SENT <- sqrt(robust_se_SENT["HSENT"]^2 + robust_se_SENT["LSENT"]^2)
    diff_t_statistic_SENT <- sprintf("(%0.2f)", round(diff_estimate_SENT / combined_se_SENT, 2))
    diff_estimate_SENT <- sprintf("%0.2f", round(diff_estimate_SENT, 2)) 
    
    # Stock Index Buzz
    
    model_ATTEN <- as.formula(paste0(anomaly, " ~ HATTEN + LATTEN + Mkt.RF + SMB + HML - 1")) 
    
    fit_ATTEN <- lm(model_ATTEN, data = data[complete.cases(data[, c(anomaly, "HATTEN", "LATTEN", "Mkt.RF", "SMB", "HML")]), ])
    
    robust_se_ATTEN <- sqrt(diag(vcovHC(fit_ATTEN, type = "HC1")))
    
    estimate_HATTEN <- sprintf("%0.2f", round(coef(fit_ATTEN)["HATTEN"], 2))
    t_statistic_HATTEN <- sprintf("(%0.2f)", round(coef(fit_ATTEN)["HATTEN"] / robust_se_ATTEN["HATTEN"], 2))
    
    estimate_LATTEN <- sprintf("%0.2f", round(coef(fit_ATTEN)["LATTEN"], 2))
    t_statistic_LATTEN <- sprintf("(%0.2f)", round(coef(fit_ATTEN)["LATTEN"] / robust_se_ATTEN["LATTEN"], 2))
    
    # High - low
    diff_estimate_ATTEN <- coef(fit_ATTEN)["HATTEN"] - coef(fit_ATTEN)["LATTEN"]
    combined_se_ATTEN <- sqrt(robust_se_ATTEN["HATTEN"]^2 + robust_se_ATTEN["LATTEN"]^2)
    diff_t_statistic_ATTEN <- sprintf("(%0.2f)", round(diff_estimate_ATTEN / combined_se_ATTEN, 2))
    diff_estimate_ATTEN <- sprintf("%0.2f", round(diff_estimate_ATTEN, 2)) 
    
    # Stock Index Fear
    
    model_FEAR <- as.formula(paste0(anomaly, " ~ HFEAR + LFEAR + Mkt.RF + SMB + HML - 1")) 
    
    fit_FEAR <- lm(model_FEAR, data = data[complete.cases(data[, c(anomaly, "HFEAR", "LFEAR", "Mkt.RF", "SMB", "HML")]), ])
    
    robust_se_FEAR <- sqrt(diag(vcovHC(fit_FEAR, type = "HC1")))
    
    estimate_HFEAR <- sprintf("%0.2f", round(coef(fit_FEAR)["HFEAR"], 2))
    t_statistic_HFEAR <- sprintf("(%0.2f)", round(coef(fit_FEAR)["HFEAR"] / robust_se_FEAR["HFEAR"], 2))
    
    estimate_LFEAR <- sprintf("%0.2f", round(coef(fit_FEAR)["LFEAR"], 2))
    t_statistic_LFEAR <- sprintf("(%0.2f)", round(coef(fit_FEAR)["LFEAR"] / robust_se_FEAR["LFEAR"], 2))
    
    # High - low
    diff_estimate_FEAR <- coef(fit_FEAR)["HFEAR"] - coef(fit_FEAR)["LFEAR"]
    combined_se_FEAR <- sqrt(robust_se_FEAR["HFEAR"]^2 + robust_se_FEAR["LFEAR"]^2)
    diff_t_statistic_FEAR <- sprintf("(%0.2f)", round(diff_estimate_FEAR / combined_se_FEAR, 2))
    diff_estimate_FEAR <- sprintf("%0.2f", round(diff_estimate_FEAR, 2)) 
    
    # Add estimates to the matrix (odd rows)
    result3[2 * i - 1, ] <- c(estimate_HSENT, estimate_LSENT, diff_estimate_SENT, estimate_HATTEN, estimate_LATTEN, diff_estimate_ATTEN, estimate_HFEAR, estimate_LFEAR, diff_estimate_FEAR)
    # Add t-statistics to the matrix (even rows)
    result3[2 * i, ] <- c(t_statistic_HSENT, t_statistic_LSENT, diff_t_statistic_SENT, t_statistic_HATTEN, t_statistic_LATTEN, diff_t_statistic_ATTEN, t_statistic_HFEAR, t_statistic_LFEAR, diff_t_statistic_FEAR)
    
    rownames(result3) <- rep(colx, each = 2) # Assign row names for each estimate/t-statistic combination
    
  }
  return(result3)
}

result3_long <- perform_lm(long_cols)  # under long leg
result3_short <- perform_lm(short_cols)  # under short leg
result3_spread <- perform_lm(spread_cols)  # long minus short

wb <- createWorkbook()

addWorksheet(wb, "result3_long_FR")
addWorksheet(wb, "result3_short_FR")
addWorksheet(wb, "result3_spread_FR")

writeData(wb, "result3_long_FR", result3_long, withFilter = FALSE)
writeData(wb, "result3_short_FR", result3_short, withFilter = FALSE)
writeData(wb, "result3_spread_FR", result3_spread, withFilter = FALSE)

saveWorkbook(wb, "C:/Data/Result_Excel/result3_FR.xlsx", overwrite = TRUE)


###################################################### Table 4 ##############################################################
# Regressing excess returns under long-short strategies on the lagged sentiment index

# Add lagged sentiment indexes
data$LAG_SENT <- lag(data$Stock_Index_Sentiment, 1)
data$LAG_ATTEN <- lag(data$Stock_Index_Buzz, 1)
data$LAG_FEAR <- lag(data$Stock_Index_Fear, 1)

perform_lm <- function(colx) {
  # Initialize the matrix to store results (3 columns only)
  result4 <- matrix(NA, nrow = 2 * length(colx), ncol = 3)
  colnames(result4) <- c("Estimate_LAGSENT", "Estimate_LAGATTEN", "Estimate_LAGFEAR")
  
  for (i in seq_along(colx)) {
    # Stock Index Sentiment
    model_LAGSENT <- as.formula(paste0(colx[i], " ~ LAG_SENT"))
    fit_LAGSENT <- lm(model_LAGSENT, data = data[complete.cases(data[, c(colx[i], "LAG_SENT")]), ])
    robust_se_LAGSENT <- sqrt(diag(vcovHC(fit_LAGSENT, type = "HC1")))
    estimate_LAGSENT <- sprintf("%0.2f", round(coef(fit_LAGSENT)["LAG_SENT"], 2))
    t_statistic_LAGSENT <- sprintf("(%0.2f)", round(coef(fit_LAGSENT)["LAG_SENT"] / robust_se_LAGSENT["LAG_SENT"], 2))
    
    # Stock Index Buzz
    model_LAGATTEN <- as.formula(paste0(colx[i], " ~ LAG_ATTEN"))
    fit_LAGATTEN <- lm(model_LAGATTEN, data = data[complete.cases(data[, c(colx[i], "LAG_ATTEN")]), ])
    robust_se_LAGATTEN <- sqrt(diag(vcovHC(fit_LAGATTEN, type = "HC1")))
    estimate_LAGATTEN <- sprintf("%0.2f", round(coef(fit_LAGATTEN)["LAG_ATTEN"], 2))
    t_statistic_LAGATTEN <- sprintf("(%0.2f)", round(coef(fit_LAGATTEN)["LAG_ATTEN"] / robust_se_LAGATTEN["LAG_ATTEN"], 2))
    
    # Stock Index Fear
    model_LAGFEAR <- as.formula(paste0(colx[i], " ~ LAG_FEAR"))
    fit_LAGFEAR <- lm(model_LAGFEAR, data = data[complete.cases(data[, c(colx[i], "LAG_FEAR")]), ])
    robust_se_LAGFEAR <- sqrt(diag(vcovHC(fit_LAGFEAR, type = "HC1")))
    estimate_LAGFEAR <- sprintf("%0.2f", round(coef(fit_LAGFEAR)["LAG_FEAR"], 2))
    t_statistic_LAGFEAR <- sprintf("(%0.2f)", round(coef(fit_LAGFEAR)["LAG_FEAR"] / robust_se_LAGFEAR["LAG_FEAR"], 2))
    
    # Add estimates to the matrix (odd rows)
    result4[2 * i - 1, ] <- c(estimate_LAGSENT, estimate_LAGATTEN, estimate_LAGFEAR)
    # Add t-statistics to the matrix (even rows)
    result4[2 * i, ] <- c(t_statistic_LAGSENT, t_statistic_LAGATTEN, t_statistic_LAGFEAR)
  }
  
  rownames(result4) <- rep(colx, each = 2) # Assign row names for each estimate/t-statistic combination
  
  return(result4)
}

result4_long <- perform_lm(long_cols)  # under long leg
result4_short <- perform_lm(short_cols)  # under short leg
result4_spread <- perform_lm(spread_cols)

wb <- createWorkbook()

addWorksheet(wb, "result4_long_FR")
addWorksheet(wb, "result4_short_FR")
addWorksheet(wb, "result4_spread_FR")

writeData(wb, "result4_long_FR", result4_long, withFilter = FALSE)
writeData(wb, "result4_short_FR", result4_short, withFilter = FALSE)
writeData(wb, "result4_spread_FR", result4_spread, withFilter = FALSE)

saveWorkbook(wb, "C:/Data/Result_Excel/result4_FR.xlsx", overwrite = TRUE)

###################################################### Table 5 ##############################################################
# Regressing benchmark-adjusted returns under long-short strategies on the lagged sentiment index

perform_lm <- function(colx) {
  
  result5 <- matrix(NA, nrow = 2 * length(colx), ncol = 3)
  colnames(result5) <- c("Estimate_LAGSENT", "Estimate_LAGATTEN", "Estimate_LAGFEAR")
  
  for (i in seq_along(colx)) {
    
    # Stock Index Sentiment
    
    model_LAGSENT <- as.formula(paste0(colx[i], " ~ LAG_SENT + Mkt.RF + SMB + HML"))
    fit_LAGSENT <- lm(model_LAGSENT, data = data[complete.cases(data[, c(colx[i], "LAG_SENT", "Mkt.RF" , "SMB" , "HML")]), ])
    
    #df_LAGSENT <- df.residual(fit_LAGSENT)
    #critical_t_LAGSENT <- qt(0.95, df_LAGSENT)
    
    robust_se_LAGSENT <- sqrt(diag(vcovHC(fit_LAGSENT, type = "HC1")))
    
    #cat("\nSummary for", colx[i], ":\n")
    #print(coeftest(fit_LAGSENT, vcov = vcovHC(fit_LAGSENT, type = "HC1")))
    
    estimate_LAGSENT <- sprintf("%0.2f", round(coef(fit_LAGSENT)["LAG_SENT"], 2))
    t_statistic_LAGSENT <- sprintf("(%0.2f)", round(coef(fit_LAGSENT)["LAG_SENT"] / robust_se_LAGSENT["LAG_SENT"], 2))
    
    #result5[i, "Critical_t_LAGSENT"] <- round(critical_t_LAGSENT, 2)
    #result5[i, "df_LAGSENT"] <- df_LAGSENT
    
    # Stock Index Buzz
    
    model_LAGATTEN <- as.formula(paste0(colx[i], " ~ LAG_ATTEN + Mkt.RF + SMB + HML"))
    fit_LAGATTEN <- lm(model_LAGATTEN, data = data[complete.cases(data[, c(colx[i], "LAG_ATTEN", "Mkt.RF" , "SMB" , "HML")]), ])
    
    robust_se_LAGATTEN <- sqrt(diag(vcovHC(fit_LAGATTEN, type = "HC1")))
    
    #cat("\nSummary for", colx[i], ":\n")
    #print(coeftest(fit_LAGATTEN, vcov = vcovHC(fit_LAGATTEN, type = "HC1")))
    
    estimate_LAGATTEN <- sprintf("%0.2f", round(coef(fit_LAGATTEN)["LAG_ATTEN"], 2))
    t_statistic_LAGATTEN <- sprintf("(%0.2f)", round(coef(fit_LAGATTEN)["LAG_ATTEN"] / robust_se_LAGATTEN["LAG_ATTEN"], 2))
    
    # Stock Index Fear
    
    model_LAGFEAR <- as.formula(paste0(colx[i], " ~ LAG_FEAR + Mkt.RF + SMB + HML"))
    fit_LAGFEAR <- lm(model_LAGFEAR, data = data[complete.cases(data[, c(colx[i], "LAG_FEAR", "Mkt.RF" , "SMB" , "HML")]), ])
    
    robust_se_LAGFEAR <- sqrt(diag(vcovHC(fit_LAGFEAR, type = "HC1")))
    
    #cat("\nSummary for", colx[i], ":\n")
    #print(coeftest(fit_LAGFEAR, vcov = vcovHC(fit_LAGFEAR, type = "HC1")))
    
    estimate_LAGFEAR <- sprintf("%0.2f", round(coef(fit_LAGFEAR)["LAG_FEAR"], 2))
    t_statistic_LAGFEAR <- sprintf("(%0.2f)", round(coef(fit_LAGFEAR)["LAG_FEAR"] / robust_se_LAGFEAR["LAG_FEAR"], 2))
    
    # Assign estimates and t-statistics to the result matrix
    result5[2 * i - 1, ] <- c(estimate_LAGSENT, estimate_LAGATTEN, estimate_LAGFEAR)
    result5[2 * i, ] <- c(t_statistic_LAGSENT, t_statistic_LAGATTEN, t_statistic_LAGFEAR)
  }
  
  rownames(result5) <- rep(colx, each = 2)
  
  return(result5)
}

result5_long <- perform_lm(long_cols)  # under long leg
result5_short <- perform_lm(short_cols)  # under short leg
result5_spread <- perform_lm(spread_cols)  # long minus short

wb <- createWorkbook()

addWorksheet(wb, "result5_long_FR")
addWorksheet(wb, "result5_short_FR")
addWorksheet(wb, "result5_spread_FR")

writeData(wb, "result5_long_FR", result5_long, withFilter = FALSE)
writeData(wb, "result5_short_FR", result5_short, withFilter = FALSE)
writeData(wb, "result5_spread_FR", result5_spread, withFilter = FALSE)

saveWorkbook(wb, "C:/Data/Result_Excel/result5_FR.xlsx", overwrite = TRUE)


#rm(list = ls())  
