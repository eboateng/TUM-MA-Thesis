#############################################################################################################################

# Construct long-short strategies and calculate the anomaly excess returns

# Output: XX_strategies

########################################################### Set Up ##########################################################
library(dplyr)
library(data.table)

load("C:/Data/Anomalies/CA_Data_long_ST.RData")
load("C:/Data/Anomalies/DE_Data_long_ST.RData")
load("C:/Data/Anomalies/FR_Data_long_ST.RData")
load("C:/Data/Anomalies/GB_Data_long_ST.RData")
load("C:/Data/Anomalies/IT_Data_long_ST.RData")
load("C:/Data/Anomalies/JP_Data_long_ST.RData")       
load("C:/Data/Anomalies/US_Data_long_ST.RData")
load("C:/Data/Anomalies/AGGR_Data_long_ST.RData")

################################################# Create Long and Short strategies ###########################################

# Calculate long, short, and spread returns for a given anomaly within a country's dataset:
# Obtain the value-weighted returns within each decile of the anomaly's sorting variable 
# and then construct long-short strategies using the extreme deciles (1st and 10th)
# Construct a combination strategy that takes equal positions across the long-short strategies constructed for each ym group
## anomaly -> ym group -> 1st and 10th deciles group

anomalyVariables <- c("GPtoAsset", "ROA", "AssetGrowth", "NetOperatingAssets", "Accruals", "InvestmentToAsset", 
                      "NetStockIssues", "CompositeEquity_Log", "MomentumReturn", "O_Score")

createLongShortStrategies <- function(data_long, anomalyVariables) {
  setDT(data_long)
  
  # Initialize the data table to store the a list for individual anomaly strategies and combined results
  strategies <- data.table(ym = unique(data_long$ym))
  combiList <- list()
  
  for (anomalyVariable in anomalyVariables) {
    
    # Calculates the 10th and 90th percentiles within each ym for each anomaly and assign the 1st and 10th deciles accordingly
    
    data_long[, c("decile", "p10", "p90") := {
      p10 <- quantile(get(anomalyVariable), probs = 0.1, na.rm = TRUE)
      p90 <- quantile(get(anomalyVariable), probs = 0.9, na.rm = TRUE)
      
      decile <- fifelse(get(anomalyVariable) <= p10, 1L,  # assign the 1st decile
                fifelse(get(anomalyVariable) >= p90, 10L, # assign the 10th decile
                        NA_integer_)) # assign NA to those that do not fall into either of the deciles
      .(decile, p10, p90)
    }, by = ym, .SDcols = anomalyVariable]
    
    # Calculate the value-weighted returns for extreme deciles under long-short strategies
    spread_returns <- data_long[decile %in% c(1, 10), .(
                      #sum(RET_USD * (LagMV_USD / sum(LagMV_USD, na.rm = TRUE))
    # Market.RET_USD = sum(RET_USD * (LagMV_USD / LagTotal.MV_USD))
      #Long_Return = sum(RET_USD * (LagMV_USD / LagTotal.MV_USD)* (decile == 10), na.rm = TRUE), # only rows of the 10th decile contribute to the sum
      #Short_Return = sum(RET_USD * (LagMV_USD / LagTotal.MV_USD) * (decile == 1), na.rm = TRUE) # only rows of the 1st decile contribute to the sum
      Long_Return = weighted.mean(RET_USD[decile == 10], LagMV_USD[decile == 10], na.rm = TRUE), 
      Short_Return = weighted.mean(RET_USD[decile == 1], LagMV_USD[decile == 1], na.rm = TRUE)   
    ), by = ym]
    spread_returns[, Spread := Long_Return - Short_Return] # Spread
    
    # Store the results with the tidy names
    colNames <- c(sprintf("Long_%s", anomalyVariable), sprintf("Short_%s", anomalyVariable), sprintf("Spread_%s", anomalyVariable))
    spread_returns[, (colNames) := .(Long_Return, Short_Return, Spread)]
    strategies <- merge(strategies, spread_returns[, .(ym, Long_Return, Short_Return, Spread)], by = "ym", all.x = TRUE, suffixes = c("", paste0("_", anomalyVariable)))
    setnames(strategies, old = c("Long_Return", "Short_Return", "Spread"), new = colNames)
    
    # Add to combiList for combination strategy calculation
    spread_returns_normalized <- spread_returns[, .(ym, Long_Return, Short_Return, Spread)]
    combiList[[anomalyVariable]] <- spread_returns_normalized
  }
  
  # Calculate the combination strategy and merge into data table
  
  # Combine all strategies into one data table
  combinedStrategies <- rbindlist(combiList, idcol = "anomalyID")
  
  # Calculate the mean of Long, Short, and Spread across all anomalies for each ym as COMBI returns 
  combination <- combinedStrategies[, .(
    Long_COMBI = mean(Long_Return, na.rm = TRUE),
    Short_COMBI = mean(Short_Return, na.rm = TRUE),
    Spread_COMBI = mean(Spread, na.rm = TRUE)
  ), by = .(ym)]
  
  strategies <- merge(strategies, combination, by = "ym", all.x = TRUE)
  
  # Add date (yyyymm) as the second column
  strategies[, date := format(ym, "%Y%m")]
  
  cols <- colnames(strategies)
  setcolorder(strategies, c("ym", "date", cols[!cols %in% c("ym", "date")]))
  
  return(strategies)
}

CA_strategies <- createLongShortStrategies(CA_Data_long_ST, anomalyVariables)
DE_strategies <- createLongShortStrategies(DE_Data_long_ST, anomalyVariables)
FR_strategies <- createLongShortStrategies(FR_Data_long_ST, anomalyVariables)
GB_strategies <- createLongShortStrategies(GB_Data_long_ST, anomalyVariables)
IT_strategies <- createLongShortStrategies(IT_Data_long_ST, anomalyVariables)
JP_strategies <- createLongShortStrategies(JP_Data_long_ST, anomalyVariables)
US_strategies <- createLongShortStrategies(US_Data_long_ST, anomalyVariables)

AGGR_strategies <- createLongShortStrategies(AGGR_Data_long_ST, anomalyVariables)

######################################### Calculate the Excess Anomaly Returns ###############################################
# Calculate excess returns under long- and short strategies (= Anomaly Return - RF)

load("C:/Data/Final Data/RiskFreeRate.RData")
RiskFreeRate <- as.data.table(RiskFreeRate)

# Align the column name and type in RiskFreeRate to match those in strategies for merging
setnames(RiskFreeRate, "ym", "date")
RiskFreeRate[, date := as.character(date)]
  
calculateExcessAnomalyReturns <- function(strategies, RiskFreeRate) {
  
  setDT(strategies)
  setDT(RiskFreeRate)
 
  strategies <- merge(strategies, RiskFreeRate, by = "date", all.x = TRUE)
  
  # Define the columns to be further calculated
  originalCols <- c("Long_", "Short_", "Spread_")
  combinationCols <- c("Long_COMBI", "Short_COMBI", "Spread_COMBI")
  
  # Calculate excess returns for each anomaly variable
  for (anomalyVariable in anomalyVariables) {
    
    for (colPrefix in originalCols) {
      originalColName <- paste0(colPrefix, anomalyVariable)
      excessColName <- paste0("ex_", originalColName)
      
      strategies[, (excessColName) := get(originalColName) - RF]
    }
  }
  
  # Calculate excess returns for combination strategy
  for (combCol in combinationCols) {
    
    excessCombColName <- paste0("ex_", combCol)
    
    strategies[, (excessCombColName) := get(combCol) - RF]
  }
  
  strategies[, RF := NULL]
  
  return(strategies)
}

CA_strategies <- calculateExcessAnomalyReturns(CA_strategies, RiskFreeRate)
DE_strategies <- calculateExcessAnomalyReturns(DE_strategies, RiskFreeRate)
FR_strategies <- calculateExcessAnomalyReturns(FR_strategies, RiskFreeRate)
GB_strategies <- calculateExcessAnomalyReturns(GB_strategies, RiskFreeRate)
IT_strategies <- calculateExcessAnomalyReturns(IT_strategies, RiskFreeRate)
JP_strategies <- calculateExcessAnomalyReturns(JP_strategies, RiskFreeRate)
US_strategies <- calculateExcessAnomalyReturns(US_strategies, RiskFreeRate)

AGGR_strategies <- calculateExcessAnomalyReturns(AGGR_strategies, RiskFreeRate)


############################################### Saving Country Specific #####################################################
# Save the final results in Excel as separate work sheet

library(openxlsx)
setwd("C:/Data/Strategies_SI_FF3M")

# Create a new workbook
wb <- createWorkbook() 

# Add each country's strategies as a new worksheet

## Canada
addWorksheet(wb, "CA")
writeData(wb, "CA", CA_strategies)

## Germany
addWorksheet(wb, "DE")
writeData(wb, "DE", DE_strategies)

## France
addWorksheet(wb, "FR")
writeData(wb, "FR", FR_strategies)

## United Kingdom
addWorksheet(wb, "GB")
writeData(wb, "GB", GB_strategies)

## Italy
addWorksheet(wb, "IT")
writeData(wb, "IT", IT_strategies)

## Japan
addWorksheet(wb, "JP")
writeData(wb, "JP", JP_strategies)

## United States
addWorksheet(wb, "US")
writeData(wb, "US", US_strategies)

# Save the workbook
AnomalyRet <- "AnomalyRet_10+1_Long_Short_Spread.xlsx"
saveWorkbook(wb, AnomalyRet, overwrite = TRUE)

cat("Excel file saved as", AnomalyRet)

########################################### Merge all Data for Country Specifically #########################################
# Import the documents which includes FF3M-factors, Sentiment Indexes and merge with anomaly excess returns under long-short strategies

library(readxl)
library(openxlsx)

anomaly_file <- "C:/Data/Strategies_SI_FF3M/AnomalyRet_10+1_Long_Short_Spread.xlsx"
SI_FF3M_file <- "C:/Data/Strategies_SI_FF3M/Sentiment_Indexes_FF3M.xlsx"
SI_FF3M_data <- read_excel(SI_FF3M_file)

# Read a specific sheet from anomaly_file and merge with sentiment index and FF3M data

merge_anomaly_with_sentiment_FF3M <- function(sentiment_FF3M_data, sheet_name) {
  
  anomaly_data <- read_excel(anomaly_file, sheet = sheet_name) %>%
    select(date, starts_with("ex_")) # Select the date column and columns that start with "ex_"
  
  merged_data <- merge(sentiment_FF3M_data, anomaly_data, by = "date", all.x = TRUE)
  return(merged_data)
}

sheet_names <- c("CA", "DE", "FR", "GB", "IT", "JP", "US")

merged_data_list <- list()

for(sheet in sheet_names) {
  merged_data_list[[sheet]] <- merge_anomaly_with_sentiment_FF3M(SI_FF3M_data, sheet)
}

# Save as Excel
for(sheet in sheet_names) {
  write.xlsx(merged_data_list[[sheet]], file = paste0("C:/Data/Merged Data/Merged_", sheet, "_Data.xlsx"))
} 

# Save as csv
for(sheet in sheet_names) {
  write.csv(merged_data_list[[sheet]], file = paste0("C:/Data/Merged Data/Merged_", sheet, "_Data.csv"), row.names = FALSE)
}

#rm(list = ls())  








