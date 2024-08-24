#############################################################################################################################

# Data preparation and cleaning

# Output: XX_Acc_Reduced and XX_Return_DS05_DS07_DS08

########################################################### Set Up ##########################################################

# Required packages
library(data.table)
library(dplyr)
library(dtplyr)
library(tidyr)
#install.packages("DataCombine")
library(DataCombine)
library(zoo)

# Loading Datasets
#load("C:/Data/Final Data/WS_yearly_data.RData")
#load("C:/Data/Final Data/DS_monthly_data.RData")
ws_yearly_data <- fread("C:/Data/Final Data/ws_yearly_data.csv")
ds_monthly_data <- fread("C:/Data/Final Data/ds_monthly_data.csv")
ds_monthly_data[, ym := as.yearmon(ym)]

################################################ Preliminary Static Screens #################################################
# Remain firms (dscode) that are present in both the accounting and market datasets with matching regions (the same located region and listed region)

# Reduce accounting data set to only include companies with valid mapping between two data sets
ws_yearly_data_reduced <- ws_yearly_data[!dscode %in% c('NA in link_data', "date error in link_data", 'NA in monthly_data', "date error in monthly_data")]

# Among firms with valid mapping in both data sets only keep the companies for which both market and accounting information is available

yearly_dscode_unique <- unique(ws_yearly_data_reduced[, .(dscode)])
ds_monthly_data_reduced <- ds_monthly_data[dscode %in% yearly_dscode_unique$dscode] # in market dataset keep firms also present in accounting dataset

monthly_dscode_unique <- unique(ds_monthly_data_reduced[, .(dscode)])
ws_yearly_data_reduced_filtered <- ws_yearly_data_reduced[dscode %in% monthly_dscode_unique$dscode] # in accounting dataset keep firms remaining in market dataset

# If any, exclude the companies whose accounting region does not correspond with the market region

yearly_region_unique <- unique(ws_yearly_data_reduced_filtered[, .(dscode, region)], by = c("dscode", "region"))
monthly_region_unique <- unique(ds_monthly_data_reduced[, .(dscode, region)], by = c("dscode", "region"))

setkey(yearly_region_unique, dscode)
setkey(monthly_region_unique, dscode)
joined_region <- merge(yearly_region_unique, monthly_region_unique, by = "dscode", all = TRUE, suffixes = c("_yearly", "_monthly"))

discrepancies <- joined_region[region_yearly != region_monthly, .(dscode, region_yearly, region_monthly)] # Filter to find discrepancies (where regions do not match)
dscode_to_remove <- discrepancies$dscode

ws_yearly_data_reduced_filtered_final <- ws_yearly_data_reduced_filtered[!dscode %in% dscode_to_remove] # Remove the corresponding dscode rows from accounting data set
ds_monthly_data_reduced_filtered <- ds_monthly_data_reduced[!dscode %in% dscode_to_remove] # Remove the corresponding dscode rows from market data set

################################################ Additional Static Screens ##################################################

# ASS2: Filter monthly data to exclude return data (RET_USD) with NA
DS_monthly_data_Clean <- ds_monthly_data_reduced_filtered[is.na(RET_USD) == FALSE]

##################################################### Dynamic Screens #######################################################

# Schmidt et al. Arx (2019) Screens: DS01, DS02, DS03, DS04 not applicable for our data set
# DS05 - We treat returns for which R_t or R_t-1 is greater than 300% and (1+R_t)(1+R_t-1)-1 (compounded return) is less than 0.55 as missing returns
# -> to filter out significant high returns that are followed or preceded by substantial negative returns
# DS07 - 300% Return Screen: All returns greater than 300% are set to NA, regardless of the returns before or after this event
# -> to filter out the exceptionally high returns

# First Part of DS05: If meet the conditions (screen criteria < 0.55 and Rt or Rt-1 > 300), delete big negative returns

x <- DS_monthly_data_Clean[, .(dscode, ym, RET_USD)]
x[, T_1_RET_USD := RET_USD] # duplicate the column RET_USD
x[, ym := ym + 1/12]  # increment ym by one month
x <- x[, .(dscode, ym, T_1_RET_USD)]  # align T_1_RET_USD (copy of RET_USD) with the incremented ym

DS_monthly_data_Clean[, ymMatcher := ym]
DS_monthly_data_Clean_DS05 <- merge(DS_monthly_data_Clean, x, by.x = c("dscode", "ymMatcher"), by.y = c("dscode", "ym"))  ## T_1_RET_USD: the previous month's return
setorder(DS_monthly_data_Clean_DS05, dscode, ym) 

DS_monthly_data_Clean_DS05[, screen := ((1 + (RET_USD/100)) * (1 + (T_1_RET_USD/100))) - 1] ## screen: compounded return of the current and previous month
DS_monthly_data_Clean_DS05[T_1_RET_USD >= 300 & screen < 0.55, RET_USD := NA] # set RET_USD to NA while meeting the conditions
DS_monthly_data_Clean_DS05[dscode == shift(dscode, 1) & shift(screen, 1) < 0.55 & shift(RET_USD, 1) >= 300, RET_USD := NA] # also set RET_USD to NA if the preceding row meets the conditions

# Second part of DS05 and execution of DS07: Delete all returns bigger than 300%

DS_monthly_data_Clean_DS05_DS07 <- DS_monthly_data_Clean_DS05[RET_USD >= 300, RET_USD := NA]

# Redo ASS2 to exclude any NAs in the RET_USD data

DS_monthly_data_Clean_DS05_DS07 <- DS_monthly_data_Clean_DS05_DS07[is.na(RET_USD) != TRUE]

# DS08 - Removal of Penny Stocks: penny stock if market cap < 5% percentile 

ds08.penny.screen <- function(Data, country_Name) {
  
  Data <- Data[region == country_Name]
  
  Data[, MV_USD_005Perc := quantile(na.omit(MV_USD), probs = 0.05), by = ym] # calculate the 5th percentile of market capitalization for each region with the same ym
  Data <- Data[MV_USD > MV_USD_005Perc] # only to keep market capitalization_USD greater than the 5th percentile value  
  Data[, MV_USD_005Perc := NULL] # remove the percentile column
  
  Data <- Data[, .(dscode, region, marketdate, MV, MV_USD, RET, RET_USD, PCH, PCH_USD, UP, NOSH, AF, ym)]
  
  return(Data)
}

CA_Return_DS05_DS07_DS08 <- ds08.penny.screen(DS_monthly_data_Clean_DS05_DS07, "CA") 
DE_Return_DS05_DS07_DS08 <- ds08.penny.screen(DS_monthly_data_Clean_DS05_DS07, "DE")
FR_Return_DS05_DS07_DS08 <- ds08.penny.screen(DS_monthly_data_Clean_DS05_DS07, "FR")
GB_Return_DS05_DS07_DS08 <- ds08.penny.screen(DS_monthly_data_Clean_DS05_DS07, "GB")
IT_Return_DS05_DS07_DS08 <- ds08.penny.screen(DS_monthly_data_Clean_DS05_DS07, "IT")
JP_Return_DS05_DS07_DS08 <- ds08.penny.screen(DS_monthly_data_Clean_DS05_DS07, "JP")
US_Return_DS05_DS07_DS08 <- ds08.penny.screen(DS_monthly_data_Clean_DS05_DS07, "US")

############################################### Saving Country specific #####################################################

setwd("C:/Data/Clean Data")

# Canada

CA_Acc <- ws_yearly_data[region == "CA"]
CA_Acc_Reduced <- ws_yearly_data_reduced_filtered_final[region == "CA"]

save(CA_Acc_Reduced, file = "CA_Acc_Reduced.RData")
save(CA_Return_DS05_DS07_DS08, file= "CA_Return_DS05_DS07_DS08.RData")

# Germany
DE_Acc <- ws_yearly_data[region == "DE"]
DE_Acc_Reduced <- ws_yearly_data_reduced_filtered_final[region == "DE"]

save(DE_Acc_Reduced, file = "DE_Acc_Reduced.RData")
save(DE_Return_DS05_DS07_DS08, file= "DE_Return_DS05_DS07_DS08.RData")

# France
FR_Acc <- ws_yearly_data[region == "FR"]
FR_Acc_Reduced <- ws_yearly_data_reduced_filtered_final[region == "FR"]

save(FR_Acc_Reduced, file = "FR_Acc_Reduced.RData")
save(FR_Return_DS05_DS07_DS08, file= "FR_Return_DS05_DS07_DS08.RData")

# United Kingdom
GB_Acc <- ws_yearly_data[region == "GB"]
GB_Acc_Reduced <- ws_yearly_data_reduced_filtered_final[region == "GB"]

save(GB_Acc_Reduced, file = "GB_Acc_Reduced.RData")
save(GB_Return_DS05_DS07_DS08, file= "GB_Return_DS05_DS07_DS08.RData")

# Italy
IT_Acc <- ws_yearly_data[region == "IT"]
IT_Acc_Reduced <- ws_yearly_data_reduced_filtered_final[region == "IT"]

save(IT_Acc_Reduced, file = "IT_Acc_Reduced.RData")
save(IT_Return_DS05_DS07_DS08, file= "IT_Return_DS05_DS07_DS08.RData")

#Japan
JP_Acc <- ws_yearly_data[region == "JP"]
JP_Acc_Reduced <- ws_yearly_data_reduced_filtered_final[region == "JP"]

save(JP_Acc_Reduced, file = "JP_Acc_Reduced.RData")
save(JP_Return_DS05_DS07_DS08, file= "JP_Return_DS05_DS07_DS08.RData")

# United States
US_Acc <- ws_yearly_data[region == "US"]
US_Acc_Reduced <- ws_yearly_data_reduced_filtered_final[region == "US"]

save(US_Acc_Reduced, file = "US_Acc_Reduced.RData")
save(US_Return_DS05_DS07_DS08, file= "US_Return_DS05_DS07_DS08.RData")

########################################## Summary stats for raw and clean data ################################################

# Combine the data for the calculation of aggregation market later

Return_DS05_DS07_DS08 <- rbind(CA_Return_DS05_DS07_DS08, DE_Return_DS05_DS07_DS08, FR_Return_DS05_DS07_DS08, GB_Return_DS05_DS07_DS08, 
                               IT_Return_DS05_DS07_DS08, JP_Return_DS05_DS07_DS08, US_Return_DS05_DS07_DS08)

Acc_Reduced <- rbind(CA_Acc_Reduced, DE_Acc_Reduced, FR_Acc_Reduced, GB_Acc_Reduced, IT_Acc_Reduced, JP_Acc_Reduced, US_Acc_Reduced)

summary_stats_raw <- ds_monthly_data[, .(Type = "raw", Total_no_firms = n_distinct(dscode), Firm_Months = .N , 
                                         Total_Size = sum(MV_USD, na.rm = T),  ## the total size of the market
                                         Mean_Size = mean(MV_USD, na.rm = T), 
                                         Median_Size = median(MV_USD, na.rm = T), 
                                         Mean_Performance = mean(RET_USD, na.rm = T),   ## the overall market performance
                                         Median__Performance = median(RET_USD, na.rm = T),   ## the typical performance
                                         Market_Volatility = sd(RET_USD, na.rm = TRUE),  ## the market volatility
                                         Positive_Returns_Percentage = sum(RET_USD > 0, na.rm = TRUE) / .N * 100,  ## the percentage of firms with positive returns
                                         Negative_Returns_Percentage = sum(RET_USD < 0, na.rm = TRUE) / .N * 100,   
                                         Positive_Price_Changese_Percentage = sum(PCH_USD > 0, na.rm = TRUE) / .N * 100, ## the percentage of firms with positive price changes
                                         Negative_Price_Change_Percentage = sum(PCH_USD < 0, na.rm = TRUE) / .N * 100, 
                                         Mean_Number = mean(NOSH, na.rm = T),  ## the average firm size in terms of the number of shares outstanding
                                         Start = min(ym), End = max(ym), Count_ym = length(unique(ym))), 
                                     by = region]

summary_stats_clean <- Return_DS05_DS07_DS08[, .(Type = "clean", Total_no_firms = n_distinct(dscode), Firm_Months = .N , 
                                                 Total_Size = sum(MV_USD, na.rm = T),  ## the total size of the market
                                                 Mean_Size = mean(MV_USD, na.rm = T), 
                                                 Median_Size = median(MV_USD, na.rm = T), 
                                                 Mean_Performance = mean(RET_USD, na.rm = T),   ## the overall market performance
                                                 Median__Performance = median(RET_USD, na.rm = T),   ## the typical performance
                                                 Market_Volatility = sd(RET_USD, na.rm = TRUE),  ## the market volatility
                                                 Positive_Returns_Percentage = sum(RET_USD > 0, na.rm = TRUE) / .N * 100,  ## the percentage of firms with positive returns
                                                 Negative_Returns_Percentage = sum(RET_USD < 0, na.rm = TRUE) / .N * 100,   
                                                 Positive_Price_Changese_Percentage = sum(PCH_USD > 0, na.rm = TRUE) / .N * 100, ## the percentage of firms with positive price changes
                                                 Negative_Price_Change_Percentage = sum(PCH_USD < 0, na.rm = TRUE) / .N * 100, 
                                                 Mean_Number = mean(NOSH, na.rm = T),  ## the average firm size in terms of the number of shares outstanding
                                                 Start = min(ym), End = max(ym), Count_ym = length(unique(ym))), 
                                             by = region]

summary_stats_data <- rbind(summary_stats_raw, summary_stats_clean)

summary_stats_data[, Start := as.numeric(Start)]
summary_stats_data[, End := as.numeric(End)]

setorder(summary_stats_data, region, Type)

setwd("C:/Data")
save(summary_stats_data, file= "summary_stats_data.RData")
save(Return_DS05_DS07_DS08, file= "Return_DS05_DS07_DS08.RData")
save(Acc_Reduced, file= "Acc_Reduced")

#rm(list = ls())  

