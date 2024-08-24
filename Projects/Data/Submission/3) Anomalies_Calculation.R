#############################################################################################################################

# Calculate 11 anomalies and Market Return

# Output: XX_Data_long_ST

########################################################### Set Up ##########################################################

# Required packages
# install.packages("data.table")
library(data.table)
# install.packages("tidyr")
library(tidyr)
# install.packages("dplyr")
library(dplyr)
#install.packages("dtplyr")
library(dtplyr)
#install.packages("DataCombine")
library(DataCombine)
library(zoo)

  
load("C:/Data/Clean Data/CA_Acc_Reduced.RData")
load("C:/Data/Clean Data/CA_Return_DS05_DS07_DS08.RData")
load("C:/Data/Clean Data/DE_Acc_Reduced.RData")
load("C:/Data/Clean Data/DE_Return_DS05_DS07_DS08.RData")
load("C:/Data/Clean Data/FR_Acc_Reduced.RData")
load("C:/Data/Clean Data/FR_Return_DS05_DS07_DS08.RData")
load("C:/Data/Clean Data/GB_Acc_Reduced.RData")
load("C:/Data/Clean Data/GB_Return_DS05_DS07_DS08.RData")
load("C:/Data/Clean Data/IT_Acc_Reduced.RData")
load("C:/Data/Clean Data/IT_Return_DS05_DS07_DS08.RData")
load("C:/Data/Clean Data/JP_Acc_Reduced.RData")
load("C:/Data/Clean Data/JP_Return_DS05_DS07_DS08.RData")
load("C:/Data/Clean Data/US_Acc_Reduced.RData")
load("C:/Data/Clean Data/US_Return_DS05_DS07_DS08.RData")

CA_Return_DS05_DS07_DS08[, `:=`(
  marketdate = as.Date(marketdate),
  AF = as.numeric(AF),
  ym = as.yearmon(ym)
)]

DE_Return_DS05_DS07_DS08[, `:=`(
  marketdate = as.Date(marketdate),
  AF = as.numeric(AF),
  ym = as.yearmon(ym)
)]

FR_Return_DS05_DS07_DS08[, `:=`(
  marketdate = as.Date(marketdate),
  AF = as.numeric(AF),
  ym = as.yearmon(ym)
)]

GB_Return_DS05_DS07_DS08[, `:=`(
  marketdate = as.Date(marketdate),
  AF = as.numeric(AF),
  ym = as.yearmon(ym)
)]

IT_Return_DS05_DS07_DS08[, `:=`(
  marketdate = as.Date(marketdate),
  AF = as.numeric(AF),
  ym = as.yearmon(ym)
)]

JP_Return_DS05_DS07_DS08[, `:=`(
  marketdate = as.Date(marketdate),
  AF = as.numeric(AF),
  ym = as.yearmon(ym)
)]

US_Return_DS05_DS07_DS08[, `:=`(
  marketdate = as.Date(marketdate),
  AF = as.numeric(AF),
  ym = as.yearmon(ym)
)]

################################################# 1. Anomaly: Gross Profitability ############################################
# More profitable firms have higher returns.
# GPtoAsset is calculated as gross profits (Net Sales Or Revenues - COGS) scaled by assets

gp_Anomaly <- function(Acc_Reduced) {
    Acc1 <- Acc_Reduced[, GPtoAsset := ( (ITEM1001-ITEM1051) / ITEM2999 )]
    
    Acc1[GPtoAsset == Inf, GPtoAsset := NA]
    Acc1[GPtoAsset == -Inf, GPtoAsset := NA]
    
  }

CA_Acc_1 <- gp_Anomaly(CA_Acc_Reduced)   
DE_Acc_1 <- gp_Anomaly(DE_Acc_Reduced) 
FR_Acc_1 <- gp_Anomaly(FR_Acc_Reduced) 
GB_Acc_1 <- gp_Anomaly(GB_Acc_Reduced) 
IT_Acc_1 <- gp_Anomaly(IT_Acc_Reduced)
JP_Acc_1 <- gp_Anomaly(JP_Acc_Reduced) 
US_Acc_1 <- gp_Anomaly(US_Acc_Reduced)
  
rm(list = "CA_Acc_Reduced", "DE_Acc_Reduced", "FR_Acc_Reduced", "GB_Acc_Reduced", "IT_Acc_Reduced" , "JP_Acc_Reduced", "US_Acc_Reduced")
  
################################################# 2. Anomaly: Return on Assets ###############################################
# Firms with higher past return on assets earn abnormally higher subsequent returns.
# ROA is calculated as net income before extraordinary items (ITEM1551) divided by previous year total assets (ITEM2999) 
# Income is for the most recent year for which the reporting date precedes the end of month t-1, and assets are for the prior year

roa_Anomaly <- function(Acc_1) {
  
    x <- Acc_1[, .(dscode, YEAR, ITEM1551, ITEM2999)]
    
    y <- copy(x)
    y[, LaggedAsset := ITEM2999]
    y[, YEAR := YEAR + 1]
    y <- y[, .(dscode, YEAR, LaggedAsset)]
    
    output <- merge(x, y, by = c("dscode", "YEAR"), all.x = T)
    output <- output[, ROA := (ITEM1551 / LaggedAsset) * 100] # convert decimal to percentage
    output <- output[, .(dscode, YEAR, ROA)]
    
    output[YEAR == 1996, ROA := NA]
    output[ROA == Inf, ROA := NA]
    output[ROA == -Inf, ROA := NA]
    
    
    Acc_2 <- merge(Acc_1, output, by =c("dscode", "YEAR"), all.x = T)
    
    return(Acc_2)
  }

CA_Acc_12 <- roa_Anomaly(CA_Acc_1)  
DE_Acc_12 <- roa_Anomaly(DE_Acc_1)
FR_Acc_12 <- roa_Anomaly(FR_Acc_1)
GB_Acc_12 <- roa_Anomaly(GB_Acc_1)
IT_Acc_12 <- roa_Anomaly(IT_Acc_1)
JP_Acc_12 <- roa_Anomaly(JP_Acc_1)
US_Acc_12 <- roa_Anomaly(US_Acc_1)
  
rm(list = "CA_Acc_1", "DE_Acc_1", "FR_Acc_1", "GB_Acc_1", "IT_Acc_1", "JP_Acc_1", "US_Acc_1")
  
################################################## 3. Anomaly: Asset Growth ##################################################
# Firms with higher total asset growth earn lower subsequent returns.
# Asset growth is measured as the growth rate of total assets in the previous fiscal year. 
# Asset growth as the most recent year-over-year annual growth rate of total assets in percent (unit is %)
  
assetGrowth_Anomaly <- function(Acc_12) {
    
    x <- Acc_12[, .(dscode, YEAR, ITEM2999)]
    
    y <- copy(x)
    y[, LaggedAsset := ITEM2999]
    y[, YEAR := (YEAR + 1)]
    y <- y[ , .(dscode, YEAR, LaggedAsset)]
    
    output <- merge(x, y, by = c("dscode", "YEAR"), all.x = T)
    output[, AssetGrowth := ((ITEM2999 - LaggedAsset) / LaggedAsset) * 100] 
    output <- output[, .(dscode, YEAR, AssetGrowth)]
    
    output[YEAR == 1996, AssetGrowth := NA]
    output[AssetGrowth == Inf, AssetGrowth := NA]
    output[AssetGrowth == -Inf, AssetGrowth := NA]
    
    Acc_123 <- merge(Acc_12, output, by = c("dscode", "YEAR"), all.x = T)
    
    return(Acc_123)
  }

CA_Acc_123 <- assetGrowth_Anomaly(CA_Acc_12)  
DE_Acc_123 <- assetGrowth_Anomaly(DE_Acc_12)
FR_Acc_123 <- assetGrowth_Anomaly(FR_Acc_12)
GB_Acc_123 <- assetGrowth_Anomaly(GB_Acc_12)
IT_Acc_123 <- assetGrowth_Anomaly(IT_Acc_12)
JP_Acc_123 <- assetGrowth_Anomaly(JP_Acc_12)
US_Acc_123 <- assetGrowth_Anomaly(US_Acc_12)
  
rm(list = "CA_Acc_12", "DE_Acc_12", "FR_Acc_12", "GB_Acc_12", "IT_Acc_12", "JP_Acc_12", "US_Acc_12")
  
############################################## 4. Anomaly: Net Operating Assets ##############################################
# Firms with high net operating assets earn lower long-run stock returns.
# NOA t = ( Operating Assets t - Operating liabilities t ) / Total Assets t-1
# Operating Assets = Total Assets - Cash and Short term Investment and Cash Equivalents 
# Operating liabilities = Total Assets - Short-term Debt - Long-term Debt - Minority Interest - Pref. Stock - Common Equity
# Total Assets: ITEM2999 ; Cash & Short Term Investments: ITEM2001 ; Short Term Debt: ITEM3051 ; Long-term Debt: ITEM3251
# Minority Interest: ITEM3426 ; Preferred Stock: ITEM3451 ; Common Equity: ITEM3501
  
netOperatingAssets_Anomaly <- function(Acc_123) {
    
    x <- Acc_123[ , .(dscode, YEAR, ITEM2999, ITEM2001, ITEM3051, ITEM3251, ITEM3426, ITEM3451, ITEM3501)]
    
    y <- copy(x)
    y[, LaggedAsset := ITEM2999]
    y[, YEAR := (YEAR + 1)]
    y <- y[, .(dscode, YEAR, LaggedAsset)]
    
    output <- merge(x, y, by = c("dscode", "YEAR"), all.x = T)
  
    # To decrease the loss of observations when calculating net operating assets, values for short-term debt, taxes payable,
    # long-term debt, minority interest, or preferred stock are set to 0.0 if they are NA
    output[is.na(ITEM3051), ITEM3051 := 0.0]
    output[is.na(ITEM3251), ITEM3251 := 0.0]
    output[is.na(ITEM3426), ITEM3426 := 0.0]
    output[is.na(ITEM3451), ITEM3451 := 0.0]
    
    output[, NetOperatingAssets := ( ( (ITEM2999 - ITEM2001) - (ITEM2999 - ITEM3051 - ITEM3251 - ITEM3426 - ITEM3451 - ITEM3501) ) ) / LaggedAsset]
    
    output <- output[, .(dscode, YEAR, NetOperatingAssets)]
    
    output[YEAR == 1996, NetOperatingAssets := NA]
    output[NetOperatingAssets == Inf, NetOperatingAssets := NA]
    output[NetOperatingAssets == -Inf, NetOperatingAssets := NA]
    
    Acc_1234 <- merge(Acc_123, output, by =c("dscode", "YEAR"), all.x = T)
    
    return(Acc_1234)
  }

CA_Acc_1234 <- netOperatingAssets_Anomaly(CA_Acc_123)  
DE_Acc_1234 <- netOperatingAssets_Anomaly(DE_Acc_123)
FR_Acc_1234 <- netOperatingAssets_Anomaly(FR_Acc_123)
GB_Acc_1234 <- netOperatingAssets_Anomaly(GB_Acc_123)
IT_Acc_1234 <- netOperatingAssets_Anomaly(IT_Acc_123)
JP_Acc_1234 <- netOperatingAssets_Anomaly(JP_Acc_123)
US_Acc_1234 <- netOperatingAssets_Anomaly(US_Acc_123)
  
rm(list = "CA_Acc_123", "DE_Acc_123", "FR_Acc_123", "GB_Acc_123", "IT_Acc_123", "JP_Acc_123", "US_Acc_123")
  
##################################################### 5. Anomaly: Accruals ##################################################
# Firms with high accruals earn abnormal lower returns.  
# Total accruals component = (Annual change in non-cash working capital - D&A expense) / by average total assets of t and t-1
# Non-cash working capital = Change in current assets (ITEM2201)
                         # - change in cash and short-term investments (ITEM2001)
                         # - change in current liabilities (ITEM3101) 
                         # + change in short-term debt included in current liabilities (ITEM3051)
                         # + change in income taxes payable (ITEM3063)
# D&A = ITEM1151
  
accruals_Anomaly <- function(Acc_1234) {
    
    x <- Acc_1234[, .(dscode, YEAR, ITEM2999, ITEM2201, ITEM2001, ITEM3101, ITEM3051, ITEM3063, ITEM1151)]
    
    y <- copy(x)
    names(y)[3] <- "LaggedAsset"
    names(y)[4] <- "LaggedCurrentAssets"
    names(y)[5] <- "LaggedCash"
    names(y)[6] <- "LaggedCurrentLiabilities"
    names(y)[7] <- "LaggedShortDebt"
    names(y)[8] <- "LaggedTaxPayable"
    y[, YEAR := (YEAR + 1)]
    y <- y[, .(dscode, YEAR, LaggedAsset, LaggedCurrentAssets, LaggedCash, LaggedCurrentLiabilities, LaggedShortDebt, LaggedTaxPayable)]
    
    output <- merge(x, y, by = c("dscode", "YEAR"), all.x = T)
    
    # To decrease the loss of observations, values for short-term debt, taxes payable, income taxes payable are set to 0.0 if they are NA
    output[is.na(ITEM3051), ITEM3051 := 0.0]
    output[is.na(LaggedShortDebt), LaggedShortDebt := 0.0]
    output[is.na(ITEM3063), ITEM3063 := 0.0]
    output[is.na(LaggedTaxPayable), LaggedTaxPayable := 0.0]
    
    output[, Accruals := ( ( ((ITEM2201 - LaggedCurrentAssets) - (ITEM2001 - LaggedCash) - (ITEM3101 - LaggedCurrentLiabilities) + (ITEM3051 - LaggedShortDebt) +
                                (ITEM3063 - LaggedTaxPayable) ) - ITEM1151 ) / ((ITEM2999 + LaggedAsset) / 2) )] 
  
    output <- output[, .(dscode, YEAR, Accruals)]
    output[YEAR == 1996, Accruals := NA]
    output[Accruals == Inf, Accruals := NA]
    output[Accruals == -Inf, Accruals := NA]
    
    Acc_12345 <- merge(Acc_1234, output, by =c("dscode", "YEAR"), all.x = T)
    return(Acc_12345)
  }

CA_Acc_12345 <- accruals_Anomaly(CA_Acc_1234)  
DE_Acc_12345 <- accruals_Anomaly(DE_Acc_1234)
FR_Acc_12345 <- accruals_Anomaly(FR_Acc_1234)
GB_Acc_12345 <- accruals_Anomaly(GB_Acc_1234)
IT_Acc_12345 <- accruals_Anomaly(IT_Acc_1234)
JP_Acc_12345 <- accruals_Anomaly(JP_Acc_1234)
US_Acc_12345 <- accruals_Anomaly(US_Acc_1234)
  
rm(list = "CA_Acc_1234", "DE_Acc_1234", "FR_Acc_1234", "GB_Acc_1234", "IT_Acc_1234", "JP_Acc_1234", "US_Acc_1234")
  
############################################### 6. Anomaly: Investment-to-Asset #############################################
# Firms with higher past investment earn abnormally lower future returns.
# Following Titman et al (2004), Inv-to-assets = (change in gross PPE ITEM2301 + changes in inventory ITEM2101)  / lagged total assets 
  
investmentToAsset_Anomaly <- function(Acc_12345) {
    
    x <- Acc_12345[, .(dscode, YEAR, ITEM2999, ITEM2301, ITEM2101)]
    
    y <- copy(x)
    names(y)[3] <- "LaggedAsset"
    names(y)[4] <- "LaggedPPE"
    names(y)[5] <- "LaggedInventory"
    y[, YEAR := (YEAR + 1)]
  
    output <- merge(x, y, by = c("dscode", "YEAR"), all.x = T)
    output[, InvestmentToAsset := ( (ITEM2301 - LaggedPPE) + (ITEM2101 - LaggedInventory) ) / LaggedAsset]
    
    output <- output[, .(dscode, YEAR, InvestmentToAsset)]
    output[YEAR == 1996, InvestmentToAsset := NA]
    output[InvestmentToAsset == Inf, InvestmentToAsset := NA]
    output[InvestmentToAsset == -Inf, InvestmentToAsset := NA]
    
    Acc_123456 <- merge(Acc_12345, output, by = c("dscode", "YEAR"), all.x = T)
    return(Acc_123456)
    
  }

CA_Acc_123456 <- investmentToAsset_Anomaly(CA_Acc_12345)  
DE_Acc_123456 <- investmentToAsset_Anomaly(DE_Acc_12345)
FR_Acc_123456 <- investmentToAsset_Anomaly(FR_Acc_12345)
GB_Acc_123456 <- investmentToAsset_Anomaly(GB_Acc_12345)
IT_Acc_123456 <- investmentToAsset_Anomaly(IT_Acc_12345)
JP_Acc_123456 <- investmentToAsset_Anomaly(JP_Acc_12345)
US_Acc_123456 <- investmentToAsset_Anomaly(US_Acc_12345)
  
rm(list = "CA_Acc_12345", "DE_Acc_12345", "FR_Acc_12345", "GB_Acc_12345", "IT_Acc_12345", "JP_Acc_12345", "US_Acc_12345")
  
################################################ 7. Anomaly: Net Stock Issuance #############################################
# In post-issue years, equity issuers underperform matching nonissuers with similar characteristics.
# Following Fama / French (2008), we measure net issuance as the annual log change in split-adjusted shares outstanding. 
# NSI variable = log(split-adjusted_t-1 / split-adjusted_t-2) --> (unit:yearly) page 24 in Appendix Fama French 2008
## measure the percentage change in a company's equity base due to new issues or repurchases, adjusted logarithmically to normalize the distribution of changes

netStockIssues_Anomaly <- function(Return_DS05_DS07_DS08) { 
    
    x <- Return_DS05_DS07_DS08[, .(dscode, marketdate, ym, NOSH, AF)]
    
    y <- copy(x)
    names(y)[4] <- "NOSHt1"
    names(y)[5] <- "AFt1"
    
    z <- copy(x)
    names(z)[4] <- "NOSHt2"
    names(z)[5] <- "AFt2"
    
    # Approximate the end of the fiscal year to November/December 
    x <- x[, Yeart1 := as.yearmon((year(marketdate) - 1) + 11/12)] # FY end of t-1
    x <- x[, Yeart2 := as.yearmon((year(marketdate) - 2) + 11/12)] # FY end of t-2
    
    output <- merge(x, y, by.x = c("dscode", "Yeart1"), by.y = c("dscode", "ym"), all.x = TRUE)
    
    output <- merge(output, z, by.x = c("dscode", "Yeart2"), by.y = c("dscode", "ym"), all.x = TRUE)
    
    output[, NetStockIssues := log( (NOSHt1 * AFt1) / (NOSHt2 * AFt2) )]
    
    output <- output[, .(dscode, ym, NetStockIssues)]
    output[NetStockIssues == Inf, NetStockIssues := NA]
    output[NetStockIssues == -Inf, NetStockIssues := NA]
    
    Return_DS05_DS07_DS08_7 <- merge(Return_DS05_DS07_DS08, output, by = c("dscode", "ym"), all.x = TRUE)
    return(Return_DS05_DS07_DS08_7)
}

CA_Return_DS05_DS07_DS08_7 <- netStockIssues_Anomaly(CA_Return_DS05_DS07_DS08)  
DE_Return_DS05_DS07_DS08_7 <- netStockIssues_Anomaly(DE_Return_DS05_DS07_DS08)
FR_Return_DS05_DS07_DS08_7 <- netStockIssues_Anomaly(FR_Return_DS05_DS07_DS08)
GB_Return_DS05_DS07_DS08_7 <- netStockIssues_Anomaly(GB_Return_DS05_DS07_DS08)
IT_Return_DS05_DS07_DS08_7 <- netStockIssues_Anomaly(IT_Return_DS05_DS07_DS08)
JP_Return_DS05_DS07_DS08_7 <- netStockIssues_Anomaly(JP_Return_DS05_DS07_DS08)
US_Return_DS05_DS07_DS08_7 <- netStockIssues_Anomaly(US_Return_DS05_DS07_DS08)
  
rm(list = "CA_Return_DS05_DS07_DS08", "DE_Return_DS05_DS07_DS08", "FR_Return_DS05_DS07_DS08", "GB_Return_DS05_DS07_DS08", "IT_Return_DS05_DS07_DS08", "JP_Return_DS05_DS07_DS08", "US_Return_DS05_DS07_DS08")
  
############################################## 8. Anomaly: Composite Equity Issuance ########################################
# issuers underperform nonissuers
# 1: log (Mt / Mt-1) -  (Sum rt zu rt-1)
# 2: (MVt - MVt-1) - (rt - rt-1)
# both with lag of 4 months and 16 months
  
compositeEquity_Log_Anomaly <- function(Return_DS05_DS07_DS08_7) { 
    
    x <- Return_DS05_DS07_DS08_7[, .(dscode, ym, MV_USD, RET_USD)]
    
    # Lagged MVs
    y <- copy(x)
    names(y)[3] <- "Lag4MV_USD"
    names(y)[4] <- "Lag4RET_USD"

    z <- copy(x)
    names(z)[3] <- "Lag16MV_USD"
    names(z)[4] <- "Lag16RET_USD"
    
    x[, Lag4 := ym - 4/12]           
    x[, Lag16 := ym - 16/12]

    output <- merge(x, y, by.x = c("dscode", "Lag4"), by.y = c("dscode", "ym"), all.x = T)  
    output <- merge(output, z, by.x = c("dscode", "Lag16"), by.y = c("dscode", "ym"), all.x = T)
    
    # Sum of returns (cumulative returns between the 16-month lag and the 4-month lag period)
    ret <- output[, .(dscode, ym, Lag16, Lag4, RET_USD)]
    ret[, end_merge := Lag4]
    retMerger <- ret[, .(dscode, ym, RET_USD)]
    
    retSum <- retMerger[ret, on = .(dscode, ym > Lag16, ym <= Lag4), # ym > 16 for the cum return calculation due to having end of month dates
                       by = .EACHI, .(end_merge, CumRet = sum(RET_USD))]
    retSum[, end_merge := end_merge + 4/12] 
    
    names(retSum)[2] <- "Lag16"
    names(retSum)[3] <- "Lag4"
    names(retSum)[4] <- "ym"
    
    # CumRet is only useful for th anomaly calculation if it is composed of 16 prior months returns
    # -> Set CumRet values to NA for observation with ym smaller then (first year + 16 months)
    
    min <- retSum[, min(ym), by = dscode]
    retSum <- merge(retSum, min, by = "dscode", all.x = T) # V1: the minimum ym for each dscode
    retSum[ym <  V1 + 15.5/12, CumRet := NA] # cutoff point for CumRet where no sufficient historical data available 
    retSum <- retSum[, .(dscode, Lag4, Lag16, CumRet)]
  
    output <- merge(output, retSum, by = c("dscode","Lag4","Lag16" ), all.x = T)
    output[, CompositeEquity_Log := log(Lag4MV_USD / Lag16MV_USD)  - CumRet/100] # Convert percentage to decimal
    output[CompositeEquity_Log == Inf, CompositeEquity_Log := NA]
    output[CompositeEquity_Log == -Inf, CompositeEquity_Log := NA]
    output <- output[, .(dscode, ym, CompositeEquity_Log)]
  
    Return_DS05_DS07_DS08_7[, ymMatcher := ym]          
    Return_DS05_DS07_DS08_78 <- merge(Return_DS05_DS07_DS08_7, output, by.x = c("dscode", "ymMatcher"), by.y = c("dscode", "ym"), all.x = T)    
    Return_DS05_DS07_DS08_78[, ymMatcher := NULL]
    
    return(Return_DS05_DS07_DS08_78)
  } 
  
CA_Return_DS05_DS07_DS08_78 <- compositeEquity_Log_Anomaly(CA_Return_DS05_DS07_DS08_7)
DE_Return_DS05_DS07_DS08_78 <- compositeEquity_Log_Anomaly(DE_Return_DS05_DS07_DS08_7)
FR_Return_DS05_DS07_DS08_78 <- compositeEquity_Log_Anomaly(FR_Return_DS05_DS07_DS08_7)
GB_Return_DS05_DS07_DS08_78 <- compositeEquity_Log_Anomaly(GB_Return_DS05_DS07_DS08_7)
IT_Return_DS05_DS07_DS08_78 <- compositeEquity_Log_Anomaly(IT_Return_DS05_DS07_DS08_7)
JP_Return_DS05_DS07_DS08_78 <- compositeEquity_Log_Anomaly(JP_Return_DS05_DS07_DS08_7)
US_Return_DS05_DS07_DS08_78 <- compositeEquity_Log_Anomaly(US_Return_DS05_DS07_DS08_7)
  
rm(list = "CA_Return_DS05_DS07_DS08_7", "DE_Return_DS05_DS07_DS08_7", "FR_Return_DS05_DS07_DS08_7", "GB_Return_DS05_DS07_DS08_7", "IT_Return_DS05_DS07_DS08_7", "JP_Return_DS05_DS07_DS08_7", "US_Return_DS05_DS07_DS08_7")
  
##################################################### 9. Anomaly: Momentum ##################################################
# Stronger momentum during high-sentiment period.
# Momentum return: 1-month lagged 11 month cumulative return
  
momentum_Anomaly <- function(Return_DS05_DS07_DS08_78) {
    
    ret <- Return_DS05_DS07_DS08_78[, .(dscode, ym, RET_USD)]
    ret[, start := ym - 1] # one year before the current month ym
    ret[, end := ym - 2/12] # two months before the current month ym
    ret[, end_merge := end]
    
    Momentum <- ret[, .(dscode, ym, start, end, RET_USD)]
    
    retMerger <- ret[, .(dscode, ym, RET_USD)]
   
    retSum <- retMerger[ret, on = .(dscode, ym >= start, ym <= end), # ym > 16 for the cum return calculation due to having end of month dates
                        by = .EACHI, 
                        .(end_merge, MomentumReturn = prod(1+RET_USD))] # the 11-month cumulative return (exclude the most recent month (the 1-month lag))
    retSum[, end_merge := end_merge + 2/12] 
    
    names(retSum)[2] <- "start"
    names(retSum)[3] <- "end"
    names(retSum)[4] <- "ym"
    
    min <- retSum[, min(ym), by = dscode]
    retSum <- merge(retSum, min, by = "dscode", all.x = T)
    retSum[ym < V1 + 11.5/12, MomentumReturn := NA] # cutoff point for MomentumReturn where no sufficient historical data available 
    retSum <- retSum[, .(dscode, start, end, MomentumReturn)]
    
    Momentum <- merge(Momentum, retSum, by = c("dscode", "start", "end"), all.x = T)
    
    Momentum[MomentumReturn == Inf, MomentumReturn := NA]
    Momentum[MomentumReturn == -Inf, MomentumReturn := NA]
    Momentum <- Momentum[, .(dscode, ym, MomentumReturn)]
    
    Return_DS05_DS07_DS08_789 <- merge(Return_DS05_DS07_DS08_78, Momentum, by =c("dscode", "ym"), all.x = T)
    return(Return_DS05_DS07_DS08_789)
    
  }

CA_Return_DS05_DS07_DS08_789 <- momentum_Anomaly(CA_Return_DS05_DS07_DS08_78)    
DE_Return_DS05_DS07_DS08_789 <- momentum_Anomaly(DE_Return_DS05_DS07_DS08_78)
FR_Return_DS05_DS07_DS08_789 <- momentum_Anomaly(FR_Return_DS05_DS07_DS08_78)
GB_Return_DS05_DS07_DS08_789 <- momentum_Anomaly(GB_Return_DS05_DS07_DS08_78)
IT_Return_DS05_DS07_DS08_789 <- momentum_Anomaly(IT_Return_DS05_DS07_DS08_78)
JP_Return_DS05_DS07_DS08_789 <- momentum_Anomaly(JP_Return_DS05_DS07_DS08_78)
US_Return_DS05_DS07_DS08_789 <- momentum_Anomaly(US_Return_DS05_DS07_DS08_78)
  
rm(list = "CA_Return_DS05_DS07_DS08_78", "DE_Return_DS05_DS07_DS08_78", "FR_Return_DS05_DS07_DS08_78", "GB_Return_DS05_DS07_DS08_78", "IT_Return_DS05_DS07_DS08_78", "JP_Return_DS05_DS07_DS08_78", "US_Return_DS05_DS07_DS08_78")
  
##################################################### 10. Anomaly: O-Score ##################################################
# Firms with high failure probability have lower subsequent returns.   
# O-Score = -0.407 SIZE + 6.03 TLTA - 1.43 WCTA + 0.076 CLCA - 1.72 OENEG - 2.37 NITA - 1.83 FUTL + 0.285 INTWO - 0.521 CHIN - 1.32
# SIZE = log(Total Assets ITEM2999)
# TLTA = (Total Debt ITEM3255) / (Total Assets ITEM2999)
# WCTA = (Working Capital WC) / (Total Assets ITEM2999)
# CLCA = (Current Liabilities ITEM3101) / (Current Asset ITEM2201)
# ONEEG = 1 if (Total Liabilities ITEM3351) > (Total Assets ITEM2999) else 0
  
# NITA = (Net Income ITEM1651) / (Total Assets ITEM2999) 
# FUTL = (Funds from Operation ITEM4201) / (Total Liabilities ITEM3351)
# INTWO = 1 if (Net Income ITEM1651) is < 0 for t-1 and t-2 
# CHIN = (NIj - NIj-1)/(|NIj|+|NIj-1|) 
  
oScore_Anomaly <- function(Acc_123456) {
    
    x <- Acc_123456[, .(dscode, YEAR, ITEM2999, ITEM3255, WC, ITEM3101, ITEM4201,ITEM2201, ITEM1651, ITEM3351)]
    
    y <- copy(x)
    y[, Lag1NetIncome := ITEM1651]
    y[, YEAR := (YEAR + 1)]
    y <- y[, .(dscode, YEAR, Lag1NetIncome)]
    output <- merge(x, y, by = c("dscode", "YEAR"), all.x = T)
    
    z <- copy(x)
    z[, Lag2NetIncome := ITEM1651]
    z[, YEAR := (YEAR + 2)]
    z <- z[, .(dscode, YEAR, Lag2NetIncome)]
    output <- merge(output, z, by = c("dscode", "YEAR"), all.x = T)
    
    output[, SIZE := log(ITEM2999)]
    output[, TLTA := ITEM3255 / ITEM2999]
    output[, WCTA := WC / ITEM2999]
    output[, CLCA := ITEM3101 / ITEM2201]
    output[, ONEEG := 0.0]
    output[ITEM3351 > ITEM2999, ONEEG := 1.0]
    output[, NITA := ITEM1651 / ITEM2999]
    output[, FUTL := ITEM4201 / ITEM3351]
    output[, INTWO := 0.0]
    output[Lag1NetIncome < 0 & Lag2NetIncome < 0, INTWO := 1.0]
    output[, CHIN := (ITEM1651 - Lag1NetIncome) / ( abs(ITEM1651) + abs(Lag1NetIncome) )]
    
    output[, O_Score := (-0.407 * SIZE) + (6.03 * TLTA) - (1.43 * WCTA) + (0.076 * CLCA) - (1.72 * ONEEG)
           - (2.37 * NITA) - (1.83 * FUTL) + (0.285 * INTWO) - (0.521 * CHIN) - 1.32]
    
    output <- output[, .(dscode, YEAR, O_Score)]
    
    output[O_Score == Inf, O_Score := NA]
    output[O_Score == -Inf, O_Score := NA]
    
    Acc_12345610 <- merge(Acc_123456, output, by =c("dscode", "YEAR"), all.x = T)
    
    return(Acc_12345610)
  }

CA_Acc_12345610 <- oScore_Anomaly(CA_Acc_123456)  
DE_Acc_12345610 <- oScore_Anomaly(DE_Acc_123456)
FR_Acc_12345610 <- oScore_Anomaly(FR_Acc_123456)
GB_Acc_12345610 <- oScore_Anomaly(GB_Acc_123456)
IT_Acc_12345610 <- oScore_Anomaly(IT_Acc_123456)
JP_Acc_12345610 <- oScore_Anomaly(JP_Acc_123456)
US_Acc_12345610 <- oScore_Anomaly(US_Acc_123456)
  
rm(list = "CA_Acc_123456", "DE_Acc_123456", "FR_Acc_123456", "GB_Acc_123456", "IT_Acc_123456", "JP_Acc_123456", "US_Acc_123456")
  
  
###################################### Transform data to a comprehensive / consistent data set #################################
# Creating a consistent data set that includes all required information for further analysis
# Long format: each row represents a unique combination of dscode and YearMatcher with the merged accounting and return data

### Merging Stambaugh way ###

ST_merge <- function(Acc_12345610, Return_DS05_DS07_DS08_789) {
    
    Merging_Accounting <- Acc_12345610[, .(dscode, region, ITEM7021, YEAR, ITEM1001, ITEM1051, ITEM1151, ITEM1250, ITEM1551, 
                                 ITEM1651, ITEM2001, ITEM2005, ITEM2101, ITEM2301, ITEM2999, ITEM3051, ITEM3063, ITEM3101, WC, 
                                 ITEM3251, ITEM3255, ITEM3351, ITEM3426, ITEM3451, ITEM3501, ITEM3999, ITEM4201, ITEM5101, 
                                 ITEM5202, ITEM5301, ITEM8001, ITEM18155, ITEM18191, ITEM18198, GPtoAsset, ROA, AssetGrowth, 
                                 NetOperatingAssets, Accruals, InvestmentToAsset, O_Score)]
    
    Merging_Accounting[, YearMatcher := YEAR]
    Merging_Accounting[, IdMatcher_ACC := dscode]
    
    Merging_Return <- Return_DS05_DS07_DS08_789[, .(dscode, region, marketdate, MV, MV_USD, RET, RET_USD, PCH, PCH_USD, UP, NOSH, 
                             AF, ym, NetStockIssues, CompositeEquity_Log, MomentumReturn)]
    
    Merging_Return[, YearMatcher := year(marketdate) - 1]
    Merging_Return[month(marketdate) < 5, YearMatcher := YearMatcher - 1] # If the market date's month is before May -> decrease YearMatcher by 1
    
    Data_long_ST <- merge(Merging_Return, Merging_Accounting, by = c("dscode", "YearMatcher"), all.x = T)
    
    return(Data_long_ST)
  }

CA_Data_long_ST <- ST_merge(CA_Acc_12345610, CA_Return_DS05_DS07_DS08_789)  
DE_Data_long_ST <- ST_merge(DE_Acc_12345610, DE_Return_DS05_DS07_DS08_789)
FR_Data_long_ST <- ST_merge(FR_Acc_12345610, FR_Return_DS05_DS07_DS08_789)
GB_Data_long_ST <- ST_merge(GB_Acc_12345610, GB_Return_DS05_DS07_DS08_789)
IT_Data_long_ST <- ST_merge(IT_Acc_12345610, IT_Return_DS05_DS07_DS08_789)
JP_Data_long_ST <- ST_merge(JP_Acc_12345610, JP_Return_DS05_DS07_DS08_789)
US_Data_long_ST <- ST_merge(US_Acc_12345610, US_Return_DS05_DS07_DS08_789)


# Calculating Excess Returns

#library(readxl)
#RiskFreeRate <- read_excel("C:/Data/RiskFreeRate.xlsx")
#save(RiskFreeRate, file = "C:/Data/Final Data/RiskFreeRate.RData")

excessReturn <- function(Data_long) {
    
    load("C:/Data/Final Data/RiskFreeRate.RData")
    RiskFreeRate <- as.data.table(RiskFreeRate)
    
    # Convert ym to match with Data_long's ym format (yearmon)
    RiskFreeRate[, ym := as.yearmon(paste0(substr(ym, 1, 4), "-", substr(ym, 5, 6)), "%Y-%m")]
   
    Data_long[, ymMatcher := ym]
  
    Data_long <- merge(Data_long, RiskFreeRate, by.x = c("ymMatcher"), by.y = c("ym"), all.x = T)
    Data_long <- Data_long[, ymMatcher := NULL]
    
    Data_long <- Data_long[, ExcessReturn_USD := RET_USD - RF ] 
    
    #rm(list = "RiskFreeRate")
    
    return(Data_long)   
    
  }

CA_Data_long_ST <- excessReturn(CA_Data_long_ST)
DE_Data_long_ST <- excessReturn(DE_Data_long_ST)
FR_Data_long_ST <- excessReturn(FR_Data_long_ST)
GB_Data_long_ST <- excessReturn(GB_Data_long_ST)
IT_Data_long_ST <- excessReturn(IT_Data_long_ST)
JP_Data_long_ST <- excessReturn(JP_Data_long_ST)
US_Data_long_ST <- excessReturn(US_Data_long_ST)

# Calculating Market Return 

marketReturn <- function(Data_Long) {
  
  x <- Data_Long[, .(dscode, ym, RET_USD, ExcessReturn_USD, MV_USD)]
  
  # Calculation of total market value and weighted excess return per asset, total market excess return per month
  
  # Calculate the lagged MV_USD
  lag <- x[, .(dscode, ym, MV_USD)]
  names(lag)[3] <- "LagMV_USD"
  lag[, ym := ym + 1/12]
  
  x <- merge(x, lag, by.x = c("dscode", "ym"), by.y = c("dscode", "ym"), all.x = TRUE)
  
  # Aggregate the lagged MV_USD
  lagTotalMV <- lag[, .(LagTotal.MV_USD = sum(LagMV_USD, na.rm = TRUE)), by = "ym"]
  
  x <- merge(x, lagTotalMV, by = "ym", all.x = TRUE)
  
  # Calculate weighted returns and excess returns using lagged values
  x[, TotalMV.Weighted.Return := RET_USD * (LagMV_USD / LagTotal.MV_USD), by = "ym"]
  x[, TotalMV.Weighted.ExReturn := ExcessReturn_USD * (LagMV_USD / LagTotal.MV_USD), by = "ym"]
  
  # Aggregate weighted returns and excess returns to get market level metrics
  market1 <- x[, .(Market.RET_USD = sum(na.omit(TotalMV.Weighted.Return), na.rm = TRUE)), by = "ym"]
  market2 <- x[, .(Market.ExRET_USD = sum(na.omit(TotalMV.Weighted.ExReturn), na.rm = TRUE)), by = "ym"]
  
  # Merge market level metrics back into the dataset
  x <- merge(x, market1, by = "ym", all.x = TRUE)
  x <- merge(x, market2, by = "ym", all.x = TRUE)
  
  x <- x[, .(dscode, ym, LagMV_USD, Total.MV_USD, LagTotal.MV_USD, TotalMV.Weighted.Return, TotalMV.Weighted.ExReturn, Market.RET_USD, Market.ExRET_USD)]
  output <- merge(Data_Long, x, by = c("dscode", "ym"), all.x = TRUE)
  
  return(output)
}

CA_Data_long_ST <- marketReturn(CA_Data_long_ST)
DE_Data_long_ST <- marketReturn(DE_Data_long_ST)
FR_Data_long_ST <- marketReturn(FR_Data_long_ST)
GB_Data_long_ST <- marketReturn(GB_Data_long_ST)
IT_Data_long_ST <- marketReturn(IT_Data_long_ST)
JP_Data_long_ST <- marketReturn(JP_Data_long_ST)
US_Data_long_ST <- marketReturn(US_Data_long_ST)
  

# Aggregate the data together
AGGR_Data_long_ST <- rbind(CA_Data_long_ST, DE_Data_long_ST, FR_Data_long_ST, GB_Data_long_ST, IT_Data_long_ST, JP_Data_long_ST, US_Data_long_ST)
  
# Ordering Data 
## Beside some metrics in ds_monthly_data and ws_yearly_data, Data_long mainly includes:
### 11 anomalies: GPtoAsset; ROA; AssetGrowth; NetOperatingAssets; Accruals; InvestmentToAsset; NetStockIssues; CompositeEquity_Log; MomentumReturn; O_Score
### Market Return: LagMV_USD; Total.MV_USD -> LagTotal.MV_USD -> TotalMV.Weighted.Return -> Market.RET_USD; RF -> ExcessReturn_USD -> TotalMV.Weighted.ExReturn -> Market.ExRET_USD; 

order_Data <- function(Data_long) {
    
    Data_long <- Data_long[, .(dscode, ym, region.x, marketdate, MV, MV_USD, LagMV_USD, Total.MV_USD, LagTotal.MV_USD, RET, RET_USD, RF, 
                        ExcessReturn_USD, TotalMV.Weighted.Return, TotalMV.Weighted.ExReturn, Market.RET_USD, Market.ExRET_USD, PCH, PCH_USD,
                        UP, NOSH, AF, YearMatcher, IdMatcher_ACC, ITEM7021, YEAR, ITEM1001, ITEM1051, ITEM1151, ITEM1250, ITEM1551,
                        ITEM1651, ITEM2001, ITEM2005,ITEM2101, ITEM2301, ITEM2999, ITEM3051, ITEM3063, ITEM3101, WC, ITEM3251, ITEM3255, ITEM3351,
                        ITEM3426, ITEM3451, ITEM3501, ITEM3999, ITEM4201, ITEM5101, ITEM5202, ITEM5301, ITEM8001, ITEM18155, ITEM18191, ITEM18198,
                        GPtoAsset, ROA, AssetGrowth, NetOperatingAssets, Accruals, InvestmentToAsset, NetStockIssues, CompositeEquity_Log,
                        MomentumReturn, O_Score)]
    
    Data_long <- Data_long[order(dscode, ym)]
    
    return(Data_long)
  }

CA_Data_long_ST <- order_Data(CA_Data_long_ST)
DE_Data_long_ST <- order_Data(DE_Data_long_ST)
FR_Data_long_ST <- order_Data(FR_Data_long_ST)
GB_Data_long_ST <- order_Data(GB_Data_long_ST)
IT_Data_long_ST <- order_Data(IT_Data_long_ST)
JP_Data_long_ST <- order_Data(JP_Data_long_ST)
US_Data_long_ST <- order_Data(US_Data_long_ST)

AGGR_Data_long_ST <- order_Data(AGGR_Data_long_ST)

setwd("C:/Data/Anomalies")

## Canada
save(CA_Data_long_ST, file = "CA_Data_long_ST.RData")

## Germany
save(DE_Data_long_ST, file = "DE_Data_long_ST.RData")

## France
save(FR_Data_long_ST, file = "FR_Data_long_ST.RData")
  
## United Kingdom
save(GB_Data_long_ST, file = "GB_Data_long_ST.RData")

## Italy
save(IT_Data_long_ST, file = "IT_Data_long_ST.RData")

## Japan
save(JP_Data_long_ST, file = "JP_Data_long_ST.RData")

## United States
save(US_Data_long_ST, file = "US_Data_long_ST.RData")

## Aggregated Market
save(AGGR_Data_long_ST, file = "AGGR_Data_long_ST.RData")

#rm(list = ls())  
  
