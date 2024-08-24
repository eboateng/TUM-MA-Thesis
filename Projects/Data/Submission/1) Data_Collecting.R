#############################################################################################################################

# Collect data from WRDS
## - Worldscope 1996 - 2020
## - Datastream 1996-01-01 - 2020-12-31 (last monthly trading date)
## - Worldscope Datastream Link (exclude enddate < 1996-01-01 / startdate > 2020-12-31)

# Output: ws_yearly_data and ds_monthly_data

####################################################### Worldscope Data #####################################################

library(dplyr)
library(data.table)
library(zoo)

Canada <- read.csv("C:/Data/Worldscope/Canada.csv")
Germany <- read.csv("C:/Data/Worldscope/Germany.csv")
France <- read.csv("C:/Data/Worldscope/France.csv")
UK <- read.csv("C:/Data/Worldscope/United Kingdom.csv")
Italy <- read.csv("C:/Data/Worldscope/Italy.csv")
Japan <- read.csv("C:/Data/Worldscope/Japan.csv")
US <- read.csv("C:/Data/Worldscope/United States.csv")

# Combine all data together as data table and add the column 'region' 

Canada <- as.data.table(Canada)[, region := 'CA']
Germany <- as.data.table(Germany)[, region := 'DE']
France <- as.data.table(France)[, region := 'FR']
UK <- as.data.table(UK)[, region := 'GB']
Italy <- as.data.table(Italy)[, region := 'IT']
Japan <- as.data.table(Japan)[, region := 'JP']
US <- as.data.table(US)[, region := 'US']

combined_data <- rbindlist(list(Canada, Germany, France, UK, Italy, Japan, US), use.names = TRUE, fill = TRUE) # ------ Fundamental Annual

# Add the column 'Working Capital' (Current Asset - Current Liabilities)
combined_data[, WC := ITEM2201 - ITEM3101]  

# Merge Fundamental Annual (country filtered) and Stock Data (country not filtered)

stock_data <- read.csv("C:/Data/Worldscope/stock_data.csv")  # ----------------------- Stock Data 
setDT(stock_data)
yearly_data <- merge(combined_data, stock_data, by = c("code", "year_"), all.x = TRUE) # ---------------------- the final Worldscope data

# Make the Worldscope data tidy

## Delete unnecessary columns
yearly_data[, c("freq.x", "freq.y", "ITEM6105.y", "ITEM6026") := NULL]

## Rename columns
setnames(yearly_data, old = c("year_", "ITEM6105.x"), new = c("YEAR", "ITEM6105"))

## Reorder columns
setcolorder(yearly_data, c("code", "region", setdiff(names(yearly_data), c("code", "region"))))
setorder(yearly_data, region, code, YEAR)

fwrite(yearly_data, "C:/Data/Final Data/yearly_data.csv", row.names = FALSE)

# Check for consecutive years availability

worldscope_data <- fread("C:/Data/Final Data/yearly_data.csv")
setDT(worldscope_data)

expected_years <- 1996:2020

# Check for consecutive yearly data for a given region
check_consecutive_years <- function(data, expected_years) {
  
  actual_years <- unique(data$YEAR)
  all_expected_present <- all(expected_years %in% actual_years) # Check if all expected years are present
  missing_years <- expected_years[!expected_years %in% actual_years]

  list(all_expected_present = all_expected_present, missing_years = missing_years)
}

results <- worldscope_data[, .(check_consecutive_years(.SD, expected_years)), by = .(region)]
print(results) # -> Find for all regions all months consecutive


###################################################### Datastream data ######################################################

# install.packages("RPostgres")
library(RPostgres)

wrds <- dbConnect(RPostgres::Postgres(),
                  dbname = "wrds",
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737,
                  user = "ge42let",
                  password = "Alexandrafu123456.",
                  sslmode = "require")

# Initialize a list to store each year's data as a data table
data_list <- list()

# Loop through each year to fetch data in chunks
for (year in 1996:2020) {
  query <- sprintf("
    SELECT 
        main.infocode,
        main.dscode,
        main.region,
        main.marketdate, 
        main.mktcap, main.mktcap_usd,
        main.ri, main.ri_usd,
        main.close, main.close_usd,
        main.numshrs,
        main.cumadjfactor 
    FROM 
        (SELECT 
             infocode,
             dscode,
             region,
             marketdate, 
             mktcap, mktcap_usd,
             ri, ri_usd,
             close, close_usd,
             numshrs,
             cumadjfactor,
             DATE_PART('year', marketdate) as year,
             DATE_PART('month', marketdate) as month
         FROM tr_ds_equities.wrds_ds2dsf
         WHERE region IN ('CA','FR','DE','IT','JP','GB','US') 
           AND marketdate BETWEEN '%d-01-01' AND '%d-12-31'
        ) main
    INNER JOIN
        (SELECT 
             MAX(marketdate) as maxdate,
             DATE_PART('year', marketdate) as year,
             DATE_PART('month', marketdate) as month
         FROM tr_ds_equities.wrds_ds2dsf
         WHERE region IN ('CA','FR','DE','IT','JP','GB','US') 
           AND marketdate BETWEEN '%d-01-01' AND '%d-12-31'
         GROUP BY year, month
        ) sub
    ON main.marketdate = sub.maxdate AND main.year = sub.year AND main.month = sub.month
    ORDER BY main.region, main.marketdate
  ", year, year, year, year)
  
  res <- dbSendQuery(wrds, query)
  data <- dbFetch(res)
  dbClearResult(res)
  
  setDT(data)
  data_list[[as.character(year)]] <- data
}

final_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

dbDisconnect(wrds)

fwrite(final_data, "C:/Data/Datastream/ds_Target.csv")

ds_all_1 <- fread("C:/Data/Datastream/ds_Target.csv")

# Check for consecutive months availability

# Create the identifier for year-month
ds_all_1[, marketdate := as.IDate(marketdate)]
ds_all_1[, `:=` (year = year(marketdate), month = month(marketdate))] 
ds_all_1[, year_month := paste(year, sprintf("%02d", month), sep = "-")] 

# Count unique year-month combinations per region per year
year_month_counts <- ds_all_1[, .(unique_months = uniqueN(year_month)), by = .(region, year)] 
year_month_counts[, expected := 12] # Expected count: 12 per year

# Adjust for start or end years that might not have complete data
year_month_counts[year == 1996 | year == 2020, expected := ifelse(year == 1996, 12 - 0, # Adjust the subtracted value based on start month in 1996
                                                                  12 - 0)] # Adjust based on end month in 2020

# Flag years that do not meet the expectation
year_month_counts[, meets_expectation := unique_months == expected]
View(year_month_counts) # -> Find there are missing year_month

# Look for missing year_month combinations

library(lubridate)

# Generate all possible combinations of region, year, and month
unique_regions <- unique(ds_all_1$region)
all_years <- seq(min(ds_all_1$year), max(ds_all_1$year))
all_months <- sprintf("%02d", 1:12)

# Cartesian join to create all expected region-year-month combinations
all_combinations <- CJ(region = unique_regions, year = all_years, month = all_months)
all_combinations[, year_month := paste(year, month, sep = "-")]

# Merge with observed data to find missing combinations
observed_combinations <- unique(ds_all_1[, .(region, year_month)])
setkey(all_combinations, region, year_month)
setkey(observed_combinations, region, year_month)

# Find missing combinations by performing an anti-join
missing_combinations <- all_combinations[!observed_combinations]
setDT(missing_combinations)

# Query for missing year_month combinations

wrds <- dbConnect(RPostgres::Postgres(),
                  dbname = "wrds",
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737,
                  user = "ge42let",
                  password = "Alexandrafu123456.",
                  sslmode = "require")

data_list <- list()

for (i in 1:nrow(missing_combinations)) {
  region <- missing_combinations$region[i]
  year <- missing_combinations$year[i]
  month <- missing_combinations$month[i]
  
  # Select the most recent available trading day within the target month
  query <- sprintf("
    SELECT 
      main.infocode,
      main.dscode,
      main.region,
      main.marketdate, 
      main.mktcap, main.mktcap_usd,
      main.ri, main.ri_usd,
      main.close, main.close_usd,
      main.numshrs,
      main.cumadjfactor 
    FROM 
      tr_ds_equities.wrds_ds2dsf main
      INNER JOIN (
        SELECT 
          MAX(marketdate) as maxdate,
          DATE_PART('year', marketdate) as year,
          DATE_PART('month', marketdate) as month
        FROM 
          tr_ds_equities.wrds_ds2dsf
        WHERE 
          region = '%s' AND 
          DATE_PART('year', marketdate) = %s AND 
          DATE_PART('month', marketdate) = %s
        GROUP BY 
          DATE_PART('year', marketdate), DATE_PART('month', marketdate)
      ) sub ON main.marketdate = sub.maxdate AND main.region = '%s'
    ORDER BY 
      main.region, main.marketdate
  ", region, year, month, region)
  
  res <- dbSendQuery(wrds, query)
  data <- dbFetch(res)
  dbClearResult(res)
  
  setDT(data)
  data_list[[i]] <- data
}

supplement_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

dbDisconnect(wrds)

fwrite(supplement_data, "C:/Data/Datastream/ds_Target_2.csv")

# Merge two datasets together to get the complete market data

ds_all_1 <- fread("C:/Data/Datastream/ds_Target.csv")
ds_all_2 <- fread("C:/Data/Datastream/ds_Target_2.csv")
ds_all <- rbindlist(list(ds_all_1, ds_all_2))
fwrite(ds_all, "C:/Data/Datastream/ds_Target_final.csv")  # ----------------------- the final Datastream Daily Stock File

# Check again for consecutive months availability

ds_all[, `:=` (year = year(marketdate), month = month(marketdate))] 
ds_all[, year_month := paste(year, sprintf("%02d", month), sep = "-")] 

year_month_counts <- ds_all[, .(unique_months = uniqueN(year_month)), by = .(region, year)] 
year_month_counts[, expected := 12]

year_month_counts[year == 1996 | year == 2020, expected := ifelse(year == 1996, 12 - 0, 12 - 0)]

year_month_counts[, meets_expectation := unique_months == expected]
View(year_month_counts) # -> Find for all regions all months consecutive

# Make the Datastream data tidy

ds_all <- fread("C:/Data/Datastream/ds_Target_final.csv")
setDT(ds_all)
ds_all[, marketdate := as.Date(marketdate)]
setorder(ds_all, region, dscode, marketdate)

# initialize columns RET, RET_USD, PCH and PCH_USD
ds_all[, `:=` (RET = NA_real_, RET_USD = NA_real_, PCH = NA_real_, PCH_USD = NA_real_)]

# Calculate the difference in days to the previous row within each dscode group
ds_all[, day_diff := marketdate - shift(marketdate, 1), by = dscode]

# Check for a one-month difference within a tolerance
ds_all[, one_month_diff := abs(day_diff) < 37, by = dscode]

# Calculate ym ()
ds_all[, ym := as.yearmon(marketdate)]

# Calculate RET, RET_USD, PCH, PCH_USD taking into account the actual time difference
ds_all[, `:=` (
  RET = ifelse(one_month_diff, ri / shift(ri, 1) - 1, NA_real_),
  RET_USD = ifelse(one_month_diff, ri_usd / shift(ri_usd, 1) - 1, NA_real_),
  PCH = ifelse(one_month_diff, close / shift(close, 1) - 1, NA_real_),
  PCH_USD = ifelse(one_month_diff, close_usd / shift(close_usd, 1) - 1, NA_real_)
), by = .(dscode)]

ds_data <- ds_all[, .(infocode, dscode, region, marketdate, MV = mktcap, MV_USD = mktcap_usd, RET, RET_USD, PCH, PCH_USD, 
                      UP = close, NOSH = numshrs, AF = cumadjfactor, ym = as.yearmon(ym))]

## MV and MV_USD (unit: mio.)
ds_data[, `:=` (
  MV = round(MV / 1000000, 2),
  MV_USD = round(MV_USD / 1000000, 2))]

## RET and RET_USD, PCH and PCH_USD (unit: %)
ds_data[, `:=` (
  RET = round(RET * 100, 2),
  RET_USD = round(RET_USD * 100, 2))]
ds_data[, `:=` (
  PCH = round(PCH * 100, 1),
  PCH_USD = round(PCH_USD * 100, 1))]

## unadjusted price UP (without the adjustment of dividends and corporate actions)
## If UP = RET -> no adjustment of dividends and corporate actions during the month
ds_data[, UP := round(UP, 2)]

## NOSH (unit: tsd.)
ds_data[, NOSH := round(NOSH / 1000, 0)]

## AF
ds_data[, AF := as.numeric(AF)]
ds_data[, AF := round(AF, 4)]

fwrite(ds_data, "C:/Data/Final Data/ds_monthly_data.csv")
save(ds_data, file = "C:/Data/Final Data/ds_monthly_data.RData")

################################################# Worldscope Datastream Link ###############################################

library(RPostgres)

wrds <- dbConnect(RPostgres::Postgres(),
                  dbname = "wrds",
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737,
                  user = "ge42let",
                  password = "Alexandrafu123456.",
                  sslmode = "require")

link_data <- dbGetQuery(wrds, "
WITH RankedLinks AS (
  SELECT
    code,
    coderank,
    infocode,
    inforank,
    startdate,
    enddate,
    ROW_NUMBER() OVER (PARTITION BY infocode, startdate, enddate ORDER BY coderank ASC) AS rownum
  FROM wrdsapps_link_datastream_wscope.ds2ws_linktable
  WHERE NOT (enddate < '1996-01-01' OR startdate > '2020-12-31')
)
SELECT code, coderank, infocode, inforank, startdate, enddate
FROM RankedLinks
WHERE rownum = 1;
")

dbDisconnect(wrds)

setDT(link_data)
fwrite(link_data, "C:/Data/Final Data/link_data.csv") # ------------------------------------------- Worldscope Datastream Link


# Link Worldscope and Datastream with Worldscope Datastream Link
# -> match with identifier (add dscode to Worldscope according to date and rank)

datastream_data <- fread("C:/Data/Final Data/ds_monthly_data.csv")
worldscope_data <- fread("C:/Data/Final Data/yearly_data.csv")
linking_table <- fread("C:/Data/Final Data/link_data.csv")

datastream_data[, c("infocode", "dscode") := .(as.character(infocode), as.character(dscode))]
worldscope_data[, code := as.character(code)]
linking_table[, c("code", "infocode") := .(as.character(code), as.character(infocode))]

# Python_Coding
# ws_yearly_data & ds_monthly_data





