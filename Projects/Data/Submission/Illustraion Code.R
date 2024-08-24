#############################################################################################################################

# Illustrate the sentiment index and market overview

# Output: 

########################################################### Set Up ##########################################################
install.packages(c("readxl", "writexl", "ggplot2", "tidyverse", "lubridate"))
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)
library(lubridate)

############################################# Illustrate Investor Sentiment Indexes ##########################################

SI_file <- "C:/Data/Sentiment Indexes/Sentiment_Index.xlsx"

# Read all sheets
sheet_names <- excel_sheets(SI_file)

for (sheet in sheet_names) {
  df <- read_excel(SI_file, sheet = sheet)
  write.csv(df, paste0("C:/Data/Sentiment Indexes/", sheet, ".csv"), row.names = FALSE)
}

# Initialize an empty list to store data
data_list <- list()

for (sheet in sheet_names) {
  df <- read.csv(paste0("C:/Data/Sentiment Indexes/", sheet, ".csv"))
  df$date <- ymd(paste0(df$date, "28")) # Convert yyyymm to Date format (28 as dd presenting the last trading date)
  df$country <- sheet
  data_list[[sheet]] <- df
}

# Combine all data frames into one
combined_df <- bind_rows(data_list)

# Stock Index Sentiment

sentiment_df <- combined_df %>%
  filter(year(date) >= 1998) %>% # Start from sample period
  select(date, country, "Stock.Index.Sentiment")

plot_index_sentiment <- ggplot(sentiment_df, aes(x = date, y = `Stock.Index.Sentiment`, color = country)) +
  geom_line() + 
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),  
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal",  
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Stock Index Sentiment across G7",
       x = "Year",
       y = "Stock Index Sentiment") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) # Control the legend layout

# Stock Index Buzz

buzz_df <- combined_df %>%
  filter(year(date) >= 1998) %>%
  select(date, country, "Stock.Index.Buzz")

plot_index_buzz <- ggplot(buzz_df, aes(x = date, y = `Stock.Index.Buzz`, color = country)) +
  geom_line() + 
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),  
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal",  
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Stock Index Buzz across G7",
       x = "Year",
       y = "Stock Index Buzz") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) 

# Stock Index Fear

fear_df <- combined_df %>%
  filter(year(date) >= 1998) %>%
  select(date, country, "Stock.Index.Fear")

plot_index_fear <- ggplot(fear_df, aes(x = date, y = `Stock.Index.Fear`, color = country)) +
  geom_line() + 
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),  
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal",  
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Stock Index Fear across G7",
       x = "Year",
       y = "Stock Index Fear") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

setwd("C:/Data/Illustration")
ggsave("C:/Data/Illustration/Stock_Index_Sentiment_Plot.png", plot = plot_index_sentiment, width = 14, height = 8, dpi = 300)
ggsave("C:/Data/Illustration/Stock_Index_Buzz_Plot.png", plot = plot_index_buzz, width = 14, height = 8, dpi = 300)
ggsave("C:/Data/Illustration/Stock_Index_Fear_Plot.png", plot = plot_index_fear, width = 14, height = 8, dpi = 300)

################################################## Illustrate Market Overview ################################################
library(dplyr)
library(lubridate)

mispricing_paths <- c("C:/Data/Mispricing/usa_mispricing.csv",
                      "C:/Data/Mispricing/jpn_mispricing.csv",
                      "C:/Data/Mispricing/can_mispricing.csv",
                      "C:/Data/Mispricing/gbr_mispricing.csv",
                      "C:/Data/Mispricing/fra_mispricing.csv",
                      "C:/Data/Mispricing/deu_mispricing.csv",
                      "C:/Data/Mispricing/ita_mispricing.csv")

regions <- c("USA", "JPN", "CAN", "GBR", "FRA", "DEU", "ITA")

data_list <- list()

for (i in seq_along(mispricing_paths)) {
  dt <- fread(mispricing_paths[i])
  dt[, region := regions[i]]
  setorder(dt, region, eom)
  dt <- dt[, .(id, eom, region, size_grp, shares, me_lag1, ret)]

  data_list[[i]] <- dt
}

combined_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

# Convert eom to a 'yearmon' object
combined_data[, date := as.yearmon(as.Date(as.character(eom), "%Y%m%d"), "%Y-%m")]

# Create a lagged dataset
combined_data_lagged <- combined_data %>%
  mutate(date = as.yearmon(format(date + 1/12, "%Y-%m"))) %>%
  rename_with(~paste0(., "_L1"), .cols = c(size_grp, shares))

# Join the original data set (all entries remain) with the lagged data set
combined_data_with_lags <- combined_data %>%
  left_join(combined_data_lagged, by = c("date", "id"))

combined_data_with_lags_relevant <- combined_data_with_lags[,.(id, date, region.x, size_grp, size_grp_L1, shares, shares_L1, me_lag1.x, ret.x)]
setorder(combined_data_with_lags_relevant, id, date, region.x)
setnames(combined_data_with_lags_relevant, 
         old = c("region.x", "size_grp_L1",       "shares_L1",  "me_lag1.x", "ret.x"),
         new = c("region", "Size_Group_L1", "Number_Shares_L1", "LagMV_USD", "RET_USD"))

# Extract year from date
combined_data_with_lags_relevant[, year := format(as.yearmon(date), "%Y")]

# Select and order columns
combined_data_with_lags_relevant <- combined_data_with_lags_relevant[, .(id, date, year, region, Size_Group_L1, Number_Shares_L1, LagMV_USD, RET_USD)]
setorder(combined_data_with_lags_relevant, date, region)

# Set the sample period
combined_data_with_lags_relevant_Period <- combined_data_with_lags_relevant[year >= 1998 & year <= 2020]

fwrite(combined_data_with_lags_relevant_Period, "C:/Data/Mispricing/merged_G7market_data.csv")

# Visualize the market size by year and region

MarketSize_YearRegion <- combined_data_with_lags_relevant_Period[, .(
  Total_Size = sum(LagMV_USD, na.rm = TRUE),
  Mean_Size = mean(LagMV_USD, na.rm = T), 
  Median_Size = median(LagMV_USD, na.rm = T)
), by = .(year, region)]

p <- ggplot(MarketSize_YearRegion, aes(x = as.numeric(year), y = Total_Size, group = region, color = region)) +
  geom_line() + 
  geom_point() +  
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  scale_x_continuous(breaks = seq(min(as.numeric(MarketSize_YearRegion$year)), max(as.numeric(MarketSize_YearRegion$year)), by = 2)) +
  labs(title = "Development of Yearly Market Size by Region",
       x = "Year",
       y = "Total Market Size (USD)",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

ggsave("C:/Data/Illustration/Market_Size_Development_Plot.png", plot = p, width = 14, height = 8, dpi = 300)

# Mean market size
p1 <- ggplot(MarketSize_YearRegion, aes(x = as.numeric(year), y = Mean_Size, group = region, color = region)) +
  geom_line() + 
  geom_point() +  
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  scale_x_continuous(breaks = seq(min(as.numeric(MarketSize_YearRegion$year)), max(as.numeric(MarketSize_YearRegion$year)), by = 2)) +
  labs(title = "Development of Yearly Average Market Size by Region",
       x = "Year",
       y = "Average Market Size (USD)",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

ggsave("C:/Data/Illustration/Market_Mean_Size_Development_Plot.png", plot = p1, width = 14, height = 8, dpi = 300)

# Median market size
p2 <- ggplot(MarketSize_YearRegion, aes(x = as.numeric(year), y = Median_Size, group = region, color = region)) +
  geom_line() + 
  geom_point() +  
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  scale_x_continuous(breaks = seq(min(as.numeric(MarketSize_YearRegion$year)), max(as.numeric(MarketSize_YearRegion$year)), by = 2)) +
  labs(title = "Development of Yearly Median Market Size by Region",
       x = "Year",
       y = "Median Market Size (USD)",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

ggsave("C:/Data/Illustration/Market_Median_Size_Development_Plot.png", plot = p2, width = 14, height = 8, dpi = 300)

# Visualize the development of Total_no_firms and the development of Mean_Share_Number per year and region

MarketOverview_YearRegion <- combined_data_with_lags_relevant_Period[, .(
  Total_no_firms = uniqueN(id),
  Mean_Share_Number = mean(Number_Shares_L1, na.rm = TRUE)
), by = .(year, region)]

p3 <- ggplot(MarketOverview_YearRegion, aes(x = as.numeric(year), y = Total_no_firms, group = region, color = region)) +
  geom_line() + 
  geom_point() +  
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  scale_x_continuous(breaks = seq(min(as.numeric(MarketOverview_YearRegion$year)), max(as.numeric(MarketOverview_YearRegion$year)), by = 2)) +
  labs(title = "Development of Total Number of Firms by Year and Region",
       x = "Year",
       y = "Total Number of Firms",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot for Total_no_firms
ggsave("C:/Data/Illustration/Total_no_firms_Development_Plot.png", plot = p3, width = 14, height = 8, dpi = 300)

# Visualize Mean Number of Shares per year and region
p4 <- ggplot(MarketOverview_YearRegion, aes(x = as.numeric(year), y = Mean_Share_Number, group = region, color = region)) +
  geom_line() + 
  geom_point() +  
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  scale_x_continuous(breaks = seq(min(as.numeric(MarketOverview_YearRegion$year)), max(as.numeric(MarketOverview_YearRegion$year)), by = 2)) +
  labs(title = "Development of Mean Number of Shares by Year and Region",
       x = "Year",
       y = "Mean Number of Shares",
       color = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot for Mean_Share_Number
ggsave("C:/Data/Illustration/Mean_Share_Number_Development_Plot.png", plot = p4, width = 14, height = 8, dpi = 300)

# Visualize the market size by region and size group

combined_data_with_lags_relevant_Period[, Size_Group_Custom := fifelse(Size_Group_L1 %in% c("mega", "large", "small"), 
                                                   Size_Group_L1, "others")] # Modify Size_Group_L1 for customized grouping

MarketSize_GroupRegion <- combined_data_with_lags_relevant_Period[, .(Total_Size = sum(LagMV_USD, na.rm = TRUE)), 
                                        by = .(region, Size_Group_Custom)]

g <- ggplot(data = MarketSize_GroupRegion, aes(x = region, y = Total_Size, fill = Size_Group_Custom)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Market Size by Region and Group",
       x = "Region",
       y = "Total Market Size",
       fill = "Size Group") +
  theme_minimal()

ggsave("C:/Data/Illustration/Market_Size_Plot.png", plot = g, width = 14, height = 8, dpi = 300)

# Market overview by region
install.packages("moments")
library(moments)   
options(scipen = 999)

combined_data_with_lags_relevant_Period[, Size_Group_Custom := fifelse(Size_Group_L1 %in% c("mega", "large", "small"), 
                                                                       Size_Group_L1, "others")]

# Count the number of firms in each size group by region
SizeGroup_Counts <- combined_data_with_lags_relevant_Period[, .(Count = .N), by = .(region, Size_Group_Custom)]

# Aggregate total number of firms per region by summing the counts of each group
TotalFirms_Per_Region <- SizeGroup_Counts[, .(Total_Firms = sum(Count)), by = region]

# Merge total firms with size group counts
SizeGroup_Percentages <- merge(SizeGroup_Counts, TotalFirms_Per_Region, by = "region")

# Calculate percentage of each size group in each region
SizeGroup_Percentages[, Percentage := round((Count / Total_Firms) * 100, 2)]

SizeGroup_Percentages_Wide <- dcast(SizeGroup_Percentages, region ~ Size_Group_Custom, value.var = "Percentage")


MarketOverview_Variable <- combined_data_with_lags_relevant_Period[,.(
                                         Total_no_firms = uniqueN(id),
                                         Total_Size = sum(LagMV_USD, na.rm = T),  ## the total size of the market
                                         Mean_Size = round(mean(LagMV_USD, na.rm = T), 2), 
                                         Median_Size = round(median(LagMV_USD, na.rm = T), 2), 
                                         Max_Size = round(max(LagMV_USD, na.rm = T), 2), 
                                         Min_Size = round(min(LagMV_USD, na.rm = TRUE), 2),
                                         Range_Size = round(max(LagMV_USD, na.rm = TRUE) - min(LagMV_USD, na.rm = TRUE), 2),
                                         Mean_Performance = round(mean(RET_USD, na.rm = T), 2),    ## the overall market performance
                                         Median_Performance = round(median(RET_USD, na.rm = T), 3),    ## the typical performance
                                         Market_Volatility = round(sd(RET_USD, na.rm = T), 2),  ## the market volatility
                                         Positive_Returns_Percentage = round(sum(RET_USD > 0, na.rm = T) / .N * 100, 2),  ## the percentage of firms with positive returns
                                         Negative_Returns_Percentage = round(sum(RET_USD < 0, na.rm = T) / .N * 100, 2), 
                                         Skewness_Returns = round(skewness(RET_USD, na.rm = TRUE), 2),
                                         Kurtosis_Returns = round(kurtosis(RET_USD, na.rm = TRUE) - 3, 2),  # Excess kurtosis
                                         Total_Share_Number = sum(Number_Shares_L1, na.rm = T),
                                         Mean_Share_Number = round(mean(Number_Shares_L1, na.rm = T), 2), 
                                         Median_Share_Number = round(median(Number_Shares_L1, na.rm = T), 2)
), by = region]


# Merge percentages with market overview
MarketOverview_Variable <- merge(MarketOverview_Variable, SizeGroup_Percentages_Wide, by = "region", all.x = TRUE)

MarketOverview_Transposed <- dcast(melt(MarketOverview_Variable, id.vars = "region"), 
                                   variable ~ region, value.var = "value")


# Visualize the returns and volatility by region

s <- ggplot(data = MarketOverview_Variable, aes(x = Market_Volatility, y = Mean_Performance, label = region)) +
  geom_point(aes(color = region), size = 4) + 
  geom_text(vjust = 1.5, hjust = 1.5) +
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  labs(
    title = "Monlthly Returns vs. Market Volatility by Region between 1998 and 2020",
    x = "Market Volatility",
    y = "Monthly Returns",
    color = "Region"
  ) +
  theme_minimal() + 
  theme(legend.position = "right")

ggsave("C:/Data/Illustration/Market_Returns_vs_Volatility.png", plot = s, width = 14, height = 8, dpi = 300)

setDT(combined_data_with_lags_relevant_Period)

# Define the function to calculate autocorrelation
calculate_autocorrelation <- function(data, region) {
  region_data <- data[region == region, .(RET_USD)]
  region_data <- na.omit(region_data)
  acf_values <- acf(region_data$RET_USD, plot = FALSE)$acf
  return(acf_values)
}

# Calculate autocorrelation for each region
autocorrelation_results <- combined_data_with_lags_relevant_Period[, .(Autocorrelation = list(calculate_autocorrelation(.SD, region))), by = region]

# Unlist and organize the autocorrelation results
autocorrelation_results <- autocorrelation_results %>%
  mutate(Autocorrelation = lapply(Autocorrelation, function(x) data.frame(Lag = seq_along(x) - 1, Autocorrelation = x))) %>%
  unnest(Autocorrelation)

a <- ggplot(autocorrelation_results, aes(x = Lag, y = Autocorrelation, color = region)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  labs(title = "Autocorrelation of Returns by Region",
       x = "Lag",
       y = "Autocorrelation") +
  theme_minimal()

ggsave("C:/Data/Illustration/Market_Returns_Autocorrelation.png", plot = a, width = 14, height = 8, dpi = 300)

# Visualize the market size, the number of firms and stocks by region

b <- ggplot(MarketOverview_Variable, aes(x = Total_no_firms, y = Total_Size, size = Total_Share_Number, color = region)) +  # Added color aesthetic
  geom_point(alpha = 0.6) +
  geom_text(aes(label = region), vjust = -1, hjust = 1.5) +
  scale_size_continuous(name = "Total Number of Shares") +
  scale_color_manual(values=c("pink", "red", "green", "orange", "grey", "purple", "blue")) +
  labs(
    title = "Firm Count, Total Market Size, and Share Volume by Region from 1998 to 2020",
    x = "Total Number of Firms",
    y = "Total Market Size (USD)",
    caption = "Bubble size represents total number of shares.",
    color = "Region"  # This label describes what the color represents
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave("C:/Data/Illustration/FirmCount_MarketSize_ShareVolume.png", plot = b, width = 14, height = 8, dpi = 300)

# Calculate the correlation of sentimen index Mood and B&W indices
library(readxl)
sentiment <- read_excel("C:/Data/Sentiment Indexes/Correlation_Mood_BW(98_20).xlsx", sheet = "Tabelle1")

# Calculate the correlation
correlation <- cor(sentiment$Stock_Index_Sentiment, sentiment$SENT_ORTH, use = "complete.obs")

# Print the correlation
print(correlation)



