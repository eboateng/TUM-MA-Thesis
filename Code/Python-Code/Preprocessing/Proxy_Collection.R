
compustat <- read.csv("C:/Users/Public/Data Storage/WRDS/Compustat/fundamental_annual.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
bwc <- readxl::read_excel("C:/Users/ge27xix/SEC/Processed10-X/data/Cluster_Proxies/Colloborate/bcwlist.xlsx")
JV <- readxl:: read_excel("C:/Users/ge27xix/Untitled Folder/SDC Platinum _64-2000.xlsx")
compustat_lag <- compustat %>%
  select(gvkey,fyear,sale,rect,invt)


#---- Culture Proxies -----
#----Miscelanoues
#Price - Earnings Ratio 
PER <- compustat %>%
  transmute(gvkey,fyear,
            per = prcc_f/epspx)
# Dividend Payout Ratio
DPR <- compustat %>%
  transmute(gvkey,fyear,
            dpr = dvc/ibadj)

#Dividend Yield 
DY <- compustat %>%
  transmute(gvkey,fyear,
            dy = (dvpsx_f/prcc_f)*100)
# Working Capital
WC <- compustat %>%
  transmute(gvkey,fyear,
            wc = (act -lct))

# 
#---- Create-----
RD <- compustat %>%
  transmute(gvkey,fyear,
            rdsa = xrd/sales,
            rdat = xrd/at)

#---- Compete-----
# HHI Index
HHI <- compustat %>%
  select(gvkey,fyear,sales)

#---- Control-----
#1. Inventory Turnover Ratio
ITR <- compustat %>%
  transmute(gvkey,fyear,
            itr1 = cogs/((invt + invt_lag)/2),
            itr2 = sale/((invt + invt_lag)/2))
            
#2. Accounts Receivable Turnover Ratio
ART <- compustat %>%
  transmute(gvkey,fyear,
            art = sales/((rect + rect_lag)/2))
#3. Accounts Payable Turnover Ratio
#APT <- compustat %>%
#  transmute(apt = ap/((rect + rect_lag)/2))
#4. Asset Turnover Ratio
ATR <- compustat %>%
  transmute(gvkey,fyear,
            atr = sales/((at + at_lag)/2))
#5. Debt/Equity Ratios
DER <- compustat %>%
  transmute(der = dltt/ceq)
# Total Liabilities - Definition 1 
TL <- compustat %>%
  transmute(gvkey,fyear,
            TL  = lt/ceq)
# Quick Ratio (Acid Test) 
TL <- compustat %>%
  transmute(gvkey,fyear,
            QR  = (che+rect)/(lct))

# Working Capital ratio
WCR <- compustat %>%
  transmute(gvkey,fyear,
            wcr = (act/lct))

#---- Colloborate-----