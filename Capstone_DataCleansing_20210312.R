#Data Science Capstone Project
#Merrimack College - Winter 2021

#Invoke the requried library packages
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(dlookr)
library(purrr)
library(psych)
# library(cluster)    # clustering algorithms
# library(factoextra) # clustering algorithms & visualization
# library(rpart) #Decision Tree
# library(rattle) #Decision Tree
# library(rpart.plot) #Decision Tree
# library(RColorBrewer) #Decision Trees

# rm(DRat, DRatings, DSFu, DSFund, DSFundamentals, DRatings_drops, DRat_drops, 
#    DSec, DSecurities, DSecurities_drops, DSec_drops, NACounts,DSFinalMerge,
#    DStocks, DSt, DStocks_drops, DSt_drops, SC, SM, Settles, DSM, combo, 
#    CompanyCheck, Final, DisCount, DSCAFS, DSFS, DSFSm)


#set the working directory to start with in your local machine
setwd ("D:/Merrimack/Capstone Project/R Work - Data Cleansing")

#Read in the Fundamentals data
DSFundamentals <- fread("D:/Merrimack/Capstone Project/Data/Fundamentals_DS.csv", header=TRUE, na.strings = c("", "NA"))
DSFundamentals <- as.data.frame(DSFundamentals)
DSFund <- filter(DSFundamentals, gsector == "25")
DSFund <- subset(DSFund, select = c("gvkey",
                                    "datadate",
                                    "datafmt",
                                    "tic",
                                    "conm",
                                    "ggroup",	#GIC Groups
                                    "gind",	#GIC Industries
                                    "gsector", #GIC Sectors
                                    "gsubind", #GIC Sub-Industries
                                    "sic", #Standard Industry Classification Code
                                    "idbflag", #International, Domestic, Both Indicator
                                    "loc", #Current ISO Country Code - Headquarters
                                    "state", #State/Province
                                    "costat",	#Active/Inactive Status Marker
                                    "acctstd", #Accounting Standard cat
                                    "auop", #Auditor Opinion
                                    "exchg", #Stock Exchange Code
                                    "dlrsn", #Research Co Reason for Deletion
                                    "act", #Current Assets Total cont
                                    "at", #Assets - Total
                                    "capx", #Capital Expenditures cont
                                    "ceq", #Common/Ordinary Equity - Total
                                    "che", #Cash and Short-Term Investments cont
                                    "ci", #Total Comprehensive Income cont
                                    "cshi", #Common Shares Issued
                                    "csho", #Common Shares Outstanding
                                    "dlc", #Debt in Current Liabilities - Total cont
                                    "dltt", #Long-Term Debt - Total cont
                                    "dv", #Cash Dividends (Cash Flow) cont
                                    "epspx", #Earnings Per Share (Basic) - Excluding Extraordinary Items
                                    "gdwl", #Goodwill
                                    #"gleps", #Gain/Loss Basic EPS Effect
                                    "gp", #Gross Profit (Loss)
                                    #"hedgegl", #Gain/Loss on Ineffective Hedges
                                    "icapt", #Invested Capital - Total
                                    "ivst", #Short-Term Investments - Total
                                    "lct", #Current Liabilities - Total
                                    "lse", #Liabilities and Stockholders Equity - Total
                                    "lt", #Liabilities - Total
                                    "mibt", #Noncontrolling Interests - Total - Balance Sheet
                                    "mkvalt", #Market Value - Total - Fiscal
                                    "ni", #Net Income (Loss)
                                    "rect", #Receivables - Total
                                    "revt", #Revenue Total
                                    "re", #Retained Earnings
                                    #"seteps", #Settlement (Litigation/Insurance) Basic EPS Effect
                                    "spce", #S&P Core Earnings
                                    "spceeps", #S&P Core Earnings EPS Basic
                                    "tfva", #Total Fair Value Assets
                                    "tfvl", #Total Fair Value Liabilities
                                    "txt", #Income Taxes - Total
                                    "txtubsettle", #Settlements with Tax Authorities  
                                    "wcap", #Working Capital (Balance Sheet)   
                                    "xopr" #Operating Expenses - Total
))

DSFu <- DSFund
DSFu <- filter(DSFu, datafmt != "SUMM_STD") 

NACounts <- as.data.frame(colSums(is.na(DSFu)))

# FundYear <- subset(DSFundamentals, select = c("gvkey","fyear","datafmt"))
# FundYear <- filter(FundYear, datafmt != "SUMM_STD") 
# FundSum <- tabyl(FundYear, gvkey, fyear)
# names(FundSum)[names(FundSum) == "2009"] <- "fy2009"
# names(FundSum)[names(FundSum) == "2010"] <- "fy2010"
# names(FundSum)[names(FundSum) == "2011"] <- "fy2011"
# names(FundSum)[names(FundSum) == "2012"] <- "fy2012"
# names(FundSum)[names(FundSum) == "2013"] <- "fy2013"
# FundSum <- FundSum %>% mutate(YearCount = rowSums(.[2:6]))

#Format datadate so it sorts correctly
DSFu$datadate = format(as.Date(DSFu$datadate, format = "%m/%d/%Y"),"%Y/%m/%d")

#Clean up FilingName
DSFu$conm <- toupper(DSFu$conm)
DSFu$conm <- str_trim(DSFu$conm, side = c("both"))
DSFu$conm <- str_replace_all(DSFu$conm, "[[:punct:]]", "")
DSFu$conm <- gsub("[[:space:]]", "_", DSFu$conm)
DSFu$FilingName <- str_replace_all(DSFu$FilingName, "CORPORATION", "CORP")
DSFu$FilingName <- str_replace_all(DSFu$FilingName, "INTERNATIONAL", "INTL")
DSFu$FilingName <- str_replace_all(DSFu$FilingName, "__ADR", "")
names(DSFu)[names(DSFu) == "conm"] <- "FilingName"

#Continuous
#Convert all NA values 0
DSFu$act[is.na(DSFu$act)] <- 0
DSFu$at[is.na(DSFu$at)] <- 0
DSFu$capx[is.na(DSFu$capx)] <- 0
DSFu$ceq[is.na(DSFu$ceq)] <- 0
DSFu$che[is.na(DSFu$che)] <- 0
DSFu$ci[is.na(DSFu$ci)] <- 0
DSFu$cshi[is.na(DSFu$cshi)] <- 0
DSFu$csho[is.na(DSFu$csho)] <- 0
DSFu$dlc[is.na(DSFu$dlc)] <- 0
DSFu$dltt[is.na(DSFu$dltt)] <- 0
DSFu$dv[is.na(DSFu$dv)] <- 0
DSFu$epspx[is.na(DSFu$epspx)] <- 0
DSFu$gdwl[is.na(DSFu$gdwl)] <- 0
#DSFu$gleps[is.na(DSFu$gleps)] <- 0
DSFu$gp[is.na(DSFu$gp)] <- 0
#DSFu$hedgegl[is.na(DSFu$hedgegl)] <- 0
DSFu$icapt[is.na(DSFu$icapt)] <- 0
DSFu$ivst[is.na(DSFu$ivst)] <- 0
DSFu$lct[is.na(DSFu$lct)] <- 0
DSFu$lse[is.na(DSFu$lse)] <- 0
DSFu$lt[is.na(DSFu$lt)] <- 0
DSFu$mibt[is.na(DSFu$mibt)] <- 0
DSFu$ni[is.na(DSFu$ni)] <- 0
DSFu$re[is.na(DSFu$re)] <- 0
DSFu$rect[is.na(DSFu$rect)] <- 0
DSFu$revt[is.na(DSFu$revt)] <- 0
#DSFu$seteps[is.na(DSFu$seteps)] <- 0
DSFu$spce[is.na(DSFu$spce)] <- 0
DSFu$spceeps[is.na(DSFu$spceeps)] <- 0
DSFu$tfva[is.na(DSFu$tfva)] <- 0
DSFu$tfvl[is.na(DSFu$tfvl)] <- 0
DSFu$txt[is.na(DSFu$txt)] <- 0
DSFu$txtubsettle[is.na(DSFu$txtubsettle)] <- 0
DSFu$wcap[is.na(DSFu$wcap)] <- 0
DSFu$xopr[is.na(DSFu$xopr)] <- 0
DSFu$mkvalt[is.na(DSFu$mkvalt)] <- 0

#NACounts <- as.data.frame(colSums(is.na(DSFu_Conts)))

DSFu_Conts <- DSFu
DSFu_Conts <- DSFu %>% group_by(gvkey) %>%
  summarize(
    act = mean(act),
    at = mean(at),
    capx = mean(capx),
    ceq = mean(ceq),
    che = mean(che),
    ci = mean(ci),
    cshi = mean(cshi),
    csho = mean(csho),
    dlc = mean(dlc),
    dltt = mean(dltt),
    dv = mean(dv),
    epspx = mean(epspx),
    gdwl = mean(gdwl),
    #gleps = mean(gleps),
    gp = mean(gp),
    #hedgegl = mean(hedgegl),
    icapt = mean(icapt),
    ivst = mean(ivst),
    lct = mean(lct),
    lse = mean(lse),
    lt = mean(lt),
    mibt = mean(mibt),
    ni = mean(ni),
    re = mean(re),
    rect = mean(rect),
    revt = mean(revt),
    #seteps = mean(seteps),
    spce = mean(spce),
    spceeps = mean(spceeps),
    tfva = mean(tfva),
    tfvl = mean(tfvl),
    txt = mean(txt),
    txtubsettle = mean(txtubsettle),
    wcap = mean(wcap),
    xopr = mean(xopr),
    mkvalt = mean(mkvalt)
  )

#Categorical
#Select the categorical, key, and filter variables from DSFu and merge records down to single row
#Eventually you will join this data frame with the averaged continuous data frame.
DSFu_Cats <- subset(DSFu,select = c("gvkey",
                                    "FilingName",
                                    "datadate",
                                    "datafmt",
                                    "tic",
                                    "ggroup",	#GIC Groups
                                    "gind",	#GIC Industries
                                    "gsector", #GIC Sectors
                                    "gsubind", #GIC Sub-Industries
                                    "sic", #Standard Industry Classification Code
                                    "idbflag", #International, Domestic, Both Indicator
                                    "loc", #Current ISO Country Code - Headquarters
                                    "state", #State/Province
                                    "costat",	#Active/Inactive Status Marker
                                    "acctstd", #Accounting Standard cat
                                    "auop", #Auditor Opinion
                                    "exchg", #Stock Exchange Code
                                    "dlrsn" #Research Co Reason for Deletion
))

#Take the categorical designations associated with the max date available
DSFu_Cats <- DSFu_Cats %>% group_by(gvkey) %>%
  filter(datadate == max(datadate))

#NACounts <- as.data.frame(colSums(is.na(DSFu_Cats)))
# unique(DSFu_Cats$auop)
#An assignment of three means 'No Opinion' or NA for us.
DSFu_Cats$auop[is.na(DSFu_Cats$auop)] <- 3

unique(DSFund$costat)

DSFu <- merge(DSFu_Cats, DSFu_Conts, by="gvkey")
#colnames(DSFu)[colSums(is.na(DSFu)) > 0]

rm(DSFu_Conts, DSFu_Cats, NACounts)

#DONE FUNDAMENTALS 20210309

##============================================================================

#Read in the Securities data
DSecurities <- fread("D:/Merrimack/Capstone Project/Data/Securities_DS.csv", header=TRUE)
DSecurities <- as.data.frame(DSecurities)
DSecurities <- filter(DSecurities, gsector == "25")
DSec <- subset(DSecurities, select = c("gvkey",	#Global Company Key
                                       "datadate",	#Data Date - Security Monthly
                                       "tic",	#Ticker Symbol
                                       "conm",	#Company Name
                                       "dvrate",	#Dividend Rate - Monthly
                                       "cshtrm",	#Trading Volume - Monthly
                                       "prccm",	#Price - Close - Monthly
                                       "prchm",	#Price - High - Monthly
                                       "prclm",	#Price - Low - Monthly
                                       "trt1m"	#Monthly Total Return
))

#Check how many unique company codes there are
#length(unique(DSecurities$gvkey))

#Add a row index column 
DSec$RowNum <- as.integer(rownames(DSec))
# NACounts <- as.data.frame(colSums(is.na(DSec)))

#Format datadate so it sorts correctly
DSec$datadate = format(as.Date(DSec$datadate, format = "%m/%d/%Y"),"%Y/%m/%d")

#Clean up FilingName
DSec$conm <- toupper(DSec$conm)
DSec$conm <- str_trim(DSec$conm, side = c("both"))
DSec$conm <- str_replace_all(DSec$conm, "[[:punct:]]", "")
DSec$conm <- gsub("[[:space:]]", "_", DSec$conm)
names(DSec)[names(DSec) == "conm"] <- "FilingName"

#Clean up Companies with multiple tickers
#Count how many rows per ticker per company and 
#Take the ticker with the largest row count
TickCount <- subset(DSec, select = c("gvkey","FilingName","tic"))
TickCount <- TickCount %>% 
  group_by(gvkey, FilingName, tic) %>% 
  summarise(count=n())

#Get the maximum row count from the previous step and apply it to all gvkeys
TickCount <- TickCount %>% 
  group_by(gvkey) %>% 
  mutate(max_tic = max(count))

#Check to see if a ticker count is the same as the max count per gvkey
#if a ticker count matches the max count, assign a 1
TickCount <- TickCount %>% 
  group_by(gvkey) %>% 
  mutate(is_max_tic = case_when(count == max_tic ~ "1"))

#Filter out rows that aren't labeled with a 1
TickCount <- TickCount %>% 
  filter(is_max_tic == "1") 

#Add a row index column 
TickCount$RowNum <- as.integer(rownames(TickCount))

#This removes rows where a gvkey has two different tickers with an equal row count
#it arbitrarily selects the tic with the largest row number.
TickCount <- TickCount %>% group_by(gvkey) %>%
  filter(RowNum == max(RowNum))

DSecF <- merge(DSec, TickCount, by= c("gvkey","tic"),  all.x=T)

DSecF <- DSecF %>% filter(!is.na(FilingName.y))
DSec <- DSecF
rm(DSecF)

DSec <- DSec %>% group_by(gvkey) %>%
  summarize(
    dvrate = mean(dvrate),
    cshtrm = mean(cshtrm),
    prccm = mean(prccm),
    prchm = mean(prchm),
    prclm = mean(prclm),
    trt1m = mean(trt1m))

#NACounts <- as.data.frame(colSums(is.na(DSec)))
#colnames(DSec)[colSums(is.na(DSec)) > 0]

#DONE SECURITIES 20210309

##============================================================================

#rm(DStocks)
#rm(DSt,last30,TickCount,last30_drops)

#Read in the Stocks data
DStocks <- fread("D:/Merrimack/Capstone Project/Data/Stocks_DS.csv", header=TRUE)
DStocks <- as.data.frame(DStocks)
DStocks <- filter(DStocks, gsector == "25")
#NACounts <- as.data.frame(colSums(is.na(DStocks)))
DSt <- subset(DStocks, select = c("gvkey",	#Global Company Key
                          "datadate",	#Data Date - Dividends
                          "tic", #Ticker Symbol
                          "conm", #Filing Name
                          #"div",	#Dividends per Share - Ex Date - Daily (Issue) - Daily - too many NAs
                          #"divd",	#Cash Dividends - Daily - too many NAs
                          #"adrrc",	#ADR Ratio - Daily - too many NAs
                          "cshoc",	#Shares Outstanding
                          "cshtrd",	#Trading Volume - Daily
                          "dvi",	#Indicated Annual Dividend - Current
                          "prccd",	#Price - Close - Daily
                          "prchd",	#Price - High - Daily
                          "prcld",	#Price - Low - Daily
                          "prcod"	#Price - Open - Daily
                          #"secstat",	#Security Status Marker WAY TOO MUCH TROUBLE
                          #"dlrsn"	#Research Co Reason for Deletion
                          # "idbflag" #International, Domestic, Both Indicator
))

length(unique(DSt$gvkey))
min(DSt$datadate)
max(DSt$datadate)


#Add a row index column 
DSt$RowNum <- as.integer(rownames(DSt))

#Format datadate so it sorts correctly
DSt$datadate = format(as.Date(DSt$datadate, format = "%m/%d/%Y"),"%Y/%m/%d")

#Clean up FilingName
DSt$conm <- toupper(DSt$conm)
DSt$conm <- str_trim(DSt$conm, side = c("both"))
DSt$conm <- str_replace_all(DSt$conm, "[[:punct:]]", "")
DSt$conm <- gsub("[[:space:]]", "_", DSt$conm)
names(DSt)[names(DSt) == "conm"] <- "FilingName"

#Clean up Companies with multiple tickers
#Count how many rows per ticker per company and 
#Take the ticker with the largest row count
TickCount <- DSt %>% select("gvkey","FilingName","tic")
TickCount <- TickCount %>% 
  group_by(gvkey, FilingName, tic) %>% 
  summarise(count=n())

#Get the maximum row count from the previous step and apply it to all gvkeys
TickCount <- TickCount %>% 
  group_by(gvkey) %>% 
  mutate(max_tic = max(count))

#Check to see if a ticker count is the same as the max count per gvkey
#if a ticker count matches the max count, assign a 1
TickCount <- TickCount %>% 
  group_by(gvkey) %>% 
  mutate(is_max_tic = case_when(count == max_tic ~ "1"))

#Filter out rows that aren't labeled with a 1
TickCount <- TickCount %>% 
  filter(is_max_tic == "1") 

#Add a row index column 
TickCount$RowNum <- as.integer(rownames(TickCount))

#This removes rows where a gvkey has two different tickers with an equal row count
#it arbitrarily selects the tic with the largest row number.
TickCount <- TickCount %>% group_by(gvkey) %>%
  filter(RowNum == max(RowNum))

DStF <- merge(DSt, TickCount, by= c("gvkey","tic"),  all.x=T)

DStF <- DStF %>% filter(!is.na(FilingName.y))
DStF_drops <- c("FilingName.y","RowNum.x","count","max_tic","is_max_tic","RowNum.y")
DStF <- DStF[ , !(names(DStF) %in% DStF_drops)]
DSt <- DStF
rm(DStF, DStF_drops)

#Continuous
#Convert all NA values 0
DSt$cshoc[is.na(DSt$cshoc)] <- 0
DSt$cshtrd[is.na(DSt$cshtrd)] <- 0
DSt$dvi[is.na(DSt$dvi)] <-  0
DSt$prccd[is.na(DSt$prccd)] <- 0
DSt$prchd[is.na(DSt$prchd)] <- 0
DSt$prcld[is.na(DSt$prcld)] <- 0
DSt$prcod[is.na(DSt$prcod)] <- 0

#Taryn - you validated that this subset is correct. 61173 obs is right.
last30 <- DSt %>% group_by(gvkey) %>% top_n(30,datadate)

last30 <- last30 %>% 
  group_by(gvkey) %>% 
  mutate(avg30_cshoc = mean(cshoc))

last30 <- last30 %>% 
  group_by(gvkey) %>% 
  mutate(avg30_cshtrd = mean(cshtrd))

last30 <- last30 %>% 
  group_by(gvkey) %>% 
  mutate(avg30_dvi = mean(dvi))

last30 <- last30 %>% 
  group_by(gvkey) %>% 
  mutate(avg30_prccd = mean(prccd))

last30 <- last30 %>% 
  group_by(gvkey) %>% 
  mutate(avg30_prchd = mean(prchd))

last30 <- last30 %>% 
  group_by(gvkey) %>% 
  mutate(avg30_prcld = mean(prcld))

last30 <- last30 %>% 
  group_by(gvkey) %>% 
  mutate(avg30_prcod = mean(prcod))

last30_drops <- c("datadate","cshoc","cshtrd","dvi","prccd","prchd","prcld","prcod",'datamonth',"tic","FilingName.x","secstat")
last30 <- last30[ , !(names(last30) %in% last30_drops)]
last30 <- unique(last30)

length(unique(DStocks$gvkey))
length(unique(DSt$gvkey))

DSt <- as.data.frame(last30)

#colnames(DSt)[colSums(is.na(DSt)) > 0]
#DONE STOCKS 20210217

##============================================================================

#Read in the Ratings File
DRatings <- fread("D:/Merrimack/Capstone Project/Data/Ratings_DS.csv", header=TRUE)
DRatings <- as.data.frame(DRatings)
DRatings <- filter(DRatings, gsector == "25")
DRatings_drops <- c("spsdrm",
                    #"datadate",
                    "city",
                    "conml",
                    "ggroup",
                    "gind",
                    "gsector",
                    "gsubind",
                    "naics",
                    "sic",
                    "state",
                    "conm",
                    "tic",
                    "idbflag",
                    "loc")

DRat <- DRatings[ , !(names(DRatings) %in% DRatings_drops)]

# length(unique(DRatings$gvkey))
# length(unique(DRatings$tic))

#Add a row index column 
DRat$RowNum <- as.integer(rownames(DRat))

min(DRatings$datadate)
max(DRatings$datadate)

#Get a count of unique values
length(unique(DRatings$gvkey))

#Format datadate so it sorts correctly
DRat$datadate = format(as.Date(DRat$datadate, format = "%m/%d/%Y"),"%Y/%m/%d")

#Convert the S&P Domestic Long Term Issuer Credit Rating to numbers
DRat <- mutate(DRat, splticrm = case_when(
  splticrm ==	"AA"	~	21,
  splticrm ==	"AA-"	~	20,
  splticrm ==	"A+"	~	19,
  splticrm ==	"A"	~	18,
  splticrm ==	"A-"	~	17,
  splticrm ==	"BBB+"	~	16,
  splticrm ==	"BBB-"	~	15,
  splticrm ==	"BBB"	~	14,
  splticrm ==	"BB+"	~	13,
  splticrm ==	"BB"	~	12,
  splticrm ==	"BB-"	~	11,
  splticrm ==	"B+"	~	10,
  splticrm ==	"B"	~	9,
  splticrm ==	"B-"	~	8,
  splticrm ==	"CCC+"	~	7,
  splticrm ==	"CCC"	~	6,
  splticrm ==	"CCC-"	~	5,
  splticrm ==	"CC"	~	4,
  splticrm ==	"D"	~	3,
  splticrm ==	"SD"	~	2,
  splticrm ==	"NA"	~	1))
#Convert all NA values to 1
DRat$splticrm[is.na(DRat$splticrm)] <-1

#Convert the S&P Domestic Short Term Issuer Credit Rating to numbers
DRat <- mutate(DRat, spsticrm = case_when(
  spsticrm ==	"A-1"	~	9,
  spsticrm ==	"A-1+"	~	8,
  spsticrm ==	"A-2"	~	7,
  spsticrm ==	"A-3"	~	6,
  spsticrm ==	"B-1"	~	5,
  spsticrm ==	"B"	~	4,
  spsticrm ==	"D"	~	3,
  spsticrm ==	"C"	~	2,
  spsticrm ==	"NA"	~	1))
#Convert all NA values 1
DRat$spsticrm[is.na(DRat$spsticrm)] <-1

#Convert the S&P Quality Ranking - Current to numbers
DRat <- mutate(DRat, spcsrc = case_when(
  spcsrc ==	"A+"	~	10,
  spcsrc ==	"A"	~	9,
  spcsrc ==	"A-"	~	8,
  spcsrc ==	"B+"	~	7,
  spcsrc ==	"B"	~	6,
  spcsrc ==	"B-"	~	5,
  spcsrc ==	"C"	~	4,
  spcsrc ==	"D"	~	3,
  spcsrc ==	"LIQ"	~	2,
  spcsrc ==	"NA"	~	1))
#Convert all NA values 1
DRat$spcsrc[is.na(DRat$spcsrc)] <-1

#S&P Industry Sector Code
#(unique(DRat$spcindcd))
#Convert all NA values 999
DRat$spcindcd[is.na(DRat$spcindcd)] <- 855

#S&P Economic Sector Code
#unique(DRat$spcseccd)
#Convert all NA values 999
DRat$spcseccd[is.na(DRat$spcseccd)] <-999

DRat <- DRat %>% group_by(gvkey) %>%
  filter(datadate == max(datadate)) 

#Get a count of unique values
length(unique(DRat$gvkey))

DRat_drops <- c("datadate","RowNum")

DRat <- DRat[ , !(names(DRat) %in% DRat_drops)]
DRat <- as.data.frame(DRat)
#DONE - RATINGS

# NACounts <- as.data.frame(colSums(is.na(DSFinalMerge)))
# colnames(DSFinalMerge)[colSums(is.na(DSFinalMerge)) > 0]

##============================================================================

#Read in the Settlement data
DSCAFS <- fread("D:/Merrimack/Capstone Project/Data/SCA Filings and Settlements.csv", header=TRUE)
DSCAFS <- as.data.frame(DSCAFS)

#Add a row index column 
DSCAFS$RowNum <- as.integer(rownames(DSCAFS))
DSFS <- DSCAFS

# length(unique(DSCAFS$Ticker))
# length(unique(DSCAFS$FilingName))
# min(DSCAFS$FilingYear)
# max(DSCAFS$FilingYear)

#Convert the S&P ratings to numbers then convert field to numeric
DSFS$SettlementAmount <- gsub("[^[:alnum:][:blank:].]","" , DSFS$SettlementAmount ,ignore.case = TRUE)
DSFS$SettlementAmount = as.numeric(DSFS$SettlementAmount)
#Convert all NA values 0
DSFS$SettlementAmount[is.na(DSFS$SettlementAmount)] <-0

DSFSm <- DSFS

#Filing Names cleaned the same way in DSFu dataframe
DSFSm$FilingName <- toupper(DSFSm$FilingName)
DSFSm$FilingName <- str_trim(DSFSm$FilingName, side = c("both"))
DSFSm$FilingName <- str_replace_all(DSFSm$FilingName, "[[:punct:]]", "")
DSFSm$FilingName <- gsub("[[:space:]]", "_", DSFSm$FilingName)
DSFSm$FilingName <- str_replace_all(DSFSm$FilingName, "CORPORATION", "CORP")
DSFSm$FilingName <- str_replace_all(DSFSm$FilingName, "INTERNATIONAL", "INTL")
DSFSm$FilingName <- str_replace_all(DSFSm$FilingName, "__ADR", "")
names(DSFSm)[names(DSFSm) == "Ticker"] <- "tic"


#This didn't end up being useful so removing 202103008
#Count how many settlement rows per company and add back to primary dataframe
# SC <- as.data.frame(table(DSFSm$FilingName))
# names(SC)[names(SC) == "Var1"] <- "FilingName"
# names(SC)[names(SC) == "Freq"] <- "zSttlCount"
# DSFSm <- merge(DSFSm, SC, by = "FilingName")        
# 

SM <- DSFSm %>% group_by(FilingName) %>% summarize(SettleAmount = sum(SettlementAmount))
DSFSm <- merge(DSFSm, SM, by = "FilingName") 

length(unique(DSFSm$FilingName))
length(unique(DSFSm$Ticker))

DSFSm <- subset( DSFSm, select = -c(Exchange,FilingYear, SettlementAmount, Dismissed, RowNum))

DSFSm <- DSFSm %>% group_by(FilingName)%>%filter(SettleAmount == max(SettleAmount))

#Create a new field to indicate whether a settlement occurred or not
DSFSm$SettleYN <-  ifelse(DSFSm$SettleAmount == 0,0,1)

DSFS <- as.data.frame(unique(DSFSm))
DSFS_FN <- subset(DSFS,select = c(FilingName,SettleAmount, SettleYN))
DSFS_TS <- subset(DSFS,select = c(tic,SettleAmount, SettleYN))
rm(DSFSm, SM)


#DONE SETTLEMENTS
rm(DSFinalMerge)
##=== Merge cleansed datasets ===============================================
DSFinalMerge <- merge(DSFu, DSt, by="gvkey",  all.x=T)
DSFinalMerge <- merge(DSFinalMerge, DSec, by="gvkey",  all.x=T)
DSFinalMerge <- merge(DSFinalMerge, DRat, by="gvkey",  all.x=T)
DSFinalMerge <- merge(DSFinalMerge, DSFS_FN, by="FilingName",  all.x=T)
DSFinalMerge <- merge(DSFinalMerge, DSFS_TS, by="tic",  all.x=T)
DSFinalMerge$SettleAmount.x <- ifelse(is.na(DSFinalMerge$SettleAmount.x),DSFinalMerge$SettleAmount.y,DSFinalMerge$SettleAmount.x)
DSFinalMerge$SettleYN.x <- ifelse(is.na(DSFinalMerge$SettleYN.x),DSFinalMerge$SettleYN.y,DSFinalMerge$SettleYN.x)


#Delete some duplicated fields
#DSFinalMerge <- subset(DSFinalMerge,select = -c(datadate.x,datafmt,tic.x,tic.y,datadate.y,FilingName.x))
DSFinalMerge <- subset(DSFinalMerge,select = -c(tic,datadate,datafmt,SettleAmount.y,SettleYN.y))
names(DSFinalMerge)[names(DSFinalMerge) == "SettleAmount.x"] <- "SettleAmount"
names(DSFinalMerge)[names(DSFinalMerge) == "SettleYN.x"] <- "SettleYN"
DSFinalMerge <- unique(DSFinalMerge)

#NACounts <- as.data.frame(colSums(is.na(DSFinalMerge)))
#write.csv(NACounts,"D:/Merrimack/Capstone Project/Data\\NACounts_210308method.csv", row.names = TRUE)

##============================================================================
#NACounts <- as.data.frame(colSums(is.na(DSFinalMerge)))

#Correct new NAs
#Conts
DSFinalMerge$avg30_cshoc[is.na(DSFinalMerge$avg30_cshoc)] <- 0
DSFinalMerge$avg30_cshtrd[is.na(DSFinalMerge$avg30_cshtrd)] <- 0
DSFinalMerge$avg30_dvi[is.na(DSFinalMerge$avg30_dvi)] <- 0
DSFinalMerge$avg30_prccd[is.na(DSFinalMerge$avg30_prccd)] <- 0
DSFinalMerge$avg30_prchd[is.na(DSFinalMerge$avg30_prchd)] <- 0
DSFinalMerge$avg30_prcld[is.na(DSFinalMerge$avg30_prcld)] <- 0
DSFinalMerge$avg30_prcod[is.na(DSFinalMerge$avg30_prcod)] <- 0
DSFinalMerge$cshtrm[is.na(DSFinalMerge$cshtrm)] <- 0
DSFinalMerge$dvrate[is.na(DSFinalMerge$dvrate)] <- 0
DSFinalMerge$prccm[is.na(DSFinalMerge$prccm)] <- 0
DSFinalMerge$prchm[is.na(DSFinalMerge$prchm)] <- 0
DSFinalMerge$prclm[is.na(DSFinalMerge$prclm)] <- 0
DSFinalMerge$trt1m[is.na(DSFinalMerge$trt1m)] <- 0

#Cats
DSFinalMerge$acctstd[is.na(DSFinalMerge$acctstd)] <- "ZZ"
DSFinalMerge$splticrm[is.na(DSFinalMerge$splticrm)] <-1
DSFinalMerge$spsticrm[is.na(DSFinalMerge$spsticrm)] <-1
DSFinalMerge$spcsrc[is.na(DSFinalMerge$spcsrc)] <-1
DSFinalMerge$spcindcd[is.na(DSFinalMerge$spcindcd)] <- 855
DSFinalMerge$spcseccd[is.na(DSFinalMerge$spcseccd)] <-999
DSFinalMerge$state[is.na(DSFinalMerge$state)] <-"ZZ"
DSFinalMerge$state <- ifelse(DSFinalMerge$state == "","ZZ",DSFinalMerge$state)
DSFinalMerge$idbflag <- ifelse(is.na(DSFinalMerge$idbflag) & DSFinalMerge$loc == "USA","D",DSFinalMerge$idbflag)
DSFinalMerge$idbflag <- ifelse(is.na(DSFinalMerge$idbflag) & DSFinalMerge$loc != "USA","B",DSFinalMerge$idbflag)
DSFinalMerge$dlrsn[is.na(DSFinalMerge$dlrsn)] <- 0

#Response Variables
DSFinalMerge$SettleAmount[is.na(DSFinalMerge$SettleAmount)] <- 0
DSFinalMerge$SettleYN[is.na(DSFinalMerge$SettleYN)] <- 0

sum(DSFinalMerge$SettleYN)


colnames(DSFinalMerge)[colSums(is.na(DSFinalMerge)) > 0]

#NACounts <- as.data.frame(colSums(is.na(DSFinalMerge)))

rm(DRat, DRatings, DSCAFS, DSec, DSecurities, DSFS, DSFu, DSFund,DSFundamentals, DSt, DStocks, last30, TickCount
   ,DRat_drops, DRatings_drops, DSFinalMerge_drops, last30_drops, DSFS_FN, DSFS_TS)

write.csv(DSFinalMerge,"D:/Merrimack/Capstone Project/Data\\DSFinalMerge_New312.csv", row.names = TRUE)
