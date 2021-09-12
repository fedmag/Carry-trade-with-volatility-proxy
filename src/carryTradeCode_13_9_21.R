# install.packages("readxl")
# install.packages("dplyr")
# install.packages("moments")
# install.packages("forecast")

library(readxl)
library(dplyr)
library(moments)
library(PerformanceAnalytics)
library(forecast)
library(tseries)

`%!in%` = Negate(`%in%`)
### Glossary
### (https://www.investopedia.com/terms/b/bid-and-ask.asp)
### - Bid: what the investor wants to pay 
### - Offer: what the owner wants to get
### - Spread bid/ask: an indicator of supply and demand for the financial instrument.
###                 the smaller the spread, the greater the liquidity of the given security.
###                 The spread represents the market maker's profit.
### - Forwards discount = f - s = forward rate - spot rate = price tomorrow vs price today -> spread  
###                       if > 0  -> appreciation of the currency wrt USD
### 
### 
### - volatility beta = https://www.investopedia.com/terms/b/beta.asp  // https://www.investopedia.com/investing/beta-gauging-price-fluctuations/
### 





################################################################################
##################### DATA PREPARATION #########################################
################################################################################



# Ingesting data
data_path = "path_to_data"
sheet_name_list = excel_sheets(data_path) # "list-column"
data_list = lapply(sheet_name_list, read_excel, path = data_path)
names(data_list) = sheet_name_list
# make the individual dfs independent from the list (take them out)
list2env(data_list, .GlobalEnv)

# merge dfs, note that we ignore the first col (containing the date) of the 2nd df we merge
bids = cbind(Bids1, Bids2[,2:length(Bids2)])
offers = cbind(Offers1, Offers2[,2:length(Offers2)])
fut_bids = cbind(FBids1, FBids2[,2:length(FBids2)])
fut_offers = cbind(FOffers1, FOffers2[,2:length(FOffers2)])
day_spot = cbind(DailySpot1, DailySpot2[,2:length(DailySpot2)])

# remove used dfs
rm(Bids1, Bids2, Offers1, Offers2, FBids1, FBids2, FOffers1, FOffers2, DailySpot1, DailySpot2, sheet_name_list, data_list)

# renaming Name as Date
names(bids)[1] = "Date" 
names(offers)[1] = "Date" 
names(fut_bids)[1] = "Date" 
names(fut_offers)[1] = "Date" 
names(day_spot)[1] = "Date"

# assign the right data type
bids$Date = as.Date(bids$Date)
offers$Date = as.Date(offers$Date)
fut_bids$Date = as.Date(fut_bids$Date)
fut_offers$Date = as.Date(fut_bids$Date)
day_spot$Date = as.Date(day_spot$Date)

# all the rest must be numeric
bids[2:length(bids)] = lapply(bids[2:length(bids)], as.numeric)
offers[2:length(offers)] = lapply(offers[2:length(offers)], as.numeric)
fut_bids[2:length(fut_bids)] = lapply(fut_bids[2:length(fut_bids)], as.numeric)
fut_offers[2:length(fut_offers)] = lapply(fut_offers[2:length(fut_offers)], as.numeric)
day_spot[2:length(day_spot)] = lapply(day_spot[2:length(day_spot)], as.numeric)

# change "direction" to be consistent: US $ to x
# bids
bids = rename(bids, "UK £  TO US $ (BBI) - BID SPOT" = "US $ TO UK £ (BBI) - BID SPOT")
bids[,"UK £  TO US $ (BBI) - BID SPOT"] = 1/bids[,"UK £  TO US $ (BBI) - BID SPOT"]
bids = rename(bids, "IRISH PUNT TO US $ (BBI) - BID SPOT" = "US $ TO IRISH PUNT (BBI) - BID SPOT")
bids[,"IRISH PUNT TO US $ (BBI) - BID SPOT"]= 1/ bids[,"IRISH PUNT TO US $ (BBI) - BID SPOT"]
# offers
offers = rename(offers, "UK £  TO US $ (BBI) - SPOT OFFERED" = "US $ TO UK £ (BBI) - SPOT OFFERED")
offers[,"UK £  TO US $ (BBI) - SPOT OFFERED"] = 1/offers[,"UK £  TO US $ (BBI) - SPOT OFFERED"]
offers = rename(offers, "IRISH PUNT TO US $ (BBI) - SPOT OFFERED" = "US $ TO IRISH PUNT (BBI) - SPOT OFFERED")
offers[,"IRISH PUNT TO US $ (BBI) - SPOT OFFERED"]= 1/ offers[,"IRISH PUNT TO US $ (BBI) - SPOT OFFERED"]
# future bids
fut_bids = rename(fut_bids, "UK £ TO US $ 1M FWD (BBI) - BID SPOT" = "US $ TO UK £ 1M FWD (BBI) - BID SPOT")
fut_bids[,"UK £ TO US $ 1M FWD (BBI) - BID SPOT"] = 1/fut_bids[,"UK £ TO US $ 1M FWD (BBI) - BID SPOT"]
fut_bids = rename(fut_bids, "IRISH PUNT TO US $ 1M FWD(BBI) DISC - BID SPOT" = "US $ TO IRISH PUNT 1M FWD(BBI) DISC - BID SPOT")
fut_bids[,"IRISH PUNT TO US $ 1M FWD(BBI) DISC - BID SPOT" ]= 1/ fut_bids[,"IRISH PUNT TO US $ 1M FWD(BBI) DISC - BID SPOT"]
# future offers
fut_offers = rename(fut_offers, "UK £ TO US $ 1M FWD (BBI) - SPOT OFFERED" = "US $ TO UK £ 1M FWD (BBI) - SPOT OFFERED")
fut_offers[,"UK £ TO US $ 1M FWD (BBI) - SPOT OFFERED"] = 1/fut_offers[,"UK £ TO US $ 1M FWD (BBI) - SPOT OFFERED"]
fut_offers = rename(fut_offers, "IRISH PUNT TO US $ 1M FWD(BBI) DISC - SPOT OFFERED" = "US $ TO IRISH PUNT 1M FWD(BBI) DISC - SPOT OFFERED")
fut_offers[,"IRISH PUNT TO US $ 1M FWD(BBI) DISC - SPOT OFFERED" ]= 1/ fut_offers[,"IRISH PUNT TO US $ 1M FWD(BBI) DISC - SPOT OFFERED"]
# day_spot - NOT SURE THIS IS NEEDED
day_spot = rename(day_spot, "UK £  TO US $ (BBI) - EXCHANGE RATE" = "US $ TO UK £ (BBI) - EXCHANGE RATE")
day_spot[,"UK £  TO US $ (BBI) - EXCHANGE RATE"] = 1/day_spot[,"UK £  TO US $ (BBI) - EXCHANGE RATE"]
day_spot = rename(day_spot, "IRISH PUNT TO US $ (BBI) - EXCHANGE RATE" = "US $ TO IRISH PUNT (BBI) - EXCHANGE RATE")
day_spot[,"IRISH PUNT TO US $ (BBI) - EXCHANGE RATE"]= 1/ day_spot[,"IRISH PUNT TO US $ (BBI) - EXCHANGE RATE"]

# create NA for european money after the euro comes into play
# from 1.1.1999 (Wikipedia) # for all countries till greece the euro replaced the local currency 1.1.2002
# germany 1999
bids$"GERMAN MARK TO US $ (BBI) - BID SPOT"[219:427] = NA
offers$"GERMAN MARK TO US $ (BBI) - SPOT OFFERED"[219:427] = NA
fut_bids$"GERMAN MARK TO US $ 1M FWD(BBI) DISC - BID SPOT"[219:427] = NA
fut_offers$"GERMAN MARK TO US $ 1M FWD(BBI) DISC - SPOT OFFERED"[219:427] = NA
# france 1999
bids$"FRENCH FRANC TO US $ (BBI) - BID SPOT"[219:427] = NA
offers$"FRENCH FRANC TO US $ (BBI) - SPOT OFFERED"[219:427] = NA
fut_bids$"FRENCH FR. TO US $ 1M FWD(BBI) DISC - BID SPOT"[219:427] = NA
fut_offers$"FRENCH FR. TO US $ 1M FWD(BBI) DISC - SPOT OFFERED"[219:427] = NA
# italy 1999
bids$"ITALIAN LIRA TO US $ (BBI) - BID SPOT"[219:427] = NA
offers$"ITALIAN LIRA TO US $ (BBI) - SPOT OFFERED"[219:427] = NA
fut_bids$"ITL.LIRA TO US $ 1M FWD (BBI) DISC - BID SPOT"[219:427] = NA
fut_offers$"ITL.LIRA TO US $ 1M FWD (BBI) DISC - SPOT OFFERED"[219:427] = NA
# netherlands 1999
bids$"NETH. GUILDER TO US $ (BBI) - BID SPOT"[219:427] = NA
offers$"NETH. GUILDER TO US $ (BBI) - SPOT OFFERED"[219:427] = NA
fut_bids$"NETH. GUILDER TO US $ 1MFWD(BBI)DISC - BID SPOT"[219:427] = NA
fut_offers$"NETH. GUILDER TO US $ 1MFWD(BBI)DISC - SPOT OFFERED"[219:427] = NA
# portugal 1999
bids$"PORTUGUESE ESCUDO TO US $ (WMR) - BID SPOT"[219:427] = NA
offers$"PORTUGUESE ESCUDO TO US $ (WMR) - SPOT OFFERED"[219:427] = NA
fut_bids$"PORTUGUESE ESCUDO TO US $ 1M FWD(WMR) - BID SPOT"[219:427] = NA
fut_offers$"PORTUGUESE ESCUDO TO US $ 1M FWD(WMR) - SPOT OFFERED"[219:427] = NA
# spain 1999
bids$"SPANISH PESETA TO US $ (WMR) - BID SPOT"[219:427] = NA
offers$"SPANISH PESETA TO US $ (WMR) - SPOT OFFERED"[219:427] = NA
fut_bids$"SPANISH PESETA TO US $ 1M FWD (WMR) - BID SPOT"[219:427] = NA
fut_offers$"SPANISH PESETA TO US $ 1M FWD (WMR) - SPOT OFFERED"[219:427] = NA
# belgium 1999
bids$"BELGIAN FRANC TO US $ (WMR) - BID SPOT"[219:427] = NA
offers$"BELGIAN FRANC TO US $ (WMR) - SPOT OFFERED"[219:427] = NA
fut_bids$"BELGIAN FRANC TO US $ 1M FWD (WMR) - BID SPOT"[219:427] = NA
fut_offers$"BELGIAN FRANC TO US $ 1M FWD (WMR) - SPOT OFFERED"[219:427] = NA
# finland 1999
bids$"FINNISH MARKKA TO US $ (WMR) - BID SPOT"[219:427] = NA
offers$"FINNISH MARKKA TO US $ (WMR) - SPOT OFFERED"[219:427] = NA
fut_bids$"FINNISH MARKKA TO US $ 1M FWD (WMR) - BID SPOT"[219:427] = NA
fut_offers$"FINNISH MARKKA TO US $ 1M FWD (WMR) - SPOT OFFERED"[219:427] = NA
# austria 1999
bids$"AUSTRIAN SCHIL.TO US $ (WMR) - BID SPOT"[219:427] = NA
offers$"AUSTRIAN SCHIL.TO US $ (WMR) - SPOT OFFERED"[219:427] = NA
fut_bids$"AUSTRIAN SCHIL.TO US $ 1M FWD (WMR - BID SPOT"[219:427] = NA
fut_offers$"AUSTRIAN SCHIL.TO US $ 1M FWD (WMR - SPOT OFFERED"[219:427] = NA
# ireland 1999
bids$"IRISH PUNT TO US $ (BBI) - BID SPOT"[219:427] = NA
offers$"IRISH PUNT TO US $ (BBI) - SPOT OFFERED"[219:427] = NA
fut_bids$"IRISH PUNT TO US $ 1M FWD(BBI) DISC - BID SPOT"[219:427] = NA
fut_offers$"IRISH PUNT TO US $ 1M FWD(BBI) DISC - SPOT OFFERED"[219:427] = NA
######
# 2001 
######
# greece 2001
bids$"GREEK DRACHMA TO US $ (WMR) - BID SPOT"[219:427] = NA
offers$"GREEK DRACHMA TO US $ (WMR) - SPOT OFFERED"[219:427] = NA
fut_bids$"GREEK DRACHMA TO US $ 1M FWD (WMR) - BID SPOT"[219:427] = NA
fut_offers$"GREEK DRACHMA TO US $ 1M FWD (WMR) - SPOT OFFERED"[219:427] = NA
######
# 2007
######
# slovenia 2007
bids$"SLOVENIAN TOLAR TO US $ (WMR) - BID SPOT"[279:427] = NA
offers$"SLOVENIAN TOLAR TO US $ (WMR) - SPOT OFFERED"[279:427] = NA
fut_bids$"SLOVENIAN TOLAR TO U $ 1M FWD (WMR) - BID SPOT"[279:427] = NA
fut_offers$"SLOVENIAN TOLAR TO U $ 1M FWD (WMR) - SPOT OFFERED"[279:427] = NA
######
# 2008
######
# cyprus 2008
bids$"CYPRUS £ TO US $ (WMR) - BID SPOT"[291:427] = NA
offers$"CYPRUS £ TO US $ (WMR) - SPOT OFFERED"[291:427] = NA
fut_bids$"CYPRUS POUND TO US $ 1M FWD (WMR) - BID SPOT"[291:427] = NA
fut_offers$"CYPRUS POUND TO US $ 1M FWD (WMR) - SPOT OFFERED"[291:427] = NA
######
# 2009
######
# slovakia 2009
bids$"SLOVAK KORUNA TO US $ (WMR) - BID SPOT"[303:427] = NA
offers$"SLOVAK KORUNA TO US $ (WMR) - SPOT OFFERED"[303:427] = NA
fut_bids$"SLOVAK KORUNA TO US $ 1M FWD (WMR) - BID SPOT"[303:427] = NA
fut_offers$"SLOVAK KORUNA TO US $ 1M FWD (WMR) - SPOT OFFERED"[303:427] = NA


# Alter the first 12 Countries
day_spot[4784:dim(day_spot)[1],c(12,13,14,15,16,17,18,22,24,33,36)] = NA

# Change Slovenia 
day_spot[6045:dim(day_spot)[1],47] = NA
# Change Cyprus
day_spot[6306:dim(day_spot)[1],44] = NA
# Change Slovak 
day_spot[6568:dim(day_spot)[1],42] = NA


################################################################################
####################### GRAPHS #################################################
################################################################################
library(RColorBrewer)
darkcols <- brewer.pal(4, "Pastel1")
par(mfrow = c(1, 1))

print_portfolios = function(funct, share, name, min_max=c(-200, 200), col) {
  new_p = funct(share)
  if (c == 1) {
    plot(ts(data = cumsum(new_p$longshort), end=c(2019, 5), frequency = 12), 
         ylim=min_max,
         col=darkcols[c], lwd=2, type='line', ylab = ("Cumulative log_returns"),
         xlab = "Date", main = paste("Cumulative Carry Trade Returns", name))
    abline(h=0, col= "black")
  } else {
    lines(ts(data = cumsum(new_p$longshort), end=c(2019, 5), frequency = 12), 
          ylim=min_max,
          col=darkcols[c], lwd=2, type='line', ylab = paste("Cumulative log_returns", share*100, "%"),
          xlab = "Date", main = paste("Cumulative Carry Trade Returns", name, share))
  }
  
  legend(x = "topleft", 
         legend = c("10%", "20%", "30%", "40%"), 
         lty = c(1,1,1,1), lwd = 3,
         col=c(darkcols[1], darkcols[2], darkcols[3], darkcols[4]))
  
  
  s = as.numeric(new_p$short)
  mean_s = mean((s), na.rm = T)
  median_s = median((s), na.rm = T)
  sdev =  sd(na.omit(s))
  sk = skewness(na.omit(s))
  kurt = kurtosis(na.omit(s))
  varisk = VaR(na.omit(s), p=.95, method = "historical") # with probability p I loose more than this value
  es = ES(na.omit(s), p=.95, FUN="StdDev")
  sharpe = SharpeRatio(na.omit(ts(data = s), end=c(2019, 5), frequency = 12))
  print(paste("Avg short", mean_s, "median:", median_s,
              "sd:", sdev , "skw:", sk, "kurt: ", kurt, "VaR:", varisk, "ES:" , es,
              "Sharpe:", sharpe[1])) # 1 StdDev 2 VaR 3 ES
  
  l = as.numeric(new_p$long)
  mean_l = mean((l), na.rm = T)
  median_l = median((l), na.rm = T)
  sdev =  sd(na.omit(l))
  sk = skewness(na.omit(l))
  kurt = kurtosis(na.omit(l))
  varisk = VaR(na.omit(l), p=.95, method = "historical") # with probability p I loose more than this value
  es = ES(na.omit(l), p=.95, FUN="StdDev")
  sharpe = SharpeRatio(na.omit(ts(data = l), end=c(2019, 5), frequency = 12))
  print(paste("Avg long", mean_l, "median:", median_l,
              "sd:", sdev , "skw:", sk, "kurt: ", kurt, "VaR:", varisk, "ES:" , es,
              "Sharpe:", sharpe[1])) # 1 StdDev 2 VaR 3 ES
  
  ls = as.numeric(new_p$longshort)
  mean_ls = mean(na.omit(ts(data = ls), end=c(2019, 5), frequency = 12), na.rm = T)
  median_ls = median(na.omit(ts(data = ls), end=c(2019, 5), frequency = 12), na.rm = T)
  sdev =  sd(na.omit(ts(data = ls), end=c(2019, 5), frequency = 12))
  sk = skewness(na.omit(ts(data = ls), end=c(2019, 5), frequency = 12))
  kurt = kurtosis(na.omit(ts(data = ls), end=c(2019, 5), frequency = 12))
  varisk = VaR(na.omit(ts(data = ls), end=c(2019, 5), frequency = 12), p=.95, method = "historical") # with probability p I loose more than this value
  es = ES(na.omit(ts(data = ls), end=c(2019, 5), frequency = 12), p=.95, FUN="StdDev")
  sharpe = SharpeRatio(na.omit(ts(data = ls), end=c(2019, 5), frequency = 12))
  print(paste("Avg long_short", mean_ls, "median:", median_ls,
              "sd:", sdev , "skw:", sk, "kurt: ", kurt, "VaR:", varisk, "ES:" , es,
              "Sharpe:", sharpe[1])) # 1 StdDev 2 VaR 3 ES
}

################################################################################
####################### PORTFOLIO CONSTRUCTION #################################
################################################################################



log_spot_bid = log(bids[,-1]) #  -1 means ignore date col
log_spot_offer = log(offers[,-1])

log_spot_bid_t1 = log_spot_bid[-1,] # moving forward 1 month 
log_spot_bid_t1 = rbind(log_spot_bid_t1, NA)
log_spot_off_t1 = log_spot_offer[-1,] # moving forward 1 month
log_spot_off_t1 = rbind(log_spot_off_t1, NA)
###### future
log_fwd_bid = log(fut_bids[,-1])
log_fwd_off = log(fut_offers[,-1])
###### returns
ret_long = log_fwd_bid - log_spot_off_t1
ret_short = log_spot_bid_t1 - log_fwd_off
## midRates -> # getting the avg between bid and ask to then compute the spread, 
## we use the log as suggested in the paper as numb are small
## ignoring the first cols as it contains dates
midRate_spot_t1 = (log_spot_bid_t1 + log_spot_off_t1)/2
midRate_fwd = (log_fwd_bid + log_fwd_off)/2
midRate_rx_t1 = (midRate_fwd - midRate_spot_t1) 
######
######
###### calculations
long_position_enter_exit = log_fwd_bid - log_spot_off_t1
short_position_enter_exit = - log_fwd_off + log_spot_bid_t1

long_position_enter_stay = log_fwd_bid - midRate_spot_t1
short_position_enter_stay = - log_fwd_off + midRate_spot_t1

long_position_exit_wasIn = midRate_fwd - log_spot_off_t1
short_position_exit_wasIn = - midRate_fwd + log_spot_bid_t1


portfolioShare.create = function(share) {
  
  returns = list()
  short = long = longshort = numeric()
  my_portfolio_long = my_portfolio_short = vector()
  
  for (i in 2:dim(midRate_rx_t1)[1]-2) {
    sizes = vector()
    
    previous = as.numeric(midRate_rx_t1[(i),])
    p_prev = round(length(na.omit(previous))*share)
    
    sizes = append(sizes, p_prev)
    
    best_last_month = (order(as.double(previous), decreasing=TRUE, na.last = TRUE, method = "shell"))[1:p_prev]
    worst_last_month = (order(as.double(previous), decreasing=FALSE, na.last = TRUE, method = "shell"))[1:p_prev]
    
    if (i==1) { # create first portfolio
      my_portfolio_long = best_last_month
      my_portfolio_short = worst_last_month
    }
    
    all_currencies_l = unique(c(my_portfolio_long, best_last_month))
    
    tmp_long = vector()
    for (l in all_currencies_l) {
      if (l %in% my_portfolio_long && l %!in% best_last_month) { # was but exits
        tmp_long = c(tmp_long, long_position_exit_wasIn[i, l]) 
      }
      else if (l %!in% my_portfolio_long && l %in% best_last_month) {
        tmp_long = c(tmp_long, long_position_enter_stay[i+1, l])
      }
      else if (l %in% my_portfolio_long && l %in% best_last_month) {
        tmp_long = c(tmp_long, midRate_rx_t1[i+1, l])
      }
      else{print("Error")}
    }
    
    long[i-1] = mean(tmp_long, na.rm = T)
    if (p_prev == 0) {long[i-1] = 0}
    
    all_currencies_s = unique(c(my_portfolio_short, worst_last_month))
    
    tmp_short = vector()
    for (s in all_currencies_s) {
      if (s %in% my_portfolio_short && s %!in% worst_last_month) { # was but exits
        tmp_short = c(tmp_short, short_position_exit_wasIn[i, s]) 
      }
      else if (s %!in% my_portfolio_short && s %in% worst_last_month) {
        tmp_short = c(tmp_short, short_position_enter_stay[i+1, s])
      }
      else if (s %in% my_portfolio_short && s %in% worst_last_month) {
        tmp_short = c(tmp_short, midRate_rx_t1[i+1, s])
      }
      else{print("Error")}
    }
    short[i-1] = mean(tmp_short, na.rm = T)
    if (p_prev == 0) {short[i-1] = 0}
    
    # longshort
    longshort[i-1] = sum(long[i-1] - short[i-1])
    
    # at the end of the month I adjust my portfolio
    my_portfolio_long = best_last_month
    my_portfolio_short = worst_last_month
    
  }
  print("-------------------------------")
  print(paste("Average Portfolio size:", mean(sizes)))
  returns$long = long
  returns$short = short
  returns$longshort = longshort
  return(returns)
} 

c = 1
for (i in c(0.1, 0.2, 0.3, 0.4)) {
  print_portfolios(portfolioShare.create, i, "Plain", c(-0.5, 3), c)
  c = c+1
}



###############################################################
#################### volatility ###############################
###############################################################



abs_daily_log_ret = matrix(NA, nrow = dim(day_spot)[1]-1, ncol = dim(day_spot)[2]-1)
abs_daily_log_ret = as.data.frame(abs_daily_log_ret)
colnames(abs_daily_log_ret) = colnames(day_spot)[2:49]
# cols
for (i in 2:dim(day_spot)[2]) { # ignoring date
  abs_daily_log_ret[,i-1] = abs(diff(log(day_spot[,i])))
}

abs_daily_log_ret$avg_daily = rowMeans(abs_daily_log_ret, na.rm = T)
abs_daily_log_ret$year_month = substr(day_spot$Date[1:dim(day_spot)[1]-1], 1, 7)

#summary value -> overall value
volatility_proxy = aggregate(abs_daily_log_ret$avg_daily, by = list(abs_daily_log_ret$year_month), FUN=mean)

innovation = matrix(NA, nrow = dim(volatility_proxy)[1], ncol=1)
innovation = as.data.frame(innovation)

for (i in 2:dim(volatility_proxy)[1]){
  innovation$first_difference[i] = volatility_proxy$x[i] - volatility_proxy$x[i-1]
}


#calculate autocorrelations 
# taken from auto.arima
arima_order = c(1,0,1)
season_order = c(2,0,0)
innovation_arima = na.omit(volatility_proxy$x)
value1 = arima((ts(data = innovation_arima, frequency = 12)), order = arima_order, seasonal = list(order = season_order, period = 12))
checkresiduals(value1)


################################################################################
#################### volatility / innovation 1 day #############################  
################################################################################

par(mfrow = c(1, 1))

# we need a value for each currency -> volatility matrix
volat = matrix(NA, nrow = 430, ncol = 48)
volat = as.data.frame(volat)

for (i in 1:48) { #iteration over cols
  tmp = aggregate(abs_daily_log_ret[,i], by = list(abs_daily_log_ret$year_month), FUN=mean)
  volat[,i] = tmp$x
}

innov = matrix(NA, nrow = dim(volat)[1], ncol=48)
innov = as.data.frame(innov)

arima_matr = matrix(NA, nrow = dim(volat)[1], ncol=48)
arima_matr = as.data.frame(arima_matr)

for (j in 1:48) { # cols
  for (i in 2:dim(volat)[1]){ # rows
    innov[i, j] = volat[i,j] - volat[i-1, j]
  }
  a_currency_ts = ts(data = ((volat[,j])), frequency = 12)
  arima_matr[,j] = resid(arima(a_currency_ts, order=arima_order, seasonal = list(order = season_order) ,include.mean = T))
}


###############################################################
#################### portfolios with AR residuals #############
###############################################################

portfolioShareAR.create = function(share) {
  
  returns = list()
  short = long = longshort = numeric()
  my_portfolio_long = my_portfolio_short = vector()
  
  for (i in 2:dim(innov)[1]-1) {
    sizes = vector()
    
    previous = as.numeric(arima_matr[(i),]) # pick innov[i,] vs ar_matr
    p_prev = round(length(na.omit(previous))*share)
    
    sizes = append(sizes, p_prev)
    
    best_last_month = (order(as.double(previous), decreasing=TRUE, na.last = TRUE, method = "shell"))[1:p_prev]
    worst_last_month = (order(as.double(previous), decreasing=FALSE, na.last = TRUE, method = "shell"))[1:p_prev]
    
    if (i==1) { # create first portfolio
      my_portfolio_long = best_last_month
      my_portfolio_short = worst_last_month
    }
    
    all_currencies_l = unique(c(my_portfolio_long, best_last_month))
    
    tmp_long = vector()
    for (l in all_currencies_l) {
      if (l %in% my_portfolio_long && l %!in% best_last_month) { # was but exits
        tmp_long = c(tmp_long, long_position_exit_wasIn[i, l])
      }
      else if (l %!in% my_portfolio_long && l %in% best_last_month) {
        tmp_long = c(tmp_long, long_position_enter_stay[i+1, l])
      }
      else if (l %in% my_portfolio_long && l %in% best_last_month) {
        tmp_long = c(tmp_long, midRate_rx_t1[i+1, l])
      }
      else{print("Error")}
    }
    
    long[i-1] = mean(tmp_long, na.rm = T)
    if (p_prev == 0) {long[i-1] = 0}
    
    all_currencies_s = unique(c(my_portfolio_short, worst_last_month))
    
    tmp_short = vector()
    for (s in all_currencies_s) {
      if (s %in% my_portfolio_short && s %!in% worst_last_month) { # was but exits
        tmp_short = c(tmp_short, short_position_exit_wasIn[i, s])
      }
      else if (s %!in% my_portfolio_short && s %in% worst_last_month) {
        tmp_short = c(tmp_short, short_position_enter_stay[i+1, s])
      }
      else if (s %in% my_portfolio_short && s %in% worst_last_month) {
        tmp_short = c(tmp_short, midRate_rx_t1[i+1, s])
      }
      else{print("Error")}
    }
    short[i-1] = mean(tmp_short, na.rm = T)
    if (p_prev == 0) {short[i-1] = 0}
    
    # longshort
    longshort[i-1] = sum(long[i-1] - short[i-1])
    
    # at the end of the month I adjust my portfolio
    my_portfolio_long = best_last_month
    my_portfolio_short = worst_last_month
    
  }
  print("-------------------------------")
  print(paste("Average Portfolio size:", mean(sizes)))
  returns$long = long
  returns$short = short
  returns$longshort = longshort
  return(returns)
} # check enter_exit vs enter_stays

c =1
for (i in c( 0.1, 0.2, 0.3,0.4)) {
  print_portfolios(portfolioShareAR.create,i, 
                   paste("ARIMA (", toString(arima_order) ,")(", toString(season_order) ,") residuals"),
                   c(-0.5,2), c)
  c = c+1
}



###############################################################
#################### portfolios with BETAS ####################
###############################################################
bench_matrix = matrix(NA, nrow=430, ncol=1)
bench_matrix = as.data.frame(bench_matrix)
colnames(bench_matrix)[1] = "benchmark"

for (i in 1:430) {
  bench_matrix[i,1] = rowMeans(midRate_rx_t1[i,], na.rm = T) 
}


par(mfrow = c(2, 2))
for( update in c(6, 18)){
  
  beta_matrix = matrix(NA, nrow=430/update, ncol=48) # updating every 5 months
  beta_matrix = as.data.frame(beta_matrix )
  colnames(beta_matrix) = colnames(midRate_rx_t1)# midrate table does not contain the date
  
  for (i in 1:48) { # cols
    for (j in seq(update, 426, update)) { # rows
      start = j - update+1
      y1 = midRate_rx_t1[start: j, i]
      x1 = bench_matrix[start: j, 1]
      if (!all(is.na(y1)) && !all(is.na(x1))){
        beta_lm = lm( y1 ~ x1, na.action = na.omit)
        beta = beta_lm[["coefficients"]][["x1"]]
        beta_matrix[j/update, i ] = beta
      }
    }
  }
  
  
  portfolioShareBETA.create = function(share) {
    
    returns = list()
    short = long = longshort = numeric()
    my_portfolio_long = my_portfolio_short = vector()
    
    for (i in seq(update, 426, update)) {
      sizes = vector()
      
      idx = (i/update)
      previous = as.numeric(beta_matrix[idx,])
      p_prev = round(length(na.omit(previous))*share)
      sizes = append(sizes, p_prev)
      if (p_prev == 0) {p_prev =1}
      # print(p_prev)
      
      best_last_month = (order(as.double(previous), decreasing=TRUE, na.last = TRUE, method = "shell"))[1:p_prev] # highest beta
      worst_last_month = (order(as.double(previous), decreasing=FALSE, na.last = TRUE, method = "shell"))[1:p_prev] # lowest beta
      
      if (idx==1) { # create first portfolio
        my_portfolio_long = best_last_month # portfolio 5
        my_portfolio_short = worst_last_month # portfolio 1
      }
      
      all_currencies_l = unique(c(my_portfolio_long, best_last_month))
      all_currencies_s = unique(c(my_portfolio_short, worst_last_month))
      
      tmp_long = vector()
      start = i+1-update
      for (l in all_currencies_l) {
        
        if (l %in% my_portfolio_long && l %!in% best_last_month) { # was but exits
          tmp_long = c(tmp_long, sum(long_position_exit_wasIn[start : i+1, l]))
        }
        else if (l %!in% my_portfolio_long && l %in% best_last_month) {
          tmp_long = c(tmp_long, sum(long_position_enter_stay[start : i+1, l]))
        }
        else if (l %in% my_portfolio_long && l %in% best_last_month) {
          tmp_long = c(tmp_long, sum(midRate_rx_t1[start:i+1, l]))
        }
        else{print("Error")}
      }
      
      long[idx] = mean(tmp_long, na.rm = T)
      if (p_prev == 0) {long[idx-1] = 0}
      
      tmp_short = vector()
      for (s in all_currencies_s) {
        if (s %in% my_portfolio_short && s %!in% worst_last_month) { # was but exits
          tmp_short = c(tmp_short, sum(long_position_exit_wasIn[start : i+1, s]))
        }
        else if (s %!in% my_portfolio_short && s %in% worst_last_month) {
          tmp_short = c(tmp_short, sum(long_position_enter_stay[start : i+1, s]))
        }
        else if (s %in% my_portfolio_short && s %in% worst_last_month) {
          tmp_short = c(tmp_short, sum(midRate_rx_t1[start:i+1, s]))
        }
        else{print("Error")}
      }
      short[idx] = mean(tmp_short, na.rm = T)
      if (p_prev == 0) {short[idx-1] = 0}
      
      # longshort
      longshort[idx-1] = sum(long[idx-1] - short[idx-1])
      
      # at the end of the month I adjust my portfolio
      my_portfolio_long = best_last_month
      my_portfolio_short = worst_last_month
      
    }
    print("-------------------------------")
    print(paste("Average Portfolio size:", mean(sizes), "Time: ", update))
    returns$long = long
    returns$short = short
    returns$longshort = longshort
    return(returns)
  }
  
  
  # customed print function for beta
  print_portfoliosBETA = function(funct, share, name, min_max=c(-200, 200), c) {
    
    new_p = funct(share)
    print(length(new_p$long))
    plot(ts(data = cumsum(new_p$long), start=c(1983,11), deltat = update),
         xaxt='n',
         ylim=min_max,
         col=darkcols[1], lwd=2, type='line', ylab = ("Cumulative log_returns"),
         xlab = "Time", main = paste("Cumulative Returns portfolio", name, share*100, "%"))
    
    abline(h=0, col= "black")
    
    lines(ts(data = cumsum(new_p$short),  start=c(1983,11), deltat = update), 
          xaxt='n',
          ylim=min_max,
          col=darkcols[2], lwd=2, type='line')

        legend(x = "topleft", 
           legend = c("High", "Low"), 
           lty = c(1,1), lwd = 3,
           col=c(darkcols[1], darkcols[2]),
           cex=1)
    
    
    s = as.numeric(new_p$short)
    mean_s = mean((s), na.rm = T)
    median_s = median((s), na.rm = T)
    sdev =  sd(na.omit(s))
    sk = skewness(na.omit(s))
    kurt = kurtosis(na.omit(s))
    varisk = VaR(na.omit(s), p=.95, method = "historical") # with probability p I loose more than this value
    es = ES(na.omit(s), p=.95, FUN="StdDev")
    sharpe = SharpeRatio(na.omit(ts(data = s), end=c(2019, 5), frequency = 12), p=0.95)
    print(paste("Avg low beta", mean_s, "median:", median_s,
                "sd:", sdev , "skw:", sk, "kurt: ", kurt, "VaR:", varisk, "ES:" , es,
                "Sharpe:", sharpe[1])) # 1 StdDev 2 VaR 3 ES
    
    l = as.numeric(new_p$long)
    mean_l = mean((l), na.rm = T)
    median_l = median((l), na.rm = T)
    sdev =  sd(na.omit(l))
    sk = skewness(na.omit(l))
    kurt = kurtosis(na.omit(l))
    varisk = VaR(na.omit(l), p=.95, method = "historical") # with probability p I loose more than this value
    es = ES(na.omit(l), p=.95, FUN="StdDev")
    sharpe = SharpeRatio(na.omit(ts(data = l), end=c(2019, 5), frequency = 12), p=0.95)
    print(paste("Avg high beta", mean_l, "median:", median_l,
                "sd:", sdev , "skw:", sk, "kurt: ", kurt, "VaR:", varisk, "ES:" , es,
                "Sharpe:", sharpe[1])) # 1 StdDev 2 VaR 3 ES
      }
  
  c = 1
  for (i in c(0.20, 0.3)) {
    print_portfoliosBETA(portfolioShareBETA.create,i, paste("BETA", update, "months"), c(-0.5, 1),c)
    c = c+1
  }
  
}
