setwd("C:/Users/david/Nextcloud2/Uni/R_Divers/Tradingbot")

library(tidyverse)
library(RedditExtractoR)
library(rvest)
library(purrr)
library(httr)
library(jsonlite)
library(binancer)
library(tidyquant)
library(forecast)
library(TTR)
library(xts)
library(geckor)
library(reshape2)
library(gridExtra)
library(lubridate)
library(Quandl)
library(RSelenium)
library(robotstxt)
library(NLP)
library(openxlsx)

# Load relevant functions
myRSI <- function (price,n){
  N <- length(price)
  U <- rep(0,N)
  D <- rep(0,N)
  rsi <- rep(NA,N)
  Lprice <- Lag(price,1)
  for (i in 2:N){
    if (price[i]>=Lprice[i]){
      U[i] <- 1
    } else {
      D[i] <- 1
    }
    if (i>n){
      AvgUp <- mean(U[(i-n+1):i])
      AvgDn <- mean(D[(i-n+1):i])
      rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
    }
  }
  rsi <- reclass(rsi, price)
  return(rsi)
}

# Define relevant parameters
prices <- c()
RSI_vec <- c()
LT_lagged_vec <- c()
up <- c()
down <- c()
oscillation <- c()
TA_list <- list()
res_list <- list()
ma_long_vec <- c()
ma_med_vec <- c()
ma_short_vec <- c()
positions_date <- c()
positions_type <- c()
transaction = T
df.transactions = data.frame(buy=NA, sell=NA, date=NA, profit=NA, date_close=NA, type=NA)
ma_dist <- c()

rm(resistance_frame)
rm(current_RSI)
rm(lt_up)

i=1
i_count=1
count_trend = 1
reset_i = 100 #after how many iterations i is reset
# if reset_i is activated, the break statement will never be TRUE, hence the code will run forever (careful!)
maxit = 500
RSI_period = 42 #days to calculate the RSI
req_speed = 10 #number of seconds between requests
param_a = 10 #short-term trend price -> depending on frequency this may change
param_b = 50 #long-term trend parameter
max_memory_lenght = 500 #set the maximum length for the list that stores TA data !has to be longer than resistance_days!
resistance_treshold = 0.0005 #set the epsilon condition for calculating the resistance interval
resistance_days = 100 #number of days the resistance bounds should be checked for
ma_long_period = 200 #days for long-term moving average
ma_med_period = 50 #days for mid-term moving average
ma_short_period = 20 #days for short-term moving average
transaction = T
RSI_upper = 70
RSI_lower = 30

# Define the profit limits (need to change this as this does not really make sense for profit_take)
profit_take = 0.008
stop_loss = 0.002

# Iteration
Tradng_bot_try = 3

# choose which indicators to use
# does not work with evaluation so far so leave it on 1 for now
rsi = T
short_term_trend = T
support_resistance = T
moving_avg = T
# generally define: 0 = sell, 1 = buy, 2 = hold

# Set up RSelenium for Bitcoin price from bitcointicker website
rD = rsDriver(port = 4577L, chromever = "111.0.5563.64", check = T, verbose = T)
remDr = rD[["client"]]
url = "https://bitcointicker.co/coinbase/btc/usd/1hr/"
remDr$navigate(url)

rD[["server"]]$stop()
# Trading algorithm
repeat{
  if (max_memory_lenght < resistance_days){
    warning("Memomry is shorter than resistance period! This produces nonsense results!")
    break
  }
  # get crypto price via binance API start #
  res <- GET("https://api.binance.com/api/v3/ticker/price?symbol=BTCUSDT")
  time <- Sys.time()
  response = rawToChar(res$content)
  data = fromJSON(response)
  prices <- append(prices, as.numeric(data$price), after = length(prices))
  # get crypto price via binance API end #
  
  # # get crypto price via RSelenium
  # price = remDr$findElements(using = "id", value = "lastTrade")
  # price.out = price[[1]]$getElementAttribute("innerHTML")
  # prices = append(prices, as.numeric(price.out[[1]]), after = length(prices))
  # time = Sys.time()
  # # end RSelenium
  
  # apply RSI function and append RSI value
  if (rsi==TRUE){
    if (i >= 2){
      RSI_vec <- append(RSI_vec, tail(myRSI(prices,RSI_period),1), after = length(RSI_vec))
      if (length(RSI_vec) >= RSI_period){
        if (RSI_vec[i-1] >= RSI_upper){
          current_RSI <- 0
          # print("sell by RSI")
        } else if (RSI_vec[i-1] <= RSI_lower){
          current_RSI <- 1
          # print("buy by RSI")
        } else {
          current_RSI <- 2
          # print("hold by RSI")
        }
      }
    }
  }
  # Try to implement moving average with TTR package
  if (i >= ma_long_period){
    ma_long_vec = SMA(prices, n=ma_long_period)
  }
  if (i >= ma_med_period){
    ma_med_vec = SMA(prices, n=ma_med_period)
    
    # Track some additional data to identify trading patterns: distance between the MAs
    ma_dist = append(ma_dist, ma_short_vec[i]-ma_med_vec[i], after = length(ma_dist))
  }
  if (i >= ma_short_period){
    ma_short_vec = SMA(prices, n=ma_short_period)
  }
  if (length(ma_long_vec) >= 2){
    ma_long_vec = na.omit(ma_long_vec)
    # Case 1: length(ma_long_vec) < ma_long_period
    if (length(ma_long_vec) <= ma_long_period){
      if (sum(diff(ma_long_vec)) > 0){
        lt_up = 1
      } else if (sum(diff(ma_long_vec)) < 0){
        lt_up = 0
      } else {
        lt_up = 2
      }
    }
    # Case 2: length(ma_long_vec) > ma_long_period
    if (length(ma_long_vec) > ma_long_period){
      if (sum(diff(ma_long_vec[count_trend:(ma_long_period+count_trend)])) > 0){
        lt_up = 1
      } else if (sum(diff(ma_long_vec[count_trend:((count_trend-1)+ma_long_period)])) < 0){
        lt_up = 0
      } else {
        lt_up = 2
      }
      count_trend = count_trend+1
    }
  }
  
  # # Variant 1: Golden Cross verified by long-term trend behavior ################
  # if (exists("lt_up")){
  #   if (tail(ma_short_vec,1)>tail(ma_med_vec,1) & lt_up == 1){
  #     crs = 1
  #     # print("buy by moving average cross")
  #   } else if (tail(ma_med_vec,1)>tail(ma_med_vec,1) & lt_up == 0){
  #     crs = 0
  #     # print("sell by moving average cross")
  #   } else {
  #     crs = 2
  #     # print("no action by moving average")
  #   }
  # }
  # # Variant 1 ends here ###########################################################
  
  # Variant 2: short-term MA crossing mid and long-term MA for trend purchase #######
  # Maybe add a candle memory later to further verify the trend 
  if (exists("lt_up")){
    if (tail(ma_short_vec,1)>tail(ma_med_vec,1) & tail(ma_short_vec,1)>tail(ma_long_vec,1)){
      crs = 1
      print("crs=1")
    } else if (tail(ma_short_vec,1)<tail(ma_med_vec,1) & tail(ma_short_vec,1) < tail(ma_long_vec,1)){
      crs = 0
    } else {
      crs = 2
    }
  }
  # Variant 2 ends here #############################################################
  
  # Collect TA output and make trading decision
  # if statement is restricted by the max_memory_length + RSI_period !! -> solve later
  if (i > ma_long_period){
    if (exists("current_RSI")){
      out <- data.frame(date = time, RSI = current_RSI, cross = crs)
      TA_list <- append(TA_list, list(out), after = length(TA_list))
      out <- NULL
      if (transaction){
        if (TA_list[[i-ma_long_period]]$RSI == 1 | TA_list[[i-ma_long_period]]$cross == 1){ #& TA_list[[i-ma_long_period]]$direction == "up"
          positions_date <- append(positions_date, i-1, after = length(positions_date))
          positions_type <- append(positions_type, 1, after = length(positions_type))
          # print("buy")
          # Transaction
          date = (i-1)
          buy = prices[i-1]
          est_close = prices[i-1]*(1+profit_take)
          transaction = F
        } else if (TA_list[[i-ma_long_period]]$RSI == 0 & TA_list[[i-ma_long_period]]$cross == 0){ #& TA_list[[i-ma_long_period]]$direction == "down" 
          positions_date <- append(positions_date, i-1, after = length(positions_date))
          positions_type <- append(positions_type, 0, after = length(positions_type))
          # print("sell")
        } else {
          print("no action")
        }
      }
      # if (length(TA_list) == max_memory_lenght){
      #   TA_list <- tail(TA_list, -1)
      # }
    }
    
    # Define stopping value -> currently; if the price has risen for a certain margin (e.g. 0.2%) then we sell. Include a full tracker
    # here that makes decision based on reversed crossing or something so that it runs until a signal and not just profit margin
    
    if (!transaction){
      if (prices[i-1] > est_close){
        profit = (prices[i-1]-buy)/buy
        tr = c(as.numeric(buy),as.numeric(prices[i-1]),as.numeric(date),as.numeric(profit*100),as.numeric((i-1)),"Take profit")
        df.transactions = rbind(df.transactions,tr)
        print("Transaction closed")
        transaction=T
      } else if (prices[i-1] < buy*(1-stop_loss)){
        profit = (prices[i-1]-buy)/buy
        tr = c(as.numeric(buy),as.numeric(prices[i-1]),as.numeric(date),as.numeric(profit*100),as.numeric((i-1)),"Stop loss")
        df.transactions = rbind(df.transactions,tr)
        print("Stop loss")
        transaction=T
      }
    }
  }
  Sys.sleep(req_speed) # set request speed
  print(i)
  i=i+1
  i_count=i_count+1
  if (i_count==100){
    i_count=1
  }
  # if reset_i is activated, the break statement will never be TRUE, hence the code will run forever (careful!)
  # if (i == reset_i){
  #   i = 1
  # }
  if (i == maxit){
    # draw a plot of the decisions (only works when a trade has been executed)
    ma_lt = c(rep(NA, length(prices)-length(ma_long_vec)), ma_long_vec)
    ma_mt = c(rep(NA, length(prices)-length(ma_med_vec)), ma_med_vec)
    ma_st = c(rep(NA, length(prices)-length(ma_short_vec)), ma_short_vec)
    d.out = data.frame(price = prices, ma_lt = ma_lt, ma_mt = ma_mt, ma_st = ma_st)
    d.out = melt(d.out)
    ind = rep(seq.int(1,length(prices)), ncol(d.out))
    d.out$ind = ind
    segments = data.frame(date = positions_date, prc = prices[positions_date])
    p1 = ggplot()+
      geom_line(data=d.out, aes(x=ind, y=value, color=factor(variable)))+
      geom_segment(data=segments, aes(x=date, y=prc+10, xend=date, yend=prc),
                   arrow = arrow(length = unit(0.2, "cm")))+
      theme_bw()+
      guides(color="none")
    
    rsi.out <- data.frame(ind=seq.int(1,length(prices)), rsi = c(rep(NA, length(prices)-length(RSI_vec)), RSI_vec))
    p2 = ggplot(data=rsi.out)+
      geom_line(aes(x=ind, y=rsi))+
      geom_hline(yintercept = c(RSI_lower,RSI_upper), color=c("red","blue"))+
      theme_bw()
    grid.arrange(p1,p2,nrow=2)
    
    #Write output to an excel file
    parameters <- data.frame(RSI_period = RSI_period, RSI_up = RSI_upper, RSI_down = RSI_lower,
                             price_frequency = req_speed, ma_long = ma_long_period, ma_med = ma_med_period,
                             ma_short = ma_short_period, SL=stop_loss, PT=profit_take)
    list_out = list("Transactions" = df.transactions, "Parameters" = parameters)
    write.xlsx(list_out, file = paste0("Trading_bot_",Tradng_bot_try,".xlsx"))
    
    png(file = paste0("C:/Users/david/Nextcloud2/Uni/R_Divers/Tradingbot/Run_number_",Tradng_bot_try,".png"), width = 1920, height = 1080)
    grid.arrange(p1,p2,nrow=2)
    dev.off()
    break
  }
}

i = 1
freq = 30
prices = c()
time_vec = c()
repeat {
  res <- GET("https://api.binance.com/api/v3/ticker/price?symbol=BTCUSDT")
  time <- Sys.time()
  response = rawToChar(res$content)
  data = fromJSON(response)
  prices <- append(prices, as.numeric(data$price), after = length(prices))
  time_vec = append(time_vec, time, after = length(time_vec))
  Sys.sleep(freq)
  print(i)
  i=i+1
  if (i == 10000){
    break
  }
}

btc_out = data.frame(price=prices, date=time_vec)
write.xlsx(btc_out, file = "btc_4.xlsx")













