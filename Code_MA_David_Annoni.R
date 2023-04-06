library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(devtools)
library(maps)
library(ggrepel)
library(RColorBrewer)
library(quantmod)
library(ggplot2)
library(tidyverse)
library(tidyquant)
library(writexl)
library(rugarch)
library(tseries)
library(PerformanceAnalytics)
library(xts)
library(moments)
library(MASS)
library(formattable)
library(readxl)
library(reshape)
library(pdqr)
library(gridExtra)
library(forecast)
library(fable)
library(PearsonDS)
library(profvis)
library(dqrng)
library(webshot)
library(htmltools)
library(likert)
library(weathermetrics)

setwd("C:/Users/david/Nextcloud2/Uni/UniZürich/Master Thesis/Cosimo Munari/Thesis Paper/Data")

############
# plot aesthetic parameters
# axis.text = element_text(size=30)
# axis.title = element_text(size=30)
# axis.title.x = element_blank()
# axis.title.y = element_blank()
# legend.text = element_text(size=30)
# legend.title = element_text(size=36)
# plot.title = element_text(size=50)

#install_version("sf", version = "1.0-7", repos = "http://cran.us.r-project.org")
#############################################
#import dataset for natural catastrophes#####
#############################################
NatCat <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZürich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Global NatCat_1970.xlsx")

#merge datasets
data(World)
WorldNatCat <- merge(World,NatCat, by="name")

#create aggregate data for plotting
AggWorldNatCat <- aggregate(WorldNatCat$Cost, by=list(name=WorldNatCat$name), FUN=sum)
AggWorldNatCat <- merge(World,AggWorldNatCat, by="name", all.x=TRUE)
names(AggWorldNatCat)[names(AggWorldNatCat)=="x"] <- "Cost"

#plotting maps
#Figure 3
tmap_mode("view")
tmap_style("col_blind")

world <- tm_shape(AggWorldNatCat)+
  tm_polygons("Cost", 
              border.col = "black",
              border.alpha = .5,
              breaks=c(seq(0,100,by=20), Inf),
              palette = "Reds",
              title="Damage US$ bln")+
  tm_layout(main.title = "Countries affected by natural disasters 1970-2020",
            main.title.size = 1,
            legend.title.size = 0.8,
            legend.text.size = 0.5,
            legend.position = c(0,0.2))

tmap_save(world, "World_map_damage.png", width=1920, height=1080, asp = 0)

#############################################################
##########Plot total damage and occurences###################
#############################################################
Type_NatCat <- NatCat %>%
  group_by(Type) %>%
  summarise(n = n(),
            tcost = sum(Cost),
            tfat = sum(Fatalities, na.rm = T))

sf <- max(Type_NatCat$tcost)/max(Type_NatCat$n)
DF_long <- Type_NatCat %>%
  mutate(n=n*sf) %>%
  pivot_longer(names_to = "y_new", values_to = "val", tcost:n)

#Figure 4
ggplot(DF_long, aes(x=reorder(Type, -val)))+
  geom_bar(aes(y=val, fill=y_new, group=y_new),
           stat="identity", position=position_dodge(),
           color="black", alpha=.6)+
  labs(fill="variable")+
  scale_fill_discrete(labels = c("Total number of events (RHS scaled at Tropical Cyclone)","Total damage (LHS)"))+
  scale_y_continuous(name="Damage in USD billion", 
                     labels = scales::comma, sec.axis = sec_axis(~./sf, name="Number of occurrences", labels = scales::comma))+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_bw()+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        legend.justification = "left",
        legend.position = c(0.4,0.75),
        legend.text = element_text(size=36),
        plot.title = element_text(size=50))+
  guides(fill = guide_legend(title = ""))+
  ggtitle("Natural disasters causing >1 billion USD damage by count and type")

#######################################################################################
######Calculating total returns########################################################
#######################################################################################
#Risk-free asset (optional)
ffr <- read.csv("fed-funds-rate-historical-chart.csv", header=T, sep=",")
ffr$date <- as.Date(ffr$date)
ffr <- ffr %>%
  filter(date >= "2001-01-01") %>%
  filter(date <= "2021-12-31")
ffr <- ffr %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(mv = mean(value))

ffr$Portfolio <- 1000
for (i in 2:nrow(ffr)){
  ffr$Portfolio[i] <- ffr$Portfolio[i-1] + ffr$Portfolio[i-1]*ffr$mv[i-1]/100
}

#annual return cash account
AR_Cashacc = (((tail(ffr$Portfolio,1)/head(ffr$Portfolio,1))^(1/nrow(ffr)))-1)*100


#load return and dividend data
ticker <- c("ALL","ALV.DE","ZURN.SW","SREN.SW","CS.PA","TRV","MUV2.DE","HNR1.DE","VIG.VI",
            "^GDAXI","^GSPC","^FCHI")
getSymbols(ticker,src="yahoo", from ="2001-01-01", to="2021-12-31", auto.assing = F)

#only use this code to get dividends once -> use csv after as the csv is cleaned and checked for errors
#also include; ALL, CAC, DAX, GSPC
# dividends <- xts()
# for (sym in ticker){
#   dividends <- merge(dividends, getDividends(sym, src="yahoo", from ="2000-01-01", to="2021-12-31"))
# }

#take date out of index to Row 1
# dividends <- as.data.frame(dividends)
# dividends <- cbind(date = rownames(dividends), dividends)
# dividends$date <- format(as.POSIXct(dividends$date), format = "%Y-%m-%d")

# write_xlsx(x=dividends, "dividends.xlsx", col_names = T, format_headers = T)

dividends <- read_excel("dividendos.xlsx")

#approximate missing values with na.approx()?
#remove unnecessary columns

ZURN.SW <- na.omit(ZURN.SW)
ZURN.SW <- subset(ZURN.SW, select = -c(ZURN.SW.Open, ZURN.SW.High, ZURN.SW.Low, ZURN.SW.Adjusted))
ZURN.SW$Returns <- CalculateReturns(ZURN.SW$ZURN.SW.Close, method="simple")

# normal_SR_annualized <- as.data.frame(matrix(data=NA, nrow=2,ncol=1))
# normal_SR_annualized$ZURN.SW[1] <- SharpeRatio.annualized(ZURN.SW$Returns, Rf=0.0)

ZURN.SW <- as.data.frame(ZURN.SW)
ZURN.SW <- cbind(date = rownames(ZURN.SW), ZURN.SW)

# Portfolio_ZURN.SW <- as.data.frame(Portfolio_ZURN.SW) 
# Portfolio_ZURN.SW <- cbind(date = rownames(Portfolio_ZURN.SW), Portfolio_ZURN.SW)
# names(Portfolio_ZURN.SW)[names(Portfolio_ZURN.SW)=="ZURN.SW.Close"] <- "ZURN.SW_Stock_return"

ZURN.SW$Portfolio_Value_CGR <- 1000                  #initial portfolio value set to 1000 #CGR = Capital gains

for (i in 2:nrow(ZURN.SW)){
  ZURN.SW$Portfolio_Value_CGR[i] <- ZURN.SW$Portfolio_Value_CGR[i-1]*(1+ZURN.SW$Returns[i])
}

ZURN.SW$Portfolio_Value_DR <- 1000                                                   #initiate CG + Dividends reinvested
ZURN.SW$SharesHeld <- ZURN.SW$Portfolio_Value_DR[1]/ZURN.SW$ZURN.SW.Close[1]        #calculate the first number of shares held

dividends_ZURN.SW <- dividends %>%
  dplyr::select(date, ZURN.SW.div)

ZURN.SW <- left_join(ZURN.SW, dividends_ZURN.SW, by="date")

#Calculate portfolio value with reinvesting dividends (buying new shares for the given price at dividend date)

#ZURN.SW indexes of ZURN.SW merged when a dividend was paid
#initialize matches index for following while loop
matches <- which(!is.na(ZURN.SW$ZURN.SW.div)==TRUE)
matches <- c(2,matches)
matches <- append(matches, nrow(ZURN.SW))

#calculate number of new shares acquired with dividends
i = matches[1]

while (i <= matches[2]){
  if (!is.na(ZURN.SW$ZURN.SW.div[i])){
    ZURN.SW$SharesHeld[i] <- ZURN.SW$SharesHeld[i-1]+ZURN.SW$SharesHeld[i-1]*(ZURN.SW$ZURN.SW.div[i]/ZURN.SW$ZURN.SW.Close[i])
    matches <- matches[-1]
  } else {
    ZURN.SW$SharesHeld[i] <- ZURN.SW$SharesHeld[i-1]
  }
  i <- i+1
}

#calculate portfolio returns (portfolio_value_DR) with #shares_held and closing prices
for (i in 2:nrow(ZURN.SW)){
  ZURN.SW$Portfolio_Value_DR[i] <- ZURN.SW$SharesHeld[i]*ZURN.SW$ZURN.SW.Close[i]
}

#annualized returns
TR_DRI = (tail(ZURN.SW$Portfolio_Value_DR, n=1)-head(ZURN.SW$Portfolio_Value_DR, n=1))/head(ZURN.SW$Portfolio_Value_DR, n=1)
TR_CGR = (tail(ZURN.SW$Portfolio_Value_CGR, n=1)-head(ZURN.SW$Portfolio_Value_CGR, n=1))/head(ZURN.SW$Portfolio_Value_CGR, n=1)
AR_TR_DRI = 100*((TR_DRI+1)^(1/21)-1)
AR_TR_CGR = 100*((TR_CGR+1)^(1/21)-1)

ZURN.SW$Returns_div <- NA

for (i in 3:nrow(ZURN.SW)){
  ZURN.SW$Returns_div[i] <- (ZURN.SW$Portfolio_Value_DR[i]-ZURN.SW$Portfolio_Value_DR[i-1])/ZURN.SW$Portfolio_Value_DR[i-1]
}

sr_returns$ZURN.SW[1] <- (nthroot((prod(1+ZURN.SW$Returns_div, na.rm = T)^252),252)-1)/sqrt(252)*sqrt(sd(ZURN.SW$Returns_div, na.rm = T))

#calculate moments of returns to check
mn <- round(mean(ZURN.SW$Returns, na.rm = T),5)
var <- round(sd(ZURN.SW$Returns, na.rm = T)^2,5)
skw <- round(skewness(ZURN.SW$Returns, na.rm = T),5)
kur <- round(kurtosis(ZURN.SW$Returns, na.rm = T),5)

#plot return distribution
x <- rnorm(10000,mean=mn,sd=sqrt(var))
x <- as.data.frame(x)

# ggplot(ZURN.SW, aes(x=Returns, y=..density..))+
#   geom_histogram(binwidth = 0.005, color="black", fill="white")+
#   geom_density(data=x, aes(x=x, y=..density..), color="red")+
#   geom_vline(xintercept = 0, color="blue", linetype="dotted")+
#   annotate("text", x=0.1,y=28, label=paste0("kurtosis=",kur))+
#   annotate("text", x=0.1,y=27, label=paste0("skewness=",skw))+
#   annotate("text", x=0.1,y=26, label=paste0("variance=",var))+
#   annotate("text", x=0.1,y=25, label=paste0("mean=",mn))+
#   ggtitle("Return distribution of ZURN.SW")+
#   theme(axis.text = element_text(size=18),
#         axis.title = element_text(size=18))+
#   scale_x_continuous(limits = c(min(ZURN.SW$Returns, na.rm = T),max(ZURN.SW$Returns, na.rm = T)), labels = scales::percent)+
#   theme_bw()

#
#plot returns matched to top 10 disasters (from xts() data)
NatCat2000_top15 <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZürich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Global NatCat_2000_top20.xlsx")
NatCat2000_top10 <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZürich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Global NatCat_2000_top10.xlsx")
NatCat2000_top52 <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZürich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Global NatCat_2000_top52_R.xlsx")
x_pos <- NatCat2000_top10$`date start`

GDAXI$date <- as.Date(GDAXI$date)

#######################################################
#############Main model starts here####################
#######################################################
###############################
#####estimating parameters#####
###############################
#load EM-DAT Dataframe
NatCat_EMDat <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZürich/Master Thesis/Cosimo Munari/Thesis Paper/Data/emdat_public_2022_07_16_query_uid-i1CDMR.xlsx")
NatCat_EMDat$Yearclass <- NA
unique(NatCat_EMDat$`Disaster Type`)

#Load climate indicators
ocean_heat <- read.table("https://www.ncei.noaa.gov/data/oceans/woa/DATA_ANALYSIS/3M_HEAT_CONTENT/DATA/basin/pentad/pent_h22-w0-2000m.dat", header=T)
sea_temp <- read.csv("sea_temp.csv", header=T, sep=",")
land_temp <- read.csv("land_temp.csv", header=T, sep=",")

#Assign yearclass
Timespan <- as.numeric(max(NatCat_EMDat$Year)) - as.numeric(min(NatCat_EMDat$Year))
Period_span <- 10
Periods <- Timespan/Period_span

for (i in 1:nrow(NatCat_EMDat)){
  if (NatCat_EMDat$Year[i] >= 1970 & NatCat_EMDat$Year[i] < 1980){
    NatCat_EMDat$Yearclass[i] <- 1
  } else if (NatCat_EMDat$Year[i] >= 1980 & NatCat_EMDat$Year[i] < 1990){
    NatCat_EMDat$Yearclass[i] <- 2
  } else if (NatCat_EMDat$Year[i] >= 1990 & NatCat_EMDat$Year[i] < 2000){
    NatCat_EMDat$Yearclass[i] <- 3
  } else if (NatCat_EMDat$Year[i] >= 2000 & NatCat_EMDat$Year[i] < 2010){
    NatCat_EMDat$Yearclass[i] <- 4
  } else {
    NatCat_EMDat$Yearclass[i] <- 5
  }
}


#find returns after disaster event
ZURN.SW$date <- as.Date(ZURN.SW$date)

#manually adjust date to the desired interval (3,5,6,7,14)
Katrina <- subset(ZURN.SW, date > "2005-08-23" & date < "2005-08-29")
Harvey <- subset(ZURN.SW, date > "2017-08-17" & date < "2017-08-23")
Maria <- subset(ZURN.SW, date > "2017-09-16" & date < "2017-09-22")
Sandy <- subset(ZURN.SW, date > "2012-10-22" & date < "2012-10-28")
Irma <- subset(ZURN.SW, date > "2017-08-30" & date < "2017-09-05")
German_floods <- subset(ZURN.SW, date > "2011-07-12" & date < "2011-07-18")
Michael <- subset(ZURN.SW, date > "2018-10-07" & date < "2018-10-13")
Wilma <- subset(ZURN.SW, date > "2005-10-15" & date < "2005-10-21")
NWS <- subset(ZURN.SW, date > "2021-02-13" & date < "2021-02-19")
Laura <- subset(ZURN.SW, date > "2020-08-20" & date < "2020-08-26")
Irene <- subset(ZURN.SW, date > "2011-08-20" & date < "2011-08-26")
Hagibis <- subset(ZURN.SW, date > "2019-10-04" & date < "2019-10-10")

Catdat <- rbind(Katrina, Harvey, Maria, Sandy, Irma, German_floods, Michael, Wilma, NWS, Laura, Irene, Hagibis)


#Setup 1 parameter estimation
#estimate shock probability
hist_shocks <- NatCat_EMDat %>%
  group_by(Yearclass) %>%
  summarize(adverse_event = round(sum(`Total Damages ('000 US$)`>1000000, na.rm=T)/nrow(NatCat_EMDat),6),
            severity = sum(`Total Damages ('000 US$)`, na.rm=T))

holt_model2 <- holt(hist_shocks$adverse_event, h=1)
holt_model1 <- holt(hist_shocks$severity, h=1)

#Setup 2 parameter estimation
#amplification
#on average the economic damage increases by a factor of 2.8x per decade
#in my model we use a factor of 1.37 -> might run it with 2.8x for more comparative results
a1 <- hist_shocks$severity[2]/hist_shocks$severity[1]
b1 <- hist_shocks$severity[3]/hist_shocks$severity[2]
c1 <- hist_shocks$severity[4]/hist_shocks$severity[3]
d1 <- hist_shocks$severity[5]/hist_shocks$severity[4]

# #probability of shock
# #on average the probability of shock increased by a factor of 2.9x
# #in my model we use a factor of 3.37% -> might run it with 7.399% for more comparative results
a <- hist_shocks$adverse_event[2]/hist_shocks$adverse_event[1]
b <- hist_shocks$adverse_event[3]/hist_shocks$adverse_event[2]
c <- hist_shocks$adverse_event[4]/hist_shocks$adverse_event[3]
d <- hist_shocks$adverse_event[5]/hist_shocks$adverse_event[4]


# parameters
# to switch between setup 1 and setup 2 manually select the 4 lines for setup 1 or setup 2
#setup 1 uses:
# prob_shock = holt_model2$mean[1]
# max_ampli = holt_model1$mean[1]/hist_shocks$severity[5]
# min_ampli = 1
# avg_returns = mean(Catdat$Returns)

#setup 2 uses:
prob_shock = hist_shocks$adverse_event[5]*((a+b+c+d)/4)
max_ampli = (a1+b1+c1+d1)/4
min_ampli = 1
avg_returns = mean(Catdat$Returns)

#some inputs
#years to forecast
sample_dist_stock <- new_r(ZURN.SW$Returns, type="continuous")
stock_returns <- sample_dist_stock(500000)
sample_dist_Catdat <- new_r(Catdat$Returns, type="continuous")
natcat_vec <- sample_dist_Catdat(5000)
#N is number of paths (J)
N <- 250
#n_path is number of obserartions per path (K)
n_path <- 500
p <- matrix(data=NA, nrow=n_path, ncol=N)
p <- as.data.frame(p)
portfolio_forecast <- as.data.frame(matrix(data=NA, nrow=n_path, ncol=N))
moments_frame <- as.data.frame(matrix(data=NA, nrow=4, ncol=N))
ind_vec <- seq(1:nrow(portfolio_forecast))
starter <- tail(GDAXI$GDAXI.Close,1)
shock_vec <- c()
forecasted_mean <- c()
forcs <- c(starter)
#number of iterations of forecast: iterations*n_path (1 iteration takes 7.5 seconds with N=100 & n_path=1000)
count <- 1
#L
iterations <- 100

###this is the algorithm (main model)
repeat {
  for (j in 1:N){
    shock_vec <- c()
    forcs <- c(starter)
    for (i in 1:n_path){
      shock <- sample.int(2, 1, prob=c(1-prob_shock,prob_shock))
      if(shock==2){
        amplification <- runif(1, min = 1, max = max_ampli)
        if (avg_returns > 0){
          ad_returns <- runif(1, 0, avg_returns)
        } else {
          ad_returns <- runif(1, avg_returns, 0)
        }
        employed_shock <- sample(natcat_vec, 1)+ad_returns*amplification
        shock_vec <- append(shock_vec, employed_shock, after = length(shock_vec))
      } else {
        shock_vec <- append(shock_vec, sample(stock_returns, 1), after = length(shock_vec))
      }
      forcs <- append(forcs, (1+shock_vec[i])*forcs[i], after = length(forcs))
    }
    p[,j] <- shock_vec
    forcs <- head(forcs, -1)
    portfolio_forecast[,j] <- forcs
  }
  portfolio_forecast$index <- ind_vec
  portfolio_forecast_long <- melt(portfolio_forecast, id.vars="index")
  portfolio_forecast$index <- NULL
  
  #mean of GDAXI endpoints of each iteration
  forecasted_mean <- append(forecasted_mean, rowMeans(portfolio_forecast[nrow(portfolio_forecast),]), after = length(forecasted_mean))
  count = count+1
  print(count)
  if (count == iterations+1){
    print(mean(forecasted_mean))
    break
  }
}


# !!!!!!run this code only once at the beginning. Otherwise it will overwrite the output (out_dat)!!!!!!
# out_dat <- as.data.frame(matrix(data=NA, nrow=7, ncol=length(ticker)))
# colnames(out_dat) <- ticker
# rownames(out_dat) <- c("S_0","S_500","ret_diff","ad_ret","avg_ret","max_ampli","Observations ('000')")
# out_dat$`^GDAXI` <- NULL
# #
# percent <- function(x, digits = 2, format = "f", ...) {
#   paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
# }


#store output in dataframe
out_dat$ZURN.SW[1] <- round(starter,2)
out_dat$ZURN.SW[2] <- round(mean(forecasted_mean),2)
out_dat$ZURN.SW[3] <- percent((as.numeric(out_dat$ZURN.SW[2])-as.numeric(out_dat$ZURN.SW[1]))/as.numeric(out_dat$ZURN.SW[1]))
out_dat$ZURN.SW[4] <- percent(round(avg_returns,4))
out_dat$ZURN.SW[5] <- percent(round(mean(ZURN.SW$Returns, na.rm=T),4))
out_dat$ZURN.SW[6] <- percent(round(max_ampli-1,4))
out_dat$ZURN.SW[7] <- N*n_path*iterations/1000


# optional
# plot portfolio development
  # ggplot(data=portfolio_forecast_long, aes(x=index, y=value))+
  #   geom_line(aes(group=variable))+
  #   geom_smooth(method="lm", se=T)+
  #   # geom_segment(aes(
  #   #   x=0, y=rowMeans(portfolio_forecast[1,]),
  #   #   xend=n_path, yend=mean(forecasted_mean), color="red"))+
  #   # annotate("text", x=20,y=max(portfolio_forecast[-(n_path+1)])*1, label=paste0("kurtosis=",out[4]))+
  #   # annotate("text", x=20,y=max(portfolio_forecast[-(n_path+1)])*0.95, label=paste0("skewness=",out[3]))+
  #   # annotate("text", x=20,y=max(portfolio_forecast[-(n_path+1)])*0.9, label=paste0("variance=",out[2]))+
  #   # annotate("text", x=20,y=max(portfolio_forecast[-(n_path+1)])*0.85, label=paste0("mean=",out[1]))+
  #   theme_bw()+
  #   ggtitle(paste("Price forecast for GDAXI"))+
  #   xlab("")+
  #   ylab("Price in US$")+
  #   theme(axis.text = element_text(size=30),
  #         axis.title = element_text(size=30),
  #         axis.title.x = element_blank(),
  #         plot.title = element_text(size=50))

# formattable(out_dat)


############################################
###Verify randomness of adverse returns#####
############################################
#Requires xts data (getSymbols)

ZURN.SW <- na.omit(ZURN.SW)
ZURN.SW <- subset(ZURN.SW, select = -c(ZURN.SW.Open, ZURN.SW.High, ZURN.SW.Low, ZURN.SW.Adjusted))
ZURN.SW$Returns <- CalculateReturns(ZURN.SW$ZURN.SW.Close, method="simple")
ZURN.SW <- as.data.frame(ZURN.SW)
ZURN.SW <- cbind(date = rownames(ZURN.SW), ZURN.SW)

count = 0
sample_size = 500
lag_period = 7  #3,5,6,7,14
returns <- c()

repeat{
  d <- c()
  d <- append(d, sample(seq(as.Date("2001-01-01"), as.Date("2021-12-31"), by="day"), 1), after = length(d))
  d <- seq(d,d+lag_period, length.out = lag_period)
  s <- subset(ZURN.SW, as.Date(date) >= d[1] & as.Date(date) <= d[lag_period])
  returns <- append(returns, mean(s$Returns), after = length(returns))
  count = count+1
  print(count)
  if (count == sample_size){
    paste("ZURN.SW avg_ret:" ,print(round(100*(mean(returns, na.rm = T)),5)), sep=" ")
    break
  }
}


##################################################
###########Prepare output plots###################
##################################################
output_total <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZürich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Output_total.xlsx")
output_total_prepared <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZürich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Output_total_prepared.xlsx")

# formattable(output_total_prepared)

#Output prep
#data prep
ret_diff <- output_total %>%
  filter(Var == "ret_diff")
ad_ret <- output_total %>%
  filter(Var == "ad_ret")
avg_ret <- output_total %>%
  filter(Var == "avg_ret")

ret_diff[14:23] <- NULL
ret_diff$Var <- NULL
ret_diff <- ret_diff %>%
  rownames_to_column("id") %>%
  gather()
ret_diff <- ret_diff[-c(1:10),]

ad_ret[14:23] <- NULL
ad_ret$Var <- NULL
ad_ret <- ad_ret %>%
  rownames_to_column("id") %>%
  gather()
ad_ret <- unique(ad_ret)
ad_ret <- ad_ret[-c(1:10),]

avg_ret[14:23] <- NULL
avg_ret$Var <- NULL
avg_ret <- unique(avg_ret)
avg_ret <- avg_ret %>%
  rownames_to_column("id") %>%
  gather()
avg_ret <- avg_ret[-c(1),]

t1 <- str_split(ad_ret$value, pattern = "%")
t2 <- str_split(avg_ret$value, pattern = "%")
t3 <- str_split(ret_diff$value, pattern = "%")
ad_ret_vec <- c()
avg_ret_vec <- c()
ret_diff_vec <- c()
for (i in 1:58){
  ad_ret_vec <- append(ad_ret_vec, t1[[i]][1], after = length(ad_ret_vec))
}
for (i in 1:12){
  avg_ret_vec <- append(avg_ret_vec, t2[[i]][1], after = length(avg_ret_vec))
}
for (i in 1:120){
  ret_diff_vec <- append(ret_diff_vec, t3[[i]][1], after = length(ret_diff_vec))
}
ad_ret$value <- ad_ret_vec
avg_ret$value <- avg_ret_vec
ret_diff$value <- ret_diff_vec

ad_ret <- ad_ret %>%
  mutate(value = as.numeric(value))
# avg_ret <- avg_ret %>%
#   mutate(value = as.numeric(value)/100)
ret_diff <- ret_diff %>%
  mutate(value = as.numeric(value))

#start plotting
ret_diff_pos <- subset(ret_diff, value >= 0.00)
ret_diff_neg <- subset(ret_diff, value < 0.00)
avg_new_ret <- ret_diff %>%
  group_by(key) %>%
  summarise(mn = mean(value))

#Figure 7 output plot
colors3 <- c("positive" = "palegreen2", "negative" = "indianred1", 
             "mean historical" = "royalblue1", "mean forecasted" = "navyblue")
p10 <- ggplot()+
  geom_point(data=ret_diff_pos, aes(x=key, y=as.numeric(value)), color="palegreen2", size=8)+
  geom_point(data=ret_diff_neg, aes(x=key, y=as.numeric(value)), color="indianred1", size=8)+
  # geom_point(data=avg_ret, aes(x=key, y=as.numeric(value)), color="royalblue1", size=6)+
  geom_point(data=avg_new_ret, aes(x=key, y=mn), color="navyblue", size=6)+
  theme_bw()+
  ggtitle(paste("Forecasted return of endpoints"))+
  xlab("")+
  ylab("Return in %")+
  scale_x_discrete(limits = c("GSPC","GDAXI","FCHI","ALL","ALV.DE","TRV","VIG.VI","ZURN.SW","CS.PA",
                              "SREN.SW","MUV2.DE","HNR1.DE"))+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(size=50),
        legend.text = element_blank())


p2 <- ggplot()+
  geom_point(data=S_500_insset, aes(x=key, y=log(as.numeric(value))), 
             size=6)+
  geom_point(data=S_0_insset, aes(x=key, y=log(as.numeric(value))),
             color="red",
             size=8)+
  geom_point(data=S_500_indexset, aes(x=key, y=log(as.numeric(value))),
             size=6)+
  geom_point(data=S_0_indexset, aes(x=key, y=log(as.numeric(value))),
             color="red",
             size=8)+
  scale_x_discrete(limits = c("ALL","ALV.DE","TRV","VIG.VI","ZURN.SW","CS.PA",
                              "SREN.SW","MUV2.DE","HNR1.DE","GSPC","GDAXI","FCHI"))+
  theme_bw()+
  ggtitle(paste("Price forecast for re-/insurers"))+
  xlab("")+
  ylab("Price log(USD)")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(size=50))

grid.arrange(p2,p10, ncol=2)

#Figure 6 plot
ad_ret_pos <- subset(ad_ret, value >= 0)
ad_ret_neg <- subset(ad_ret, value <0)

colors4 <- c("positive" = "palegreen2", "negative" = "indianred1")
ggplot()+
  geom_point(data=ad_ret_pos, aes(x=key, y=as.numeric(value), color="positive"), size=8)+
  geom_point(data=ad_ret_neg, aes(x=key, y=as.numeric(value), color="negative"), size=8)+
  theme_bw()+
  ggtitle(paste("Periodic returns for disaster subsets"))+
  xlab("")+
  ylab("Return in %")+
  scale_x_discrete(guide = guide_axis(n.dodge=2), limits = c("GSPC","GDAXI","FCHI","ALL","ALV.DE","TRV","VIG.VI","ZURN.SW","CS.PA",
                                                             "SREN.SW","MUV2.DE","HNR1.DE"))+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=50),
        legend.text = element_text(size=30))+
  scale_color_manual(values=colors4)+
  labs(color="")


#########################################
##Insurance Data Graphics################
#########################################
Sigma_explorer_data <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Sigma explorer data.xlsx")

Sigma_explorer_data <- Sigma_explorer_data %>%
  mutate(`Real premium growth life`=`Real premium growth life`/100)

ggplot(Sigma_explorer_data)+
  geom_point(aes(x=Date, y=`Total damage`), color="red")+
  geom_point(aes(x=Date, y=`Insured losses`), color="blue")+
  theme_bw()

colors <- c("Total damage" = "blue", "Insured damage" = "red")

ggplot(Sigma_explorer_data)+
  geom_line(aes(x=Date, y=`Total damage`, color="Total damage"))+
  geom_smooth(aes(x=Date, y=`Total damage`), method = lm, se=F, color="blue", linetype="dotted")+
  geom_line(aes(x=Date, y=`Insured losses`, color="Insured damage"))+
  geom_smooth(aes(x=Date, y=`Insured losses`), method = lm, se=F, color="red", linetype="dotted")+
  ggtitle("Total and insured damage from 1970-2021 caused by natural disasters (inflation adjusted)")+
  labs(x="Date",
       y="US$ billion",
       color="Legend")+
  scale_color_manual(values=colors)+
  theme_bw()+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        legend.justification = "left",
        legend.position = c(.1,.85),
        legend.title = element_blank(),
        legend.text = element_text(size=24),
        plot.title = element_text(size=36))+
  annotate(geom = "point", x=c(1995,2005,2008,2011,2017), y=c(282,324,311,476,368), color="blue", size=2)+
  annotate(geom = "text", x=1995, y=305, label="USD 284.38 bln", size=10)+
  annotate(geom = "text", x=2005, y=355, label="USD 326.14 bln", size=10)+
  annotate(geom = "text", x=2008, y=334, label="USD 313.33 bln", size=10)+
  annotate(geom = "text", x=2011, y=499, label="USD 478.70 bln", size=10)+
  annotate(geom = "text", x=2017, y=391, label="USD 370.09 bln", size=10)

###########################################
#######Prepare companies to look at########
###########################################

####################
##Interest rates####
####################

ir1 <- read.csv("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Interest rates/WS_CBPOL_D_csv_row.csv", header=T, sep=",")
ir1 <- ir1[-c(2,3,4,5,6,7,8),]
ir1 <- ir1[c("Frequency","D.Daily.36","D.Daily.35")]
ir1$Frequency <- as.character(ir1$Frequency)
ir1$D.Daily.36 <- as.numeric(as.character(ir1$D.Daily.36))
ir1$D.Daily.35 <- as.numeric(as.character(ir1$D.Daily.35))
ir1 <- subset(ir1, Frequency >= "2000-01-01" & Frequency <= "2021-31-12")

ir1$interest_diff_EU <- NA
ir1$interest_diff_US <- NA

for (i in 2:nrow(ir1)){
  ir1$interest_diff_EU[i] <- ir1$D.Daily.36[i]-ir1$D.Daily.36[i-1]
  ir1$interest_diff_US[i] <- ir1$D.Daily.35[i]-ir1$D.Daily.35[i-1]
}
ir1 <- subset(ir1, interest_diff_US!="0" | interest_diff_EU!="0")
colnames(ir1) <- c("date start","EU","US","interest_diff_EU","interest_diff_US")

ir1$`date start` <- as.Date(ir1$`date start`)

top_52 <- read_excel("Global NatCat_2000_top52.xlsx")
top_52$`date start` <- as.Date(top_52$`date start`)
top_52$`date end` <- as.Date(top_52$`date end`)

for (i in 1:nrow(ir1)){
  for (j in 1:nrow(top_52)){
    if(between(ir1$`date start`[i], top_52$`date start`[j], top_52$`date end`[j])){
      top_52$`interest rate change`[j] <- 1
    }
  }
}

write_xlsx(top_52, "Global NatCat_2000_top52_R.xlsx")

#######################################################
##################Real effects analysis################
#######################################################
ALL_ratio <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Ratios/Allstate.xlsx")
ALL_ratio <- ALL_ratio %>%
  mutate(`Net income` = `Net income`/1000)
TRV_ratio <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Ratios/Travelers.xlsx")
AXA_ratio <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Ratios/Axa.xlsx")
Sren_ratio <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Ratios/Swiss Re.xlsx")
Mren_ratio <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Ratios/Munich Re.xlsx")
Hren_ratio <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Ratios/Hannover Re.xlsx")
Vienna_ratio <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Ratios/Vienna ins.xlsx")
Zurn_ratio <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Ratios/Zurich.xlsx")
ALV_ratio <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Ratios/Allianz.xlsx")

ratios <- rbind(ALL_ratio, TRV_ratio, AXA_ratio, Sren_ratio, Mren_ratio, Hren_ratio, Vienna_ratio, Zurn_ratio, ALV_ratio)
#scaling net income
ratios <- ratios %>%
  mutate(scaled_ni = 100*(`Net income`/mean(`Net income`, na.rm = T)))

ratios$Year <- strptime(as.character(ratios$Year), "%Y")
ratios$Year <- format(ratios$Year, "%Y-%m-%d")

ratio_prim <- rbind(ALL_ratio, TRV_ratio, AXA_ratio, Vienna_ratio, Zurn_ratio, ALV_ratio)
ratios_re <- rbind(Sren_ratio, Mren_ratio, Hren_ratio)

ratio_prim <- ratio_prim %>%
  mutate(scaled_ni = 100*(`Net income`/mean(`Net income`, na.rm = T)))
ratios_re <- ratios_re %>%
  mutate(scaled_ni = 100*(`Net income`/mean(`Net income`, na.rm = T)))

ratio_prim$Year <- strptime(as.character(ratio_prim$Year), "%Y")
ratio_prim$Year <- format(ratio_prim$Year, "%Y-%m-%d")

ratios_re$Year <- strptime(as.character(ratios_re$Year), "%Y")
ratios_re$Year <- format(ratios_re$Year, "%Y-%m-%d")


p1 <- ggplot(data=ratios_re)+
  geom_point(aes(x=as.Date(Year), y=`Combined ratio`, color=Symbol), size=6)+
  geom_hline(yintercept = 100, color="red", linetype="dotted", lwd=1.5)+
  # geom_vline(xintercept = as.numeric(as.Date(x_pos)),
  #            color="red", size=.5, linetype="dashed")+
  theme_bw()+
  ggtitle("Combined ratio and Net income")+
  labs(x = "",
       y = "CR %")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size=50),
        legend.text = element_text(size=30),
        legend.title = element_blank())

p2 <- ggplot(data=ratio_prim)+
  geom_point(aes(x=as.Date(Year), y=`Combined ratio`, color=Symbol), size=6)+
  geom_hline(yintercept = 100, color="red", linetype="dotted", lwd=1.5)+
  # geom_vline(xintercept = as.numeric(as.Date(x_pos)),
  #            color="red", size=.5, linetype="dashed")+
  theme_bw()+
  labs(x = "",
       y = "CR %")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size=50),
        legend.text = element_text(size=30),
        legend.title = element_blank())

p3 <- ggplot(data=ratios_re)+
  geom_line(aes(x=as.Date(Year), y=`Net income`, color=Symbol), lwd=2)+
  theme_bw()+
  labs(x = "",
       y = "NI USD Billion")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size=50),
        legend.text = element_text(size=30),
        legend.title = element_blank())

p4 <- ggplot(data=ratio_prim)+
  geom_line(aes(x=as.Date(Year), y=`Net income`, color=Symbol), lwd=2)+
  scale_x_date(date_breaks = "3 year", date_labels = "%Y")+
  theme_bw()+
  labs(x = "",
       y = "NI USD Billion")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=50),
        legend.text = element_text(size=30),
        legend.title = element_blank())

png(file = "C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Graphics/Combined_ratio.png", width = 1920, height = 1080)
grid.arrange(p1,p3,p2,p4, nrow=4)
dev.off()

Sren <- lm(data=Sren_ratio, `Net income`~`Combined ratio`)
summary(Sren)
Hren <- lm(data=Hren_ratio, `Net income`~`Combined ratio`)
summary(Hren)
Mren <- lm(data=Mren_ratio, `Net income`~`Combined ratio`)
summary(Mren)
all <- lm(data=ALL_ratio, `Net income`~`Combined ratio`)
summary(all)
trv <- lm(data=TRV_ratio, `Net income`~`Combined ratio`)
summary(trv)
axa <- lm(data=AXA_ratio, `Net income`~`Combined ratio`)
summary(axa)
vin <- lm(data=Vienna_ratio, `Net income`~`Combined ratio`)
summary(vin)
zurn <- lm(data=Zurn_ratio, `Net income`~`Combined ratio`)
summary(zurn)
alv <- lm(data=ALV_ratio, `Net income`~`Combined ratio`)
summary(alv)

Sren$coefficients[2]#* 1%
Hren$coefficients[2]#
Mren$coefficients[2]#* 1%
all$coefficients[2]#*** 0%
trv$coefficients[2]#*** 0%
axa$coefficients[2]#** 0.1%
vin$coefficients[2]#
zurn$coefficients[2]#*** 0%
alv$coefficients[2]#** 0.1%

# Combined Ratio plot (optional)
# ggplot(data=ratios)+
#   geom_line(aes(x=as.Date(Year), y=`Combined ratio`, color=Symbol))+
#   geom_hline(yintercept = 100, color="red")+
#   geom_vline(xintercept = as.numeric(as.Date(x_pos)),
#              color="red", size=.5, linetype="dashed")+
#   theme_bw()+
#   ggtitle("Combined ratio matched to top 12 natural catastrophes")+
#   labs(x = "",
#        y = "Combined ratio in %",
#        color = "Stock")+
#   theme(axis.text = element_text(size=30),
#         axis.title = element_text(size=30),
#         axis.title.x = element_blank(),
#         plot.title = element_text(size=50))

#################################
##########Figure 1###############
#################################

#total return and sr plots (returns data is calculated above)
#Calculate total annualized returns 
returns <- as.data.frame(matrix(data=NA, ncol=9, nrow=2))
colnames(returns) <- ticker_returns
rownames(returns) <- c("R_ann_DRI","R_ann_R")

#SR and IR
sr_returns <- as.data.frame(matrix(data=NA, nrow=1, ncol=9))
rownames(sr_returns) <- c("SR_ann")
colnames(sr_returns) <- ticker_returns

GSPC$Returns_div <- NA

for (i in 3:nrow(GSPC)){
  GSPC$Returns_div[i] <- (GSPC$Portfolio_Value_DR[i]-GSPC$Portfolio_Value_DR[i-1])/GSPC$Portfolio_Value_DR[i-1]
}

sr_returns$GSPC[1] <- (nthroot((prod(1+GSPC$Returns_div, na.rm = T)^252),252)-1)/sqrt(252)*sqrt(sd(GSPC$Returns_div, na.rm = T))

#Figure 1
test <- melt(returns)
test$DRI <- seq(1,0)
test <- test %>%
  mutate(value = value/100)
test2 <- melt(sr_returns)

p1 <- ggplot(data = test, aes(x=value, y=reorder(variable, +value), fill=factor(DRI)))+
  geom_bar(stat="identity", position = "dodge2")+
  theme_bw()+
  ggtitle("Total return 2001-2021")+
  ylab("Stock")+
  scale_x_continuous(labels = scales::percent)+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        legend.text = element_text(size=30),
        legend.title = element_text(size=36),
        plot.title = element_text(size=50))+
  guides(fill = guide_legend(title = "DRI"))

p2 <- ggplot(data=test2, aes(x=value, y=reorder(variable, +value)))+
  geom_bar(stat="identity")+
  theme_bw()+
  ggtitle("Annualized Sharpe Ratio")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=30),
        legend.title = element_text(size=36),
        plot.title = element_text(size=50))

grid.arrange(p1, p2, ncol=2)


#Figure 9
re_dat <- subset(ratios_re, Symbol == "MUV2.DE" | Symbol == "HNR1.DE")

ggplot(data=subset(re_dat, Year > "2001-31-12"), aes(x=as.Date(Year), y=`natcat effect`, fill=Symbol))+
  geom_bar(stat="identity", position="dodge2")+
  geom_smooth(se=F, method = "lm")+
  theme_bw()+
  scale_x_date(date_breaks = "3 year", date_labels = "%Y")+
  ggtitle("Natural disaster effect on combined ratio")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=24),
        legend.title = element_blank(),
        plot.title = element_text(size=50))

#Figure 17
damo <- read_excel("C:/Users/david/Nextcloud2/Uni/UniZ?rich/Master Thesis/Cosimo Munari/Thesis Paper/Data/Damodaran/Dataset.xlsx")
damo$Year <- as.numeric(damo$Year)

ggplot(data=damo, aes(x=Year, y=Inst_holdings, fill=factor(Industry)))+
  geom_bar(stat="identity", position = "dodge2")+
  scale_fill_brewer(palette = "Paired")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  ggtitle("Share of institutional ownership")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=24),
        legend.title = element_blank(),
        plot.title = element_text(size=50))



#Figure 12
ggplot(NatCat_EMDat, aes(sample=`Total Damages ('000 US$)`/1000000, color=`Disaster Type`))+
  stat_qq(size=6)+
  stat_qq_line()+
  facet_grid(cols = vars(Yearclass))+
  theme_bw()+
  ggtitle(paste("QQ plot of damage by Type"))+
  xlab("")+
  ylab("Count")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=50),
        strip.text = element_text(size=20),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30))

#Figure 13
ggplot(data = NatCat_EMDat)+
  geom_histogram(aes(x=log(`Total Damages ('000 US$)`), y=..count.., color=`Disaster Type`, fill=`Disaster Type`), bins=25)+
  facet_grid(cols=vars(Yearclass))+
  theme_bw()+
  ggtitle(paste("Log distribution of damage by Type"))+
  xlab("Log(billion USD)")+
  ylab("Count")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=50),
        strip.text = element_text(size=20),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30))



#Figure 4
s <- NatCat_EMDat %>%
  filter(`Total Damages ('000 US$)`/1000000 > 1) 

totd <- s %>%
  dplyr::select(`Disaster Subtype`,`Total Damages ('000 US$)`)
names(totd)[2] <- "damage"

totd <- totd %>%
  group_by(`Disaster Subtype`) %>%
  summarise(damage = sum(damage))
totd$type <- 1

totinsd <- s %>%
  dplyr::select(`Disaster Subtype`,`Insured Damages ('000 US$)`)
names(totinsd)[2] <- "damage"
totinsd <- totinsd %>%
  group_by(`Disaster Subtype`) %>%
  summarise(damage = sum(damage, na.rm = T))
totinsd$type <- 2

number <- s %>%
  dplyr::select(`Disaster Subtype`) %>%
  count(`Disaster Subtype`)
number$type <- 3
names(number)[2] <- "damage"

sf <- max(totd$damage)/max(number$damage)
number <- number %>%
  mutate(damage = damage*sf)

total <- rbind(totd, totinsd, number)
total$`Disaster Subtype`[9] <- "Land fire"
total$`Disaster Subtype`[22] <- "Land fire"
total$`Disaster Subtype`[35] <- "Land fire"

ggplot(data=total, aes(x=reorder(`Disaster Subtype`, -damage), y=damage/1000000, fill=factor(type)))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_y_continuous("Billion USD",
                     sec.axis = sec_axis(~./5, name="Number of observations"))+
  scale_fill_manual(values=c("dodgerblue", "dodgerblue4" , "darkseagreen"))+
  theme_bw()+
  ggtitle(paste("Economic and insured damage by subtype"))+
  xlab("")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=50),
        legend.title = element_blank(),
        legend.position = "none")

#Figure 14
country <- s %>%
  group_by(Country) %>%
  summarise(sm = sum(`Total Damages ('000 US$)`),
            smi = sum(`Insured Damages ('000 US$)`, na.rm=T)) 

country <- arrange(country, -sm)
big <- head(country,sum(country$sm > mean(country$sm)))
small <- tail(country,sum(country$sm <= mean(country$sm)))

big[nrow(big)+1,1] <- "Other"
big[nrow(big),2] <- sum(small$sm)
big[nrow(big),3] <- sum(small$smi)


smi <- big %>%
  dplyr::select(Country, smi)
sm <- big %>%
  dplyr::select(Country, sm)

smi$type <- 2
sm$type <- 1

names(smi)[2] <- "damage"
names(sm)[2] <- "damage"

tot <- rbind(smi, sm)
tot$Country[1] <- "USA"
tot$Country[11] <- "USA"

ggplot(data=tot, aes(x=reorder(Country, -damage), y=damage/1000000, fill=factor(type)))+
  geom_bar(stat="identity", position = "dodge2", color="black")+
  theme_bw()+
  ggtitle(paste("Economic and insured damage by country"))+
  scale_fill_manual(values = c("dodgerblue","dodgerblue4"))+
  xlab("")+
  ylab("USD Billion")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=50),
        legend.title = element_blank(),
        legend.position = "none")

#Figure 15
country <- s %>%
  group_by(Country) %>%
  count(Country) 

country <- arrange(country, -n)
big <- head(country,sum(country$n > mean(country$n)))
small <- tail(country,sum(country$n <= mean(country$n)))

big[nrow(big)+1,1] <- "Other"
big[nrow(big),2] <- sum(small$n)

sm <- big %>%
  dplyr::select(Country, n)

sm$Country[1]<-"USA"
sm$Country[10]<-"UK"

ggplot(data=sm, aes(x=reorder(Country, -n), y=n))+
  geom_bar(stat="identity", color="black", fill="darkseagreen")+
  theme_bw()+
  ggtitle("Number of events by geographic region")+
  xlab("")+
  ylab("Number of events")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=50))


#Figure 10 
#plot dividend development
colors1 <- c("Travelers" = "royalblue", "Allstate" = "palevioletred", "Axa" = "orangered",
             "Vienna ins" = "palegreen", "Zurich" = "darkcyan", "Allianz" = "cyan")
colors2 <- c("Swiss Re"="royalblue","Hannover Re"="palevioletred","Munich Re"="orangered")

p1 <- ggplot(data=dividends)+
  geom_point(aes(x=as.Date(date), y=SREN.SW.div, color="Swiss Re"), size=4)+
  geom_point(aes(x=as.Date(date), y=MUV2.DE.div, color="Munich Re"), size=4)+
  geom_point(aes(x=as.Date(date), y=HNR1.DE.div, color="Hannover Re"), size=4)+
  geom_vline(xintercept = as.numeric(as.Date(x_pos)),
             color="red", size=.5, linetype="dashed")+
  ggtitle("Dividends per share in USD")+
  scale_color_manual(values = colors2)+
  theme_bw()+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=24),
        legend.title = element_blank(),
        plot.title = element_text(size=50))

p2 <- ggplot(data=dividends)+
  geom_point(aes(x=as.Date(date), y=TRV.div, color="Travelers"), size=4)+
  geom_point(aes(x=as.Date(date), y=ALL.div, color="Allstate"), size=4)+
  geom_point(aes(x=as.Date(date), y=CS.PA.div, color="Axa"), size=4)+
  geom_point(aes(x=as.Date(date), y=VIG.VI.div, color="Vienna ins"), size=4)+
  geom_point(aes(x=as.Date(date), y=log(ZURN.SW.div), color="Zurich"), size=4)+
  geom_point(aes(x=as.Date(date), y=ALV.DE.div, color="Allianz"), size=4)+
  geom_vline(xintercept = as.numeric(as.Date(x_pos)),
             color="red", size=.5, linetype="dashed")+
  scale_color_manual(values = colors1)+
  theme_bw()+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=24),
        legend.title = element_blank(),
        plot.title = element_text(size=50))

grid.arrange(p1,p2, nrow=2)



#Figure 5
#decadal calculations
moments_frame_EMDAT <- as.data.frame(matrix(data=NA, nrow=Periods, ncol=4))
moments_frame_EMDAT <- NatCat_EMDat %>%
  group_by(Yearclass) %>%
  summarise(Mean = mean(`Total Damages ('000 US$)`/1000000, na.rm=T),
            Variance = sd(`Total Damages ('000 US$)`/1000000, na.rm=T)^2,
            Skewness = skewness(`Total Damages ('000 US$)`/1000000, na.rm=T),
            Kurtosis = kurtosis(`Total Damages ('000 US$)`/1000000, na.rm=T))

moments_frame_EMDAT$meangrowth <- NA
moments_frame_EMDAT$vargrowth <- NA
moments_frame_EMDAT$skewgrowth <- NA
moments_frame_EMDAT$kurgrowth <- NA

for (i in 2:nrow(moments_frame_EMDAT)){
  moments_frame_EMDAT$meangrowth[i] <- round((moments_frame_EMDAT$Mean[i]-moments_frame_EMDAT$Mean[i-1])/moments_frame_EMDAT$Mean[i-1],3)
  moments_frame_EMDAT$vargrowth[i] <- round((moments_frame_EMDAT$Variance[i]-moments_frame_EMDAT$Variance[i-1])/moments_frame_EMDAT$Variance[i-1],3)
  moments_frame_EMDAT$skewgrowth[i] <- round((moments_frame_EMDAT$Skewness[i]-moments_frame_EMDAT$Skewness[i-1])/moments_frame_EMDAT$Skewness[i-1],3)
  moments_frame_EMDAT$kurgrowth[i] <- round((moments_frame_EMDAT$Kurtosis[i]-moments_frame_EMDAT$Kurtosis[i-1])/moments_frame_EMDAT$Kurtosis[i-1],3)
}

#annual calculations
moments_frame_EMDAT_annual <- NatCat_EMDat %>%
  group_by(Year) %>%
  summarise(Mean = mean(`Total Damages ('000 US$)`/1000000, na.rm=T),
            Variance = sd(`Total Damages ('000 US$)`/1000000, na.rm=T)^2,
            Skewness = skewness(`Total Damages ('000 US$)`/1000000, na.rm=T),
            Kurtosis = kurtosis(`Total Damages ('000 US$)`/1000000, na.rm=T))

moments_frame_EMDAT_annual$meangrowth <- NA
moments_frame_EMDAT_annual$vargrowth <- NA
moments_frame_EMDAT_annual$skewgrowth <- NA
moments_frame_EMDAT_annual$kurgrowth <- NA

for (i in 2:nrow(moments_frame_EMDAT_annual)){
  moments_frame_EMDAT_annual$meangrowth[i] <- round((moments_frame_EMDAT_annual$Mean[i]-moments_frame_EMDAT_annual$Mean[i-1])/moments_frame_EMDAT_annual$Mean[i-1],3)
  moments_frame_EMDAT_annual$vargrowth[i] <- round((moments_frame_EMDAT_annual$Variance[i]-moments_frame_EMDAT_annual$Variance[i-1])/moments_frame_EMDAT_annual$Variance[i-1],3)
  moments_frame_EMDAT_annual$skewgrowth[i] <- round((moments_frame_EMDAT_annual$Skewness[i]-moments_frame_EMDAT_annual$Skewness[i-1])/moments_frame_EMDAT_annual$Skewness[i-1],3)
  moments_frame_EMDAT_annual$kurgrowth[i] <- round((moments_frame_EMDAT_annual$Kurtosis[i]-moments_frame_EMDAT_annual$Kurtosis[i-1])/moments_frame_EMDAT_annual$Kurtosis[i-1],3)
}

df <- NatCat_EMDat %>%
  dplyr::select(Year, Yearclass)
moments_frame_EMDAT_annual <- left_join(moments_frame_EMDAT_annual, df, by="Year")
moments_frame_EMDAT_annual <- unique(moments_frame_EMDAT_annual)


ocean_heat <- subset(ocean_heat, YEAR < 2022 & YEAR >= 1970)
ocean_heat <- ocean_heat %>%
  mutate(YEAR = floor(YEAR))

lmdat <- NatCat_EMDat %>%
  group_by(Year) %>%
  summarise(mn = mean(`Total Damages ('000 US$)`/1000000, na.rm=T))
lmdat <- head(lmdat, -1)

lmdat$Year <- as.numeric(lmdat$Year)
sea_temp$Year <- as.numeric(sea_temp$Year)

lmdat <- left_join(lmdat, sea_temp)
names(lmdat)[3] <- paste("sea_temp_ano")

df <- NatCat_EMDat %>%
  dplyr::select(Year, Yearclass)
df$Year <- as.numeric(df$Year)
lmdat <- left_join(lmdat, df, by="Year")
lmdat <- unique(lmdat)

df <- ocean_heat %>%
  dplyr::select(YEAR,WO)
colnames(df) <- c("Year","WO")
df$Year <- as.numeric(df$Year)
lmdat <- left_join(lmdat, df, by="Year")

lmdat <- left_join(lmdat, land_temp, by="Year")
names(lmdat)[6] <- paste("land_temp_ano")

moments_frame_EMDAT_annual$Year <- as.numeric(moments_frame_EMDAT_annual$Year)
lmdat <- full_join(lmdat, moments_frame_EMDAT_annual, by="Year")

lmdat$Yearclass.x <- NULL
lmdat$mn <- NULL

#predict indicator data
lmdat <- na.omit(lmdat)

sta_holt <- holt(lmdat$sea_temp_ano, 11)
wo_holt <- holt(lmdat$WO, 11)
lta_holt <- holt(lmdat$land_temp_ano, 11)

df <- as.data.frame(matrix(data=NA, ncol=13, nrow=11))
colnames(df) <- colnames(lmdat)
lmdat <- rbind(lmdat, df)

for (i in 50:nrow(lmdat)){
  lmdat$sea_temp_ano[i] <- sta_holt$mean[i-49]
  lmdat$WO[i] <- wo_holt$mean[i-49]
  lmdat$land_temp_ano[i] <- lta_holt$mean[i-49]
  lmdat$Yearclass.y[i] <- 6
  lmdat$Year[i] <- 2019+(i-49)
}


p1 <- ggplot(lmdat, aes(x=Year, y=WO, color=factor(Yearclass.y)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  theme_bw()+
  ggtitle("Ocean heat content")+
  xlab("")+
  ylab("Zettajoule")+
  theme(legend.position = "none",
        axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=33))

p2 <- ggplot(lmdat, aes(x=Year, y=land_temp_ano, color=factor(Yearclass.y)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  theme_bw()+
  ggtitle("Annual land temperature anomaly")+
  xlab("")+
  ylab("Kelvin")+
  theme(legend.position = "none",
        axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=33))

p3 <- ggplot(lmdat, aes(x=Year, y=sea_temp_ano, color=factor(Yearclass.y)))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  theme_bw()+
  ggtitle("Annual sea temperature anomaly")+
  labs(x="",
       y="Kelvin",
       color=" ")+
  scale_colour_discrete(labels=c("1970-1979","1980-1989","1990-1999","2000-2009","2010-2019","E2020-E2029"))+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=33),
        legend.text = element_text(size=30),
        legend.position = c(-2.05,0.85))

grid.arrange(p1,p2,p3, ncol=3)


# Figure 16
sstpdi = read.csv("cyclones_fig-3.csv", header = T, sep=",")
colnames(sstpdi) = c("Year","SST","PDI")

sstpdi = sstpdi %>%
  mutate(SST=fahrenheit.to.celsius(SST))

sp = melt(sstpdi)
sp = sp %>%
  filter(variable != "Year") %>%
  mutate(Year = rep(seq(min(sstpdi$Year),max(sstpdi$Year)),2))

var.labs = c("SST (in °C)","PDI (as units)")
names(var.labs) = c("SST", "PDI")

ggplot(data=sp, aes(x=Year, y=value, color=variable))+
  geom_line(lwd=2)+
  geom_smooth(method = "lm", se=F, color="black")+
  facet_grid(vars(variable), scales = "free", labeller = labeller(variable = var.labs))+
  theme_bw()+
  scale_x_continuous(breaks = seq(1950,2015,5))+
  xlab("")+
  ylab("")+
  ggtitle("Hurricane Power Dissipation and Sea Surface Temperture in the North Atlanic")+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30),
        axis.title.x = element_blank(),
        plot.title = element_text(size=33),
        legend.text = element_text(size=30),
        legend.position = c(-2.05,0.85),
        strip.text.y = element_text(size=30))






