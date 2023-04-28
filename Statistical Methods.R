setwd("C:/Users/david/Nextcloud2/Uni/R_Divers")

library(tidyverse)
library(plm)
library(e1071)
library(stargazer)
library(nlme)
library(gsynth)
library(gridExtra)
library(quantmod)
library(PerformanceAnalytics)
library(ggpubr)
library(patchwork)

#generate some random panel data
randat <- data.frame(year = rep(seq(2010,2020,1),4), 
                     x = rnorm(44,0,1), y = rnorm(44, 0.5, 0.09), 
                     x2 = rnorm(44,0,1), y2 = rnorm(44, 0.3, 0.1))
units <- c(rep(1,11), rep(2,11), rep(3,11), rep(4,11))
randat$units <- units

ggplot(data=randat)+
  geom_point(aes(x=x,y=y), color="red")+
  geom_point(aes(x=x2,y=y2), color="green")+
  theme_bw()

Standard_OLS <- lm(data=randat, x~y)
#### Gsynth with Stock data ####

dat_stocks <- read.csv("Stock_returns_data.csv", header=T, sep=",")

#calculate market excess return
dat_stocks <- dat_stocks %>%
  mutate(excess_return_company = return - gbond,
         treat_ind = ifelse(date >= as.Date("2021-06-06"),1,0),
         policy = treat_ind * treatment)

#filter data for time period to calculate the alphas and betas
gsynth_control <- dat_stocks %>%
  filter(date >= as.Date("2019-02-01") & date <= as.Date("2021-12-31"))%>%
  na.omit()

gsynth_control$date <- as.Date(gsynth_control$date)
gsynth_control <- as_tibble(gsynth_control)

fitted_values <- lmList(data=gsynth_control, return ~ dax_return | ticker)
coefs <- as.data.frame(summary(fitted_values)$coefficients)
coefs <- coefs[,c(1,5)]
colnames(coefs) <- c("alpha","beta")
coefs$ticker <- rownames(coefs)

dat_stocks <- left_join(dat_stocks, coefs, by="ticker")

dat_stocks <- dat_stocks %>%
  mutate(abnormal_return=return-alpha-beta*dax_return) %>%
  filter(date >= (as.Date("2021-06-06") - 200) & date <= (as.Date("2021-08-14") + 30)) %>%
  group_by(ticker)%>%
  mutate(car = cumsum(abnormal_return),
         date = as.Date(date))

######################
### Apply GSC ########
######################

dat_stocks <- dat_stocks %>%
  select(ticker, date, treatment, car, volume, policy) %>%
  na.omit()

# generate a loop to study parameters

# att <- c()
# cfact <- c()
# act <- c()
# max_r <- 25
# 
# for (k in 1:max_r){
#   system.time(
#     out <- gsynth(abnormal_return ~ int_start, 
#                   data = dat_stocks,
#                   index = c("ticker","date"), 
#                   force = "two-way", 
#                   CV = F, 
#                   r = k, 
#                   se = TRUE, 
#                   inference = "parametric", 
#                   nboots = 1000, 
#                   parallel = F)
#   )
#   att <- append(att, out$att, after = length(att))
#   cfact <- append(cfact, out$Y.bar[,2], after = length(cfact))
#   act <- append(act, out$Y.bar[,1], after = length(act))
#   print(k)
# }
# 
# result <- data.frame(att = att, counterfact = cfact, actual = act, date=NA, indicator=NA)
# result$date <- rep(date$date,max_r) 
# result$date <- as.Date(date$date)
# result$indicator <- rep(1:max_r, each=nrow(date))

# ggplot(data=result, aes(x=date))+
#   geom_line(aes(y=actual), lwd=1)+
#   geom_line(aes(y=counterfact), color="blue", lwd=1, linetype=2)+
#   geom_line(aes(y=att), color="firebrick1", linetype=3)+
#   geom_vline(xintercept = c(as.Date("2021-06-06")-15,as.Date("2021-06-06"), as.Date("2021-09-21")))+
#   geom_hline(yintercept = 0)+
#   theme_bw()

# car_factors_plot <- ggplot(data=result, aes(x=date, y=att, color=factor(indicator)))+
#   geom_line()+
#   theme_bw()+
#   scale_color_discrete(name="Number of factors (r)")

# Ab_ret_factors_plot <- ggplot(data=result, aes(x=date, y=att, color=factor(indicator)))+
#   geom_line()+
#   theme_bw()+
#   scale_color_discrete(name="Number of factors (r)")
# 
# grid.arrange(car_factors_plot, Ab_ret_factors_plot)

# # plot latent factors
# plot(out, type="factors", xlab="Time")

# Non-loop code from here onwards
system.time(
  out <- gsynth(car ~ policy,
                data = dat_stocks,
                index = c("ticker","date"),
                force = "two-way",
                CV = T,
                r = c(0,25),
                se = TRUE,
                inference = "parametric",
                nboots = 100,
                parallel = F)
)

################################################################################################################
out.dat <- data.frame(ytr=out$Y.bar, time=out$time, att=out$att)
out.dat <- out.dat %>%
  rename(Treatment = ytr.Y.tr.bar,
         Counterfactual = ytr.Y.ct.bar,
         Control = ytr.Y.co.bar)

#own plot with ggplot
result <- data.frame(att = out$att, counterfact = out$Y.bar[,2], actual = out$Y.bar[,1], date=NA)

date <- dat_stocks %>%
  filter(ticker=="COP.DE") %>%
  select(date)

result$date <- as.Date(date$date)

ggplot(data=result, aes(x=date))+
  geom_line(aes(y=actual), lwd=1)+
  geom_line(aes(y=counterfact), color="blue", lwd=1, linetype=2)+
  geom_line(aes(y=att), color="firebrick1", linetype=3)+
  geom_vline(xintercept = as.Date("2021-06-06"), color="red")+
  geom_hline(yintercept = 0)+
  theme_bw()

atts <- data.frame(att_excess_return_company = out$att, att_return = NA)
atts$att_return <- out$att

##########################################################################
##########################################################################
##########################################################################

#Gsynth on insurance companies
ticker <- c("ALL","ALV.DE","ZURN.SW","SREN.SW","CS.PA","TRV","MUV2.DE","HNR1.DE","VIG.VI","^GSPC")
ticker <- c("ALL","ALV.DE","ZURN.SW","SREN.SW","CS.PA","TRV","MUV2.DE","HNR1.DE","VIG.VI","^GDAXI","^GSPC","^FCHI")

Start = ("2020-04-01")
End = ("2022-01-01")

Stocks = lapply(ticker, function(sym) {
  na.omit(getSymbols(sym, from=Start, to=End, auto.assign=FALSE))
})

# test <- do.call(merge,Stocks)
#calculate the simple arithmetic returns
names(Stocks) <- ticker

ret <- function(x,i){
  cbind(x[i], CalculateReturns(x[i][,4]))
}
Stocks <- lapply(Stocks, ret)
Stocks <- lapply(Stocks, as.data.frame)

for (i in 1:length(Stocks)){
  Stocks[[i]] <- setNames(Stocks[[i]], c("open","high","low","close","volume","adjusted","return"))
  Stocks[[i]]$ticker <- names(Stocks)[i]
}

#get lengths of each dataframe in list for later

for(i in 1:length(Stocks)){
  Stocks[[i]]$date <- as.Date(rownames(Stocks[[i]]))
  rownames(Stocks[[i]]) <- seq(1,nrow(Stocks[[i]]),1)
}

Stocks <- do.call(rbind, Stocks)

# Load US 10 yr treasury 
USTB <- read.csv("DGS10.csv", header = T, sep=",")
USTB$DATE <- as.Date(USTB$DATE)
USTB$DGS10 <- as.numeric(USTB$DGS10)
USTB$DGS10 <- na.approx(USTB$DGS10)

Stocks_wip <- left_join(Stocks, USTB, by=c("date" = "DATE"))

# n_days = as.numeric(max(Stocks_wip$date)-min(Stocks_wip$date))

Stocks_wip <- Stocks_wip %>%
  na.omit() %>%
  group_by(ticker) %>%
  mutate(cum_ret = cumsum(return),
         n=1:n()) %>%
  ungroup() %>%
  mutate(DGS10 = DGS10/(100*365)) #annual payment

GSPC <- Stocks_wip %>%
  filter(ticker=="^GSPC") %>%
  select(date, return) %>%
  rename(index_return = return)

Stocks_wip <- left_join(Stocks_wip, GSPC, by="date")
Stocks_wip$index_return <- na.approx(Stocks_wip$index_return)
Stocks_wip <- Stocks_wip %>%
  mutate(mkt_excess = index_return - DGS10)

# rescale the treasury bond yield to daily returns to calculate daily market excess returns
ggplot(data=Stocks_wip, aes(x=date, y=cum_ret, color=ticker))+
  geom_line()+
  theme_bw()

# Estimate alpha and beta to calculate abnormal returns
Stocks_wip <- Stocks_wip %>%
  filter(!(ticker=="^GSPC"))
fits <- lmList(data=Stocks_wip, (return-DGS10) ~ mkt_excess | ticker)
coefs <- as.data.frame(summary(fits)$coefficients)
coefs <- coefs[,c(1,5)]
colnames(coefs) <- c("alpha","beta")
coefs$ticker <- rownames(coefs)

Stocks_wip <- left_join(Stocks_wip, coefs, by="ticker")

Stocks_wip <- Stocks_wip %>%
  mutate(ab_ret = return-alpha-beta*mkt_excess) %>%
  group_by(ticker) %>%
  mutate(car = cumsum(ab_ret)) %>%
  ungroup()

ggplot(data=Stocks_wip, aes(x=date, y=car, color=ticker))+
  geom_line()+
  theme_bw()

