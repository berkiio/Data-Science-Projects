setwd()

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
Stock_data4 <- read.csv("Stock_data_7.csv", header=T, sep=",")

#calculate market excess return
Stock_data5 <- Stock_data4 %>%
  mutate(mkt_excess = dax_return - gbond,
         ind_excess = return - gbond,
         treat_ind = ifelse(date >= as.Date("2021-09-04"),1,0),
         policy = treat_ind * treatment)

#Calculate the abnormal return using the market model [OLS](R_it = alpha_i + beta_i*MKT_t) for the control period from 2020-07-01 to 2021-07-26
#filter data for time period to calculate the alphas and betas
Stock_data_DID_control_period <- Stock_data5 %>%
  filter(date >= as.Date("2020-07-01") & date <= as.Date("2021-10-16"))%>%
  na.omit()
#count Einträge per ticker
# x <- Stock_data_DID_control_period %>% 
#   filter(!is.na(return)) %>% 
#   group_by(ticker) %>% 
#   count()

Stock_data_DID_control_period$date <- as.Date(Stock_data_DID_control_period$date)
Stock_data_DID_control_period <- as_tibble(Stock_data_DID_control_period)

#run regression for each ticker and store the results in a df
fits <- lmList(data=Stock_data_DID_control_period, return ~ dax_return | ticker)
coefs <- as.data.frame(summary(fits)$coefficients)
coefs <- coefs[,c(1,5)]
colnames(coefs) <- c("alpha","beta")
coefs$ticker <- rownames(coefs)

#ad the results to the data set
Stock_data5 <- left_join(Stock_data5, coefs, by="ticker")

#calculate the abnormal return
Stock_data5 <- Stock_data5 %>%
  mutate(ab_ret_manual=return-alpha-beta*dax_return)

#calculate the cumulative abnormal return for within a specific time period
as.Date("2021-09-04")-246
#246
#define the period to calculate the abnormal return
#daysbefore = days before the AD
daysbefore <- 246
#daysafter = days after the ED
daysafter <- 15

Stock_data5 <- Stock_data5%>%
  filter(date >= (as.Date("2021-09-04") - daysbefore) & date <= (as.Date("2021-09-20") + daysafter))

#calculate cumulative abnormal return
Stock_data5 <- Stock_data5%>%
  group_by(ticker)%>%
  mutate(CAR = cumsum(ab_ret_manual))

#create an interaction variable between treatment and period before/after the time of interest 
Stock_data5 <- Stock_data5 %>%
  mutate(start = (ifelse (date >= as.Date("2021-08-16"),1,0))) %>%
  mutate(int_start = treatment * start)

# change "date" form character to date
Stock_data5$date <- as.Date(Stock_data5$date) 

###############################################################
############### Generalized synthetic Control #################
################# Cumulative Abnormal Return ##################
###############################################################
# #count Einträge per ticker
#Stock_data_gsynth_p1_car %>% 
#   filter(!is.na(CAR)) %>% 
#   group_by(ticker) %>% 
#   count()

#filter data set
Stock_data5 <- Stock_data5 %>%
  mutate(CAR=CAR,
         ab_ret_manual = ab_ret_manual,
         ind_excess = ind_excess)

Stock_data5 <- Stock_data5 %>%
  select(ticker, date, treatment, return, dax_return, gbond, mkt_excess, 
         ind_excess, alpha, beta, ab_ret_manual, CAR, start, int_start, volume) %>%
  na.omit()

#Run OLS regression to study explaining factors

#apply generalized synthetic control
#eventually insert cluster by industry (cl = "industry")

#Loop code from here onwards until Non-loop code segment

att <- c()
cfact <- c()
act <- c()
max_r <- 25

for (k in 1:max_r){
  system.time(
    out <- gsynth(ab_ret_manual ~ int_start, 
                  data = Stock_data5,
                  index = c("ticker","date"), 
                  force = "two-way", 
                  CV = F, 
                  r = k, 
                  se = TRUE, 
                  inference = "parametric", 
                  nboots = 1000, 
                  parallel = F)
  )
  att <- append(att, out$att, after = length(att))
  cfact <- append(cfact, out$Y.bar[,2], after = length(cfact))
  act <- append(act, out$Y.bar[,1], after = length(act))
  print(k)
}

result <- data.frame(att = att, counterfact = cfact, actual = act, date=NA, indicator=NA)
result$date <- rep(date$date,max_r) 
result$date <- as.Date(date$date)
result$indicator <- rep(1:max_r, each=nrow(date))

# ggplot(data=result, aes(x=date))+
#   geom_line(aes(y=actual), lwd=1)+
#   geom_line(aes(y=counterfact), color="blue", lwd=1, linetype=2)+
#   geom_line(aes(y=att), color="firebrick1", linetype=3)+
#   geom_vline(xintercept = c(as.Date("2021-09-04")-15,as.Date("2021-09-04"), as.Date("2021-09-21")))+
#   geom_hline(yintercept = 0)+
#   theme_bw()

# CAR_factors_plot <- ggplot(data=result, aes(x=date, y=att, color=factor(indicator)))+
#   geom_line()+
#   theme_bw()+
#   scale_color_discrete(name="Number of factors (r)")

# Ab_ret_factors_plot <- ggplot(data=result, aes(x=date, y=att, color=factor(indicator)))+
#   geom_line()+
#   theme_bw()+
#   scale_color_discrete(name="Number of factors (r)")
# 
# grid.arrange(CAR_factors_plot, Ab_ret_factors_plot)

# # plot latent factors
# plot(out, type="factors", xlab="Time")

# Non-loop code from here onwards
system.time(
  out <- gsynth(volume ~ int_start,
                data = Stock_data5,
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
################################################################################################################
out.dat <- data.frame(ytr=out$Y.bar, time=out$time, att=out$att)
out.dat <- out.dat %>%
  rename(Treatment = ytr.Y.tr.bar,
         Counterfactual = ytr.Y.ct.bar,
         Control = ytr.Y.co.bar)

#Figure XY: Showing observed treatment CAR and counterfactual
#data preparation
figure_count_d <- out.dat%>%
  pivot_longer(
    cols = c("Treatment", "Counterfactual", "Control", "att"),
    names_to = "category")%>%
  filter(category %in% c("Treatment", "Counterfactual"))

figure_count_d$category <- factor(figure_count_d$category, levels = c("Treatment", "Counterfactual"))

figure_count <- ggplot(data=figure_count_d)+
  geom_line(aes(x=time, y=value, color=category, group=category), lwd=1.5, key_glyph="path")+
  scale_x_date(date_labels = "%b %d")+
  ggtitle("(b) Actual and counterfactual CAR")+
  geom_vline(aes(xintercept = as.Date(c("2021-08-16")), linetype="AD15"), color = "black", lwd=1.5)+
  geom_vline(aes(xintercept = as.Date(c("2021-09-04")), linetype="AD"), color = "black", lwd=1.5)+
  geom_vline(aes(xintercept = as.Date(c("2021-09-20")), linetype="ED"), color = "black", lwd=1.5)+
  xlab("")+ 
  ylab("CAR %")+
  labs(color=NULL)+
  scale_linetype_manual(name = "", values = c(AD15="solid", AD = "dashed",ED = "dotdash"))+
  scale_color_manual(name = "" , values = c(Counterfactual = "blue", Treatment = "green"))+
  theme_bw()+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold"),
    legend.text = element_text(size=14),
    legend.spacing.y = unit(1, "cm"),
    legend.key.size = unit(1.5, "cm"),
    legend.position = "bottom"
    )+
  guides(color = guide_legend(byrow = T),
         linetype = guide_legend(byrow = T))


#Apendix xy (showing that paralell trend assumption is unlikely to hold)
#data preparation "#2E9FDF"
figure_DID_d <- out.dat%>%
  pivot_longer(
    cols = c("Treatment", "Counterfactual", "Control", "att"),
    names_to = "category")%>%
  filter(category %in% c("Treatment", "Control"))

figure_DID <- ggplot(data=figure_DID_d)+
  geom_line(aes(x=time, y=value, color=category, group=category, linetype=category), lwd=1.5)+
  scale_x_date(date_breaks = "week", date_labels = "%d-%m")+
  geom_vline(aes(xintercept = as.Date(c("2021-08-16")), color = "AD15", linetype="AD15"), lwd=1.5)+
  geom_vline(aes(xintercept = as.Date(c("2021-09-04")), color = "AD", linetype="AD",), lwd=1.5)+
  geom_vline(aes(xintercept = as.Date(c("2021-09-20")),  color = "ED", linetype="ED"), lwd=1.5)+
  ggtitle("(a) Parallel trend in a DID-Setting")+
  xlab("")+ 
  ylab("CAR %")+
  labs(color=NULL)+
  scale_color_manual(name = "test", values = c(Control = "red", Treatment="green", AD15="#000000", AD = "#000000",ED = "#000000"))+
  scale_linetype_manual(name = "test", values = c(Control = "solid", Treatment="solid", AD15="solid", AD = "dashed",ED = "dotdash"))+
  theme_bw()+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold")
  )

# Combine the datasets for legend
figure_all <- out.dat %>%
  pivot_longer(
    cols = c("Treatment", "Counterfactual", "Control"),
    names_to = "category")

figure_all$category <- factor(figure_all$category, levels = c("Treatment","Control","Counterfactual"))

leg_full <- ggplot(data=figure_all, aes(x=time, y=value, group=category, color=category))+
  geom_line(lwd=1.5)+ #key_glyph = "point"
  facet_grid(category~.)+
  geom_vline(aes(xintercept = as.Date(c("2021-08-16")), linetype="AD-15"), key_glyph = "vline", lwd = 1.5)+
  geom_vline(aes(xintercept = as.Date(c("2021-09-04")), linetype="AD"), key_glyph = "vline", lwd = 1.5)+
  geom_vline(aes(xintercept = as.Date(c("2021-09-20")), linetype="ED"), key_glyph = "vline", lwd = 1.5)+
  ggtitle("(a) Parallel trend in a DID-Setting")+
  xlab("")+ 
  ylab("CAR %")+
  labs(color=NULL)+
  scale_color_manual(name = "", values = c(Control = "red", Treatment="green", 
                                           Counterfactual = "blue"))+
  scale_linetype_manual(name = "", values = c(`AD-15`="solid", AD = "dashed",ED = "dotdash"),
                        breaks = c("AD-15","AD","ED"))+
  theme_bw()+
  theme(
    plot.title = element_text(color="black", size=24, face="bold.italic"),
    axis.title.x = element_text(color="black", size=20, face="bold"),
    axis.title.y = element_text(color="black", size=20, face="bold"),
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.spacing.y = unit(1, "cm"),
    legend.key.size = unit(1.5, "cm"))+
  guides(color = guide_legend(byrow = T),
         linetype = guide_legend(byrow = T))

leg <- get_legend(leg_full)

rm_legend <- function(p){p + theme(legend.position = "none")}
plots <- ggarrange(rm_legend(figure_DID), rm_legend(figure_count), nrow = 2)
ggarrange(plots, leg, widths = c(0.95,0.2))
################################################################################################################
################################################################################################################

plot(out)
plot(out, type = "counterfactual")

# print(out)
# out$est.att
# ATT_policy1_ar <- as.data.frame(out$est.att)
# out$est.avg

#own plot with ggplot
result <- data.frame(att = out$att, counterfact = out$Y.bar[,2], actual = out$Y.bar[,1], date=NA)

date <- Stock_data5 %>%
  filter(ticker=="COP.DE") %>%
  select(date)

result$date <- as.Date(date$date)

ggplot(data=result, aes(x=date))+
  geom_line(aes(y=actual), lwd=1)+
  geom_line(aes(y=counterfact), color="blue", lwd=1, linetype=2)+
  geom_line(aes(y=att), color="firebrick1", linetype=3)+
  geom_vline(xintercept = c(as.Date("2021-09-04")-15,as.Date("2021-09-04"), as.Date("2021-09-21")))+
  geom_hline(yintercept = 0)+
  theme_bw()

atts <- data.frame(att_ind_excess = out$att, att_return = NA)
atts$att_return <- out$att

##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
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

#ad the results to the data set
Stocks_wip <- left_join(Stocks_wip, coefs, by="ticker")

Stocks_wip <- Stocks_wip %>%
  mutate(ab_ret = return-alpha-beta*mkt_excess) %>%
  group_by(ticker) %>%
  mutate(CAR = cumsum(ab_ret)) %>%
  ungroup()

ggplot(data=Stocks_wip, aes(x=date, y=CAR, color=ticker))+
  geom_line()+
  theme_bw()

