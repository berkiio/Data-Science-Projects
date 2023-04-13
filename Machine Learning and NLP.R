setwd()

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

library(text2vec)
library(caret)
library(glmnet)
library(Matrix)
library(Rcpp)
library(R6)
library(data.table)
library(rsparse)
library(stringi)
library(stringr)
library(mlapi)
library(lgr)
library(digest)
library(purrr)
library(purrrlyr)
library(tidytext)
library(academictwitteR)
library(tokenizers)
library(ggpubr)
library(lmtest)
library(e1071)
library(tm)
library(tidymodels)
library(textrecipes)
library(keras)
library(tensorflow)
library(stopwords)
library(textdata)
library(reticulate)
# library(spacyr)
# spacy_install()
# spacy_initialize()
library(textfeatures)
# library(kerasR)

# devtools::install_github("quanteda/spacyr", build_vignettes = F)
# reticulate::use_condaenv("spacy_condaenv", required=T)
# install_tensorflow()

#####using zeebo? dataset
stock_twtr <- read.csv("tweets.csv", header=T, sep=",")
test <- stock_twtr %>%
  dplyr::select(text, created_at, user_id)
s <- sample(seq(1,nrow(test),1), size=1000000, replace=F)
test <- test[s,]

part1 <- substring(test$created_at,5,7)
part1 <- match(part1, month.abb)
part2 <- substring(test$created_at,9,10)
part3 <- substring(test$created_at,27,30)

dates <- paste(part3,part1,part2,sep="-")
dates <- as.data.frame(dates)
names(dates)[1] <- "date"

twtr <- cbind(test,dates)
twtr <- twtr[,-2]

################################################
#### Sentiment classification with R bing ######
################################################

classif <- get_sentiments(lexicon = "bing")

a <- tokenize_words(twtr$text,
                    lowercase = T,
                    stopwords = F,
                    strip_punct = F,
                    strip_numeric = F)
a[[4]]




# #####test using kaggle data
# twtr <- read.csv("training.1600000.processed.noemoticon.csv", header=F, sep=",")
# 
# colnames(twtr) <- c("Sent","id","date","query","tweeter","text")
# 
# part1 <- substring(twtr$date,5,7)
# part1 <- match(part1, month.abb)
# part2 <- substring(twtr$date,9,10)
# part3 <- substring(twtr$date,25,28)
# 
# for (i in 1:length(part1)){
#   dates[i] <- paste(part3[i],part1[i],part2[i],sep="-")
# }
# dates <- as.data.frame(dates)
# dates$dates <- as.Date(dates$dates)
# 
# twtr_dates <- cbind(twtr,dates)
# 
# twtr_dates <- twtr_dates[,-3]
# names(twtr_dates)[6] <- "date"
# 
# #find matching dates in both dataframes
# test <- full_join(twtr_dates, a_neg, by="date")
# test <- test %>%
#   drop_na()
# 
# test %>%
#   group_by(date) %>%
#   summarise(Sent = mean(Sent))



# loading packages
#################################################################
#################################################################
# install.packages("text2vec", type = "source") #################
#################################################################

### loading and preprocessing a training set of tweets
# function for converting some symbols
conv_fun <- function(x) {iconv(x, "latin1", "ASCII", "")}

##### loading classified tweets ######
# source: http://help.sentiment140.com/for-students/
# 0 - the polarity of the tweet (0 = negative, 4 = positive)
# 1 - the id of the tweet
# 2 - the date of the tweet
# 3 - the query. If there is no query, then this value is NO_QUERY.
# 4 - the user that tweeted
# 5 - the text of the tweet

# tweets_classified <- read_csv('training.1600000.processed.noemoticon.csv',
#                               col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
#   # converting some symbols
#   dmap_at('text', conv_fun) %>%
#   # replacing class values
#   mutate(sentiment = ifelse(sentiment == 0, 0, 1))

############################################################################
# Use combined dataset #####################################################
############################################################################

tweets_classified <- read.csv("Combined_data_set.csv", header = T, sep=",") %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))
# data splitting on train and test
# Preprocessing

# Textprocessing <- function(x){
#   gsub("http[[:alnum:]]*",'', x)
#   gsub('http//S+//s*', '', x) ## Remove URLs
#   gsub('//b+RT', '', x) ## Remove RT
#   gsub('#//S+', '', x) ## Remove Hashtags
#   gsub('@//S+', '', x) ## Remove Mentions
#   gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
#   gsub("//d", '', x) ## Remove Controls and special characters
#   gsub('[[:punct:]]', '', x) ## Remove Punctuations
#   gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
#   gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
#   gsub(' +',' ',x) ## Remove extra whitespaces
# }

for (i in 1:nrow(tweets_classified)){
  tweets_classified$text[i] <- gsub("//$//w+", "",tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("//#//w+", "",tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("[[:digit:]]","", tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("[[:punct:]]", " ",tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("/n*", "", tweets_classified$text[i])
  tweets_classified$text[i] <- gsub(" +"," ", tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("^[[:space:]]*","", tweets_classified$text[i])
}

set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

##### doc2vec #####
# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer
it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = tweets_train$id,
                   n_chunks = 10)
it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = tweets_test$id,
                  n_chunks = 10)

# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train, ngram = c(1,2))
vocab <- prune_vocabulary(vocab, term_count_min = 2, term_count_max = 150)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)
# define tf-idf model
tfidf <- TfIdf$new()
# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)

########################################################################################################################
# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf, y = tweets_train[['sentiment']],
                               family = 'binomial',
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 5-fold cross-validation
                               nfolds = 5,
                               # high value is less accurate, but has faster training
                               thresh = 1e-1,
                               # again lower number of iterations for faster training
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))
plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))
preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
glmnet:::auc(as.numeric(tweets_test$sentiment), preds)
# # save the model for future using
# saveRDS(glmnet_classifier, 'glmnet_classifier.RDS')

preds_check <- cbind(tweets_test, preds)
preds_check <- preds_check %>%
  mutate(preds = ifelse(preds<=0.5,0,1))

preds_check <- preds_check %>%
  mutate(TP = ifelse(sentiment == 1 & preds == 1, 1, 0),
         FP = ifelse(sentiment == 0 & preds == 1, 1, 0),
         FN = ifelse(sentiment == 1 & preds == 0, 1, 0),
         TN = ifelse(sentiment == 0 & preds == 0, 1, 0)) 
#Accuracy
Accuracy <- (sum(preds_check$TP)+ sum(preds_check$TN))/ (sum(preds_check$TP)+sum(preds_check$TN)+sum(preds_check$FP)+sum(preds_check$FN))
print(Accuracy)
# Precision
Precision <- (sum(preds_check$TP))/(sum(preds_check$TP)+sum(preds_check$FP))

# Recall
Recall <- (sum(preds_check$TP))/(sum(preds_check$TP)+sum(preds_check$FN))

########################################################################################################################

# install.packages("devtools", type="source")
# devtools::install_github("cjbarrie/academictwitteR", build_vignettes = TRUE) 

########################################################################################################################
#### Scrape twitter data
# set_bearer()
# get_bearer()
# 
# vwtweets <- get_all_tweets(
#   query = c("vw","volkswagen", "vwscandal", "vwgate"),
#   start_tweets = "2015-09-19T00:00:00Z",
#   end_tweets = "2015-09-26T00:00:00Z",
#   file = "tweetsdieselgateafter",
#   data_path = "",
#   lang = "en",
#   n = 150000,
#   remove_promoted = T
# )

#######################################################################################################################
# converting some symbols
twtr_2 <- twtr %>% 
  dmap_at('text', conv_fun)

# preprocessing and tokenization
it_tweets <- itoken(twtr_2$text,
                      preprocessor = prep_fun,
                      tokenizer = tok_fun,
                      ids = twtr_2$user_id,
                      progressbar = TRUE)


# creating vocabulary and document-term matrix
dtm_vwtweets <- create_dtm(it_tweets, vectorizer)
# transforming data with tf-idf
dtm_tweets_tfidf <- fit_transform(dtm_vwtweets, tfidf)
# loading classification model
glmnet_classifier <- readRDS("~/glmnet_classifier.RDS")
# predict probabilities of positiveness
preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]
# adding rates to initial dataset
twtr_2$sentiment <- preds_tweets


# # color palette
# cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")
# set.seed(932)
# samp_ind <- sample(c(1:nrow(vwtweetsab)), nrow(vwtweetsab) * 0.1) # 10% for labeling
# 
# #########################
# #convert created_at to time
# 
# vwtweetsab$date_round <- as.Date(vwtweetsab$created_at)
# #########################
# 
# # plotting
# ggplot(vwtweetsab, aes(x = created_at, y = sentiment, color = sentiment)) +
#   theme_minimal() +
#   scale_color_gradientn(colors = cols, limits = c(0, 1),
#                         breaks = seq(0, 1, by = 1/4),
#                         labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
#                         guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
#   geom_point(aes(color = sentiment), alpha = 0.8) +
#   geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
#   geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
#   geom_smooth(size = 1.2, alpha = 0.2) +
#   geom_label_repel(data = vwtweetsab[samp_ind, ],
#                    aes(label = round(sentiment, 2)),
#                    fontface = 'bold',
#                    size = 2.5,
#                    max.iter = 100) +
#   theme(legend.position = 'bottom',
#         legend.direction = "horizontal",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
#         axis.title.x = element_text(size = 16),
#         axis.title.y = element_text(size = 16),
#         axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
#         axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
#   ggtitle("Tweets Sentiment rate (probability of positiveness)")
# 

###

#assignt trinary sentiment score
twtr_2_fin <- twtr_2 %>%
  mutate(sent=case_when(
    sentiment>=0.65~1,
    sentiment<=0.35~-1,
    sentiment<0.65 & sentiment >0.35 ~ 0
  ))


test <- twtr_2_test %>%
  dplyr::select(date,sent)
prate <- table(test)
prate <- as.data.frame.matrix(prate)

pratenames <- c( "neg", "neu", "pos")
colnames(prate) <- pratenames
prate$rate <- prate$pos/prate$neg

# plot(rownames(prate), prate$rate)

prate <- tibble::rownames_to_column(prate, "Date")

prate$Date <- as.Date(prate$Date)

################################################################################################
#define start and end time
start = "2014-01-01"
end = "2020-01-01"

getSymbols("^DJI",src="yahoo", from = start, to = end, auto.assing = T)

DJI <- subset(DJI, select = -c(DJI.Open, DJI.High, DJI.Low, DJI.Adjusted))
DJI$Returns <- round(CalculateReturns(DJI$DJI.Close, method="simple"),4)
DJI <- as.data.frame(DJI)
DJI <- cbind(date = rownames(DJI), DJI)

# ggplot()+
#   geom_line(data=DJI, aes(x=date, y=DJI.Close, group=1))+
#   theme_bw()

ticker <- c("MMM","AXP","AMGN","AAPL","BA","CAT","CVX","CSCO","KO","DIS","DOW","GS","HD","HON","IBM","INTC",
            "JNJ","JPM","MCD","MRK","MSFT","NKE","PG","CRM","TRV","UNH","VZ","V","WBA","WMT")

#how many companies ordered alphabetically
# ticker_seq <- ticker[1:5]

#get stock data
getSymbols("CAT",src="yahoo", from = start, to = end, auto.assing = T)

#########################################
#from here change stock tickers manually#
#########################################
CVX <- subset(CVX, select = -c(CVX.Open, CVX.High, CVX.Low, CVX.Adjusted))
CVX$Returns <- round(CalculateReturns(CVX$CVX.Close, method="simple"),4)
CVX <- as.data.frame(CVX)
CVX <- cbind(date = rownames(CVX), CVX)

CVX$excess <- NA

for (i in 2:nrow(CVX)){
  CVX$excess[i] <- CVX$Returns[i]-DJI$Returns[i]
}

p1 <- DJI %>%
  dplyr::select(Returns)

CVX <- cbind(CVX,p1)
names(CVX)[6] <- "Returns_DJI"

#define threshold for cutoff and find all dates that match condition
t <- 0.04

CVX$date <- as.Date(CVX$date)
CVX <- CVX[-1,]

# exclude financial crisis
# BA <- BA %>%
#   filter(date > "2009-12-31" | date < "2007-12-31")


# #filter positive and negative events
# a_pos <- BA %>%
#   filter(excess > t)
# 
# a_neg <- BA %>%
#   filter(abs(excess) > t & sign(excess)==-1)
# 
# 
# #save the abnormal dates
# d_neg <- a_neg$date
# d_pos <- a_pos$date
# 
# ggplot()+
#   geom_point(data=a_pos, aes(x=date, y=excess), color="darkgreen")+
#   geom_point(data=a_neg, aes(x=date, y=excess), color="red")+
#   scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
#   theme_bw()
###############################################################################################
#sent change & scaling

prate$change <- NA

for (i in 2:nrow(prate)){
  prate$change[i] <- (prate$rate[i]-prate$rate[i-1])/prate$rate[i-1]
}

BA <- BA[-1,]
prate <- prate[-1,]

# sf <- max(prate$change)/max(BA$returns)
# BA <- BA %>%
#   mutate(ska = returns*sf)

a_neg_prate <- subset(prate, Date > d_neg-21 & Date < d_neg+21)

#Plotting
p1 <- ggplot()+
  geom_line(data = a_neg_prate, aes(x=Date, y=change, group = 1), color="red")+
  theme_bw()
p2 <- ggplot()+
  geom_line(data = BA, aes(x=Date, y=returns, group = 1), color="green")+
  theme_bw()

grid.arrange(p1,p2,nrow=2)

# 
# x <- merge(prate, AAPL, by="Date")
# x <- x[complete.cases(x), ]
# 
# cor(x$change, x$returns)

############################################
####Scrape twitter volume###################
############################################

#Token: ####

#IMPORTANT NOTE#
#change stock ticker and company name in the code below (e.g. AAPL -> BA and Apple -> Amgen)

`tweets_$CVX` <- count_all_tweets(
  query="$CVX",
  start_tweets = "2014-01-01T00:00:00Z",
  end_tweets = "2020-01-01T00:00:00Z",
  granularity = "day",
  n = 5000
)

`tweets_#Chevron` <- count_all_tweets(
  query="#Chevron",
  start_tweets = "2014-01-01T00:00:00Z",
  end_tweets = "2020-01-01T00:00:00Z",
  granularity = "day",
  n = 5000
)

fin <- `tweets_$CVX` %>%
  left_join(`tweets_#Chevron`, by=c("end","start"))

colnames(fin) <- c("end","start","$CVX","#Chevron")

##create dataframe to work with & export to excel for later use


write_xlsx(fin, "CVX_tweets.xlsx", col_names = T)

fin <- read_xlsx("CVX_tweets.xlsx")

wip_data <- fin

wip_data$end <- as.Date(wip_data$end)
wip_data$start <- as.Date(wip_data$start)

#collect events which show abnormal (stock) behavior
#first need to load stock data at the beginning of code
#threshold for trimming stock events
t = 0.03

CVX <- CVX %>%
  mutate(adj_ret = ifelse(excess<=t & excess>=-t,0,excess))

#threshold for trimming twitter volume (higher than x%)
c=.95
q1 <- quantile(wip_data$`#Chevron`, probs=c)[[1]]
q2 <- quantile(wip_data$`$CVX`, probs=c)[[1]]

wip_test <- wip_data %>%
  mutate(adj_hashtag = ifelse(`#Chevron`>= q1,`#Chevron`,0),
         adj_cashtag = ifelse(`$CVX`>=q2, `$CVX`,0))

# p1 <- ggplot(data=wip_test, aes(x=end))+
#   geom_line(aes(y=adj_hashtag/1000), color="darVrange")+
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
#   theme_classic()
# 
# p2 <- ggplot(data=wip_test, aes(x=end))+
#   geom_line(aes(y=adj_cashtag/1000), color="darVrange")+
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
#   theme_classic()
# 
# p3 <- ggplot(data=CVX, aes(x=date))+
#   geom_line(aes(y=adj_ret), color="dodgerblue")+
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
#   scale_y_continuous(labels = scales::percent)+
#   theme_classic()
# 
# grid.arrange(p1,p2,p3,nrow=3)


################################
### try to find optimal p-value#
################################
#number of max iterations if no significance is found
x = 14

#level of confidence
y = 0.05

#start iterations (optimize code later because its slow as hell)
#if signif at 14 code runs to infinity
ord = 1
repeat {
  test <- grangertest(CVX$excess,wip_data$`#Chevron`,order=ord)
  print(ord)
  ord=ord+1
  if (test$`Pr(>F)`[2] <= y){
    print(paste("Found significant order at:", ord,"."))
    print(test$`Pr(>F)`[2])
  } else if(ord==x){
    break
  }
}

#Here are the final dates that pass all exclusion criteria (dates of interest)
wip_data_prep <- wip_test %>%
  filter(adj_hashtag != 0 & adj_cashtag != 0)

CVX_final_dates <- CVX %>%
  mutate(tagged = ifelse(date %in% wip_data_prep$end,1,0)) %>%
  filter(tagged == 1 & adj_ret != 0)

##########################
### read earninV dates###
##########################
ea <- read.csv("earnings_latest.csv", header=T, sep=",")
ea <- ea %>%
  filter(symbol %in% ticker)
ea$date <- as.Date(ea$date)

CVX_ea <- ea %>%
  filter(symbol=="CVX")

p1 <- ggplot(data=wip_test, aes(x=end))+
  geom_line(aes(y=adj_hashtag), color="orange")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  # geom_vline(xintercept=V_ea$date, color="red", alpha=0.3)+
  theme_classic()

p2 <- ggplot(data=wip_test, aes(x=end))+
  geom_line(aes(y=adj_cashtag), color="orange")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  # geom_vline(xintercept=V_ea$date, color="red", alpha=0.3)+
  theme_classic()

p3 <- ggplot(data=CVX, aes(x=date))+
  geom_line(aes(y=adj_ret), color="dodgerblue")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  geom_vline(xintercept=CVX_ea$date, color="red", alpha=0.3)+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()

grid.arrange(p1,p2,p3,nrow=3)

########################################
# Get events CVXsed on excess return ####
########################################

CVX_analysis <- CVX %>%
  filter(adj_ret != 0)

#substract 1 day because of return calculation lag (only for event identifiCVXion)

CVX_analysis_twtr <- CVX_analysis %>%
  mutate(date = date-1)

seqvec <- c()

for (i in 1:nrow(CVX_ea)){
  seqvec <- append(seqvec, seq(CVX_ea$date[i]-7,CVX_ea$date[i]+7,1), after = length(seqvec))
}

CVX_analysis_twtr <- subset(CVX_analysis_twtr, !(date %in% seqvec))
#output for Twitter scraping

CVX_analysis_twtr <- CVX_analysis_twtr %>%
  mutate(start = date-14,
         end = date+14,
         tag1 = "$CVX",
         tag2 = "#Chevron",
         event = seq(1,nrow(CVX_analysis_twtr),1))

write_xlsx(CVX_analysis_twtr, "CVX_events_no_EA.xlsx", col_names = T)

period = 14
datavec <- c()

for (i in 1:nrow(CVX_analysis)){
  d <- CVX_analysis$date[i]
  datavec <- append(datavec, seq(d-period,d+period+1, 1), after = length(datavec))
}

frame <- as.data.frame(matrix(data=NA, nrow=length(datavec), ncol=1))
frame$V1 <- datavec

colnames(frame) <- "date"
frame <- frame %>%
  left_join(CVX, by="date")
  
frame <- frame %>%
  left_join(wip_data, by=c("date"="end"))

write_xlsx(frame, "CVX_events.xlsx", col_names = T)

# p1 <- ggplot(data=frame)+
#   geom_line(aes(x=date, y=Returns), color="dodgerblue4")+
#   theme_classic()
# 
# p2 <- ggplot(data=frame)+
#   geom_line(aes(x=date, y=`#Chevron`), color="red")+
#   theme_classic()
# 
# grid.arrange(p1,p2, nrow=2)

sum(frame$`$CVX`, na.rm = T)
sum(frame$`#Chevron`, na.rm = T) 












###########################################################################################################################
# Some other analysis #####################################################################################################
###########################################################################################################################
setwd("~/TwitterVolume")

apple <- count_all_tweets(
  query="apple",
  start_tweets = "2014-01-01T00:00:00Z",
  end_tweets = "2014-02-01T00:00:00Z",
  granularity = "day",
  n = 5000
)

Apple <- count_all_tweets(
  query="Apple",
  start_tweets = "2014-01-01T00:00:00Z",
  end_tweets = "2014-02-01T00:00:00Z",
  granularity = "day",
  n = 5000
)

write_xlsx(inflation, "inflation_volume.xlsx", col_names = T)
# write_xlsx(iphone, "iphone_volume.xlsx", col_names = T)
# write_xlsx(test, "apple_volume.xlsx", col_names = T)

sum(test$tweet_count)
sum(iphone$tweet_count)

ggplot()+
  geom_line(data=test, aes(x=end, y=tweet_count, group=1), color="red")+
  geom_line(data=iphone, aes(x=end, y=tweet_count, group=1), color="blue")

grangertest(test$tweet_count, iphone$tweet_count, order = 7)
cor.test(test$tweet_count, iphone$tweet_count)


###vector of tweets to scrape
start = "2010-01-01T00:00:00Z"
end = "2022-01-01T00:00:00Z"
granularity = "day"
nam <- c("NVDA","Nvidia","Geforce","AMD")

###generate empty list to assign scraped date
mylist <- list()

for (i in nam){
  mylist[[i]] <- count_all_tweets(
    query=i,
    start_tweets = start,
    end_tweets = end,
    granularity = granularity,
    n = 5000
  )
}

###get dataframes from list
#path to save excel files
setwd()
for (i in 1:length(mylist)){
  assign(nam[i], mylist[[i]])
  write_xlsx(mylist[[i]], paste(nam[i],"_volume.xlsx"), col_names = T)
}

ggplot()+
  geom_line(data=AMD, aes(x=end, y=tweet_count, group=1), color="red")+
  geom_line(data=NVDA, aes(x=end, y=tweet_count, group=1), color="blue")

###################################################################################
###################################################################################
###################################################################################
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

tweets_classified <- read_csv('training.1600000.processed.noemoticon.csv',
                              col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))


a <- tokenize_words(tweets_classified$text,
                    lowercase = T,
                    stopwords = F,
                    strip_punct = F,
                    strip_numeric = F)


filter_vector <- c("equity","stocks","bonds","finance")
lines <- c()

for (i in 1:length(a)){
  for (j in 1:length(filter_vector)){
    if(filter_vector[j] %in% a[[i]]){
    lines <- append(lines, i, after = length(lines))
    }
  }
}

b <- tweets_classified[lines,]
###################################################################################
###################################################################################
###################################################################################
# SVM attempt #
###############
conv_fun <- function(x) {iconv(x, "latin1", "ASCII", "")}
tweets_classified <- read.csv("Combined_data_set.csv", header = T, sep=",") %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))
# data splitting on train and test
# Preprocessing

# Textprocessing <- function(x){
#   gsub("http[[:alnum:]]*",'', x)
#   gsub('http//S+//s*', '', x) ## Remove URLs
#   gsub('//b+RT', '', x) ## Remove RT
#   gsub('#//S+', '', x) ## Remove Hashtags
#   gsub('@//S+', '', x) ## Remove Mentions
#   gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
#   gsub("//d", '', x) ## Remove Controls and special characters
#   gsub('[[:punct:]]', '', x) ## Remove Punctuations
#   gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
#   gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
#   gsub(' +',' ',x) ## Remove extra whitespaces
# }

#remove URLs
tweets_classified$text <- gsub("http[[:alnum:]]*", "",tweets_classified$text)

#remove Punctuation
tweets_classified$text <- gsub("[[:punct:]]", "",tweets_classified$text)

#remove digits
tweets_classified$text <- gsub("[[:digit:]]","", tweets_classified$text)


for (i in 1:nrow(tweets_classified)){
  tweets_classified$text[i] <- gsub("$//S+//w+", "",tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("#//S+//w+", "",tweets_classified$text[i])
  tweets_classified$text[i] <- gsub(" . ", "",tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("[[:digit:]]","", tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("[[:punct:]]", " ",tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("/n*", "", tweets_classified$text[i])
  tweets_classified$text[i] <- gsub(" +"," ", tweets_classified$text[i])
  tweets_classified$text[i] <- gsub("^[[:space:]]*","", tweets_classified$text[i])
}

set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]


##### doc2vec #####
# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer
it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = tweets_train$X,
                   n_chunks = 1)
it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = tweets_test$X,
                  n_chunks = 1)

# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train, ngram = c(1,2))
vocab <- prune_vocabulary(vocab, term_count_min = 2, term_count_max = 150)

vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)
# define tf-idf model
tfidf <- TfIdf$new()
# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

s <- sample(seq(1,nrow(dataset),1), size=0.8*nrow(dataset), replace=F)
training_set <- dataset[s,]
test_set <- dataset[-s,]

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])


classifier <- svm(Purchased~.,
                  data=training_set,
                  type="C-classification",
                  kernel="linear")

y_pred <- predict(classifier, newdata=test_set[-3])
cm <- table(test_set[,3], y_pred)
print(cm)

y_pred <- cbind(y_pred, test_set)


preds_check <- y_pred %>%
  mutate(TP = ifelse(y_pred == 1 & Purchased == 1, 1, 0),
         FP = ifelse(y_pred == 0 & Purchased == 1, 1, 0),
         FN = ifelse(y_pred == 1 & Purchased == 0, 1, 0),
         TN = ifelse(y_pred == 0 & Purchased == 0, 1, 0)) 
#Accuracy
Accuracy <- (sum(preds_check$TP)+ sum(preds_check$TN))/ (sum(preds_check$TP)+sum(preds_check$TN)+sum(preds_check$FP)+sum(preds_check$FN))
print(Accuracy)

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
# Dense Neural Network approach # https://smltar.com/dldnn.html #

conv_fun <- function(x) {iconv(x, "latin1", "ASCII", "")}

# Using own searched dataset ~3k observations
tweets_classified <- read.csv("Combined_data_set.csv", header = T, sep=",") %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))

tweets_classified$text <- gsub("\\$[[:alnum:]]+", "",tweets_classified$text)
#################################################

# # Using Standford 1.6mil labeled dataset
# tweets_classified <- read_csv('training.1600000.processed.noemoticon.csv',
#                               col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
#   # converting some symbols
#   dmap_at('text', conv_fun) %>%
#   # replacing class values
#   mutate(sentiment = ifelse(sentiment == 0, 0, 1)) %>%
#   dplyr::select(text,sentiment)
# 
# s <- sample(seq(1,nrow(tweets_classified),1), size=2000, replace=F)
# tweets_classified <- tweets_classified[s,]
#################################################

clean_tweets <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Remove cashtags
    str_remove_all("$[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Remove URLs
    str_remove_all("http\\w+") %>%
    # Remove all digits 
    str_remove_all("[[:digit:]_]+") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

tweets_classified$text <- tweets_classified$text %>% clean_tweets

tweets_classified$text <- gsub("  "," ",tweets_classified$text)
tweets_classified$text <- gsub(" . "," ",tweets_classified$text)

# https://smltar.com/dldnn.html
set.seed(1234)

kickstarter_split <- tweets_classified %>%
  filter(nchar(text) >= 20) %>%
  initial_split()

kickstarter_train <- training(kickstarter_split)
kickstarter_test <- testing(kickstarter_split)

# change parameters
max_words <- 2e6
max_length <- 30

# https://smltar.com/dldnn.html -> check padding and truncation
kick_rec <- recipe(~text, data=kickstarter_train) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = max_words) %>%
  step_sequence_onehot(text, sequence_length = max_length)

kick_prep <- prep(kick_rec)
#tc_train changed to tc_train_baked
kick_train <- bake(kick_prep, new_data = NULL, composition="matrix")

# Start training of dense neural network
dense_model <- keras_model_sequential() %>%
  #input dim = min(dim<max_words,max_words)
  layer_embedding(input_dim=max_words+1,
                  output_dim=12,
                  input_length = max_length) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

dense_model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy", 
  metrics = c("accuracy")
)

set.seed(234)
kick_val <- validation_split(kickstarter_train, strata=sentiment)

kick_analysis <- bake(kick_prep, new_data = analysis(kick_val$splits[[1]]),
                      composition = "matrix")
kick_assess <- bake(kick_prep, new_data = assessment(kick_val$splits[[1]]),
                    composition = "matrix")

state_analysis <- analysis(kick_val$splits[[1]]) %>% pull(sentiment)
state_assess <- assessment(kick_val$splits[[1]]) %>% pull(sentiment)

# val_history <- dense_model %>%
#   fit(
#     x = kick_analysis,
#     y = state_analysis,
#     batch_size = 512,
#     epochs = 10,
#     validation_data = list(kick_assess, state_assess),
#     verbose = FALSE
#   )

keras_predict <- function(model, baked_data, response) {
  predictions <- predict(model, baked_data)[, 1]
  tibble(
    .pred_1 = predictions,
    .pred_class = if_else(.pred_1 < 0.5, 0, 1),
    sentiment = response
  ) %>%
    mutate(across(c(sentiment, .pred_class),            ## create factors
                  ~ factor(.x, levels = c(1, 0))))  ## with matching levels
}

val_res <- keras_predict(dense_model, kick_assess, state_assess)
m <- metrics(val_res, sentiment, .pred_class)

# create dataframe to store accuracy data
acc_frame <- as.data.frame(matrix(data=NA, nrow=1, ncol=5))
colnames(acc_frame) <- c("standard DNN","BOW_DNN","word_embed_DNN","CV_DNN","CNN")
rownames(acc_frame) <- "accuracy"
acc_frame$`standard DNN`[1] <- m$.estimate[1]

val_res %>%
  conf_mat(sentiment, .pred_class) %>%
  autoplot(type="heatmap")

val_res %>%
  roc_curve(truth=sentiment, .pred_1) %>%
  autoplot()

##########################
# Including Bag of Words #
##########################
kick_bow_rec <- recipe(~text, data=kickstarter_train) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 1e3) %>%
  #can use step_tfidf() here
  step_tf(text)

kick_bow_prep <- prep(kick_bow_rec)

kick_bow_analysis <- bake(kick_bow_prep, 
                          new_data = analysis(kick_val$splits[[1]]),
                          composition = "matrix")

kick_bow_assess <- bake(kick_bow_prep, 
                        new_data = assessment(kick_val$splits[[1]]),
                        composition = "matrix")

bow_model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(1e3)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

bow_model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# bow_history <- bow_model %>%
#   fit(
#     x = kick_bow_analysis,
#     y = state_analysis,
#     batch_size = 512,
#     epochs = 10,
#     validation_data = list(kick_bow_assess, state_assess),
#     verbose = FALSE
#   )

bow_res <- keras_predict(bow_model, kick_bow_assess, state_assess)
metrics(bow_res, sentiment, .pred_class)

m <- metrics(bow_res, sentiment, .pred_class)
acc_frame$BOW_DNN[1] <- m$.estimate[1]
###############################
# pre-trained word embeddings #
###############################

# only load this once
# glove6b <- embedding_glove6b(dimensions = 50) %>% dplyr::select(1:13)

tidy(kick_prep, number = 3)

glove6b_matrix <- tidy(kick_prep, 3) %>%
  dplyr::select(token) %>%
  left_join(glove6b, by = "token",) %>%
  mutate(across(!token, replace_na, 0)) %>%
  dplyr::select(-token) %>%
  as.matrix() %>%
  rbind(0, .)

dense_model_pte <- keras_model_sequential() %>%
  layer_embedding(input_dim = nrow(glove6b_matrix),
                  output_dim = ncol(glove6b_matrix),
                  input_length = max_length) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

dense_model_pte %>%
  get_layer(index = 1) %>%
  set_weights(list(glove6b_matrix))

dense_model_pte %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# dense_pte_history <- dense_model_pte %>%
#   fit(
#     x = kick_analysis,
#     y = state_analysis,
#     batch_size = 512,
#     epochs = 20,
#     validation_data = list(kick_assess, state_assess),
#     verbose = FALSE
#   )

pte_res <- keras_predict(dense_model_pte, kick_assess, state_assess)
metrics(pte_res, sentiment, .pred_class)

m <- metrics(pte_res, sentiment, .pred_class)
acc_frame$word_embed_DNN[1] <- m$.estimate[1]
########################
# CV approach for DL ###
########################

#only use CV if dataset is very small
set.seed(345)
kick_folds <- vfold_cv(kickstarter_train, v = 5)
kick_folds

fit_split <- function(split, prepped_rec) {
  ## preprocessing
  x_train <- bake(prepped_rec, new_data = analysis(split),
                  composition = "matrix")
  x_val   <- bake(prepped_rec, new_data = assessment(split),
                  composition = "matrix")
  
  ## create model
  y_train <- analysis(split) %>% pull(sentiment)
  y_val   <- assessment(split) %>% pull(sentiment)
  
  mod <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_words + 1,
                    output_dim = 12,
                    input_length = max_length) %>%
    layer_flatten() %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid") %>% compile(
      optimizer = "adam",
      loss = "binary_crossentropy",
      metrics = c("accuracy")
    )
  
  ## fit model
  mod %>%
    fit(
      x_train,
      y_train,
      epochs = 10,
      validation_data = list(x_val, y_val),
      batch_size = 512,
      verbose = FALSE
    )
  
  ## evaluate model
  keras_predict(mod, x_val, y_val) %>%
    metrics(sentiment, .pred_class, .pred_1)
}

cv_fitted <- kick_folds %>%
  mutate(validation = map(splits, fit_split, kick_prep))

cv_fitted %>%
  unnest(validation)

m <- cv_fitted %>%
  unnest(validation) %>%
  group_by(.metric) %>%
  summarize(
    mean = mean(.estimate),
    n = n(),
    std_err = sd(.estimate) / sqrt(n)
  )

acc_frame$CV_DNN[1] <- m$mean[1]

########################################
# Build Convolutional Neural Network ###
########################################

simple_cnn_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1, output_dim = 16,
                  input_length = max_length) %>%
  layer_conv_1d(filter = 32, kernel_size = 10, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filter = 64, kernel_size = 5, activation = "relu") %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

simple_cnn_model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

val_res <- keras_predict(simple_cnn_model, kick_assess, state_assess)
metrics(val_res, sentiment, .pred_class, .pred_1)

v <- metrics(val_res, sentiment, .pred_class, .pred_1)
acc_frame$CNN[1] <- v$.estimate[1]

val_res %>%
  conf_mat(sentiment, .pred_class) %>%
  autoplot(type = "heatmap")

#############################################################################
#############################################################################
#############################################################
# The total CNN model #
#############################################################
#############################################################################
#############################################################################
conv_fun <- function(x) {iconv(x, "latin1", "ASCII", "")}

#write perdict function
keras_predict <- function(model, baked_data, response) {
  predictions <- predict(model, baked_data)[, 1]
  tibble(
    .pred_1 = predictions,
    .pred_class = if_else(.pred_1 < 0.5, 0, 1),
    sentiment = response
  ) %>%
    mutate(across(c(sentiment, .pred_class),            ## create factors
                  ~ factor(.x, levels = c(1, 0))))  ## with matching levels
}
#############################################################
# Clean tweets function #
#############################################################
clean_tweets <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Remove cashtags
    str_remove_all("$[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Remove URLs
    str_remove_all("http\\w+") %>%
    # Remove all digits 
    str_remove_all("[[:digit:]_]+") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}
#############################################################################
#############################################################################
# Using own searched dataset ~3k observations
# tweets_classified <- read.csv("Combined_data_set.csv", header = T, sep=",") %>%
#   # converting some symbols
#   dmap_at('text', conv_fun) %>%
#   # replacing class values
#   mutate(sentiment = ifelse(sentiment == 0, 0, 1))
# 
# tweets_classified$text <- gsub("\\$[[:alnum:]]+", "",tweets_classified$text)
#############################################################################
#############################################################################

size <- c(50000,100000,250000,500000)
output <- as.data.frame(matrix(data=NA, nrow=length(size),ncol=3))
colnames(output) <- c("size","time","accuracy")
output$size <- size

time_vec <- c()
accuracy_vec <- c()

for (i in size){
# Using Standford 1.6mil labeled dataset
tweets_classified <- read_csv('training.1600000.processed.noemoticon.csv',
                              col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1)) %>%
  dplyr::select(text,sentiment)

s <- sample(seq(1,nrow(tweets_classified),1), size=i, replace=F)
tweets_classified <- tweets_classified[s,]
#################################################

tweets_classified$text <- tweets_classified$text %>% clean_tweets
tweets_classified$text <- gsub("  "," ",tweets_classified$text)
tweets_classified$text <- gsub(" . "," ",tweets_classified$text)

# https://smltar.com/dldnn.html
set.seed(1234)

kickstarter_split <- tweets_classified %>%
  filter(nchar(text) >= 10) %>%
  initial_split()

kickstarter_train <- training(kickstarter_split)
kickstarter_test <- testing(kickstarter_split)

max_words <- 2e6
max_length <- 30

kick_rec <- recipe(~text,data = kickstarter_train) %>%
  step_tokenize(text) %>%
  # step_word_embeddings(text, embeddings = glove6b) %>%
  # # lemmatization with engine = "spacyr" in step_tokenize
  # step_lemma(text) %>%
  # step_stem(text) %>%
  # step_stopwords(text, stopword_source = "snowball") %>%
  # step_ngram(min_num_tokens = 1, num_tokens = 4) %>%
  step_tokenfilter(text, max_tokens = max_words) %>%
  # step_texthash() %>%
  # step_tfidf/tf is too big -> look for solution
  # step_tfidf(text) %>%
  step_sequence_onehot(text, sequence_length = max_length)

kick_prep <- prep(kick_rec)
kick_matrix <- bake(kick_prep, new_data = NULL, composition = "matrix")

dim(kick_matrix)

final_mod <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words, output_dim = 16,
                  input_length = max_length) %>%
  layer_conv_1d(filter = 32, kernel_size = 7,
                strides = 1, activation = "relu") %>%
  layer_conv_1d(filter = 64, kernel_size = 5, activation = "relu") %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

final_mod %>%
  compile(
    #Adamax (78%) binary_crossentropy
    #Adam (74%) binary_crossentropy
    #Nadam (76.5%) binary_crossentropy
    optimizer = "Adamax",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )

t_start <- Sys.time()
final_history <- final_mod %>%
  fit(
    kick_matrix,
    kickstarter_train$sentiment,
    epochs = 10,
    validation_split = 0.3,
    batch_size = 512,
    verbose = FALSE
  )
print(difftime(Sys.time(), t_start, units = 'mins'))
# time_vec <- append(time_vec, difftime(Sys.time(), t_start, units = 'mins'), after = length(time_vec))

plot(final_history)
final_history$metrics[4]

kick_matrix_test <- bake(kick_prep, new_data = kickstarter_test,
                         composition = "matrix")
final_res <- keras_predict(final_mod, kick_matrix_test, kickstarter_test$sentiment)
final_res %>% metrics(sentiment, .pred_class, .pred_1)

x <- final_res %>% metrics(sentiment, .pred_class, .pred_1)
accuracy_vec <- append(accuracy_vec, x$.estimate[1], after = length(accuracy_vec))

# final_res %>%
#   roc_curve(sentiment, .pred_1) %>%
#   autoplot()
# final_res %>%
#   conf_mat(sentiment, .pred_class) %>%
#   autoplot(type = "heatmap")
}

output$time <- time_vec
output$accuracy <- accuracy_vec

out <- rbind(output, output2)

# write_xlsx(out, "performance_cnn.xlsx", col_names = T)

##################################################################
# Predict cashtag dataset #
##################################################################

tweets_classify <- read.csv("Combined_data_set.csv", header = T, sep=",") %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))

tweets_classify$text <- gsub("\\$[[:alnum:]]+", "",tweets_classify$text)
tweets_classify$text <- tweets_classify$text %>% clean_tweets
tweets_classify$text <- gsub(" . "," ",tweets_classify$text)
tweets_classify$text <- gsub("  "," ",tweets_classify$text)
tweets_classify <- tweets_classify %>%
  dplyr::select(text,sentiment)


# kickstarter_split <- tweets_classified %>%
#   filter(nchar(text) >= 30) %>%
#   initial_split()
# 
# kickstarter_train <- training(kickstarter_split)

# kick_rec <- recipe(~text, data = kickstarter_train) %>%
#   step_tokenize(text) %>%
#   step_tokenfilter(text, max_tokens = max_words) %>%
#   step_sequence_onehot(text, sequence_length = max_length)
# kick_prep <- prep(kick_rec)

# Some parameters #

# train the model with best number of epochs based on validation data
# 7-10 have done well so far with 30% split
# 75% out of sample accuracy
# 2/3 dense layers
# 3 convolutional layers



# test <- tweets_classify %>%
#   step_tokenize(text) %>%
#   step_stem(text) %>%
#   step_untokenize(text)

kick_matrix_test <- bake(kick_prep, new_data = tweets_classify,
                         composition = "matrix")
fin <- keras_predict(final_mod, kick_matrix_test, tweets_classify$sentiment)
fin %>% metrics(sentiment, .pred_class, .pred_1)
fin %>%
  conf_mat(sentiment, .pred_class) %>%
  autoplot(type = "heatmap")
############################################################################################
#################################### Model summary #########################################
############################################################################################

# Verify above output manually -> it is the same so can use above to measure accuracy
# tweets_classify$predicted <- fin$.pred_class
# 
# preds_check <- tweets_classify %>%
#   mutate(TP = ifelse(predicted == 1 & sentiment == 1, 1, 0),
#          FP = ifelse(predicted == 1 & sentiment == 0, 1, 0),
#          FN = ifelse(predicted == 0 & sentiment == 1, 1, 0),
#          TN = ifelse(predicted == 0 & sentiment == 0, 1, 0)) 
# #Accuracy
# Accuracy <- (sum(preds_check$TP)+ sum(preds_check$TN))/ (sum(preds_check$TP)+sum(preds_check$TN)+sum(preds_check$FP)+sum(preds_check$FN))
# print(Accuracy)

tweets_classify2 <- tweets_classify %>%
  dplyr::select(text)

kick_matrix_test <- bake(kick_prep, new_data = tweets_classify2,
                         composition = "matrix")
fin <- keras_predict(final_mod, kick_matrix_test, tweets_classify2)

tweets_classify$pred_class <- fin$.pred_class
tweets_classify$pred_class <- as.numeric(tweets_classify$pred_class)
tweets_classify <- tweets_classify %>%
  mutate(pred_class=ifelse(pred_class==2,0,1))

preds_check <- tweets_classify %>%
  mutate(TP = ifelse(pred_class == 1 & sentiment == 1, 1, 0),
         FP = ifelse(pred_class == 1 & sentiment == 0, 1, 0),
         FN = ifelse(pred_class == 0 & sentiment == 1, 1, 0),
         TN = ifelse(pred_class == 0 & sentiment == 0, 1, 0))
#Accuracy
Accuracy <- (sum(preds_check$TP)+ sum(preds_check$TN))/ (sum(preds_check$TP)+sum(preds_check$TN)+sum(preds_check$FP)+sum(preds_check$FN))
print(Accuracy)

###############################################################################
# Test CNN model with totally new stock data #
###############################################################################

test <- read.csv("./Neuer Ordner/stockerbot-export.csv", header=T, sep=",")
test <- test %>%
  dplyr::select(text)

test$text <- gsub("\\$[[:alnum:]]+", "",test$text)
test$text <- test$text %>% clean_tweets
test$text <- gsub(" . "," ",test$text)
test$text <- gsub("  "," ",test$text)

kick_matrix_test <- bake(kick_prep, new_data = test,
                         composition = "matrix")

fin <- keras_predict(final_mod, kick_matrix_test, test)

test <- cbind(test,fin$.pred_class)
test <- cbind(test,fin$.pred_1)


#####################################################
#############
#####################################################

t_seq= c("AIR.DE", "ZAL.DE", "SHL.DE", "SY1.DE", "SRT.DE", "PAH3.DE", "BNR.DE", "QIA.DE")
start <- "2019-01-01"
end <- "2022-12-31"
  
c_seq = c("AT1.DE","AFX.DE","BOSS.DE","NDA.DE","BC8.DE","CBK.DE",
            "DUE.DE","EVK.DE","EVT.DE","FRA.DE","FNTN.DE","FPE.DE","G1A.DE","GXI.DE","GYC.DE",
            "SDF.DE","KGX.DE","LXS.DE","LEG.DE",
            "NEM.DE","PSM.DE","RHM.DE","G24.DE",
            "SOW.DE","SAX.DE","TEG.DE","O2D.DE","UN01.DE","UTDI.DE")

index <- c("^GDAXI","^MDAXI")
Indices <- lapply(index, function(sym) {
    getSymbols(sym, from=start, to=end, auto.assign=FALSE)
})


Stocks_control_did <- Stocks_prices_dax_c[[1]][,1]
Stocks_treatment_did <- Stocks_prices_dax_t[[1]][,1]

Index_group <- Indices[[1]][,1]
Index_group$MDAXI.Open <- Indices[[2]][,1]

for (i in 2:length(c_seq)) {
  Stocks_control_did <- cbind(Stocks_control_did,Stocks_prices_dax_c[[i]][,1])
}

for (i in 2:length(t_seq)) {
  Stocks_treatment_did <- cbind(Stocks_treatment_did,Stocks_prices_dax_t[[i]][,1])
}


Stocks_control_did$avg <- rowMeans(Stocks_control_did)
Stocks_treatment_did$avg <- rowMeans(Stocks_treatment_did)

Index_group <- as.data.frame(Index_group)
Index_group <- cbind(date = rownames(Index_group), Index_group)

Treat <- as.data.frame(Stocks_treatment_did)
Treat <- cbind(date = rownames(Treat), Treat)

Control <- as.data.frame(Stocks_control_did)
Control <- cbind(date = rownames(Control), Control)

Index_group <- cbind(Index_group, Treat$avg)
Index_group <- cbind(Index_group, Control$av)

Index_group$date <- as.Date(Index_group$date)

ggplot(data=Index_group)+
  # geom_line(aes(x=date, y=log(GDAXI.Open), group=1), color="red")+
  # geom_line(aes(x=date, y=log(MDAXI.Open), group=1), color="green")+
  geom_line(aes(x=date, y=(`Treat$avg`), group=1), color="blue")+
  geom_line(aes(x=date, y=(`Control$av`), group=1))+
  theme_bw()+
  geom_vline(xintercept = as.Date("2021-09-04"))+
  geom_vline(xintercept = as.Date("2021-09-21"))+
  geom_vline(xintercept = as.Date("2020-11-24"))

#####################################################################
############## DiD Test #############################################
#####################################################################
setwd("C:/Users/david/Desktop/Jonas Masterarbeit SNF")

library(tidyverse)
library(plm)
library(dynlm)
library(readxl)

Stock_data <- read.csv("Stock_data8.csv", header = T, sep=",")

#create variable if before or after announcement and interaction term 
Stock_data <- Stock_data %>%
  mutate(after1= (ifelse (date >= as.Date("2021-09-04"),1,0))) %>%
  mutate(policy1 = treatment * after1,
         date = as.Date(date))

#create variable if before or after invention and interaction term 
Stock_data <- Stock_data %>%
  mutate(after2= (ifelse (date >= as.Date("2021-09-20"),1,0))) %>%
  mutate(policy2 = treatment * after2)

# ggplot(data=Stock_data, aes(x=log(close), color=ticker))+
#   geom_histogram()

#remove some rows and recalculate them#
Stock_data <- Stock_data %>%
  dplyr::select(-c(16:29)) %>%
  na.omit()

Stock_data <- Stock_data %>%
  group_by(ticker) %>%
  mutate(beta = cov(return,mdax_return)/var(mdax_return),
         abnormal_return = beta * return,
         cum_ab_ret = cumprod(1+abnormal_return),
         ma_3=forecast::ma(abnormal_return, order = 3)) %>%
  ungroup()

#######################################
after = 14
before = 21

Stock_data <- subset(Stock_data, date <= (as.Date("2021-09-20")+after) & date > (as.Date("2021-09-04")-before))

pdata <- pdata.frame(x=Stock_data, index=c("ticker","date"))

#model parameters: effect=individual, model=within -> policy1 insignificant, policy2 1%
#return = abnormalreturn_m or abnormalreturn_q
#model params: " regress on simple returns, p1 10%, p2 insignificant
m1 <- plm(log(1+abnormalreturn_q) ~ policy1 + policy2, data=pdata, effect="twoways", model="within")
summary(m1)

lm1 <- lm(abnormalreturn_q ~ policy1 + policy2, data=Stock_data)
summary(lm1)
#create loop for models with a new beta every day
#create interval of interest from point of interest (2021-09-04 & 09-20)
#add and remove in number of days

# Stock_data_cut <- subset(Stock_data10, date <= (as.Date("2021-09-20")+add) & date > (as.Date("2021-09-04")-rem))
# pdata_cut <- pdata.frame(x=Stock_data_cut, index = c("ticker","date"))
# m1 <- plm(return ~ policy1 + policy2, data=pdata_cut, effect="individual", model="within")
# summary(m1)[1]

ret = 1000
days_to_add = 10
results <- c()

#care for the weekends
for (i in 1:days_to_add){
  Stock_data_cut <- subset(Stock_data10, date <= (as.Date("2021-09-20")+i) & date > (as.Date("2021-09-04")-ret))
  pdata_cut <- pdata.frame(x=Stock_data_cut, index = c("ticker","date"))
  m1 <- plm(abnormalreturn_m ~ policy1 + policy2, data=pdata_cut, effect="individual", model="within")
  results <- append(results, summary(m1)[1], after = length(results))
}

#### manual DiD ####
dat <- Stock_data %>%
  dplyr::select(ticker, date, policy1, policy2, treatment, return, sum_abnormal_q, after1, after2)

#cut the period of interest
#number of days to lay off before and after treatment
before = 30
after = 15

data_cut <- subset(dat, date <= (as.Date("2021-09-20")+after) & date > (as.Date("2021-09-04")-before))

#calculate the pre treatment mean of both groups
pre_treat_treat <- dat %>%
  filter(after1 == 0 & treatment == 1) %>%
  mutate(avg_ret = mean(sum_abnormal_q))

pre_treat_control <- dat %>%
  filter(after1 == 0 & treatment == 0) %>%
  mutate(avg_ret = mean(sum_abnormal_q))

pre_tt <- mean(pre_treat_treat$sum_abnormal_q, na.rm=T)
pre_tc <- mean(pre_treat_control$sum_abnormal_q, na.rm=T)

#calculate the post treatment means of both groups

post_treat_treat <- dat %>%
  filter(after1 == 1 & treatment == 1) %>%
  mutate(avg_ret = mean(sum_abnormal_q))

post_treat_control <- dat %>%
  filter(after1 == 1 & treatment == 0) %>%
  mutate(avg_ret = mean(sum_abnormal_q))

post_tt <- mean(post_treat_treat$sum_abnormal_q, na.rm=T)
post_tc <- mean(post_treat_control$sum_abnormal_q, na.rm=T)

#DiD estimate for whole period

#post_T-pre_T
x1 <- post_tt-pre_tt
x2 <- post_tc-pre_tc

print(paste("The sum indexing effect is estimated to be: ",round((x2-x1)*100,4),"%, for the full period."))


generate_betas <- function(before,after){

#calculate for a daily basis for the post_treatment period

#take pre-treatment means from above (watch the period!!)

post_tt <- c()
post_tc <- c()

for (i in 1:after){
  dat_cut <- subset(dat, date <= (as.Date("2021-09-04")+i) & date > (as.Date("2021-09-04")))
  post_treat_treat <- subset(dat_cut, after1 == 1 & treatment == 1)
  post_treat_control <- subset(dat_cut, after1 == 1 & treatment == 0) 
  post_tt <- append(post_tt, mean(post_treat_treat$return, na.rm=T), after = length(post_tt))
  post_tc <- append(post_tc, mean(post_treat_control$return, na.rm=T), after = length(post_tc))
}

dat_cut <- subset(dat, date <= as.Date("2021-09-04") & date > (as.Date("2021-09-04")-before))

pre_treat_treat <- dat_cut %>%
  filter(after1 == 0 & treatment == 1)

pre_treat_control <- dat_cut %>%
  filter(after1 == 0 & treatment == 0)

pre_tt <- mean(pre_treat_treat$return, na.rm=T)
pre_tc <- mean(pre_treat_control$return, na.rm=T)

x1 <- post_tt-pre_tt
x2 <- post_tc-pre_tc

eff <- x2-x1

out <- data.frame(effects = 100*eff, date = seq(as.Date("2021-09-05"),as.Date("2021-09-04")+after,1))

ggplot(data=out, aes(x=date, y=effects))+
  geom_point(group=1, lwd=2)+
  theme_bw()+
  ylab(expression(paste("DiD estimator ", beta)))+
  xlab("Date")+
  ggtitle(expression(paste("DiD estimator ", beta)))
}

generate_betas(50,50)

#### Plot individual stocks ####
t <- Stock_data %>%
  filter(treatment==1, date >= "2021-01-01", date <= "2021-31-12") %>%
  mutate(date=as.Date(date))

ggplot(data=t, aes(x=date, y=abnormalreturn_q))+
  geom_line(aes(group=ticker, color=ticker))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m")+
  theme_bw()


############################
#### Apply SDID package ####
############################

#we need a balanced panel, hence kick all data not matching number of rows

library(synthdid)

Stock_data_sdid <- Stock_data %>%
  select(ticker, abnormal_return, date, policy2) %>%
  filter(date <= as.Date("2021-09-30") & date > as.Date("2021-08-01")) %>%
  na.omit() %>% 
  mutate(cum_ab_ret = cumprod(1+abnormal_return))

Stock_data_sdid$ticker <- as.factor(Stock_data_sdid$ticker)
Stock_data_sdid$policy1 <- as.integer(Stock_data_sdid$policy2)
Stock_data_sdid <- as.data.frame(Stock_data_sdid)

setup <- panel.matrices(Stock_data_sdid, unit = "ticker", time = "date", outcome = "cum_ab_ret", treatment = "policy2")
tau.hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)

se <- sqrt(vcov(tau.hat, method="placebo"))
sprintf("point estimate: %1.2f", tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

plot(tau.hat)

tau.hat
########################################
#### Synthetic Control Method ##########
########################################

#https://bookdown.org/mike/data_analysis/synthetic-control.html

library(Synth)
library(gsynth)
library(reshape2)
library(forecast)

set.seed(1)
year         <- rep(1:30, 10)
state        <- rep(LETTERS[1:10], each = 30)
X1           <- round(rnorm(300, mean = 2, sd = 1), 2)
X2           <- round(rbinom(300, 1, 0.5) + rnorm(300), 2)
Y            <- round(1 + 2 * X1 + rnorm(300), 2)
df           <- as.data.frame(cbind(Y, X1, X2, state, year))
df$Y         <- as.numeric(as.character(df$Y))
df$X1        <- as.numeric(as.character(df$X1))
df$X2        <- as.numeric(as.character(df$X2))
df$year      <- as.numeric(as.character(df$year))
df$state.num <- rep(1:10, each = 30)
df$state     <- as.character(df$state)
df$`T`       <- ifelse(df$state == "A" & df$year >= 15, 1, 0)
df$Y         <- ifelse(df$state == "A" & df$year >= 15, df$Y + 20, df$Y)

# dataprep.out <- Synth::dataprep(
#   df,
#   predictors = c("X1", "X2"),
#   dependent     = "Y",
#   unit.variable = "state.num",
#   time.variable = "year",
#   unit.names.variable = "state",
#   treatment.identifier  = 1,
#   controls.identifier   = c(2:10),
#   time.predictors.prior = c(1:14),
#   time.optimize.ssr     = c(1:14),
#   time.plot             = c(1:30)
# )

#unit variables needs to be numeric -> convert ticker to numeric indicator
Stock_data_sc <- Stock_data %>%
  select(ticker, abnormal_return, date, treatment, after1, after2, ma_3, cum_ab_ret) %>%
  filter(date <= as.Date("2021-09-30") & date > as.Date("2021-06-01")) %>%
  na.omit() 

Stock_data_sc_test <- Stock_data_sc %>%
  group_by(ticker) %>%
  mutate(indicator=as.integer(cur_group_id()),
         treatment=as.numeric(treatment)) %>%
  ungroup()

Stock_data_sc_test <- as.data.frame(Stock_data_sc_test)

#make sure data is ONLY a dataframe -> use class() to check
#code only works with a single treatet unit

stocks.out <- Synth::dataprep(
  Stock_data_sc_test,
  predictors = c("treatment","after1"),
  dependent = "cumulative_abnormal_q",
  unit.variable = "indicator",
  time.variable = "date",
  treatment.identifier = 1,
  controls.identifier = c(2:10),
  time.predictors.prior = c(1:14),
  time.optimize.ssr = c(1:14),
  time.plot = c(1:30)
)

synth.stocks.out <- Synth::synth(stocks.out)

print(synth.tables <- Synth::synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
)

Synth::path.plot(synth.res    = synth.out,
                 dataprep.res = dataprep.out,
                 Ylab         = c("Y"),
                 Xlab         = c("Year"),
                 Legend       = c("State A","Synthetic State A"),
                 Legend.position = c("topleft")
)
abline(v   = 15,
       lty = 2)
###################################################
#multiple treated units
Stock_data_sc <- Stock_data %>%
  select(ticker, abnormal_return, date, treatment, after1, after2, volume, policy1, policy2, ma_3, cum_ab_ret) %>%
  filter(date <= as.Date("2021-10-30") & date > as.Date("2021-07-01")) %>%
  na.omit() 

Stock_data_sc_test <- Stock_data_sc %>%
  group_by(ticker) %>%
  mutate(indicator=as.integer(cur_group_id()),
         treatment=as.numeric(treatment),
         ma_3=ma_3*100) %>%
  ungroup()

Stock_data_sc_test <- as.data.frame(Stock_data_sc_test)

str(Stock_data_sc_test)

#does not work with time invariant parameters -> we need characteristics that actually explain the return varieties
gsynth.out <- gsynth::gsynth(
  cum_ab_ret ~ `policy1`,
  data = Stock_data_sc_test,
  index = c("ticker", "date"),
  force = "none",
  CV = TRUE,
  r = c(0, 10),
  se = TRUE,
  inference = "parametric",
  nboots = 1000,
  parallel = F,
  normalize = T
)

plot(gsynth.out)
plot(gsynth.out, type = "counterfactual", theme.bw = T)+
  theme(plot.title = element_text(size=12))
plot(gsynth.out, type = "counterfactual", raw = "all")

out.dat <- data.frame(ytr=gsynth.out$Y.bar, time=gsynth.out$time, att=gsynth.out$att)

ggplot(data=out.dat, aes(x=time))+
  geom_line(aes(y=ytr.Y.tr.bar), color="black", lwd=1)+
  geom_line(aes(y=ytr.Y.ct.bar), color="dodgerblue", lwd=1, linetype=2)+
  geom_line(aes(y=ytr.Y.co.bar), color="green", lwd=1, linetype=2)+
  scale_x_date(date_breaks = "1 week", date_labels = "%W")+
  geom_vline(xintercept = as.Date(c("2021-09-04","2021-09-20")))+
  # geom_hline(yintercept = 0)+
  theme_bw()

ggplot(data=out.dat, aes(x=time))+
  geom_line(aes(y=att), color="darkgreen")+
  scale_x_date(date_breaks = "1 week", date_labels = "%W")+
  geom_vline(xintercept = as.Date(c("2021-09-04","2021-09-20")))+
  # geom_hline(yintercept = 0)+
  theme_bw()

####################################
#### Expand SC to SDiD manually ####
####################################

#Create policy variables
m <- Stock_data %>%
  select(after1, after2, date)

out.dat <- out.dat %>% left_join(m, by=c("time"="date"))
out.dat <- out.dat[!duplicated(out.dat),]
out.dat <- out.dat %>%
  mutate(DiD_beta = ytr.Y.ct.bar-ytr.Y.tr.bar)

ggplot(data=out.dat, aes(x=time))+
  geom_point(aes(y=DiD_beta))+
  theme_bw()

