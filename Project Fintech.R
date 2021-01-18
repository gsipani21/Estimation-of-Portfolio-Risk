library(data.table)
library(ggplot2)
library(scales)
library(rlang)
library(sandwich)
library(broom)
library(quantmod)
library(twitteR)
library(dplyr)
library(plyr)
library(lubridate)
library(NLP)
library(tm)
library(gsubfn)
library(stringr)
library(reshape2)
library(wordcloud)
library(syuzhet)
library(Rdpack)
library(PerformanceAnalytics)
library(readxl)
library(TTR)
library(plm)
library(DataCombine)

#Accessing the Twitter API
consumer_key<-"Pk1snLImUmIfuLgYecCTUGOQo"
consumer_secret<-"0qj9xSpzvuJaTKNe48nEQEboLO9xXHANQX22qMQAzujhjdmBfw"
access_token<- "709253951187587072-5oRbsWBHWgZZv2kWhnlcvWeTKvRdOET"
access_secret<- "CLKJlKfAzO22BWwG6NLYu4OV6OfBkZITB7onb0KqCWjwv"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Extracting recent tweets from the Twitter
GLDtweets1<- searchTwitter("$gold",since = '2019-12-01',until='2019-12-07',n=5000)
head(GLDtweets1)
PEPtweets<- searchTwitter("$pep",since ='2019-11-0',until='2019-12-31',n=5000)
head(PEPtweets)
WMTtweets<- searchTwitter("$wmt",n=5000)
head(WMTtweets)
AMDtweets<- searchTwitter("$amd",n=5000)
head(AMDtweets)
NVDAtweets<- searchTwitter("$nvda",n=5000)
head(NVDAtweets)
MSFTtweets<- searchTwitter("$msft",n=5000)
head(MSFTtweets)
getCurRateLimitInfo()


#Gold twitter data to Dataframe
GLDtweets2<- ldply(GLDtweets,function(t) t$toDataFrame())
GLD_table<- twListToDF(GLDtweets)
gldtweets<-GLD_table

#Pepsi Co twitter data to Dataframe
PEPtweets2<- ldply(PEPtweets,function(t) t$toDataFrame())
PEP_table<- twListToDF(PEPtweets)
peptweets<-PEP_table

#Walmart twitter data to Dataframe
WMTtweets2<- ldply(WMTtweets,function(t) t$toDataFrame())
WMT_table<- twListToDF(WMTtweets)
wmttweets<-WMT_table

#AMD twitter data to Dataframe
AMDtweets2<- ldply(AMDtweets,function(t) t$toDataFrame())
AMD_table<- twListToDF(AMDtweets)
amdtweets<-AMD_table

#NVDA twitter data to Dataframe
NVDAtweets2<- ldply(NVDAtweets,function(t) t$toDataFrame())
NVDA_table<- twListToDF(NVDAtweets)
tweets<-NVDA_table

#Microsoft twitter data to Dataframe
MSFTtweets2<- ldply(MSFTtweets,function(t) t$toDataFrame())
MSFT_table<- twListToDF(MSFTtweets)
msfttweets<-MSFT_table

#Creating Excel files of the dataframe
write.csv(gldtweets,file='gldtweets.csv',append = TRUE)
write.csv(peptweets,file='peptweets.csv',append = TRUE)
write.csv(wmttweets,file='wmttweets.csv',append = TRUE)
write.csv(amdtweets,file='amdtweets.csv',append = TRUE)
write.csv(nvdatweets,file='nvdatweets.csv',append = TRUE)
write.csv(msfttweets,file='msfttweets.csv',append = TRUE)

#Gold Stock Analysis
 
#reading the csv files
gldtweets<- read.csv('gldtweets.csv',stringsAsFactors = FALSE)

#count of number of tweets
gldtweets$timestamp <- mdy_hm(gldtweets$created)

ggplot(data = gldtweets, aes(x = timestamp))+
  geom_histogram(aes(fill = ..count..),bins=20)+
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#Sentimental Analysis
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
# Reading Poitive and Negative words
pos <- scan('positive-words.txt', what='character', comment.char=';')
neg <- scan('negative-words.txt', what='character', comment.char=';')

pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'hack', 'breach', 'cybersecurity')

GldDataset <- gldtweets
GldDataset$text <- gldtweets[,2]
scores <- score.sentiment(GldDataset$text, pos.words, neg.words)
scores$score

gldtweets<- read.csv('gldtweets.csv',stringsAsFactors = FALSE)
mySentiment <- get_nrc_sentiment(gldtweets$text)
head(mySentiment)
gldtweets <- cbind(gldtweets, mySentiment)
sentimentTotals <- data.frame(colSums(gldtweets[,c(19:28)])) 
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")


#Score and Tweets per day
temp<- gldtweets
temp$timestamp <- mdy_hm(temp$created)
temp$date<- as.Date(temp$timestamp)
temp<- cbind(temp,scores$score)

names(temp)[30]<-"score"

ggplot(data = temp, aes(x = date))+
  geom_histogram(aes(fill = ..count..),binwidth = 1)+
  stat_summary(aes(y=score),fun.y=sum,geom='line')+
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


#Pepsi Stock Analysis

#reading the csv files
peptweets<- read.csv('peptweets.csv',stringsAsFactors = FALSE)

#count of number of tweets
peptweets$timestamp <- mdy_hm(peptweets$created)

ggplot(data = peptweets, aes(x = timestamp))+
  geom_histogram(aes(fill = ..count..),bins=20)+
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#Sentimental Analysis

PepDataset <- peptweets
PepDataset$text <- peptweets[,2]
Pepscores <- score.sentiment(PepDataset$text, pos.words, neg.words)
Pepscores$score

peptweets<- read.csv('peptweets.csv',stringsAsFactors = FALSE)
PepsiSentiment <- get_nrc_sentiment(peptweets$text)
head(PepsiSentiment)
peptweets <- cbind(peptweets, PepsiSentiment)
PepsentimentTotals <- data.frame(colSums(peptweets[,c(19:28)])) 
names(PepsentimentTotals) <- "count"
PepsentimentTotals <- cbind("sentiment" = rownames(PepsentimentTotals), PepsentimentTotals)
rownames(PepsentimentTotals) <- NULL
ggplot(data = PepsentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")


#Score and Tweets per day
temp2<- peptweets
temp2$timestamp <- mdy_hm(temp2$created)
temp2$date<- as.Date(temp2$timestamp)
temp2<- cbind(temp2,Pepscores$score)

names(temp2)[30]<-"score"

ggplot(data = temp2, aes(x = date))+
  geom_histogram(aes(fill = ..count..),binwidth = 1)+
  stat_summary(aes(y=score),fun.y=sum,geom='line')+
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#AMD Stock Analysis

#reading the csv files
amdtweets<- read.csv('amdtweets.csv',stringsAsFactors = FALSE)

#count of number of tweets
amdtweets$timestamp <- mdy_hm(amdtweets$created)

ggplot(data = amdtweets, aes(x = timestamp))+
  geom_histogram(aes(fill = ..count..),bins=20)+
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#Sentimental Analysis

AMDDataset <- amdtweets
AMDDataset$text <- amdtweets[,2]
AMDscores <- score.sentiment(AMDDataset$text, pos.words, neg.words)
AMDscores$score

amdtweets<- read.csv('amdtweets.csv',stringsAsFactors = FALSE)
AMDSentiment <- get_nrc_sentiment(amdtweets$text)
head(AMDSentiment)
amdtweets <- cbind(amdtweets, AMDSentiment)
AMDsentimentTotals <- data.frame(colSums(amdtweets[,c(19:28)])) 
names(AMDsentimentTotals) <- "count"
AMDsentimentTotals <- cbind("sentiment" = rownames(AMDsentimentTotals), AMDsentimentTotals)
rownames(AMDsentimentTotals) <- NULL
ggplot(data = AMDsentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")


#Score and Tweets per day
temp3<- amdtweets
temp3$timestamp <- mdy_hm(temp3$created)
temp3$date<- as.Date(temp3$timestamp)
temp3<- cbind(temp3,AMDscores$score)

names(temp3)[30]<-"score"

ggplot(data = temp3, aes(x = date))+
  geom_histogram(aes(fill = ..count..),binwidth = 1)+
  stat_summary(aes(y=score),fun.y=sum,geom='line')+
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#Walmart Stock Analysis

#reading the csv files
wmttweets<- read.csv('wmttweets.csv',stringsAsFactors = FALSE)

#count of number of tweets
wmttweets$timestamp <- mdy_hm(wmttweets$created)

ggplot(data = wmttweets, aes(x = timestamp))+
  geom_histogram(aes(fill = ..count..),bins=20)+
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#Sentimental Analysis
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
# Reading Poitive and Negative words
pos <- scan('positive-words.txt', what='character', comment.char=';')
neg <- scan('negative-words.txt', what='character', comment.char=';')

pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'hack', 'breach', 'cybersecurity')

wmtDataset <- wmttweets
wmtDataset$text <- wmttweets[,2]
wmtscores <- score.sentiment(wmtDataset$text, pos.words, neg.words)
wmtscores$score

wmttweets<- read.csv('wmttweets.csv',stringsAsFactors = FALSE)
wmtSentiment <- get_nrc_sentiment(wmttweets$text)
head(wmtSentiment)
wmttweets <- cbind(wmttweets, wmtSentiment)
wmtsentimentTotals <- data.frame(colSums(wmttweets[,c(19:28)])) 
names(wmtsentimentTotals) <- "count"
wmtsentimentTotals <- cbind("sentiment" = rownames(wmtsentimentTotals), wmtsentimentTotals)
rownames(wmtsentimentTotals) <- NULL
ggplot(data = wmtsentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")


#Score and Tweets per day
temp4<- wmttweets
temp4$timestamp <- mdy_hm(temp4$created)
temp4$date<- as.Date(temp4$timestamp)
temp4<- cbind(temp4,wmtscores$score)

names(temp4)[30]<-"score"

ggplot(data = temp4, aes(x = date))+
  geom_histogram(aes(fill = ..count..),binwidth = 1)+
  stat_summary(aes(y=score),fun.y=sum,geom='line')+
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

##Calculating Portfolio Risk
df <- read_excel("Daily.xlsx")
head(df)
gld = subset(df, Company=='GLD')
pep = subset(df, Company=='PEP')
wmt = subset(df, Company=='WMT')
amd = subset(df, Company=='AMD')
head(gld)

#Fundamental Analysis
model_amd = lm(Returns~Volume+EPS+SP500+DebtEquity_Ratio+PE_ratio,data=amd)
summary(model_amd)
step(model_amd)
model_wmt = lm(Returns~Volume+EPS+SP500+DebtEquity_Ratio+PE_ratio,data=wmt)
summary(model_wmt)
step(model_wmt)
model_pep = lm(Returns~Volume+EPS+SP500+DebtEquity_Ratio+PE_ratio,data=pep)
summary(model_pep)
step(model_pep)
model_gld = lm(Returns~Volume+SP500,data=gld)
summary(model_gld)

#Technical Analysis


#Simple Moving Average

amd$sma20_amd <- SMA(amd[c('Price')],n=20) # 20-day moving average AMD
wmt$sma20_wmt <- SMA(wmt[c('Price')],n=20) # 20-day moving average WMT
pep$sma20_pep <- SMA(pep[c('Price')],n=20) # 20-day moving average PEP
gld$sma20_gld <- SMA(gld[c('Price')],n=20) # 20-day moving average GLD
#sma_combined= data.frame(sma20_amd, sma20_gld, sma20_pep, sma20_wmt)

##Exponential Moving Average 
amd$ema14_amd = EMA(amd[c('Price')],n=14) #14-day EMA AMD
pep$ema14_pep = EMA(pep[c('Price')],n=14) #14-day EMA PEP
wmt$ema14_wmt = EMA(wmt[c('Price')],n=14) #14-day EMA WMT
gld$ema14_gld = EMA(gld[c('Price')],n=14) #14-day EMA GLD
#tail(ema14_gld)
##Bollinger bands for all stocks
amd = cbind(amd,BBands(amd[c('Price')], sd=2.0))
pep = cbind(pep,BBands(pep[c('Price')], sd=2.0)) 
wmt = cbind(wmt,BBands(wmt[c('Price')], sd=2.0)) 
gld = cbind(gld,BBands(gld[c('Price')], sd=2.0))
#bb_combined= data.frame(bb20_amd, bb20_gld, bb20_pep, bb20_wmt)
#tail(bb20_gld)
#RSI for all stocks
wmt$rsi_wmt = RSI(wmt[c('Price')], n=14)
pep$rsi_pep = RSI(pep[c('Price')], n=14)
amd$rsi_amd = RSI(amd[c('Price')], n=14)
gld$rsi_gld = RSI(gld[c('Price')], n=14)
#rsi_combined= data.frame(rsi_amd, rsi_gld, rsi_pep, rsi_wmt)
#tail(rsi_gld)
#MACD for all stocks
wmt$macd_wmt = MACD(wmt[c('Price')], nFast=12, nSlow=26, nSig=9, maType=SMA)[,1]
pep$macd_pep = MACD(pep[c('Price')], nFast=12, nSlow=26, nSig=9, maType=SMA)[,1]
amd$macd_amd = MACD(amd[c('Price')], nFast=12, nSlow=26, nSig=9, maType=SMA)[,1]
gld$macd_gld = MACD(gld[c('Price')], nFast=12, nSlow=26, nSig=9, maType=SMA)[,1]
#macd_combined= data.frame(macd_amd, macd_gld, macd_pep, macd_wmt)
#CPI
CPI <- as.data.frame(read_xlsx("CPI.xlsx"))
dim(CPI)

#DataFrame
amd<-data.frame(amd)
wmt<-data.frame(wmt)
gld<-data.frame(gld)
pep<-data.frame(pep)

#Adding CPI data
amd<-cbind(amd,CPI)
wmt<-cbind(wmt,CPI)
pep<-cbind(pep,CPI)
gld<-cbind(gld,CPI)


amd$Ticker <- "AMD"
wmt$Ticker <- "WMT"
pep$Ticker <- "PEP"
gld$Ticker <- "GLD"


#Preparing Train data
amd_train <- amd[1:660,]
wmt_train <- wmt[1:660,]
pep_train <- pep[1:660,]
gld_train <- gld[1:660,]

amd_train <- DropNA(amd_train)
wmt_train <- DropNA(wmt_train)
gld_train <- DropNA(gld_train)
pep_train <- DropNA(pep_train)

amd<-amd[-c(2)]
wmt<-wmt[-c(2)]
gld<-gld[-c(2)]
pep<-pep[-c(2)]

amd_train<- amd_train[-c(2)]
wmt_train<- wmt_train[-c(2)]
gld_train<- gld_train[-c(2)]
pep_train<- pep_train[-c(2)]

colnames(amd_train)
colnames(amd_train) <- c("Company", "Price","Volume","SP500","EPS","PE_ratio","DebtEquity_Ratio","Returns","sma", "ema", "dn","mavg", "up", "pctB", "rsi", "macd", "Date","CPI","CPIReturns","Ticker")
colnames(wmt_train) <- c("Company", "Price","Volume","SP500","EPS","PE_ratio","DebtEquity_Ratio","Returns","sma", "ema", "dn","mavg", "up", "pctB", "rsi", "macd", "Date","CPI","CPIReturns","Ticker")
colnames(pep_train) <- c("Company", "Price","Volume","SP500","EPS","PE_ratio","DebtEquity_Ratio","Returns","sma", "ema", "dn","mavg", "up", "pctB", "rsi", "macd", "Date","CPI","CPIReturns","Ticker")
colnames(gld_train) <- c("Company", "Price","Volume","SP500","EPS","PE_ratio","DebtEquity_Ratio","Returns","sma", "ema", "dn","mavg", "up", "pctB", "rsi", "macd", "Date","CPI","CPIReturns","Ticker")

mergedata<-rbind(amd_train, wmt_train, pep_train, gld_train)
mergedata<- as.data.frame(mergedata)

#Converting to panel data 
table(index(pdata), useNA = "ifany")
colnames(pdata)
pdata<- pdata.frame(mergedata, index= c('Ticker', 'Date'))
panel_lm <- plm(data=pdata ,Returns ~ EPS+DebtEquity_Ratio+PE_ratio+ sma+ ema +up+dn+macd+pctB+ rsi+CPIReturns , index = c('Ticker', 'Date'), model= 'fd')
panel_lm1 <- plm(data=pdata ,Returns ~ macd+ rsi+CPIReturns , index = c('Ticker', 'Date'), model= 'fd')

summary(panel_lm)
summary(panel_lm1)
coefficients(panel_lm1)

## 
MACD_coeff<- unname(coefficients(panel_lm1)[2])
rsi_coeff<- unname(coefficients(panel_lm1)[3])
CPIReturns_coeff<- unname(coefficients(panel_lm1)[4])


##Preparing Test Data
amd_test <- amd[661:945,]
wmt_test <- wmt[661:945,]
pep_test <- pep[661:945,]
gld_test <- gld[661:945,]

amd_test <- DropNA(amd_test)
wmt_test <- DropNA(wmt_test)
gld_test <- DropNA(gld_test)
pep_test <- DropNA(pep_test)


colnames(amd_test) <- c("Company", "Price","Volume","SP500","EPS","PE_ratio","DebtEquity_Ratio","Returns","sma", "ema", "dn","mavg", "up", "pctB", "rsi", "macd", "Date","CPI","CPIReturns","Ticker")
colnames(wmt_test) <- c("Company", "Price","Volume","SP500","EPS","PE_ratio","DebtEquity_Ratio","Returns","sma", "ema", "dn","mavg", "up", "pctB", "rsi", "macd", "Date","CPI","CPIReturns","Ticker")
colnames(pep_test) <- c("Company", "Price","Volume","SP500","EPS","PE_ratio","DebtEquity_Ratio","Returns","sma", "ema", "dn","mavg", "up", "pctB", "rsi", "macd", "Date","CPI","CPIReturns","Ticker")
colnames(gld_test) <- c("Company", "Price","Volume","SP500","EPS","PE_ratio","DebtEquity_Ratio","Returns","sma", "ema", "dn","mavg", "up", "pctB", "rsi", "macd", "Date","CPI","CPIReturns","Ticker")

mergetestdata<-rbind(amd_test, wmt_test, pep_test, gld_test)
mergetestdata<- as.data.frame(mergetestdata)

##Returns

clean_test_df<-mergetestdata %>% select(Date, Ticker,Returns, macd, CPIReturns, rsi)
clean_test_df$Predicted_return<- (MACD_coeff*clean_test_df$macd) +(rsi_coeff*clean_test_df$rsi) +(CPIReturns_coeff*clean_test_df$CPIReturns)

# Gathering predicted return
long_data_fmt_df<-clean_test_df %>% select(Date,Ticker,Predicted_return)
wide_data_fmt_df<-tidyr::spread(long_data_fmt_df,Ticker, Predicted_return)
stock_daily_retn<- wide_data_fmt_df
#wide_data_fmt_df

## Ranking the returns

for (i in 1:nrow(wide_data_fmt_df)){
  wide_data_fmt_df[i,2:5]<- rank(wide_data_fmt_df[i,2:5])
}

#Deciding which stocks to buy/sell
long_short_df<- data.frame()
long_short_df<- wide_data_fmt_df
long_short_df[,2:5][long_short_df[,2:5]<=4]<- -1
long_short_df[,2:5][long_short_df[,2:5]>4]<-1

## Compiling actual returns for these stocks
daily_log_returns<- clean_test_df %>% select(Date, Ticker, Returns)
actual_rtn_df<- tidyr:: spread(daily_log_returns, Ticker,Returns)
actual_rtn_df[,2:5]<- actual_rtn_df[,2:5]*100

##Calculating log/Short returns
long_short_rtn<- long_short_df
long_short_rtn[,2:5]<- long_short_df[,2:5]*actual_rtn_df[,2:5]
for (i in 1:nrow(long_short_rtn)){
  long_short_rtn[i,"Total_Portfolio_rtn"]<- sum(long_short_rtn[i,2:6])
}

## Calculating Portfolio Metrics
sum(actual_rtn_df[,5])
((StdDev(actual_rtn_df[,5])))/sqrt(285)*sqrt(252)

##Regression With Sentiment Analysis
df <- read_excel("sent_reg.xlsx")
head(df$CPI)
gld = subset(df, STOCK=='GOLD')
pep = subset(df, STOCK=='PEPSI')
wmt = subset(df, STOCK=='WALMART')
amd = subset(df, STOCK=='AMD')

#Simple Moving Average
sma3_amd <- SMA(amd[c('Price')],n=3) # 20-day moving average AMD
sma3_wmt <- SMA(wmt[c('Price')],n=3) # 20-day moving average WMT
sma3_pep <- SMA(pep[c('Price')],n=3) # 20-day moving average PEP
sma3_gld <- SMA(gld[c('Price')],n=3) # 20-day moving average GLD
tail(sma3_gld)
##Exponential Moving Average 
ema2_amd = EMA(amd[c('Price')],n=2) #14-day EMA AMD
ema2_pep = EMA(pep[c('Price')],n=2) #14-day EMA PEP
ema2_wmt = EMA(wmt[c('Price')],n=2) #14-day EMA WMT
ema2_gld = EMA(gld[c('Price')],n=2) #14-day EMA GLD
tail(ema14_gld)
##Bollinger bands for all stocks
bb_amd = BBands(amd[c('Price')], sd=1.0) 
bb_pep = BBands(pep[c('Price')], sd=1.0) 
bb_wmt = BBands(wmt[c('Price')], sd=1.0) 
bb_gld = BBands(gld[c('Price')], sd=1.0) 
tail(bb_gld)
#RSI for all stocks
rsi_wmt = RSI(wmt[c('Price')], n=3)
rsi_pep = RSI(pep[c('Price')], n=3)
rsi_amd = RSI(amd[c('Price')], n=3)
rsi_gld = RSI(gld[c('Price')], n=3)
tail(rsi_gld)
#MACD for all stocks
macd_wmt = MACD(wmt[c('Price')], nFast=3, nSlow=6, nSig=2, maType=SMA)
macd_pep = MACD(pep[c('Price')], nFast=3, nSlow=6, nSig=2, maType=SMA)
macd_amd = MACD(amd[c('Price')], nFast=3, nSlow=6, nSig=2, maType=SMA)
macd_gld = MACD(gld[c('Price')], nFast=3, nSlow=6, nSig=2, maType=SMA)
tail(macd_gld)

#Adding all the technical indicators to the data
amd_full = data.frame(amd, sma3_amd, ema2_amd,rsi_amd, macd_amd)
wmt_full = data.frame(wmt, sma3_wmt, ema2_wmt,rsi_wmt, macd_wmt)
pep_full = data.frame(pep, sma3_pep, ema2_pep, rsi_pep, macd_pep)
gld_full = data.frame(gld, sma3_gld, ema2_gld, rsi_gld, macd_gld)

model_amd=lm(Returns~CPI+Overall_Sentiment+Volume+SP500+sma3_amd+ema2_amd+rsi_amd+macd,data=amd_full)
summary(model_amd)
step(model_amd)
#best Factors for AMD
model_amd=lm(Returns~sma3_amd+macd,data=amd_full)

model_wmt=lm(Returns~CPI+Overall_Sentiment+Volume+SP500+sma3_wmt+ema2_wmt+rsi_wmt+macd,data=wmt_full)
summary(model_wmt)
step(model_wmt)
#best Factors for WMT
model_wmt=lm(Returns~SP500+sma3_wmt+macd,data=amd_full)

model_pep=lm(Returns~Overall_Sentiment+Volume+SP500+sma3_pep+ema2_pep+rsi_pep+macd,data=pep_full)
summary(model_pep)
step(model_pep)
#best Factors for pep
model_pep=lm(Returns~Overall_Sentiment+sma3_pep+ema2_pep+rsi_pep+macd,data=amd_full)

model_gld=lm(Returns~Overall_Sentiment+Volume+SP500+sma3_amd+ema2_gld+rsi_gld+macd,data=gld_full)
summary(model_gld)
step(model_gld)
#best factors for gold
model_pep=lm(Returns~Overall_Sentiment+Volume+sma3_gld+ema2_gld+rsi_gld+macd,data=amd_full)

write.csv(gld_full, 'C:/Users/gaura/downloads/gldtweet.csv')
write.csv(amd_full, 'C:/Users/gaura/downloads/amdtweet.csv')
write.csv(pep_full, 'C:/Users/gaura/downloads/peptweet.csv')
write.csv(wmt_full, 'C:/Users/gaura/downloads/wmttweet.csv')

install.packages("plm")
library(plm)
df1 <- read.csv("amdtweet.csv")
df2 <- read.csv("wmttweet.csv")
df2 <- read.csv("peptweet.csv")
df4 <- read.csv("gldtweet.csv")

#plotting for the year 2020 for every stock

library(zoo) # Load the zoo package
getSymbols('AMD',src='yahoo',from='2020-01-01',to='2020-10-01')
getSymbols('GLD',src='yahoo',from='2020-01-01',to='2020-10-01')
chartSeries(AMD,subset='2020-09-14::2020-10-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=6,sd=2,),addSMA(n=3,col="blue"),addSMA(n=3,col="black"), 
                 addRSI(n=6),addVo(),addMACD(),addWPR()))  


getSymbols("WMT")
chartSeries(WMT,subset='2020-09-14::2020-10-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=6,sd=2,),addSMA(n=3,col="blue"),addSMA(n=3,col="black"), 
                 addRSI(n=6),addVo(),addMACD(),addWPR())) 

getSymbols("PEP")
chartSeries(PEP,subset='2020-09-14::2020-10-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=6,sd=2,),addSMA(n=3,col="blue"),addSMA(n=3,col="black"), 
                 addRSI(n=6),addVo(),addMACD(),addWPR())) 

chartSeries(GLD,subset='2020-09-14::2020-10-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=6,sd=2,),addSMA(n=3,col="blue"),addSMA(n=3,col="black"), 
                 addRSI(n=6),addVo(),addMACD(),addWPR()))  



