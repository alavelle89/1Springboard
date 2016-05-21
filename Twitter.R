#===========================
#SET-UP AND INSTALL
#===========================

#Install twitteR package
install.packages("twitteR")

#Load googleVis library
library("twitteR")
library(ggplot2)

#Load authorizaiton token from Twitter
api_key <- "DR1IhMhj1xmxKmEPTto90e3oC"
api_secret <- "3UkdvTQTnpNDPfhjTVOBZeMoxFt0Ne6vXFqzDXUrHYSw5z3G8F"
access_token <- "725860155733090305-UNvUFSFavMG5Cm9xaX1yDwdehDCLljo"
access_token_secret <- "Gud7aalsnTCZW8Yi9iJ7jHgK7ObbyUNjryTG2ixLXv2Qh"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#===========================
#Install other packages for later
install.packages("tm")
install.packages("devtools")
install.packages("rjson")
install.packages("bit64")
install.packages("httr")

#Install twitteR from GitHub
install.packages(c("devtools", "rjson", "bit64", "httr", "tm"))

#RESTART R session!

library(devtools)
install_github("twitteR", username="URBN_LaVelle")
library(twitteR)
library(plyr)
library(dplyr)
require(ggplot2)
#===========================

#===========================
#EXTRACTING TWEETS

#Search Tweets

tweets <-searchTwitter("@Chipotle",n=20000, lang="en", retryOnRateLimit =5 )


tweeterdata1=laply(tweets, function(t) t$getText())
tweeterdata1 <- sapply(tweeterdata1,function(row) iconv(row, "latin1", "ASCII", sub=""))
length(tweeterdata1)


#post = readLines("positive_words.txt")
#grep("and", tweeterdata1)
#pos <- laply(pos, function(t) as.character(t))

pos.words = scan('positive_words.txt',
                  what='character', comment.char=';')
neg.words = scan('negative_words.txt',
                  what='character', comment.char=';')
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

sample = tweeterdata1
result = score.sentiment(sample, pos.words, neg.words)
colnames(result)
result <- result[,1]

Neg_8 = length(which(result==-8))
Neg_7 <- length(which(result==--7))
Neg_6 <- length(which(result==-6))
Neg_5 <- length(which(result==-5))
Neg_4 <- length(which(result==-4))
Neg_3 <- length(which(result==-3))
Neg_2 <- length(which(result==-2))
Neg_1 <- length(which(result==-1))
##Zero <- length(which(result==0))
Pos_1 <- length(which(result==1))
Pos_2 <- length(which(result==2))
Pos_3 <- length(which(result==3))
Pos_4 <- length(which(result==4))
Pos_5 <- length(which(result==5))
Pos_6 <- length(which(result==6))
Pos_7 <- length(which(result==7))
Pos_8 <- length(which(result==8))

#Combine all counts into one set
master_count = cbind(Neg_8, Neg_7, Neg_6, Neg_5, Neg_4, Neg_3, Neg_2, Neg_1, Pos_1, Pos_2, Pos_3, Pos_4, Pos_5, Pos_6, Pos_7, Pos_8)

#Bar Plot the data and include new names
barplot(master_count,xpd = FALSE, main="Sentiment Word Count", xlab = "Trigger Words per Tweet", ylab = "Number of Instances", col=c("red"),names.arg = c("(-8)", "(-7)", "(-6)", "(-5)","(-4)","(-3)","(-2)","(-1)","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)"))

#Create output set with total, positives, and average tweet sentiment per tweet
output = as.data.frame(cbind(length(result), sum(result), sum(result/length(result))))
names(output[,1]) <- "Total"
colnames(output) <- c("Total", "Sentiment", "Avg per Tweet")
output

