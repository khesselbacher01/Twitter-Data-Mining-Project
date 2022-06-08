library(wordcloud)
library(twitteR)
library(ROAuth)

load(file="credS2022TwitteR.Rdata")
setup_twitter_oauth(cred$consumerKey, cred$consumerSecret, cred$oauthKey, cred$oauthSecret)

twitterName = "@elonmusk"
user <- getUser(twitterName)
View(user)

#get elons tweets
elonsTweets <- userTimeline(user, n= 3200, includeRts = TRUE, excludeReplies = FALSE) 
# elonsTweetsTwo <- userTimeline(user, n= 3200, includeRts = TRUE, excludeReplies = FALSE) ### just for testing, not used anywhere else

elonsTweetsDF <- twListToDF(elonsTweets)
View(elonsTweetsDF) 

elonsTweetsDF <- tweetsDF  ### for the FinalExamDataset if required 

save(elonsTweetsDF, file="./FinalExamDataset.Rdata")
load(file="./FinalExamDataset.Rdata") 
load(file="./elonsTweetsDFClean.Rdata")

####################################################### Cleaning the Tweets
removeNonASCII<- function(txt){
  return(iconv(txt, to="ASCII", sub=""))
}

elonsTweetsDF$text <- removeNonASCII(elonsTweetsDF$text)


removeControlCharacters<- function(x){
  x<- gsub("[[:cntrl:]]",  "",x)
  return(x)
}

elonsTweetsDF$text <- removeControlCharacters(elonsTweetsDF$text)



grep("http(s?)://[[:alnum:]].\\S*", elonsTweetsDF$text, value=TRUE)




removeRetweetSpaces <- function(x) {
  x<- gsub("(^rt|^mrt) @", "@",x, ignore.case=TRUE)
  return(x)
}

removeRetweetSpaces(elonsTweetsDF$text)
elonsTweetsDF$text <- removeRetweetSpaces(elonsTweetsDF$text)


grep("\'|\"", elonsTweetsDF$text, value=TRUE)
grep("[\r\n]", elonsTweetsDF$text, value=TRUE)


removeColons<- function(x){
  x<- gsub(":",  " ",x)
  return(x)
}

elonsTweetsDF$text <- removeColons(elonsTweetsDF$text)

removeAllPeriods<- function(x){
  x<- gsub("[.]",  " ",x)
  return(x)
}

elonsTweetsDF$text <- removeAllPeriods(elonsTweetsDF$text)

removeExtraSpaces<- function(x){
  x<- gsub("[[:space:]]+",  " ",x)
  return(x)
}

elonsTweetsDF$text <- removeExtraSpaces(elonsTweetsDF$text)

grep("^[[:space:]]", elonsTweetsDF$text, value=TRUE) 

removeLeadingTrailingSpaces<- function(x){
  x<- gsub("^[[:space:]]",  "",x)
  x<- gsub("[[:space:]]$",  "",x) 
  return(x)
}


grep("&.*;", elonsTweetsDF$text, value=TRUE) #&amp
m<- regexpr("&.*;", elonsTweetsDF$text)
regmatches(elonsTweetsDF$text, m)
removeMany<- function(x){
  x<- gsub("&.*;",  " ",x)
  x<- gsub("/",  " ",x)
  x<- gsub(",",  " ",x)
  x<- gsub("http",  " ",x)
  x<- gsub("https",  " ",x)
}




elonsTweetsDF$text <- removeLeadingTrailingSpaces(elonsTweetsDF$text)

elonsTweetsDFClean <- elonsTweetsDF
save(elonsTweetsDFClean, file= "./elonsTweetsDFClean.Rdata")


#################################################################mentions
grep("@[[:alnum:]]+", elonsTweetsDF$text, value=TRUE)
mentionsRegex <- regexpr("@[[:alnum:]]+", elonsTweetsDF$text)
mentions <- data.frame(table(regmatches(elonsTweetsDF$text, mentionsRegex)))
names(mentions)<- c("mention", "Freq")
mentions <- mentions[order(mentions$Freq),]
View(mentions)

wordcloud(words=mentions$mention, freq=mentions$Freq, scale=c(3, 0.5), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=1, max.words=Inf)

########################################################################## find the devices he uses
appUsed<- sub(".*>(.*)</a>", "\\1", elonsTweetsDFClean$statusSource)
appUsed<- tolower(appUsed)
apps <- data.frame(table(appUsed))
names(apps) <-c("app", "Freq")
View(apps[order(-apps$Freq),])
apps<- apps[ apps$Freq> 1,]
apps <- apps[order(apps$Freq),]
windows()

wordcloud(words=apps$app, freq=apps$Freq, scale=c(4, 1), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=1, max.words=Inf)

###############################################################################  cluster analysis
library(apcluster)
simMatrix <- negDistMat(elonsTweetsDFClean, method="euclidean", r=2) 

apclusterK(simMatrix, details=TRUE, K=4, verbose=TRUE, seed=98765)

################################################################################ another wordcloud for individual words

library(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(elonsTweetsDFClean$text,scale=c(3, 0.6), random.order=T, colors=pal2)
grep("colorad.*", elonsTweetsDFClean$text, value=TRUE)
elonsTweetsDFClean$text <- gsub("colorad.*",  "CO",elonsTweetsDFClean$text)

grep("CO", elonsTweetsDFClean$text, value=TRUE)

####################################################################################### Elon's name on the wider web
nameMentions <- searchTwitter('Elon Musk', n=3200)
nameMentionsDF <- twListToDF(nameMentions)

removeNonASCII<- function(txt){
  return(iconv(txt, to="ASCII", sub=""))
}

nameMentionsDF$text <- removeNonASCII(nameMentionsDF$text)


removeControlCharacters<- function(x){
  x<- gsub("[[:cntrl:]]",  "",x)
  return(x)
}

elonsTweetsDF$text <- removeControlCharacters(nameMentionsDF$text)



grep("http(s?)://[[:alnum:]].\\S*", nameMentionsDF$text, value=TRUE)




removeRetweetSpaces <- function(x) {
  x<- gsub("(^rt|^mrt) @", "@",x, ignore.case=TRUE)
  return(x)
}

removeRetweetSpaces(nameMentionsDF$text)
nameMentionsDF$text <- removeRetweetSpaces(nameMentionsDF$text)


grep("\'|\"", nameMentionsDF$text, value=TRUE)
grep("[\r\n]", nameMentionsDF$text, value=TRUE)


removeColons<- function(x){
  x<- gsub(":",  " ",x)
  return(x)
}

nameMentionsDF$text <- removeColons(nameMentionsDF$text)

removeAllPeriods<- function(x){
  x<- gsub("[.]",  " ",x)
  return(x)
}

nameMentionsDF$text <- removeAllPeriods(nameMentionsDF$text)

removeExtraSpaces<- function(x){
  x<- gsub("[[:space:]]+",  " ",x)
  return(x)
}

nameMentionsDF$text <- removeExtraSpaces(nameMentionsDF$text)

grep("^[[:space:]]", nameMentionsDF$text, value=TRUE) 

removeLeadingTrailingSpaces<- function(x){
  x<- gsub("^[[:space:]]",  "",x)
  x<- gsub("[[:space:]]$",  "",x) 
  return(x)
}
nameMentionsDF$text <- removeLeadingTrailingSpaces(nameMentionsDF$text)

nameMentionsClean <- nameMentionsDF
save(nameMentionsClean, file="./nameMentions.Rdata")


#########################people who mentioned elons name and the number of times they mentioned it
grep("@[[:alnum:]]+", nameMentionsClean$text, value=TRUE)
mentionsRegex <- regexpr("@[[:alnum:]]+", nameMentionsClean$text)
mentions <- data.frame(table(regmatches(nameMentionsClean$text, mentionsRegex)))
names(mentions)<- c("mention", "Freq")
mentions <- mentions[order(mentions$Freq),]
View(mentions)

wordcloud(words=mentions$mention, freq=mentions$Freq, scale=c(3, 0.5), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=1, max.words=Inf)

####################################################### More color for the coud 
library(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(nameMentionsClean$text,scale=c(3, 0.6), min.freq = 75, random.order=FALSE, colors=pal2)
grep("colorad.*", nameMentionsClean$text, value=TRUE)
nameMentionsClean$text <- gsub("colorad.*",  "CO",nameMentionsClean$text)

grep("CO", nameMentionsClean$text, value=TRUE)


###################################################################### another wordcloud for mentions
appUsed<- sub(".*>(.*)</a>", "\\1", nameMentionsDF$statusSource)
appUsed<- tolower(appUsed)
apps <- data.frame(table(appUsed))
names(apps) <-c("app", "Freq")
View(apps[order(-apps$Freq),])
apps<- apps[ apps$Freq> 1,]
apps <- apps[order(apps$Freq),]
windows()

wordcloud(words=apps$app, freq=apps$Freq, scale=c(4, 1), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=1, max.words=Inf)



grep("&.*;", nameMentionsDF$text, value=TRUE) #&amp
m<- regexpr("&.*;", atdf$text)
regmatches(nameMentionsDF$text, m)
removeMany<- function(x){
  x<- gsub("&.*;",  " ",x)
  x<- gsub("/",  " ",x)
  x<- gsub(",",  " ",x)
  x<- gsub(" http",  " ",x)
  x<- gsub("https ",  " ",x)
}


######## Tokenize the words and look for the most common three words
library(tokenizers)
threeGramCloud <- tokenize_ngrams(nameMentionsDF$text, n = 3, n_min = 3,lowercase=TRUE)
unlist(threeGramCloud)
g3<- table(unlist(threeGramCloud))

windows(width=10, height=10)
wordcloud(words=names(g3), freq=g3, scale=c(3.5, 1), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=3, max.words=Inf)

################################ Cluster analysis of mentioned tweets
library(apcluster)
simMatrix <- negDistMat(nameMentionsDF$text, method="euclidean", r=2) 

apclusterK(simMatrix, details=TRUE, K=4, verbose=TRUE, seed=28828)


