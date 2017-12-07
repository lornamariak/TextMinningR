#Rscript by @lornamariak

#twitter API lib
library(twitteR)
#to draw the wordcloud
library(wordcloud)
library(RColorBrewer)
#to clean the data
library(tm)

#credentials
consumer_key <- "**********"
consumer_secret<- "****************"
access_token <- "*********************"
access_secret <- "********************"

#set up authenticate
# choose yes for auto authenticationa dn no otherwise
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)
#fetch tweets
tweets <- twitteR::searchTwitter("#EndSARS",n =1000,lang ="en",since = '2017-12-01')

#convert to data frame
df <- twListToDF(tweets)

#extract the data frame save it locally
saveRDS(df, file="mytweets.rds")
df2 <- readRDS("mytweets.rds")

#time to clean our data
#cleaning lib tm comes in action

newdata<- iconv(df2$text, "ASCII", "UTF-8", sub="")

#extract text from the data frame build your own corpus(a corpus is a collection of text files)
mydata <- Corpus(VectorSource(newdata))

# convert to lower case
mydata <- tm_map(mydata, content_transformer(tolower))

#remove ������ what would be emojis
mydata<-tm_map(mydata, content_transformer(gsub), pattern="\\W",replace=" ")

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeURL)
                 )
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeNumPunct))

# remove stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))

#u can create custom stop words using the code below.
#myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
#mydata <- tm_map(mydata, removeWords, myStopwords)

# remove extra whitespace
mydata <- tm_map(mydata, stripWhitespace)

# Remove numbers
mydata <- tm_map(mydata, removeNumbers)

# Remove punctuations
mydata <- tm_map(mydata, removePunctuation)

# keep a copy for stem completion later
mydataCopy <- mydata

#build a term document matrix
dtm <- TermDocumentMatrix(mydata)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)


#draw the word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,max.words=500, random.order=FALSE,scale = c(3, 0.5), colors = rainbow(50))
#80 lines and you're done!