library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(RTextTools)
library(e1071)
library(dplyr)

#reading the csv file
demonetization = read_csv("C:/Users/Jigyasa/Downloads/tweets_demonetization.csv")

# remove retweet entities
text <- as.character(demonetization$text)
class(text)
sub_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)

# remove at people
sub_text <- gsub("@\\w+", "", sub_text)
##let's clean html links
sub_text<-gsub("http[^[:blank:]]+","",sub_text)
# remove punctuation
sub_text <- gsub("[[:punct:]]", "", sub_text)
# remove numbers
sub_text <- gsub("[[:digit:]]", "", sub_text)
# remove html links
sub_text <- gsub("http\\w+", "", sub_text)
# remove unnecessary spaces
sub_text <- gsub("[ \t]{2,}", "", sub_text)

sub_text<- gsub("^\\s+|\\s+$", "", sub_text)
##let's remove number (alphanumeric)
sub_text <- gsub("[^[:alnum:]]"," ",sub_text)

class(sub_text)
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
sub_text <- sapply(sub_text, try.error)

# remove NAs in some_txt
sub_text <- sub_text[!is.na(sub_text)]
names(sub_text) = NULL
class(text)

# Perform Sentiment Analysis

# classify emotion
class_emo = classify_emotion(sub_text, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
# classify polarity
class_pol = classify_polarity(sub_text, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
# data frame with results
sent_df = data.frame(text=sub_text, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
sent_df
emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE)))
#Let's do some plots of the obtained results
# plot distribution of emotions
# Create the bar chart
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets", title ='Twitter sentiment for Demonetization 2016')

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity", y="number of tweets")

#Separate the text by emotions and visualize the words with a comparison cloud
# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = sub_text[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus1 = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)



#top 20 words
mat= create_matrix(sub_text, language="english", 
                   removeStopwords=TRUE, removeNumbers=TRUE, 
                   stemWords=TRUE, tm::weightTfIdf)
mean_train= sort(colMeans(as.matrix(mat)),decreasing =T)
mean_train[1:20]
average_top20 = mean(mean_train[1:20])
average_top20

##show the frequency In BarPlot 

barplot(mean_train[1:20],border = NA,las = 3,xlab = "top 20 words",
        ylab = "Frequency", ylim = c(0,3))
##wordcloud 
wordcloud(names(mean_train[1:20]),mean_train[1:20] , scale=c(5,1),colors = brewer.pal(nemo, "Dark2"))

#another insight
text <- as.character(demonetization$text)
corpus = Corpus(VectorSource(list(text)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, stemDocument)
dtm_up = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_up <- colSums(as.matrix(dtm_up))

#Calculating Sentiments
sentiments_up = calculate_sentiment(names(freq_up))
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]

cat("We have far lower negative Sentiments: ",sum(sent_neg_up$freq_up)," than positive: ",sum(sent_pos_up$freq_up))

DT::datatable(sent_pos_up)

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(sent_pos_up$text,sent_pos_up$freq,min.freq=10,colors=brewer.pal(6,"Dark2"))
#------------ End of word cloud-----------------#
#-------Starting code to count maximum posts & their locations---#
library(dplyr)
user_info = demonetization_new$from_user_screen_name
#help(group_by)

tweetsByName <-
  demonetization_new %>%
  group_by(from_user_screen_name) %>%
  summarize(freq=n()) %>%
  arrange(desc(freq))

topPosters <-
  tweetsByName %>%
  top_n(n = 10) %>%
  left_join(demonetization_new) %>%
  group_by(from_user_screen_name)

# To find users who post the most
topPosters %>% 
  select(from_user_screen_name) %>% 
  unique()

# To find the location of top posters
topPosters %>% 
  select(from_user_location) %>% 
  unique()
