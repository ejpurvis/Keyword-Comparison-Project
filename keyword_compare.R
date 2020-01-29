# Install Load and Definitions 

# Install
# install.packages("tm") # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator
# install.packages("RColorBrewer") # color palettes
# install.packages("ggplot2") #plotting tool
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("compare")

# Load
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(forcats)
library(dplyr)
library(reshape2)
library(compare)

# Define Functions
# Text-mining and word cloud creation derived from STHDA "Text mining and word cloud fundamentals in R : 5 simple steps you should know" and modified to fit the application
# Stemming algorithm borrowed from Snowball - Porter Stemming Algorithm
keyword_ranking <- function(filePath){
  text <- readLines(filePath, warn = FALSE)
  
  # Load data as a corpus
  docs <- Corpus(VectorSource(text))
  
  # Inspect content of document
  inspect(docs)
  
  # Converts characters to spaces
  toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
  
  # List of characters to convert to spaces
  docs <-tm_map(docs, toSpace, "/")
  docs <-tm_map(docs, toSpace, "\t")
  docs <-tm_map(docs, toSpace, "•")
  docs <-tm_map(docs, toSpace, "–")
  docs <-tm_map(docs, toSpace, "@")
  docs <-tm_map(docs, toSpace, "\\|")
  
  # Text Cleaning (ie. lowercase, remove numbers, remove punctuation, etc...)
  
  # Convert text to lowercase
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("English"))
  # Remove punctuation
  docs <- tm_map(docs, removePunctuation)
  #Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming (Optional - efectively does this: Management && Managed == Manage*)
  docs <- tm_map(docs, stemDocument)
  # Specify your own stopwords if desired
  docs <- tm_map(docs, removeWords, c("use", "can", "find", "will", "make", "abil", "set", "'ll", "take", "want", "abl", "role", "area", "practic", "include", "work"))
  
  #For later: add in functionality to see what the stem is/was and what actual full words are contained in it
  #For later: add in functionality to identify phrases (ie. "data analytics" when applicable instead of "data" and "analytics" used separately)
  
}


# Body

# Read text from following file location
filePathC <- file.choose()
filePathR <- file.choose()

wdpath <- gsub("(.*)/.*","\\1",filePathC)

setwd(wdpath)

title <- basename(filePathC)
titleR <- basename(filePathR)

# Invoke keyword ranking function for the posting
docs <- keyword_ranking(filePath = filePathC)

# Building a term-document matrix - gauges frequency of terms and creatres a frequency table (ie. what is required for rankings)
dtm_a <- TermDocumentMatrix(docs)
ma <- as.matrix(dtm_a)
va <- sort(rowSums(ma), decreasing = TRUE)
da <- data.frame(word = names (va), freqA=va, stringsAsFactors = FALSE)
head(da,10)

# Invoke keyword ranking function for the resumé
docs <- keyword_ranking(filePath = filePathR)

# Building a term-document matrix - gauges frequency of terms and creatres a frequency table (ie. what is required for rankings)
dtm_b <- TermDocumentMatrix(docs)
mb <- as.matrix(dtm_b)
vb <- (sort(rowSums(mb), decreasing = TRUE))
db <- data.frame(word = names (vb), freqB=vb, stringsAsFactors = FALSE)
head(db, 10)

# Generating the wordcloud for posting
set.seed(1234)
wordcloud(word = row.names(da), freq = da$freqA, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Blues"))

# Generating the wordcloud for resumé
set.seed(1234)
wordcloud(word = row.names(db), freq = db$freqB, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Reds"))

ljoin <- left_join(da,db)
ljoin

d <- subset(ljoin, freqA > 1)

d[is.na(d)] <- 0

d <- mutate(d,word = fct_reorder(word, desc(freqA)))

# Key for displaying the data - Super important!! Cannot stress this enough!!!
d <- melt(d, id.vars='word')

colnames(d) <- c("word","type","freq")

# Flagging x-axis labels as good or bad (black or red respectively)
ax <- d[d$type == "freqB",]
a <- ifelse(ax$freq == 0 , "red", "black")

# Create export file containing words that are used at least twice from the posting
ay <- d[d$type == "freqA",]
ay[, "title"] <- title

co <- sub("\\ –.*", "", ay$title)  
ay[, "co"] <- co

pos <- gsub(".*– |.txt.*", "", ay$title)
ay[,"pos"] <- pos

drops <- c("title")
ay <- ay[,!(names(ay) %in% drops)]

ay <- ay[,c(4,5,1,2,3)]

s <- gsub("(.*).txt.*","\\1",filePathC)
csvpath <- paste(s,".csv",sep = "")
write.csv(ay,csvpath)

# Grouped barplot
ggplot(d, aes(word, freq, fill = type)) +
  # geom_bar(stat = "identity", position = position_dodge2(padding = 0.2)) +
  geom_col(position = "dodge", width = 0.75) +
  scale_x_discrete(limits = ax$word) +
  geom_text(check_overlap = TRUE,aes(label=freq, group = type), size = 3, vjust = 1.25, colour = "black", position = position_dodge(0.75)) +
  ggtitle(paste("Ranked Keywords for: ",title)) +
  theme(plot.title=element_text(size=15, vjust=6)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = a))
