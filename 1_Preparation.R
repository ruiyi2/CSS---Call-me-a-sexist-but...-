library(tm)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(readr)
library(RTextTools)
library(e1071)
library(randomForest)
library(caret)

# read data set 
sexism_data <- read_csv("sexism_data.csv")

#### Descriptive Analysis ####
table(sexism_data$sexist) # 11822 no sexist, 1809 sexist
table(sexism_data$dataset) # 1080 benevolent, 2431 callme, 1257 hostile, 7985 other, 878 scales

d <- ggplot(sexism_data, aes(x = toxicity)) +
  geom_histogram(col = "white") +
  labs(x = "Toxicity", y = "Frequency") +
  theme_minimal()

ggplot_build(d) # for x = 0.10154741 the count is the highest (x e [0.08462285, 0.11847198])

##### Differentiation of the datasets #####
table(sexism_data$dataset)

table(sexism_data$dataset[sexism_data$sexist])

##### Load that Vector or text data as a Corpus #####

corpus <- iconv(sexism_data$text)
corpus <- Corpus(VectorSource(corpus))

# Cleaning process
# Convert all the text into lowercase

cleaning.FUN <- function(x) {
  # lowercase words only
  x <- tolower(x)
  
  ## remove specific characters
  # remove mentions
  x <- gsub("mention([[:digit:]]+)", "", x)
  # remove all hashtags
  x <- gsub("#([[:graph:]]+)", "", x)
  # Remove links
  x <- gsub("http(s?)://([[:graph:]]+)", "", x)
  # Remove all non-ASCII characters
  x <- gsub("[^\x01-\x7F]", "", x)
  
  ## generic cleaning
  # remove numbers from the text data
  x <- removeNumbers(x)
  # Remove stop words, have little value in terms of extracting useful information from the text
  # E.g. "The, is, at, on"
  x <- removeWords(x, words = stopwords("english"))
  x <- gsub("'ve", "", x)
  # removing special characters from the text
  x <- removePunctuation(x)
  # Strip extra whitespace from a text document
  stripWhitespace(x)
}

cleanset <- tm_map(x = corpus, FUN = cleaning.FUN)

##### Term document matrix #####
# Matter the incidence of every word, to perceive famous or tredning topics
# Construct a Document Matrix - a table containing the frequency of words 

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)

## Bar plot
# plotting the words using a bar chart 

ww <- sort(x = rowSums(tdm), decreasing = T)
ww <- data.frame("word" = names(ww), "freq" = ww, row.names = NULL)
ww <- ww[1:20, ]

ggplot(data = ww, 
       mapping = aes(x = freq, y = reorder(word, freq))) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = NULL) +
  theme_minimal()
  