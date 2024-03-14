#### Sentiment analysis ####
source(file = "1_Preparation.R")
# classified as positive, neutral or negative
# can be also represented on a numeric scale, to better express the degree of positive or negative 
# strength of the sentiment
# use of Syuzhet package for generating sentiment scores
# offers a method for accessing the sentiment extraction tool developed by NLP group at Standford
# selected method determines which of the four available sentiment extraction methods will be used
# 4 methods: syuzhet, bing, afinn, nrc

# clean text
tweets_clean <- cleaning.FUN(sexism_data$text)
tweets_clean <- stemDocument(tweets_clean)
tweets_clean <- iconv(tweets_clean)

# clean sentences
sentences_split <- get_sentences(sexism_data$text)
sentences_clean <- stemDocument(cleaning.FUN(sentences_split))
sentences_clean <- iconv(sentences_clean)

##### Using NRC method #####
# sentiment analysis on tweets
s1 <- get_nrc_sentiment(tweets_clean)
# sentiment analysis on sentences
s <- get_nrc_sentiment(sentences_clean)

summary(s1)
# mean of positive: 0.5636, Max = 5
# mean of negative: 0.4289, Max = 5
# mean of anger: 0.2216, Max = 4
# mean of anticipation: 0.3232, Max = 4
# mean of disgust: 0.1914, Max = 4
# mean of fear: 0.2179, Max = 4
# mean of joy: 0.2721, Max = 4
# mean of sadness: 0.1792, Max = 4
# mean of surprise: 0.1637, Max = 4
# mean of trust: 0.3611, Max = 5

# Bind both datasets together -> Sexism dataset in combination with its sentiments
complete <- cbind(sexism_data, s1)
complete$dataset <- as.factor(complete$dataset)

# Bind sentence with its sentiment
complete_sen <- cbind(sentences_split, s)

# descriptive analysis under sentiments

# negative sentiment
sencom <- complete %>%
  group_by(dataset) %>%
  filter(negative == 1)

table(sencom$dataset)

# anger emotions
angcom <- complete %>%
  group_by(dataset) %>%
  filter(anger == 1)

table(angcom$dataset)

# disgust emotions
discom <- complete %>%
  group_by(dataset) %>%
  filter(disgust == 1)

table(discom$dataset)

# fear emotions
fearcom <- complete %>%
  group_by(dataset) %>%
  filter(fear == 1)

table(fearcom$dataset)

# sadness
sadcom <- complete %>%
  group_by(dataset) %>%
  filter(sadness == 1)

table(sadcom$dataset)


# Elements which is defined as negative

negative_items = which(s$negative > 0)
negativesd <- sentences_split[negative_items]

# Possible Tweet: "Appreciate your lady, your mum, your sister today and every day # WomensDay" 
#                  (Questionable)
# Possible Tweet: "You all fucking hate each other."


# Elements which is defined as positive
positive_items = which(s$positive > 0)
positivesd <- sentences_split[positive_items]

# Possible Tweet: "It is wrong for a man to enter a traditionally female career" (Questionable)
# Possible Tweet: "Household chores should not be allocated by sex"


# Using Syuzhet method
syuzhet_vector <- get_sentiment(tweets_clean, method = "syuzhet")

# Using Bing method
bing_vector <- get_sentiment(tweets_clean, method = "bing")

# Using Afinn method
afinn_vector <- get_sentiment(tweets_clean, method = "afinn")

##### Using Nrc method #####
nrc_vector <- get_sentiment(tweets_clean, method = "nrc", language = "english")

# bind sentiment using NRC method with original data
nrcs <- cbind(sexism_data, nrc_vector)

# bind all scores and use sign for better scaling
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)),
  sign(head(nrc_vector))
)

# overall emotional valence: 1572.35 -> positive emotional valence
sum(syuzhet_vector)

# central tendency: mean emotional valence: 0.115351 -> positive central tendency
mean(syuzhet_vector)


ggplot(mapping = aes(x = 1:100, y = nrc_vector[1:100])) +
  geom_line() +
  labs(x = "Narrative Time", y = "Emotional Valence", title = "Example Plot Trajectory") +
  theme_minimal()

# mean emotional valence for each chunk
percent_vals <- get_percentage_values(nrc_vector, bins = 100)
ggplot(mapping = aes(x = 1:100, y = percent_vals)) +
  geom_line(col = "red") +
  labs(x = "Narrative Time", y = "Emotional Valence", 
       title = "Tweets Using Percentage-Based Means") +
  theme_minimal()

# Plot Sentiment Count for each emotion
sum_s1 <- colSums(s1)
sum_s1 <- data.frame("sentiment" = names(sum_s1), "count" = colSums(s1), row.names = NULL)
ggplot(data = sum_s1, mapping = aes(x = count, y = sentiment)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = NULL, title = "Sentiment Scores Tweets") +
  theme_minimal()


# shape smoothing and normalization (Fourier based transformation)
ft_values <- get_transformed_values(
  nrc_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)

ggplot(mapping = aes(x = 1:100, y = ft_values)) +
  geom_line(col = "red") +
  labs(x = "Narrative Time", y = "Emotional Valence", title = "Tweets using Transformed Values") +
  theme_minimal()

# Discrete cosine transformation
dct_values <- get_dct_transform(
  nrc_vector, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)
ggplot(mapping = aes(x = 1:100, y = dct_values)) +
  geom_line(col = "red") +
  labs(x = "Narrative Time", y = "Emotional Valence", title = "Tweets using Transformed Values") +
  theme_minimal()

# simple plot 
simple_plot(nrc_vector, legend_pos = "bottomleft")

# negative sentiment & sexist = FALSE
NF <- complete %>%
  filter(!sexist & negative == 1)
# positive sentiment & sexist = TRUE
PT <- complete %>%
  filter(sexist & positive == 1)
