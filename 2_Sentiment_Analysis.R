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

# table sentiment count for each tweet
table(complete$anger) # 0: 11013, 1: 2256, 2: 325, 3: 33, 4: 4
table(complete$anticipation) # 0: 9952, 1: 3027, 2: 589, 3: 52, 4: 11
table(complete$disgust) # 0: 11270, 1: 2136, 2: 204, 3: 19, 4: 2
table(complete$fear) # 0: 11048, 1: 2236, 2: 310, 3: 34, 4: 3
table(complete$joy) # 0: 10451, 1: 2716, 2: 411, 3: 41, 4: 12
table(complete$sadness) # 0: 11432, 1: 1978, 2: 202, 3: 15, 4: 4
table(complete$surprise) # 0: 11620, 1: 1806, 2: 191, 3: 13, 4: 1
table(complete$trust) # 0: 9671, 1: 3137, 2: 708, 3: 94, 4: 18, 5: 3
table(complete$negative) # 0: 9022, 1: 3583, 2: 858, 3: 134, 4: 25, 5: 9
table(complete$positive) # 0: 7975, 1: 4069, 2: 1210, 3: 318, 4: 56, 5: 3


# table sentiment count for each sentence
table(complete_sen$anger) # 0: 21066, 1: 2501, 2: 254, 3: 20, 4: 3
table(complete_sen$anticipation) # 0: 19927, 1: 3390, 2: 490, 3: 34, 4: 3
table(complete_sen$disgust) # 0: 21358, 1: 2328, 2: 143, 3: 14, 4: 1
table(complete_sen$fear) # 0: 21096, 1: 2498, 2: 233, 3: 14, 4: 3
table(complete_sen$joy) # 0: 20441, 1: 3042, 2: 333, 3: 22, 4: 6
table(complete_sen$sadness) # 0: 21530, 1: 2150, 2: 155, 3: 8, 4: 1
table(complete_sen$surprise) # 0: 21751, 1: 1924, 2: 159, 3: 9, 4: 1
table(complete_sen$trust) # 0: 19602, 1: 3568, 2: 592, 3: 69, 4: 11, 5: 2
table(complete_sen$negative) # 0: 18807, 1: 4258, 2: 676, 3: 87, 4: 12, 5: 4
table(complete_sen$positive) # 0: 17612, 1: 4917, 2: 1051, 3: 236, 4: 28

# descriptive analysis under sentiments

# negative sentiment
sencom <- complete %>%
  group_by(dataset) %>%
  filter(negative > 0)

table(sencom$dataset)

# benevolent: 302
# callme: 922
# hostile: 538
# other: 2642
# scales: 205

# anger emotions
angcom <- complete %>%
  group_by(dataset) %>%
  filter(anger > 0)

table(angcom$dataset)

# benevolent: 109
# callme: 534
# hostile: 324
# other: 1561
# scales: 90

# disgust emotions
discom <- complete %>%
  group_by(dataset) %>%
  filter(disgust > 0)

table(discom$dataset)

# benevolent: 139
# callme: 539
# hostile: 276
# other: 1341
# scales: 66

# fear emotions
fearcom <- complete %>%
  group_by(dataset) %>%
  filter(fear > 0)

table(fearcom$dataset)

# benevolent: 155
# callme: 520
# hostile: 321
# other: 1509
# scales: 78

# sadness
sadcom <- complete %>%
  group_by(dataset) %>%
  filter(sadness > 0)

table(sadcom$dataset)

# benevolent: 152
# callme: 441
# hostile: 265
# other: 1236
# scales: 105

# anticipation
anticipationcom <- complete %>%
  group_by(dataset) %>%
  filter(anticipation > 0)

table(anticipationcom$dataset)

# benevolent: 408
# callme: 630
# hostile: 311
# other: 2051
# scales: 279

# joy
joycom <- complete %>%
  group_by(dataset) %>%
  filter(joy > 0)

table(joycom$dataset)

# benevolent: 532
# callme: 529
# hostile: 269
# other: 1591
# scales: 259

# surprise
surprisecom <- complete %>%
  group_by(dataset) %>%
  filter(surprise > 0)

table(surprisecom$dataset)

# benevolent: 265
# callme: 304
# hostile: 179
# other: 1165
# scales: 98

# trust
trustcom <- complete %>%
  group_by(dataset) %>%
  filter(trust > 0)

table(trustcom$dataset)

# benevolent: 481
# callme: 751
# hostile: 380
# other: 2009
# scales: 339

# positive sentiment
poscom <- complete %>%
  group_by(dataset) %>%
  filter(positive > 0)

table(poscom$dataset)

# benevolent: 720
# callme: 1041
# hostile: 540
# other: 2872
# scales: 483

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

mean(nrc_vector) # 0.1399

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
simple_plot(nrc_vector, legend_pos = "bottomleft") # nrc_vector
simple_plot(syuzhet_vector, legend_pos = "bottomleft") # syuzhet_vector
simple_plot(afinn_vector, legend_pos = "bottomleft") # afinn_vector
simple_plot(bing_vector, legend_pos = "bottomleft") # bing_vector

# negative sentiment & sexist = FALSE
NF <- complete %>%
  filter(sexist == FALSE & negative > 0) # 4007 tweets
# positive sentiment & sexist = TRUE
PT <- complete %>%
  filter(sexist == TRUE & positive > 0) # 817 tweets
