# Initialization

**Loading Packages**
```{r loading packages}
library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(tidyverse)
library(stringr)
library(SnowballC)


#sb_20 <- read.csv("flat_sb_2020.csv", stringsAsFactors = FALSE)
sb_20_red <- read.csv("2020-dataset-reduced-columns.csv", stringsAsFactors = FALSE)

```


**Creating Functions for Plotting**\
*Wordcloud*
```{r wordcloud}

create_wordcloud <- function(x)
  {
    wordcloud_corpus <- VCorpus(VectorSource(x))
    
    wordcloud_corpus <- tm_map(wordcloud_corpus, removeNumbers) # remove numbers
    
    
    # create a Term Document Matrix.
    dtm <- TermDocumentMatrix(wordcloud_corpus,control = list(weighting = weightTfIdf))
    
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 30,
              max.words=100, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"),scale=c(3, 0.7))
  }

```

*Topic Model*
```{r topicmodel}

create_topicmodel <- function(x, beschriftung)
  {
    corpus <- VCorpus(VectorSource(x))
    corpus <- tm_map(corpus, removeNumbers) # remove numbers
    corpus <- tm_map(corpus, removePunctuation) # remove punctuations
    
  
    # now we are Creating a Document Term Matrix.
    dtm <- DocumentTermMatrix(corpus)
    rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
    
    # remove dtm rows with no words i.e., tweets that have no words
    # after preprocessing text.
    dtm <- dtm[rowTotals> 0, ]
    
    set.seed(234)
    lda <- LDA(dtm, k = 4, method = "Gibbs", control = NULL)
    
    # the method used for fitting can be either "VEM" or "Gibbs"
    topics <- tidy(lda, matrix = "beta") # beta is the topic-word density
    
    top_terms <- topics %>%
      group_by(topic) %>%
      top_n(10, beta) %>% # top_n picks 10 topics.
      ungroup() %>%
      arrange(topic, -beta)
    
    top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip()+
      labs(title=beschriftung,
        y ="Topic-Word Density",
        x = "Word")
  }

```


# Analysis

## Data Cleaning
**Filter for brand name**
```{r filtering for brands}
#head(sb_20)
#str(sb_20)

google <- sb_20_red %>% 
  filter(Brand == "Google") %>% 
  select(text, AdTitle, Brand, sentiment, retweet_count) #%>% 

```

**Text Cleaning**
```{r data cleaning}

# regex for parsing tweets
replace_reg <- "https?://[^\\s]+|&amp;|&lt;|&gt;|\bRT\\b|[^[:ascii:]]"

# split into words
words <- google %>%
  filter(retweet_count == 0, ifelse(startsWith(text,"RT") == TRUE, FALSE, TRUE)) %>% # removes ca 10,000 tweets
  mutate(text = str_replace_all(text, replace_reg, ""),
         linenumber = row_number()) %>%
  unnest_tokens(word, text, token = "tweets")


# remove stop words
words <- words %>%
  anti_join(stop_words, by = "word")


```

**Text separation**
```{r data separation}

# selection mentions
mentions <- words  %>%
  filter(grepl("@",word) & word != "@") %>% 
  # clean possessives
  mutate(word = gsub("'s","", word)) %>%
  count(word) %>%
  arrange(-n)

# selection hashtags
hashtags <- words  %>%
  filter(grepl("#",word) & word != "#") %>% 
  # clean possessives
  mutate(word = gsub("'s","", word)) %>%
  count(word) %>%
  arrange(-n)

# removing hashtags and mentions from data set
words_clean <- words %>% 
  filter(ifelse(startsWith(word,"@") == TRUE, FALSE, TRUE), ifelse(startsWith(word,"#") == TRUE, FALSE, TRUE))
```

```{r}
# stemming the words
system.time(
  words_clean_stemmed <-words_clean %>%
    mutate(word_stem = wordStem(word, language="english"))
)
words_clean_stemmed <- words_clean_stemmed %>% 
  select(-word) %>% 
  rename(word = word_stem)

# Merging single words back to tweet
tidy_tweets <- words_clean %>%
    group_by(linenumber) %>% 
    summarize(text = str_c(word, collapse = " ")) %>%
    ungroup()

# Merging single stemmed words back to tweet
tidy_tweets_stemmed <- words_clean_stemmed %>%
    group_by(linenumber) %>% 
    summarize(text = str_c(word, collapse = " ")) %>%
    ungroup()


# Creating bigrams
# split stemmed tweets into word pairs
bigrams <- tidy_tweets_stemmed %>% 
  #filter(retweet_count == 0, ifelse(startsWith(text,"RT") == TRUE, FALSE, TRUE)) %>% # removes ca 10,000 tweets
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# remove stop words
bigrams <- bigrams %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") &
         str_detect(second, "[a-z]"))

```

**Aggregating words and bigrams**
```{r aggregating words and bigrams}

words_count <- words_clean_stemmed %>%
   group_by(word) %>%
   count()

google_words_arranged <- words_count %>%
   arrange(-n)

bigrams_count <- bigrams %>%
   group_by(bigram) %>%
   count()

google_bigrams_arranged <- bigrams_count %>%
   arrange(-n)

```


## Plotting


### Barplot Top 10 Mentions
```{r mentions}

excluded_mentions <- c("@google")
mentions %>% 
  filter(ifelse(word %in% excluded_mentions == TRUE, FALSE, TRUE))  %>%
  top_n(10) %>% 
  ggplot(aes(reorder(word,n), n, fill = word)) +
  geom_col()+
  coord_flip()+
  labs(title="Top 10 Mentioned Accounts; Brand: Google",
        x ="Mentioned Accounts", y = "Number of Occurence")+
  theme(legend.position = "none") + 
  theme_minimal()

```
![mentsions_google](https://user-images.githubusercontent.com/63118478/83046959-f80aeb80-a004-11ea-88d3-d5996124a9a8.png)

### Barplot Top 10 Hashtags
```{r hashtags}

excluded_hashtags <- c("google|commercial|ad|im|superbowl|sbliv")
hashtags %>% 
  filter(ifelse(str_detect(word, excluded_hashtags) == TRUE, FALSE, TRUE))  %>%
  top_n(10) %>% 
  ggplot(aes(reorder(word,n), n, fill = word)) +
  geom_col()+
  coord_flip()+
  labs(title="Top 10 Used Hashtags; Brand: Google",
        x ="Hashtags", y = "Number of Occurence")+
  theme(legend.position = "none") + 
  theme_minimal()

```
![hashtags_google](https://user-images.githubusercontent.com/63118478/83046954-f7725500-a004-11ea-9cbb-2aad2d1cfd59.png)

### Barplot Top 10 Words
```{r words}

excluded_words <- c("googl|commercial|ad|im|(super bowl)|bowl|super|commerci")
google_words_arranged %>% 
  filter(ifelse(str_detect(word, excluded_words) == TRUE, FALSE, TRUE)) %>%
  head(n=10L) %>% 
  ggplot(aes(reorder(word,n), n, fill=word)) +
  geom_col()+
  coord_flip() +
  labs(title="Top 10 Tweeted Words; Brand: Google",
        x ="Word", y = "Number of Occurence")+
  theme(legend.position = "none") + 
  theme_minimal()

```
![words_google](https://user-images.githubusercontent.com/63118478/83046970-f9d4af00-a004-11ea-831c-26dcb00be510.png)

### Barplot Top 10 Bigrams
```{r bigrams}

google_bigrams_arranged %>%
  filter(ifelse(str_detect(bigram, excluded_words) == TRUE, FALSE, TRUE)) %>% 
  head(n=10L) %>% 
  ggplot(aes(reorder(bigram,n), n,fill =bigram)) +
  geom_col()+
  coord_flip() +
  labs(title="Top 10 Tweeted Word Pairs; Brand: Google",
        x ="Word Pairs", y = "Number of Occurence")+
  theme(legend.position = "none") + 
  theme_minimal()

```
![word-pairs_google](https://user-images.githubusercontent.com/63118478/83046968-f93c1880-a004-11ea-9090-22e96fb9cc51.png)

### Barplot Top 10 Words without 'Loretta'
```{r words_wo_loretta}

excluded_words <- c("googl|commercial|ad|im|(super bowl)|bowl|super|commerci|loretta")
google_words_arranged %>% 
  filter(ifelse(str_detect(word, excluded_words) == TRUE, FALSE, TRUE)) %>%
  head(n=10L) %>% 
  ggplot(aes(reorder(word,n), n, fill=word)) +
  geom_col()+
  coord_flip() +
  labs(title="Top 10 Tweeted Words without 'loretta'; Brand: Google",
        x ="Word", y = "Number of Occurence")+
  theme(legend.position = "none") + 
  theme_minimal()

```
![words_google_without-loretta](https://user-images.githubusercontent.com/63118478/83046972-f9d4af00-a004-11ea-8be0-396d61c36967.png)


### Wordcloud
```{r wordcloud}
wordcloud_data <- tidy_tweets_stemmed %>% 
    filter(ifelse(str_detect(text, excluded_words) == TRUE, FALSE, TRUE))

create_wordcloud(wordcloud_data)

```
![wordcloud_google](https://user-images.githubusercontent.com/63118478/83046964-f8a38200-a004-11ea-96fd-83180702ed00.png)

### Topic Model
```{r topic_model}
tidy_tweets_stemmed %>% 
    filter(ifelse(str_detect(text, excluded_words) == TRUE, FALSE, TRUE)) %>% 
  create_topicmodel(beschriftung="Topic Model; Brand: Google") + 
  theme_minimal()

```
![topicmodel_google](https://user-images.githubusercontent.com/63118478/83046962-f8a38200-a004-11ea-8351-08b47ec15bad.png)

