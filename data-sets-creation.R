
tweet_corp <- corpus(air_tweets$text)
docvars(tweet_corp, "airline") <- air_tweets$airline
docvars(tweet_corp, "created_at") <- air_tweets$created_at
docvars(tweet_corp, "status_id") <- air_tweets$status_id
summary(tweet_corp, 10)

airlines <- table(air_tweets$airline) %>% rownames()

airline_dfm <- list()
for (i in 1:6) {
  airline_dfm[[i]] <-   corpus_subset(tweet_corp, airline == airlines[i]) %>% 
    dfm(remove = c(stopwords("english"), 
                   tweet_handles, "flight", "flights"), 
        stem = TRUE, 
        remove_punct = TRUE,
        remove_numbers = TRUE)
  
}

identical(airline_dfm[[1]], airline_dfm[[2]])

names(airline_dfm) <- airlines


make_wordcloud(airline = "Alaska")


airlines_data <- readRDS("airlines-data.rds")


sent_data <- quanteda.dictionaries::liwcalike(airlines_data[[1]],
                                 dictionary = data_dictionary_NRC) %>% 
  select(anger, anticipation,  disgust, fear, joy, sadness, surprise, trust,
         positive, negative, WPS, WC) %>% 
  cbind(air_tweets %>% select(created_at, airline, status_id))
  


saveRDS(list(tweet_corp, airline_dfm, sent_data), "airlines-data.rds")


names(sent_data)
