make_wordcloud <- function(airline, min_freq = 200){
  set.seed(333)
  textplot_wordcloud(airlines_data[[2]][[airline]], 
                     min_count = min_freq, 
                     random_order = FALSE,
                     rotation = .25, 
                     color = RColorBrewer::brewer.pal(8,"Dark2"))
}



get_sentiment <- function(airline) {
  if (airline == "All") return(airlines_data[[3]])
  else (airlines_data[[3]] %>% 
        filter(!!airline == airline) %>% 
        relocate(airline, created_at))
}



make_lineplot <- 
  function(data, emotion, 
           start_date = "2020-06-01", 
           end_date = "2020-7-31") {
  data %>% 
    mutate(created_at = as.Date(created_at)) %>% 
    filter(created_at >= as.Date(start_date) & created_at <= as.Date(end_date)) %>% 
    group_by(created_at) %>% 
      summarize({{emotion}} := mean({{emotion}}, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(x = created_at, y = {{emotion}})) +
    geom_line() +
    labs(x = "Date Tweet Created",
         caption = "Graph created using Ashwin Malshe's GenAI lesson.") +
    theme_minimal()
}
  
  
make_barplot <- function(){
  get_sentiment("All") %>% 
    group_by(airline) %>% 
      summarize(across(c(anger, anticipation,  disgust, fear, joy, sadness, surprise, trust,
                       positive, negative),
                ~ mean(.x, na.rm = TRUE), .names = "{col}")) %>% 
    ungroup() %>% 
    pivot_longer(cols = !starts_with("airline")) %>% 
    ggplot(aes(name, value)) +
    geom_col() +
    coord_flip() +
    facet_wrap(. ~airline) +
    theme_minimal() +
    labs(x = "Emotion", y = "% Frequency",
         caption = "Graph created using Ashwin Malshe's GenAI lesson.")
    
}
  

  

