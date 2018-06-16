

summary.data <- function(data){
  summary <- data %>% #data has to be as data$lexicon es.csv
  dplyr::count(sentiment, created, wt = log1p(retweet))%>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(n = (joy + trust + anger + fear + sadness + disgust + surprise),
         joy=(joy/n), trust=(trust/n), anger=(anger/n), fear=fear/n,surprise=surprise/n,
         
         sadness=sadness/n, disgust=disgust/n)%>%
  ungroup()
  return(summary)}
