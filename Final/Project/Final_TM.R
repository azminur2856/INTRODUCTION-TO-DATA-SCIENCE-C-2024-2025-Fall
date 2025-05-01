
library(readr)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(topicmodels)


file_path <- "C:/Users/User/Downloads/data_csv.csv"
data <- read.csv(file_path, header = TRUE, sep = ",")


colnames(data)


clean_text <- function(text) {
  text %>%
    tolower() %>%                               
    str_replace_all("[^[:alnum:][:space:]]", "") %>%  
    str_replace_all("\\d+", "") %>%          
    removeWords(stopwords("en")) %>%          
    stripWhitespace()                            
}


data_cleaned <- data %>%
  mutate(cleaned_text = clean_text(Article))


head(data_cleaned)




corpus <- Corpus(VectorSource(data$Article))

if (inherits(corpus, "SimpleCorpus")) {
  corpus <- VCorpus(VectorSource(corpus))
}

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)                 
corpus <- tm_map(corpus, removeNumbers)                      
corpus <- tm_map(corpus, removeWords, stopwords("en"))       
corpus <- tm_map(corpus, stripWhitespace)                   
corpus <- tm_map(corpus, stemDocument)                       

text_data <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)


output_file <- "C:/Users/User/Downloads/corpus_data.csv"


write.csv(text_data, file = output_file, row.names = FALSE)










dtm <- DocumentTermMatrix(corpus)

print(dtm)
inspect(dtm[1:15, 1:15])  

dtm_sparse <- removeSparseTerms(dtm, 0.99)  


saveRDS(dtm, file = "C:/Users/User/Downloads/dtm.rds")
saveRDS(dtm_sparse, file = "C:/Users/User/Downloads/dtm_sparse.rds")




dtm_tfidf <- weightTfIdf(dtm)


normalize <- function(x) {
  return(x / sqrt(sum(x^2)))
}
dtm_tfidf_normalized <- as.DocumentTermMatrix(apply(as.matrix(dtm_tfidf), 1, normalize), weighting = weightTfIdf)

print(dtm_tfidf)
inspect(dtm_tfidf[1:15, 1:15])  

corpus_clean <- corpus[sapply(corpus, function(x) nchar(as.character(x)) > 0)]

dtm <- DocumentTermMatrix(corpus_clean)


if (any(rowSums(as.matrix(dtm_tfidf)) == 0)) {
  stop("The DTM contains empty rows. Please clean or filter the data.")
}


num_topics <- 8  
lda_model <- LDA(dtm, k = num_topics, method = "Gibbs", control = list(seed = 1234))


topics <- terms(lda_model, 10) 
print("Top terms for each topic:")
print(topics)

topic_proportions <- posterior(lda_model)$topics
print("Topic proportions for the first 5 documents:")
print(topic_proportions[1:5, ])


dominant_topics <- apply(topic_proportions, 1, which.max)

documents_with_topics <- data.frame(Document = 1:nrow(topic_proportions),
                                    Dominant_Topic = dominant_topics)




average_topic_proportions <- colMeans(topic_proportions)

print("Average topic proportions across all documents:")
print(average_topic_proportions)

library(ggplot2)
topic_labels <- paste0("Topic ", 1:length(average_topic_proportions))
avg_topic_df <- data.frame(Topic = topic_labels, Proportion = average_topic_proportions)

ggplot(avg_topic_df, aes(x = Topic, y = Proportion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Topic Proportions", x = "Topics", y = "Proportion") +
  theme_minimal()


data$topic <- documents_with_topics$Dominant_Topic

topic_names <- c(
  "Research and Development",
  "Politics and Policy",
  "Technology and Global Leadership",
  "Conflict and International Relations",
  "Finance and Infrastructure",
  "Economics and Financial Policies",
  "Environment and Urban Issues",
  "Governance and Administration"
)

data$topic_name <- factor(data$topic, levels = 1:8, labels = topic_names)

write_csv(data, "C:/Users/User/Downloads/final_text_data.csv")


