install.packages("rvest")
install.packages("polite")
install.packages("dplyr")
install.packages("tokenizers")
install.packages("tm")
install.packages("textstem")
install.packages("stringr")
install.packages("hunspell")
install.packages("openxlsx")
install.packages("pbapply")

library(dplyr)
library(rvest)
library(polite)
library(tokenizers)
library(tm)
library(textstem)
library(stringr)
library(hunspell)
library(openxlsx)
library(pbapply)


url <- "https://en.prothomalo.com/"

page <- read_html(url)

Titles <- page %>%
  html_nodes("a.title-link span.tilte-no-link-parent") %>%
  html_text()

links <- page %>%
  html_nodes("a.title-link") %>%
  html_attr("href")

count <- 1
data <- data.frame(Title = character(), Article = character(), stringsAsFactors = FALSE)

for (i in seq_along(Titles)) {
  full_url <- links[i]
  
  article_page <- read_html(full_url)
  
  title <- article_page %>%
    html_nodes("h1.IiRps") %>%
    html_text() %>%
    .[1]
  
  article <- article_page %>%
    html_nodes("div.VzzDZ p") %>%
    html_text() %>%
    paste(collapse = " ")
  
  data <- rbind(data, data.frame(Title = title, Article = article, stringsAsFactors = FALSE))
  
  result <- paste(as.character(count), "out of", as.character(length(Titles)), '-', title)
  count <- count + 1
  print(result)
}



num_rows_with_empty <- sum(apply(data, 1, function(x) any(is.na(x) | x == "")))
print(num_rows_with_empty)

data <- data[!apply(data, 1, function(x) any(is.na(x) | x == "")), ]

write.xlsx(data, "data_original.xlsx")
write.csv(data, file = "C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Final/Project/raw_text_data.csv", row.names = FALSE)

data_no_emojis <- data %>%
  mutate(across(c(Article), ~ str_remove_all(., "[^\x01-\x7F]")))

ordinal_map <- c("first" = "one", "second" = "two", "third" = "three", 
                 "fourth" = "four", "fifth" = "five", "sixth" = "six", 
                 "seventh" = "seven", "eighth" = "eight", "ninth" = "nine", 
                 "tenth" = "ten", "1st" = "first", "2nd" = "second", "3rd" = "third")
data_sampled <- data_no_emojis %>%
  mutate(across(c(Article), ~ sapply(., function(x) {
    words <- unlist(strsplit(x, " "))  
    replaced_words <- sapply(words, function(word) {
      ifelse(word %in% names(ordinal_map), ordinal_map[word], word)  
    })
    paste(replaced_words, collapse = " ")  
  })))

data_cleaned <- data_sampled %>%
  mutate(
    Article = Article %>%
      tolower() %>%                             
      gsub("<[^>]+>", "", .) %>%                
      gsub("[[:punct:]]+", "", .) %>%           
      gsub("[0-9]", " ", .) %>%                  
      gsub("[^[:alnum:] ]", "", .) %>%        
      gsub("\\s+", " ", .) %>%                 
      trimws()                                  
  )

data_tokenized <- data_cleaned %>%
  mutate(across(c(Article), 
                ~ sapply(., function(x) {
                  tokens <- tokenize_words(x)  
                  tokens[[1]] 
                })))

data_stemmed_lemmetated <- data_tokenized %>%
  mutate(across(c(Article), 
                ~ sapply(., function(x) {
                  lemmatized_text <- lemmatize_words(x)
                  
                  return(lemmatized_text) 
                })))

stop_words <- tm::stopwords("en")

custom_stop_words <- c(
  'call', 'upon', 'still', 'nevertheless', 'down', 'every', 'forty', '‘re', 'always', 
  'whole', 'side', "n't", 'now', 'however', 'an', 'show', 'least', 'give', 'below', 
  'did', 'sometimes', 'which', "'s", 'nowhere', 'per', 'hereupon', 'yours', 'she', 
  'moreover', 'eight', 'somewhere', 'within', 'whereby', 'few', 'has', 'so', 'have', 
  'for', 'noone', 'top', 'were', 'those', 'thence', 'eleven', 'after', 'no', '’ll', 
  'others', 'ourselves', 'themselves', 'though', 'that', 'nor', 'just', '’s', 'before', 
  'had', 'toward', 'another', 'should', 'herself', 'and', 'these', 'such', 'elsewhere', 
  'further', 'next', 'indeed', 'bottom', 'anyone', 'his', 'each', 'then', 'both', 
  'became', 'third', 'whom', '‘ve', 'mine', 'take', 'many', 'anywhere', 'to', 'well', 
  'thereafter', 'besides', 'almost', 'front', 'fifteen', 'towards', 'none', 'be', 
  'herein', 'two', 'using', 'whatever', 'please', 'perhaps', 'full', 'ca', 'we', 
  'latterly', 'here', 'therefore', 'us', 'how', 'was', 'made', 'the', 'or', 'may', 
  '’re', 'namely', "'ve", 'anyway', 'amongst', 'used', 'ever', 'of', 'there', 'than', 
  'why', 'really', 'whither', 'in', 'only', 'wherein', 'last', 'under', 'own', 
  'therein', 'go', 'seems', '‘m', 'wherever', 'either', 'someone', 'up', 'doing', 
  'on', 'rather', 'ours', 'again', 'same', 'over', '‘s', 'latter', 'during', 'done', 
  "'re", 'put', "'m", 'much', 'neither', 'among', 'seemed', 'into', 'once', 'my', 
  'otherwise', 'part', 'everywhere', 'never', 'myself', 'must', 'will', 'am', 'can', 
  'else', 'although', 'as', 'beyond', 'are', 'too', 'becomes', 'does', 'a', 'everyone', 
  'but', 'some', 'regarding', '‘ll', 'against', 'throughout', 'yourselves', 'him', 
  "'d", 'it', 'himself', 'whether', 'move', '’m', 'hereafter', 're', 'while', 'whoever', 
  'your', 'first', 'amount', 'twelve', 'serious', 'other', 'any', 'off', 'seeming', 
  'four', 'itself', 'nothing', 'beforehand', 'make', 'out', 'very', 'already', 'various', 
  'until', 'hers', 'they', 'not', 'them', 'where', 'would', 'since', 'everything', 'at', 
  'together', 'yet', 'more', 'six', 'back', 'with', 'thereupon', 'becoming', 'around', 
  'due', 'keep', 'somehow', 'n‘t', 'across', 'all', 'when', 'i', 'empty', 'nine', 
  'five', 'get', 'see', 'been', 'name', 'between', 'hence', 'ten', 'several', 'from', 
  'whereupon', 'through', 'hereby', "'ll", 'alone', 'something', 'formerly', 'without', 
  'above', 'onto', 'except', 'enough', 'become', 'behind', '’d', 'its', 'most', 
  'n’t', 'might', 'whereas', 'anything', 'if', 'her', 'via', 'fifty', 'is', 'thereby', 
  'twenty', 'often', 'whereafter', 'their', 'also', 'anyhow', 'cannot', 'our', 'could', 
  'because', 'who', 'beside', 'by', 'whence', 'being', 'meanwhile', 'this', 'afterwards', 
  'whenever', 'mostly', 'what', 'one', 'nobody', 'seem', 'less', 'do', '‘d', 'say', 
  'thus', 'unless', 'along', 'yourself', 'former', 'thru', 'he', 'hundred', 'three', 
  'sixty', 'me', 'sometime', 'whose', 'you', 'quite', '’ve', 'about', 'even', 
  "th", "rd", "say", "one", "two", "three", "four", "five", "six", "seven", "eight", 
  "nine", "ten", "will", "can", "shall", "may", "would"
)

stop_words <- c(stop_words, custom_stop_words)

stop_words <- unique(stop_words)

print(stop_words)

data_no_stopwords <- data_stemmed_lemmetated %>%
  mutate(across(c(Article), 
                ~ sapply(., function(x) {
                  tokens_no_stopwords <- x[!x %in% stop_words]  
                  return(tokens_no_stopwords)  
                })))

data_processed <- data_no_stopwords %>%
  mutate(across(c(Article), 
                ~ sapply(., function(x) {
                  return(paste(unlist(x), collapse = ", "))  
                })))

num_rows_with_empty <- sum(apply(data_processed, 1, function(x) any(is.na(x) | x == "")))
print(num_rows_with_empty)

data_processed <- data_processed[!apply(data_processed, 1, function(x) any(is.na(x) | x == "")), ]

write.csv(data_processed, file = "C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Final/Project/process_text_data.csv", row.names = FALSE)


