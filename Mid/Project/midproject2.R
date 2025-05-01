library(rvest)
library(stringr)
library(tm)
library(textstem)
library(hunspell)

url <- "https://www.bbc.com/travel/article"
page <- read_html(url)
raw_text <- page %>% html_nodes("p") %>% html_text()

print(raw)

process_text <- function(paragraph) {
  paragraph <- iconv(paragraph, from = "UTF-8", to = "ASCII//TRANSLIT")
  paragraph <- gsub("<[^>]*>", "", paragraph)
  paragraph <- gsub("[^[:alnum:] ]", "", paragraph)
  paragraph <- tolower(paragraph)
  
  tokens <- unlist(strsplit(paragraph, " "))
  
  tokens <- tokens[!grepl("^[0-9]+$", tokens)]
  
  stopwords <- stopwords("en")
  tokens <- tokens[!tokens %in% stopwords]
  
  tokens <- lemmatize_words(tokens)
  
  contraction_map <- list(
    "cant" = "cannot", "wont" = "will not", "whats" = "what is", "youre" = "you are",
    "isnt" = "is not", "arent" = "are not", "doesnt" = "does not", "didnt" = "did not",
    "dont" = "do not", "ive" = "i have", "youve" = "you have"
  )
  tokens <- sapply(tokens, function(word) {
    if (word %in% names(contraction_map)) {
      contraction_map[[word]]
    } else {
      word
    }
  })
  
  emoji_map <- list(
    ":)" = "smile", ":(" = "sad", ":D" = "laugh"
  )
  tokens <- sapply(tokens, function(word) {
    if (word %in% names(emoji_map)) {
      emoji_map[[word]]
    } else {
      word
    }
  })
  
  tokens <- sapply(tokens, function(word) {
    if (!hunspell_check(word)) {
      suggestions <- hunspell_suggest(word)[[1]]
      if (length(suggestions) > 0) {
        suggestions[1]
      } else {
        word
      }
    } else {
      word
    }
  })
  
  cleaned_paragraph <- paste(tokens, collapse = " ")
  cleaned_paragraph <- gsub("\\s+", " ", cleaned_paragraph)
  str_trim(cleaned_paragraph)
}

processed_paragraphs <- sapply(raw_text, process_text)

raw_data_frame <- data.frame(OriginalText = raw_text)
processed_data_frame <- data.frame(ProcessedText = processed_paragraphs)
combined_data_frame <- data.frame(
  OriginalText = raw_text,
  ProcessedText = processed_paragraphs
)

write.csv(raw_data_frame, file = "C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Mid/Lab/Project/raw_text_data.csv", row.names = FALSE)
write.csv(processed_data_frame, file = "C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Mid/Lab/Project/processed_text_data.csv", row.names = FALSE)
write.csv(combined_data_frame, file = "C:/Users/AZMINUR RAHMAN/OneDrive - American International University-Bangladesh/2024-2025, Fall/INTRODUCTION TO DATA SCIENCE [C]/Mid/Lab/Project/combind_text_data.csv", row.names = FALSE)

print(head(raw_data_frame))
print(head(processed_data_frame))
print(head(combined_data_frame))