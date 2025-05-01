# Load necessary libraries
library(tm)
library(topicmodels)
library(LDAvis)
library(readr)

# Step 1: Load the data
data <- read_csv("/mnt/data/process_text_data.csv")

# Step 2: Preprocess the text data
corpus <- Corpus(VectorSource(data$Article))

corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)           # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)               # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove stopwords
corpus <- tm_map(corpus, stripWhitespace)             # Remove extra whitespace

# Step 3: Create Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Step 4: Topic Modeling with LDA
control_params <- list(iter = 5000, burnin = 3000, thin = 3, verbose = 2)
num_topics <- 10
lda_model <- LDA(dtm, k = num_topics, method = "Gibbs", control = control_params)

# Step 5.1: Get the most probable words for each topic
phi <- posterior(lda_model)$terms  # Topic-term distributions
if (!is.null(phi)) {
  phi <- as.matrix(phi)
  top_terms <- terms(lda_model, 10) # Top 10 terms for each topic
  print("Top terms for each topic:")
  print(top_terms)
} else {
  print("Error: Unable to extract topic-term distributions.")
}

# Step 5.2: Get the topic proportions for each document
theta <- posterior(lda_model)$topics
if (!is.null(theta)) {
  theta <- as.matrix(theta)
} else {
  print("Error: Unable to extract topic proportions.")
}

# Step 6: Calculate coherence
# Function to calculate coherence
CalcProbCoherence <- function(phi, dtm, M) {
  coherence <- numeric(nrow(phi))
  for (topic in 1:nrow(phi)) {
    terms <- order(phi[topic, ], decreasing = TRUE)[1:M]
    coherence[topic] <- mean(rowSums(dtm[, terms]))
  }
  return(coherence)
}

if (exists("phi") && exists("dtm")) {
  dtm_matrix <- as.matrix(dtm)                 # Convert DTM to a matrix
  coherence <- CalcProbCoherence(phi = phi, dtm = dtm_matrix, M = 5)
  average_coherence <- mean(coherence)
  print("Coherence scores for each topic:")
  print(coherence)
  print(paste("Average coherence score:", round(average_coherence, 4)))
} else {
  print("Error: Unable to calculate coherence scores due to missing phi or DTM.")
}

# Visualize Term Probabilities
if (exists("dtm_matrix")) {
  term_frequency <- colSums(dtm_matrix)
  vocabulary <- colnames(dtm_matrix)
  
  data_for_plot <- data.frame(Term = names(term_frequency), Probability = term_frequency)
  data_for_plot <- data_for_plot[order(-data_for_plot$Probability), ][1:20, ]
  
  # Plot term frequencies
  library(ggplot2)
  ggplot(data_for_plot, aes(x = reorder(Term, -Probability), y = Probability)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Top 20 Terms by Frequency", x = "Terms", y = "Frequency")
} else {
  print("Error: Unable to visualize term probabilities due to missing DTM matrix.")
}

# Step 7: LDA Visualization
if (exists("phi") && exists("theta") && exists("vocabulary") && exists("term_frequency")) {
  json_lda <- createJSON(
    phi = phi, 
    theta = theta, 
    doc.length = rowSums(dtm_matrix), 
    vocab = vocabulary, 
    term.frequency = term_frequency
  )
  
  serVis(json_lda, out.dir = "lda_vis", open.browser = TRUE)
} else {
  print("Error: Unable to create LDA visualization due to missing required components.")
}

