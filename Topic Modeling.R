#Topic Modeling of Hotel Reviews



library(tm)
library(tokenizers)
library(textstem)
library(gutenbergr)
library(dplyr)
library(stringr) # package for dealing with strings
library(RColorBrewer)# package to get special theme color
library(wordcloud) # package to create wordcloud
library(topicmodels) # package for topic modelling
library(ggplot2) # basic data visualization
library(LDAvis) # LDA specific visualization 
library(servr) # interactive support for LDA visualization



reviews <- read.csv("HotelsData.csv", stringsAsFactors = FALSE, encoding = 'UTF-8')


set.seed(406)
reviews_sample <-sample_n(reviews, 2000)


sum(is.na(reviews_sample))

sum(nchar(reviews_sample$Text.1))


reviews_clean <- reviews_sample[reviews_sample$Review.score != 3, ]

library(cld2)
library(googleLanguageR)
library(tidyverse)
library(gtranslate)

# First detecting language of each entry and assigning it to a new column
lang <- detect_language(reviews_clean$Text.1)
reviews_clean$lang <- lang



which(is.na(lang))
# Counting the number of non english texts
non_en_count <- sum(lang != 'en', na.rm = TRUE)
non_en_count

reviews_translated <- reviews_clean



# Filter non-English text
logic <- reviews_clean$lang != 'en' | is.na(reviews_clean$lang)
non_english_indices <- which(logic)

# Translate and replace non-English text
for (i in non_english_indices) {
  reviews_translated[i, "Text.1"] <- translate(reviews_clean[i, "Text.1"],from = 'auto', to = 'en')
}

#Checking for sample translations:
reviews_clean[1, ]
reviews_translated[1, ]


# Spliting the reviews dataset to two positive and negative reviews and only keeping the text
positive_reviews <- reviews_translated[reviews_translated$Review.score>3,]$Text.1
negative_reviews <- reviews_translated[reviews_translated$Review.score<3,]$Text.1


length(negative_reviews)
length(positive_reviews)

Encoding(reviews_translated$Text.1)

positive_reviews <- stringr::str_conv(positive_reviews, "UTF-8")
negative_reviews <- stringr::str_conv(negative_reviews, "UTF-8")
Encoding(positive_reviews)
positive_reviews <- stri_encode(positive_reviews, "", "UTF-8")

# Cleaning texts and creating corpus:
stops <- c('hotel','good', 'stay', 'great', 'room', 'much', 'can', 'one', 'get')


clean_text <- function(text) {
  text <- tolower(text)  # convert to lower case
  text <- removePunctuation(text)  # remove punctuation
  text <- removeNumbers(text)  # remove all numbers
  text <- stripWhitespace(text)  # remove extra white spaces
  text <- lemmatize_strings(text)  # apply lemmatization
  text <- removeWords(text, c(stopwords("english"), stops))  # remove stopwords
  
  return(text)
}

library(stringi)
all(stri_enc_isutf8(positive_reviews))

# Apply cleaning function with lemmatization
positive_corpus <- Corpus(VectorSource(positive_reviews))
positive_corpus <- tm_map(positive_corpus, content_transformer(clean_text))

negative_corpus <- Corpus(VectorSource(negative_reviews))
negative_corpus <- tm_map(negative_corpus, content_transformer(clean_text))

length(positive_corpus)

print(positive_corpus[[1]]$content)


##############################################
# Creating document term matrix for LDA

# Document Term Matrix for positive reviews:
dtm_pos <- DocumentTermMatrix(positive_corpus)

raw.sum=apply(dtm_pos,1,FUN=sum)
dtm_pos=dtm_pos[raw.sum!=0,]


dtm_pos.new <- as.matrix(dtm_pos)
frequency_pos <- colSums(dtm_pos.new)
frequency_pos <- sort(frequency_pos, decreasing=TRUE)
doc_length_pos <- rowSums(dtm_pos.new)

frequency_pos[1:30] #Example of the output


words <- names(frequency_pos)# get back the word

wordcloud(words[1:100], frequency_pos[1:100], rot.per=0.10, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))



# Document Term Matrix for negative reviews:
dtm_neg <- DocumentTermMatrix(negative_corpus)

raw.sum=apply(dtm_neg,1,FUN=sum)
dtm_neg=dtm_neg[raw.sum!=0,]


dtm_neg.new <- as.matrix(dtm_neg)
frequency_neg <- colSums(dtm_neg.new)
frequency_neg <- sort(frequency_neg, decreasing=TRUE)
doc_length_neg <- rowSums(dtm_neg.new)

frequency_neg[1:10] #Example of the output


words <- names(frequency_neg)# get back the word

wordcloud(words[1:100], frequency_neg[1:100], rot.per=0.15, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))



#selecting  number of topics based on Arun2010, CaoJuan2009 and Griffiths2004 criteria
library(ldatuning)
result <- FindTopicsNumber(
  dtm_pos.new,
  topics = seq(from = 5, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)


result_neg <- FindTopicsNumber(
  dtm_neg.new,
  topics = seq(from = 5, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result_neg)


# Topic modeling of positive reviews:
ldaOut_pos <-LDA(dtm_pos,10, method="Gibbs", 
                 control=list(iter=5000,seed=1000))
phi <- posterior(ldaOut_pos)$terms %>% as.matrix 
#matrix, with each row containing the distribution over terms for a topic,
theta <- posterior(ldaOut_pos)$topics %>% as.matrix 
#matrix, with each row containing the probability distribution over topics for a document,


ldaOut_pos.terms <- as.matrix(terms(ldaOut_pos, 10))


ldaOut_pos.terms

# Write the matrix to an Excel file
library(openxlsx)
write.xlsx(as.data.frame(ldaOut_pos.terms), file = "LDA_Term_Matrix positive.xlsx",
           sheetName = "Positive", rowNames = TRUE)


# Topic modeling of negative reviews:
ldaOut_neg <-LDA(dtm_neg,11, method="Gibbs", 
                 control=list(iter=5000,seed=1000))
phi <- posterior(ldaOut_neg)$terms %>% as.matrix 
#matrix, with each row containing the distribution over terms for a topic,
theta <- posterior(ldaOut_neg)$topics %>% as.matrix 
#matrix, with each row containing the probability distribution over topics for a document,


ldaOut_neg.terms <- as.matrix(terms(ldaOut_neg, 10))


ldaOut_neg.terms
write.xlsx(as.data.frame(ldaOut_neg.terms), file = "LDA_Term_Matrix negative.xlsx",
           sheetName = "Negative", rowNames = TRUE)




# Grouping positive reviews by topics that were found:
positive_reviews <- data.frame(positive_reviews)
ldaOut_pos.topics <- data.frame(topics(ldaOut_pos))
ldaOut_pos.topics$index <- as.numeric(row.names(ldaOut_pos.topics))
positive_reviews$index <- as.numeric(row.names(positive_reviews))
datawithtopic_pos <- merge(positive_reviews, ldaOut_pos.topics, by='index',all.x=TRUE)
datawithtopic_pos <- datawithtopic_pos[order(datawithtopic_pos$index), ]

datawithtopic_pos[0:10,]

length(datawithtopic_pos[datawithtopic_pos["topics.ldaOut_pos."]==4])
################################################################################
# Grouping negative reviews by topics that were found:
negative_reviews <- data.frame(negative_reviews)
ldaOut_neg.topics <- data.frame(topics(ldaOut_neg))
ldaOut_neg.topics$index <- as.numeric(row.names(ldaOut_neg.topics))
negative_reviews$index <- as.numeric(row.names(negative_reviews))
datawithtopic_neg <- merge(negative_reviews, ldaOut_neg.topics, by='index',all.x=TRUE)
datawithtopic_neg <- datawithtopic_neg[order(datawithtopic_neg$index), ]


length(datawithtopic_neg[datawithtopic_neg["topics.ldaOut_neg."]==3])

datawithtopic_neg[datawithtopic_neg["topics.ldaOut_neg."]==4]
vocab <- colnames(phi) #vocab list in DTM

# create the JSON object to feed the visualization in LDAvis:
json_lda <- createJSON(phi = phi, theta = theta, 
                       vocab = vocab, doc.length = doc_length_pos, 
                       term.frequency = frequency_pos)


serVis(json_lda, out.dir = 'vis', open.browser = TRUE)