##############################################################################
## R script regarding the thesis Populism, dimensions and topics in the social media communication
## of Tomio Okamura by Dominika Betáková
## the script does not contain all preprocessing steps, but it is enough to build a topic model
## similiar as is in the thesis from the R objects provided in the archive


## all relevant files are saved as an R objects in the electronic archive


### First part of this script presents code for inspecting the hand-coded quantitative content analysis
## dataset saved as an excel file through simple tables
## 

## reading the data (the library readxl needs to be installed) and you would problably need to set the
## directory where you saved the file

library(readxl)

hand_coded_dataset <- read_excel("dataset_hand_coded__quantitative_content_analysis.xlsx")

View(hand_coded_dataset)

## getting the absolute values of populist strategies

absolute_results <- table(hand_coded_dataset$Populist_strategy_from_0_to_9)

absolute_results

## although the categories were evaluated based on their belonging to the three populist elements
## the coding rules can be found on the page 51 of the thesis

## getting the relative values

relative_values <- prop.table(absolute_results)

relative_values


## inspecting which type of elite was blamed the most
## 1 - domestic political actors
## 2 - other foreign political actors
## 3 - the EU
## 4 - the economic elites
## 5 - the Media
## 6 - the Courts or jurisdiction
## 7 - others versus us
## 8 - unspecified
## when the data is NA, the type of populist element was not conflictive

blame <- table(hand_coded_dataset$when_conflictive_who_is_blamed)

blame

## inspecting the re-used and original sentences
## 0 = original; 1 = copied; 2 - partly copied;


original <- table(hand_coded_dataset$Copied_1_yes_0_no_2_partly)

original


### Second part of this script presents code for constructing the LDA
## as is in the thesis - all needed data are saved as R files
##

## the data with Okamura's posts need to be read in the RStudio. The data are already lowercased,
## without the punctuation, diacritics
## the websites, lo

Okamura_posts <- readRDS(file="Okamura_posts")

## can be inspected with str, head or tail

head(Okamura_posts)


## these posts need to be tokenized, you would need the library quanteda

library(quanteda)

tokens_original <- tokens(Okamura_posts$message, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE,
                          remove_url=TRUE, ngrams=1)

## you need to read the dictonary with lemmas. The dictionary is already clean - there is no punctiation and 
## diacritics, the strings needed to be splitted and functions lapply and sapply were used to indicate which 
## column is to be replaced and which is the lemma (that replaces the word). You would need the library quanteda

library(quanteda)

czech_dict <- readRDS(file="Czech_dictionary")


## now the original tokens would be lemmatized


lemmatized_tokens <- tokens_lookup(x=tokens_original, dictionary=czech_dict,
                                   capkeys=FALSE, exclusive=FALSE)


## now the tokens are lemmatized - you can inspect the R object

str(lemmatized_tokens)


## reading in the document with stopwords. They are already lowercasted and without the
## punctuation and the duplicites were deleted

stopwords <- readRDS(file="Stopwords")


## now the document-term matrix can be created

document_term_matrix <- dfm(lemmatized_tokens, tolower=TRUE, stem=FALSE, remove_punct=TRUE,
                            remove=stopwords)


str(document_term_matrix)


## the document-term matrix should be trimmed to delete the most common and uncommon words

document_term_matrix_2 <- dfm_trim(document_term_matrix, min_count=1, min_docfreq = 6)

document_term_matrix_3 <- dfm_trim(document_term_matrix_2, min_count=1, max_docfreq = 563)


## now we can inspect the most occuring words

words <- colSums(document_term_matrix_3)

## needs to be sorted

words_decreasing <- sort(words, decreasing = TRUE)

## now you can inspect the most occuring words

words_decreasing


## constructing the topic model - the library stm is needed

library(stm)

## you need to convert the document-term matrix to stm format in order to try the Lee's and Mimno's
## algorithm

stm_format <- convert(x=document_term_matrix_3, to="stm")

## trying the algorithm by Lee and Mimno to define the number of topics - you will get
## the different result each time you try it
## it will take a while

algorithm <- searchK(documents=stm_format$documents, vocab=stm_format$vocab, K=0,
                     init.type="Spectral")


## you can plot the diagnostics for different number of topics
## it will again take some time

k_vector <- c(50:58)

k_top_result <- searchK(documents=stm_format$documents, vocab=stm_format$vocab, K=k_vector,
                        init.type="LDA")



## you can plot the results - it is very likely that you will get different results than are in the thesis
## it is slightly different each time

plot(k_top_result)

## constructing the topic model with 53 topics

topic_model_53 <- stm(document_term_matrix_3, K=53, init.type="LDA")

## again, it is possible that you will get different result than is in the thesis
## the models are different each time
## if you want to get the same one twice the argument seed needs to be used

## labeling the topics 

labelTopics(topic_model_53)


## viewing the top topics

windows(height=8, width=12)

## most used topics

plot.STM(x=topic_model_53, type="summary", labeltype="frex", ylim=c(30,53))

## least used topics

plot.STM(x=topic_model_53, type="summary", labeltype="frex", ylim=c(1,29))

## you can inspect the theta matrix of the topic model

topic_model_53$theta

## you can plot wordclouds - although they only shows the words with highest probabilities
## (not frex or lift)

## the wordcloud for topic 1

cloud(topic_model_53, topic=1)

## you can inspect the topic correlations

topic_model_cor <- topicCorr(topic_model_53)

topic_model_cor

plot(topic_model_cor)


## plotting the topics in time

## you can create a descending character vector. The Facebook posts in the document
## are descending - meaning the first is the newest one, therefore when plotted on the
## vector time, the newest document would be on the right side (corresponds to topic
## prevalence in time when read from left to right, the numbers correspond to dates
## as in the figures in the thesis)

time <- c(569:1)

## the time prevalence of topic 1

effect_topic_1 <- estimateEffect(c(1)~s(time), stmobj = topic_model_53,
                                 metadata = NULL, uncertainty = "Global")


plot_effect_topic_1 <- plot.estimateEffect(effect_topic_1,covariate="time",
                                           model=topic_model_53,topics=1,
                                           method="continuous")






