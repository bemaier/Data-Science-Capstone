# Install required packages

if(!require(quanteda)) install.packages("quanteda")
if(!require(quanteda.textmodels)) install.packages("quanteda.textmodels")
if(!require(tm)) install.packages("tm")
if(!require(SnowballC)) install.packages("SnowballC")
if(!require(wordcloud)) install.packages("wordcloud")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(caret)) install.packages("caret")
if(!require(ggfortify)) install.packages("ggfortify")
if(!require(stringr)) install.packages("stringr")
if(!require(readr)) install.packages("readr")
if(!require(readxl)) install.packages("readxl")
if(!require(matrixStats)) install.packages("matrixStats")
if(!require(pamr)) install.packages("pamr")
if(!require(tinytex)) install.packages("tinytex")

# Load required libraries
library(quanteda)
library(quanteda.textmodels)
library(tm)
library(ggplot2)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(caret)
library(ggfortify)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(matrixStats)
library(pamr)
library(tinytex)

# Load the speeches from the directory and create a data frame

collection <- list.files("capstone_project")
speeches <- list()
for(file in collection){
  speeches[[file]] <- read_file(paste("capstone_project/",file,sep=""))
}



#Extract the speeches
speechtable <- c()
speechtable <- sapply(1:length(speeches), function(x)
{
  append(speechtable,speeches[[x]])
})

# Extract the presidents
presidents <- sapply(1:length(collection), function(x){
  gsub("_.*", "", collection[x])
})

# Remove Debates and the info text
data <- data.frame(presidents = presidents, text = speechtable)
data <- data %>% filter(!str_detect(data$text, pattern = "title=\\\"Debate with"))

data <- data %>% mutate(text = gsub(".*\">", "", text))

dim(data)
length(unique(data$presidents))

################################################################################
########################### Data Exploration ###################################

# Copy data for use of exploration
exp_data <- data

# Count how many words there are in each speech
exp_data <- exp_data %>% mutate(word_count = sapply(strsplit(exp_data$text, " "), length))

# How many speeches did each president give
prsum <- exp_data %>% 
  group_by(presidents) %>% 
  summarize(speeches = n(), 
            words = sum(word_count), 
            s_length = sum(word_count)/n())

prsum %>% ggplot(aes(x=reorder(presidents, speeches),y=speeches)) + 
  geom_bar(stat = "identity", color = "white", fill = "blue") + 
  coord_flip() +
  labs(y = "Speeches per US-President",
       x = "US-President") 

# Remove presidents with fewer than 10 speeches

filter <- prsum %>% mutate(filt = speeches >= 10)
filter <- filter %>% select(presidents, filt)
exp_data <- left_join(exp_data,filter, by = "presidents")
exp_data <- exp_data %>% filter(filt == TRUE)
exp_data <- exp_data %>% select(presidents, text)
exp_data$presidents <- factor(exp_data$presidents)


#-------------------------------------------------------------------------------
### Base-line setting with Quanteda

# Create quanteda corpus
speechcorpus <- corpus(exp_data, text_field = "text")
docvars(speechcorpus,"id") <- 1:ndoc(speechcorpus)

# Create training and test data
set.seed(1337, sample.kind = "Rounding")
index <- createDataPartition(exp_data$presidents, times = 1, p = 0.7)

train_corpus <- 
  corpus_subset(speechcorpus, !id %in% index$Resample1) %>%
  dfm(stem = TRUE)

test_corpus <- 
  corpus_subset(speechcorpus, id %in% index$Resample1) %>%
  dfm(stem = TRUE)

test_corpus <-   dfm_match(test_corpus, features = featnames(train_corpus))



# -----------------------------------------------------------------------------
# SVM model
speechmodel <- textmodel_svm(train_corpus, docvars(train_corpus, "presidents"))
selffit1 <- predict(speechmodel, train_corpus)
confusionMatrix(table(docvars(train_corpus, "presidents"), selffit1))$overall["Accuracy"]

fit <- predict(speechmodel, test_corpus)

# Plot balanced accuracy for each President
result_svm <- confusionMatrix(table(docvars(test_corpus, "presidents"), fit))
bal_acc_svm <- data.frame(president = names(result_svm$byClass[,"Balanced Accuracy"]), 
                          "balanced accuracy" = result_svm$byClass[,"Balanced Accuracy"])
bal_acc_svm <- bal_acc_svm %>% mutate(president = sapply(1:nrow(bal_acc_svm), function(x) {
  (gsub(".*: ", "", bal_acc_svm$president[x]))
}))
bal_acc_svm %>% ggplot(aes(y = reorder(president, balanced.accuracy), 
                           x = balanced.accuracy)) + geom_col()+ 
  geom_col(color = "white", fill = "blue") +
  labs(x = "Balanced Accuracy", 
       y = "",
       title = "Quanteda Model")
result_svm$overall["Accuracy"]
mean(bal_acc_svm$balanced.accuracy, na.rm = T)

model_performance <- data.frame(quanteda = result_svm$overall["Accuracy"])

################################################################################
############################################################### Data Exploration

# How many speeches did each president give?
prsum %>% ggplot(aes(x=reorder(presidents, speeches),y=speeches)) + 
  geom_bar(stat = "identity", color = "white", fill = "blue") + 
  coord_flip() +
  labs(y = "Speeches per US-President",
       x = "US-President") 

# How many words did he speak?
prsum %>% ggplot(aes(x=reorder(presidents, words),y=words)) + 
  geom_bar(stat = "identity", color = "white", fill = "blue") + 
  coord_flip() +
  labs(y = "Words per US-President",
       x = "US-President")

# How many words were there per speech?
prsum %>% ggplot(aes(x=reorder(presidents, s_length),y= s_length)) + 
  geom_bar(stat = "identity", color = "white", fill = "blue") + 
  coord_flip() +
  labs(y = "Mean speech length in words",
       x = "US-President")
max(prsum$s_length)
min(prsum$s_length)


### Textmining with library tm

# Create a corpus of words from the strings of speeches
corp_data <- Corpus(VectorSource(exp_data$text))

# Convert the text to lower case
corp_data <- tm_map(corp_data, content_transformer(tolower))

# Remove stopwords (will take a while)
corp_data <- tm_map(corp_data, removeWords, stopwords("english"))

# Remove punctuations
corp_data <- tm_map(corp_data, removePunctuation)

# Remove numbers
corp_data <- tm_map(corp_data, removeNumbers)

# Remove white spaces
corp_data <- tm_map(corp_data, stripWhitespace)

# Remove other common words not in stopwords and "applause" and "laughter" from transcripts
corp_data <- tm_map(corp_data, removeWords, c("the", "will","applause", "laughter" ,"can", "if", "else", "for"))

# Build a term matrix of all words said by all presidents with their frequencies
term_matrix <- TermDocumentMatrix(corp_data)
term_matrix <- as.matrix(term_matrix)
colnames(term_matrix) <- exp_data$presidents

#-------------------------------------------------------------------------------
### Word cloud examination with library wordcloud

# Subdivide matrix by presidents, select representative ones
# We take presidents with famous hallmark achievements or problems

# Woodrow Wilson - Pacifist who nonetheless had to fight WW1 
wilson <- term_matrix[,colnames(term_matrix) == "wilson"]

# Only president to serve more than 2 terms. Led the US during WW2
fdroosevelt <- term_matrix[,colnames(term_matrix) == "fdroosevelt"]

# Most recent president in the data set. Health insurance
obama <- term_matrix[,colnames(term_matrix) == "obama"]

# Civil war president. Ended slavery. 
lincoln <- term_matrix[,colnames(term_matrix) == "lincoln"]

# Good ol Teddy. Slightly crazy. Got shot and shrugged it of. Also Panama canal
roosevelt <- term_matrix[,colnames(term_matrix) == "roosevelt"]

# The foundiest of founding fathers
washington <- term_matrix[,colnames(term_matrix) == "washington"]

# Economic reforms. Hated goverment spending. Did pretty well with the cold war.
reagan <- term_matrix[,colnames(term_matrix) == "reagan"]

#########################
#########################
# Create the clouds 

# Cloud for Woodrow Wilson
ww <- sort(rowSums(wilson),decreasing=TRUE)
wilsonc <- data.frame(word = names(ww),freq=ww)

wordcloud(words = wilsonc$word, freq = wilsonc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Cloud for Teddy Roosevelt
tr <- sort(rowSums(roosevelt),decreasing=TRUE)
teddyc <- data.frame(word = names(tr),freq=tr)

wordcloud(words = teddyc$word, freq = teddyc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2") )

# Cloud for George Washington
gw <- sort(rowSums(washington),decreasing=TRUE)
washingtonc <- data.frame(word = names(gw),freq=gw)

wordcloud(words = washingtonc$word, freq = washingtonc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Cloud for Ronald Reagan 
rr <- sort(rowSums(reagan),decreasing=TRUE)
reaganc <- data.frame(word = names(rr),freq=rr)

wordcloud(words = reaganc$word, freq = reaganc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Cloud for Barack Obama
bo <- sort(rowSums(obama),decreasing=TRUE)
obamac <- data.frame(word = names(bo),freq=bo)

wordcloud(words = obamac$word, freq = obamac$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Cloud for Abraham Lincoln
al <- sort(rowSums(lincoln),decreasing=TRUE)
lincolnc <- data.frame(word = names(al),freq=al)

wordcloud(words = lincolnc$word, freq = lincolnc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Cloud for FDR
fdr <- sort(rowSums(fdroosevelt),decreasing=TRUE)
fdrc <- data.frame(word = names(fdr),freq=fdr)

wordcloud(words = fdrc$word, freq = fdrc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



rm(ww, dde, tr, gw, rr, bo, al, fdr, wilsonc, 
   teddyc, washingtonc, fdrc, obamac, lincolnc,
   wilson, roosevelt, eisenhower, lincoln, washington,
   fdroosevelt, reagan, obama)

#--------------------------------------------------------------------------------
### Word frequency analysis

# favorite words per president
presnames <- unique(exp_data$presidents)
pres_pref <- sapply(1:length(presnames), function(x) {
  namef <- colnames(term_matrix) == presnames[x]
  rowSums(term_matrix[,namef])/sum(namef)
})
colnames(pres_pref) <- presnames

# Show the "political stop words"
psw <- c("united","states","upon","government","must","will","people", "country")
swfilter <- rownames(pres_pref) %in% psw
par(mar=c(6,6,4,3)+.1)
t(pres_pref[swfilter,]) %>% barplot(col=rainbow(ncol(pres_pref)),xlim=c(0, ncol(pres_pref)*0.4), las = 2)
legend("topright", legend = colnames(pres_pref), 
       ncol = 2,
       cex = 0.6,
       fill = rainbow(ncol(pres_pref)),
       col=rainbow(ncol(pres_pref)))
"'test'"


# Calculate the frequency of words 
word_frequency <- data.frame(words = rownames(term_matrix), 
                             count = rowSums(term_matrix))

word_frequency %>% ggplot(aes(x=count)) + 
  geom_histogram(bins = 100, fill = "blue", color = "white") + 
  xlim(0,7500) + 
  scale_y_continuous(trans="log10")+
  labs(x = "Number of times words appear in presidential speeches",
       y = "Number of words that appear x-times")

# What percentage of words is used fewer than 10 times or more than 1000x
sum(word_frequency$count < 10) / nrow(term_matrix) * 100
sum(word_frequency$count > 1000) / nrow(term_matrix) * 100

# What percentage of total spoken words are the most frequent ones?
frequent <- word_frequency %>% filter(count > 1000) %>% summarize(count = sum(count)) 
all <- word_frequency %>% summarize(count = sum(count))
frequent/all

# Plot the most frequent words
word_frequency[order(word_frequency$count, decreasing = TRUE),] %>% 
  top_n(n=30, wt = count) %>% 
  ggplot(aes(y=reorder(words, count), x=count)) + 
  geom_col(fill="blue", color="white") +
  labs(x = "Word frequency", y = "Most common words")


# How many words did each president use?
verbosity <- data.frame(presidents = colnames(pres_pref), 
                        words= colSums(pres_pref > 0))
verbosity %>% ggplot(aes(x=words,
                         y = reorder(presidents, words))) + 
  geom_col(color = "white", fill="blue") +
  labs(x = "vocabulary",
       y = "US-Presidents")

# Look at example key-words that might enable an algorithm to differentiate between
# US- presidents

# Who had to content with the Germans?
germany <- data.frame(presidents = colnames(pres_pref), 
                      count = pres_pref["germany",])
germany %>% ggplot(aes(x=count, y= reorder(presidents,count))) + 
  geom_bar(stat="identity", fill = "blue", color = "white") +
  labs(x= "Use cases per speech", y = "US-Presidents")

# Who used outdated, probably racist terms?
indian <- data.frame(presidents = colnames(pres_pref), 
                     count = pres_pref["indian",]) 
indian %>% ggplot(aes(x=count, y= reorder(presidents,count))) + 
  geom_bar(stat="identity", fill = "blue", color = "white") +
  labs(x= "Use cases per speech", y = "US-Presidents")

# For whom was the threat red?
communism <- data.frame(presidents = colnames(pres_pref), 
                        count = pres_pref["communism",]) 
communism %>% ggplot(aes(x=count, y= reorder(presidents,count))) + 
  geom_bar(stat="identity", fill = "blue", color = "white") +
  labs(x= "Use cases per speech", y = "US-Presidents")

# Removing most frequent and least frequent words should clean the data 
wordfilter <- word_frequency$count < 1000 & word_frequency$count > 10
lftm <- term_matrix[wordfilter,]
dim(term_matrix)
dim(lftm)



# Let's check with the presidents again. This time we also include Kennedy.
wilson <- lftm[,colnames(lftm) == "wilson"]
eisenhower <- lftm[,colnames(lftm) == "eisenhower"]
fdroosevelt <- lftm[,colnames(lftm) == "fdroosevelt"]
obama <- lftm[,colnames(lftm) == "obama"]
lincoln <- lftm[,colnames(lftm) == "lincoln"]
roosevelt <- lftm[,colnames(lftm) == "roosevelt"]
washington <- lftm[,colnames(lftm) == "washington"]
reagan <- lftm[,colnames(lftm) == "reagan"]
kennedy <- lftm[,colnames(lftm) == "kennedy"]

#########################
#########################
# Create the new clouds 

# Cloud for Woodrow Wilson
ww <- sort(rowSums(wilson),decreasing=TRUE)
wilsonc <- data.frame(word = names(ww),freq=ww)

wordcloud(words = wilsonc$word, freq = wilsonc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3,0.05),
          colors=brewer.pal(8, "Dark2"))


# Cloud for Teddy Roosevelt
tr <- sort(rowSums(roosevelt),decreasing=TRUE)
teddyc <- data.frame(word = names(tr),freq=tr)

wordcloud(words = teddyc$word, freq = teddyc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2") )

# Cloud for George Washington
gw <- sort(rowSums(washington),decreasing=TRUE)
washingtonc <- data.frame(word = names(gw),freq=gw)

wordcloud(words = washingtonc$word, freq = washingtonc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,  scale=c(3,0.01),
          colors=brewer.pal(8, "Dark2"))

# Cloud for Ronald Reagan 
rr <- sort(rowSums(reagan),decreasing=TRUE)
reaganc <- data.frame(word = names(rr),freq=rr)

wordcloud(words = reaganc$word, freq = reaganc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Cloud for Barack Obama
bo <- sort(rowSums(obama),decreasing=TRUE)
obamac <- data.frame(word = names(bo),freq=bo)

wordcloud(words = obamac$word, freq = obamac$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3,0.2),
          colors=brewer.pal(8, "Dark2"))

# Cloud for Abraham Lincoln
al <- sort(rowSums(lincoln),decreasing=TRUE)
lincolnc <- data.frame(word = names(al),freq=al)

wordcloud(words = lincolnc$word, freq = lincolnc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Cloud for FDR
fdr <- sort(rowSums(fdroosevelt),decreasing=TRUE)
fdrc <- data.frame(word = names(fdr),freq=fdr)

wordcloud(words = fdrc$word, freq = fdrc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3,0.01),
          colors=brewer.pal(8, "Dark2"))

# Cloud for JFK
jfk <- sort(rowSums(kennedy),decreasing=TRUE)
jfkc <- data.frame(word = names(jfk),freq=jfk)

wordcloud(words = jfkc$word, freq = jfkc$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3,0.1),
          colors=brewer.pal(8, "Dark2"))

rm(ww, dde, tr, gw, rr, bo, al, fdr, wilsonc, 
   teddyc, washingtonc, fdrc, obamac, lincolnc,
   wilson, roosevelt, eisenhower, lincoln, washington,
   fdroosevelt, reagan, obama, kennedy, jfk, jfkc)


#------------------------------------------------------------------------------
# Cluster the presidents differnt from each other?
selected_pres <- c("bush", "jefferson", "roosevelt")
cluster_example <- lftm[, colnames(lftm) %in% selected_pres]

ex_cluster <- dist(scale(t(cluster_example), center = T, scale = T))
tree <- hclust(ex_cluster, method = "ward.D2")
plot(tree, hang = - 1, ylab = "Height", xlab = "Presidents", sub = "", cex = 0.8)



### Text analysis with caret----------------------------------------------------

set.seed(1337, sample.kind = "Rounding")
index <- createDataPartition(colnames(lftm), p = 0.7, list = FALSE)

testset <- lftm[,-index]
trainset <- lftm[,index]
#-------------------------------------------------------------------------------
# Random selection
dim(testset)
rs <- sample(presnames, 258, replace = T)
confusionMatrix(as.factor(rs), as.factor(colnames(testset)))$overall["Accuracy"]

#-------------------------------------------------------------------------------
# Train SVM model
svm_model <- train(t(trainset), colnames(trainset), method = "svmLinear3") 

# How well does it fit the train data?
selffit2 <- predict(svm_model, t(trainset))
confusionMatrix(as.factor(selffit2), as.factor(colnames(trainset)))$overall["Accuracy"]

# How accurate is the model?
svm_fit <- predict(svm_model, t(testset))
svm <- confusionMatrix(as.factor(svm_fit),as.factor(colnames(testset)))
svm$overall["Accuracy"]

model_performance <- cbind.data.frame(model_performance, SVM = svm$overall["Accuracy"])

# Calculate and plot balanced accuracy for each president
bal_acc_svm <- data.frame(president = names(svm$byClass[,"Balanced Accuracy"]), 
                          "balanced accuracy" = svm$byClass[,"Balanced Accuracy"])
bal_acc_svm <- bal_acc_svm %>% mutate(president = sapply(1:nrow(bal_acc_svm), function(x) {
  (gsub(".*: ", "", bal_acc_svm$president[x]))
}))
bal_acc_svm %>% ggplot(aes(y = reorder(president, balanced.accuracy), 
                           x = balanced.accuracy)) + geom_col()+ 
  geom_col(color = "white", fill = "blue") +
  labs(x = "Balanced Accuracy", 
       y = "",
       title = "SVM model")


#------------------------------------------------------------------------------
reglog_model <- train(t(trainset), colnames(trainset), method = "regLogistic") 
selffit3 <- predict(reglog_model, t(trainset))
confusionMatrix(as.factor(selffit3), as.factor(colnames(trainset)))$overall["Accuracy"]

reglog_fit <- predict(reglog_model, t(testset))
reglog <- confusionMatrix(as.factor(reglog_fit),as.factor(colnames(testset)))
reglog$overall["Accuracy"]

bal_acc_rlr <- data.frame(president = names(reglog$byClass[,"Balanced Accuracy"]), 
                          "balanced accuracy" = reglog$byClass[,"Balanced Accuracy"])
bal_acc_rlr <- bal_acc_rlr %>% mutate(president = sapply(1:nrow(bal_acc_rlr), function(x) {
  (gsub(".*: ", "", bal_acc_rlr$president[x]))
}))
bal_acc_rlr %>% ggplot(aes(y = reorder(president, balanced.accuracy), 
                           x = balanced.accuracy)) + geom_col()+ 
  geom_col(color = "white", fill = "blue") +
  labs(x = "Balanced Accuracy", 
       y = "",
       title = "Regularized logReg model")

model_performance <- cbind.data.frame(model_performance, regLog = reglog$overall["Accuracy"])

#------------------------------------------------------------------------------
# Term importance
#-------------------------------------------------------------------------------

nsc_model <- train(t(trainset), colnames(trainset), method = "pam") 
nsc_fit <- predict(nsc_model, t(testset))
nsc <- confusionMatrix(as.factor(nsc_fit),as.factor(colnames(testset)))
nsc$overall["Accuracy"]
mean(nsc$byClass[,"Balanced Accuracy"])
variableImp <- varImp(nsc_model)$importance


bal_acc_nsc <- data.frame(president = names(nsc$byClass[,"Balanced Accuracy"]), 
                          "balanced accuracy" = nsc$byClass[,"Balanced Accuracy"])
bal_acc_nsc <- bal_acc_nsc %>% mutate(president = sapply(1:nrow(bal_acc_nsc), function(x) {
  (gsub(".*: ", "", bal_acc_nsc$president[x]))
}))
bal_acc_nsc %>% ggplot(aes(y = reorder(president, balanced.accuracy), 
                           x = balanced.accuracy)) + geom_col()+ 
  geom_col(color = "white", fill = "blue") +
  labs(x = "Balanced Accuracy", 
       y = "",
       title = "PAM model")

examples <- variableImp %>% select(fdroosevelt, kennedy, reagan, obama)
examples <- cbind(words = rownames(examples), examples)



f <- examples %>% slice_max(n=20, order_by = fdroosevelt) %>% 
            ggplot(aes(y=reorder(words, fdroosevelt), x = fdroosevelt)) + 
            geom_col(fill="blue", color = "white") +
            labs(x = "",
                 y = "Top20 most important variables",
                 title = "Franklin D. Roosevelt")

k <- examples %>% slice_max(n=20, order_by = kennedy) %>% 
             ggplot(aes(y=reorder(words, kennedy), x = kennedy)) + 
             geom_col(fill="blue", color = "white") +
             labs(x = "",
                  y = "Top20 most important variables",
                  title = "John F. Kennedy")

rr <- examples %>% slice_max(n=20, order_by = reagan) %>% 
             ggplot(aes(y=reorder(words, reagan), x = reagan)) + 
             geom_col(fill="blue", color = "white") +
             labs(x = "",
                  y = "Top20 most important variables",
                  title = "Ronald Reagan")

o <- examples %>% slice_max(n=20, order_by = obama) %>% 
            ggplot(aes(y=reorder(words, obama), x = obama)) + 
            geom_col(fill="blue", color = "white") +
            labs(x = "",
                 y = "Top20 most important variables",
                 title = "Barack Obama")

ggarrange(f, k, o, rr, 
          ncol = 2, nrow = 2)
##-----------------------------------testing


