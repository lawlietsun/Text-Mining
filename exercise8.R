setwd("~/Google Drive/MSc Data Analytics/Core/CS909 Data Mining/exercises/exercise8")

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("tm")
install.packages("koRpus")
install.packages("SnowballC")
install.packages("topicmodels")
install.packages("randomForest")

require("NLP")
require("tm")
require("openNLP")
require("NLP")
require("openNLPmodels.en")
require("koRpus")
require("SnowballC")
require("topicmodels")
require("e1071")
require("randomForest")

mydata <- read.csv(file="reutersCSV.csv",header=T,sep=",")
my_data <- read.csv(file="finalclean.csv",header=T,sep=",")
tfidf <- read.csv(file = "tfidf.csv")
featureselected <- read.csv(file = "featureselected.csv")

################### Tasks 1 : Pre-Prosessing ################### 

r <- nrow(mydata)
c <- ncol(mydata)

rmdata <- mydata

# remove rows that have no 1s (the topic has no text belong to)
rnrow <- c()
for(i in 1:c){
  if(is.integer(mydata[,i])){
    if(sum(mydata[,i]) == 0){
      rnrow <- c(rnrow,i)
    }
  }
}
rmdata <- mydata[,-rnrow]

# remove cols that have no 1s (the text has no topic belong to)
rncol <- c()
for(j in 1:r){
  if(sum(rmdata[j,4:121]) == 0){
    rncol <- c(rncol,j)
  }
}
rmdata <- rmdata[-rncol,]

# write.csv(rmdata, file = "cleanedData.csv", row.names=F)

my_data[, "cleaned.title"] <- NA
my_data[, "cleaned.text"] <- NA

r <- nrow(my_data)
c <- ncol(my_data)

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()

person_annotator <- Maxent_Entity_Annotator(kind = "person")
location_annotator <- Maxent_Entity_Annotator(kind = "location")
organization_annotator <- Maxent_Entity_Annotator(kind = "organization")
date_annotator <- Maxent_Entity_Annotator(kind = "date")

for(ii in 1:r){
  
  if(my_data[ii,122] != ""){
    
    s <- my_data[ii,122]
    s <- as.String(s)
    
    # 1. Tokenize: divide into words (unigrams)
    
    s <- gsub("/", " ", s)  # replace / to space 
    spans <- whitespace_tokenizer(s)
    tokens <- s[spans]
    
    # 2. Remove punctuation, replace links
    
    tokens <- gsub("[[:punct:]]", "", tokens)
    tokens <- gsub("http\\S+\\s*", "LINK", tokens)
    
    # 3. POS tagging & Lemmatisation
    
    tagged.results <- treetag(tokens, treetagger="manual", format="obj",
                              lang="en",
                              stopwords = tm::stopwords("SMART"),
                              stemmer = SnowballC::wordStem,
                              TT.options=list(path="./tree-tagger", preset="en"))
    
    lemma <- tagged.results@TT.res$lemma
    
    # 4. Remove stop words
    
    non_stopwords <- tagged.results@TT.res[which(tagged.results@TT.res$stop == FALSE),]
    
    for(i in 1:nrow(non_stopwords)){
      if(non_stopwords$lemma[i] == "@card@"){
        non_stopwords$lemma[i] <- non_stopwords$stem[i]
      }
      if(non_stopwords$lemma[i] == "<unknown>"){
        non_stopwords$lemma[i] <- non_stopwords$stem[i]
      }
    }
    
    # 5. NE recognition:
    
    ss <- paste(non_stopwords$lemma, collapse = " ")
    ss <- as.String(ss)
    
    a2 <- annotate(ss, list(sent_token_annotator, word_token_annotator))
    
    if(length(person_annotator(ss,a2)) != 0){
      pt <- ss[person_annotator(ss,a2)]
    }
    
    if(length(location_annotator(ss,a2)) != 0){
      lt <- ss[location_annotator(ss,a2)]
    }
    
    if(length(organization_annotator(ss,a2)) != 0){
      ot <- ss[organization_annotator(ss,a2)]
    }
    
    if(length(date_annotator(ss,a2)) != 0){
      dt <- ss[date_annotator(ss,a2)]
    }
    
    if(length(person_annotator(ss,a2)) != 0){  
      if(length(pt) == 1){
        ss <- gsub(pt, "PER", ss)
      }else{
        for(i in 1:length(pt)){
          ss <- gsub(pt[i], "PER", ss)
        }
      }
    }
    
    if(length(location_annotator(ss,a2)) != 0){
      if(length(lt) == 1){
        ss <- gsub(lt, "LOC", ss)
      }else{
        for(i in 1:length(lt)){
          ss <- gsub(lt[i], "LOC", ss)
        }
      }
    }
    
    if(length(organization_annotator(ss,a2)) != 0){
      if(length(ot) == 1){
        ss <- gsub(ot, "ORG", ss)
      }else{
        for(i in 1:length(ot)){
          ss <- gsub(ot[i], "ORG", ss)
        }
      }
    }
    
    if(length(date_annotator(ss,a2)) != 0){
      if(length(dt) == 1){
        ss <- gsub(dt, "DAT", ss)
      }else{
        for(i in 1:length(dt)){
          ss <- gsub(dt[i], "DAT", ss)
        }
      }
    }
    
    ss <- gsub("[[:digit:]]+", "NUM", ss)
    
    my_data[ii,124] <- ss
    
    print(ii)
  }
}

# write.csv(my_data, file = "cleanedDatav6.csv", row.names=F)

# check NA
which((which(my_data[,122] == "") == which(is.na(my_data[,124]))) == FALSE)

which((which(my_data[,123] == "") == which(is.na(my_data[,125]))) == FALSE)

i = 0
n = 0
for(i in 1:11340){
  if(my_data[i,122] == "" && is.na(my_data[i,124])){
    n = n + 1
  }
}

i = 0
n = 0
for(i in 1:11340){
  if(is.na(my_data[i,124])){
    n = n + 1
  }
}

# combine text and title
my_data[, "doc.title.text.cleaned"] <- NA

for(i in 1: 11340){
  s1 <- as.String(my_data[i,124])
  s2 <- as.String(my_data[i,125])
  s3 <- as.String(c(s1,s2))
  s3 <- gsub("[[:punct:]]", "", s3)
  my_data[i,126] <- s3
}

################### Tasks 2 : Feature representations ################### 

# aa <- my_data
doc.vec <- VectorSource(my_data[,126])
doc.corpus <- Corpus(doc.vec)
dtm <- DocumentTermMatrix(doc.corpus)

# inspect(dtm[1:5,1:10])
# findFreqTerms(dtm, lowfreq = 2000, highfreq = 3000)
# findAssocs(dtm,'opec',0.5)
dtm2 <- removeSparseTerms(dtm,sparse=0.95)
# inspect(dtm2[10:15,100:110])

featureMx <- inspect(dtm2)

tf2 <- featureMx * 0

for(i in 1:11340){
  for(j in 1:136){
    tf2[i,j] <- featureMx[i,j] / max(featureMx[,j])
  }
  print(i)
}

df = matrix(0,1,136)

for(j in 1:136){
  doc = 0
  for(i in 1:11340){
    if(featureMx[i,j] != 0){
      doc = doc + 1
    }
  }
  df[1,j] <- doc
  print(j)
}

idf = matrix(0,1,136)
for(j in 1:136){
 idf[1,j] <- log2(11340/df[1,j])
 print(j)
}

ttf <- featureMx * 0
for(i in 1:11340){
  ttf[i,] <- tf2[i,] * idf
  print(i)
}

tfidf <- ttf

write.csv(tfidf, file = "tfidf2.csv", row.names=F)

for(i in 1:11340){
  n <- round(length(which(tfidf[i,] != 0)) * 0.5)
  o <- order(tfidf[i,], decreasing = T)
  tfidf[i,] <- tfidf[i,]*0
  for(x in 1:n){
    tfidf[i,o[x]] = 1
  }
  print(i)
}

sm = matrix(0,1,136)
for(i in 1:136){
    sm[1,i] <- sum(tfidf[,i])
}

med <- median(sm[1,])
featureselected <- tfidf[,-which(sm < med)]

# write.csv(featureselected, file = "featureselected.csv", row.names=F)
featureselected <- read.csv(file = "featureselected.csv")

################### Tasks 3 : Build classifiers ################### 

colname <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", 
             "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")

top10 <- cbind(featureselected, my_data[colname])

train <- top10[which(my_data$purpose == "train"),]
test <- top10[which(my_data$purpose == "test"),]

trainx <- train[1:68]
trainy <- train[69:78]
testx <- test[1:68]
testy <- test[69:78]

for(i in 1:10){
  colnames(trainy)[i] = "class"
  colnames(testy)[i] = "class"
}

# write.csv(top10, file = "top10v2.csv", row.names=F)

# SVM

for(i in 1:10){
  
  train1 <- cbind(trainx,trainy[i])
  train1[,69] <- as.factor(train1[,69])
  
  test1 <- cbind(testx,testy[i])
  test1[,69] <- as.factor(test1[,69])
  
  model <- svm(train1$class ~ ., data = train1, scale=F)
  p <- predict(model, test1[,-ncol(test1)])
  t <- table(p, test1$class)
  acc <- (t[1,1] + t[2,2]) / (t[1,1] + t[1,2] + t[2,1] + t[2,2])
  print(colname[i])
  print(t)
  print(acc)
  cat("-------------------\n")
}

# naiveBayes

for(i in 1:10){
  
  train1 <- cbind(trainx,trainy[i])
  train1[,69] <- as.factor(train1[,69])
  
  test1 <- cbind(testx,testy[i])
  test1[,69] <- as.factor(test1[,69])
  
  model <- naiveBayes(train1$class ~ ., data = train1)
  p <- predict(model, test1[,-ncol(test1)])
  t <- table(p, test1$class)
  acc <- (t[1,1] + t[2,2]) / (t[1,1] + t[1,2] + t[2,1] + t[2,2])
  print(colname[i])
  print(t)
  print(acc)
  cat("-------------------\n")
}

# randomForest

for(i in 1:10){
  
  train1 <- cbind(trainx,trainy[i])
  train1[,69] <- as.factor(train1[,69])
  
  test1 <- cbind(testx,testy[i])
  test1[,69] <- as.factor(test1[,69])
  
  model <- randomForest(train1$class ~ ., data = train1, maxnodes = 4, ntree = 30)
  p <- predict(model, test1[,-ncol(test1)])
  t <- table(p, test1$class)
  acc <- (t[1,1] + t[2,2]) / (t[1,1] + t[1,2] + t[2,1] + t[2,2])
  print(colname[i])
  print(t)
  print(acc)
  cat("-------------------\n")
}

############################################################################################################################


sort(tfidf[1,], decreasing = T)
order(tfidf[1,], decreasing = T)


sss <- apply(dtm2,1,sum)
dtm3 = dtm2[-which(sss == 0),]
inspect(dtm3[1:100,100:110])

t <- inspect(dtm3)
tt <- c()

for(i in 1:136){
  if(sum(t[,i]) > 2000){
    tt <- cbind(tt, t[,i])
  }
}


ttt <- cbind(my_data, inspect(dtm2))



lda <- LDA(dtm3, method = "VEM", control = list(alpha = 0.1), k = 2)


## Determine the distribution of POS tags for word tokens.
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tags
table(tags)

## Extract token/POS pairs (all of them): easy.
sprintf("%s/%s", s[a3w], tags)
## Extract pairs of word tokens and POS tags for second sentence:
a3ws2 <- annotations_in_spans(subset(a3, type == "word"),
                              subset(a3, type == "sentence")[2L])[[1L]]
sprintf("%s/%s", s[a3ws2], sapply(a3ws2$features, `[[`, "POS"))

