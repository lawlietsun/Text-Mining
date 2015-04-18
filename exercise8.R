setwd("~/Google Drive/MSc Data Analytics/Core/CS909 Data Mining/exercises/exercise8")

mydata <- read.csv(file="reutersCSV.csv",header=T,sep=",")

r <- nrow(mydata)
c <- ncol(mydata)

##### Pre-Prosessing ##### 

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("tm")
install.packages("koRpus")
install.packages("SnowballC")
install.packages("topicmodels")

require("NLP")
require("tm")
require("openNLP")
require("NLP")
require("openNLPmodels.en")
require("koRpus")
require("SnowballC")
require("topicmodels")
require("e1071")

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

write.csv(rmdata, file = "cleanedData.csv", row.names=F)

my_data <- read.csv(file="finalclean.csv",header=T,sep=",")

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

write.csv(my_data, file = "cleanedDatav6.csv", row.names=F)

###################################

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

######## combine text and title
my_data[, "doc.title.text.cleaned"] <- NA

for(i in 1: 11340){
  s1 <- as.String(my_data[i,124])
  s2 <- as.String(my_data[i,125])
  s3 <- as.String(c(s1,s2))
  s3 <- gsub("[[:punct:]]", "", s3)
  my_data[i,126] <- s3
}

#######

doc.vec <- VectorSource(my_data[,126])
doc.corpus <- Corpus(doc.vec)
dtm <- DocumentTermMatrix(doc.corpus)

inspect(dtm[1:5,1:10])
findFreqTerms(dtm, lowfreq = 2000, highfreq = 3000)
findAssocs(dtm,'opec',0.5)
dtm2 <- removeSparseTerms(dtm,sparse=0.95)
inspect(dtm2[10:15,100:110])

featureMx <- inspect(dtm2)
featureMx[1:40,1:10]

tf <- matrix(0,11340,136)

for(i in 1:11340){
  for(j in 1:136){
    tf[i,j] <- featureMx[i,j] / max(featureMx[,j])
  }
}

tf2 <- featureMx

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
}

idf = matrix(0,1,136)
for(j in 1:136){
 idf[1,j] <- log2(11340/df[1,j])
}

ttf <- tf2
for(i in 1:11340){
  ttf[i,] <- ttf[i,] * idf
}

tfidf <- ttf

write.table(tfidf, file = "tfidf", row.names = TRUE, col.names = TRUE)
write.csv(tfidf, file = "tfidf.csv", row.names=F)


features <- findFreqTerms(dtm2)
# doc <- my_data[1,126]
# doc <- as.String(doc)
# spans <- whitespace_tokenizer(doc)
# tokens <- doc[spans]

# terms = 0
# for(i in 1:length(tokens)){
#   for(j in 1:length(features)){
#     if(tokens[i] == features[j]){
#       terms = terms + 1
#     }
#   }
# }

x <- 1
for(i in 1:11340){
  if(x < length(sort(tfidf[i,], decreasing = T)[which(sort(tfidf[i,], decreasing = T) != 0)])){
    x <- length(sort(tfidf[i,], decreasing = T)[which(sort(tfidf[i,], decreasing = T) != 0)])
  }
}


length(sort(tfidf[4,], decreasing = T)[which(sort(tfidf[4,], decreasing = T) != 0)])

tfidf <- ttf

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
aaa <- tfidf[,-which(sm < med)]

##############
colname <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")
noofcol <- c()
for(i in 1:length(colname)){
  noofcol <- c(noofcol, which(colnames(my_data) == colname[i]))
}

df <- cbind(my_data[,noofcol], aaa)
train <- df[which(my_data$purpose == "train"),]
test <- df[which(my_data$purpose == "test"),]


# topic.money.fx
moneyfx <- svm(topic.money.fx ~ ., data = train, scale=F)
p <- predict(moneyfx, newdata = test)
for (i in 1:length(p)){
  if(p[i]>=0.5){
    p[i] <- 1    
  }else{
    p[i] <- 0    
  }
}
t <- table(round(p), test$topic.money.fx)
acc <- (t[1,1] + t[2,2]) / (t[1,1] + t[1,2] + t[2,1] + t[2,2])

grain <- svm(topic.grain ~ ., data = train)
p <- predict(grain, newdata = test)
for (i in 1:length(p)){
  if(p[i]>=0.5){
    p[i] <- 1    
  }else{
    p[i] <- 0    
  }
}
t <- table(round(p), test$topic.grain)
acc <- (t[1,1] + t[2,2]) / (t[1,1] + t[1,2] + t[2,1] + t[2,2])


acq <- svm(topic.acq ~ ., data = train)
p <- predict(acq, newdata = test)
for (i in 1:length(p)){
  if(p[i]>=0.5){
    p[i] <- 1    
  }else{
    p[i] <- 0    
  }
}
t <- table(round(p), test$topic.acq)
acc <- (t[1,1] + t[2,2]) / (t[1,1] + t[1,2] + t[2,1] + t[2,2])


##############

earn <- naiveBayes(topic.earn ~., data = train)
p <- predict(earn, newdata = test)
table(p, test$topic.earn)


##############


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

