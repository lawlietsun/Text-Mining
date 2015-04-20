################### Packages Install ################### 

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("tm")
install.packages("koRpus")
install.packages("SnowballC")
install.packages("topicmodels")
install.packages("randomForest")
install.packages("fpc")

################### Prepare ################### 
setwd("~/Google Drive/MSc Data Analytics/Core/CS909 Data Mining/exercises/exercise8")

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
my_data <- read.csv(file="finalcleanv2.csv",header=T,sep=",")
tfidf <- read.csv(file = "tfidf2.csv")
featureselected <- read.csv(file = "featureselected.csv")
top10 <- read.csv(file = "top10v2.csv")
finalldamx <- read.csv(file = "finalldamx.csv")
final_final_lda_myfeature <- read.csv(file = "final_final_lda_myfeaturev4.csv")

write.csv(my_data, file = "finalcleanv2.csv", row.names=F)

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

# Revmove un-used data

x <- c()
for(i in 1:11340){
  if(my_data[i,3] == "not-used"){
    x <- c(x,i)
  }
  print(i)
}

my_data <- my_data[-x,]

################### Tasks 2 : Feature representations LDA ONLY ################### 

finaldata <- my_data[-c(1:2,122:125)]

doc.vec <- VectorSource(finaldata[,120])
doc.corpus <- Corpus(doc.vec)
dtm <- DocumentTermMatrix(doc.corpus)

# inspect(dtm[1:5,1:10])
# findFreqTerms(dtm, lowfreq = 2000, highfreq = 3000)
# findAssocs(dtm,'opec',0.5)
dtm2 <- removeSparseTerms(dtm,sparse=0.95)

sss <- apply(dtm2,1,sum)
dtm3 = dtm2[-which(sss == 0),]
inspect(dtm3[1:100,100:110])

lda <- LDA(dtm, method = "VEM", control = list(alpha = 0.1), k = 10)
topics(lda)
ldafeatures <- terms(lda,10)
ldafeaturesstr <- as.String(ldafeatures)
spans <- whitespace_tokenizer(ldafeaturesstr)
tokens <- ldafeaturesstr[spans]
tokens <- gsub("[[:punct:]]", "", tokens)
unique(tokens)

ldamx <- matrix(0,10777,length(unique(tokens))+1)
ldamx <- as.data.frame(ldamx)
colnames(ldamx) <- c(unique(tokens), "topic")
ldamx$topic <- topics(lda)

for(i in 1:10777){
    topicnum <- ldamx$topic[i]
    colnum <- match(ldafeatures[,topicnum],unique(tokens))
    for(k in colnum){
      ldamx[i,k] <- 1
    } 
print(i)
}

write.csv(ldamx, file = "ldamxv5.csv", row.names=F)
ldamx <- read.csv(file = "ldamx.csv")
ldamx[,67] <- as.factor(ldamx[,67])

# purpose!!!!!!!!! removed by exicident
ldamx <-cbind(finaldata[,-120],ldamx)
# colnames(ldamx)[1] = "purpose"

# x <- c()
# for(i in 1:11340){
#   if(ldamx[i,1] == "not-used"){
#     x <- c(x,i)
#   }
#   print(i)
# }

ldamx <- ldamx[-x,]

colnames(ldamx)[169] = "class"

colname <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", 
             "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")

finalldamx <- cbind(ldamx[,1],ldamx[colname],ldamx[,120:169])
colnames(finalldamx)[1] = "purpose"

finalldamx <- finalldamx[,-ncol(finalldamx)]

rm <- c()
for(i in 1:10777){
  if(sum(finalldamx[i,2:11]) == 0){
    rm <- c(rm, i)
  }
  print(i)
}

t <- finalldamx[-rm,]

finalldamx <- t

fmx <- c()
for(i in 1:9042){
  for(j in 2:11){
    if(finalldamx[i,j] == 1){
      cs <- colnames(finalldamx)[j]
      fmx <- rbind(fmx, cbind(finalldamx[i,-(2:11)], cs))
    }
  }
  print(i)
}

write.csv(fmx, file = "fmxv1.csv", row.names=F)

t <- fmx[,-51]

colnames(t)[51] = "class"

fmx <- t

write.csv(fmx, file = "fmxv2.csv", row.names=F)

finalldamx <- fmx
# svm(finalldamx)


write.csv(finalldamx, file = "finalldamx.csv", row.names=F)

################### Tasks 2 : Feature representations LDA + MyFeature ################### 

dtm2 <- removeSparseTerms(dtm,sparse=0.95)

featureMx <- inspect(dtm2)

tf2 <- featureMx * 0

for(i in 1:10777){
  for(j in 1:131){
    tf2[i,j] <- featureMx[i,j] / max(featureMx[,j])
  }
  print(i)
}

df = matrix(0,1,131)

for(j in 1:131){
  doc = 0
  for(i in 1:10777){
    if(featureMx[i,j] != 0){
      doc = doc + 1
    }
  }
  df[1,j] <- doc
  print(j)
}

idf = matrix(0,1,131)
for(j in 1:131){
 idf[1,j] <- log2(10777/df[1,j])
 print(j)
}

ttf <- featureMx * 0
for(i in 1:10777){
  ttf[i,] <- tf2[i,] * idf
  print(i)
}

tfidf <- ttf

write.csv(tfidf, file = "tfidf2.csv", row.names=F)

for(i in 1:10777){
  n <- round(length(which(tfidf[i,] != 0)) * 0.5)
  o <- order(tfidf[i,], decreasing = T)
  tfidf[i,] <- tfidf[i,]*0
  for(x in 1:n){
    tfidf[i,o[x]] = 1
  }
  print(i)
}

sm = matrix(0,1,131)
for(i in 1:131){
    sm[1,i] <- sum(tfidf[,i])
}

med <- median(sm[1,])
featureselected <- tfidf[,-which(sm < med)]

# write.csv(top10, file = "top10v3.csv", row.names=F)
featureselected <- read.csv(file = "featureselectedv2.csv")

final_lda_myfeature <- cbind(ldamx[,1], ldamx[colname], ldamx[,120:168], featureselected, ldamx[,169])

colnames(final_lda_myfeature)[1] = "purpose"
colnames(final_lda_myfeature)[129] = "class"

rm <- c()
for(i in 1:10777){
  if(sum(final_lda_myfeature[i,2:11]) == 0){
    rm <- c(rm, i)
  }
  print(i)
}

t <- final_lda_myfeature[-rm,]

final_lda_myfeature <- t 

write.csv(final_lda_myfeature, file = "final_lda_myfeaturev2.csv", row.names=F)

# merge duplucated colnames
k <- final_lda_myfeature
n = 1
for(i in 12:128){
  for(j in (i+1):128){
    if(colnames(k)[i] == colnames(k)[j]){
      merge <- c()
      merge <- k[,i] + k[,j]
      k <- cbind(k, merge)
      colnames(k)[129 + n] = colnames(k)[i]
      n = n + 1
    }
  }
}

# change value 2 to 1
for(i in 1:nrow(k)){
  for(j in 130:145){
    if(k[i,j] > 1){
      k[i,j] = 1
    }
  }
  print(i)
}

rmc <- c()
for(i in 12:128){
  for(j in 130:145){
    if(colnames(k)[i] == colnames(k)[j]){
      rmc <- c(rmc, i)
    }
  }
}

k <- k[,-rmc]

fk <- cbind(k[,1:97], k[,99:114], k[,98])
colnames(fk)[114] = "class"

final_final_lda_myfeature <- fk[,-114] 

# expend multiple topics
fmx <- c()
for(i in 1:9042){
  for(j in 2:11){
    if(final_final_lda_myfeature[i,j] == 1){
      cs <- colnames(final_final_lda_myfeature)[j]
      fmx <- rbind(fmx, cbind(final_final_lda_myfeature[i,1], final_final_lda_myfeature[i,12:113], cs))
    }
  }
  print(i)
}

colnames(fmx)[1] <- "purpose"
colnames(fmx)[104] <- "class"

final_final_lda_myfeature <- fmx

write.csv(final_final_lda_myfeature, file = "final_final_lda_myfeaturev4.csv", row.names=F)

# 
# # Remove no topic docs
# a <- c()
# for(i in 1:10777){
#   if(sum(top10[i,69:78]) == 0){
#     a <- c(a,i)
#     k = k+1
#   }
#   print(i)
# }
# top10 <- top10[-a,]
# 
# # purpose!!!!!!!!! removed by exicident
# 
# a <- top10 
# a <- c(a, my_data$purpose)
# a <- cbind( my_data$purpose,a)
# colnames(a)[1] = "purpose"
# 
# 
# 
# # combine classes/ topics
# mx <- c()
# mx <- a
# 
# fmx <- c()
# for(i in 1:9042){
#   for(j in 70:79){
#     if(mx[i,j] == 1){
#       cs <- names(mx[,1:79])[j]
#       fmx <- rbind(fmx, cbind(mx[i,1:69], cs))
#     }
#   }
#   print(i)
# }
# 
# colnames(fmx)[70] <- "class"
# 
# write.csv(fmx, file = "multiclassv3.csv", row.names=F)

################### Tasks 3 : Build classifiers ################### 

finaldata <- finalldamx
finaldata <- final_final_lda_myfeature

train <- finaldata[which(finaldata$purpose == "train"),]
test <- finaldata[which(finaldata$purpose == "test"),]

train <- train[,-1]
test <- test[,-1]

rname <- c("SVM", "naiveBayes", "randomForest")
cname <- colname
result <- matrix(0, 3, 10, dimnames = list(rname, cname))

# SVM
model <- svm(train$class ~ ., data = train, scale=F)

# naiveBayes
model <- naiveBayes(train$class ~ ., data = train)

# randomForest
model <- randomForest(train$class ~ ., data = train)

p <- predict(model, test[,-ncol(test)])
t <- table(p, test$class)

ptrue = 0
for(i in 1:10){
  ptrue = ptrue + t[i,i]
}
acc <- ptrue / nrow(test)
result[1,i] <- acc
print(colname[i])
print(t)
print(acc)
cat("-------------------\n")

# TABLE #
mytable <- matrix(0,10,8)
rownames(mytable) <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", 
                       "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")
colnames(mytable) <- c("TP", "TN", "FN", "FP", "Recall", "Precision", "Accuracy", "F-measure")
  
for(i in 1:ncol(t)){
  tp <- t[i,i]
  tn <- 0
  fn <- sum(t[i,-i], na.rm = TRUE)
  fp <- sum(t[-i,i], na.rm = TRUE)
  recall <- tp/(tp + fn)
  precision <- tp/(tp + fp)
  fmeasure <- (2 * precision * recall)/(precision + recall)
  accuracy <- (tp + tn)/nrow(test)
  
  mytable[i,1] <- tp
  mytable[i,2] <- tn
  mytable[i,3] <- fn
  mytable[i,4] <- fp
  mytable[i,5] <- recall
  mytable[i,6] <- precision
  mytable[i,7] <- accuracy
  mytable[i,8] <- fmeasure
}

################################# IGNORE ###############################################################################
#   
# 
# # naiveBayes
# 
# # TABLE #
# mytable2 <- matrix(0,10,8)
# rownames(mytable2) <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", 
#                        "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")
# colnames(mytable2) <- c("TP", "TN", "FN", "FP", "Recall", "Precision", "Accuracy", "F-measure")
# 
# 
# for(i in 1:10){
#   
#   train1 <- cbind(trainx,trainy[i])
#   train1[,69] <- as.factor(train1[,69])
#   
#   test1 <- cbind(testx,testy[i])
#   test1[,69] <- as.factor(test1[,69])
#   
#   model <- naiveBayes(train$class ~ ., data = train)
#   p <- predict(model, test[,-ncol(test)])
#   t <- table(p, test$class)
#   acc <- (t[1,1] + t[2,2]) / (t[1,1] + t[1,2] + t[2,1] + t[2,2])
#   result[2,i] <- acc
#   print(colname[i])
#   print(t)
#   print(acc)
#   cat("-------------------\n")
#   
#   
#   #   for(j in 1:ncol(t)){
#   tp <- t[1,1]
#   tn <- t[2,2]
#   #     fn <- sum(t[i,-i], na.rm = TRUE)
#   #     fp <- sum(t[-i,i], na.rm = TRUE)
#   fn <- t[1,2]
#   fp <- t[2,1]
#   recall <- tp/(tp + fn)
#   precision <- tp/(tp + fp)
#   fmeasure <- (2 * precision * recall)/(precision + recall)
#   accuracy <- (tp + tn)/(tp + tn + fn + fp)
#   
#   mytable2[i,1] <- tp
#   mytable2[i,2] <- tn
#   mytable2[i,3] <- fn
#   mytable2[i,4] <- fp
#   mytable2[i,5] <- recall
#   mytable2[i,6] <- precision
#   mytable2[i,7] <- accuracy
#   mytable2[i,8] <- fmeasure
# }
# 
# # randomForest
# 
# 
# # TABLE #
# mytable3 <- matrix(0,10,8)
# rownames(mytable3) <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", 
#                        "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")
# colnames(mytable3) <- c("TP", "TN", "FN", "FP", "Recall", "Precision", "Accuracy", "F-measure")
# 
# for(i in 1:10){
#   
#   train1 <- cbind(trainx,trainy[i])
#   train1[,69] <- as.factor(train1[,69])
#   
#   test1 <- cbind(testx,testy[i])
#   test1[,69] <- as.factor(test1[,69])
#   
#   model <- randomForest(train$class ~ ., data = train)
#   p <- predict(model, test[,-ncol(test)])
#   t <- table(p, test$class)
#   acc <- (t[1,1] + t[2,2]) / (t[1,1] + t[1,2] + t[2,1] + t[2,2])
#   result[3,i] <- acc
#   print(colname[i])
#   print(t)
#   print(acc)
#   cat("-------------------\n")
#   
#   #   for(j in 1:ncol(t)){
#   tp <- t[1,1]
#   tn <- t[2,2]
#   #     fn <- sum(t[i,-i], na.rm = TRUE)
#   #     fp <- sum(t[-i,i], na.rm = TRUE)
#   fn <- t[1,2]
#   fp <- t[2,1]
#   recall <- tp/(tp + fn)
#   precision <- tp/(tp + fp)
#   fmeasure <- (2 * precision * recall)/(precision + recall)
#   accuracy <- (tp + tn)/(tp + tn + fn + fp)
#   
#   mytable3[i,1] <- tp
#   mytable3[i,2] <- tn
#   mytable3[i,3] <- fn
#   mytable3[i,4] <- fp
#   mytable3[i,5] <- recall
#   mytable3[i,6] <- precision
#   mytable3[i,7] <- accuracy
#   mytable3[i,8] <- fmeasure
# }

# write.csv(result, file = "result.csv")

############################################################################################################################
km <- kmeans(top10, 10)

table(km$cluster)

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(top10, km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, km$cluster)

data.frame(testx, km$cluster)
