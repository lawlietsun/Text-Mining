################### Packages Install ################### 

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("tm")
install.packages("koRpus")
install.packages("SnowballC")
install.packages("topicmodels")
install.packages("randomForest")
install.packages("fpc")
install.packages("NLP")
install.packages("openNLP")
install.packages("e1071")
install.packages("fpc")
install.packages("cluster") 
install.packages("pvclust")
install.packages("mclust")
install.packages("EMCluster")

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
require("fpc")
require("cluster") 
require("pvclust")
require("mclust")
require("EMCluster")

mydata <- read.csv(file="reutersCSV.csv",header=T,sep=",")
my_data <- read.csv(file="finalcleanv2.csv",header=T,sep=",")
rmdata <- read.csv(file="1.cleanedData_10777_123.csv",header=T,sep=",")
tfidf <- read.csv(file = "tfidf2.csv")
featureselected <- read.csv(file = "featureselected.csv")
top10 <- read.csv(file = "top10v2.csv")
finalldamx <- read.csv(file = "finalldamx.csv")
final_final_lda_myfeature <- read.csv(file = "final_final_lda_myfeaturev4.csv")
clusterdata <- read.csv(file = "clusterdata.csv")
alldatac <- read.csv(file = "alldata_alltopics.csv")

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

# Revmove un-used data
x <- c()
for(i in 1:nrow(rmdata)){
  if(rmdata[i,3] == "not-used"){
    x <- c(x,i)
  }
  print(i)
}
rmdata <- rmdata[-x,]

# write.csv(rmdata, file = "1.cleanedData_10777_123.csv", row.names=F)

my_data <- rmdata

my_data[, "doc.title.text.cleaned"] <- NA
# my_data[, "cleaned.text"] <- NA

r <- nrow(my_data)
c <- ncol(my_data)

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()

person_annotator <- Maxent_Entity_Annotator(kind = "person")
location_annotator <- Maxent_Entity_Annotator(kind = "location")
organization_annotator <- Maxent_Entity_Annotator(kind = "organization")
date_annotator <- Maxent_Entity_Annotator(kind = "date")

# combine text and title
for(i in 1:r)){
  title <- as.String(my_data$doc.title[i])
  text <- as.String(my_data$doc.text[i])
  title_text <- as.String(c(title,text))
  
  my_data[i,ncol(my_data)] <- title_text
  
  print(i)
}

for(ii in 1:r){
  
  if(my_data[ii,c] != ""){
    
    s <- my_data[ii,c]
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

################### Tasks 2 : Feature representations LDA ONLY ################### 

finaldata <- my_data

doc.vec <- VectorSource(finaldata[,ncol(finaldata)])
doc.corpus <- Corpus(doc.vec)
dtm <- DocumentTermMatrix(doc.corpus)


lda <- LDA(dtm, method = "VEM", control = list(alpha = 0.1), k = 10)
# topics(lda)
ldafeatures <- terms(lda,10)
ldafeaturesstr <- as.String(ldafeatures)
spans <- whitespace_tokenizer(ldafeaturesstr)
tokens <- ldafeaturesstr[spans]
tokens <- gsub("[[:punct:]]", "", tokens)
# unique(tokens)

ldamx <- matrix(0,nrow(finaldata),length(unique(tokens))+1)
ldamx <- as.data.frame(ldamx)
colnames(ldamx) <- c(unique(tokens), "topic")
ldamx$topic <- topics(lda)

for(i in 1:nrow(finaldata)){
    topicnum <- ldamx$topic[i]
    colnum <- match(ldafeatures[,topicnum],unique(tokens))
    for(k in colnum){
      ldamx[i,k] <- 1
    } 
print(i)
}

colname <- c("purpose","topic.earn", "topic.acq", "topic.money.fx", 
             "topic.grain", "topic.crude", "topic.trade", "topic.interest", 
             "topic.ship", "topic.wheat", "topic.corn")

finalldamx <- cbind(rmdata[colname],ldamx)
colnames(finalldamx)[ncol(finalldamx)] = "class"


rm <- c()
for(i in 1:nrow(finalldamx)){
  if(sum(finalldamx[i,2:11]) == 0){
    rm <- c(rm, i)
  }
  print(i)
}

t <- finalldamx[-rm,]

finalldamx <- t

#expand multiclasses
fmx <- c()
for(i in 1:nrow(finalldamx)){
  for(j in 2:11){
    if(finalldamx[i,j] == 1){
      cs <- colnames(finalldamx)[j]
      fmx <- rbind(fmx, cbind(finalldamx[i,-(2:11)], cs))
    }
  }
  print(i)
}

f <- fmx[,-(ncol(fmx)-1)]
colnames(f)[ncol(f)] <- "class"
lda <- f

write.csv(lda, file = "lda_9990_50.csv", row.names=F)

################### Tasks 2 : Feature representations LDA + MyFeature ################### 

dtm2 <- removeSparseTerms(dtm,sparse=0.95)

featureMx <- inspect(dtm2)

tf2 <- featureMx * 0

for(i in 1:nrow(tf2)){
  for(j in 1:ncol(tf2)){
    tf2[i,j] <- featureMx[i,j] / max(featureMx[,j])
  }
  print(i)
}

df = matrix(0,1,ncol(tf2))

for(j in 1:ncol(featureMx)){
  doc = 0
  for(i in 1:nrow(featureMx)){
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

k <- culsterdata
n = 1
for(i in 1:116){
  for(j in (i+1):116){
    if(colnames(k)[i] == colnames(k)[j]){
      merge <- c()
      merge <- k[,i] + k[,j]
      k <- cbind(k, merge)
      colnames(k)[116 + n] = colnames(k)[i]
      n = n + 1
    }
  }
}

# change value 2 to 1
for(i in 1:nrow(k)){
  for(j in 117:134){
    if(k[i,j] > 1){
      k[i,j] = 1
    }
  }
  print(i)
}

rmc <- c()
for(i in 1:116){
  for(j in 117:134){
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

clusterdata <- k

write.csv(clusterdata, file = "clusterdata.csv", row.names=F)

################### Tasks 3 : Build classifiers ################### 

finaldata <- finalldamx
finaldata <- final_final_lda_myfeature
finaldata1 <- finaldata[,-1]

train <- finaldata[which(finaldata$purpose == "train"),]
test <- finaldata[which(finaldata$purpose == "test"),]

train <- train[,-1]
test <- test[,-1]

datamining <- function(k, model){
  totalt <- matrix(0,10,8)
  totalmytable <- matrix(0,10,8)
  num_levels = length(levels(droplevels(train$class)))
  
  switch(model,
         RandomForest = {
           mr <- randomForest(train$class ~ . , data = train)
           pr <- predict(mr, test[,-ncol(test)])
           t <- table(pr, test$class)
         },
         SVM = {
           ms <- svm(train$class ~ . , data = train, scale = FALSE)
           ps <- predict(ms, test[,-ncol(test)])
           t <- table(ps, test$class)
         },
         naiveBayes = {
           mn <- naiveBayes(train$class ~ . , data = train)
           pn <- predict(mn, test[,-ncol(test)])
           t <- table(pn, test$class)
         }
  )
  
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
  
  
  }
  
  micro_precision <- sum((mytable[,1])/ sum(mytable[,c(1,4)]), na.rm = TRUE)
  

  
  print(t)
  cat("Total Accuracy : ", sum(totalmytable[,7], na.rm = TRUE))
}

datamining("SVM")
datamining("naiveBayes")
datamining("RandomForest")

################### Tasks 4 : clustering ################### 

write.csv(alldatac, file = "alldata_alltopics.csv", row.names=F)

md <- alldatac[,-1]

  print(i)
}

# Determine number of clusters
wss <- (nrow(md)-1)*sum(apply(md,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(md, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


# Ward Hierarchical Clustering
d <- dist(test) # distance matrix
dd <- dist(as.matrix(md[,-99])) # distance matrix
ddd <- dist(as.matrix(md[1:50,-99])) # distance matrix
fit <- hclust(d)
fit2 <- hclust(ddd, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=10) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=10, border="red")

test <- cbind(md10, groups)

a <- test[,103:104]

a[which(a$groups == 1),]

t <- table(a)
t <- as.data.frame(t)

nt <- rbind(t[4,], t[1,], t[7,], t[5,], t[3,], t[9,], t[6,], t[8,], t[10,], t[2,])


# TABLE #
mytable <- matrix(0,10,8)
rownames(mytable) <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", 
                       "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")
colnames(mytable) <- c("TP", "TN", "FN", "FP", "Recall", "Precision", "Accuracy", "F-measure")

for(i in 1:ncol(nt)){
  tp <- nt[i,i]
  tn <- 0
  fn <- sum(nt[i,-i], na.rm = TRUE)
  fp <- sum(nt[-i,i], na.rm = TRUE)
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

sum(mytable[,7], na.rm = TRUE)

colname <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", 
             "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")



clustersdata <- read.csv(file = "featureselectedv2.csv")



# K-Means Clustering with 5 clusters
fit <- kmeans(test[,-103], 10)
library(cluster) 
clusplot(test[,-103], fit$cluster)
library(fpc)
plotcluster(test[,-103], fit$cluster)
plot(fit)
a <- cbind(test, fit$cluster)
a <- a[,103:104]
t <- table(a)
nt <- rbind(t[4,], t[1,], t[7,], t[5,], t[3,], t[9,], t[6,], t[8,], t[10,], t[2,])






# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(clusterdata, km$cluster)


plot(mc, data=test, what=c('classification'),dimens=c(3,4))
table(iris$Species, mc$classification)


cluster <- dbscan(test, eps=0.6, MinPts=4)
plot(cluster, sampleiris)
plot(cluster, sampleiris[,c(1,4)])
# Notice points in cluster 0 are unassigned outliers
table(cluster$cluster, sampleiris$Species)

