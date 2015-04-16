setwd("~/Google Drive/MSc Data Analytics/Core/CS909 Data Mining/exercises/exercise8")

mydata <- read.csv(file="reutersCSV.csv",header=T,sep=",")

r <- nrow(mydata)
c <- ncol(mydata)

##### Pre-Prosessing ##### 

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("tm")

require("NLP")
require("tm")
require("openNLP")
require("NLP")
require("openNLPmodels.en")

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


s <-rmdata[1,123]
s <- as.String(s)
# 3. 
# Replace Link
# x <- "asdfsd sfkdak dksfa sdfsdkf asd fasdf asdf . fsa http://stat.umn.edu:80/xyz , sdfasdf,sdf,asdfasd sdfasdf,"
s <- gsub("http\\S+\\s*", "LINK", s)

# Remove puncuation
s <- gsub("/", " ", s) 
s <- gsub("[^[:alnum:][:space:]']", "", s)

## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))

# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a2 <- annotate(s, pos_tag_annotator, a2)
a2


## Entity recognition for persons.
# entity_annotator <- Maxent_Entity_Annotator(kind = "date")
entity_annotator <- Maxent_Entity_Annotator(kind = "organization")
entity_annotator(s,a2)
t <- s[entity_annotator(s,a2)]
annotate(s, entity_annotator, a2)
for(i in 1:length(t)){
  s <- gsub(t[i], "ORG", s)
}

a2w <- subset(a2, type == "word")
tags <- sapply(a2w$features, `[[`, "POS")
tags
table(tags)

# Repalce Num
s <- gsub("[[:digit:]]+", "NUM", s)

# 1. Tokenize: divide into words (unigrams)

spans <- whitespace_tokenizer(s)
# spans <- wordpunct_tokenizer(s)
spans
s[spans]

install.packages("koRpus")
require("koRpus")
tagged.results <- treetag(c("run", "ran", "running"), treetagger="manual", format="obj",
                          TT.tknz=FALSE , lang="en",
                          TT.options=list(path="./TreeTagger", preset="en"))
tagged.results@TT.res

setDict("/Users/yuesun/Downloads/dict/")

filter <- getTermFilter("StartsWithFilter", "running", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)


require("NLP")
## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a3 <- annotate(s, pos_tag_annotator, a2)
a3

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

