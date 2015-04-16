setwd("~/Google Drive/MSc Data Analytics/Core/CS909 Data Mining/exercises/exercise8")

mydata <- read.csv(file="reutersCSV.csv",header=T,sep=",")

r <- nrow(mydata)
c <- ncol(mydata)

##### Pre-Prosessing ##### 

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("tm")
install.packages("koRpus")

require("NLP")
require("tm")
require("openNLP")
require("NLP")
require("openNLPmodels.en")
require("koRpus")

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

my_data <- read.csv(file="cleanedData.csv",header=T,sep=",")

s <-my_data[1,123]
s <- as.String(s)
s <- "I am so excited by the great robot designs from the Newsround audience! http://www.bbc.co.uk/i/b03x47dm/ Send in your design: http://www.bbc.co.uk/newsround/26490802"

##### 1. Tokenize: divide into words (unigrams)

s <- gsub("/", " ", s)  # replace / to space 
spans <- whitespace_tokenizer(s)
# spans <- wordpunct_tokenizer(s)
spans
tokens <- s[spans]

##### 2. Remove punctuation, replace links

tokens <- gsub("[^[:alnum:][:space:]']", "", tokens)
tokens <- gsub("http\\S+\\s*", "LINK", tokens)

##### 3. POS tagging & Lemmatisation

# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
# 
# pos_tag_annotator <- Maxent_POS_Tag_Annotator()
# a2 <- annotate(tokens, pos_tag_annotator, a2)
# a2

tagged.results <- treetag(tokens, treetagger="manual", format="obj",
                          lang="en",
                          stopwords = tm::stopwords("SMART"),
                          TT.options=list(path="./tree-tagger", preset="en"))

lemma <- tagged.results@TT.res$lemma
tagged.results@TT.res[which]

##### 4. Remove stop words:

rmtokens <- tagged.results@TT.res[which(tagged.results@TT.res$tag == "PP" |
                                          tagged.results@TT.res$tag == "DT" | 
                                          tagged.results@TT.res$tag == "IN" | 
                                          tagged.results@TT.res$tag == "PP$"),]

lemma[c(-as.numeric(row.names(rmtokens)))]

## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
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



# setDict("/Users/yuesun/Downloads/dict/")
# 
# filter <- getTermFilter("StartsWithFilter", "car", TRUE)
# terms <- getIndexTerms("NOUN", 5, filter)
# sapply(terms, getLemma)

## Need sentence and word token annotations.
# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
# pos_tag_annotator <- Maxent_POS_Tag_Annotator()
# pos_tag_annotator
# a3 <- annotate(s, pos_tag_annotator, a2)
# a3

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

