setwd("~/Google Drive/MSc Data Analytics/Core/CS909 Data Mining/exercises/exercise8")

mydata <- read.csv(file="reutersCSV.csv",header=T,sep=",")

r <- nrow(mydata)
c <- ncol(mydata)

##### Pre-Prosessing ##### 

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("tm")
install.packages("koRpus")
install.packages("SnowballC")

require("NLP")
require("tm")
require("openNLP")
require("NLP")
require("openNLPmodels.en")
require("koRpus")
require("SnowballC")

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
  ###################################
  
  if(my_data[ii,123] != ""){
    
    s <- my_data[ii,123]
    s <- as.String(s)
    # s <- "I am so excited by the great robot designs from the Newsround audience! http://www.bbc.co.uk/i/b03x47dm/ Send in your design: http://www.bbc.co.uk/newsround/26490802"
    
    ##### 1. Tokenize: divide into words (unigrams)
    
    s <- gsub("/", " ", s)  # replace / to space 
    spans <- whitespace_tokenizer(s)
    # spans <- wordpunct_tokenizer(s)
    # spans
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
    
    # ss <- removePunctuation(tokens, preserve_intra_word_dashes = FALSE)
    
    tagged.results <- treetag(tokens, treetagger="manual", format="obj",
                              lang="en",
                              #                           TT.tknz = TRUE,
                              stopwords = tm::stopwords("SMART"),
                              stemmer = SnowballC::wordStem,
                              TT.options=list(path="./tree-tagger", preset="en"))
    
    lemma <- tagged.results@TT.res$lemma
    
    ##### Remove stop words ##### 
    
    non_stopwords <- tagged.results@TT.res[which(tagged.results@TT.res$stop == FALSE),]
    
    # nrow(non_stopwords)
    for(i in 1:nrow(non_stopwords)){
      if(non_stopwords$lemma[i] == "@card@"){
        non_stopwords$lemma[i] <- non_stopwords$stem[i]
      }
      if(non_stopwords$lemma[i] == "<unknown>"){
        non_stopwords$lemma[i] <- non_stopwords$stem[i]
      }
    }
    
    
    
    
    ##### 4. Remove stop words:
    
    # rmtokens <- tagged.results@TT.res[which(tagged.results@TT.res$tag == "PP" |
    #                                           tagged.results@TT.res$tag == "DT" | 
    #                                           tagged.results@TT.res$tag == "IN" | 
    #                                           tagged.results@TT.res$tag == "PP$"),]
    # 
    # lemma[c(-as.numeric(row.names(rmtokens)))]
    
    
    ##### 5. NE recognition:
    
    ss <- paste(non_stopwords$lemma, collapse = " ")
    ss <- as.String(ss)
    
    # sent_token_annotator <- Maxent_Sent_Token_Annotator()
    # word_token_annotator <- Maxent_Word_Token_Annotator()
    a2 <- annotate(ss, list(sent_token_annotator, word_token_annotator))
    
    # person_annotator <- Maxent_Entity_Annotator(kind = "person")
    # annotate(ss, person_annotator, a2)
    # person_annotator(ss,a2)
    if(length(person_annotator(ss,a2)) != 0){
      pt <- ss[person_annotator(ss,a2)]
    }
    
    # a2 <- annotate(ss, list(sent_token_annotator, word_token_annotator))
    # location_annotator <- Maxent_Entity_Annotator(kind = "location")
    # annotate(ss, location_annotator, a2)
    # location_annotator(ss,a2)
    if(length(location_annotator(ss,a2)) != 0){
      lt <- ss[location_annotator(ss,a2)]
    }
    # a2 <- annotate(ss, list(sent_token_annotator, word_token_annotator))
    # organization_annotator <- Maxent_Entity_Annotator(kind = "organization")
    # annotate(ss, organization_annotator, a2)
    # organization_annotator(ss,a2)
    if(length(organization_annotator(ss,a2)) != 0){
      ot <- ss[organization_annotator(ss,a2)]
    }
    
    # a2 <- annotate(ss, list(sent_token_annotator, word_token_annotator))
    # date_annotator <- Maxent_Entity_Annotator(kind = "date")
    # annotate(ss, date_annotator, a2)
    # date_annotator(ss,a2)
    if(length(date_annotator(ss,a2)) != 0){
      dt <- ss[date_annotator(ss,a2)]
    }
    # 
    # annotations <- annotate(ss, list(sent_token_annotator, 
    #                                  word_token_annotator,
    #                                  person_annotator,
    #                                  location_annotator,
    #                                  organization_annotator,
    #                                  date_annotator))
    
    if(length(person_annotator(ss,a2)) != 0){  
      if(length(pt) == 1){
        ss <- gsub(pt, "--------PER", ss)
      }else{
        for(i in 1:length(pt)){
          ss <- gsub(pt[i], "--------PER", ss)
        }
      }
    }
    
    if(length(location_annotator(ss,a2)) != 0){
      if(length(lt) == 1){
        ss <- gsub(lt, "+++++++++LOC", ss)
      }else{
        for(i in 1:length(lt)){
          ss <- gsub(lt[i], "+++++++++LOC", ss)
        }
      }
    }
    
    if(length(organization_annotator(ss,a2)) != 0){
      if(length(ot) == 1){
        ss <- gsub(ot, ".............ORG", ss)
      }else{
        for(i in 1:length(ot)){
          ss <- gsub(ot[i], ".............ORG", ss)
        }
      }
    }
    
    if(length(date_annotator(ss,a2)) != 0){
      if(length(dt) == 1){
        ss <- gsub(dt, "********DAT", ss)
      }else{
        for(i in 1:length(dt)){
          ss <- gsub(dt[i], "********DAT", ss)
        }
      }
    }
    
    
    ss <- gsub("[[:digit:]]+", "^^^^^^^^^^^NUM", ss)
    
    
    my_data[ii,125] <- ss
    
    print(ii)
  }
###################################
}






date_annotator <- Maxent_Entity_Annotator(kind = "date")
organization_annotator <- Maxent_Entity_Annotator(kind = "organization")
location_annotator <- Maxent_Entity_Annotator(kind = "location")


a2 <- annotate(s, list(sent_token_annotator, 
                       word_token_annotator, 
                       date_annotator, 
                       organization_annotator, 
                       location_annotator, 
                       person_annotator))


entity_annotator <- Maxent_Entity_Annotator()
entity_annotator

annotate(ss, entity_annotator, a2)

entity_annotator(ss,a2)
t <- s[entity_annotator(ss,a2)]

annotate(ss, organization_annotator, a2)
for(i in 1:length(t)){
  ss <- gsub(t[i], "ORG", ss)
}



## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(ss, list(sent_token_annotator, word_token_annotator))
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a2 <- annotate(s, pos_tag_annotator, a2)
a2

## Entity recognition for persons.
entity_annotator <- Maxent_Entity_Annotator(kind = "date")
entity_annotator <- Maxent_Entity_Annotator(kind = "organization")
entity_annotator <- Maxent_Entity_Annotator(kind = "location")
entity_annotator <- Maxent_Entity_Annotator(kind = "person")
entity_annotator(ss,a2)
t <- ss[entity_annotator(ss,a2)]
annotate(s, entity_annotator, a2)
for(i in 1:length(t)){
  s <- gsub(t[i], "ORG", s)
}

a2w <- subset(a2, type == "word")
tags <- sapply(a2w$features, `[[`, "POS")
tags
table(tags)

# Repalce Num
tokens <- gsub("[[:digit:]]+", "NUM", tokens)



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

