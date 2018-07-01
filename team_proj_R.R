# libraries
library("readr")
library("tm")
library("tidyr")
library("tidytext")
library("dplyr")
library("SnowballC")
library("lsa")

# set directory
setwd("~/Documents/NYU/SICSS/nyu-team-project/output/")

# Load data
topics<-read_delim("topics.csv",delim=",",col_names=TRUE)
  topics$X1<-NULL

# generate vector of top ten terms for each topic
top.ten.topics <- data.frame(topic=character(),term=character(),beta=numeric())
 
list.of.topics <- unique(topics$topic)

for(i in 1:length(list.of.topics)){
  subset.topics <- subset(topics,topics$topic==list.of.topics[i])
  order.topics <- subset.topics[order(subset.topics$topic,-subset.topics$beta),] 
# this pulls out the top ten terms, may want to change 
  top.ten <- order.topics[1:10,]
  top.ten.topics <- rbind(top.ten, top.ten.topics)
}

# create vector for each topic with all terms
top.ten.topics <- top.ten.topics[order(top.ten.topics$topic),]
list.of.topics <- unique(top.ten.topics$topic)
topic.terms <-unique(top.ten.topics$term)

topic.array<-spread(top.ten.topics,key=term,value=beta)
topic.array<-cbind(list.of.topics,topic.array)

topic.array[is.na(topic.array)]<-as.character(0)

#write.csv(topic.array,"topic_term_matrix.csv")

###########
# correlate with event description data
##########

# load data
setwd("~/Documents/NYU/SICSS/")
events<-read_delim("2017_Year_Events.csv",delim=",")

events$description <- paste0(events$name, events$description)

# Text Cleaning
tidy_events <- events %>% dplyr::select(event_id, description)
tidy_events$description <- gsub("<.*?>", "", tidy_events$description)
tidy_events <- tidy_events %>% unnest_tokens("word", description)

data("stop_words")
tidy_events <- tidy_events %>% anti_join(stop_words, by=c("word"))

#removing numbers
tidy_events<-tidy_events[-grep("\\b\\d+\\b", tidy_events$word),]

# removing whitespaces
tidy_events$word <- gsub("\\s+","",tidy_events$word)

# Stemming
tidy_events <- tidy_events %>% mutate_at("word", funs(wordStem((.), language="en")))

# Remove out the terms that are not relevant to our topics
topical.events<-subset(tidy_events,tidy_events$word %in% topic.terms)

# Aggregate by the number of each term within each event.
topical.events.agg <- group_by(topical.events, event_id, word) %>% count(event_id, word)

# Convert from long to wide matrix
topic.events.agg.w<-spread(topical.events.agg,key=word,value=n)
topic.events.agg.w[is.na(topic.events.agg.w)]<-0

# count total number of instances of word
topic.events.agg.w$total<-NA
topic.events.agg.w<-topic.events.agg.w[,c(1,ncol(topic.events.agg.w),2:(ncol(topic.events.agg.w)-1))]
topic.events.agg.w$total<-rowSums(topic.events.agg.w[,c(3:ncol(topic.events.agg.w))])

# calculate the proportion of edits made of each type w/in each event.
topic.term.prop <- topic.events.agg.w
topic.term.prop[,3:ncol(topic.events.agg.w)]<-sweep(topic.term.prop[,3:ncol(topic.events.agg.w)],1,topic.term.prop$total,"/")

#write.csv(topic.term.prop,"topic_term_proportion.csv")

# cosine similarity matrix
topic.matrix<-topic.array[,2:ncol(topic.array)]
event.matrix<-topic.term.prop[,3:ncol(topic.term.prop)]

topic.term.prop$primary.topic<-NA
topic.term.prop<-topic.term.prop[,c(1,2,ncol(topic.term.prop),3:(ncol(topic.term.prop)-1))]

for(i in 1:nrow(topic.term.prop)){
  event<-as.vector(as.numeric(topic.term.prop[i,4:ncol(topic.term.prop)]))
  temp.mat<-data.frame(topic=numeric(),cos.sim=numeric())
  colnames(temp.mat)<-c("topic","cos.sim")
  for(j in 1:nrow(topic.array)){
    topic<-as.numeric(topic.array[j,2:ncol(topic.array)])
    similarity<-cosine(event,topic)
    
    temp.mat[j,1]<-j
    temp.mat[j,2]<-similarity
  }
  primary.topic<-temp.mat[max(temp.mat$cos.sim)==temp.mat$cos.sim,]$topic
  topic.term.prop$primary.topic[i]<-primary.topic
}


### calculate shannon entropy based on the diversity of topics found in each city
#topic.term.prop<-topic.term.prop[complete.cases(topic.term.prop),] # this was done to expedite development by looking at a subset

primary.topics<-topic.term.prop[,c("event_id","primary.topic")]
cities<-events[,c("event_id","city")]
  
city.topics<-merge(cities,primary.topics,by="event_id")

city.topics.count<-as.data.frame(xtabs(~city.topics$city+city.topics$primary.topic))
  colnames(city.topics.count)<-c("city","topic","count")

cities.count<-unique(city.topics.count$city)
topic.count<-unique(topic.array$list.of.topics)
num.topics<-nrow(topic.array)

# normalize the number of events within each city
city.topics.count$total.events<-NA

for(i in 1:length(cities.count)){
  c<-cities.count[i]
  num.events<-sum(city.topics.count[city.topics.count$city==c,]$count)
  city.topics.count[city.topics.count$city==c,]$total.events<-num.events
}

city.topics.count$normalized.count<-city.topics.count$count/city.topics.count$total.events

# add 0.00000001 to 0 values
city.topics.count$normalized.count[city.topics.count$normalized.count==0]<-0.00000001

# calulate shannon entropy for each normalized 
city.entropy<-data.frame(matrix(ncol=2,nrow=length(cities.count)))
  colnames(city.entropy)<-c("city","shannon.entropy")
  city.entropy$city<-cities.count

for(i in 1:length(cities.count)){
  city<-cities.count[i]
  
  p<-(city.topics.count[city.topics.count$city==city,]$normalized.count)
  entropy<-sum(p*log(p))
  
  city.entropy[city.entropy$city==city,]$shannon.entropy<-entropy
}


