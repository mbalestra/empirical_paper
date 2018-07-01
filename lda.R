library(stringr)
library(readr)
library(dplyr)
library(classInt)
require(kohonen)
library(deldir)
library(spatstat)
library(spatstat.utils)
library(anocva)
library(tm)
library(ldatuning)
library(topicmodels)
library(tidytext)
library(SnowballC)
library(data.table)

# Basic loading
setwd("~/nyu-team-project/data/")
# setwd("/Users/myeong/git/nyu-team-project/data/")
events <- read_delim("2017_Year_Events.csv", delim = ",",col_names = TRUE ) 
# groups = read_delim("2017_Year_Groups.csv", delim = ",",col_names = TRUE )

# Select only Washington DC for testing
events$city <- as.factor(events$city)
events <- events[events$city == "Washington" | events$city == "Pittsburgh",]

events["date_1"] <- lapply(events["date"],function(x) as.Date(x, "%d %B %Y"))
events$month <- month(events$date_1)
events$year <- year(events$date_1)
events$day <- mday(events$date_1)
events$date <- paste(events$year, str_pad(events$month, width=2, side="left", pad="0"), str_pad(events$day, width=2, side="left", pad="0"),sep="")
events$date <- as.integer(events$date)

events$description <- paste0(events$name, events$description)

print("Event Data Loaded")

# Text Cleaning
tidy_events <- events %>% dplyr::select(event_id, description)
tidy_events$description <- gsub("<.*?>", "", tidy_events$description)
tidy_events <- tidy_events %>% unnest_tokens("word", description)
# tidy_events %>% count(word) %>% arrange(desc(n))

data("stop_words")
tidy_events <- tidy_events %>% anti_join(stop_words, by=c("word"))

#removing numbers
tidy_events<-tidy_events[-grep("\\b\\d+\\b", tidy_events$word),]

# removing whitespaces
tidy_events$word <- gsub("\\s+","",tidy_events$word)

# Stemming
tidy_events <- tidy_events %>% mutate_at("word", funs(wordStem((.), language="en")))

# Document Term Matrix
tidy_events_DTM <- tidy_events %>% count(event_id, word) %>% cast_dtm(event_id, word, n)

print("Term Matrix Generated...")

# Determining the number of K
# result <- FindTopicsNumber(
#   tidy_events_DTM,
#   topics = seq(from = 20, to = 70, by = 5),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# 
# write.csv(result, file="../output/griffith_output.csv")
# print("K determined...")

# FindTopicsNumber_plot(result)
# range <- result[result$topics >= 20 & result$topics <= 70,c("topics", "Griffiths2004")]
# K <- range[max(range$Griffiths2004)==range$Griffiths2004,]$topics


# LDA
event_topic_model<-LDA(tidy_events_DTM, k=40, control = list(seed = 321)) 
event_topics <- tidy(event_topic_model, matrix = "beta")
write.csv(event_topics, file="../output/topics.csv")
print("Done.")
