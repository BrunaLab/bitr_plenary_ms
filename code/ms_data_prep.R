# # Prep the file of ALL keywords and titlewords for use in BITR MS.
# 


# load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(tidytext)
# 
# 
if (!dir.exists("./data/data_ms")) {
  dir.create("./data/data_ms")
} else {
  print("lucky you...the folder already exists!") # placeholder
}
#
if (!dir.exists("./data/data_original")) {
  dir.create("./data/data_original")
} else {
  print("lucky you...the folder already exists!") # placeholder
}


# title words
urlfile_titlewords<-("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/tw_clean.csv")
titlewords<-read_csv(url(urlfile_titlewords))
write_csv(titlewords, here("data","data_original","tw_clean.csv"))


# journal titles 
urlfile_jrnls<-("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/jrnls.csv")
jrnls<-read_csv(url(urlfile_jrnls))
write_csv(jrnls, here("data","data_original","jrnls.csv"))


# system-related terms
urlfile_system<-"https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/system.csv"
system<-read_csv(url(urlfile_system))
write_csv(system, here("data","data_original","system.csv")) 

# keywords
urlfile_keywords<-("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/keywords.csv")
keywords<-read_csv(url(urlfile_keywords))
write_csv(keywords, here("data","data_original","keywords.csv"))

# versions of the datasets
# v jrnls
urlfile_v_jrnls<-("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/version_jrnls.txt")
v_jrnls<-read_csv(url(urlfile_v_jrnls),col_names = FALSE)
write_delim(v_jrnls, here("data","data_original","version_jrnls.txt"))


# v keywords
urlfile_v_keywords<-("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/version_keywords.txt")
v_keywords<-read_csv(url(urlfile_v_keywords),col_names = FALSE)
write_delim(v_keywords, here("data","data_original","version_keywords.txt"))

# v system terms
urlfile_v_system<-("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/version_system.txt")
v_system<-read_csv(url(urlfile_v_system),col_names = FALSE)
write_delim(v_system, here("data","data_original","version_system.txt"))

# v tw_clean
urlfile_v_tw_clean<-("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/version_tw_clean.txt")
v_tw_clean<-read_csv(url(urlfile_v_tw_clean),col_names = FALSE)
write_delim(v_tw_clean, here("data","data_original","version_tw_clean.txt"))

v_system
v_tw_clean
v_keywords
v_jrnls
# data file version
versions_for_ms<-bind_rows(v_system,
                           v_tw_clean,
                           v_keywords,
                           v_jrnls) 

names(versions_for_ms)<-c("version", "date", "file")

write_csv(versions_for_ms,here("data","data_ms","versions_for_ms.csv"))

# process data for use in MS  ---------------------------------------------

# system terms for MS 
system_list <- read_csv(here("data","data_original","system.csv")) %>% 
  filter(geo==TRUE) %>% 
  write_csv(here("data","data_ms","system_list.csv")) 

# title words for MS 
journal_titles <- read_csv(here("data","data_original","jrnls.csv")) %>% 
  write_csv(here("data","data_ms","journal_titles.csv")) 

tw_ms <- read_csv(here("data","data_original","tw_clean.csv")) %>% 
  filter(SO!="rbt") %>% 
  inner_join(journal_titles) %>% 
  relocate(title,.after=SO) %>% 
  mutate(title=as.factor(title)) %>% 
  mutate(pub_cat=as.factor(pub_cat_2)) %>% 
  mutate(jrnl_cat=as.factor(jrnl_cat)) %>% 
  mutate(index = row_number()) %>% 
  mutate(pub_cat_2=as.factor(pub_cat_2)) %>% 
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  write_csv(here("data","data_ms","tw_ms.csv")) 

# keywords for MS
kw_ms<-read_csv(here("data","data_original","keywords.csv")) %>% 
  inner_join(journal_titles) %>%
  relocate(title,.after=SO) %>%
  mutate(title=as.factor(title)) %>%
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  write_csv(here("data","data_ms","kw_ms.csv")) 


# n-grams for use in MS

# prep titles for n-gram extraction
extract_ngram_data <- function(tw) {
  ngram_data<-tw_ms %>% 
    select(refID,PY,SO,pub_cat_2,jrnl_cat,final) %>%  
    arrange(refID,PY,SO) %>% 
    group_by(refID,PY,SO) %>% mutate(word=row_number()) %>% 
    relocate(word,.before=final) %>% 
    mutate(place="place") %>% 
    relocate(place,.before=word) 
  
  ngram_data<-ngram_data %>% replace_na(list(final= "deleteme")) %>% 
    pivot_wider(names_from = c(place,word),
                values_from = final,
                values_fn = list, values_fill = list("deleteme")
    ) %>%
    ungroup()
  
  last_col<-ngram_data %>% select(last_col())
  last_col<-last(names(ngram_data))
  ngram_data<- ngram_data %>% 
    unite("tw", place_1:last_col, na.rm = FALSE, remove = TRUE, sep= " ") %>% 
    ungroup()
  return(ngram_data)
}


# data used to generate bigrams
ngram_data<-extract_ngram_data(tw_ms)
ngram_data_gen<-ngram_data %>% filter(pub_cat_2=="general")
ngram_data_trop<-ngram_data %>% filter(pub_cat_2=="tropical")


# generate bigrams
generate_bigrams <- function(ngram_data) {
  tw_bigrams <- ngram_data %>% 
    select(tw) %>% 
    unnest_tokens(bigram, tw, token = "ngrams", n = 2) %>% 
    count(bigram, sort = TRUE) 
  bigrams_separated <- tw_bigrams %>% 
    separate(bigram, c("word1", "word2"), sep = " ")
  bigrams_filtered <- bigrams_separated  %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    arrange(desc(n)) %>% 
    filter(str_detect("deleteme",word1,negate=TRUE)) %>% 
    filter(str_detect("deleteme",word2,negate=TRUE)) 
  
  
  bigrams_filtered<-bigrams_filtered %>% 
    mutate(word1_first=substring(word1, 1, 1)) %>% 
    mutate(word2_first=substring(word2, 1, 1)) %>% 
    mutate(number=case_when(
      (str_detect(word1_first,".*[0-9].*")) == TRUE ~ "number",
      (str_detect(word2_first,".*[0-9].*")) == TRUE ~ "number",
      TRUE ~ "keep")) %>% 
    filter(number=="keep") %>% 
    select(-number,-word1_first,-word2_first) %>% 
    mutate(perc=n/sum(n)*100)
  return(bigrams_filtered)
}

generate_bigrams <- function(ngram_data) {
  
  n_titles<-ngram_data %>% summarize(n=n_distinct(refID))
  
  tw_bigrams <- ngram_data %>% 
    select(tw,refID) %>% 
    unnest_tokens(bigram, tw, token = "ngrams", n = 2) %>% 
    distinct(refID,bigram)
  
  bigrams_separated <- tw_bigrams %>% 
    separate(bigram, c("word1", "word2"), sep = " ")
  bigrams_filtered <- bigrams_separated  %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    filter(str_detect("deleteme",word1,negate=TRUE)) %>% 
    filter(str_detect("deleteme",word2,negate=TRUE)) 

bigrams_filtered<-bigrams_filtered %>% 
    mutate(word1_first=substring(word1, 1, 1)) %>% 
    mutate(word2_first=substring(word2, 1, 1)) %>% 
    mutate(number=case_when(
      (str_detect(word1_first,".*[0-9].*")) == TRUE ~ "number",
      (str_detect(word2_first,".*[0-9].*")) == TRUE ~ "number",
      TRUE ~ "keep")) %>% 
    filter(number=="keep") %>% 
    select(-number,-word1_first,-word2_first)  %>% 
    group_by(word1,word2) %>% 
    summarize(n=n()) %>% 
    arrange(desc(n)) %>% 
    mutate(perc=n/n_titles$n*100)
  return(bigrams_filtered)
}


# bigrams - all pubs
bigrams<-generate_bigrams(ngram_data)  
write_csv(bigrams,here("data","data_ms","all_bigrams.csv")) 



# bigrams ranked withing geographic category (tropical and nontropical)
bigrams_count<- bigrams %>% 
  unite("bigram",word1:word2, sep = " ") %>% 
  tally()

# bigrams: NON-tropical
NonTrop_bigrams<-generate_bigrams(ngram_data_gen) %>% 
  mutate(cat="non-tropical")  %>%  
  mutate(bigram=paste(word1,word2,sep=" ")) %>% 
  ungroup() %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) 

# bigrams: tropical
Trop_bigrams<-generate_bigrams(ngram_data_trop) %>% 
  mutate(cat="tropical")  %>% 
  mutate(bigram=paste(word1,word2,sep=" ")) %>% 
  ungroup() %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) 

## all bigrams together
rankings_pub<-bind_rows(Trop_bigrams,NonTrop_bigrams) %>% 
  mutate(system = if_else((bigram %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>% 
  group_by(cat) %>% 
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) %>% 
  write_csv(here("data","data_ms","ranked_bigrams.csv")) 

