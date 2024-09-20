# This prepares the data used in the manuscript. it pulls the complete
# dataset from the "Tropical Bibliometrics" repository, as well as two functions
# used to edit and clean the data.


# load libraries ----------------------------------------------------------
library(here)
library(tidyverse)
library(tidytext)

# pull data used in MS from the data repository  --------------------------

# create a folder to download data

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


# title words data set
urlfile_titlewords <- ("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/tw_clean.csv")
titlewords <- read_csv(url(urlfile_titlewords))
write_csv(titlewords, here("data", "data_original", "tw_clean.csv"))


# journal titles data set
urlfile_jrnls <- ("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/jrnls.csv")
jrnls <- read_csv(url(urlfile_jrnls))
write_csv(jrnls, here("data", "data_original", "jrnls.csv"))


# system-related terms
urlfile_system <- "https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/system.csv"
system <- read_csv(url(urlfile_system))
write_csv(system, here("data", "data_original", "system.csv"))

# keywords dataset
urlfile_keywords <- ("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/keywords.csv")
keywords <- read_csv(url(urlfile_keywords))
write_csv(keywords, here("data", "data_original", "keywords.csv"))

# versions of the data sets used in this MS
# jrnls version
urlfile_v_jrnls <- ("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/version_jrnls.txt")
v_jrnls <- read_csv(url(urlfile_v_jrnls), col_names = FALSE)
write_delim(v_jrnls, here("data", "data_original", "version_jrnls.txt"))


# keywords version
urlfile_v_keywords <- ("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/version_keywords.txt")
v_keywords <- read_csv(url(urlfile_v_keywords), col_names = FALSE)
write_delim(v_keywords, here("data", "data_original", "version_keywords.txt"))

# system terms version
urlfile_v_system <- ("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/version_system.txt")
v_system <- read_csv(url(urlfile_v_system), col_names = FALSE)
write_delim(v_system, here("data", "data_original", "version_system.txt"))

# title words version
urlfile_v_tw_clean <- ("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/data/data_archive/version_tw_clean.txt")
v_tw_clean <- read_csv(url(urlfile_v_tw_clean), col_names = FALSE)
write_delim(v_tw_clean, here("data", "data_original", "version_tw_clean.txt"))

# bind into a single file version df
versions_for_ms <- bind_rows(
  v_system,
  v_tw_clean,
  v_keywords,
  v_jrnls
)

names(versions_for_ms) <- c("version", "date", "file")

# save a df listing the versions used in this ms
write_csv(versions_for_ms, here("data", "data_ms", "versions_for_ms.csv"))

#  read and save functions to clean up bigrams -------------------------------------
keyword_editor <- ("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/code/code_data_cleaning/keyword_editor.R")
keyword_editor <- read_lines(url(keyword_editor))
write_lines(keyword_editor, here("code", "keyword_editor.R"))

keyword_depluralizer <- ("https://raw.githubusercontent.com/BrunaLab/tropical_bibliometrics/main/code/code_data_cleaning/keyword_depluralizer.R")
keyword_depluralizer <- read_lines(url(keyword_depluralizer))
write_lines(keyword_depluralizer, here("code", "keyword_depluralizer.R"))

# reduce the complete data set to what is analyzed in this MS -------------

# Note that 'revista de biologia tropical' is excluded from this analysis 

# journal titles
journal_titles <- read_csv(here("data", "data_original", "jrnls.csv")) %>%
  filter(SO != "rbt") %>% 
  write_csv(here("data", "data_ms", "journal_titles.csv"))
  

# system terms for MS: using GEOGRAPHIC only (not species or study system)
system_list <- read_csv(here("data", "data_original", "system.csv")) %>%
  filter(geo == TRUE) %>%
  select(system,geo) %>% 
  write_csv(here("data", "data_ms", "system_list.csv"))


# title words for MS
tw_ms <- read_csv(here("data", "data_original", "tw_clean.csv")) %>%
  filter(SO != "rbt") %>%
  inner_join(journal_titles) %>%
  relocate(title, .after = SO) %>%
  mutate(title = as.factor(title)) %>%
  mutate(pub_cat = as.factor(pub_cat_2)) %>%
  mutate(jrnl_cat = as.factor(jrnl_cat)) %>%
  mutate(index = row_number()) %>%
  mutate(pub_cat_2 = as.factor(pub_cat_2)) %>%
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>%
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>%
  filter(SO != "rbt") %>%
  select(refID,
         PY,
         final,
         SO,
         jrnl_cat,
         title,
         pub_cat_2,
         system) %>% 
  write_csv(here("data", "data_ms", "tw_ms.csv"))

# keywords for MS
kw_ms <- read_csv(here("data", "data_original", "keywords.csv")) %>%
  inner_join(journal_titles) %>%
  relocate(title, .after = SO) %>%
  mutate(title = as.factor(title)) %>%
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>%
  mutate(system = if_else((final %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>%
  filter(SO != "rbt") %>%
  select(refID,
         PY,
         final,
         pub_cat_2,
         jrnl_cat,
         SO,
         system) %>%
  write_csv(here("data", "data_ms", "kw_ms.csv"))



# generate and rank the title bigrams -------------------------------------


# prep titles (ie, title words) for n-gram extraction
extract_ngram_data <- function(tw) {
  ngram_data <- tw_ms %>%
    select(refID, PY, SO, pub_cat_2, jrnl_cat, final) %>%
    arrange(refID, PY, SO) %>%
    group_by(refID, PY, SO) %>%
    mutate(word = row_number()) %>%
    relocate(word, .before = final) %>%
    mutate(place = "place") %>%
    relocate(place, .before = word)

  ngram_data <- ngram_data %>%
    replace_na(list(final = "deleteme")) %>%
    pivot_wider(
      names_from = c(place, word),
      values_from = final,
      values_fn = list, values_fill = list("deleteme")
    ) %>%
    ungroup()

  last_col <- ngram_data %>% select(last_col())
  last_col <- last(names(ngram_data))
  ngram_data <- ngram_data %>%
    unite("tw", place_1:last_col, na.rm = FALSE, remove = TRUE, sep = " ") %>%
    ungroup()
  return(ngram_data)
}

# data used to generate bigrams
ngram_data <- extract_ngram_data(tw_ms)


# generate bigrams

tw_bigrams <- ngram_data %>%
  select(tw, refID) %>%
  unnest_tokens(bigram, tw, token = "ngrams", n = 2) %>%
  distinct(refID, bigram)

bigrams_separated <- tw_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(str_detect("deleteme", word1, negate = TRUE)) %>%
  filter(str_detect("deleteme", word2, negate = TRUE))


bigrams_filtered <- bigrams_filtered %>%
  mutate(word1_first = substring(word1, 1, 1)) %>%
  mutate(word2_first = substring(word2, 1, 1)) %>%
  mutate(number = case_when(
    (str_detect(word1_first, ".*[0-9].*")) == TRUE ~ "number",
    (str_detect(word2_first, ".*[0-9].*")) == TRUE ~ "number",
    TRUE ~ "keep"
  )) %>%
  filter(number == "keep") %>%
  select(-number, -word1_first, -word2_first) %>%
  unite("bigram", word1:word2, sep = " ")

# bind keywords and bigrams -----------------------------------------------

tw_info <- tw_ms %>%
  select(
    refID,
    PY,
    SO,
    title,
    pub_cat_2,
    jrnl_cat,
    pub_cat_2
  ) %>%
  distinct()


bigrams <- bigrams_filtered %>%
  rename(original = bigram) %>%
  left_join(tw_info, by = "refID") %>%
  mutate(term_cat = "bigram") 
# %>%
#   select(-pub_cat_2)

# systematize the bigrams and consolidate plurals/singulars of same term

source(here("code", "keyword_editor.R"))
clean_bigrams <- keyword_editor(bigrams)
summary(clean_bigrams$original == clean_bigrams$edited)
source(here("code", "keyword_depluralizer.R"))
clean_bigrams <- keyword_depluralizer(clean_bigrams)
summary(clean_bigrams$edited == clean_bigrams$original) 


clean_bigrams <- clean_bigrams %>%
  mutate(term = edited) %>%
  mutate(PY = as.numeric(PY)) %>% 
  rename(pub_cat=pub_cat_2) %>% 
  select(-title)


# save the bigrams
write_csv(clean_bigrams, here("data", "data_ms", "clean_bigrams.csv"))


# Ranking the bigrams

# rank separately in each geographic category (tropical and nontropical)

# total no of bigrams
bigrams_count <- clean_bigrams %>%
  distinct(term) %>%
  tally()

# bigrams: NON-tropical
NonTrop_bigrams <- clean_bigrams %>%
  filter(pub_cat == "general") %>%
  ungroup() %>%
  select(term) %>%
  count(term, sort = TRUE) %>%
  mutate(perc = n / sum(n) * 100) %>%
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) %>%
  mutate(cat = "non-tropical")

# bigrams: tropical
Trop_bigrams <- clean_bigrams %>%
  filter(pub_cat == "tropical") %>%
  ungroup() %>%
  select(term) %>%
  count(term, sort = TRUE) %>%
  mutate(perc = n / sum(n) * 100) %>%
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) %>%
  mutate(cat = "tropical")

# all bigrams together
rankings_pub <- bind_rows(Trop_bigrams, NonTrop_bigrams) %>%
  mutate(system = if_else((term %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system)) %>%
  group_by(cat) %>%
  mutate(rank_perc = rank(desc(perc), ties.method = "random")) %>%
  write_csv(here("data", "data_ms", "ranked_bigrams.csv"))

# put kw & bigrams together as "terms" ------------------------------------
terms <- kw_ms %>%
  select(refID,
    term = final,
    pub_cat = pub_cat_2,
    jrnl_cat,
    SO,
    PY
  ) %>%
  mutate(term_cat = "kw")


terms <- bind_rows(terms, clean_bigrams)


terms <- terms %>%
  mutate(term = case_when(
    term == "lifehistory" ~ "life history",
    .default = as.character(term)
  ))
terms <- terms %>%
  mutate(system = if_else((term %in% system_list$system == TRUE), "Y", "N")) %>%
  mutate(system = as.factor(system))

pubs_bigrams_terms <- terms %>%
  filter(term_cat == "bigram") %>%
  select(refID, term_cat, PY, SO, pub_cat) %>%
  distinct()
pubs_kw_terms <- terms %>%
  filter(term_cat == "kw") %>%
  select(refID, term_cat, PY, SO, pub_cat) %>%
  distinct()

# use only the articles that had both titles and KW to avoid biases

all_pubs_terms <- full_join(pubs_kw_terms, pubs_bigrams_terms, by = "refID")
complete_allpubs <- drop_na(all_pubs_terms)

terms <- terms %>%
  filter(refID %in% complete_allpubs$refID)


terms <- terms %>%
  rename(final = term) %>% 
  relocate(c(SO,PY),.after=refID) %>% 
  relocate(system,.after=jrnl_cat) %>% 
  relocate(term_cat,.before=1)

write_csv(terms, here("data", "data_ms", "terms.csv"))
