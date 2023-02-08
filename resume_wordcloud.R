library(officer)
library(ggwordcloud)
library(dplyr)
library(tidytext)

# read docx file
doc <- read_docx("PATH/filename.docx")

content <- docx_summary(doc)

# remove numbers
content <- gsub(content$text, pattern = "[0-9]", replacement = "")

# tibble to make vector
content <- tibble(line = content) %>%
    tidytext::unnest_tokens(word, line)

# Remove common stop words
content <- content %>%
    anti_join(stop_words)

# top ten
top20_words <- content %>%
    count(word, sort = TRUE) %>%
    top_n(20) %>%
    mutate(percentage = n/sum(n))

# word cloud
ggplot(top20_words, aes(label = word, size = n, color = word)) +
    geom_text_wordcloud() +
    scale_size_area(max_size = 8) +
    theme_classic()
