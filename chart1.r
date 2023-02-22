library("tidyverse")

checkout_df <- read.csv("2013-2023-5-Checkouts-SPL.csv")

publishers <- unique(checkout_df$Publisher)


top_authors <- checkout_df %>% filter(Checkouts > 5 & Creator != "") %>% group_by(Creator) %>% summarise(sum_checkouts = sum(Checkouts))
