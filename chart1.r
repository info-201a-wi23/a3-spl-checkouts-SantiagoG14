library("tidyverse")
library("ggplot2")
library("plotly")
library("scales")
library("textcat")


#reading in data from spl

checkout_df <- read.csv("Checkouts_by_Title.csv")

authors <- checkout_df$Creator

# cleaning up author name

authors <- gsub("[0-9].", "", authors)
authors <- gsub("\\(.*.", "", authors)
authors <- gsub("-", "", authors)
authors <- str_trim(authors)


# inserting new authors vectors back into original df
checkout_df <- checkout_df %>% mutate(Creator = authors)

# get top 5 authors with the most amout of checkouts

top_5_authors <-
  checkout_df %>% group_by(Creator) %>% summarise(checkouts = sum(Checkouts)) %>% arrange(desc(checkouts)) %>% slice(1:5) %>% pull(Creator)

authors_new <- gsub(",", "", top_5_authors)
authors_new <- gsub("\\.", "", authors_new)
authors_new <- str_split(authors_new, " ")
authors_new[1]
# get the checkouts over time for those authors

top_5_authors_trends <-
  checkout_df %>% filter(Creator %in% top_5_authors) %>% group_by(CheckoutYear, Creator) %>% summarise(checkouts_per_year = sum(Checkouts)) %>% filter(CheckoutYear != "2023")

# creating author trends plot

top_5_authors_trends_graph <-
  ggplot(top_5_authors_trends) + geom_line(aes(x = CheckoutYear, y = checkouts_per_year, color = Creator)) + geom_point(aes(x = CheckoutYear, y = checkouts_per_year, color = Creator)) + labs(x = "Year", y = "Checkouts per year", title = "Top 5 authors checkouts over time") + scale_y_continuous(labels = label_number())

# medium trends for books

medium_trends <-
  checkout_df %>%  group_by(MaterialType, CheckoutYear) %>% summarise(checkouts_per_year = sum(Checkouts)) %>% arrange(desc(checkouts_per_year)) %>% filter(CheckoutYear != "2023")

# creating medium trends plot

medium_trends_plot <-
  ggplot(medium_trends) + geom_line(aes(x = CheckoutYear, y = checkouts_per_year, color = MaterialType)) + geom_point(aes(x = CheckoutYear, y = checkouts_per_year, color = MaterialType)) + labs(x = "Year", y = "Checkouts per year", color = "Medium", title = "Medium checkouts over time") + scale_y_continuous(labels = label_number_si())

# filtering for all rows with title that contains harry potter and their title is in english


harry_potter <- checkout_df %>% filter(str_starts(Title, "Harry Potter") | str_starts(Title, "harry potter")) %>% mutate(Language = textcat(Title)) %>% filter(Language == "english")
books <- gsub("\\:.*", "", harry_potter$Title)
books <- gsub("\\/.*", "", books)
books <- gsub("\\[.*", "", books)
books <- gsub("\\..*", "", books)
books <- tolower(str_trim(books))


# adding the titles back to the data frame

harry_potter <- harry_potter %>% mutate(Title = books)

#getting all the checkouts per harry potter books

checkouts_per_book <- harry_potter %>% group_by(Title) %>% summarise(total_checkouts = sum(Checkouts)) %>% filter(Title != "harry potter")

#getting the checkouts per year per all harry potter books

harry_potter_over_time <- harry_potter %>% group_by(Title, CheckoutYear) %>% summarise(total_checkouts = sum(Checkouts)) %>% filter(Title != "harry potter")

# creating plot for checkouts per all harry potter books 

harry_potter_viz <- ggplot(checkouts_per_book) + geom_col(aes(x = total_checkouts, y = str_wrap(Title, 30), fill = str_wrap(Title, 30), text = paste("Checkouts:", total_checkouts), title = Title)) + labs(title = "Harry potter book checkouts", x = "Total checkouts", y = "Book title", fill = "Book title")

# creating plot for checkoust per year per all harry potter books

harry_potter_over_time_viz <- ggplot(harry_potter_over_time) + geom_line(aes(x = CheckoutYear, y = total_checkouts, color = str_wrap(Title, 30))) + geom_point(aes(x = CheckoutYear, y = total_checkouts, color = str_wrap(Title, 30))) + labs(title = "Harry potter overtime", x = "Checkout year", y = "Total checkouts", color = "Title")



