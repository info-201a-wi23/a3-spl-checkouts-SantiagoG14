library("tidyverse")
library("ggplot2")
library("plotly")
library("scales")

# reading in data from spl

checkout_df <- read.csv("Checkouts_by_Title.csv")

authors <- checkout_df$Creator

# cleaning up author name
authors <- gsub("[0-9].", "", authors)
authors <- gsub("\\(.*.", "", authors)
authors <- gsub("-", "", authors)
authors <- str_trim(authors)


# inserting new authors vectors back into original df
checkout_df <- checkout_df %>% mutate(Creator = authors)

# fetching the top 10 authors with the most checkouts

top_10_authors <- checkout_df %>% group_by(Creator) %>% summarise(checkouts = sum(Checkouts)) %>% arrange(desc(checkouts)) %>% slice(1:10)
author_most_checkout <- top_10_authors %>% filter(checkouts == max(checkouts)) %>% pull(checkouts)

# fetching the top 10 books with the most checkouts 

top_10_books <- checkout_df %>%  group_by(Title) %>% summarise(checkouts = sum(Checkouts)) %>% arrange(desc(checkouts)) %>% slice(1:10)

# fetching the top 10 publishers with the most checkouts

top_10_publishers <- checkout_df %>%  group_by(Publisher) %>% summarise(checkouts = sum(Checkouts)) %>% arrange(desc(checkouts)) %>% slice(1:10)

# fetching the creators of the book with the most amount of checkouts

book_most_checkout_creator <- checkout_df %>% filter(Title == top_10_books$Title[1]) %>% slice(1:1) %>% pull(Creator)

# fetching annual and monthly total checkouts

highest_checkout_year <- checkout_df %>% group_by(CheckoutYear) %>% summarise(Total_checkouts = sum(Checkouts))
checkout_by_month <- checkout_df %>% group_by(CheckoutMonth) %>% summarise(Total_checkouts = sum(Checkouts)) %>% mutate(CheckoutMonth = as.Date(paste("2020", CheckoutMonth, "01", sep = "-")))

# plots for top author, book publisher number with the most checkouts

author_plot = ggplot(top_10_authors) + geom_col(aes(x = checkouts, y = Creator, fill = Creator)) + scale_x_continuous(labels = label_number_si()) + labs(title = "Top tep most checked out authors")

book_plot = ggplot(top_10_books) + geom_col(aes(x = checkouts, y = str_wrap(Title, 30), fill = str_wrap(Title, 30), text = paste("Checkouts:", checkouts))) + labs(title = "Top ten most checked out books",x = "Number of checkouts", y = "Book title", fill="Titles")


publisher_plot = ggplot(top_10_publishers) + geom_col(aes(x = checkouts, y = Publisher, fill = Publisher)) + scale_x_continuous(labels = label_number_si()) + labs(title = "Top tep most checked out publishers")


# plots for annual and monthly amount of total checkouts

year_plot = ggplot(highest_checkout_year) + geom_line(aes(x =CheckoutYear, y = Total_checkouts)) + geom_point(aes(x =CheckoutYear, y = Total_checkouts)) + scale_y_continuous(labels = label_number_si()) + labs(title = "Checkout by years", x = "Checkout year", y = "Total checkouts")

month_plot = ggplot(checkout_by_month) + geom_line(aes(x = CheckoutMonth, y = Total_checkouts)) + geom_point(aes(x = CheckoutMonth, y = Total_checkouts)) + scale_y_continuous(labels = label_number_si()) + labs(title = "Checkout by month", x = "Checkout year", y = "Total checkouts") + scale_x_date(date_labels = "%b", date_breaks = "1 month")








