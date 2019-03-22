# Which posts on Slate Star Codex (https://slatestarcodex.com/) are most often
# referenced (linked to) in future posts?
#
# Dillon Plunkett
# 2019-03-22
# MIT Licensed

library(tidyverse)
library(rvest)
library(lubridate)


# Scrape title and links in post body from a Slate Star Codex post.
scrape <- function(url) {
  Sys.sleep(.1)
  site_html <- read_html(url)
  title <- site_html %>% html_nodes(".pjgm-posttitle") %>% html_text()
  links <- site_html %>% html_nodes(".pjgm-postcontent a") %>% html_attr("href") %>% list()
  print(title)
  return(tibble(title = title, links = links))
}


# Scrape all posts (or load data from file).
if (file.exists("links_in_posts.csv")) {
  links_in_posts <- read_csv("links_in_posts.csv")
} else {
  post_urls <-
    read_html("https://slatestarcodex.com/archives/") %>%
    html_nodes(".sya_postcontent a") %>%
    html_attr("href")
  links_in_posts <-
    tibble(post_url = post_urls) %>%
    filter(!str_detect(post_url, "open-thread|ot[0-9]|acc-entry")) %>%
    rowwise() %>%
    do(scrape(.$post_url)) %>%
    unnest(links) %>%
    rename(full_link = links)
  write_csv(links_in_posts, "links_in_posts.csv")
}

# Remove "http", "https", "www", and trailing slashes, and ignore case to
# ensure slight variations still get counted as links to the same post.
links_in_posts <-
  links_in_posts %>%
  mutate(link = str_replace(full_link, "https*://|www\\.|/$", "") %>% str_to_lower())

# Get most referenced posts. (Don't count multiple links from the same
# post as multiple references. This almost never comes up anyway.)
most_referenced <-
  links_in_posts %>%
  drop_na() %>%  # Removes within-post Table of Contents-style links
  distinct() %>%
  count(link, sort = TRUE)
most_referenced %>% print(n = 32)

# Get most frequently referenced posts (references per year since being posted).
# Using an arbitrary cutoff of minimum 5 references to keep the list from being
# swamped by very recent posts that were immediately referenced once or twice.
normalized_by_date <-
  most_referenced %>%
  mutate(
    date = as_date(str_extract(link, "(?<=^slatestarcodex\\.com/)[0-9/]*(?=/)")),
    references_per_year = n / time_length(interval(date, today()), unit = "year")
  ) %>%
  drop_na() %>%
  arrange(desc(references_per_year))
normalized_by_date %>% filter(n >= 6) %>% print(n = 32)

# Get posts with most links to past SSC posts.
links_in_posts %>%
  drop_na() %>%
  filter(
    str_detect(link, "^slatestarcodex\\.com/"),
    !str_detect(link, "#comment|/blog_images/|/tag/")
  ) %>%
  count(title) %>%
  arrange(desc(n))

# Get posts with most links of any kind (other than Links posts).
links_in_posts %>%
  drop_na() %>%
  filter(
    !str_detect(link, "/blog_images/"),
    !str_detect(title, "^Links")
  ) %>%
  count(title) %>%
  arrange(desc(n))
