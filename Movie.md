Movie
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
library(readr)
library(lubridate)
library(ggrepel)
```

``` r
basics <- read_tsv("title.basics.tsv.gz",
                   na = "\\N",
                   progress = TRUE)
```

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 11621071 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (5): tconst, titleType, primaryTitle, originalTitle, genres
    ## dbl (4): isAdult, startYear, endYear, runtimeMinutes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(basics)
```

``` r
ratings <- read_tsv("title.ratings.tsv.gz",
                    na = "\\N",
                    progress = TRUE)
```

    ## Rows: 1562853 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (1): tconst
    ## dbl (2): averageRating, numVotes
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(ratings)
```

``` r
romance_movies <- basics %>%
  # Filter for movies that include Romance in genres
  filter(titleType == "movie",
         str_detect(genres, "Romance"),
         !is.na(startYear),
         startYear >= 1900) %>%
  # Convert startYear to numeric (handling any remaining NAs)
  mutate(startYear = as.numeric(startYear)) %>%
  # Join with ratings data
  inner_join(ratings, by = "tconst") %>%
  # Create decade column for broader trends
  mutate(decade = floor(startYear / 10) * 10)
```

``` r
# Get top movie per year with minimum vote threshold
top_movies_yearly <- romance_movies %>%
  filter(titleType == "movie",
         numVotes >= 1000) %>%  # Minimum 1,000 votes
  group_by(startYear) %>%
  slice_max(order_by = averageRating, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(label = paste0(primaryTitle, " (", startYear, ")\n", 
                       round(averageRating, 1), " ★, ", 
                       scales::comma(numVotes), " votes"))

# Create the visualization
ggplot(top_movies_yearly, aes(x = startYear, y = averageRating)) +
  geom_point(aes(size = numVotes, color = averageRating), alpha = 0.8) +
  geom_smooth(method = "loess", color = "grey40", se = FALSE, linetype = "dashed") +
  
  # Smart labeling - show every 5 years + exceptionally popular movies
  geom_label_repel(
    data = top_movies_yearly %>% 
      filter(startYear %% 5 == 0 | numVotes > 50000),
    aes(label = label),
    size = 3,
    box.padding = 0.5,
    max.overlaps = 20,
    segment.color = "grey50"
  ) +
  
  # Scales and colors
  scale_color_gradient(low = "#ffb6c1", high = "#e63946", 
                      name = "IMDb Rating", limits = c(0, 10)) +
  scale_size_continuous(name = "Number of Votes", 
                       range = c(2, 8),
                       labels = scales::comma) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
  
  # Labels and theme
  labs(
    title = "Highest-Rated Romance Movie Each Year",
    subtitle = "Only movies with ≥1,000 votes included | Size = Popularity | Color = Rating",
    x = "Release Year",
    y = "IMDb Rating (0-10)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: ggrepel: 39 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](Movie_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# First calculate the peak years
peak_years <- romance_movies %>%
  filter(titleType == "movie") %>%
  group_by(startYear) %>%
  summarise(
    avg_votes = mean(numVotes, na.rm = TRUE),
    avg_rating = mean(averageRating, na.rm = TRUE),
    movie_count = n()
  ) %>%
  filter(movie_count >= 5) %>%
  summarise(
    peak_vote_year = startYear[which.max(avg_votes)],
    peak_rating_year = startYear[which.max(avg_rating)]
  ) %>%
  distinct()

# Create the plot with vertical lines
romance_movies %>%
  filter(titleType == "movie") %>%
  group_by(startYear) %>%
  summarise(
    avg_votes = mean(numVotes, na.rm = TRUE),
    avg_rating = mean(averageRating, na.rm = TRUE),
    movie_count = n()
  ) %>%
  filter(movie_count >= 5) %>%
  pivot_longer(cols = c(avg_votes, avg_rating), 
               names_to = "metric", 
               values_to = "value") %>%
  mutate(metric = case_when(
    metric == "avg_votes" ~ "Average Votes",
    metric == "avg_rating" ~ "Average Rating (0-10)"
  )) %>%
  ggplot(aes(x = startYear, y = value, color = metric)) +
  # Vertical lines for peak years
  geom_vline(data = data.frame(metric = "Average Votes", 
                              xintercept = peak_years$peak_vote_year),
             aes(xintercept = xintercept), 
             color = "#1f77b4", linetype = "solid", size = 0.7, alpha = 0.7) +
  geom_vline(data = data.frame(metric = "Average Rating (0-10)", 
                              xintercept = peak_years$peak_rating_year),
             aes(xintercept = xintercept), 
             color = "#ff7f0e", linetype = "solid", size = 0.7, alpha = 0.7) +
  # Main plot elements
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  # Peak year labels
  geom_text(data = data.frame(metric = "Average Votes",
                             x = peak_years$peak_vote_year,
                             y = Inf,
                             label = paste("Peak:", peak_years$peak_vote_year)),
            aes(x = x, y = y, label = label),
            vjust = 1.5, color = "#1f77b4", size = 3.5) +
  geom_text(data = data.frame(metric = "Average Rating (0-10)",
                             x = peak_years$peak_rating_year,
                             y = Inf,
                             label = paste("Peak:", peak_years$peak_rating_year)),
            aes(x = x, y = y, label = label),
            vjust = 1.5, color = "#ff7f0e", size = 3.5) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Average Votes" = "#1f77b4", 
                               "Average Rating (0-10)" = "#ff7f0e")) +
  labs(
    title = "Romance Movie Trends with Peak Years Highlighted",
    subtitle = "Vertical lines show years with highest average votes and ratings",
    x = "Release Year",
    y = "",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## `geom_smooth()` using formula = 'y ~ x'

![](Movie_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
