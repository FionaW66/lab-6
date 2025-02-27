Lab 06 - Ugly charts and Simpson’s paradox
================
Fiona Wang
2025-02-25

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
library(scales)
library(forcats)
```

Load data

``` r
staff <- read_csv("data/instructional-staff.csv")
#reshape from wide format to long format
staff_long <- staff %>% 
  pivot_longer(cols = -faculty_type, names_to = "year") %>% 
  mutate(value = as.numeric(value))
staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # ℹ 45 more rows

Plot the graph

``` r
staff_long %>% 
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line()
```

![](lab-06_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Exercise 1

``` r
staff_long %>% 
  ggplot(aes(
    x = year,
    y = value / 100,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line() +
  labs(title = "Faculty Hiring Trends: Percentage of Each Type Hired Per Year",
       subtitle = "1975 - 2011",
       x = "Year",
       y = NULL,
       color = "Faculty Type") + 
  scale_y_continuous(labels = percent_format())
```

![](lab-06_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Exercise 2

To tell the story that part-time faculty hiring has been increasing
while other faculty types have been decreasing in hiring percentages, I
think changing the colors of the lines, as well as the line types would
make this contrast stands out more.

``` r
staff_long %>% 
  ggplot(aes(
    x = year,
    y = value / 100,
    group = faculty_type,
    color = faculty_type,
    linetype = faculty_type
  )) +
  geom_line() +
  labs(title = "Faculty Hiring Trends: Percentage of Each Type Hired Per Year",
       subtitle = "1975 - 2011",
       x = "Year",
       y = NULL,
       color = "Faculty Type") + 
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c(
    "Full-Time Tenured Faculty" = "#2b8cbe",
    "Full-Time Tenure-Track Faculty" = "#2b8cbe",
    "Full-Time Non-Tenure-Track Faculty" = "#2b8cbe",
    "Graduate Student Employees" = "#2b8cbe",
    "Part-Time Faculty" = "#de2d26"
  )
  ) +
  scale_linetype_manual(values = c(
    "Full-Time Tenured Faculty" = "longdash",
    "Full-Time Tenure-Track Faculty" = "longdash",
    "Full-Time Non-Tenure-Track Faculty" = "longdash",
    "Graduate Student Employees" = "longdash",
    "Part-Time Faculty" = "solid"
  ))
```

![](lab-06_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# I don't know if there is a more concise way to write the code.
```

### Exercise 3

``` r
fisheries <- read_csv("data/fisheries.csv")
```

How to improve the graph? The first density plot is not very legible.
All the countries, except China, have very low frequencies. For the pie
chart, it looks pretty fancy, it’s just that I can’t tell which part is
which country.

``` r
fisheries_aqua <- fisheries %>% 
  arrange(desc(aquaculture)) %>% 
  slice(c(1:16))
fisheries_cap <- fisheries %>% 
  arrange(desc(capture)) %>% 
  slice(c(1:16))
fisheries_total <- fisheries %>% 
  arrange(desc(total)) %>% 
  slice(c(1:16))
fisheries_total %>% 
  arrange(desc(total)) %>% 
  ggplot(aes(x = reorder(country, -total), y = total, fill = "pink")) + 
  geom_col(position = "dodge") + 
  geom_text(aes(label = comma(total))) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = ~str_wrap(.x, width = 10)) + 
  labs(title = "Tonnage of fish captured and farmed in total for each country",
       x = "Country",
       y = "Tonnage") +
  theme(text = element_text(size = 12.5))
```

![](lab-06_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

This graph here only represents the total tonnage (capture +
aquaculture).  
What I changed:  
1. I used bar plot instead of density/line plot. This is because a trend
between countries doesn’t make a lot of sense.  
2. I labeled the exact values on each bar. Because China has a very high
value, other countries have shorter bars in the graph. It’s not easy to
make sense of the value of those countries.  
3. Added labels so that it’s clear what this graph is about.  
4. Width of the graph so that it’s more readable.

One thing that could be changed: represents capture, aquaculture, and
total in the same graph.

Let’s try doing that. This will need to include what we just learned:
pivot_longer.

``` r
fisheries_long <- fisheries_total %>% 
  pivot_longer(cols = c(capture, aquaculture),
               names_to = "type",
               values_to = "tonnage") %>% 
  select(-total)

fisheries_long <- fisheries_long %>% 
  group_by(country) %>% 
  mutate(total_tonnage = sum(tonnage)) %>% 
  ungroup()

fisheries_long %>% 
  ggplot(aes(x = reorder(country, -total_tonnage), y = tonnage, fill = type)) + 
  geom_col() +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = ~str_wrap(.x, width = 10)) + 
  labs(title = "Tonnage of fish captured and farmed in total for each country",
       subtitle = "Capture and aquaculture",
       x = "Country",
       y = "Tonnage") +
  theme(text = element_text(size = 12.5))
```

![](lab-06_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Now, this includes more information, and it makes sense.
