R Notebook
================

# library

``` r
library(googlesheets4)
library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)
library(ggbeeswarm)
```

# data

## prepare googlesheets4

`ss_input` is not used. Downloaded, unmodified version is available at
`"./input/Pokemon TCG Spreadsheet V3.25.xlsx"`.

``` r
ss_input <- "https://docs.google.com/spreadsheets/d/1IkplPAxSxqEW8qIKH_KW3cDp7C261kNZ-AX-AmCxnMY/edit#gid=840081758" 
ss_output <- "https://docs.google.com/spreadsheets/d/1VcVuCtDyKI5oqGsWJPKJGO7y-vIVZge6JrRqrE_kAQY/edit#gid=0"
```

## read data

For this scale of data, I’d rather use `read_xlsx()` instead of
`googlesheets4::read_sheet()`

### release dates, series name

``` r
excel_path <- "./input/Pokemon TCG Spreadsheet V3.25.xlsx"
sheet_names <- excel_sheets(excel_path)
sheet_names2 <- sheet_names[-1] # remove "Overview" sheet

release_dates <- 
  map(
    sheet_names2,
    ~ read_xlsx(
      excel_path,
      sheet = .x, 
      range = "B1:B2"
    )
  ) |> 
  map(~ pull(.x)) |> 
  reduce(c)
release_dates_df <- 
  tibble(
    series = sheet_names2, 
    release_date = release_dates
  )
release_dates_df
```

    ## # A tibble: 125 × 2
    ##    series                    release_date       
    ##    <chr>                     <dttm>             
    ##  1 Wizards Black Star Promos 1999-07-02 00:00:00
    ##  2 Base Set                  1999-01-09 00:00:00
    ##  3 Jungle                    1999-06-16 00:00:00
    ##  4 Fossil                    1999-10-10 00:00:00
    ##  5 Base Set 2                2000-02-24 00:00:00
    ##  6 Team Rocket               2000-04-24 00:00:00
    ##  7 Gym Heroes                2000-08-04 00:00:00
    ##  8 Gym Challenge             2000-10-16 00:00:00
    ##  9 Legendary Collection      2002-05-04 00:00:00
    ## 10 Neo Genesis               2000-12-16 00:00:00
    ## # … with 115 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

``` r
overview_series_range <- c(
  "D4:D12",
  "G4:G11",
  "J4:J26",
  "M4:M22",
  "D31:D43",
  "G31:G50",
  "J31:J47",
  "M31:M46"
  )
series_gen <- 
  map(
    .x = overview_series_range,
    ~ read_xlsx(
      excel_path,
      col_names = "series",
      sheet = "Overview", 
      range = .x
    )
  ) |> 
  map2(
    .y = 1:8,
    ~ mutate(.data = .x, series_gen = rep(.y, nrow(.x)))
  ) |> 
  bind_rows()
release_dates_df2 <- 
  left_join(release_dates_df, series_gen, by = "series") |> 
  arrange(release_date) |> 
  mutate(
    lead = lead(series_gen),
    series_gen = if_else(is.na(series_gen), lead, series_gen),
    lead = NULL) 
release_dates_df2
```

    ## # A tibble: 125 × 3
    ##    series                    release_date        series_gen
    ##    <chr>                     <dttm>                   <int>
    ##  1 Base Set                  1999-01-09 00:00:00          1
    ##  2 Jungle                    1999-06-16 00:00:00          1
    ##  3 Wizards Black Star Promos 1999-07-02 00:00:00          1
    ##  4 Fossil                    1999-10-10 00:00:00          1
    ##  5 Base Set 2                2000-02-24 00:00:00          1
    ##  6 Team Rocket               2000-04-24 00:00:00          1
    ##  7 Gym Heroes                2000-08-04 00:00:00          1
    ##  8 Gym Challenge             2000-10-16 00:00:00          1
    ##  9 Neo Genesis               2000-12-16 00:00:00          2
    ## 10 Neo Discovery             2001-06-01 00:00:00          2
    ## # … with 115 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

#### release date, main expansion or not, from bulbagarden

Use [Convert Wiki Tables to CSV](https://wikitable2csv.ggor.de/). Set
the Table Selector to `.sortable` and fetch tables provided by
[bulbagarden](https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_Trading_Card_Game_expansions)

| csv no. | content                | name                                                      |
|---------|------------------------|-----------------------------------------------------------|
| 1       | Main expansions        | `List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_1.csv` |
| 2       | Special expansions     | `List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_2.csv` |
| 3       | Black Star Promos      | `List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_3.csv` |
| 4       | POP Series             | `List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_4.csv` |
| 5       | McDonald’s Collections | `List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_5.csv` |

``` r
series_main_expansions_df <- read_csv(
  file = "./input/List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_1.csv"
) |> 
  mutate(series_class = "Main Expansion")
```

    ## Rows: 95 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (11): EN no., JP no., EN symbol, JP symbol, English name, Japanese name,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
colnames(series_main_expansions_df)
```

    ##  [1] "EN no."          "JP no."          "EN symbol"       "JP symbol"      
    ##  [5] "English name"    "Japanese name"   "EN cards"        "JP cards"       
    ##  [9] "EN release date" "JP release date" "Set abb."        "series_class"

``` r
series_special_expansions_df <- read_csv(
  file = "./input/List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_2.csv"
) |> 
  mutate(series_class = "Special Expansion")
```

    ## Rows: 21 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): EN symbol, JP symbol, English name, Japanese name, EN cards, JP car...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
series_black_star_promos_df <- read_csv(
  file = "./input/List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_3.csv"
) |> 
  mutate(series_class = "Black Star Promo")
```

    ## Rows: 8 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): JP symbol, English name, Japanese name, EN cards, JP cards, EN rele...
    ## lgl (1): EN symbol
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
series_pop_series_df <- read_csv(
  file = "./input/List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_4.csv"
) |> 
  mutate(series_class = "Pop Series")
```

    ## Rows: 11 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): EN symbol, JP symbol, English name, Japanese name, EN cards, JP car...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
series_mcdonalds_df <- read_csv(
  file = "./input/List_of_Pok%C3%A9mon_Trading_Card_Game_expansions_5.csv"
) |> 
  mutate(series_class = "McDonalds Collection",
         `EN cards` = as.character(`EN cards`))
```

    ## Rows: 11 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (7): JP symbol, English name, Japanese name, JP cards, EN release date, ...
    ## dbl (1): EN cards
    ## lgl (1): EN symbol
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
series_mcdonalds_df
```

    ## # A tibble: 11 × 10
    ##    `EN symbol` JP symb…¹ Engli…² Japan…³ EN ca…⁴ JP ca…⁵ EN re…⁶ JP re…⁷ Set a…⁸
    ##    <lgl>       <chr>     <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 NA          <NA>      McDona… Collec… 12      9       June 1… Septem… MCD11  
    ##  2 NA          —         McDona… —       12      —       June 1… —       MCD12  
    ##  3 NA          —         McDona… —       12      —       Octobe… —       MCD13  
    ##  4 NA          —         McDona… —       12      —       May 23… —       MCD14  
    ##  5 NA          —         McDona… —       12      —       Novemb… —       MCD15  
    ##  6 NA          —         McDona… —       12      —       August… —       MCD16  
    ##  7 NA          —         McDona… —       12      —       Novemb… —       MCD17  
    ##  8 NA          —         McDona… —       12      —       Octobe… —       MCD18  
    ##  9 NA          —         McDona… —       12      —       Octobe… —       MCD19  
    ## 10 NA          —         McDona… —       25      —       From F… —       MCD21  
    ## 11 NA          —         McDona… —       15      —       From A… —       MCD22  
    ## # … with 1 more variable: series_class <chr>, and abbreviated variable names
    ## #   ¹​`JP symbol`, ²​`English name`, ³​`Japanese name`, ⁴​`EN cards`, ⁵​`JP cards`,
    ## #   ⁶​`EN release date`, ⁷​`JP release date`, ⁸​`Set abb.`
    ## # ℹ Use `colnames()` to see all variable names

``` r
series_classification_df <- bind_rows(
  series_main_expansions_df, 
  series_special_expansions_df, 
  series_black_star_promos_df,
  series_pop_series_df,
  series_mcdonalds_df)
series_classification_df
```

    ## # A tibble: 146 × 12
    ##    `EN no.` `JP no.` `EN symbol` JP sy…¹ Engli…² Japan…³ EN ca…⁴ JP ca…⁵ EN re…⁶
    ##    <chr>    <chr>    <chr>       <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 1        1        —           —       Base S… Expans… 102     102     Januar…
    ##  2 2        2        <NA>        <NA>    Jungle  Pokémo… 64      48      June 1…
    ##  3 3        3        <NA>        <NA>    Fossil  Myster… 62      48      Octobe…
    ##  4 4        —        <NA>        —       Base S… —       130     —       Februa…
    ##  5 5        4        <NA>        <NA>    Team R… Rocket… 83      65      April …
    ##  6 6        5        <NA>        <NA>    Gym He… Leader… 132     96      August…
    ##  7 7        6        <NA>        <NA>    Gym Ch… Challe… 132     98      Octobe…
    ##  8 8        7        <NA>        <NA>    Neo Ge… Gold, … 111     96      Decemb…
    ##  9 9        8        <NA>        <NA>    Neo Di… Crossi… 75      56      June 1…
    ## 10 10       9        <NA>        <NA>    Neo Re… Awaken… 66      57      Septem…
    ## # … with 136 more rows, 3 more variables: `JP release date` <chr>,
    ## #   `Set abb.` <chr>, series_class <chr>, and abbreviated variable names
    ## #   ¹​`JP symbol`, ²​`English name`, ³​`Japanese name`, ⁴​`EN cards`, ⁵​`JP cards`,
    ## #   ⁶​`EN release date`
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

#### merge with release dates df

Merge `series_classification_df` with `release_dates_df2`

``` r
series_classification_df |> colnames()
```

    ##  [1] "EN no."          "JP no."          "EN symbol"       "JP symbol"      
    ##  [5] "English name"    "Japanese name"   "EN cards"        "JP cards"       
    ##  [9] "EN release date" "JP release date" "Set abb."        "series_class"

``` r
release_dates_df2 |> colnames()
```

    ## [1] "series"       "release_date" "series_gen"

``` r
# str_view_all("October, 2003 - September, 2006 ", ".+(?=\\s-)")
# str_view_all("September 7, 2018 ", "\\s\\*\\s|(?<=[a-z]{3,10}\\s\\d{4})\\s")
# 
# str_view_all(
#   "January, 2003 - February, 2004 February, 2004 - July, 2006", 
#   "\\s\\*\\s|(?<=[a-z]{3,13},?\\s\\d{4})\\s(?!-)")
```

``` r
series_classification_df2 <- 
  series_classification_df |> 
  rename(
    series = `English name`,
    series_ja = `Japanese name`,
    series_abb = `Set abb.`,
    cards_total = `EN cards`,
    cards_total_ja = `JP cards`,
    release_date = `EN release date`,
    release_date_ja = `JP release date`,
         ) |> 
  select(-`EN symbol`, -`JP symbol`, -`EN no.`, -`JP no.`) |>
  # separate https://tidyr.tidyverse.org/reference/separate.html
  separate(
    cards_total_ja, 
    into = c("cards_total_ja_1", "cards_total_ja_2"),
    fill = "right"
  )
series_classification_df2
```

    ## # A tibble: 146 × 9
    ##    series        serie…¹ cards…² cards…³ cards…⁴ relea…⁵ relea…⁶ serie…⁷ serie…⁸
    ##    <chr>         <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 Base Set      Expans… 102     "102"   <NA>    Januar… Octobe… BS      Main E…
    ##  2 Jungle        Pokémo… 64      "48"    <NA>    June 1… March … JU      Main E…
    ##  3 Fossil        Myster… 62      "48"    <NA>    Octobe… June 2… FO      Main E…
    ##  4 Base Set 2    —       130     ""      ""      Februa… —       B2      Main E…
    ##  5 Team Rocket   Rocket… 83      "65"    <NA>    April … Novemb… TR      Main E…
    ##  6 Gym Heroes    Leader… 132     "96"    <NA>    August… Octobe… G1      Main E…
    ##  7 Gym Challenge Challe… 132     "98"    <NA>    Octobe… June 2… G2      Main E…
    ##  8 Neo Genesis   Gold, … 111     "96"    <NA>    Decemb… Februa… N1      Main E…
    ##  9 Neo Discovery Crossi… 75      "56"    <NA>    June 1… July 7… N2      Main E…
    ## 10 Neo Revelati… Awaken… 66      "57"    <NA>    Septem… Novemb… N3      Main E…
    ## # … with 136 more rows, and abbreviated variable names ¹​series_ja,
    ## #   ²​cards_total, ³​cards_total_ja_1, ⁴​cards_total_ja_2, ⁵​release_date,
    ## #   ⁶​release_date_ja, ⁷​series_abb, ⁸​series_class
    ## # ℹ Use `print(n = ...)` to see more rows

``` r
# separate release dates of Japanese versions that are released as separate series
series_classification_df3 <- 
  series_classification_df2 |> 
  mutate(
    release_date_ja = 
      str_replace(
        release_date_ja,
        "\\s\\*\\s|(?<=[a-z]{3,13},?\\s\\d{4})\\s(?!-)",
        "SEPARATE"
      )) |> 
  separate(
    release_date_ja, 
    into = c("release_date_ja_1", "release_date_ja_2"),
    sep = "\\s\\*\\s|SEPARATE",
    fill = "right"
  ) |> 
  mutate(
    across(
      everything(), 
      ~ recode(
        .,
        "—" = NA_character_, 
        "Ongoing" = NA_character_)
    ),
    release_date_ja_2 = str_replace(release_date_ja_2, "\\s\\*", "")
  )
series_classification_df3
```

    ## # A tibble: 146 × 10
    ##    series        serie…¹ cards…² cards…³ cards…⁴ relea…⁵ relea…⁶ relea…⁷ serie…⁸
    ##    <chr>         <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 Base Set      Expans… 102     "102"   <NA>    Januar… Octobe… <NA>    BS     
    ##  2 Jungle        Pokémo… 64      "48"    <NA>    June 1… March … <NA>    JU     
    ##  3 Fossil        Myster… 62      "48"    <NA>    Octobe… June 2… <NA>    FO     
    ##  4 Base Set 2    <NA>    130     ""      ""      Februa… <NA>    <NA>    B2     
    ##  5 Team Rocket   Rocket… 83      "65"    <NA>    April … Novemb… <NA>    TR     
    ##  6 Gym Heroes    Leader… 132     "96"    <NA>    August… Octobe… <NA>    G1     
    ##  7 Gym Challenge Challe… 132     "98"    <NA>    Octobe… June 2… <NA>    G2     
    ##  8 Neo Genesis   Gold, … 111     "96"    <NA>    Decemb… Februa… <NA>    N1     
    ##  9 Neo Discovery Crossi… 75      "56"    <NA>    June 1… July 7… <NA>    N2     
    ## 10 Neo Revelati… Awaken… 66      "57"    <NA>    Septem… Novemb… <NA>    N3     
    ## # … with 136 more rows, 1 more variable: series_class <chr>, and abbreviated
    ## #   variable names ¹​series_ja, ²​cards_total, ³​cards_total_ja_1,
    ## #   ⁴​cards_total_ja_2, ⁵​release_date, ⁶​release_date_ja_1, ⁷​release_date_ja_2,
    ## #   ⁸​series_abb
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
# test
# str_view_all(
#   "From February 9, 2021", 
#   "January|February|March|April|May|June|July|August|September|October|November|December")
# str_view_all(
#   "April - August, 2006", 
#   "[:digit:]{4}")
# str_view_all(
#   "October 13th - November 26th, 2013", 
#   "[:digit:]{4}")
# str_view_all(
#   "September, 2008 - March, 2009", 
#   "[:digit:]{4}")

# https://lubridate.tidyverse.org/reference/parse_date_time.html

parse_date_time(
  x = "2021 February 21",
  orders = "ybd"
)
```

    ## [1] "2021-02-21 UTC"

``` r
# run

series_classification_df4 <-
  series_classification_df3 |> 
  mutate(
    release_date_month = 
      str_extract(
        release_date,
        "January|February|March|April|May|June|July|August|September|October|November|December"
      ),
    release_date_day = 
      str_extract(
        release_date,
        "[:digit:]{1,3}(?=,)|[:digit:]{1,3}(?=[:lower:]{2})(?![:lower:]{2},)"
      ) |> replace_na(replace = "1"),
    release_date_year = 
      str_extract(
        release_date,
        "[:digit:]{4}"
      ),
    release_date = 
      str_c(
        release_date_year, 
        release_date_month, 
        release_date_day
      ) |> 
      parse_date_time(orders = "ybd"),
    release_date_month = 
      str_extract(
        release_date_ja_1,
        "January|February|March|April|May|June|July|August|September|October|November|December"
      ),
    release_date_day = 
      str_extract(
        release_date_ja_1,
        "[:digit:]{1,3}(?=,)|[:digit:]{1,3}(?=[:lower:]{2})(?![:lower:]{2},)"
      ) |> replace_na(replace = "1"),
    release_date_year = 
      str_extract(
        release_date_ja_1,
        "[:digit:]{4}"
      ),
    release_date_ja_1 = 
      str_c(
        release_date_year, 
        release_date_month, 
        release_date_day
      ) |> 
      parse_date_time(orders = "ybd"),
    release_date_month = 
      str_extract(
        release_date_ja_2,
        "January|February|March|April|May|June|July|August|September|October|November|December"
      ),
    release_date_day = 
      str_extract(
        release_date_ja_2,
        "[:digit:]{1,3}(?=,)|[:digit:]{1,3}(?=[:lower:]{2})(?![:lower:]{2},)"
      ) |> replace_na(replace = "1"),
    release_date_year = 
      str_extract(
        release_date_ja_2,
        "[:digit:]{4}"
      ),
    release_date_ja_2 = 
      str_c(
        release_date_year, 
        release_date_month, 
        release_date_day
      ) |> 
      parse_date_time(orders = "ybd"),
  )
series_classification_df4
```

    ## # A tibble: 146 × 13
    ##    series         series_ja          cards…¹ cards…² cards…³ release_date       
    ##    <chr>          <chr>              <chr>   <chr>   <chr>   <dttm>             
    ##  1 Base Set       Expansion Pack     102     "102"   <NA>    1999-01-09 00:00:00
    ##  2 Jungle         Pokémon Jungle     64      "48"    <NA>    1999-06-16 00:00:00
    ##  3 Fossil         Mystery of the Fo… 62      "48"    <NA>    1999-10-10 00:00:00
    ##  4 Base Set 2     <NA>               130     ""      ""      2000-02-24 00:00:00
    ##  5 Team Rocket    Rocket Gang        83      "65"    <NA>    2000-04-24 00:00:00
    ##  6 Gym Heroes     Leaders' Stadium   132     "96"    <NA>    2000-08-14 00:00:00
    ##  7 Gym Challenge  Challenge from th… 132     "98"    <NA>    2000-10-16 00:00:00
    ##  8 Neo Genesis    Gold, Silver, to … 111     "96"    <NA>    2000-12-16 00:00:00
    ##  9 Neo Discovery  Crossing the Ruin… 75      "56"    <NA>    2001-06-01 00:00:00
    ## 10 Neo Revelation Awakening Legends  66      "57"    <NA>    2001-09-21 00:00:00
    ## # … with 136 more rows, 7 more variables: release_date_ja_1 <dttm>,
    ## #   release_date_ja_2 <dttm>, series_abb <chr>, series_class <chr>,
    ## #   release_date_month <chr>, release_date_day <chr>, release_date_year <chr>,
    ## #   and abbreviated variable names ¹​cards_total, ²​cards_total_ja_1,
    ## #   ³​cards_total_ja_2
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
series_classification_df5 <- 
  series_classification_df4 |> 
  select(
    series_class,
    series,
    series_abb,
    series_ja,
    cards_total,
    release_date,
    cards_total_ja_1,
    cards_total_ja_2,
    release_date_ja_1,
    release_date_ja_2,
  )
series_classification_df5
```

    ## # A tibble: 146 × 10
    ##    series_c…¹ series serie…² serie…³ cards…⁴ release_date        cards…⁵ cards…⁶
    ##    <chr>      <chr>  <chr>   <chr>   <chr>   <dttm>              <chr>   <chr>  
    ##  1 Main Expa… Base … BS      Expans… 102     1999-01-09 00:00:00 "102"   <NA>   
    ##  2 Main Expa… Jungle JU      Pokémo… 64      1999-06-16 00:00:00 "48"    <NA>   
    ##  3 Main Expa… Fossil FO      Myster… 62      1999-10-10 00:00:00 "48"    <NA>   
    ##  4 Main Expa… Base … B2      <NA>    130     2000-02-24 00:00:00 ""      ""     
    ##  5 Main Expa… Team … TR      Rocket… 83      2000-04-24 00:00:00 "65"    <NA>   
    ##  6 Main Expa… Gym H… G1      Leader… 132     2000-08-14 00:00:00 "96"    <NA>   
    ##  7 Main Expa… Gym C… G2      Challe… 132     2000-10-16 00:00:00 "98"    <NA>   
    ##  8 Main Expa… Neo G… N1      Gold, … 111     2000-12-16 00:00:00 "96"    <NA>   
    ##  9 Main Expa… Neo D… N2      Crossi… 75      2001-06-01 00:00:00 "56"    <NA>   
    ## 10 Main Expa… Neo R… N3      Awaken… 66      2001-09-21 00:00:00 "57"    <NA>   
    ## # … with 136 more rows, 2 more variables: release_date_ja_1 <dttm>,
    ## #   release_date_ja_2 <dttm>, and abbreviated variable names ¹​series_class,
    ## #   ²​series_abb, ³​series_ja, ⁴​cards_total, ⁵​cards_total_ja_1, ⁶​cards_total_ja_2
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

[Multiple observations per
row](https://tidyr.tidyverse.org/articles/pivot.html#multiple-observations-per-row)を参考に、`cards_total_ja`と`release_date_ja`を一気にpivot_longerすることが…できるのでは・・・

``` r
series_classification_df6 <-
  series_classification_df5 |> 
  # rowid_to_column("id") |>
  pivot_longer(
    !series_class:release_date,
    # !series_class:release_date, 
    names_to = c(".value", "ja_variation"),
    names_pattern = "(.*)_(1|2)",
    # names_sep = "_", 
    values_drop_na = TRUE
  ) |> 
  select(-ja_variation)
series_classification_df6
```

    ## # A tibble: 179 × 8
    ##    series_class   series     serie…¹ serie…² cards…³ release_date        cards…⁴
    ##    <chr>          <chr>      <chr>   <chr>   <chr>   <dttm>              <chr>  
    ##  1 Main Expansion Base Set   BS      Expans… 102     1999-01-09 00:00:00 "102"  
    ##  2 Main Expansion Jungle     JU      Pokémo… 64      1999-06-16 00:00:00 "48"   
    ##  3 Main Expansion Fossil     FO      Myster… 62      1999-10-10 00:00:00 "48"   
    ##  4 Main Expansion Base Set 2 B2      <NA>    130     2000-02-24 00:00:00 ""     
    ##  5 Main Expansion Base Set 2 B2      <NA>    130     2000-02-24 00:00:00 ""     
    ##  6 Main Expansion Team Rock… TR      Rocket… 83      2000-04-24 00:00:00 "65"   
    ##  7 Main Expansion Gym Heroes G1      Leader… 132     2000-08-14 00:00:00 "96"   
    ##  8 Main Expansion Gym Chall… G2      Challe… 132     2000-10-16 00:00:00 "98"   
    ##  9 Main Expansion Neo Genes… N1      Gold, … 111     2000-12-16 00:00:00 "96"   
    ## 10 Main Expansion Neo Disco… N2      Crossi… 75      2001-06-01 00:00:00 "56"   
    ## # … with 169 more rows, 1 more variable: release_date_ja <dttm>, and
    ## #   abbreviated variable names ¹​series_abb, ²​series_ja, ³​cards_total,
    ## #   ⁴​cards_total_ja
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

#### join

表記ゆれや数日程度の`release_date`のずれがある。基本的にBulbapediaのものにあわせる
sheet名もそれに伴って書き換えなければいけないが、仕方ない

``` r
release_dates_df3 <-
  release_dates_df2 |> 
  mutate(
    series = recode(
      series,
      `McDonalds Collection 2021` = "McDonald's Collection 2021",
      `Champions Path` = "Champion's Path",
      `Sword and Shield` = "Sword & Shield",
      `Sword and Shield Promos` = "SWSH Black Star Promos",
      `Sun and Moon` = "Sun & Moon",
      `Sun and Moon Promos` = "SM Black Star Promos",
      `Mcdonalds Collection 2015` = "McDonald's Collection 2015",
      `Mcdonalds Collection 2014` = "McDonald's Collection 2014",
      `McDonalds Collection 2012` = "McDonald's Collection 2012", # not Mcd!
      `X and Y` = "XY",
      `X and Y Promos` = "XY Black Star Promos",
      `McDonalds Collection` = "McDonald's Collection",
      `Black and White` = "Black & White", 
      `HeartGold and SoulSilver` = "HeartGold & SoulSilver"
    ),
    series = str_replace(series, "Pop", "POP")
  )
release_dates_df3
```

    ## # A tibble: 125 × 3
    ##    series                    release_date        series_gen
    ##    <chr>                     <dttm>                   <int>
    ##  1 Base Set                  1999-01-09 00:00:00          1
    ##  2 Jungle                    1999-06-16 00:00:00          1
    ##  3 Wizards Black Star Promos 1999-07-02 00:00:00          1
    ##  4 Fossil                    1999-10-10 00:00:00          1
    ##  5 Base Set 2                2000-02-24 00:00:00          1
    ##  6 Team Rocket               2000-04-24 00:00:00          1
    ##  7 Gym Heroes                2000-08-04 00:00:00          1
    ##  8 Gym Challenge             2000-10-16 00:00:00          1
    ##  9 Neo Genesis               2000-12-16 00:00:00          2
    ## 10 Neo Discovery             2001-06-01 00:00:00          2
    ## # … with 115 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

つぎに、Bulbapediaを母体に一緒にしつつもTrainer
KitはBulbapediaのリストにはないため`series_class`に`Trainer Kit`を新設しつつ、`release_date`についてはBulbapediaを優先する。

``` r
series_release_df1 <- full_join(
  series_classification_df6, 
  release_dates_df3,
  by = c("series")) |> 
  distinct() |> 
  mutate(
    series_class = 
      case_when(
        str_detect(series, "Trainer\\sKit") ~ "Trainer Kit", 
        TRUE ~ series_class
      )
  )
series_release_df1
```

    ## # A tibble: 157 × 10
    ##    series_class   series     serie…¹ serie…² cards…³ release_date.x      cards…⁴
    ##    <chr>          <chr>      <chr>   <chr>   <chr>   <dttm>              <chr>  
    ##  1 Main Expansion Base Set   BS      Expans… 102     1999-01-09 00:00:00 "102"  
    ##  2 Main Expansion Jungle     JU      Pokémo… 64      1999-06-16 00:00:00 "48"   
    ##  3 Main Expansion Fossil     FO      Myster… 62      1999-10-10 00:00:00 "48"   
    ##  4 Main Expansion Base Set 2 B2      <NA>    130     2000-02-24 00:00:00 ""     
    ##  5 Main Expansion Team Rock… TR      Rocket… 83      2000-04-24 00:00:00 "65"   
    ##  6 Main Expansion Gym Heroes G1      Leader… 132     2000-08-14 00:00:00 "96"   
    ##  7 Main Expansion Gym Chall… G2      Challe… 132     2000-10-16 00:00:00 "98"   
    ##  8 Main Expansion Neo Genes… N1      Gold, … 111     2000-12-16 00:00:00 "96"   
    ##  9 Main Expansion Neo Disco… N2      Crossi… 75      2001-06-01 00:00:00 "56"   
    ## 10 Main Expansion Neo Revel… N3      Awaken… 66      2001-09-21 00:00:00 "57"   
    ## # … with 147 more rows, 3 more variables: release_date_ja <dttm>,
    ## #   release_date.y <dttm>, series_gen <int>, and abbreviated variable names
    ## #   ¹​series_abb, ²​series_ja, ³​cards_total, ⁴​cards_total_ja
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
df_series <- series_release_df1 |> 
  # select(release_date.x, release_date.y) |> 
  mutate(
    # indicate if the series is in V2.35 spreadsheet or 
    # only available on bulbagarden's list
    meta_is_bulba_only = is.na(release_date.y),
    meta_is_v325_only = is.na(release_date.x),
    release_date = 
      case_when(
        is.na(release_date.x) & !is.na(release_date.y) ~ release_date.y,
        
        !is.na(release_date.x) & is.na(release_date.y) ~ release_date.x,
        release_date.x == release_date.y ~ release_date.x,
        release_date.x != release_date.y ~ release_date.x,
        # TRUE ~ NA_Date_
      ),
      release_date.x = NULL,
      release_date.y = NULL,
      cards_total = cards_total |> as.character() |> as.integer(),
      cards_total_ja = cards_total |> as.character() |> as.integer(),
      series_class = case_when(
        series == "Pokemon GO" ~ "Special Expansion",
        TRUE ~ series_class
      )
  ) |> 
  select(series_class, series, release_date, cards_total, series_gen, series_abb,  series_ja, release_date_ja, cards_total_ja, everything() )
df_series # 157 x 11
```

    ## # A tibble: 157 × 11
    ##    series_class   series     release_date        cards…¹ serie…² serie…³ serie…⁴
    ##    <chr>          <chr>      <dttm>                <int>   <int> <chr>   <chr>  
    ##  1 Main Expansion Base Set   1999-01-09 00:00:00     102       1 BS      Expans…
    ##  2 Main Expansion Jungle     1999-06-16 00:00:00      64       1 JU      Pokémo…
    ##  3 Main Expansion Fossil     1999-10-10 00:00:00      62       1 FO      Myster…
    ##  4 Main Expansion Base Set 2 2000-02-24 00:00:00     130       1 B2      <NA>   
    ##  5 Main Expansion Team Rock… 2000-04-24 00:00:00      83       1 TR      Rocket…
    ##  6 Main Expansion Gym Heroes 2000-08-14 00:00:00     132       1 G1      Leader…
    ##  7 Main Expansion Gym Chall… 2000-10-16 00:00:00     132       1 G2      Challe…
    ##  8 Main Expansion Neo Genes… 2000-12-16 00:00:00     111       2 N1      Gold, …
    ##  9 Main Expansion Neo Disco… 2001-06-01 00:00:00      75       2 N2      Crossi…
    ## 10 Main Expansion Neo Revel… 2001-09-21 00:00:00      66       2 N3      Awaken…
    ## # … with 147 more rows, 4 more variables: release_date_ja <dttm>,
    ## #   cards_total_ja <int>, meta_is_bulba_only <lgl>, meta_is_v325_only <lgl>,
    ## #   and abbreviated variable names ¹​cards_total, ²​series_gen, ³​series_abb,
    ## #   ⁴​series_ja
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

### df_series: colnames explanation

``` r
df_series |> colnames()
```

    ##  [1] "series_class"       "series"             "release_date"      
    ##  [4] "cards_total"        "series_gen"         "series_abb"        
    ##  [7] "series_ja"          "release_date_ja"    "cards_total_ja"    
    ## [10] "meta_is_bulba_only" "meta_is_v325_only"

| `colname`            | `meaning`                                                                                                                                                                                                                                                                                                                                                                                                  | str     |
|----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------|
| `series_class`       | Retrieved from Bulbagarden. Either `"Main Expansion"`, `"Special Expansion"`, `"Black Star Promo"`, `"Pop Series"`, `"McDonalds Collection"`, or `"Trainer Kit"`.                                                                                                                                                                                                                                          | chr     |
| `series`             | The name of the series. “`Neo Genesis`”, “`Sun & Moon`” etc. The names are based on the ones listed in Bulbapedia, not the [V3.25 spreadsheet](https://docs.google.com/spreadsheets/d/10G8wEY70qJ7rEnGbDzJp13vdOB4ikIjz4E3SQVRglRc/edit#gid=59182613).                                                                                                                                                     | chr     |
| `release_date`       | The release date of the series. The dates are based on the ones listed in Bulbapedia, not the ones on the [V3.25 spreadsheet](https://docs.google.com/spreadsheets/d/10G8wEY70qJ7rEnGbDzJp13vdOB4ikIjz4E3SQVRglRc/edit#gid=59182613), from which the Bulbapedia’s date slightly differ. Release dates with no specific dates are converted to the 1st day of the month, e.g., `May 2003` -\> `2003-05-01`. | POSIXct |
| `cards_total`        | the amount of total cards of the series, according to Bulbapedia. The official count is sometimes lower than this. This does not necessarily match the amount of total cards of the series that can be calculated from the card list. The card list is based on V3.25 spreadsheet and the data is not exhaustive.                                                                                          | int     |
| `series_gen`         | generation of the series, from 1-8. This is based on V3.25 spreadsheet and may not match from Bulbapedia’s classification (e.g., `Legendary Collection` classified as series_gen 1 in spreadsheet, series_gen 2 in Bulbapedia).                                                                                                                                                                            | int     |
| `series_abb`         | from Bulbapedia. might be useful for plotting to avoid overcrowding.                                                                                                                                                                                                                                                                                                                                       | chr     |
| `series_ja`          | Name of the series in Japanese version according to Bulbapedia.                                                                                                                                                                                                                                                                                                                                            | chr     |
| `release_date_ja`    |                                                                                                                                                                                                                                                                                                                                                                                                            | POSIXct |
| `cards_total_ja`     |                                                                                                                                                                                                                                                                                                                                                                                                            | int     |
| `meta_is_bulba_only` | `TRUE` if the series is only from Bulbapedia.                                                                                                                                                                                                                                                                                                                                                              | lgl     |
| `meta_is_v325_only`  | `TRUE` if the series is only from spreadsheet. “Trainer Kit”s.                                                                                                                                                                                                                                                                                                                                             | lgl     |

# card data

## read raw

using `sheet_names2` vector (which does not necessarily match the series
names provided by Bulbapedia), read the excels.

``` r
raw_xlsx <- 
  map(
    sheet_names2,
    ~ read_xlsx(
      excel_path,
      sheet = .x,
      skip = 3,
      col_types = "text"
    )
  ) 
```

## convert to df

`raw_xlsx` is still a list. Convert it to df:

``` r
pokemon_df_list <- 
  map2(
    .x = raw_xlsx,
    .y = sheet_names2,
    ~ mutate(.data = .x, series = rep(.y, nrow(.x)))
  ) |> 
  map(
    ~ mutate(
      .data = .x,
      `Set #` = `Set #` |> as.character() # some are num, some are char
    )
  )
# bind rows
everything_df_raw <- 
  pokemon_df_list |> 
  bind_rows() |> 
  filter(!is.na(Type)) |> # remove empty rows
  select(`Set #`, Name, Type, series)
```

### Match series names to Bulbapedia

``` r
everything_df_bulbanized <- 
  everything_df_raw |> 
  mutate(
    series = recode(
      series,
      `McDonalds Collection 2021` = "McDonald's Collection 2021",
      `Champions Path` = "Champion's Path",
      `Sword and Shield` = "Sword & Shield",
      `Sword and Shield Promos` = "SWSH Black Star Promos",
      `Sun and Moon` = "Sun & Moon",
      `Sun and Moon Promos` = "SM Black Star Promos",
      `Mcdonalds Collection 2015` = "McDonald's Collection 2015",
      `Mcdonalds Collection 2014` = "McDonald's Collection 2014",
      `McDonalds Collection 2012` = "McDonald's Collection 2012", # not Mcd!
      `X and Y` = "XY",
      `X and Y Promos` = "XY Black Star Promos",
      `McDonalds Collection` = "McDonald's Collection",
      `Black and White` = "Black & White", 
      `HeartGold and SoulSilver` = "HeartGold & SoulSilver"
    ),
    series = str_replace(series, "Pop", "POP")
  )
```

### Merge series df & poke df

Now that the series names are Bulbanized, merge these datasets to
retrieve `series_type` and `release_date` and `series_gen`:

``` r
df_series_en <- df_series |> select(!ends_with("_ja"))
everything_df_bulbanized$series_gen
```

    ## Warning: Unknown or uninitialised column: `series_gen`.

    ## NULL

``` r
df_series_en$series_gen
```

    ##   [1]  1  1  1  1  1  1  1  2  2  2  2 NA NA  1  2  2  2  2  2  3  3  3  3  3  3
    ##  [26]  3  3  3  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4  4  4  4  4  4  4 NA
    ##  [51] NA  5  5  5  5  5  5  5  5  5  5  5  6  6  6  6  6  6  6  6  6  6  6  6  7
    ##  [76]  7  7  7  7  7  7  7  7  7  7  7  8  8  8  8  8  8  8  8  8  8 NA  2 NA NA
    ## [101] NA  6 NA  6 NA NA NA  7 NA NA  7  7  7 NA  8  8  8 NA  1  3  3  4  4 NA NA
    ## [126]  6  7  8 NA NA  3  3  3  3 NA  4 NA  4  4  5  5 NA  6  6 NA NA NA NA  8 NA
    ## [151]  3  3  4  6  6  6  8

``` r
everything_df_merged <-
  left_join(
    everything_df_bulbanized,
    df_series_en,
    by = c("series")
  ) 
everything_df_merged # 15029 x 11
```

    ## # A tibble: 15,029 × 11
    ##    `Set #` Name       Type    series serie…¹ release_date        cards…² serie…³
    ##    <chr>   <chr>      <chr>   <chr>  <chr>   <dttm>                <int>   <int>
    ##  1 1.0     Pikachu    Lightn… Wizar… Black … 1999-07-01 00:00:00      53       1
    ##  2 2.0     Electabuzz Lightn… Wizar… Black … 1999-07-01 00:00:00      53       1
    ##  3 3.0     Mewtwo     Psychic Wizar… Black … 1999-07-01 00:00:00      53       1
    ##  4 4.0     Pikachu    Lightn… Wizar… Black … 1999-07-01 00:00:00      53       1
    ##  5 5.0     Dragonite  Colorl… Wizar… Black … 1999-07-01 00:00:00      53       1
    ##  6 6.0     Arcanine   Fire    Wizar… Black … 1999-07-01 00:00:00      53       1
    ##  7 7.0     Jigglypuff Colorl… Wizar… Black … 1999-07-01 00:00:00      53       1
    ##  8 8.0     Mew        Psychic Wizar… Black … 1999-07-01 00:00:00      53       1
    ##  9 9.0     Mew        Psychic Wizar… Black … 1999-07-01 00:00:00      53       1
    ## 10 10.0    Meowth     Colorl… Wizar… Black … 1999-07-01 00:00:00      53       1
    ## # … with 15,019 more rows, 3 more variables: series_abb <chr>,
    ## #   meta_is_bulba_only <lgl>, meta_is_v325_only <lgl>, and abbreviated variable
    ## #   names ¹​series_class, ²​cards_total, ³​series_gen
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

## fix typos in Type

“Fightning”, “Coloress” etc.

``` r
everything_df_merged |> dim() # 15029 x 10
```

    ## [1] 15029    11

``` r
# check typos

pokemon_df_summarised <- everything_df_merged |> 
  group_by(Type) |> 
  summarise(n = n()) |> 
  arrange(n |> desc())
everything_df_raw$Type |> unique()
```

    ##  [1] "Lightning"          "Psychic"            "Colorless"         
    ##  [4] "Fire"               "Grass"              "Trainer"           
    ##  [7] "Water"              "Metal"              "Fighting"          
    ## [10] "Stadium"            "Energy"             "FIGHting"          
    ## [13] "Darkness"           "Tool"               "Fightning"         
    ## [16] "Supporter"          "TM"                 "Grass/Darkness"    
    ## [19] "Water/Darkness"     "Lightning/Darkness" "Fighting/Darkness" 
    ## [22] "Psychic/Darkness"   "Fire/Darkness"      "Psychic/Metal"     
    ## [25] "Darkness/Metal"     "Grass/Metal"        "Lightning/Metal"   
    ## [28] "Fire/Metal"         "Fighting/Metal"     "Water/Metal"       
    ## [31] "Lightnijg"          "Fire/Lightning"     "Lightning/Water"   
    ## [34] "Water/Fire"         "Water/Fighting"     "Colorless/Psychic" 
    ## [37] "Item"               "Dragon"             "Lighting"          
    ## [40] "Fairy"              "Fire/Water"         "Normal"            
    ## [43] "Grass/Fire"         "Lightning/Grass"    "Metal/Fighting"    
    ## [46] "Fairy/Water"        "Fairy/Psychic"      "Electric"          
    ## [49] "Colorelss"          "Coloress"

TM=わざマシン(technical machine)らしい。なにそれ
[ref](https://seesaawiki.jp/w/jester_the_pcg/d/%A5%EF%A5%B6%A5%DE%A5%B7%A5%F3)　~~ポケモンではないので上の方でOmitしています~~　後でします

``` r
everything_df_typo <- everything_df_merged |> 
  mutate(
    Type = recode(Type, 
      Fightning = "Fighting",
      FIGHting = "Fighting",
      Coloress = "Colorless",
      Colorelss = "Colorless",
      Normal = "Colorless",
      Electric = "Lightning",
      Lightnijg = "Lightning",
      Lighting = "Lightning"
    )
  )

types_non_pokemon <-  c("Trainer", "Energy", "Supporter", "Item", "Stadium", "Tool", "TM")
pokemon_type_ranking <- everything_df_typo |>
  filter(!Type %in% types_non_pokemon) |> 
  group_by(Type) |> 
  summarise(n = n()) |> 
  arrange(desc(n))
major_types <- pokemon_type_ranking |> 
  filter(n > 100) |> # omit type+type pokemons
  select(Type) |> 
  pull()
mixed_types <- pokemon_type_ranking |> 
  filter(n <= 100) |> # select type+type pokemons
  select(Type) |> 
  pull()
```

### color the types

[kawaii](https://pokepalettes.com/) [palettetown and
pokepal](https://github.com/timcdlucas/palettetown) not so much helpful
here [r base color
sucks](https://bookdown.org/hneth/ds4psy/D-3-apx-colors-basics.html)
[manually pick colour](https://codepen.io/HunorMarton/details/eWvewo)

``` r
everything_df_coloured <- everything_df_typo |> 
  mutate(
    colour = recode(
      Type, 
      Water = "#80DDFF",
      Grass = "#80FF82",
      Psychic = "#361C63",
      Colorless = "#D1D1D1",
      Fighting = "#8F441E",
      Fire = "#FF0F23",
      Lightning = "#F0C800",
      Darkness = "#1D1D1B", # black
      Metal = "#463F43", # silver
      Dragon = "#ABAD00", # gold
      Fairy = "#FF4DAF", # purple
    ),
    Type2 = case_when(
      Type %in% mixed_types ~ "mixed",
      TRUE ~ Type),
    is_pokemon = !Type %in% types_non_pokemon # TODO: 
  ) 
```

*colours*

``` r
colours_named_vector <- 
  c("Water" = "#80DDFF",
      "Grass"= "#80FF82",
      "Psychic" = "#361C63",
      "Colorless" = "#D1D1D1",
      "Fighting" = "#8F441E",
      "Fire" = "#FF0F23",
      "Lightning" = "#F0C800",
      "Darkness" = "#1D1D1B", # black
      "Metal" = "#463F43", # silver
      "Dragon" = "#ABAD00", # gold
      "Fairy" = "#FF4DAF", # purple
      "mixed" = "black"
    )
```

### repair set numbers that are converted to Dates

test

``` r
# check problems
"2022-04-22" |> 
  ymd() %>% # default pipe does not support {} in RHS
  {str_c(month(.), "/", day(.))}
```

    ## [1] "4/22"

``` r
# pop series 9 1/17 Garchomp
"44213.0" |> 
  as.numeric() |> 
  as.Date(origin = "1899/12/30") %>% # excel date starts from 12/30
  {str_c(month(.), "/", day(.))}
```

    ## [1] "1/17"

regexについては[koko](https://rdrr.io/cran/stringi/man/about_search_regex.html)

``` r
everything_df_date <- everything_df_coloured |> 
  # filter(series %in% c("EX Trainer Kit 2", "Pop Series 9")) |>
  mutate(
    set = 
      case_when(
        str_detect(
          `Set #`,
          pattern = "\\d{4}-\\d{2}-\\d{2}"
        ) ~
        # use {} to use dot operator more than once,
        # and use %>% instead of base |> to do so. base pipe doesn't support it
        ymd(`Set #`) %>%
          {str_c(month(.), "/", day(.))}, # 11086 failed to parse
        str_detect(
          `Set #`,
          pattern = "\\d{5}.0"
        ) ~
        as.character(`Set #`) |> 
          as.numeric() |>
          as.Date(origin = "1899/12/30") %>%
          {str_c(month(.), "/", day(.))}, # NAs introduced by coersion
        TRUE ~ `Set #`
      )
  ) |> 
  select(set, everything(), -`Set #`)
```

    ## Warning: 11655 failed to parse.

    ## Warning in as.Date(as.numeric(as.character(`Set #`)), origin = "1899/12/30"):
    ## NAs introduced by coercion

``` r
everything_df_date
```

    ## # A tibble: 15,029 × 14
    ##    set   Name   Type  series serie…¹ release_date        cards…² serie…³ serie…⁴
    ##    <chr> <chr>  <chr> <chr>  <chr>   <dttm>                <int>   <int> <chr>  
    ##  1 1.0   Pikac… Ligh… Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ##  2 2.0   Elect… Ligh… Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ##  3 3.0   Mewtwo Psyc… Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ##  4 4.0   Pikac… Ligh… Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ##  5 5.0   Drago… Colo… Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ##  6 6.0   Arcan… Fire  Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ##  7 7.0   Jiggl… Colo… Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ##  8 8.0   Mew    Psyc… Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ##  9 9.0   Mew    Psyc… Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ## 10 10.0  Meowth Colo… Wizar… Black … 1999-07-01 00:00:00      53       1 WP     
    ## # … with 15,019 more rows, 5 more variables: meta_is_bulba_only <lgl>,
    ## #   meta_is_v325_only <lgl>, colour <chr>, Type2 <chr>, is_pokemon <lgl>, and
    ## #   abbreviated variable names ¹​series_class, ²​cards_total, ³​series_gen,
    ## #   ⁴​series_abb
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

Warningsはでるけど、きれいにフォーマットできている気がするので大丈夫かなと…。

## card number, promo or not

[regex
stringr](https://stringr.tidyverse.org/articles/regular-expressions)
[cheat
sheet](https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf)
look aroundsが使えそう

``` r
# https://stackoverflow.com/questions/6109882/regex-match-all-characters-between-two-strings
# str_view_all("BH233/BH244", "(?<=[:alpha:]{0,8})\\d+(?=/)")
# str_view_all("50a/147", "(?<=[:alpha:]{0,8})\\d+(?=[:lower:]{0,1}/)")

everything_df_card_no <- everything_df_date |> 
  mutate(
    card_number = 
      case_when(
        str_detect(set, ".?/.?") ~
          str_extract(
            set, 
            "(?<=[:alpha:]{0,3})\\d+(?=[:lower:]{0,1}/)"
          ) |> 
          as.integer(),
        str_detect(set, "\\d+") ~
          str_extract(set, "\\d+") |> as.integer(),
        str_detect(set, "\\d+$") ~
          str_extract(set, "\\d+$") |> as.integer(),
        # str_detect(set, "[A-Z]+\\d+") ~
        #   str_extract(set, "\\d")
        TRUE ~ NA_integer_
      ),
    cards_total_official = 
      if_else(
        str_detect(set, ".?/.?"),
        str_extract(
          set,
          "(?<=/[:alpha:]{0,3})\\d+"
        ) |> 
        as.integer(),
        NA_integer_
      ),
    is_secret_card =  (card_number > cards_total_official) # !is.na(card_total) &&
  )
everything_df_card_no2 <- 
  everything_df_card_no |> 
  select(
    Name, 
    series_gen, 
    series, 
    release_date, 
    set, 
    card_number, 
    cards_total_official, 
    starts_with("is_"),  
    everything()
  )
everything_df_card_no2
```

    ## # A tibble: 15,029 × 17
    ##    Name       series_…¹ series release_date        set   card_…² cards…³ is_po…⁴
    ##    <chr>          <int> <chr>  <dttm>              <chr>   <int>   <int> <lgl>  
    ##  1 Pikachu            1 Wizar… 1999-07-01 00:00:00 1.0         1      NA TRUE   
    ##  2 Electabuzz         1 Wizar… 1999-07-01 00:00:00 2.0         2      NA TRUE   
    ##  3 Mewtwo             1 Wizar… 1999-07-01 00:00:00 3.0         3      NA TRUE   
    ##  4 Pikachu            1 Wizar… 1999-07-01 00:00:00 4.0         4      NA TRUE   
    ##  5 Dragonite          1 Wizar… 1999-07-01 00:00:00 5.0         5      NA TRUE   
    ##  6 Arcanine           1 Wizar… 1999-07-01 00:00:00 6.0         6      NA TRUE   
    ##  7 Jigglypuff         1 Wizar… 1999-07-01 00:00:00 7.0         7      NA TRUE   
    ##  8 Mew                1 Wizar… 1999-07-01 00:00:00 8.0         8      NA TRUE   
    ##  9 Mew                1 Wizar… 1999-07-01 00:00:00 9.0         9      NA TRUE   
    ## 10 Meowth             1 Wizar… 1999-07-01 00:00:00 10.0       10      NA TRUE   
    ## # … with 15,019 more rows, 9 more variables: is_secret_card <lgl>, Type <chr>,
    ## #   series_class <chr>, cards_total <int>, series_abb <chr>,
    ## #   meta_is_bulba_only <lgl>, meta_is_v325_only <lgl>, colour <chr>,
    ## #   Type2 <chr>, and abbreviated variable names ¹​series_gen, ²​card_number,
    ## #   ³​cards_total_official, ⁴​is_pokemon
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# name the final df df, and export

``` r
df <- everything_df_card_no2
```
