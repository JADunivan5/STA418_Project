NFL\_Project
================

``` r
library(tidyverse)
library(janitor)
library(moderndive)
```

``` r
NFL <- read_csv(here::here("Data", "pbp-2021.csv")) %>% 
  clean_names() %>%
  select(-`x11`,-`x13`,-`x17`,-`x18`)
```

    ## Warning: Missing column names filled in: 'X11' [11], 'X13' [13], 'X17' [17],
    ## 'X18' [18]

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_double(),
    ##   GameDate = col_date(format = ""),
    ##   OffenseTeam = col_character(),
    ##   DefenseTeam = col_character(),
    ##   X11 = col_logical(),
    ##   X13 = col_logical(),
    ##   Description = col_character(),
    ##   X17 = col_logical(),
    ##   X18 = col_logical(),
    ##   Formation = col_character(),
    ##   PlayType = col_character(),
    ##   PassType = col_character(),
    ##   Challenger = col_logical(),
    ##   RushDirection = col_character(),
    ##   YardLineDirection = col_character(),
    ##   PenaltyTeam = col_character(),
    ##   PenaltyType = col_character()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
 play_down_freq <- NFL %>% 
  count(play_type,down)
play_down_freq
```

    ## # A tibble: 53 x 3
    ##    play_type    down     n
    ##    <chr>       <dbl> <int>
    ##  1 CLOCK STOP      1    46
    ##  2 CLOCK STOP      2    10
    ##  3 CLOCK STOP      3     4
    ##  4 EXCEPTION       0     3
    ##  5 EXCEPTION       1     9
    ##  6 EXCEPTION       2     1
    ##  7 EXCEPTION       3     1
    ##  8 EXCEPTION       4     1
    ##  9 EXTRA POINT     0  1107
    ## 10 FIELD GOAL      1    22
    ## # … with 43 more rows

``` r
ggplot( data=play_down_freq, aes(x=down, y=play_type, size=n) ) +
geom_point( )
```

![](NFL_Project_files/figure-gfm/Chart%20of%20play%20type%20and%20down-1.png)<!-- -->

``` r
NFL %>% filter(is.na(play_type))
```

    ## # A tibble: 1,953 x 41
    ##       game_id game_date  quarter minute second offense_team defense_team  down
    ##         <dbl> <date>       <dbl>  <dbl>  <dbl> <chr>        <chr>        <dbl>
    ##  1 2021112500 2021-11-25       4      2      0 <NA>         <NA>             0
    ##  2 2021112501 2021-11-25       2      2      0 <NA>         <NA>             0
    ##  3 2021112501 2021-11-25       2      2      0 DAL          LV               1
    ##  4 2021112110 2021-11-21       4      1     57 <NA>         <NA>             0
    ##  5 2021121210 2021-12-12       2      1     56 <NA>         <NA>             0
    ##  6 2021121904 2021-12-19       2      2      0 <NA>         <NA>             0
    ##  7 2021121906 2021-12-19       3      0      0 <NA>         <NA>             0
    ##  8 2021121600 2021-12-16       2      2      0 <NA>         <NA>             0
    ##  9 2021122002 2021-12-20       2      1     56 <NA>         <NA>             0
    ## 10 2021122000 2021-12-20       2      7     28 <NA>         <NA>             0
    ## # … with 1,943 more rows, and 33 more variables: to_go <dbl>, yard_line <dbl>,
    ## #   series_first_down <dbl>, next_score <dbl>, description <chr>,
    ## #   team_win <dbl>, season_year <dbl>, yards <dbl>, formation <chr>,
    ## #   play_type <chr>, is_rush <dbl>, is_pass <dbl>, is_incomplete <dbl>,
    ## #   is_touchdown <dbl>, pass_type <chr>, is_sack <dbl>, is_challenge <dbl>,
    ## #   is_challenge_reversed <dbl>, challenger <lgl>, is_measurement <dbl>,
    ## #   is_interception <dbl>, is_fumble <dbl>, is_penalty <dbl>,
    ## #   is_two_point_conversion <dbl>, is_two_point_conversion_successful <dbl>,
    ## #   rush_direction <chr>, yard_line_fixed <dbl>, yard_line_direction <chr>,
    ## #   is_penalty_accepted <dbl>, penalty_team <chr>, is_no_play <dbl>,
    ## #   penalty_type <chr>, penalty_yards <dbl>

``` r
#Specifying the data set
#filter by description and play type
#play type must contain NA and Description must contain DIRECT SNAP
#mutate/case to do if else statement, Denny's activity Example.
#Chapter 14, Stringer commands, Week 5 Wednesdays class
NFL %>% mutate(play_type=case_when(is.na(play_type) & str_detect(description,"DIRECT SNAP")~ "Wildcat",
                                   TRUE~play_type))
```

    ## # A tibble: 42,795 x 41
    ##       game_id game_date  quarter minute second offense_team defense_team  down
    ##         <dbl> <date>       <dbl>  <dbl>  <dbl> <chr>        <chr>        <dbl>
    ##  1 2021092612 2021-09-26       1      9     10 MIN          SEA              1
    ##  2 2021092612 2021-09-26       1      8     32 MIN          SEA              1
    ##  3 2021092612 2021-09-26       1      7     52 MIN          SEA              2
    ##  4 2021092612 2021-09-26       1      7     13 MIN          SEA              1
    ##  5 2021101008 2021-10-10       1      9     50 WAS          NO               2
    ##  6 2021101008 2021-10-10       1      9     11 WAS          NO               1
    ##  7 2021101706 2021-10-17       3      1     19 LA           NYG              2
    ##  8 2021102405 2021-10-24       4      9     34 KC           TEN              4
    ##  9 2021103101 2021-10-31       4      4     53 BUF          MIA              1
    ## 10 2021102406 2021-10-24       2      3     38 LA           DET              2
    ## # … with 42,785 more rows, and 33 more variables: to_go <dbl>, yard_line <dbl>,
    ## #   series_first_down <dbl>, next_score <dbl>, description <chr>,
    ## #   team_win <dbl>, season_year <dbl>, yards <dbl>, formation <chr>,
    ## #   play_type <chr>, is_rush <dbl>, is_pass <dbl>, is_incomplete <dbl>,
    ## #   is_touchdown <dbl>, pass_type <chr>, is_sack <dbl>, is_challenge <dbl>,
    ## #   is_challenge_reversed <dbl>, challenger <lgl>, is_measurement <dbl>,
    ## #   is_interception <dbl>, is_fumble <dbl>, is_penalty <dbl>,
    ## #   is_two_point_conversion <dbl>, is_two_point_conversion_successful <dbl>,
    ## #   rush_direction <chr>, yard_line_fixed <dbl>, yard_line_direction <chr>,
    ## #   is_penalty_accepted <dbl>, penalty_team <chr>, is_no_play <dbl>,
    ## #   penalty_type <chr>, penalty_yards <dbl>

``` r
#look into function to change more appealing style of text Title Case
#str_to_title is the function.
```

``` r
#Want to first compute the Average of the league and then compare it to the Chicago Bears.
#(DONE Removed all plays with 0 penalty yards), so the league average was computed and then we filter it to only include the Chicago Bears

  
LA_Penalty <- NFL %>% 
  filter(!penalty_yards==0) %>% 
  summarise(LA_mean=mean(penalty_yards))
Penaltys_team <- NFL %>% 
  count(offense_team,penalty_yards) %>% filter(offense_team=="CHI", !penalty_yards==0)
  Penaltys_team
```

    ## # A tibble: 15 x 3
    ##    offense_team penalty_yards     n
    ##    <chr>                <dbl> <int>
    ##  1 CHI                      1     1
    ##  2 CHI                      3     3
    ##  3 CHI                      4     3
    ##  4 CHI                      5    42
    ##  5 CHI                      6     3
    ##  6 CHI                      7     1
    ##  7 CHI                      8     1
    ##  8 CHI                     10    16
    ##  9 CHI                     12     1
    ## 10 CHI                     14     1
    ## 11 CHI                     15    11
    ## 12 CHI                     21     1
    ## 13 CHI                     26     2
    ## 14 CHI                     32     1
    ## 15 CHI                     48     1

``` r
  ggplot( data=Penaltys_team, aes(x=offense_team, y=penalty_yards, size=n) ) +
geom_point(alpha=.5) + geom_hline(yintercept = 8.62)
```

![](NFL_Project_files/figure-gfm/Penalty%20Yards%20for%20Chicago%20Bears-1.png)<!-- -->

``` r
  Penaltys_team
```

    ## # A tibble: 15 x 3
    ##    offense_team penalty_yards     n
    ##    <chr>                <dbl> <int>
    ##  1 CHI                      1     1
    ##  2 CHI                      3     3
    ##  3 CHI                      4     3
    ##  4 CHI                      5    42
    ##  5 CHI                      6     3
    ##  6 CHI                      7     1
    ##  7 CHI                      8     1
    ##  8 CHI                     10    16
    ##  9 CHI                     12     1
    ## 10 CHI                     14     1
    ## 11 CHI                     15    11
    ## 12 CHI                     21     1
    ## 13 CHI                     26     2
    ## 14 CHI                     32     1
    ## 15 CHI                     48     1

``` r
Playtype_Yardage <- NFL %>% filter(!play_type%in%c("TIMEOUT","CLOCK STOP", "EXCEPTION", "PENALTY","KICK OFF","EXTRA POINT", "QB KNEEL", "NO PLAY", "FIELD GOAL","PUNT","TWO-POINT CONVERSION",NA,"FUMBLES")) %>%  
  count(yards, play_type)
Playtype_Yardage
```

    ## # A tibble: 227 x 3
    ##    yards play_type     n
    ##    <dbl> <chr>     <int>
    ##  1   -20 SACK          1
    ##  2   -19 SACK          2
    ##  3   -18 SACK          3
    ##  4   -17 SACK          2
    ##  5   -16 SACK          3
    ##  6   -15 SACK          6
    ##  7   -14 SACK         13
    ##  8   -13 RUSH          1
    ##  9   -13 SACK         20
    ## 10   -12 SACK         27
    ## # … with 217 more rows

``` r
ggplot( data=Playtype_Yardage, aes(x=play_type, y=yards, size=n) ) +
geom_point(alpha=.5 )
```

![](NFL_Project_files/figure-gfm/Play%20Type%20Yardage-1.png)<!-- -->

``` r
#Removed variables in graph that cannot report gains of yards, sacks can only result in negative yards, otherwise it is considered a scramble. The rest of chart is showing the frequency of plays and what amount of yards they result in.
```

``` r
NFL %>% 
  group_by(down,is_touchdown) %>% 
  summarise(n=n()) %>% 
  mutate(prob=n/sum(n))
```

    ## `summarise()` has grouped output by 'down'. You can override using the `.groups` argument.

    ## # A tibble: 10 x 4
    ## # Groups:   down [5]
    ##     down is_touchdown     n    prob
    ##    <dbl>        <dbl> <int>   <dbl>
    ##  1     0            0  7392 0.999  
    ##  2     0            1     9 0.00122
    ##  3     1            0 13872 0.965  
    ##  4     1            1   497 0.0346 
    ##  5     2            0 10330 0.963  
    ##  6     2            1   398 0.0371 
    ##  7     3            0  6368 0.948  
    ##  8     3            1   350 0.0521 
    ##  9     4            0  3499 0.978  
    ## 10     4            1    80 0.0224

``` r
set.seed(2000)
NFL_CHI_boots <- NFL %>% filter(offense_team=="CHI",down == 3) %>% 
  rep_sample_n(size= nrow(.), reps=1000, replace = TRUE) %>% 
  group_by(down,is_touchdown) %>% 
  summarise(n=n()) %>% 
  mutate(prob=n/sum(n))
```

    ## `summarise()` has grouped output by 'down'. You can override using the `.groups` argument.
