Statistical assignment 1
================
\[Toby Baines 670030310\]
\[01/02/20\]

## Open data (10 points)

In this assignment you will work with the individual level data from
wave 8 of the Understanding Society survey. First, you need to open the
data set. Please complete the code below.

``` r
library(tidyverse)
getwd()
```

    ## [1] "/Users/user/assignment1-tgb205"

``` r
setwd("/Users/user/assignment1-tgb205")
knitr::opts_chunk$set(echo = TRUE)
getwd()
```

    ## [1] "/Users/user/assignment1-tgb205"

``` r
Data <- read_tsv("UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

Now you have got your data frame stored as Data.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society were collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the result.

``` r
Data <- Data %>%
        select(pidp, h_eumem, h_sex_dv, h_age_dv, h_memorig)
```

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of analysis (see the User Guide, pp. 65-71). We
will not do this in this assignment. However, what we want to do is to
keep data from the original Understanding Society sample only (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain. You need to choose the observations
where *h\_memorig* has the value of 1.

``` r
Data <- Data %>%
        filter(h_memorig == 1)
```

## Recode data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
the numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8>
.

We want to do the following:

1)  Recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know”) as NA.
2)  Recode sex into a character vector with the values “male” or
    “female”.
3)  Recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables.

``` r
Data <- Data %>%
   mutate(EU = case_when(
          h_eumem == 1 ~ "1",
          h_eumem == 2 ~ "0",
          h_eumem < 1 ~ NA_character_
        )
      
        ) %>%
        mutate(sex = case_when(
          h_sex_dv == 1 ~ "male",
          h_sex_dv == 2 ~ "female",
          h_sex_dv == 0 ~ NA_character_
        )
          
        )  %>%
        mutate(agegr = case_when(
          between(h_age_dv, 16, 25) ~ "16 to 25",
          between(h_age_dv, 26, 40) ~ "26 to 40",
          between(h_age_dv, 41, 55) ~ "41 to 55",
          between(h_age_dv, 56, 70) ~ "56 to 70",
          h_age_dv > 70 ~ "Over 70"
        ))
```

## Summarise data (20 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
Data %>%
  count(EU) %>%
  mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 3 x 3
    ##   EU        n  perc
    ##   <chr> <int> <dbl>
    ## 1 0      9338  40.7
    ## 2 1     11118  48.4
    ## 3 <NA>   2501  10.9

Write a couple of sentences with the interpretation of this result. How
this compares with the result of the 2016 referendum?
Why?

# When compared to the result of the 2016 refereundum there is a significant point to comment on. 52% of voters elected to leave the European Union in 2016 whereas this data indicates that only 41% would vote to leave. However this data is similar to the referendum result in terms of remain percentages with both the reflecting 48% voting to remain. There may be some survey bias meaning there was not an accurate representation of leave voters hence the opposite result to the referendum.

## Summarise data by sex and age (30 points)

Now let us look at the support for Leave and Remain by sex and age. Use
your newly created variables.

``` r
Data %>%
  count(sex, EU) %>%
  mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 7 x 4
    ##   sex    EU        n     perc
    ##   <chr>  <chr> <int>    <dbl>
    ## 1 female 0      4859 21.2    
    ## 2 female 1      6371 27.8    
    ## 3 female <NA>   1256  5.47   
    ## 4 male   0      4479 19.5    
    ## 5 male   1      4746 20.7    
    ## 6 male   <NA>   1245  5.42   
    ## 7 <NA>   1         1  0.00436

``` r
Data %>%
  count(agegr, EU) %>%
  mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 15 x 4
    ##    agegr    EU        n  perc
    ##    <chr>    <chr> <int> <dbl>
    ##  1 16 to 25 0       763  3.32
    ##  2 16 to 25 1      1749  7.62
    ##  3 16 to 25 <NA>    373  1.62
    ##  4 26 to 40 0      1508  6.57
    ##  5 26 to 40 1      2440 10.6 
    ##  6 26 to 40 <NA>    436  1.90
    ##  7 41 to 55 0      2509 10.9 
    ##  8 41 to 55 1      3013 13.1 
    ##  9 41 to 55 <NA>    628  2.74
    ## 10 56 to 70 0      2737 11.9 
    ## 11 56 to 70 1      2646 11.5 
    ## 12 56 to 70 <NA>    558  2.43
    ## 13 Over 70  0      1821  7.93
    ## 14 Over 70  1      1270  5.53
    ## 15 Over 70  <NA>    506  2.20

Write a couple of sentences interpreting your results. \# Female
remainers make up the most significant share of the result accounting
for 27.8%. 21.2% of the vote can be attributed to women voting to leave.
Men voting for both remain and leave make up a smaller proportion of the
vote compared to women which would indicate women are more politically
active.

# With regards to the support for leave and remain by age groups, 13% of the voting was 41-55 electing to remain, this is a higher proportion than any other age group. The 16-25 year old group accounts for a very low proportion of the voting which can obviously be attributed to under 18’s ineligibility to vote.
