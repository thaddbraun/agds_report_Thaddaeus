report_agds_tb
================
Thaddäus Braun
2023-03-10

This R Markdown is part of the course AGDS I. Specifically, it comprises
the first Report Exercice and deals with the Data wrangling. In a first
step I load all the necessary packages and the dataset “measurements”.
The data has undergone manual cleaning in an Excel spreadsheet prior to
the current analysis.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.4
    ## ✔ ggplot2   3.4.1     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.2     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.1     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(readr)
library(tidyr)



measurements <- read_delim("/Users/ThaddaeusBraun/Desktop/AGDS I/R_Codes/AGDS_Report_Exercices/agds_report_thaddaeusbraun/Report_1_TB.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)
```

    ## Rows: 234 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## chr (3): Experiment, Depth, sample_date
    ## dbl (3): time_years, ambient_CO2_n, increased_CO2_n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

In a second step, I calculate the log response ration (LRR) for each
parallel oberservation of SOC. By calculating LRRs for soil organic
carbon measurements, you can better understand and compare the relative
changes in response to ambient and elevated CO2 concentrations across
experiments and sample dates.

``` r
# calculate the log response


df_log <- measurements %>%
  group_by(Experiment) %>%
  mutate(log_ratio = log(increased_CO2_n / ambient_CO2_n))

knitr::kable(df_log)
```

| Experiment                  | Depth     | sample_date             | time_years | ambient_CO2_n | increased_CO2_n |  log_ratio |
|:----------------------------|:----------|:------------------------|-----------:|--------------:|----------------:|-----------:|
| ArizonaFACE_wheat \_high_N  | 0-15 cm   | Nov 95                  |       0.00 |        1344.0 |          1228.0 | -0.0902634 |
| ArizonaFACE_wheat \_high_N  | 0-15 cm   | may 1996                |       0.50 |        1363.0 |          1305.0 | -0.0434851 |
| ArizonaFACE_wheat \_high_N  | 0-15 cm   | dec. 1996               |       1.00 |        1254.0 |          1264.0 |  0.0079429 |
| ArizonaFACE_wheat \_high_N  | 0-15 cm   | may 1997                |       1.50 |        1258.0 |          1400.0 |  0.1069491 |
| Biosphere_2                 | 0-25 cm   | average 1999            |       0.00 |        5950.0 |          6362.0 |  0.0669516 |
| Biosphere_3                 | 0-25 cm   | average 2000            |       1.00 |        5254.0 |          4735.0 | -0.1040080 |
| Biosphere_4                 | 0-25 cm   | average 2001            |       2.00 |        6084.0 |          4785.0 | -0.2401763 |
| Biosphere_5                 | 0-25 cm   | average 2002            |       3.00 |        5470.0 |          4064.0 | -0.2971109 |
| Biosphere_6                 | 0-25 cm   | average 2003            |       4.00 |        5335.0 |          4220.0 | -0.2344538 |
| China_FACE_low_N            | 0-15 cm   | mar. 2008               |       3.66 |        1937.0 |          2192.0 |  0.1236740 |
| China_FACE_low_N            | 0-15 cm   | Jun 08                  |       4.00 |        1937.0 |          2123.0 |  0.0916898 |
| China_FACE_high_N           | 0-15 cm   | mar. 2008               |       3.66 |        1926.0 |          2135.0 |  0.1030213 |
| China_FACE_high_N           | 0-15 cm   | Jun 08                  |       4.00 |        1937.0 |          2105.0 |  0.0831751 |
| China_OTC_low_N             | 0-20 cm   | Jul 05                  |       0.33 |        1103.0 |          1168.0 |  0.0572591 |
| China_OTC_low_N             | 0-20 cm   | Nov 05                  |       0.67 |        1036.0 |          1097.0 |  0.0572120 |
| China_OTC_low_N             | 0-20 cm   | Apr 06                  |       1.00 |        1936.0 |          2385.0 |  0.2085751 |
| China_OTC_low_N             | 0-20 cm   | Aug 06                  |       1.33 |        2319.0 |          2218.0 | -0.0445302 |
| China_OTC_low_N             | 0-20 cm   | Nov 06                  |       1.66 |        2060.0 |          2051.0 | -0.0043785 |
| China_OTC_low_N             | 0-20 cm   | Apr 07                  |       2.00 |        2431.0 |          2301.0 | -0.0549589 |
| China_OTC_low_N             | 0-20 cm   | Aug 07                  |       2.33 |        2263.0 |          2361.0 |  0.0423939 |
| China_OTC_low_N             | 0-20 cm   | Nov 07                  |       2.66 |        2116.0 |          2158.0 |  0.0196544 |
| China_OTC_low_N             | 0-20 cm   | Apr 08                  |       3.00 |        3625.0 |          3315.0 | -0.0893967 |
| China_OTC_low_N             | 0-20 cm   | Aug 08                  |       3.33 |        3253.0 |          3279.0 |  0.0079609 |
| China_OTC_low_N             | 0-20 cm   | Nov 08                  |       3.66 |        3287.0 |          3410.0 |  0.0367370 |
| China_OTC_high_N            | 0-20 cm   | Jul 05                  |       0.33 |        1201.0 |          1198.0 | -0.0025010 |
| China_OTC_high_N            | 0-20 cm   | Nov 05                  |       0.67 |         922.0 |          1409.0 |  0.4240903 |
| China_OTC_high_N            | 0-20 cm   | Apr 06                  |       1.00 |        1930.0 |          2373.0 |  0.2066350 |
| China_OTC_high_N            | 0-20 cm   | Aug 06                  |       1.33 |        1951.0 |          2514.0 |  0.2535330 |
| China_OTC_high_N            | 0-20 cm   | Nov 06                  |       1.66 |        1587.0 |          3054.0 |  0.6546068 |
| China_OTC_high_N            | 0-20 cm   | Apr 07                  |       2.00 |        2015.0 |          3030.0 |  0.4079434 |
| China_OTC_high_N            | 0-20 cm   | Aug 07                  |       2.33 |        2273.0 |          2725.0 |  0.1813679 |
| China_OTC_high_N            | 0-20 cm   | Nov 07                  |       2.66 |        2187.0 |          2702.0 |  0.2114615 |
| China_OTC_high_N            | 0-20 cm   | Apr 08                  |       3.00 |        3602.0 |          3947.0 |  0.0914665 |
| China_OTC_high_N            | 0-20 cm   | Aug 08                  |       3.33 |        3581.0 |          4170.0 |  0.1522739 |
| China_OTC_high_N            | 0-20 cm   | Nov 08                  |       3.66 |        3645.0 |          4158.0 |  0.1316778 |
| FACTS_I                     | 0-15 cm   | Aug 96                  |       0.00 |        1977.0 |          2142.0 |  0.0801594 |
| FACTS_I                     | 0-15 cm   | oct. 1999               |       3.17 |        1902.0 |          2208.0 |  0.1491812 |
| FACTS_I                     | 0-15 cm   | Aug 02                  |       6.00 |        2407.0 |          2734.0 |  0.1273846 |
| FACTS_I                     | 0-15 cm   | Aug 06                  |       9.00 |        2164.0 |          2096.0 | -0.0319276 |
| FACTS_II_Aspen              | 0-10 cm   | 2001                    |       4.00 |        3575.0 |          3562.0 | -0.0036430 |
| FACTS_II_Aspen              | 0-10 cm   | 2003                    |       6.00 |        3625.0 |          4435.0 |  0.2016733 |
| FACTS_II_Aspen              | 0-10 cm   | 2004                    |       7.00 |        3235.0 |          3243.0 |  0.0024699 |
| FACTS_II_Aspen              | 0-10 cm   | 2005                    |       8.00 |        3741.0 |          3506.0 | -0.0648772 |
| FACTS_II_Aspen              | 0-10 cm   | 2006                    |       9.00 |        4243.0 |          3841.0 | -0.0995378 |
| FACTS_II_Aspen              | 0-10 cm   | 2007                    |      10.00 |        4374.0 |          3760.0 | -0.1512590 |
| FACTS_II_Aspen              | 0-10 cm   | 2008                    |      11.00 |        5119.0 |          3267.0 | -0.4490870 |
| FACTS_II_Aspen\_+O3         | 0-10 cm   | 2001                    |       4.00 |        3193.0 |          3199.0 |  0.0018773 |
| FACTS_II_Aspen\_+O3         | 0-10 cm   | 2003                    |       6.00 |        3081.0 |          3433.0 |  0.1081803 |
| FACTS_II_Aspen\_+O3         | 0-10 cm   | 2004                    |       7.00 |        3418.0 |          2804.0 | -0.1980086 |
| FACTS_II_Aspen\_+O3         | 0-10 cm   | 2005                    |       8.00 |        3886.0 |          2890.0 | -0.2961238 |
| FACTS_II_Aspen\_+O3         | 0-10 cm   | 2006                    |       9.00 |        4073.0 |          3444.0 | -0.1677462 |
| FACTS_II_Aspen\_+O3         | 0-10 cm   | 2007                    |      10.00 |        4382.0 |          3268.0 | -0.2933271 |
| FACTS_II_Aspen\_+O3         | 0-10 cm   | 2008                    |      11.00 |        4783.0 |          3141.0 | -0.4205267 |
| FACTS_II_Aspen_Birch        | 0-10 cm   | 2001                    |       4.00 |        3256.0 |          3817.0 |  0.1589653 |
| FACTS_II_Aspen_Birch        | 0-10 cm   | 2003                    |       6.00 |        3347.0 |          3274.0 | -0.0220519 |
| FACTS_II_Aspen_Birch        | 0-10 cm   | 2004                    |       7.00 |        3408.0 |          3457.0 |  0.0142756 |
| FACTS_II_Aspen_Birch        | 0-10 cm   | 2005                    |       8.00 |        3653.0 |          3436.0 | -0.0612407 |
| FACTS_II_Aspen_Birch        | 0-10 cm   | 2006                    |       9.00 |        4433.0 |          4196.0 | -0.0549449 |
| FACTS_II_Aspen_Birch        | 0-10 cm   | 2007                    |      10.00 |        4125.0 |          3832.0 | -0.0736792 |
| FACTS_II_Aspen_Birch        | 0-10 cm   | 2008                    |      11.00 |        3484.0 |          3946.0 |  0.1245213 |
| FACTS_II_Aspen_Birch\_+O3   | 0-10 cm   | 2001                    |       4.00 |        3305.0 |          3121.0 | -0.0572830 |
| FACTS_II_Aspen_Birch\_+O3   | 0-10 cm   | 2003                    |       6.00 |        4105.0 |          3798.0 | -0.0777311 |
| FACTS_II_Aspen_Birch\_+O3   | 0-10 cm   | 2004                    |       7.00 |        2943.0 |          3255.0 |  0.1007628 |
| FACTS_II_Aspen_Birch\_+O3   | 0-10 cm   | 2005                    |       8.00 |        3578.0 |          3351.0 | -0.0655452 |
| FACTS_II_Aspen_Birch\_+O3   | 0-10 cm   | 2006                    |       9.00 |        4083.0 |          4064.0 | -0.0046643 |
| FACTS_II_Aspen_Birch\_+O3   | 0-10 cm   | 2007                    |      10.00 |        3909.0 |          4290.0 |  0.0930051 |
| FACTS_II_Aspen_Birch\_+O3   | 0-10 cm   | 2008                    |      11.00 |        4113.0 |          3849.0 | -0.0663393 |
| FACTS_II_Aspen_Mapel        | 0-10 cm   | 2001                    |       4.00 |        2908.0 |          3541.0 |  0.1969436 |
| FACTS_II_Aspen_Mapel        | 0-10 cm   | 2003                    |       6.00 |        3327.0 |          3186.0 | -0.0433048 |
| FACTS_II_Aspen_Mapel        | 0-10 cm   | 2004                    |       7.00 |        3135.0 |          3270.0 |  0.0421608 |
| FACTS_II_Aspen_Mapel        | 0-10 cm   | 2005                    |       8.00 |        4206.0 |          3960.0 | -0.0602681 |
| FACTS_II_Aspen_Mapel        | 0-10 cm   | 2006                    |       9.00 |        4023.0 |          3850.0 | -0.0439547 |
| FACTS_II_Aspen_Mapel        | 0-10 cm   | 2007                    |      10.00 |        4014.0 |          4450.0 |  0.1031158 |
| FACTS_II_Aspen_Mapel        | 0-10 cm   | 2008                    |      11.00 |        3948.0 |          4675.0 |  0.1690200 |
| FACTS_II_Aspen_Mapel\_+O3   | 0-10 cm   | 2001                    |       4.00 |        3449.0 |          3334.0 | -0.0339115 |
| FACTS_II_Aspen_Mapel\_+O3   | 0-10 cm   | 2003                    |       6.00 |        3512.0 |          2961.0 | -0.1706586 |
| FACTS_II_Aspen_Mapel\_+O3   | 0-10 cm   | 2004                    |       7.00 |        3321.0 |          3240.0 | -0.0246926 |
| FACTS_II_Aspen_Mapel\_+O3   | 0-10 cm   | 2005                    |       8.00 |        3564.0 |          3376.0 | -0.0541919 |
| FACTS_II_Aspen_Mapel\_+O3   | 0-10 cm   | 2006                    |       9.00 |        4115.0 |          3739.0 | -0.0958206 |
| FACTS_II_Aspen_Mapel\_+O3   | 0-10 cm   | 2007                    |      10.00 |        3982.0 |          3737.0 | -0.0635011 |
| FACTS_II_Aspen_Mapel\_+O3   | 0-10 cm   | 2008                    |      11.00 |        4093.0 |          3723.0 | -0.0947484 |
| GiFACE                      | 0-7.5 cm  | may 1998                |       0.00 |        3420.0 |          3528.0 |  0.0310906 |
| GiFACE                      | 0-7.5 cm  | june 2007               |       9.08 |        3150.0 |          3096.0 | -0.0172915 |
| Hohenheim                   | 0-10 cm   | mar. 2004               |       2.00 |        1725.0 |          1767.0 |  0.0240561 |
| Hohenheim                   | 0-10 cm   | oct. 2004               |       2.50 |        1707.0 |          1641.0 | -0.0394316 |
| Hohenheim                   | 0-10 cm   | mar. 2005               |       3.00 |        1805.0 |          1744.0 | -0.0343793 |
| Hohenheim                   | 0-10 cm   | oct. 2005               |       3.50 |        1567.0 |          1588.0 |  0.0133124 |
| Hohenheim                   | 0-10 cm   | mar. 2006               |       4.00 |        1680.0 |          1717.0 |  0.0217848 |
| Hohenheim                   | 0-10 cm   | oct. 2006               |       4.50 |        1721.0 |          1763.0 |  0.0241114 |
| Jasper_Ridge_sandstone      | 0-15 cm   | Jan 92                  |       0.00 |        2646.0 |          2646.0 |  0.0000000 |
| Jasper_Ridge_sandstone      | 0-15 cm   | may 1993                |       1.41 |        2646.0 |          2354.0 | -0.1169331 |
| Jasper_Ridge_sandstone      | 0-15 cm   | may 1994                |       2.41 |        2252.0 |          2339.0 |  0.0379048 |
| Jasper_Ridge_sandstone      | 0-15 cm   | may 1995                |       3.41 |        2315.0 |          2315.0 |  0.0000000 |
| Jasper_Ridge_sandstone      | 0-15 cm   | may 1996                |       4.41 |        2497.0 |          2486.0 | -0.0044150 |
| Jasper_Ridge_sandstone      | 0-15 cm   | may 1997                |       5.41 |        2612.0 |          2750.0 |  0.0514847 |
| JRGCE_Control               | 0-15 cm   | 2000                    |       2.00 |        2384.0 |          2344.0 | -0.0169209 |
| JRGCE_Control               | 0-15 cm   | 2006                    |       8.00 |        2428.0 |          2173.0 | -0.1109592 |
| JRGCE_Burn                  | 0-15 cm   | 2000                    |       2.00 |        2296.0 |          2497.0 |  0.0839215 |
| JRGCE_Burn                  | 0-15 cm   | 2006                    |       8.00 |        2544.0 |          2269.0 | -0.1143984 |
| JRGCE_N                     | 0-15 cm   | 2000                    |       2.00 |        2248.0 |          2381.0 |  0.0574796 |
| JRGCE_N                     | 0-15 cm   | 2006                    |       8.00 |        2552.0 |          2329.0 | -0.0914384 |
| JRGCE_N\_x_Burn             | 0-15 cm   | 2000                    |       2.00 |        2359.0 |          2425.0 |  0.0275937 |
| JRGCE_N\_x_Burn             | 0-15 cm   | 2006                    |       8.00 |        2529.0 |          2226.0 | -0.1276177 |
| JRGCE_Water                 | 0-15 cm   | 2000                    |       2.00 |        2292.0 |          2268.0 | -0.0105264 |
| JRGCE_Water                 | 0-15 cm   | 2006                    |       8.00 |        2311.0 |          2085.0 | -0.1029115 |
| JRGCE_Water_x\_Burn         | 0-15 cm   | 2000                    |       2.00 |        2394.0 |          2563.0 |  0.0682128 |
| JRGCE_Water_x\_Burn         | 0-15 cm   | 2006                    |       8.00 |        2396.0 |          2301.0 | -0.0404569 |
| JRGCE_Water_x\_N            | 0-15 cm   | 2000                    |       2.00 |        2193.0 |          2264.0 |  0.0318627 |
| JRGCE_Water_x\_N            | 0-15 cm   | 2006                    |       8.00 |        2626.0 |          2442.0 | -0.0726444 |
| JRGCE_Water_x\_N_x\_Burn    | 0-15 cm   | 2000                    |       2.00 |        2251.0 |          2569.0 |  0.1321422 |
| JRGCE_Water_x\_N_x\_Burn    | 0-15 cm   | 2006                    |       8.00 |        2232.0 |          2385.0 |  0.0663011 |
| JRGCE_Heat                  | 0-15 cm   | 2000                    |       2.00 |        2298.0 |          2295.0 | -0.0013063 |
| JRGCE_Heat                  | 0-15 cm   | 2006                    |       8.00 |        2385.0 |          2438.0 |  0.0219789 |
| JRGCE_Heat_x\_N             | 0-15 cm   | 2000                    |       2.00 |        2288.0 |          2454.0 |  0.0700413 |
| JRGCE_Heat_x\_N             | 0-15 cm   | 2006                    |       8.00 |        2283.0 |          2449.0 |  0.0701894 |
| JRGCE_Heat_x\_Water         | 0-15 cm   | 2000                    |       2.00 |        2327.0 |          2310.0 | -0.0073324 |
| JRGCE_Heat_x\_Water         | 0-15 cm   | 2006                    |       8.00 |        2074.0 |          2414.0 |  0.1518060 |
| JRGCE_Heat_x\_Water_x\_N    | 0-15 cm   | 2000                    |       2.00 |        2393.0 |          2453.0 |  0.0247640 |
| JRGCE_Heat_x\_Water_x\_N    | 0-15 cm   | 2006                    |       8.00 |        2110.0 |          2407.0 |  0.1316932 |
| Merritt_Island              | 0-10 cm   | dec. 1996               |       0.62 |        1209.0 |          1335.0 |  0.0991377 |
| Merritt_Island              | 0-10 cm   | Jun 98                  |       2.09 |        1131.0 |          1220.0 |  0.0757487 |
| Merritt_Island              | 0-10 cm   | Jul 98                  |       2.17 |        1264.0 |          1292.0 |  0.0219101 |
| Merritt_Island              | 0-10 cm   | Aug 98                  |       2.31 |        1356.0 |          1281.0 | -0.0568982 |
| Merritt_Island              | 0-10 cm   | Nov 98                  |       2.55 |        1734.0 |          1567.0 | -0.1012679 |
| Merritt_Island              | 0-10 cm   | Sep 99                  |       3.36 |        1782.0 |          1712.0 | -0.0400741 |
| Merritt_Island              | 0-10 cm   | mar. 2001               |       4.87 |        2191.0 |          1941.0 | -0.1211548 |
| Merritt_Island              | 0-10 cm   | may 2002                |       6.00 |        2099.0 |          1741.0 | -0.1870014 |
| Merritt_Island              | 0-10 cm   | may 2002                |       6.00 |        1999.0 |          1628.0 | -0.2052948 |
| Merritt_Island              | 0-10 cm   | may 2007                |      11.00 |        1512.0 |          1411.0 | -0.0691346 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 1997           |       0.00 |        2685.0 |          2465.0 | -0.0854889 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 1998           |       1.00 |        2650.0 |          2490.0 | -0.0622769 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 1999           |       2.00 |        2655.0 |          2525.0 | -0.0502036 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 2000           |       3.00 |        2675.0 |          2565.0 | -0.0419909 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 2001           |       4.00 |        2515.0 |          2445.0 | -0.0282277 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 2002           |       5.00 |        2585.0 |          2490.0 | -0.0374428 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 2003           |       6.00 |        2519.0 |          2510.0 | -0.0035792 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 2004           |       7.00 |        2599.0 |          2534.0 | -0.0253277 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 2005           |       8.00 |        2659.0 |          2659.0 |  0.0000000 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 2006           |       9.00 |        2657.0 |          2722.0 |  0.0241692 |
| New_Zealand_FACE            | 0-5 cm    | may/oct. 2007           |      10.00 |        2722.0 |          2822.0 |  0.0360789 |
| New_Zealand_Greenhouse      | 0-5 cm    | \-                      |       0.00 |        2112.2 |          2122.4 |  0.0048175 |
| New_Zealand_Greenhouse      | 0-5 cm    | \-                      |       0.25 |        2122.4 |          2142.9 |  0.0096125 |
| New_Zealand_Greenhouse      | 0-5 cm    | \-                      |       0.50 |        1969.4 |          1979.6 |  0.0051659 |
| New_Zealand_Greenhouse      | 0-5 cm    | \-                      |       0.75 |        1969.4 |          2000.0 |  0.0154183 |
| New_Zealand_OTC \_P_radiata | 0-10 cm   | Apr 94                  |       0.00 |         100.0 |           100.0 |  0.0000000 |
| New_Zealand_OTC \_P_radiata | 0-10 cm   | mar. 2000               |       6.00 |         412.0 |           530.0 |  0.2518537 |
| New_Zealand_OTC \_N_fusca   | 0-10 cm   | Apr 94                  |       0.00 |         100.0 |           100.0 |  0.0000000 |
| New_Zealand_OTC \_N_fusca   | 0-10 cm   | mar. 2000               |       6.00 |        2403.0 |          3402.0 |  0.3476455 |
| ORNL_FACE                   | 0-15 cm   | oct. 1997               |       0.00 |        2672.0 |          2791.0 |  0.0435727 |
| ORNL_FACE                   | 0-15 cm   | Nov 00                  |       2.42 |        2785.0 |          2866.0 |  0.0286695 |
| ORNL_FACE                   | 0-15 cm   | oct. 2002               |       4.33 |        2730.0 |          2991.0 |  0.0913062 |
| ORNL_FACE                   | 0-15 cm   | june 2009               |      11.00 |        3370.0 |          3850.0 |  0.1331604 |
| Placerville_low_N           | 0-30 cm   | mar. 1991               |       0.00 |        8400.0 |          8000.0 | -0.0487902 |
| Placerville_low_N           | 0-30 cm   | mar. 1993               |       1.75 |        7600.0 |          7600.0 |  0.0000000 |
| Placerville_low_N           | 0-30 cm   | Sep 96                  |       5.50 |        7400.0 |          7600.0 |  0.0266682 |
| Placerville_medium_N        | 0-30 cm   | mar. 1991               |       0.00 |        7800.0 |          7900.0 |  0.0127390 |
| Placerville_medium_N        | 0-30 cm   | mar. 1993               |       1.75 |        7700.0 |          7800.0 |  0.0129034 |
| Placerville_medium_N        | 0-30 cm   | Sep 96                  |       5.50 |        7400.0 |          8300.0 |  0.1147755 |
| Placerville_high_N          | 0-30 cm   | mar. 1991               |       0.00 |        8100.0 |          7500.0 | -0.0769610 |
| Placerville_high_N          | 0-30 cm   | mar. 1993               |       1.75 |        7500.0 |          8500.0 |  0.1251631 |
| Placerville_high_N          | 0-30 cm   | Sep 96                  |       5.50 |        7400.0 |          8100.0 |  0.0903841 |
| POPFACE_alba                | 0-10 cm   | oct. 1999               |       0.00 |        1320.0 |          1128.0 | -0.1571856 |
| POPFACE_alba                | 0-10 cm   | oct. 2000               |       1.00 |        1365.0 |          1195.0 | -0.1330082 |
| POPFACE_alba                | 0-10 cm   | oct. 2001               |       2.00 |        1524.0 |          1295.0 | -0.1628278 |
| POPFACE_alba                | 0-10 cm   | oct. 2003               |       4.00 |        1629.0 |          1377.0 | -0.1680591 |
| POPFACE_alba                | 0-10 cm   | oct. 2004               |       5.00 |        1647.0 |          1480.0 | -0.1069134 |
| POPFACE_euramericana        | 0-10 cm   | oct. 1999               |       0.00 |        1252.0 |          1165.0 | -0.0720212 |
| POPFACE_euramericana        | 0-10 cm   | oct. 2000               |       1.00 |        1429.0 |          1213.0 | -0.1638783 |
| POPFACE_euramericana        | 0-10 cm   | oct. 2001               |       2.00 |        1405.0 |          1084.0 | -0.2593794 |
| POPFACE_euramericana        | 0-10 cm   | oct. 2003               |       4.00 |        1635.0 |          1431.0 | -0.1332693 |
| POPFACE_euramericana        | 0-10 cm   | oct. 2004               |       5.00 |        1686.0 |          1370.0 | -0.2075481 |
| POPFACE_nigra               | 0-10 cm   | oct. 1999               |       0.00 |        1379.0 |          1149.0 | -0.1824666 |
| POPFACE_nigra               | 0-10 cm   | oct. 2000               |       1.00 |        1458.0 |          1169.0 | -0.2209170 |
| POPFACE_nigra               | 0-10 cm   | oct. 2001               |       2.00 |        1534.0 |          1112.0 | -0.3217185 |
| POPFACE_nigra               | 0-10 cm   | oct. 2003               |       4.00 |        1518.0 |          1246.0 | -0.1974553 |
| POPFACE_nigra               | 0-10 cm   | oct. 2004               |       5.00 |        1509.0 |          1344.0 | -0.1157969 |
| Shortgrass_prairie          | 0-5 cm    | Nov 95                  |       0.00 |         617.0 |           645.0 |  0.0443813 |
| Shortgrass_prairie          | 0-5 cm    | end growing season 1997 |       1.00 |         527.0 |           542.0 |  0.0280655 |
| Shortgrass_prairie          | 0-5 cm    | end growing season 1998 |       2.00 |         488.0 |           522.0 |  0.0673522 |
| Shortgrass_prairie          | 0-5 cm    | end growing season 1999 |       3.00 |         618.0 |           616.0 | -0.0032415 |
| Shortgrass_prairie          | 0-5 cm    | end growing season 2000 |       4.00 |         603.0 |           593.0 | -0.0167228 |
| Soy_FACE                    | 0-12.5 cm | spring 2001             |       0.00 |        4803.0 |          4142.0 | -0.1480620 |
| Soy_FACE                    | 0-12.5 cm | spring 2006             |       5.00 |        4671.0 |          4100.0 | -0.1303862 |
| Soy_FACE                    | 0-12.5 cm | spring 2007             |       6.00 |        4437.0 |          3836.0 | -0.1455483 |
| SwissFACE_grass_low_N       | 0-10 cm   | may 1995                |       2.00 |        2878.0 |          2862.0 | -0.0055749 |
| SwissFACE_grass_low_N       | 0-10 cm   | may 1996                |       3.00 |        2796.0 |          3584.0 |  0.2482897 |
| SwissFACE_grass_low_N       | 0-10 cm   | may 1997                |       4.00 |        2855.0 |          2790.0 | -0.0230302 |
| SwissFACE_grass_low_N       | 0-10 cm   | may 1998                |       5.00 |        3305.0 |          3368.0 |  0.0188826 |
| SwissFACE_grass_low_N       | 0-10 cm   | may 1999                |       6.00 |        2970.0 |          2946.0 | -0.0081136 |
| SwissFACE_grass_low_N       | 0-10 cm   | may 2000                |       7.00 |        2606.0 |          2759.0 |  0.0570518 |
| SwissFACE_grass_low_N       | 0-10 cm   | may 2001                |       8.00 |        3733.0 |          3834.0 |  0.0266964 |
| SwissFACE_grass_low_N       | 0-10 cm   | may 2002                |       9.00 |        3350.0 |          3498.0 |  0.0432310 |
| SwissFACE_grass_low_N       | 0-10 cm   | may 2003                |      10.00 |        3293.0 |          3198.0 | -0.0292734 |
| SwissFACE_grass_high_N      | 0-10 cm   | may 1995                |       2.00 |        3528.0 |          3223.0 | -0.0904185 |
| SwissFACE_grass_high_N      | 0-10 cm   | may 1996                |       3.00 |        3052.0 |          3892.0 |  0.2431261 |
| SwissFACE_grass_high_N      | 0-10 cm   | may 1997                |       4.00 |        3075.0 |          3091.0 |  0.0051898 |
| SwissFACE_grass_high_N      | 0-10 cm   | may 1998                |       5.00 |        3494.0 |          3606.0 |  0.0315519 |
| SwissFACE_grass_high_N      | 0-10 cm   | may 1999                |       6.00 |        2986.0 |          3045.0 |  0.0195662 |
| SwissFACE_grass_high_N      | 0-10 cm   | may 2000                |       7.00 |        2920.0 |          2588.0 | -0.1206982 |
| SwissFACE_grass_high_N      | 0-10 cm   | may 2001                |       8.00 |        3434.0 |          3114.0 | -0.0978177 |
| SwissFACE_grass_high_N      | 0-10 cm   | may 2002                |       9.00 |        3302.0 |          3439.0 |  0.0406524 |
| SwissFACE_grass_high_N      | 0-10 cm   | may 2003                |      10.00 |        3258.0 |          3332.0 |  0.0224592 |
| SwissFACE_clover_low_N      | 0-10 cm   | may 1995                |       2.00 |        2822.0 |          2872.0 |  0.0175628 |
| SwissFACE_clover_low_N      | 0-10 cm   | may 1996                |       3.00 |        3048.0 |          3500.0 |  0.1382773 |
| SwissFACE_clover_low_N      | 0-10 cm   | may 1997                |       4.00 |        2862.0 |          2931.0 |  0.0238230 |
| SwissFACE_clover_low_N      | 0-10 cm   | may 1998                |       5.00 |        3034.0 |          3291.0 |  0.0813096 |
| SwissFACE_clover_low_N      | 0-10 cm   | may 1999                |       6.00 |        2758.0 |          2911.0 |  0.0539909 |
| SwissFACE_clover_low_N      | 0-10 cm   | may 2000                |       7.00 |        2544.0 |          2987.0 |  0.1605319 |
| SwissFACE_clover_low_N      | 0-10 cm   | may 2001                |       8.00 |        3443.0 |          3145.0 | -0.0905293 |
| SwissFACE_clover_low_N      | 0-10 cm   | may 2002                |       9.00 |        3053.0 |          3591.0 |  0.1623060 |
| SwissFACE_clover_low_N      | 0-10 cm   | may 2003                |      10.00 |        2999.0 |          3076.0 |  0.0253512 |
| SwissFACE_clover_high_N     | 0-10 cm   | may 1995                |       2.00 |        2916.0 |          3496.0 |  0.1814066 |
| SwissFACE_clover_high_N     | 0-10 cm   | may 1996                |       3.00 |        3256.0 |          3688.0 |  0.1245849 |
| SwissFACE_clover_high_N     | 0-10 cm   | may 1997                |       4.00 |        2725.0 |          2996.0 |  0.0948096 |
| SwissFACE_clover_high_N     | 0-10 cm   | may 1998                |       5.00 |        3233.0 |          3406.0 |  0.0521281 |
| SwissFACE_clover_high_N     | 0-10 cm   | may 1999                |       6.00 |        2759.0 |          3153.0 |  0.1334861 |
| SwissFACE_clover_high_N     | 0-10 cm   | may 2000                |       7.00 |        2630.0 |          2434.0 | -0.0774479 |
| SwissFACE_clover_high_N     | 0-10 cm   | may 2001                |       8.00 |        3528.0 |          3572.0 |  0.0123945 |
| SwissFACE_clover_high_N     | 0-10 cm   | may 2002                |       9.00 |        3209.0 |          3456.0 |  0.0741525 |
| SwissFACE_clover_high_N     | 0-10 cm   | may 2003                |      10.00 |        2993.0 |          3236.0 |  0.0780618 |
| Swiss_Greenhouse            | 0-20 cm   | \-                      |       0.00 |        1280.0 |          1280.0 |  0.0000000 |
| Swiss_Greenhouse            | 0-20 cm   | \-                      |       0.25 |        1205.0 |           980.0 | -0.2066823 |
| Tallgrass_Prairie           | 0-5 cm    | Nov 92                  |       2.50 |        2985.0 |          2860.0 | -0.0427781 |
| Tallgrass_Prairie           | 0-5 cm    | oct. 1996               |       8.00 |        2181.0 |          2372.0 |  0.0839500 |
| UMBS_aspen                  | 0-15 cm   | may 1991                |       0.00 |         297.0 |           297.0 |  0.0000000 |
| UMBS_aspen                  | 0-15 cm   | oct. 1991               |       0.50 |         242.0 |           290.0 |  0.1809432 |
| WSL_loam_low_N              | 0-10 cm   | Jan 95                  |       0.00 |        1290.0 |          1290.0 |  0.0000000 |
| WSL_loam_low_N              | 0-10 cm   | Sep 98                  |       3.75 |        1317.0 |          1471.0 |  0.1105860 |
| WSL_loam_high_N             | 0-10 cm   | Jan 95                  |       0.00 |        1290.0 |          1290.0 |  0.0000000 |
| WSL_loam_high_N             | 0-10 cm   | Sep 98                  |       3.75 |        1482.0 |          1547.0 |  0.0429250 |
| WSL_sand_low_N              | 0-10 cm   | Jan 95                  |       0.00 |        1310.0 |          1310.0 |  0.0000000 |
| WSL_sand_low_N              | 0-10 cm   | Sep 98                  |       3.75 |        1167.0 |          1214.0 |  0.0394843 |
| WSL_sand_high_N             | 0-10 cm   | Jan 95                  |       0.00 |        1310.0 |          1310.0 |  0.0000000 |
| WSL_sand_high_N             | 0-10 cm   | Sep 98                  |       3.75 |        1120.0 |          1183.0 |  0.0547249 |

Furthermore, the exercice asks me to aggregate log response ratios by
taking their mean. Aggregating log response ratios by taking their mean
simplifies the interpretation, facilitates comparative analysis, and
provides a representative summary measure of the overall response to
different treatments or conditions.

``` r
# Aggregate the data


df_agg <- df_log %>%
  group_by(Experiment) %>%
  summarise(log_ratio = mean(log_ratio), .groups = "drop")

knitr::kable(df_agg)
```

| Experiment                  |  log_ratio |
|:----------------------------|-----------:|
| ArizonaFACE_wheat \_high_N  | -0.0047141 |
| Biosphere_2                 |  0.0669516 |
| Biosphere_3                 | -0.1040080 |
| Biosphere_4                 | -0.2401763 |
| Biosphere_5                 | -0.2971109 |
| Biosphere_6                 | -0.2344538 |
| China_FACE_high_N           |  0.0930982 |
| China_FACE_low_N            |  0.1076819 |
| China_OTC_high_N            |  0.2465959 |
| China_OTC_low_N             |  0.0215026 |
| FACTS_I                     |  0.0811994 |
| FACTS_II_Aspen              | -0.0806087 |
| FACTS_II_Aspen\_+O3         | -0.1808107 |
| FACTS_II_Aspen_Birch        |  0.0122636 |
| FACTS_II_Aspen_Birch\_+O3   | -0.0111136 |
| FACTS_II_Aspen_Mapel        |  0.0519590 |
| FACTS_II_Aspen_Mapel\_+O3   | -0.0767893 |
| GiFACE                      |  0.0068995 |
| Hohenheim                   |  0.0015756 |
| JRGCE_Burn                  | -0.0152385 |
| JRGCE_Control               | -0.0639400 |
| JRGCE_Heat                  |  0.0103363 |
| JRGCE_Heat_x\_N             |  0.0701153 |
| JRGCE_Heat_x\_Water         |  0.0722368 |
| JRGCE_Heat_x\_Water_x\_N    |  0.0782286 |
| JRGCE_N                     | -0.0169794 |
| JRGCE_N\_x_Burn             | -0.0500120 |
| JRGCE_Water                 | -0.0567189 |
| JRGCE_Water_x\_Burn         |  0.0138780 |
| JRGCE_Water_x\_N            | -0.0203909 |
| JRGCE_Water_x\_N_x\_Burn    |  0.0992216 |
| Jasper_Ridge_sandstone      | -0.0053264 |
| Merritt_Island              | -0.0584029 |
| New_Zealand_FACE            | -0.0249354 |
| New_Zealand_Greenhouse      |  0.0087535 |
| New_Zealand_OTC \_N_fusca   |  0.1738228 |
| New_Zealand_OTC \_P_radiata |  0.1259268 |
| ORNL_FACE                   |  0.0741772 |
| POPFACE_alba                | -0.1455988 |
| POPFACE_euramericana        | -0.1672193 |
| POPFACE_nigra               | -0.2076709 |
| Placerville_high_N          |  0.0461954 |
| Placerville_low_N           | -0.0073740 |
| Placerville_medium_N        |  0.0468060 |
| Shortgrass_prairie          |  0.0239669 |
| Soy_FACE                    | -0.1413322 |
| SwissFACE_clover_high_N     |  0.0748418 |
| SwissFACE_clover_low_N      |  0.0636248 |
| SwissFACE_grass_high_N      |  0.0059568 |
| SwissFACE_grass_low_N       |  0.0364622 |
| Swiss_Greenhouse            | -0.1033411 |
| Tallgrass_Prairie           |  0.0205859 |
| UMBS_aspen                  |  0.0904716 |
| WSL_loam_high_N             |  0.0214625 |
| WSL_loam_low_N              |  0.0552930 |
| WSL_sand_high_N             |  0.0273624 |
| WSL_sand_low_N              |  0.0197422 |

Explanation of the aggregated data: In the context of this data, where
you I compare soil organic carbon measurements between ambient (low) and
elevated (high) CO2 concentrations, a negative log response ratio would
imply a decrease in soil organic carbon in the elevated CO2 condition
compared to the ambient CO2 condition. A larger negative value indicates
a more substantial reduction in the response variable. A positive value
can be interpreted just the other way round.

In a last step, I aggregate data across all experiments for different
years since the start of the experiment, distinguishing an early phase
(\<3 years since start), a mid-phase (3-6 years since start), and a late
phase (\>6 years since start).

``` r
# Aggregating data across all experiments
## Create a variable for phase based on years since start
measurements$phase <- ifelse(measurements$time_years < 3, "Early",
                     ifelse(measurements$time_years >= 3 & measurements$time_years <= 6, "Mid",
                            ifelse(measurements$time_years > 6, "Late", NA)))

# Calculate the log-response ratio for each phase
library(dplyr)
log_phase <- measurements %>%
  group_by(phase) %>%
  summarise(log_response_ratio = mean(log(increased_CO2_n / ambient_CO2_n), na.rm = TRUE))


# Sort DataFrame

new_order = c("Early","Mid","Late")

knitr::kable(log_phase)
```

| phase | log_response_ratio |
|:------|-------------------:|
| Early |          0.0124980 |
| Late  |         -0.0306330 |
| Mid   |          0.0172306 |

Interpretation of the Results: In the early phase, there is a small
positive increase in soil organic carbon in the elevated CO2 condition
compared to the ambient CO2 condition. However, the magnitude of the
increase is relatively small. In the mid-phase, there is a slightly
larger positive increase in soil organic carbon in the elevated CO2
condition compared to the ambient CO2 condition. The magnitude of the
increase is larger than in the early phase but still relatively modest.
In the late phase, there is a significant decrease in soil organic
carbon in the elevated CO2 condition compared to the ambient CO2
condition. The negative log response ratio indicates a reduction in soil
organic carbon, suggesting that the elevated CO2 concentration has a
negative impact on soil organic carbon levels in the long term.
