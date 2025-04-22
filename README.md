## Playback experiments reveal sources of disturbance associated with pastoralism in an endangered, endemic African antelope

### Overview

Authors: TBC

This repository contains all R scripts for a paper (in prep) using playback experiments to investigate the behaviour of mountain nyala in response to anthropogenic disturbance in Bale Mountains National Park. This paper is part of a broader project assessing the impact of livestock grazing and other human encroachment on the endangered and endemic mountain nyala in Bale Mountains National Park.

Data associated with this paper to be used in conjunction with this code is available at: TBC

To cite this paper: TBC

To cite this repo: TBC

## Session Info

Please see below for the output of devtools::session_info() used to perform the analyses for this paper.

```
- Session info --------------------------------------------------------------------------------------------------------
 setting  value
 version  R version 4.4.2 (2024-10-31 ucrt)
 os       Windows 11 x64 (build 26100)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  English_United Kingdom.1252
 ctype    English_United Kingdom.1252
 tz       Europe/London
 date     2025-04-22
 rstudio  2021.09.0+351 Ghost Orchid (desktop)
 pandoc   NA
 quarto   NA

- Packages ------------------------------------------------------------------------------------------------------------
 package     * version  date (UTC) lib source
 abind         1.4-8    2024-09-12 [1] CRAN (R 4.4.1)
 aod         * 1.3.3    2023-12-13 [1] CRAN (R 4.4.1)
 backports     1.5.0    2024-05-23 [1] CRAN (R 4.4.0)
 bayestestR    0.15.2   2025-02-07 [1] CRAN (R 4.4.2)
 boot          1.3-31   2024-08-28 [2] CRAN (R 4.4.2)
 broom         1.0.7    2024-09-26 [1] CRAN (R 4.4.1)
 cachem        1.1.0    2024-05-16 [1] CRAN (R 4.4.2)
 car           3.1-3    2024-09-27 [1] CRAN (R 4.4.2)
 carData       3.0-5    2022-01-06 [1] CRAN (R 4.4.2)
 cli           3.6.4    2025-02-13 [1] CRAN (R 4.4.3)
 colorspace    2.1-1    2024-07-26 [1] CRAN (R 4.4.1)
 datawizard    1.0.0    2025-01-10 [1] CRAN (R 4.4.2)
 devtools      2.4.5    2022-10-11 [1] CRAN (R 4.4.2)
 digest        0.6.37   2024-08-19 [1] CRAN (R 4.4.2)
 dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.1)
 effectsize  * 1.0.0    2024-12-10 [1] CRAN (R 4.4.2)
 ellipsis      0.3.2    2021-04-29 [1] CRAN (R 4.4.2)
 fastmap       1.2.0    2024-05-15 [1] CRAN (R 4.4.2)
 forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.1)
 Formula       1.2-5    2023-02-24 [1] CRAN (R 4.4.0)
 fs            1.6.5    2024-10-30 [1] CRAN (R 4.4.2)
 generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.1)
 ggeffects     1.7.2    2024-10-13 [1] CRAN (R 4.4.1)
 ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.1)
 ggtext      * 0.1.2    2022-09-16 [1] CRAN (R 4.4.1)
 glue          1.8.0    2024-09-30 [1] CRAN (R 4.4.1)
 gridtext      0.1.5    2022-09-16 [1] CRAN (R 4.4.1)
 gtable        0.3.6    2024-10-25 [1] CRAN (R 4.4.2)
 htmltools     0.5.8.1  2024-04-04 [1] CRAN (R 4.4.2)
 htmlwidgets   1.6.4    2023-12-06 [1] CRAN (R 4.4.2)
 httpuv        1.6.15   2024-03-26 [1] CRAN (R 4.4.2)
 insight       1.0.2    2025-02-06 [1] CRAN (R 4.4.2)
 knitr         1.48     2024-07-07 [1] CRAN (R 4.4.1)
 later         1.4.1    2024-11-27 [1] CRAN (R 4.4.2)
 lattice       0.22-6   2024-03-20 [2] CRAN (R 4.4.2)
 lifecycle     1.0.4    2023-11-07 [1] CRAN (R 4.4.1)
 lme4        * 1.1-35.5 2024-07-03 [1] CRAN (R 4.4.1)
 magrittr      2.0.3    2022-03-30 [1] CRAN (R 4.4.1)
 MASS          7.3-61   2024-06-13 [2] CRAN (R 4.4.2)
 Matrix      * 1.7-1    2024-10-18 [2] CRAN (R 4.4.2)
 memoise       2.0.1    2021-11-26 [1] CRAN (R 4.4.2)
 mime          0.12     2021-09-28 [1] CRAN (R 4.4.0)
 miniUI        0.1.1.1  2018-05-18 [1] CRAN (R 4.4.2)
 minqa         1.2.8    2024-08-17 [1] CRAN (R 4.4.1)
 munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.1)
 nlme          3.1-166  2024-08-14 [2] CRAN (R 4.4.2)
 nloptr        2.1.1    2024-06-25 [1] CRAN (R 4.4.1)
 parameters    0.24.1   2025-01-14 [1] CRAN (R 4.4.2)
 pbkrtest    * 0.5.3    2024-06-26 [1] CRAN (R 4.4.1)
 performance   0.13.0   2025-01-15 [1] CRAN (R 4.4.2)
 pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
 pkgbuild      1.4.6    2025-01-16 [1] CRAN (R 4.4.2)
 pkgconfig     2.0.3    2019-09-22 [1] CRAN (R 4.4.1)
 pkgload       1.4.0    2024-06-28 [1] CRAN (R 4.4.2)
 profvis       0.4.0    2024-09-20 [1] CRAN (R 4.4.2)
 promises      1.3.2    2024-11-28 [1] CRAN (R 4.4.2)
 purrr         1.0.4    2025-02-05 [1] CRAN (R 4.4.3)
 R6            2.6.1    2025-02-15 [1] CRAN (R 4.4.2)
 Rcpp          1.0.13   2024-07-17 [1] CRAN (R 4.4.1)
 remotes       2.5.0    2024-03-17 [1] CRAN (R 4.4.2)
 rlang         1.1.5    2025-01-17 [1] CRAN (R 4.4.3)
 ROCR        * 1.0-11   2020-05-02 [1] CRAN (R 4.4.1)
 rstudioapi    0.17.1   2024-10-22 [1] CRAN (R 4.4.2)
 scales        1.3.0    2023-11-28 [1] CRAN (R 4.4.1)
 sessioninfo   1.2.3    2025-02-05 [1] CRAN (R 4.4.2)
 shiny         1.10.0   2024-12-14 [1] CRAN (R 4.4.2)
 sjlabelled    1.2.0    2022-04-10 [1] CRAN (R 4.4.1)
 sjmisc        2.8.10   2024-05-13 [1] CRAN (R 4.4.1)
 sjPlot      * 2.8.16   2024-05-13 [1] CRAN (R 4.4.1)
 sjstats       0.19.0   2024-05-14 [1] CRAN (R 4.4.1)
 tibble        3.2.1    2023-03-20 [1] CRAN (R 4.4.1)
 tidyr       * 1.3.1    2024-01-24 [1] CRAN (R 4.4.1)
 tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.4.1)
 urlchecker    1.0.1    2021-11-30 [1] CRAN (R 4.4.2)
 usethis       3.1.0    2024-11-26 [1] CRAN (R 4.4.2)
 vctrs         0.6.5    2023-12-01 [1] CRAN (R 4.4.1)
 withr         3.0.2    2024-10-28 [1] CRAN (R 4.4.2)
 xfun          0.48     2024-10-03 [1] CRAN (R 4.4.1)
 xml2          1.3.6    2023-12-04 [1] CRAN (R 4.4.1)
 xtable        1.8-4    2019-04-21 [1] CRAN (R 4.4.2)

 [1] C:/Users/alexw/AppData/Local/R/win-library/4.4
 [2] C:/Program Files/R/R-4.4.2/library
 * -- Packages attached to the search path.

-----------------------------------------------------------------------------------------------------------------------

```

