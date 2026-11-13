# Lab 5: Correlation, National Climate Policy, and Country
Characteristics
Shima Khodagholi
2026-11-13

- [<span class="toc-section-number">1</span>
  Introduction](#introduction)
  - [<span class="toc-section-number">1.1</span> Learning
    objectives](#learning-objectives)
- [<span class="toc-section-number">2</span> 1. Load packages and
  inspect the data](#1-load-packages-and-inspect-the-data)
  - [<span class="toc-section-number">2.1</span> Variables in the
    dataset](#variables-in-the-dataset)
- [<span class="toc-section-number">3</span> 2. Explore the
  distributions of the
  variables](#2-explore-the-distributions-of-the-variables)
- [<span class="toc-section-number">4</span> 3. Calculate correlations
  for 2020](#3-calculate-correlations-for-2020)
- [<span class="toc-section-number">5</span> 4. Compare correlations
  between 2000 and 2020](#4-compare-correlations-between-2000-and-2020)
- [<span class="toc-section-number">6</span> 5. Examine changes within
  countries](#5-examine-changes-within-countries)
- [<span class="toc-section-number">7</span> Challenge!](#challenge)
  - [<span class="toc-section-number">7.1</span> Final submission
    stuff](#final-submission-stuff)

## Introduction

How are national climate policies related to countries’ political
institutions, economic development, and carbon emissions? Do these
relationships look different in 2000 and 2020?

In this lab, you will use data from the OECD, the World Development
Indicators (WDI), and the Varieties of Democracy (V-Dem) project to
investigate these questions. You will use descriptive statistics,
visualizations, correlation coefficients, and statistical tests to
explore patterns across 34 countries.

Correlation can help identify relationships between variables, but it
cannot establish that one variable causes another. For example, an
association between democracy and climate-policy stringency does not
demonstrate that democratic institutions cause stronger climate
policies.

### Learning objectives

This lab assesses the following learning standards:

10. Derive new variables from existing ones through calculated
    transformations, including unit conversions, differences across time
    periods, and categorical dummy variables.
11. Assess the normality of a variable and use those results to select
    between parametric and non-parametric statistical tests.
12. Conduct correlation analysis across multiple variable pairs and
    evaluate results with respect to direction, magnitude, and
    significance, including changes in correlation structure across time
    periods or subgroups.

## 1. Load packages and inspect the data

We will use `tidyverse` for data manipulation and visualization, `Hmisc`
to calculate correlation matrices, and `corrplot` to visualize those
matrices.

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.6
    ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ✔ ggplot2   4.0.1     ✔ tibble    3.3.1
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
    ✔ purrr     1.2.1     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(Hmisc)
```


    Attaching package: 'Hmisc'

    The following objects are masked from 'package:dplyr':

        src, summarize

    The following objects are masked from 'package:base':

        format.pval, units

``` r
library(corrplot)
```

    corrplot 0.95 loaded

``` r
library(knitr)

all_vars<-read_csv("data/climpolicy_combined.csv")
```

    Rows: 68 Columns: 8
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (2): country_text_id, country_name
    dbl (6): year, oecd_climpol_stringent, co2_emissions, gdp, tot_population, v...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The combined dataset, `all_vars`, contains observations for 34 countries
in 2000 and 2020. Each row represents a country in a particular year.

### Variables in the dataset

| Variable | Description |
|:---|:---|
| `country_name` | Country name |
| `country_text_id` | Country identifier |
| `year` | Observation year |
| `oecd_climpol_stringent` | OECD-derived climate-policy stringency measure |
| `co2_emissions` | Total CO₂ emissions measure |
| `gdp` | Gross domestic product (constant 2015 US dollars) |
| `tot_population` | Total population |
| `v2x_polyarchy` | V-Dem electoral democracy index |

We will also calculate GDP per capita and CO₂ emissions per capita.

``` r
all_vars <- all_vars %>%
  mutate(
    gdp_per_capita = gdp / tot_population,
    co2_per_capita = co2_emissions * 1e6 / tot_population
  )
```

This calculation assumes `co2_emissions` is in million tonnes and
population is measured in persons. The resulting `co2_per_capita`
measure is tonnes per person. GDP per capita is in 2015 US dollars,
divided by population.

Inspect the new variables.

``` r
all_vars %>%
  select(
    country_name, year, oecd_climpol_stringent,
    co2_per_capita, gdp_per_capita, v2x_polyarchy
  ) %>%
  slice_head(n = 10) %>%
  kable(digits = 2)
```

| country_name | year | oecd_climpol_stringent | co2_per_capita | gdp_per_capita | v2x_polyarchy |
|:---|---:|---:|---:|---:|---:|
| Iceland | 2000 | 0.00 | 10.14 | 43472.34 | 0.87 |
| Iceland | 2020 | 1.00 | 8.38 | 52464.31 | 0.86 |
| Ireland | 2020 | 3.00 | 6.80 | 80927.94 | 0.89 |
| Denmark | 2000 | 2.00 | 9.95 | 49266.99 | 0.91 |
| Australia | 2000 | 0.83 | 18.60 | 45979.08 | 0.89 |
| Australia | 2020 | 2.92 | 15.30 | 58377.77 | 0.84 |
| Austria | 2000 | 1.89 | 8.32 | 38571.09 | 0.86 |
| Austria | 2020 | 3.31 | 7.06 | 43428.70 | 0.86 |
| Belgium | 2000 | 1.56 | 12.18 | 35878.32 | 0.88 |
| Belgium | 2020 | 3.44 | 7.97 | 40769.02 | 0.90 |

**Task 1** *Why might GDP per capita and CO₂ emissions per capita be
more useful than total GDP and total emissions when comparing
countries?*

{Your response here.}

## 2. Explore the distributions of the variables

Before calculating correlations, examine the distributions of the
variables. Correlation coefficients summarize relationships between
variables, and their interpretation can be affected by skewed
distributions and unusual observations.

We will focus on four measures:

- Climate-policy stringency
- CO₂ emissions per capita
- GDP per capita
- Electoral democracy

Create a subset for 2020.

``` r
data_2020 <- all_vars %>%
  filter(year == 2020)
```

We can then create histograms to examine the distributions for each of
the four variables of interest.

``` r
data_2020 %>%
  select(
    oecd_climpol_stringent,
    co2_per_capita,
    gdp_per_capita,
    v2x_polyarchy
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 12, color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distributions of National Characteristics, 2020",
    x = NULL,
    y = "Number of countries"
  )
```

![](Lab5_correlation_climate_files/figure-commonmark/unnamed-chunk-4-1.png)

The facets use different horizontal scales because the variables have
different units.

You can also examine normal Q-Q plots.

``` r
data_2020 %>%
  select(
    oecd_climpol_stringent,
    co2_per_capita,
    gdp_per_capita,
    v2x_polyarchy
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Normal Q-Q Plots, 2020",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  )
```

![](Lab5_correlation_climate_files/figure-commonmark/unnamed-chunk-5-1.png)

**Task 2** *Which variables appear most skewed? Pick one of these and
identify some of the countries that are outliers for this variable.*

> The histograms show that the data is not normally distributed.
> V2x_polyarchy shows the highest skew followed by
> oecd_climpol_stringent.

``` r
data_2020 %>%
  select(co2_per_capita, gdp_per_capita, oecd_climpol_stringent, v2x_polyarchy) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(W = shapiro.test(value)$statistic,
            p_value = shapiro.test(value)$p.value) %>%
  kable(digits = 3)
```

| variable               |     W | p_value |
|:-----------------------|------:|--------:|
| co2_per_capita         | 0.916 |   0.013 |
| gdp_per_capita         | 0.934 |   0.041 |
| oecd_climpol_stringent | 0.924 |   0.021 |
| v2x_polyarchy          | 0.752 |   0.000 |

``` r
data_2020 %>%
  select(country_name, co2_per_capita) %>%
  arrange(desc(co2_per_capita)) %>%
  kable(digits =2, col.names = c("Country", "co2 per capita"))
```

| Country        | co2 per capita |
|:---------------|---------------:|
| Australia      |          15.30 |
| Canada         |          14.42 |
| Luxembourg     |          12.68 |
| Estonia        |           9.13 |
| Czechia        |           8.87 |
| Japan          |           8.40 |
| Iceland        |           8.38 |
| Netherlands    |           8.15 |
| Norway         |           8.12 |
| Belgium        |           7.97 |
| Poland         |           7.91 |
| Germany        |           7.72 |
| South Africa   |           7.44 |
| Finland        |           7.14 |
| Austria        |           7.06 |
| New Zealand    |           6.84 |
| Ireland        |           6.80 |
| Israel         |           6.66 |
| Slovenia       |           6.44 |
| Hungary        |           5.13 |
| Italy          |           4.97 |
| Greece         |           4.89 |
| United Kingdom |           4.79 |
| Denmark        |           4.77 |
| Spain          |           4.53 |
| Chile          |           4.52 |
| France         |           4.25 |
| Switzerland    |           4.16 |
| Portugal       |           3.95 |
| Sweden         |           3.67 |
| Mexico         |           3.46 |
| Indonesia      |           2.17 |
| Brazil         |           2.15 |
| India          |           1.68 |

> Based on the table, the outliers for co2 per capita are Australia,
> Canada, Luxembourg in the high end, and India, Brazil, and Indonesia
> in the lower end.

## 3. Calculate correlations for 2020

Correlation coefficients measure the direction and strength of
relationships between two numerical variables. Pearson’s correlation
coefficient is typically used when variables are approximately normally
distributed and exhibit a linear relationship, whereas Spearman’s rank
correlation coefficient is appropriate for non-normal or ordinal data.

**Task 3** *Select just the four variables of interest and create a
correlation matrix using the appropriate measure of correlation. Print
this table once you’ve created it.*

``` r
vars_2020 <- data_2020 %>%
  select(co2_per_capita, gdp_per_capita, oecd_climpol_stringent, v2x_polyarchy)

cor_2020 <- rcorr(as.matrix(vars_2020), type = "spearman")
cor_2020
```

                           co2_per_capita gdp_per_capita oecd_climpol_stringent
    co2_per_capita                   1.00           0.42                   0.20
    gdp_per_capita                   0.42           1.00                   0.57
    oecd_climpol_stringent           0.20           0.57                   1.00
    v2x_polyarchy                    0.13           0.72                   0.50
                           v2x_polyarchy
    co2_per_capita                  0.13
    gdp_per_capita                  0.72
    oecd_climpol_stringent          0.50
    v2x_polyarchy                   1.00

    n= 34 


    P
                           co2_per_capita gdp_per_capita oecd_climpol_stringent
    co2_per_capita                        0.0145         0.2652                
    gdp_per_capita         0.0145                        0.0004                
    oecd_climpol_stringent 0.2652         0.0004                               
    v2x_polyarchy          0.4473         0.0000         0.0026                
                           v2x_polyarchy
    co2_per_capita         0.4473       
    gdp_per_capita         0.0000       
    oecd_climpol_stringent 0.0026       
    v2x_polyarchy                       

**Task 4** *Create a correlation plot from this matrix using `corrplot`
and the parameters of your choosing*

``` r
corrplot(cor_2020$r)
```

![](Lab5_correlation_climate_files/figure-commonmark/Task4-1.png)

**Task 5** Based on your correlation matrix and correlation plot, is
climate-policy stringency associated with electoral democracy, GDP per
capita, or CO₂ emissions per capita? Describe the direction and strength
of each relationship. Do the results support your expectations?

## 4. Compare correlations between 2000 and 2020

Next, examine whether relationships between climate policy and national
characteristics differ across the two years. First, we can create a new
dataset for the observations in 2000.

``` r
data_2000 <- all_vars %>%
  filter(year == 2000)
```

**Task 6** *Create a new correlation matrix for 2000 and print it, just
as you did for the 2020 data. Then print both matrices so they can be
compared side-by-side.*

``` r
vars_2000 <- data_2000 %>%
  select(co2_per_capita, gdp_per_capita, oecd_climpol_stringent, v2x_polyarchy)

cor_2000 <- rcorr(as.matrix(vars_2000), type = "spearman")
cor_2000
```

                           co2_per_capita gdp_per_capita oecd_climpol_stringent
    co2_per_capita                   1.00           0.57                   0.23
    gdp_per_capita                   0.57           1.00                   0.51
    oecd_climpol_stringent           0.23           0.51                   1.00
    v2x_polyarchy                    0.31           0.52                   0.44
                           v2x_polyarchy
    co2_per_capita                  0.31
    gdp_per_capita                  0.52
    oecd_climpol_stringent          0.44
    v2x_polyarchy                   1.00

    n= 34 


    P
                           co2_per_capita gdp_per_capita oecd_climpol_stringent
    co2_per_capita                        0.0005         0.1958                
    gdp_per_capita         0.0005                        0.0023                
    oecd_climpol_stringent 0.1958         0.0023                               
    v2x_polyarchy          0.0761         0.0015         0.0086                
                           v2x_polyarchy
    co2_per_capita         0.0761       
    gdp_per_capita         0.0015       
    oecd_climpol_stringent 0.0086       
    v2x_polyarchy                       

``` r
corrplot(cor_2000$r)
```

![](Lab5_correlation_climate_files/figure-commonmark/Task6-1.png)

``` r
par(mfrow = c(1, 2))
corrplot(cor_2000$r, title = "Spearman Correlation, 2000")
corrplot(cor_2020$r, title = "Spearman Correlation, 2020")
```

![](Lab5_correlation_climate_files/figure-commonmark/Task6-2.png)

**Task 7** How did the correlation between climate-policy stringency and
electoral democracy change between 2000 and 2020? What about the
relationship between policy stringency and CO₂ emissions per capita?

> Correlation between climate-policy stringency and electoral democracy
> drastically decreases from 0.50 to 0.0026. Correlation between policy
> stringency and CO₂ emissions per capita increased from 0.2 to 0.26.

## 5. Examine changes within countries

Remember that these matrices describe relationships across countries
within each year. A change in a correlation across years is not the same
as a change experienced by an individual country. To examine changes
over time, we can reshape the dataset so that each country’s 2000 and
2020 observations appear in the same row.

``` r
country_changes <- all_vars %>%
  select(
    country_text_id,
    country_name,
    year,
    oecd_climpol_stringent,
    co2_per_capita,
    gdp_per_capita,
    v2x_polyarchy
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(
      oecd_climpol_stringent,
      co2_per_capita,
      gdp_per_capita,
      v2x_polyarchy
    ),
    names_sep = "_"
  ) 
```

**Task 8** *Calculate the change in each variable between 2000 and 2020
for every country.*

**Task 9** *To assess these changes, create a correlation matrix for
these four new variables. This will show *correlation in changes* across
countries.*

**Task 10** Identify the five countries with the largest increases and
the five countries with the largest decreases in climate-policy
stringency. Repeat this analysis for CO₂ emissions per capita.

{Your response here}

**Task 11** How does this within-country change analysis differ from
correlating climate-policy stringency and emissions across countries in
2020? Why might the two analyses produce different results?

{Your response here}

## Challenge!

Let’s map those variables showing change. The code below downloads
country boundaries from Natural Earth
(https://www.naturalearthdata.com/). Join your final dataset to these
boundaries and use `tmap` to create maps of each of the four variables
showing change from 2000-2020. What patterns do you see? How does it add
to/change your findings from the main part of the lab?

``` r
# install.packages("rnaturalearth")
library(rnaturalearth)
countries<-ne_download(type="admin_0_countries")
```

    Reading 'ne_110m_admin_0_countries.zip' from naturalearth...

### Final submission stuff

**Disclosure of assistance:** Besides class materials, what other
sources of assistance did you use while completing this lab? These can
include input from classmates, relevant material identified through web
searches (e.g., Stack Overflow), or assistance from ChatGPT or other AI
tools. How did these sources support your own learning in completing
this lab?

{Response here.}

**Lab reflection:** How do you feel about the work you did on this lab?
Was it easy, moderate, or hard? What are the biggest things you learned
by completing it?

> The lab felt moderate. The biggest thing I learned was creating
> correlation matrix and plots and interpret them.
