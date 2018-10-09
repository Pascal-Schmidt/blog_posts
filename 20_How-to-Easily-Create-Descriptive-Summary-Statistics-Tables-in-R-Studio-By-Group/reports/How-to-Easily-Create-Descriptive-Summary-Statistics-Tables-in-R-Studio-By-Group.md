How to Easily Create Descriptive Summary Statistics Tables in R Studio - By Group
================
Pascal Schmidt
July 11, 2018

``` r
library(arsenal)
library(tidyverse)
library(gapminder)
data(gapminder)
```

Summary statistics tables or an exploratory data analysis are the most common ways in order to familiarize oneself with a data set. In addition to that, summary statistics tables are very easy and fast to create and therefore, so common. In this blog post I will be show you how to create descriptive summary statistics tables in R. Almost all of these packages can create a normal descriptive summary statistic table in R and also one by groupings. Meaning, we can choose a factor column and stratify this column by its levels (very useful!). Moreover, one can easily knit their results to html, pdf, or word. This is a great way to use these tables in one's report or presentation.

Let's get started with a quick look at the packages we are going to present:

-   arsenal
-   qwraps2
-   amisc
-   table1
-   tangram
-   furniture
-   tableone
-   compareGroups
-   htmltable

### Choosing our Data Set to Create Descriptive Summary Statistics Tables in R

For all of these packages I am providing some code which shows the basics behind the tables and their functionality. For additional information, there is a link to the corresponding vignette which has even more examples and code snippets. In order for you to follow my code, I used the gapminder data set from the gapminder package.

In the code below, I am modifying the `gapminder` data set a little bit. I transformed the `gdpPercap` column to a factor variable with two levels. High is for countries with `gdpPercap` higher than the median `gdpPercap` and low for lower than the median `gdpPercap`. After that I divided the population by one million to make the table more readable. In addition to that I also randomly introduced missing values in the data. I did that because in the real world we rarely experience data sets without any NA values. Therefore, it is important to know how different packages deal with missing values.

``` r
median_gdp <- median(gapminder$gdpPercap)
gapminder %>%
  select(-country) %>%
  mutate(gdpPercap = ifelse(gdpPercap > median_gdp, "high", "low")) %>%
  mutate(gdpPercap = factor(gdpPercap)) %>%
  mutate(pop = pop / 1000000) -> gapminder

gapminder <- lapply(gapminder, function(x) x[sample(c(TRUE, NA),
    prob = c(0.9, 0.1),
    size = length(x),
    replace = TRUE
  )])
```

Let's start and create descriptive summary statistics tables in R.

### Create Descriptive Summary Statistics Tables in R with arsenal

`arsenal` is my favourite package. It has so much functionality that we essentially could stop right here. We can basically customize anything and the best part about the packages is that it requires only little code.

In the code block below, we are displaying how to create a table with the `tableby()` function and only two lines of code.

``` r
table_one <- tableby(continent ~ ., data = gapminder)
summary(table_one, title = "Gapminder Data")
```

<table>
<caption>Gapminder Data</caption>
<colgroup>
<col width="17%" />
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="5%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"></th>
<th align="center">Africa (N=564)</th>
<th align="center">Americas (N=273)</th>
<th align="center">Asia (N=360)</th>
<th align="center">Europe (N=314)</th>
<th align="center">Oceania (N=22)</th>
<th align="center">Total (N=1533)</th>
<th align="right">p value</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><strong>year</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">0.984</td>
</tr>
<tr class="even">
<td align="left">   N-Miss</td>
<td align="center">58</td>
<td align="center">27</td>
<td align="center">30</td>
<td align="center">19</td>
<td align="center">1</td>
<td align="center">135</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Mean (SD)</td>
<td align="center">1979.549 (17.293)</td>
<td align="center">1979.398 (17.283)</td>
<td align="center">1979.576 (17.343)</td>
<td align="center">1979.424 (16.937)</td>
<td align="center">1981.762 (17.283)</td>
<td align="center">1979.536 (17.206)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Range</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left"><strong>lifeExp</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">&lt; 0.001</td>
</tr>
<tr class="even">
<td align="left">   N-Miss</td>
<td align="center">47</td>
<td align="center">19</td>
<td align="center">37</td>
<td align="center">20</td>
<td align="center">1</td>
<td align="center">124</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Mean (SD)</td>
<td align="center">48.923 (9.251)</td>
<td align="center">64.676 (9.359)</td>
<td align="center">59.759 (11.758)</td>
<td align="center">71.862 (5.565)</td>
<td align="center">74.716 (3.804)</td>
<td align="center">59.418 (12.910)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Range</td>
<td align="center">23.599 - 76.442</td>
<td align="center">37.579 - 80.653</td>
<td align="center">30.332 - 82.603</td>
<td align="center">43.585 - 81.757</td>
<td align="center">69.390 - 81.235</td>
<td align="center">23.599 - 82.603</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left"><strong>pop</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">&lt; 0.001</td>
</tr>
<tr class="even">
<td align="left">   N-Miss</td>
<td align="center">53</td>
<td align="center">29</td>
<td align="center">43</td>
<td align="center">28</td>
<td align="center">1</td>
<td align="center">154</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Mean (SD)</td>
<td align="center">9.765 (14.865)</td>
<td align="center">23.779 (50.164)</td>
<td align="center">76.040 (207.810)</td>
<td align="center">17.556 (20.639)</td>
<td align="center">8.331 (6.595)</td>
<td align="center">29.074 (105.840)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Range</td>
<td align="center">0.060 - 119.901</td>
<td align="center">0.663 - 301.140</td>
<td align="center">0.120 - 1318.683</td>
<td align="center">0.165 - 82.401</td>
<td align="center">1.995 - 20.434</td>
<td align="center">0.060 - 1318.683</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left"><strong>gdpPercap</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">&lt; 0.001</td>
</tr>
<tr class="even">
<td align="left">   N-Miss</td>
<td align="center">61</td>
<td align="center">32</td>
<td align="center">35</td>
<td align="center">36</td>
<td align="center">1</td>
<td align="center">165</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   high</td>
<td align="center">84 (16.7%)</td>
<td align="center">178 (73.9%)</td>
<td align="center">134 (41.2%)</td>
<td align="center">259 (93.2%)</td>
<td align="center">21 (100.0%)</td>
<td align="center">676 (49.4%)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   low</td>
<td align="center">419 (83.3%)</td>
<td align="center">63 (26.1%)</td>
<td align="center">191 (58.8%)</td>
<td align="center">19 (6.8%)</td>
<td align="center">0 (0.0%)</td>
<td align="center">692 (50.6%)</td>
<td align="right"></td>
</tr>
</tbody>
</table>

Obviously, this table is far from perfect but especially when we are dealing with large data sets, these two lines are very powerful.

In the next code block, we are customizing our table. We are now adding a median with first and third quantiles and are also changing the order of how the statistics are displayed. The argument `Nmiss2` shows the missing values and if there are none, it shows 0. If you put the argument" `Nmiss` and there are no missing values, then it won't display a line for missing values. Moreover, we can display the missing values not only as counts but also as percentages (more examples in the [vignette](https://cran.r-project.org/web/packages/arsenal/vignettes/tableby.html)).

For categorical variables the table uses a chi-squared test and for numerical variables it uses a Kruskal Wallis test for calculating p-values. However, we can use many different tests like an f-test statistic. In fact, we can add our own p-values if we would like (more in the [vignette](https://cran.r-project.org/web/packages/arsenal/vignettes/tableby.html)).

We can also label our columns with more appropriate names and add a title to our table.

``` r
my_controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing"
  )
)


my_labels <- list(
  lifeExp = "Life Expectancy",
  pop = "Population (million)",
  gdpPercap = "GDP per capita",
  year = "Year"
)


table_two <- tableby(continent ~ .,
  data = gapminder,
  control = my_controls
)

summary(table_two,
  labelTranslations = my_labels,
  title = "Summary Statistic of Gapminder Data"
)
```

<table style="width:100%;">
<caption>Summary Statistic of Gapminder Data</caption>
<colgroup>
<col width="14%" />
<col width="13%" />
<col width="13%" />
<col width="13%" />
<col width="13%" />
<col width="13%" />
<col width="13%" />
<col width="3%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"></th>
<th align="center">Africa (N=564)</th>
<th align="center">Americas (N=273)</th>
<th align="center">Asia (N=360)</th>
<th align="center">Europe (N=314)</th>
<th align="center">Oceania (N=22)</th>
<th align="center">Total (N=1533)</th>
<th align="right">p value</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><strong>Year</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">0.984</td>
</tr>
<tr class="even">
<td align="left">   Mean (SD)</td>
<td align="center">1979.549 (17.293)</td>
<td align="center">1979.398 (17.283)</td>
<td align="center">1979.576 (17.343)</td>
<td align="center">1979.424 (16.937)</td>
<td align="center">1981.762 (17.283)</td>
<td align="center">1979.536 (17.206)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Median (Q1, Q3)</td>
<td align="center">1982.000 (1962.000, 1995.750)</td>
<td align="center">1977.000 (1967.000, 1997.000)</td>
<td align="center">1977.000 (1963.250, 1997.000)</td>
<td align="center">1982.000 (1967.000, 1992.000)</td>
<td align="center">1982.000 (1967.000, 1997.000)</td>
<td align="center">1982.000 (1963.250, 1995.750)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Min - Max</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Missing</td>
<td align="center">58</td>
<td align="center">27</td>
<td align="center">30</td>
<td align="center">19</td>
<td align="center">1</td>
<td align="center">135</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left"><strong>Life Expectancy</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">&lt; 0.001</td>
</tr>
<tr class="odd">
<td align="left">   Mean (SD)</td>
<td align="center">48.923 (9.251)</td>
<td align="center">64.676 (9.359)</td>
<td align="center">59.759 (11.758)</td>
<td align="center">71.862 (5.565)</td>
<td align="center">74.716 (3.804)</td>
<td align="center">59.418 (12.910)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Median (Q1, Q3)</td>
<td align="center">47.784 (42.460, 54.425)</td>
<td align="center">67.055 (58.336, 71.498)</td>
<td align="center">61.195 (50.948, 69.270)</td>
<td align="center">72.211 (69.612, 75.550)</td>
<td align="center">73.840 (71.520, 77.560)</td>
<td align="center">60.430 (48.211, 70.805)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Min - Max</td>
<td align="center">23.599 - 76.442</td>
<td align="center">37.579 - 80.653</td>
<td align="center">30.332 - 82.603</td>
<td align="center">43.585 - 81.757</td>
<td align="center">69.390 - 81.235</td>
<td align="center">23.599 - 82.603</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Missing</td>
<td align="center">47</td>
<td align="center">19</td>
<td align="center">37</td>
<td align="center">20</td>
<td align="center">1</td>
<td align="center">124</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left"><strong>Population (million)</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">&lt; 0.001</td>
</tr>
<tr class="even">
<td align="left">   Mean (SD)</td>
<td align="center">9.765 (14.865)</td>
<td align="center">23.779 (50.164)</td>
<td align="center">76.040 (207.810)</td>
<td align="center">17.556 (20.639)</td>
<td align="center">8.331 (6.595)</td>
<td align="center">29.074 (105.840)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Median (Q1, Q3)</td>
<td align="center">4.657 (1.350, 10.865)</td>
<td align="center">6.313 (2.989, 17.964)</td>
<td align="center">14.117 (3.867, 45.598)</td>
<td align="center">8.606 (4.421, 22.372)</td>
<td align="center">3.908 (3.165, 14.074)</td>
<td align="center">7.021 (2.821, 18.577)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Min - Max</td>
<td align="center">0.060 - 119.901</td>
<td align="center">0.663 - 301.140</td>
<td align="center">0.120 - 1318.683</td>
<td align="center">0.165 - 82.401</td>
<td align="center">1.995 - 20.434</td>
<td align="center">0.060 - 1318.683</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Missing</td>
<td align="center">53</td>
<td align="center">29</td>
<td align="center">43</td>
<td align="center">28</td>
<td align="center">1</td>
<td align="center">154</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left"><strong>GDP per capita</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">&lt; 0.001</td>
</tr>
<tr class="odd">
<td align="left">   high</td>
<td align="center">84 (16.7%)</td>
<td align="center">178 (73.9%)</td>
<td align="center">134 (41.2%)</td>
<td align="center">259 (93.2%)</td>
<td align="center">21 (100.0%)</td>
<td align="center">676 (49.4%)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   low</td>
<td align="center">419 (83.3%)</td>
<td align="center">63 (26.1%)</td>
<td align="center">191 (58.8%)</td>
<td align="center">19 (6.8%)</td>
<td align="center">0 (0.0%)</td>
<td align="center">692 (50.6%)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Missing</td>
<td align="center">61</td>
<td align="center">32</td>
<td align="center">35</td>
<td align="center">36</td>
<td align="center">1</td>
<td align="center">165</td>
<td align="right"></td>
</tr>
</tbody>
</table>

Another nice feature of this package is that we can stratify our table by more than one grouping variable. Here, we group by continent and `gdpPercap`.

``` r
table_three <- tableby(interaction(continent, gdpPercap) ~ .,
  data = gapminder,
  control = my_controls
)

summary(table_three,
  labelTranslations = my_labels,
  title = "Summary Statistic of Gapminder Data"
)
```

<table>
<caption>Summary Statistic of Gapminder Data</caption>
<colgroup>
<col width="9%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="2%" />
</colgroup>
<thead>
<tr class="header">
<th align="left"></th>
<th align="center">Africa.high (N=84)</th>
<th align="center">Americas.high (N=178)</th>
<th align="center">Asia.high (N=134)</th>
<th align="center">Europe.high (N=259)</th>
<th align="center">Oceania.high (N=21)</th>
<th align="center">Africa.low (N=419)</th>
<th align="center">Americas.low (N=63)</th>
<th align="center">Asia.low (N=191)</th>
<th align="center">Europe.low (N=19)</th>
<th align="center">Total (N=1368)</th>
<th align="right">p value</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><strong>Year</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">&lt; 0.001</td>
</tr>
<tr class="even">
<td align="left">   Mean (SD)</td>
<td align="center">1988.053 (14.884)</td>
<td align="center">1982.472 (16.638)</td>
<td align="center">1984.480 (16.099)</td>
<td align="center">1980.272 (16.809)</td>
<td align="center">1981.500 (17.689)</td>
<td align="center">1977.816 (17.155)</td>
<td align="center">1969.119 (15.459)</td>
<td align="center">1976.943 (17.354)</td>
<td align="center">1962.833 (13.853)</td>
<td align="center">1979.480 (17.218)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Median (Q1, Q3)</td>
<td align="center">1987.000 (1977.000, 2002.000)</td>
<td align="center">1982.000 (1969.500, 1997.000)</td>
<td align="center">1987.000 (1972.000, 1997.000)</td>
<td align="center">1982.000 (1967.000, 1992.000)</td>
<td align="center">1982.000 (1967.000, 1997.000)</td>
<td align="center">1977.000 (1962.000, 1992.000)</td>
<td align="center">1967.000 (1957.000, 1982.000)</td>
<td align="center">1977.000 (1962.000, 1992.000)</td>
<td align="center">1957.000 (1952.000, 1967.000)</td>
<td align="center">1977.000 (1962.000, 1992.000)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Min - Max</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 2002.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="center">1952.000 - 1997.000</td>
<td align="center">1952.000 - 2007.000</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Missing</td>
<td align="center">8</td>
<td align="center">19</td>
<td align="center">11</td>
<td align="center">16</td>
<td align="center">1</td>
<td align="center">45</td>
<td align="center">4</td>
<td align="center">17</td>
<td align="center">1</td>
<td align="center">122</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left"><strong>Life Expectancy</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">&lt; 0.001</td>
</tr>
<tr class="odd">
<td align="left">   Mean (SD)</td>
<td align="center">59.177 (10.521)</td>
<td align="center">68.133 (6.805)</td>
<td align="center">67.884 (8.693)</td>
<td align="center">72.639 (4.358)</td>
<td align="center">74.736 (3.901)</td>
<td align="center">46.729 (7.410)</td>
<td align="center">55.037 (8.394)</td>
<td align="center">54.606 (10.251)</td>
<td align="center">61.810 (6.466)</td>
<td align="center">59.408 (12.904)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Median (Q1, Q3)</td>
<td align="center">59.797 (52.906, 67.064)</td>
<td align="center">69.582 (65.087, 72.529)</td>
<td align="center">69.390 (64.406, 73.600)</td>
<td align="center">72.390 (70.293, 75.915)</td>
<td align="center">73.665 (71.450, 77.877)</td>
<td align="center">46.230 (41.735, 51.531)</td>
<td align="center">56.168 (48.042, 61.664)</td>
<td align="center">56.060 (46.532, 62.474)</td>
<td align="center">61.130 (59.193, 65.600)</td>
<td align="center">60.461 (48.127, 70.777)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Min - Max</td>
<td align="center">31.999 - 76.442</td>
<td align="center">46.263 - 80.653</td>
<td align="center">39.875 - 82.603</td>
<td align="center">57.996 - 81.757</td>
<td align="center">69.390 - 81.235</td>
<td align="center">23.599 - 69.615</td>
<td align="center">40.414 - 70.836</td>
<td align="center">30.332 - 74.249</td>
<td align="center">48.079 - 72.950</td>
<td align="center">23.599 - 82.603</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Missing</td>
<td align="center">11</td>
<td align="center">15</td>
<td align="center">13</td>
<td align="center">17</td>
<td align="center">1</td>
<td align="center">33</td>
<td align="center">3</td>
<td align="center">19</td>
<td align="center">1</td>
<td align="center">113</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left"><strong>Population (million)</strong></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="right">&lt; 0.001</td>
</tr>
<tr class="even">
<td align="left">   Mean (SD)</td>
<td align="center">10.351 (17.528)</td>
<td align="center">29.458 (56.308)</td>
<td align="center">31.786 (124.995)</td>
<td align="center">18.523 (21.388)</td>
<td align="center">8.581 (6.663)</td>
<td align="center">9.572 (14.637)</td>
<td align="center">8.465 (14.824)</td>
<td align="center">116.525 (256.316)</td>
<td align="center">7.565 (9.148)</td>
<td align="center">30.811 (111.450)</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Median (Q1, Q3)</td>
<td align="center">1.343 (0.754, 10.276)</td>
<td align="center">8.141 (3.172, 24.657)</td>
<td align="center">6.249 (2.579, 22.041)</td>
<td align="center">8.890 (4.664, 24.997)</td>
<td align="center">4.012 (3.106, 14.352)</td>
<td align="center">4.847 (1.836, 10.719)</td>
<td align="center">4.041 (2.501, 5.968)</td>
<td align="center">21.266 (8.420, 70.145)</td>
<td align="center">3.585 (1.856, 7.900)</td>
<td align="center">7.106 (2.760, 19.373)</td>
<td align="right"></td>
</tr>
<tr class="even">
<td align="left">   Min - Max</td>
<td align="center">0.179 - 80.265</td>
<td align="center">0.765 - 301.140</td>
<td align="center">0.120 - 1318.683</td>
<td align="center">0.165 - 82.401</td>
<td align="center">1.995 - 20.434</td>
<td align="center">0.065 - 119.901</td>
<td align="center">0.663 - 76.039</td>
<td align="center">0.508 - 1280.400</td>
<td align="center">0.414 - 29.789</td>
<td align="center">0.065 - 1318.683</td>
<td align="right"></td>
</tr>
<tr class="odd">
<td align="left">   Missing</td>
<td align="center">11</td>
<td align="center">20</td>
<td align="center">19</td>
<td align="center">21</td>
<td align="center">1</td>
<td align="center">35</td>
<td align="center">6</td>
<td align="center">19</td>
<td align="center">4</td>
<td align="center">136</td>
<td align="right"></td>
</tr>
</tbody>
</table>

And of course, we can also only create a very simple table without any groupings.

``` r
table_four <- tableby(~year + continent + lifeExp + gdpPercap + pop, data = gapminder)
summary(table_four)
```

|               |   Overall (N=1704)  |
|:--------------|:-------------------:|
| **year**      |                     |
|    N-Miss     |         156         |
|    Mean (SD)  |  1979.484 (17.261)  |
|    Range      | 1952.000 - 2007.000 |
| **continent** |                     |
|    N-Miss     |         171         |
|    Africa     |     564 (36.8%)     |
|    Americas   |     273 (17.8%)     |
|    Asia       |     360 (23.5%)     |
|    Europe     |     314 (20.5%)     |
|    Oceania    |      22 (1.4%)      |
| **lifeExp**   |                     |
|    N-Miss     |         144         |
|    Mean (SD)  |   59.521 (12.859)   |
|    Range      |   23.599 - 82.603   |
| **gdpPercap** |                     |
|    N-Miss     |         179         |
|    high       |     756 (49.6%)     |
|    low        |     769 (50.4%)     |
| **pop**       |                     |
|    N-Miss     |         170         |
|    Mean (SD)  |   29.104 (103.217)  |
|    Range      |   0.060 - 1318.683  |

I only covered the most essential parts of the package. Consequently, there is a lot more to discover. If you want to customize your tables even more, check out the [vignette](https://cran.r-project.org/web/packages/arsenal/vignettes/tableby.html) for the package which shows more in-depth examples.

### Create Descriptive Summary Statistics Tables in R with qwraps2

Another great package is the `qwraps2` package. It has very high flexibility for which we have to pay a price! The price we have to pay for it are lots of lines of code. Especially if we have a large data set with lots of columns and levels.

This package uses a nested list and the function `summary_table()` to create the statistics table.

``` r
library(qwraps2)
options(qwraps2_markup = "markdown")
gapminder <- as.data.frame(gapminder)
summary_statistics <-
  list(
    "Life Expectancy" =
      list(
        "mean (sd)" = ~qwraps2::mean_sd(lifeExp, na_rm = TRUE),
        "median (Q1, Q3)" = ~qwraps2::median_iqr(lifeExp, na_rm = TRUE),
        "min" = ~min(lifeExp, na.rm = TRUE),
        "max" = ~max(lifeExp, na.rm = TRUE),
        "Missing" = ~sum(is.na(lifeExp))
      ),
    "Population" =
      list(
        "mean (sd)" = ~qwraps2::mean_sd(pop, na_rm = TRUE),
        "median (Q1, Q3)" = ~qwraps2::median_iqr(pop, na_rm = TRUE),
        "min" = ~min(pop, na.rm = TRUE),
        "max" = ~max(pop, na.rm = TRUE),
        "Missing" = ~sum(is.na(pop))
      ),
    "GDP per Capita" =
      list(
        "High GDP per Capita" = ~qwraps2::n_perc(na.omit(gdpPercap) %in% "high"),
        "Low GDP per Capita" = ~qwraps2::n_perc(na.omit(gdpPercap) %in% "low"),
        "Missing" = ~sum(is.na(gdpPercap))
      )
  )

summary_table(gapminder, summary_statistics)
```

|                        | gapminder (N = 1704)        |
|:-----------------------|:----------------------------|
| **Life Expectancy**    |                             |
|    mean (sd)           | 1,560; 59.52 ± 12.86        |
|    median (Q1, Q3)     | 1,560; 60.71 (48.30, 70.82) |
|    min                 | 23.599                      |
|    max                 | 82.603                      |
|    Missing             | 144                         |
| **Population**         |                             |
|    mean (sd)           | 1,534; 29.10 ± 103.22       |
|    median (Q1, Q3)     | 1,534; 7.11 (2.83, 19.83)   |
|    min                 | 0.060011                    |
|    max                 | 1318.683                    |
|    Missing             | 170                         |
| **GDP per Capita**     |                             |
|    High GDP per Capita | 756 (49.57%)                |
|    Low GDP per Capita  | 769 (50.43%)                |
|    Missing             | 179                         |

As you can see, it is way more lines of code than the previous package. However, it has the great flexibility to customize every single line of our summary table. This is **awesome**!

Now, we are going to show how to display a table stratified by a grouping. The way to do that is with the `group_by` function from the `dplyr` package.

``` r
print(qwraps2::summary_table(
  dplyr::group_by(gapminder, continent),
  summary_statistics
),
rtitle = "Summary Statistics Table for the Gapminder Data Set"
)
```

<table style="width:100%;">
<colgroup>
<col width="23%" />
<col width="12%" />
<col width="13%" />
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Summary Statistics Table for the Gapminder Data Set</th>
<th align="left">continent: Africa (N = 564)</th>
<th align="left">continent: Americas (N = 273)</th>
<th align="left">continent: Asia (N = 360)</th>
<th align="left">continent: Europe (N = 314)</th>
<th align="left">continent: Oceania (N = 22)</th>
<th align="left">continent: NA (N = 171)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><strong>Life Expectancy</strong></td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
</tr>
<tr class="even">
<td align="left">   mean (sd)</td>
<td align="left">517; 48.92 ± 9.25</td>
<td align="left">254; 64.68 ± 9.36</td>
<td align="left">323; 59.76 ± 11.76</td>
<td align="left">294; 71.86 ± 5.56</td>
<td align="left">21; 74.72 ± 3.80</td>
<td align="left">151; 60.48 ± 12.37</td>
</tr>
<tr class="odd">
<td align="left">   median (Q1, Q3)</td>
<td align="left">517; 47.78 (42.46, 54.42)</td>
<td align="left">254; 67.05 (58.34, 71.50)</td>
<td align="left">323; 61.20 (50.95, 69.27)</td>
<td align="left">294; 72.21 (69.61, 75.55)</td>
<td align="left">21; 73.84 (71.52, 77.56)</td>
<td align="left">151; 62.61 (50.80, 71.25)</td>
</tr>
<tr class="even">
<td align="left">   min</td>
<td align="left">23.599</td>
<td align="left">37.579</td>
<td align="left">30.332</td>
<td align="left">43.585</td>
<td align="left">69.390</td>
<td align="left">34.078</td>
</tr>
<tr class="odd">
<td align="left">   max</td>
<td align="left">76.442</td>
<td align="left">80.653</td>
<td align="left">82.603</td>
<td align="left">81.757</td>
<td align="left">81.235</td>
<td align="left">80.884</td>
</tr>
<tr class="even">
<td align="left">   Missing</td>
<td align="left">47</td>
<td align="left">19</td>
<td align="left">37</td>
<td align="left">20</td>
<td align="left">1</td>
<td align="left">20</td>
</tr>
<tr class="odd">
<td align="left"><strong>Population</strong></td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
</tr>
<tr class="even">
<td align="left">   mean (sd)</td>
<td align="left">511; 9.76 ± 14.87</td>
<td align="left">244; 23.78 ± 50.16</td>
<td align="left">317; 76.04 ± 207.81</td>
<td align="left">286; 17.56 ± 20.64</td>
<td align="left">21; 8.33 ± 6.59</td>
<td align="left">155; 29.38 ± 76.27</td>
</tr>
<tr class="odd">
<td align="left">   median (Q1, Q3)</td>
<td align="left">511; 4.66 (1.35, 10.87)</td>
<td align="left">244; 6.31 (2.99, 17.96)</td>
<td align="left">317; 14.12 (3.87, 45.60)</td>
<td align="left">286; 8.61 (4.42, 22.37)</td>
<td align="left">21; 3.91 (3.16, 14.07)</td>
<td align="left">155; 7.69 (2.92, 24.61)</td>
</tr>
<tr class="even">
<td align="left">   min</td>
<td align="left">0.060011</td>
<td align="left">0.662850</td>
<td align="left">0.120447</td>
<td align="left">0.165110</td>
<td align="left">1.994794</td>
<td align="left">0.063149</td>
</tr>
<tr class="odd">
<td align="left">   max</td>
<td align="left">119.90127</td>
<td align="left">301.13995</td>
<td align="left">1318.68310</td>
<td align="left">82.40100</td>
<td align="left">20.43418</td>
<td align="left">788.00000</td>
</tr>
<tr class="even">
<td align="left">   Missing</td>
<td align="left">53</td>
<td align="left">29</td>
<td align="left">43</td>
<td align="left">28</td>
<td align="left">1</td>
<td align="left">16</td>
</tr>
<tr class="odd">
<td align="left"><strong>GDP per Capita</strong></td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
<td align="left">  </td>
</tr>
<tr class="even">
<td align="left">   High GDP per Capita</td>
<td align="left">84 (16.70%)</td>
<td align="left">178 (73.86%)</td>
<td align="left">134 (41.23%)</td>
<td align="left">259 (93.17%)</td>
<td align="left">21 (100.00%)</td>
<td align="left">80 (50.96%)</td>
</tr>
<tr class="odd">
<td align="left">   Low GDP per Capita</td>
<td align="left">419 (83.30%)</td>
<td align="left">63 (26.14%)</td>
<td align="left">191 (58.77%)</td>
<td align="left">19 (6.83%)</td>
<td align="left">0 (0.00%)</td>
<td align="left">77 (49.04%)</td>
</tr>
<tr class="even">
<td align="left">   Missing</td>
<td align="left">61</td>
<td align="left">32</td>
<td align="left">35</td>
<td align="left">36</td>
<td align="left">1</td>
<td align="left">14</td>
</tr>
</tbody>
</table>

Again, more functionality and examples can be found in the [vignette](https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html).

### Create Descriptive Summary Statistics Tables in R with Amisc

`Amisc` is a great package for summary statistics tables. Notice however, that this package can only produce tables with groupings. If it has to build a simple summary statistics table, it will fail. Another point worth mentioning is that you can get this package from github. It is currently not on CRAN. Let's jump to the code.

``` r
# devtools::install_github("AlineTalhouk/Amisc")
library(Amisc)
library(pander)
pander::pandoc.table(Amisc::describeBy(
  data = gapminder,
  var.names = c("lifeExp", "pop", "gdpPercap"),
  by1 = "continent",
  dispersion = "sd", Missing = TRUE,
  stats = "non-parametric"
),
split.tables = Inf
)
```

\[1\] "171 missing in the Input Argument continent . "

<table>
<colgroup>
<col width="8%" />
<col width="8%" />
<col width="11%" />
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="11%" />
<col width="12%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Variable</th>
<th align="center">Levels</th>
<th align="center">Africa</th>
<th align="center">Americas</th>
<th align="center">Asia</th>
<th align="center">Europe</th>
<th align="center">Oceania</th>
<th align="center">Total</th>
<th align="center">PValue</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"></td>
<td align="center">N</td>
<td align="center">564</td>
<td align="center">273</td>
<td align="center">360</td>
<td align="center">314</td>
<td align="center">22</td>
<td align="center">1704</td>
<td align="center">Kruskal_Wallis</td>
</tr>
<tr class="even">
<td align="center"><strong>lifeExp</strong></td>
<td align="center">Mean (sd)</td>
<td align="center">48.9( ± 9.3 )</td>
<td align="center">64.7( ± 9.4 )</td>
<td align="center">59.8( ± 11.8 )</td>
<td align="center">71.9( ± 5.6 )</td>
<td align="center">74.7( ± 3.8 )</td>
<td align="center">59.5( ± 12.9 )</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center"></td>
<td align="center">Median (IQR)</td>
<td align="center">47.8( ± 12 )</td>
<td align="center">67.1( ± 13.2 )</td>
<td align="center">61.2( ± 18.3 )</td>
<td align="center">72.2( ± 5.9 )</td>
<td align="center">73.8( ± 6 )</td>
<td align="center">60.7( ± 22.5 )</td>
<td align="center"></td>
</tr>
<tr class="even">
<td align="center"></td>
<td align="center">Missing</td>
<td align="center">47</td>
<td align="center">19</td>
<td align="center">37</td>
<td align="center">20</td>
<td align="center">1</td>
<td align="center">144</td>
<td align="center"></td>
</tr>
<tr class="odd">
<td align="center"><strong>pop</strong></td>
<td align="center">Mean (sd)</td>
<td align="center">9.8( ± 14.9 )</td>
<td align="center">23.8( ± 50.2 )</td>
<td align="center">76( ± 207.8 )</td>
<td align="center">17.6( ± 20.6 )</td>
<td align="center">8.3( ± 6.6 )</td>
<td align="center">29.1( ± 103.2 )</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center"></td>
<td align="center">Median (IQR)</td>
<td align="center">4.7( ± 9.5 )</td>
<td align="center">6.3( ± 15 )</td>
<td align="center">14.1( ± 41.7 )</td>
<td align="center">8.6( ± 18 )</td>
<td align="center">3.9( ± 10.9 )</td>
<td align="center">7.1( ± 17 )</td>
<td align="center"></td>
</tr>
<tr class="odd">
<td align="center"></td>
<td align="center">Missing</td>
<td align="center">53</td>
<td align="center">29</td>
<td align="center">43</td>
<td align="center">28</td>
<td align="center">1</td>
<td align="center">170</td>
<td align="center"></td>
</tr>
<tr class="even">
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center"></td>
<td align="center">PearsonChi_square</td>
</tr>
<tr class="odd">
<td align="center"><strong>gdpPercap</strong></td>
<td align="center">high</td>
<td align="center">84(16.7%)</td>
<td align="center">178(73.9%)</td>
<td align="center">134(41.2%)</td>
<td align="center">259(93.2%)</td>
<td align="center">21(100%)</td>
<td align="center">676(49.4%)</td>
<td align="center">0.000</td>
</tr>
<tr class="even">
<td align="center"></td>
<td align="center">low</td>
<td align="center">419(83.3%)</td>
<td align="center">63(26.1%)</td>
<td align="center">191(58.8%)</td>
<td align="center">19(6.8%)</td>
<td align="center">0(0%)</td>
<td align="center">692(50.6%)</td>
<td align="center"></td>
</tr>
<tr class="odd">
<td align="center"></td>
<td align="center">Missing</td>
<td align="center">61</td>
<td align="center">32</td>
<td align="center">35</td>
<td align="center">36</td>
<td align="center">1</td>
<td align="center">179</td>
<td align="center"></td>
</tr>
</tbody>
</table>

The table is very simple but informative. It shows, mean, median and the interquartile range, and the missing values as counts and not percentages. The package uses the `pandoc.table()` function from the pander package to display a nicely looking table. Overall, I really like the simplicity of the table. Unfortunately, there is not much documentation about this package.

### Create Descriptive Summary Statistics Tables in R with table1

The next summary statistics package which creates a beautiful table is table1. In the code below, we are first relabelling our columns for aesthetics. Then we are creating the table with only one line of code. We again created a table by groupings.

``` r
table1::label(gapminder$lifeExp) <- "Life Expectancy"
table1::label(gapminder$pop) <- "Population"
table1::label(gapminder$gdpPercap) <- "Gdp Per Capita"


table1::table1(~lifeExp + pop + gdpPercap | continent, data = gapminder)
```

<table class="Rtable1">
<thead>
<tr>
<th class="rowlabel firstrow lastrow">
</th>
<th class="firstrow lastrow">
<span class="stratlabel">Africa<br><span class="stratn">(n=564)</span></span>
</th>
<th class="firstrow lastrow">
<span class="stratlabel">Americas<br><span class="stratn">(n=273)</span></span>
</th>
<th class="firstrow lastrow">
<span class="stratlabel">Asia<br><span class="stratn">(n=360)</span></span>
</th>
<th class="firstrow lastrow">
<span class="stratlabel">Europe<br><span class="stratn">(n=314)</span></span>
</th>
<th class="firstrow lastrow">
<span class="stratlabel">Oceania<br><span class="stratn">(n=22)</span></span>
</th>
<th class="firstrow lastrow">
<span class="stratlabel">Overall<br><span class="stratn">(n=1704)</span></span>
</th>
</tr>
</thead>
<tbody>
<tr>
<td class="rowlabel firstrow">
<span class="varlabel">Life Expectancy</span>
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
</tr>
<tr>
<td class="rowlabel">
Mean (SD)
</td>
<td>
48.9 (9.25)
</td>
<td>
64.7 (9.36)
</td>
<td>
59.8 (11.8)
</td>
<td>
71.9 (5.56)
</td>
<td>
74.7 (3.80)
</td>
<td>
59.5 (12.9)
</td>
</tr>
<tr>
<td class="rowlabel">
Median \[Min, Max\]
</td>
<td>
47.8 \[23.6, 76.4\]
</td>
<td>
67.1 \[37.6, 80.7\]
</td>
<td>
61.2 \[30.3, 82.6\]
</td>
<td>
72.2 \[43.6, 81.8\]
</td>
<td>
73.8 \[69.4, 81.2\]
</td>
<td>
60.7 \[23.6, 82.6\]
</td>
</tr>
<tr>
<td class="rowlabel lastrow">
Missing
</td>
<td class="lastrow">
47 (8.3%)
</td>
<td class="lastrow">
19 (7.0%)
</td>
<td class="lastrow">
37 (10.3%)
</td>
<td class="lastrow">
20 (6.4%)
</td>
<td class="lastrow">
1 (4.5%)
</td>
<td class="lastrow">
144 (8.5%)
</td>
</tr>
<tr>
<td class="rowlabel firstrow">
<span class="varlabel">Population</span>
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
</tr>
<tr>
<td class="rowlabel">
Mean (SD)
</td>
<td>
9.76 (14.9)
</td>
<td>
23.8 (50.2)
</td>
<td>
76.0 (208)
</td>
<td>
17.6 (20.6)
</td>
<td>
8.33 (6.59)
</td>
<td>
29.1 (103)
</td>
</tr>
<tr>
<td class="rowlabel">
Median \[Min, Max\]
</td>
<td>
4.66 \[0.0600, 120\]
</td>
<td>
6.31 \[0.663, 301\]
</td>
<td>
14.1 \[0.120, 1320\]
</td>
<td>
8.61 \[0.165, 82.4\]
</td>
<td>
3.91 \[1.99, 20.4\]
</td>
<td>
7.11 \[0.0600, 1320\]
</td>
</tr>
<tr>
<td class="rowlabel lastrow">
Missing
</td>
<td class="lastrow">
53 (9.4%)
</td>
<td class="lastrow">
29 (10.6%)
</td>
<td class="lastrow">
43 (11.9%)
</td>
<td class="lastrow">
28 (8.9%)
</td>
<td class="lastrow">
1 (4.5%)
</td>
<td class="lastrow">
170 (10.0%)
</td>
</tr>
<tr>
<td class="rowlabel firstrow">
<span class="varlabel">Gdp Per Capita</span>
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
<td class="firstrow">
</td>
</tr>
<tr>
<td class="rowlabel">
high
</td>
<td>
84 (14.9%)
</td>
<td>
178 (65.2%)
</td>
<td>
134 (37.2%)
</td>
<td>
259 (82.5%)
</td>
<td>
21 (95.5%)
</td>
<td>
756 (44.4%)
</td>
</tr>
<tr>
<td class="rowlabel">
low
</td>
<td>
419 (74.3%)
</td>
<td>
63 (23.1%)
</td>
<td>
191 (53.1%)
</td>
<td>
19 (6.1%)
</td>
<td>
0 (0.0%)
</td>
<td>
769 (45.1%)
</td>
</tr>
<tr>
<td class="rowlabel lastrow">
Missing
</td>
<td class="lastrow">
61 (10.8%)
</td>
<td class="lastrow">
32 (11.7%)
</td>
<td class="lastrow">
35 (9.7%)
</td>
<td class="lastrow">
36 (11.5%)
</td>
<td class="lastrow">
1 (4.5%)
</td>
<td class="lastrow">
179 (10.5%)
</td>
</tr>
</tbody>
</table>
``` r
table1::table1(~lifeExp + pop + gdpPercap, data = gapminder)
```

<table class="Rtable1">
<thead>
<tr>
<th class="rowlabel firstrow lastrow">
</th>
<th class="firstrow lastrow">
<span class="stratlabel">Overall<br><span class="stratn">(n=1704)</span></span>
</th>
</tr>
</thead>
<tbody>
<tr>
<td class="rowlabel firstrow">
<span class="varlabel">Life Expectancy</span>
</td>
<td class="firstrow">
</td>
</tr>
<tr>
<td class="rowlabel">
Mean (SD)
</td>
<td>
59.5 (12.9)
</td>
</tr>
<tr>
<td class="rowlabel">
Median \[Min, Max\]
</td>
<td>
60.7 \[23.6, 82.6\]
</td>
</tr>
<tr>
<td class="rowlabel lastrow">
Missing
</td>
<td class="lastrow">
144 (8.5%)
</td>
</tr>
<tr>
<td class="rowlabel firstrow">
<span class="varlabel">Population</span>
</td>
<td class="firstrow">
</td>
</tr>
<tr>
<td class="rowlabel">
Mean (SD)
</td>
<td>
29.1 (103)
</td>
</tr>
<tr>
<td class="rowlabel">
Median \[Min, Max\]
</td>
<td>
7.11 \[0.0600, 1320\]
</td>
</tr>
<tr>
<td class="rowlabel lastrow">
Missing
</td>
<td class="lastrow">
170 (10.0%)
</td>
</tr>
<tr>
<td class="rowlabel firstrow">
<span class="varlabel">Gdp Per Capita</span>
</td>
<td class="firstrow">
</td>
</tr>
<tr>
<td class="rowlabel">
high
</td>
<td>
756 (44.4%)
</td>
</tr>
<tr>
<td class="rowlabel">
low
</td>
<td>
769 (45.1%)
</td>
</tr>
<tr>
<td class="rowlabel lastrow">
Missing
</td>
<td class="lastrow">
179 (10.5%)
</td>
</tr>
</tbody>
</table>
Here, the missing values are displayed as percentages. I prefer to have the missing values displayed only as counts. More often than not, I am interested in the percentage of the factor variables without the NA values included when calculating the percentage. This package unfortunately has only the option to show the missing values as percentages. So essentially it acts as a third factor with high and low together in the gdpPercap column. If you do not mind having the missing values displayed like that then this package is for you.

In the code below, we are showing how to create a table without stratification by any group.

``` r
table1::table1(~lifeExp + pop + gdpPercap, data = gapminder)
```

<table class="Rtable1">
<thead>
<tr>
<th class="rowlabel firstrow lastrow">
</th>
<th class="firstrow lastrow">
<span class="stratlabel">Overall<br><span class="stratn">(n=1704)</span></span>
</th>
</tr>
</thead>
<tbody>
<tr>
<td class="rowlabel firstrow">
<span class="varlabel">Life Expectancy</span>
</td>
<td class="firstrow">
</td>
</tr>
<tr>
<td class="rowlabel">
Mean (SD)
</td>
<td>
59.5 (12.9)
</td>
</tr>
<tr>
<td class="rowlabel">
Median \[Min, Max\]
</td>
<td>
60.7 \[23.6, 82.6\]
</td>
</tr>
<tr>
<td class="rowlabel lastrow">
Missing
</td>
<td class="lastrow">
144 (8.5%)
</td>
</tr>
<tr>
<td class="rowlabel firstrow">
<span class="varlabel">Population</span>
</td>
<td class="firstrow">
</td>
</tr>
<tr>
<td class="rowlabel">
Mean (SD)
</td>
<td>
29.1 (103)
</td>
</tr>
<tr>
<td class="rowlabel">
Median \[Min, Max\]
</td>
<td>
7.11 \[0.0600, 1320\]
</td>
</tr>
<tr>
<td class="rowlabel lastrow">
Missing
</td>
<td class="lastrow">
170 (10.0%)
</td>
</tr>
<tr>
<td class="rowlabel firstrow">
<span class="varlabel">Gdp Per Capita</span>
</td>
<td class="firstrow">
</td>
</tr>
<tr>
<td class="rowlabel">
high
</td>
<td>
756 (44.4%)
</td>
</tr>
<tr>
<td class="rowlabel">
low
</td>
<td>
769 (45.1%)
</td>
</tr>
<tr>
<td class="rowlabel lastrow">
Missing
</td>
<td class="lastrow">
179 (10.5%)
</td>
</tr>
</tbody>
</table>
Again, many more things are possible with this package. For example, you can create subgroupings. In addition to that it is also possible to put p-values as a separate column at the end of the table. If you are interested, check out the [vignette](https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html).

Now let's switch the data set. It is becoming a bit boring to see the same data again and again. For the remaining tables, we are using the mtcars data set. Again, a bit modified and with the introduction of missing values.

``` r
library(tangram)
library(purrrlyr)
library(dplyr)
```

``` r
data(mtcars)
mtcars %>%
  mutate(cylinder = factor(cyl), transmission = factor(am), weight = wt, milesPergallon = mpg) %>%
  select(cylinder, transmission, weight, milesPergallon) -> mtcars
mtcars$cylinder <- recode(mtcars$cylinder, `4` = "4 cylinders", `6` = "6 cylinders", `8` = "8 cylinders")

mtcars <- lapply(mtcars, function(x) x[sample(c(TRUE, NA),
    prob = c(0.8, 0.2),
    size = length(x),
    replace = TRUE
  )])
mtcars <- as.data.frame(mtcars)
```

### Create Descriptive Summary Statistics Tables in R with tangram

I really really like the next package. The design is very beautiful and the code is also very short. The only drawback of this package is that it only knits to html. You can't compile it to word :(. Another (tiny) drawback is that this table does not show the missing values by default. However, the package includes a function called `insert_row()`, where you can insert missing values or any other values (confidence interval for the mean etc.) that you have calculated.

``` r
tan <- tangram::tangram("cylinder ~ transmission + weight + milesPergallon",
  data = mtcars,
  msd = TRUE,
  quant = seq(0, 1, 0.25)
)
html5(tan, fragment = TRUE, inline = "hmisc.css", caption = "Summary Statistics of Gapminder Data Set", id = "tbl2")
```

<!--html_preserve-->
<script type="text/javascript">/*!
 * clipboard.js v1.5.10
 * https://zenorocha.github.io/clipboard.js
 *
 * Licensed MIT Â© Zeno Rocha
 */
!function(t){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=t();else if("function"==typeof define&&define.amd)define([],t);else{var e;e="undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this,e.Clipboard=t()}}(function(){var t,e,n;return function t(e,n,o){function i(c,a){if(!n[c]){if(!e[c]){var s="function"==typeof require&&require;if(!a&&s)return s(c,!0);if(r)return r(c,!0);var l=new Error("Cannot find module '"+c+"'");throw l.code="MODULE_NOT_FOUND",l}var u=n[c]={exports:{}};e[c][0].call(u.exports,function(t){var n=e[c][1][t];return i(n?n:t)},u,u.exports,t,e,n,o)}return n[c].exports}for(var r="function"==typeof require&&require,c=0;c<o.length;c++)i(o[c]);return i}({1:[function(t,e,n){var o=t("matches-selector");e.exports=function(t,e,n){for(var i=n?t:t.parentNode;i&&i!==document;){if(o(i,e))return i;i=i.parentNode}}},{"matches-selector":5}],2:[function(t,e,n){function o(t,e,n,o,r){var c=i.apply(this,arguments);return t.addEventListener(n,c,r),{destroy:function(){t.removeEventListener(n,c,r)}}}function i(t,e,n,o){return function(n){n.delegateTarget=r(n.target,e,!0),n.delegateTarget&&o.call(t,n)}}var r=t("closest");e.exports=o},{closest:1}],3:[function(t,e,n){n.node=function(t){return void 0!==t&&t instanceof HTMLElement&&1===t.nodeType},n.nodeList=function(t){var e=Object.prototype.toString.call(t);return void 0!==t&&("[object NodeList]"===e||"[object HTMLCollection]"===e)&&"length"in t&&(0===t.length||n.node(t[0]))},n.string=function(t){return"string"==typeof t||t instanceof String},n.fn=function(t){var e=Object.prototype.toString.call(t);return"[object Function]"===e}},{}],4:[function(t,e,n){function o(t,e,n){if(!t&&!e&&!n)throw new Error("Missing required arguments");if(!a.string(e))throw new TypeError("Second argument must be a String");if(!a.fn(n))throw new TypeError("Third argument must be a Function");if(a.node(t))return i(t,e,n);if(a.nodeList(t))return r(t,e,n);if(a.string(t))return c(t,e,n);throw new TypeError("First argument must be a String, HTMLElement, HTMLCollection, or NodeList")}function i(t,e,n){return t.addEventListener(e,n),{destroy:function(){t.removeEventListener(e,n)}}}function r(t,e,n){return Array.prototype.forEach.call(t,function(t){t.addEventListener(e,n)}),{destroy:function(){Array.prototype.forEach.call(t,function(t){t.removeEventListener(e,n)})}}}function c(t,e,n){return s(document.body,t,e,n)}var a=t("./is"),s=t("delegate");e.exports=o},{"./is":3,delegate:2}],5:[function(t,e,n){function o(t,e){if(r)return r.call(t,e);for(var n=t.parentNode.querySelectorAll(e),o=0;o<n.length;++o)if(n[o]==t)return!0;return!1}var i=Element.prototype,r=i.matchesSelector||i.webkitMatchesSelector||i.mozMatchesSelector||i.msMatchesSelector||i.oMatchesSelector;e.exports=o},{}],6:[function(t,e,n){function o(t){var e;if("INPUT"===t.nodeName||"TEXTAREA"===t.nodeName)t.focus(),t.setSelectionRange(0,t.value.length),e=t.value;else{t.hasAttribute("contenteditable")&&t.focus();var n=window.getSelection(),o=document.createRange();o.selectNodeContents(t),n.removeAllRanges(),n.addRange(o),e=n.toString()}return e}e.exports=o},{}],7:[function(t,e,n){function o(){}o.prototype={on:function(t,e,n){var o=this.e||(this.e={});return(o[t]||(o[t]=[])).push({fn:e,ctx:n}),this},once:function(t,e,n){function o(){i.off(t,o),e.apply(n,arguments)}var i=this;return o._=e,this.on(t,o,n)},emit:function(t){var e=[].slice.call(arguments,1),n=((this.e||(this.e={}))[t]||[]).slice(),o=0,i=n.length;for(o;i>o;o++)n[o].fn.apply(n[o].ctx,e);return this},off:function(t,e){var n=this.e||(this.e={}),o=n[t],i=[];if(o&&e)for(var r=0,c=o.length;c>r;r++)o[r].fn!==e&&o[r].fn._!==e&&i.push(o[r]);return i.length?n[t]=i:delete n[t],this}},e.exports=o},{}],8:[function(e,n,o){!function(i,r){if("function"==typeof t&&t.amd)t(["module","select"],r);else if("undefined"!=typeof o)r(n,e("select"));else{var c={exports:{}};r(c,i.select),i.clipboardAction=c.exports}}(this,function(t,e){"use strict";function n(t){return t&&t.__esModule?t:{"default":t}}function o(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}var i=n(e),r="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol?"symbol":typeof t},c=function(){function t(t,e){for(var n=0;n<e.length;n++){var o=e[n];o.enumerable=o.enumerable||!1,o.configurable=!0,"value"in o&&(o.writable=!0),Object.defineProperty(t,o.key,o)}}return function(e,n,o){return n&&t(e.prototype,n),o&&t(e,o),e}}(),a=function(){function t(e){o(this,t),this.resolveOptions(e),this.initSelection()}return t.prototype.resolveOptions=function t(){var e=arguments.length<=0||void 0===arguments[0]?{}:arguments[0];this.action=e.action,this.emitter=e.emitter,this.target=e.target,this.text=e.text,this.trigger=e.trigger,this.selectedText=""},t.prototype.initSelection=function t(){this.text?this.selectFake():this.target&&this.selectTarget()},t.prototype.selectFake=function t(){var e=this,n="rtl"==document.documentElement.getAttribute("dir");this.removeFake(),this.fakeHandler=document.body.addEventListener("click",function(){return e.removeFake()}),this.fakeElem=document.createElement("textarea"),this.fakeElem.style.fontSize="12pt",this.fakeElem.style.border="0",this.fakeElem.style.padding="0",this.fakeElem.style.margin="0",this.fakeElem.style.position="fixed",this.fakeElem.style[n?"right":"left"]="-9999px",this.fakeElem.style.top=(window.pageYOffset||document.documentElement.scrollTop)+"px",this.fakeElem.setAttribute("readonly",""),this.fakeElem.value=this.text,document.body.appendChild(this.fakeElem),this.selectedText=(0,i.default)(this.fakeElem),this.copyText()},t.prototype.removeFake=function t(){this.fakeHandler&&(document.body.removeEventListener("click"),this.fakeHandler=null),this.fakeElem&&(document.body.removeChild(this.fakeElem),this.fakeElem=null)},t.prototype.selectTarget=function t(){this.selectedText=(0,i.default)(this.target),this.copyText()},t.prototype.copyText=function t(){var e=void 0;try{e=document.execCommand(this.action)}catch(n){e=!1}this.handleResult(e)},t.prototype.handleResult=function t(e){e?this.emitter.emit("success",{action:this.action,text:this.selectedText,trigger:this.trigger,clearSelection:this.clearSelection.bind(this)}):this.emitter.emit("error",{action:this.action,trigger:this.trigger,clearSelection:this.clearSelection.bind(this)})},t.prototype.clearSelection=function t(){this.target&&this.target.blur(),window.getSelection().removeAllRanges()},t.prototype.destroy=function t(){this.removeFake()},c(t,[{key:"action",set:function t(){var e=arguments.length<=0||void 0===arguments[0]?"copy":arguments[0];if(this._action=e,"copy"!==this._action&&"cut"!==this._action)throw new Error('Invalid "action" value, use either "copy" or "cut"')},get:function t(){return this._action}},{key:"target",set:function t(e){if(void 0!==e){if(!e||"object"!==("undefined"==typeof e?"undefined":r(e))||1!==e.nodeType)throw new Error('Invalid "target" value, use a valid Element');if("copy"===this.action&&e.hasAttribute("disabled"))throw new Error('Invalid "target" attribute. Please use "readonly" instead of "disabled" attribute');if("cut"===this.action&&(e.hasAttribute("readonly")||e.hasAttribute("disabled")))throw new Error('Invalid "target" attribute. You can\'t cut text from elements with "readonly" or "disabled" attributes');this._target=e}},get:function t(){return this._target}}]),t}();t.exports=a})},{select:6}],9:[function(e,n,o){!function(i,r){if("function"==typeof t&&t.amd)t(["module","./clipboard-action","tiny-emitter","good-listener"],r);else if("undefined"!=typeof o)r(n,e("./clipboard-action"),e("tiny-emitter"),e("good-listener"));else{var c={exports:{}};r(c,i.clipboardAction,i.tinyEmitter,i.goodListener),i.clipboard=c.exports}}(this,function(t,e,n,o){"use strict";function i(t){return t&&t.__esModule?t:{"default":t}}function r(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}function c(t,e){if(!t)throw new ReferenceError("this hasn't been initialised - super() hasn't been called");return!e||"object"!=typeof e&&"function"!=typeof e?t:e}function a(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function, not "+typeof e);t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,enumerable:!1,writable:!0,configurable:!0}}),e&&(Object.setPrototypeOf?Object.setPrototypeOf(t,e):t.__proto__=e)}function s(t,e){var n="data-clipboard-"+t;if(e.hasAttribute(n))return e.getAttribute(n)}var l=i(e),u=i(n),f=i(o),d=function(t){function e(n,o){r(this,e);var i=c(this,t.call(this));return i.resolveOptions(o),i.listenClick(n),i}return a(e,t),e.prototype.resolveOptions=function t(){var e=arguments.length<=0||void 0===arguments[0]?{}:arguments[0];this.action="function"==typeof e.action?e.action:this.defaultAction,this.target="function"==typeof e.target?e.target:this.defaultTarget,this.text="function"==typeof e.text?e.text:this.defaultText},e.prototype.listenClick=function t(e){var n=this;this.listener=(0,f.default)(e,"click",function(t){return n.onClick(t)})},e.prototype.onClick=function t(e){var n=e.delegateTarget||e.currentTarget;this.clipboardAction&&(this.clipboardAction=null),this.clipboardAction=new l.default({action:this.action(n),target:this.target(n),text:this.text(n),trigger:n,emitter:this})},e.prototype.defaultAction=function t(e){return s("action",e)},e.prototype.defaultTarget=function t(e){var n=s("target",e);return n?document.querySelector(n):void 0},e.prototype.defaultText=function t(e){return s("text",e)},e.prototype.destroy=function t(){this.listener.destroy(),this.clipboardAction&&(this.clipboardAction.destroy(),this.clipboardAction=null)},e}(u.default);t.exports=d})},{"./clipboard-action":8,"good-listener":4,"tiny-emitter":7}]},{},[9])(9)});
</script>
<style>

#tbl2 .figure  {  font-family: "Computer Modern Serif", serif;
            margin-left:  auto;
                  margin-right: auto;
                  font-size: 66%;
  -webkit-print-color-adjust: exact;
         }

#tbl2 .caption { text-align:   center;
             margin-left:  auto;
             margin-right: auto;
             padding: 1.5em 0 1.5em 0;
           }

#tbl2 .figbody {
               text-align:   center;
             margin-left:  auto;
             margin-right: auto;
             border-style: double none solid none;
             border-width: 3pt 0 1pt 0;
             width:        100%;
         }

#tbl2 .figbody td {
                      padding: 0.2em 0.5em 0.2em 0.5em;
            }

#tbl2 .data:hover {
              background-color:yellow;
            }

#tbl2 .tangram {
    border-spacing: 0;
}

#tbl2 .subheader    { font-size: smaller; }

#tbl2 .subheader tr { border-collapse: collapse; }
#tbl2 .subheader td { border-bottom: 1pt solid black; }

#tbl2 .quantile .q25 {
    font-size: smaller;
    padding-right: 0.5em;
}

#tbl2 .quantile .q75 {
    font-size: smaller;
    padding-left: 0.5em;
}

#tbl2 .quantile .q50 {
  font-weight: bold;
}

#tbl2 .variable {
    float:      left;
    text-align: left;
}

#tbl2 .units {
    float:      right;
    font-size:  x-small;
    text-align: right;
    padding-left: 1em;
    vertical-align: text-bottom; /* FIXME why doesn't this work */
}

#tbl2 thead .header {font-weight: bold;}
#tbl2 .header.N  { font-style: italic; }
#tbl2 .header.N:before { content: "N="; }


#tbl2 .fraction .percentage {display: none;}
#tbl2 .fraction .numerator:before {content: "\00A0";}
#tbl2 .fraction .numerator { font-size: x-small; vertical-align: super; }
#tbl2 .fraction .numerator:after {content: " \2044";}
#tbl2 .fraction .denominator { font-size: x-small; vertical-align: sub;}
#tbl2 .statistics .description {font-style: italic;}
#tbl2 .statistic {padding-right: 0.5em;}

#tbl2 td .align{
    display: inline-block;
    margin: 0 auto;
}

#tbl2 .nobr {
   white-space: nowrap;
}

#tbl2 .supsub {
   display: inline-block;
   margin: -9em 0;
   vertical-align: -0.55em;
   line-height: 1.35em;
   font-size: x-small;
   text-align: left;
}

#tbl2 .sup:before {content: "\00A0";}
#tbl2 .sup { font-size: x-small; vertical-align: super; }
#tbl2 .sup:after {content: "\2044"; }
#tbl2 .sub { font-size: x-small; vertical-align: sub;}


</style>
Summary Statistics of Gapminder Data Set

<table class="tangram">
<thead>
<tr>
<td class="header even tg-label">
<span class="variable"></span>
</td>
<td class="header even tg-label">
<span class="variable">N</span>
</td>
<td class="header even tg-label">
<span class="variable">4 cylinders</span>
</td>
<td class="header even tg-label">
<span class="variable">6 cylinders</span>
</td>
<td class="header even tg-label">
<span class="variable">8 cylinders</span>
</td>
<td class="header even tg-label">
<span class="variable">Test Statistic</span>
</td>
</tr>
<tr class="subheaderrow">
<td class="subheader header even tg-label">
<span class="variable"></span>
</td>
<td class="subheader header even tg-label">
<span class="variable"></span>
</td>
<td class="subheader header even data N" data-clipboard-text="{list(index = " NDFh", src="tangram:transmission:cylinder[4 cylinders]:N" , value="8" ) N="NULL}&quot;">
<span class="N">8</span>
</td>
<td class="subheader header even data N" data-clipboard-text="{list(index = " NDE4", src="tangram:transmission:cylinder[6 cylinders]:N" , value="6" ) N="NULL}&quot;">
<span class="N">6</span>
</td>
<td class="subheader header even data N" data-clipboard-text="{list(index = " ZTE1", src="tangram:transmission:cylinder[8 cylinders]:N" , value="12" ) N="NULL}&quot;">
<span class="N">12</span>
</td>
<td class="subheader header even tg-label">
<span class="variable"></span>
</td>
</tr>
</thead>
<tbody>
<tr>
<td class="header odd tg-label">
<span class="variable">transmission : 1</span>
</td>
<td class="odd">
26
</td>
<td class="odd">
<span class="odd fraction" data-clipboard-text="{list(index = "NGM2", src = "tbl2:transmission[transmission : 1]:cylinder[4 cylinders]:numerator", value = "6") list(index = "ZTQw", src = "tbl2:transmission[transmission : 1]:cylinder[4 cylinders]:ratio", value = "0.857")}"><span class="ratio">0
.

857</span><span class="percentage">85.714</span><span class="numerator">6</span><span class="denominator">7</span></span>
</td>
<td class="odd">
<span class="odd fraction" data-clipboard-text="{list(index = "MTZm", src = "tbl2:transmission[NA]:cylinder[4 cylinders]:numerator", value = "3") list(index = "Yzk5", src = "tbl2:transmission[NA]:cylinder[4 cylinders]:ratio", value = "0.600")}"><span class="ratio">0
.

600</span><span class="percentage">60.000</span><span class="numerator">3</span><span class="denominator">5</span></span>
</td>
<td class="odd">
<span class="odd fraction" data-clipboard-text="{list(index = "MTZm", src = "tbl2:transmission[NA]:cylinder[4 cylinders]:numerator", value = "2") list(index = "Yzk5", src = "tbl2:transmission[NA]:cylinder[4 cylinders]:ratio", value = "0.182")}"><span class="ratio">0
.

182</span><span class="percentage">18.182</span><span class="numerator"> 2</span><span class="denominator">11</span></span>
</td>
<td class="odd data statistics" data-clipboard-text="{list(index = " Njhi", src="tbl2:transmission:cylinder:&lt;U+03C7&gt;">
^{2}", value = "8.20") list(index = "ODBk", src = "tbl2:transmission:cylinder:P", value = "0.017")}"&gt;<span class="statistic"><span class="description"><span class="nobr">χ<span class="supsub">2<br/>2</span></span> = </span>8.20,</span><span class="pvalue"><span class="description">P = </span>0.017</span><sup>2</sup>
</td>
</tr>
<tr>
<td class="header even tg-label">
<span class="variable">weight</span>
</td>
<td class="even data N" data-clipboard-text="{list(index = " OTkw", src="tbl2:weight:cylinder:cell_n1" , value="24" ) N="NULL}&quot;">
<span class="N">24</span>
</td>
<td class="even">
<span class="even data quantile"><span class="q25">1.83</span><span class="q25">1.90</span><span class="q50">2.14</span><span class="q75">2.90</span><span class="q75">3.15</span><br/><span>2.37±0.57</span>
</td>
<td class="even">
<span class="even data quantile"><span class="q25">2.77</span><span class="q25">2.84</span><span class="q50">3.21</span><span class="q75">3.44</span><span class="q75">3.44</span><br/><span>3.15±0.31</span>
</td>
<td class="even">
<span class="even data quantile"><span class="q25">3.17</span><span class="q25">3.51</span><span class="q50">3.67</span><span class="q75">3.86</span><span class="q75">5.34</span><br/><span>3.81±0.59</span>
</td>
<td class="even data statistics" data-clipboard-text="{list(index = " OThh", src="tbl2:weight:cylinder:F" , value="19.52" ) list(index="M2Nm" , src="tbl2:weight:cylinder:df2" , value="17" )}">
<span class="statistic"><span class="description">F<sub>2,17</sub> = </span>19.52,</span><span class="pvalue"><span class="description">P = </span>&lt;0.001<sup>1</sup></span>
</td>
</tr>
<tr>
<td class="header odd tg-label">
<span class="variable">milesPergallon</span>
</td>
<td class="odd data N" data-clipboard-text="{list(index = " NDRh", src="tbl2:milesPergallon:cylinder:cell_n1" , value="24" ) N="NULL}&quot;">
<span class="N">24</span>
</td>
<td class="odd">
<span class="odd data quantile"><span class="q25">22.8</span><span class="q25">25.8</span><span class="q50">30.4</span><span class="q75">31.6</span><span class="q75">33.9</span><br/><span>29.0±4.2</span>
</td>
<td class="odd">
<span class="odd data quantile"><span class="q25">17.8</span><span class="q25">18.7</span><span class="q50">19.7</span><span class="q75">21.1</span><span class="q75">21.4</span><br/><span>19.8±1.4</span>
</td>
<td class="odd">
<span class="odd data quantile"><span class="q25">10.4</span><span class="q25">14.2</span><span class="q50">15.2</span><span class="q75">16.0</span><span class="q75">19.2</span><br/><span>15.1±2.4</span>
</td>
<td class="odd data statistics" data-clipboard-text="{list(index = " MzA0", src="tbl2:milesPergallon:cylinder:F" , value="37.84" ) list(index="YTY0" , src="tbl2:milesPergallon:cylinder:df2" , value="16" )}">
<span class="statistic"><span class="description">F<sub>2,16</sub> = </span>37.84,</span><span class="pvalue"><span class="description">P = </span>&lt;0.001<sup>1</sup></span>
</td>
</tr>
</tbody>
</table>

N is the number of non-missing value. <sup>1</sup>Kruskal-Wallis. <sup>2</sup>Pearson. <sup>3</sup>Wilcoxon.

<script>new Clipboard('.data');</script>
<!--/html_preserve-->
In the next code block, I am showing you how to insert missing values. For the first three lines, I am using the purrrlyr package. This package is a combination of the dplyr and purrr packages. So what I am doing is separating the levels of the column I want to group by. In this case cylinders. After that, I am calculating the missing values of each cylinder group (4, 6 and 8) for every column.

Then we are removing the last column of our tibble which contains the missing values for cylinders. Then we are calculating the total missing cylinder values for each column. After that, we are doing an rbind and them and removing the column names.

``` r
mtcars %>%
  slice_rows("cylinder") %>%
  dmap(~sum(is.na(.))) -> by_cyl


by_cyl <- select(by_cyl[-4, ], transmission, weight, milesPergallon) # make sure variables are in the same order they appear in the tangram() function above
column_sums <- colSums(by_cyl)
by_cyl <- rbind(column_sums, by_cyl)
names(by_cyl) <- NULL

### This is how the insert row function works ###
# tan <- insert_row(tan, 3, "Missing", by_cyl[1, 1], by_cyl[1, 2], by_cyl[1, 3])
# tan <- insert_row(tan, 5, "Missing", by_cyl[2, 1], by_cyl[2, 2], by_cyl[2, 3])
# tan <- insert_row(tan, 7, "Missing", by_cyl[3, 1], by_cyl[3, 2], by_cyl[3, 3])
```

The out commented section is how the `insert_row()` function works. The first argument is the tan object that we have create in the above code block. The next argument is the number where you want to insert a row. Then we specify how we want to name the row. In our case we are naming it "Missing". The next four arguments represent the values that we want to insert in the row. First, the total missing values for the corresponding column. Then the missing values for the corresponding column by cylinder group.

We do not have to necessarily insert the missing values. We can insert any number we want. For example, a trimmed mean.

If you have a lot of rows to insert, this method becomes tedious and you have to write a lot of code. To make your lives easier, I created a generic function which will take care of almost everything. The only part that needs specification is the part where we specify at what position the row should be inserted in the table. You can specify the positions in the row\_number vector. The `argument_number` object specifies how many arguments the `insert_function()` takes. The first three arguments are reserved for the table (tan), the row number, and the row label. Then you have an argument for the total number of missing values. After that the number of arguments in the `insert_row()` function depends on how many levels the column has you want to group by. The code below shows the generic function.

``` r
split_by <- mtcars$cylinder
row_numbers <- c(3, 5, 7)
argument_number <- nlevels(split_by) + 1 + 3 # plus 1 refers to total column in table
# plus 3 refers to the first 3 args of insert_row

j <- 1
for (c in row_numbers) {
  args <- list(1:argument_number)
  args[[1]] <- tan
  args[[2]] <- c
  args[[3]] <- "Missing"
  for (i in c(4:argument_number)) {
    args[[i]] <- by_cyl[i - 3, j]
  }
  tan <- do.call(tangram::insert_row, args)
  j <- j + 1
}
```

``` r
html5(tan, fragment = TRUE, inline = "hmisc.css", caption = "Summary Statistics of Gapminder Data Set", id = "tbl2")
```

<!--html_preserve-->
<script type="text/javascript">/*!
 * clipboard.js v1.5.10
 * https://zenorocha.github.io/clipboard.js
 *
 * Licensed MIT Â© Zeno Rocha
 */
!function(t){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=t();else if("function"==typeof define&&define.amd)define([],t);else{var e;e="undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this,e.Clipboard=t()}}(function(){var t,e,n;return function t(e,n,o){function i(c,a){if(!n[c]){if(!e[c]){var s="function"==typeof require&&require;if(!a&&s)return s(c,!0);if(r)return r(c,!0);var l=new Error("Cannot find module '"+c+"'");throw l.code="MODULE_NOT_FOUND",l}var u=n[c]={exports:{}};e[c][0].call(u.exports,function(t){var n=e[c][1][t];return i(n?n:t)},u,u.exports,t,e,n,o)}return n[c].exports}for(var r="function"==typeof require&&require,c=0;c<o.length;c++)i(o[c]);return i}({1:[function(t,e,n){var o=t("matches-selector");e.exports=function(t,e,n){for(var i=n?t:t.parentNode;i&&i!==document;){if(o(i,e))return i;i=i.parentNode}}},{"matches-selector":5}],2:[function(t,e,n){function o(t,e,n,o,r){var c=i.apply(this,arguments);return t.addEventListener(n,c,r),{destroy:function(){t.removeEventListener(n,c,r)}}}function i(t,e,n,o){return function(n){n.delegateTarget=r(n.target,e,!0),n.delegateTarget&&o.call(t,n)}}var r=t("closest");e.exports=o},{closest:1}],3:[function(t,e,n){n.node=function(t){return void 0!==t&&t instanceof HTMLElement&&1===t.nodeType},n.nodeList=function(t){var e=Object.prototype.toString.call(t);return void 0!==t&&("[object NodeList]"===e||"[object HTMLCollection]"===e)&&"length"in t&&(0===t.length||n.node(t[0]))},n.string=function(t){return"string"==typeof t||t instanceof String},n.fn=function(t){var e=Object.prototype.toString.call(t);return"[object Function]"===e}},{}],4:[function(t,e,n){function o(t,e,n){if(!t&&!e&&!n)throw new Error("Missing required arguments");if(!a.string(e))throw new TypeError("Second argument must be a String");if(!a.fn(n))throw new TypeError("Third argument must be a Function");if(a.node(t))return i(t,e,n);if(a.nodeList(t))return r(t,e,n);if(a.string(t))return c(t,e,n);throw new TypeError("First argument must be a String, HTMLElement, HTMLCollection, or NodeList")}function i(t,e,n){return t.addEventListener(e,n),{destroy:function(){t.removeEventListener(e,n)}}}function r(t,e,n){return Array.prototype.forEach.call(t,function(t){t.addEventListener(e,n)}),{destroy:function(){Array.prototype.forEach.call(t,function(t){t.removeEventListener(e,n)})}}}function c(t,e,n){return s(document.body,t,e,n)}var a=t("./is"),s=t("delegate");e.exports=o},{"./is":3,delegate:2}],5:[function(t,e,n){function o(t,e){if(r)return r.call(t,e);for(var n=t.parentNode.querySelectorAll(e),o=0;o<n.length;++o)if(n[o]==t)return!0;return!1}var i=Element.prototype,r=i.matchesSelector||i.webkitMatchesSelector||i.mozMatchesSelector||i.msMatchesSelector||i.oMatchesSelector;e.exports=o},{}],6:[function(t,e,n){function o(t){var e;if("INPUT"===t.nodeName||"TEXTAREA"===t.nodeName)t.focus(),t.setSelectionRange(0,t.value.length),e=t.value;else{t.hasAttribute("contenteditable")&&t.focus();var n=window.getSelection(),o=document.createRange();o.selectNodeContents(t),n.removeAllRanges(),n.addRange(o),e=n.toString()}return e}e.exports=o},{}],7:[function(t,e,n){function o(){}o.prototype={on:function(t,e,n){var o=this.e||(this.e={});return(o[t]||(o[t]=[])).push({fn:e,ctx:n}),this},once:function(t,e,n){function o(){i.off(t,o),e.apply(n,arguments)}var i=this;return o._=e,this.on(t,o,n)},emit:function(t){var e=[].slice.call(arguments,1),n=((this.e||(this.e={}))[t]||[]).slice(),o=0,i=n.length;for(o;i>o;o++)n[o].fn.apply(n[o].ctx,e);return this},off:function(t,e){var n=this.e||(this.e={}),o=n[t],i=[];if(o&&e)for(var r=0,c=o.length;c>r;r++)o[r].fn!==e&&o[r].fn._!==e&&i.push(o[r]);return i.length?n[t]=i:delete n[t],this}},e.exports=o},{}],8:[function(e,n,o){!function(i,r){if("function"==typeof t&&t.amd)t(["module","select"],r);else if("undefined"!=typeof o)r(n,e("select"));else{var c={exports:{}};r(c,i.select),i.clipboardAction=c.exports}}(this,function(t,e){"use strict";function n(t){return t&&t.__esModule?t:{"default":t}}function o(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}var i=n(e),r="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol?"symbol":typeof t},c=function(){function t(t,e){for(var n=0;n<e.length;n++){var o=e[n];o.enumerable=o.enumerable||!1,o.configurable=!0,"value"in o&&(o.writable=!0),Object.defineProperty(t,o.key,o)}}return function(e,n,o){return n&&t(e.prototype,n),o&&t(e,o),e}}(),a=function(){function t(e){o(this,t),this.resolveOptions(e),this.initSelection()}return t.prototype.resolveOptions=function t(){var e=arguments.length<=0||void 0===arguments[0]?{}:arguments[0];this.action=e.action,this.emitter=e.emitter,this.target=e.target,this.text=e.text,this.trigger=e.trigger,this.selectedText=""},t.prototype.initSelection=function t(){this.text?this.selectFake():this.target&&this.selectTarget()},t.prototype.selectFake=function t(){var e=this,n="rtl"==document.documentElement.getAttribute("dir");this.removeFake(),this.fakeHandler=document.body.addEventListener("click",function(){return e.removeFake()}),this.fakeElem=document.createElement("textarea"),this.fakeElem.style.fontSize="12pt",this.fakeElem.style.border="0",this.fakeElem.style.padding="0",this.fakeElem.style.margin="0",this.fakeElem.style.position="fixed",this.fakeElem.style[n?"right":"left"]="-9999px",this.fakeElem.style.top=(window.pageYOffset||document.documentElement.scrollTop)+"px",this.fakeElem.setAttribute("readonly",""),this.fakeElem.value=this.text,document.body.appendChild(this.fakeElem),this.selectedText=(0,i.default)(this.fakeElem),this.copyText()},t.prototype.removeFake=function t(){this.fakeHandler&&(document.body.removeEventListener("click"),this.fakeHandler=null),this.fakeElem&&(document.body.removeChild(this.fakeElem),this.fakeElem=null)},t.prototype.selectTarget=function t(){this.selectedText=(0,i.default)(this.target),this.copyText()},t.prototype.copyText=function t(){var e=void 0;try{e=document.execCommand(this.action)}catch(n){e=!1}this.handleResult(e)},t.prototype.handleResult=function t(e){e?this.emitter.emit("success",{action:this.action,text:this.selectedText,trigger:this.trigger,clearSelection:this.clearSelection.bind(this)}):this.emitter.emit("error",{action:this.action,trigger:this.trigger,clearSelection:this.clearSelection.bind(this)})},t.prototype.clearSelection=function t(){this.target&&this.target.blur(),window.getSelection().removeAllRanges()},t.prototype.destroy=function t(){this.removeFake()},c(t,[{key:"action",set:function t(){var e=arguments.length<=0||void 0===arguments[0]?"copy":arguments[0];if(this._action=e,"copy"!==this._action&&"cut"!==this._action)throw new Error('Invalid "action" value, use either "copy" or "cut"')},get:function t(){return this._action}},{key:"target",set:function t(e){if(void 0!==e){if(!e||"object"!==("undefined"==typeof e?"undefined":r(e))||1!==e.nodeType)throw new Error('Invalid "target" value, use a valid Element');if("copy"===this.action&&e.hasAttribute("disabled"))throw new Error('Invalid "target" attribute. Please use "readonly" instead of "disabled" attribute');if("cut"===this.action&&(e.hasAttribute("readonly")||e.hasAttribute("disabled")))throw new Error('Invalid "target" attribute. You can\'t cut text from elements with "readonly" or "disabled" attributes');this._target=e}},get:function t(){return this._target}}]),t}();t.exports=a})},{select:6}],9:[function(e,n,o){!function(i,r){if("function"==typeof t&&t.amd)t(["module","./clipboard-action","tiny-emitter","good-listener"],r);else if("undefined"!=typeof o)r(n,e("./clipboard-action"),e("tiny-emitter"),e("good-listener"));else{var c={exports:{}};r(c,i.clipboardAction,i.tinyEmitter,i.goodListener),i.clipboard=c.exports}}(this,function(t,e,n,o){"use strict";function i(t){return t&&t.__esModule?t:{"default":t}}function r(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}function c(t,e){if(!t)throw new ReferenceError("this hasn't been initialised - super() hasn't been called");return!e||"object"!=typeof e&&"function"!=typeof e?t:e}function a(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function, not "+typeof e);t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,enumerable:!1,writable:!0,configurable:!0}}),e&&(Object.setPrototypeOf?Object.setPrototypeOf(t,e):t.__proto__=e)}function s(t,e){var n="data-clipboard-"+t;if(e.hasAttribute(n))return e.getAttribute(n)}var l=i(e),u=i(n),f=i(o),d=function(t){function e(n,o){r(this,e);var i=c(this,t.call(this));return i.resolveOptions(o),i.listenClick(n),i}return a(e,t),e.prototype.resolveOptions=function t(){var e=arguments.length<=0||void 0===arguments[0]?{}:arguments[0];this.action="function"==typeof e.action?e.action:this.defaultAction,this.target="function"==typeof e.target?e.target:this.defaultTarget,this.text="function"==typeof e.text?e.text:this.defaultText},e.prototype.listenClick=function t(e){var n=this;this.listener=(0,f.default)(e,"click",function(t){return n.onClick(t)})},e.prototype.onClick=function t(e){var n=e.delegateTarget||e.currentTarget;this.clipboardAction&&(this.clipboardAction=null),this.clipboardAction=new l.default({action:this.action(n),target:this.target(n),text:this.text(n),trigger:n,emitter:this})},e.prototype.defaultAction=function t(e){return s("action",e)},e.prototype.defaultTarget=function t(e){var n=s("target",e);return n?document.querySelector(n):void 0},e.prototype.defaultText=function t(e){return s("text",e)},e.prototype.destroy=function t(){this.listener.destroy(),this.clipboardAction&&(this.clipboardAction.destroy(),this.clipboardAction=null)},e}(u.default);t.exports=d})},{"./clipboard-action":8,"good-listener":4,"tiny-emitter":7}]},{},[9])(9)});
</script>
<style>

#tbl2 .figure  {  font-family: "Computer Modern Serif", serif;
            margin-left:  auto;
                  margin-right: auto;
                  font-size: 66%;
  -webkit-print-color-adjust: exact;
         }

#tbl2 .caption { text-align:   center;
             margin-left:  auto;
             margin-right: auto;
             padding: 1.5em 0 1.5em 0;
           }

#tbl2 .figbody {
               text-align:   center;
             margin-left:  auto;
             margin-right: auto;
             border-style: double none solid none;
             border-width: 3pt 0 1pt 0;
             width:        100%;
         }

#tbl2 .figbody td {
                      padding: 0.2em 0.5em 0.2em 0.5em;
            }

#tbl2 .data:hover {
              background-color:yellow;
            }

#tbl2 .tangram {
    border-spacing: 0;
}

#tbl2 .subheader    { font-size: smaller; }

#tbl2 .subheader tr { border-collapse: collapse; }
#tbl2 .subheader td { border-bottom: 1pt solid black; }

#tbl2 .quantile .q25 {
    font-size: smaller;
    padding-right: 0.5em;
}

#tbl2 .quantile .q75 {
    font-size: smaller;
    padding-left: 0.5em;
}

#tbl2 .quantile .q50 {
  font-weight: bold;
}

#tbl2 .variable {
    float:      left;
    text-align: left;
}

#tbl2 .units {
    float:      right;
    font-size:  x-small;
    text-align: right;
    padding-left: 1em;
    vertical-align: text-bottom; /* FIXME why doesn't this work */
}

#tbl2 thead .header {font-weight: bold;}
#tbl2 .header.N  { font-style: italic; }
#tbl2 .header.N:before { content: "N="; }


#tbl2 .fraction .percentage {display: none;}
#tbl2 .fraction .numerator:before {content: "\00A0";}
#tbl2 .fraction .numerator { font-size: x-small; vertical-align: super; }
#tbl2 .fraction .numerator:after {content: " \2044";}
#tbl2 .fraction .denominator { font-size: x-small; vertical-align: sub;}
#tbl2 .statistics .description {font-style: italic;}
#tbl2 .statistic {padding-right: 0.5em;}

#tbl2 td .align{
    display: inline-block;
    margin: 0 auto;
}

#tbl2 .nobr {
   white-space: nowrap;
}

#tbl2 .supsub {
   display: inline-block;
   margin: -9em 0;
   vertical-align: -0.55em;
   line-height: 1.35em;
   font-size: x-small;
   text-align: left;
}

#tbl2 .sup:before {content: "\00A0";}
#tbl2 .sup { font-size: x-small; vertical-align: super; }
#tbl2 .sup:after {content: "\2044"; }
#tbl2 .sub { font-size: x-small; vertical-align: sub;}


</style>
Summary Statistics of Gapminder Data Set

<table class="tangram">
<thead>
<tr>
<td class="header even tg-label">
<span class="variable"></span>
</td>
<td class="header even tg-label">
<span class="variable">N</span>
</td>
<td class="header even tg-label">
<span class="variable">4 cylinders</span>
</td>
<td class="header even tg-label">
<span class="variable">6 cylinders</span>
</td>
<td class="header even tg-label">
<span class="variable">8 cylinders</span>
</td>
<td class="header even tg-label">
<span class="variable">Test Statistic</span>
</td>
</tr>
<tr class="subheaderrow">
<td class="subheader header even tg-label">
<span class="variable"></span>
</td>
<td class="subheader header even tg-label">
<span class="variable"></span>
</td>
<td class="subheader header even data N" data-clipboard-text="{list(index = " NDFh", src="tangram:transmission:cylinder[4 cylinders]:N" , value="8" ) N="NULL}&quot;">
<span class="N">8</span>
</td>
<td class="subheader header even data N" data-clipboard-text="{list(index = " NDE4", src="tangram:transmission:cylinder[6 cylinders]:N" , value="6" ) N="NULL}&quot;">
<span class="N">6</span>
</td>
<td class="subheader header even data N" data-clipboard-text="{list(index = " ZTE1", src="tangram:transmission:cylinder[8 cylinders]:N" , value="12" ) N="NULL}&quot;">
<span class="N">12</span>
</td>
<td class="subheader header even tg-label">
<span class="variable"></span>
</td>
</tr>
</thead>
<tbody>
<tr>
<td class="header odd tg-label">
<span class="variable">transmission : 1</span>
</td>
<td class="odd">
26
</td>
<td class="odd">
<span class="odd fraction" data-clipboard-text="{list(index = "NGM2", src = "tbl2:transmission[transmission : 1]:cylinder[4 cylinders]:numerator", value = "6") list(index = "ZTQw", src = "tbl2:transmission[transmission : 1]:cylinder[4 cylinders]:ratio", value = "0.857")}"><span class="ratio">0
.

857</span><span class="percentage">85.714</span><span class="numerator">6</span><span class="denominator">7</span></span>
</td>
<td class="odd">
<span class="odd fraction" data-clipboard-text="{list(index = "MTZm", src = "tbl2:transmission[NA]:cylinder[4 cylinders]:numerator", value = "3") list(index = "Yzk5", src = "tbl2:transmission[NA]:cylinder[4 cylinders]:ratio", value = "0.600")}"><span class="ratio">0
.

600</span><span class="percentage">60.000</span><span class="numerator">3</span><span class="denominator">5</span></span>
</td>
<td class="odd">
<span class="odd fraction" data-clipboard-text="{list(index = "MTZm", src = "tbl2:transmission[NA]:cylinder[4 cylinders]:numerator", value = "2") list(index = "Yzk5", src = "tbl2:transmission[NA]:cylinder[4 cylinders]:ratio", value = "0.182")}"><span class="ratio">0
.

182</span><span class="percentage">18.182</span><span class="numerator"> 2</span><span class="denominator">11</span></span>
</td>
<td class="odd data statistics" data-clipboard-text="{list(index = " Njhi", src="tbl2:transmission:cylinder:&lt;U+03C7&gt;">
^{2}", value = "8.20") list(index = "ODBk", src = "tbl2:transmission:cylinder:P", value = "0.017")}"&gt;<span class="statistic"><span class="description"><span class="nobr">χ<span class="supsub">2<br/>2</span></span> = </span>8.20,</span><span class="pvalue"><span class="description">P = </span>0.017</span><sup>2</sup>
</td>
</tr>
<tr>
<td class>
Missing
</td>
<td class>
3
</td>
<td class>
1
</td>
<td class>
1
</td>
<td class>
1
</td>
<td class="tg-label">
<span class="variable"></span>
</td>
</tr>
<tr>
<td class="header even tg-label">
<span class="variable">weight</span>
</td>
<td class="even data N" data-clipboard-text="{list(index = " OTkw", src="tbl2:weight:cylinder:cell_n1" , value="24" ) N="NULL}&quot;">
<span class="N">24</span>
</td>
<td class="even">
<span class="even data quantile"><span class="q25">1.83</span><span class="q25">1.90</span><span class="q50">2.14</span><span class="q75">2.90</span><span class="q75">3.15</span><br/><span>2.37±0.57</span>
</td>
<td class="even">
<span class="even data quantile"><span class="q25">2.77</span><span class="q25">2.84</span><span class="q50">3.21</span><span class="q75">3.44</span><span class="q75">3.44</span><br/><span>3.15±0.31</span>
</td>
<td class="even">
<span class="even data quantile"><span class="q25">3.17</span><span class="q25">3.51</span><span class="q50">3.67</span><span class="q75">3.86</span><span class="q75">5.34</span><br/><span>3.81±0.59</span>
</td>
<td class="even data statistics" data-clipboard-text="{list(index = " OThh", src="tbl2:weight:cylinder:F" , value="19.52" ) list(index="M2Nm" , src="tbl2:weight:cylinder:df2" , value="17" )}">
<span class="statistic"><span class="description">F<sub>2,17</sub> = </span>19.52,</span><span class="pvalue"><span class="description">P = </span>&lt;0.001<sup>1</sup></span>
</td>
</tr>
<tr>
<td class>
Missing
</td>
<td class>
6
</td>
<td class>
3
</td>
<td class>
1
</td>
<td class>
2
</td>
<td class="tg-label">
<span class="variable"></span>
</td>
</tr>
<tr>
<td class="header odd tg-label">
<span class="variable">milesPergallon</span>
</td>
<td class="odd data N" data-clipboard-text="{list(index = " NDRh", src="tbl2:milesPergallon:cylinder:cell_n1" , value="24" ) N="NULL}&quot;">
<span class="N">24</span>
</td>
<td class="odd">
<span class="odd data quantile"><span class="q25">22.8</span><span class="q25">25.8</span><span class="q50">30.4</span><span class="q75">31.6</span><span class="q75">33.9</span><br/><span>29.0±4.2</span>
</td>
<td class="odd">
<span class="odd data quantile"><span class="q25">17.8</span><span class="q25">18.7</span><span class="q50">19.7</span><span class="q75">21.1</span><span class="q75">21.4</span><br/><span>19.8±1.4</span>
</td>
<td class="odd">
<span class="odd data quantile"><span class="q25">10.4</span><span class="q25">14.2</span><span class="q50">15.2</span><span class="q75">16.0</span><span class="q75">19.2</span><br/><span>15.1±2.4</span>
</td>
<td class="odd data statistics" data-clipboard-text="{list(index = " MzA0", src="tbl2:milesPergallon:cylinder:F" , value="37.84" ) list(index="YTY0" , src="tbl2:milesPergallon:cylinder:df2" , value="16" )}">
<span class="statistic"><span class="description">F<sub>2,16</sub> = </span>37.84,</span><span class="pvalue"><span class="description">P = </span>&lt;0.001<sup>1</sup></span>
</td>
</tr>
<tr>
<td class>
Missing
</td>
<td class>
7
</td>
<td class>
3
</td>
<td class>
1
</td>
<td class>
3
</td>
<td class="tg-label">
<span class="variable"></span>
</td>
</tr>
</tbody>
</table>

N is the number of non-missing value. <sup>1</sup>Kruskal-Wallis. <sup>2</sup>Pearson. <sup>3</sup>Wilcoxon.

<script>new Clipboard('.data');</script>
<!--/html_preserve-->
This package has [way more functionality](https://cran.r-project.org/web/packages/tangram/vignettes/fda-example.html) than we have shown. The [documentation](https://cran.r-project.org/web/packages/tangram/tangram.pdf) is very long. However, not very detailed. The [vignette](https://cran.r-project.org/web/packages/tangram/vignettes/example.html) does not show many more examples and when it does, it is a pain to understand the code behind it. Overall it is a good package and when you want to customize it more I would suggest using another package.

### Create Descriptive Summary Statistics Tables in R with furniture

Our next package will be the `furniture` package. It is an okay package in my opinion. Missing values are only displayed for categorical variables and only as percentages again. The overall look of the table is very simple.

``` r
furniture::table1(mtcars,
  "Miles per US gallon" = milesPergallon, "Transmission" = transmission, "Weight 1000 lbs" = weight,
  splitby = ~cylinder,
  test = TRUE,
  na.rm = FALSE,
  format_number = TRUE
) -> tab11
```

``` r
kable(tab11)
```

<table class="kable_wrapper">
<tbody>
<tr>
<td>
|                     | 4 cylinders | 6 cylinders | 8 cylinders | P-Value  |
|---------------------|:------------|:------------|:------------|:---------|
|                     | n = 7       | n = 6       | n = 12      |          |
| Miles.per.US.gallon |             |             |             | &lt;.001 |
|                     | 29.0 (4.2)  | 19.8 (1.4)  | 15.1 (2.4)  |          |
| Transmission        |             |             |             | 0.017    |
| 0                   | 1 (14.3%)   | 2 (33.3%)   | 9 (75%)     |          |
| 1                   | 6 (85.7%)   | 3 (50%)     | 2 (16.7%)   |          |
| NA                  | 0 (0%)      | 1 (16.7%)   | 1 (8.3%)    |          |
| Weight.1000.lbs     |             |             |             | &lt;.001 |
|                     | 2.4 (0.6)   | 3.1 (0.3)   | 3.8 (0.6)   |          |

</td>
</tr>
</tbody>
</table>
``` r
# only shows missingness for factor variables
```

There is nothing much more to say and if you are interested you can find the vignette \[here\](<https://cran.r-project.org/web/packages/furniture/vignettes/Furniture.html>.

### Create Descriptive Summary Statistics Tables in R with tableone

The `tableone` package is more aesthetic than the `furniture` package. However, it does not display missing values. If you want to display missing values, you must print them out in a separate table with the `summary()` function.

We first have to specify which variables are continuous and which variables are categorical in our data set.

``` r
factor_variables <- c("transmission")
variable_list <- c("milesPergallon", "weight", "transmission")
```

``` r
table_one <- CreateTableOne(
  vars = variable_list,
  strata = "cylinder",
  data = mtcars,
  factorVars = factor_variables
)
```

``` r
table_one_matrix <- print(table_one,
  includeNA = TRUE,
  showAllLevels = TRUE
)
```

The following table does not include missing values

``` r
pandoc_table <- pandoc.table(table_one_matrix,
  split.table = Inf,
  style = "rmarkdown",
  caption = "mtcars summary statistics table"
)
```

|                                | level |  4 cylinders |  6 cylinders |  8 cylinders |     p     | test |
|:------------------------------:|:-----:|:------------:|:------------:|:------------:|:---------:|:----:|
|              **n**             |       |       8      |       6      |      12      |           |      |
| **milesPergallon (mean (sd))** |       | 28.96 (4.16) | 19.82 (1.45) | 15.06 (2.36) | &lt;0.001 |      |
|     **weight (mean (sd))**     |       |  2.37 (0.57) |  3.15 (0.31) |  3.81 (0.59) | &lt;0.001 |      |
|      **transmission (%)**      |   0   |   1 (14.3)   |   2 (40.0)   |   9 (81.8)   |   0.017   |      |
|                                |   1   |   6 (85.7)   |   3 (60.0)   |   2 (18.2)   |           |      |

For a more detailed summary on missing values.

``` r
summary(table_one)
```

    ## 
    ##      ### Summary of continuous variables ###
    ## 
    ## cylinder: 4 cylinders
    ##                n miss p.miss mean  sd median p25 p75 min max skew kurt
    ## milesPergallon 8    3     38   29 4.2     30  27  30  23  34 -0.6  0.6
    ## weight         8    3     38    2 0.6      2   2   3   2   3  0.7 -1.9
    ## -------------------------------------------------------- 
    ## cylinder: 6 cylinders
    ##                n miss p.miss mean  sd median p25 p75 min max skew kurt
    ## milesPergallon 6    1     17   20 1.4     20  19  21  18  21 -0.4 -0.9
    ## weight         6    1     17    3 0.3      3   3   3   3   3 -0.3 -2.7
    ## -------------------------------------------------------- 
    ## cylinder: 8 cylinders
    ##                 n miss p.miss mean  sd median p25 p75 min max skew kurt
    ## milesPergallon 12    3     25   15 2.4     15  15  16  10  19 -0.4    2
    ## weight         12    2     17    4 0.6      4   4   4   3   5  2.1    6
    ## 
    ## p-values
    ##                     pNormal   pNonNormal
    ## milesPergallon 5.327177e-07 0.0005935734
    ## weight         4.798813e-04 0.0013354004
    ## 
    ## Standardize mean differences
    ##                 average   1 vs 2   1 vs 3   2 vs 3
    ## milesPergallon 3.158870 2.934299 4.109743 2.432569
    ## weight         1.859762 1.693423 2.482342 1.403523
    ## 
    ## =======================================================================================
    ## 
    ##      ### Summary of categorical variables ### 
    ## 
    ## cylinder: 4 cylinders
    ##           var n miss p.miss level freq percent cum.percent
    ##  transmission 8    1   12.5     0    1    14.3        14.3
    ##                                 1    6    85.7       100.0
    ##                                                           
    ## -------------------------------------------------------- 
    ## cylinder: 6 cylinders
    ##           var n miss p.miss level freq percent cum.percent
    ##  transmission 6    1   16.7     0    2    40.0        40.0
    ##                                 1    3    60.0       100.0
    ##                                                           
    ## -------------------------------------------------------- 
    ## cylinder: 8 cylinders
    ##           var  n miss p.miss level freq percent cum.percent
    ##  transmission 12    1    8.3     0    9    81.8        81.8
    ##                                  1    2    18.2       100.0
    ##                                                            
    ## 
    ## p-values
    ##                 pApprox     pExact
    ## transmission 0.01658932 0.01642731
    ## 
    ## Standardize mean differences
    ##               average    1 vs 2   1 vs 3   2 vs 3
    ## transmission 1.128814 0.6040404 1.833899 0.948504

As with the furniture package there is nothing more to add and the vignette can be found [here](https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html).

### Create Descriptive Summary Statistics Tables in R with compareGroups

`ComapareGroups` is another great package that can stratify our table by groups. It is very simple to use. One drawback however is that it does not display missing values by default. When we want to add missing values we must include the argument include.miss = TRUE. The missing values are only displayed as percentages. As with the tableone package, we can display missing values in a separate table.

Another drawback of the `compareGroups` package is that the table can only display five or less groups. So, when we have a column with more than five levels, `compareGroups` cannot create the table and we must pivot to another one. Fortunately, for the mtcars data set we only have three groupings (4 cylinders, 6 cylinders, and 8 cylinders).

``` r
library(compareGroups)
```

``` r
table <- compareGroups(cylinder ~ ., data = mtcars)
pvals <- getResults(table, "p.overall")
p.adjust(pvals, method = "BH")
```

transmission weight milesPergallon 1.642731e-02 7.198219e-04 1.598153e-06

``` r
export_table <- createTable(table)
export2word(export_table, file = "table.docx")
```

There is a lot more to discover for this package in the [vignette](https://cran.r-project.org/web/packages/compareGroups/vignettes/compareGroups_vignette.html).

### Create Descriptive Summary Statistics Tables in R with Gmisc

The `Gmisc` package is another great package which will create an awesome looking summary statistics table for you. Relabelling variables is very easy and the table looks really beautiful. The only drawback is that the table can only be created in an html file. It unfortunately cannot be knitted to a word document.

``` r
library(Gmisc)
```

``` r
getT1Stat <- function(varname, digits = 0) {
  getDescriptionStatsBy(mtcars[, varname],
    mtcars$cylinder,
    add_total_col = TRUE,
    show_all_values = TRUE,
    hrzl_prop = TRUE,
    statistics = FALSE,
    html = TRUE,
    digits = digits
  )
}

table_data <- list()
```

``` r
table_data[["Miles/(US) gallon"]] <- getT1Stat("milesPergallon")
table_data[["Weight (1000 lbs)"]] <- getT1Stat("weight")
table_data[["Transmission (0 = automatic, 1 = manual)"]] <- getT1Stat("transmission")
```

``` r
rgroup <- c()
n.rgroup <- c()
output_data <- NULL
for (varlabel in names(table_data)) {
  output_data <- rbind(
    output_data,
    table_data[[varlabel]]
  )
  rgroup <- c(
    rgroup,
    varlabel
  )
  n.rgroup <- c(
    n.rgroup,
    nrow(table_data[[varlabel]])
  )
}


htmlTable(output_data,
  align = "rrrr",
  rgroup = rgroup, n.rgroup = n.rgroup,
  rgroupCSSseparator = "",
  rowlabel = "",
  caption = "Summary Statistics",
  ctable = TRUE
)
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Summary Statistics
</td>
</tr>
<tr>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Total
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
4 cylinders
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
6 cylinders
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
8 cylinders
</th>
</tr>
</thead>
<tbody>
<tr>
<td colspan="5" style="font-weight: 900;">
Miles/(US) gallon
</td>
</tr>
<tr>
<td style="text-align: left;">
  Mean (SD)
</td>
<td style="text-align: right;">
20 (± 6)
</td>
<td style="text-align: right;">
29 (± 4)
</td>
<td style="text-align: right;">
20 (± 1)
</td>
<td style="text-align: right;">
15 (± 2)
</td>
</tr>
<tr>
<td style="text-align: left;">
  Missing
</td>
<td style="text-align: right;">
7 (27%)
</td>
<td style="text-align: right;">
3 (38%)
</td>
<td style="text-align: right;">
1 (17%)
</td>
<td style="text-align: right;">
3 (25%)
</td>
</tr>
<tr>
<td colspan="5" style="font-weight: 900;">
Weight (1000 lbs)
</td>
</tr>
<tr>
<td style="text-align: left;">
  Mean (SD)
</td>
<td style="text-align: right;">
3 (± 1)
</td>
<td style="text-align: right;">
2 (± 1)
</td>
<td style="text-align: right;">
3 (± 0)
</td>
<td style="text-align: right;">
4 (± 1)
</td>
</tr>
<tr>
<td style="text-align: left;">
  Missing
</td>
<td style="text-align: right;">
6 (23%)
</td>
<td style="text-align: right;">
3 (38%)
</td>
<td style="text-align: right;">
1 (17%)
</td>
<td style="text-align: right;">
2 (17%)
</td>
</tr>
<tr>
<td colspan="5" style="font-weight: 900;">
Transmission (0 = automatic, 1 = manual)
</td>
</tr>
<tr>
<td style="text-align: left;">
  0
</td>
<td style="text-align: right;">
12 (46%)
</td>
<td style="text-align: right;">
1 (8%)
</td>
<td style="text-align: right;">
2 (17%)
</td>
<td style="text-align: right;">
9 (75%)
</td>
</tr>
<tr>
<td style="text-align: left;">
  1
</td>
<td style="text-align: right;">
11 (42%)
</td>
<td style="text-align: right;">
6 (55%)
</td>
<td style="text-align: right;">
3 (27%)
</td>
<td style="text-align: right;">
2 (18%)
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid grey; text-align: left;">
  Missing
</td>
<td style="border-bottom: 2px solid grey; text-align: right;">
3 (12%)
</td>
<td style="border-bottom: 2px solid grey; text-align: right;">
1 (33%)
</td>
<td style="border-bottom: 2px solid grey; text-align: right;">
1 (33%)
</td>
<td style="border-bottom: 2px solid grey; text-align: right;">
1 (33%)
</td>
</tr>
</tbody>
</table>
For more information and examples have a look at the [vignette](https://cran.r-project.org/web/packages/Gmisc/vignettes/Descriptives.html).

I discovered all of these packages during my data science internship. If you want to know what else I had to do and what I learned from this data science internship then you can read about it [here](http://thatdatatho.com/2018/07/23/data-science-internship/).

I hope you all have enjoyed this post and that you have found a package which suits your needs. If there are any other packages you know of that I have missed in this blog post please let me know in the comments below.

If you liked this blog post, you might also like a [collection of other packages](https://htmlpreview.github.io/?https://github.com/ropenscilabs/packagemetrics/blob/master/inst/examples/tableGallery.html#7:_tableby) which can create tables for you (anova tables, linear models output tables, and more descriptive summary statistics tables). The ones I presented are the best ones for descriptive summary tables I believe.

Lastly, [this](https://dabblingwithdata.wordpress.com/2018/01/02/my-favourite-r-package-for-summarising-data/) is another great blog post that presents how to easily summarise data in R.
