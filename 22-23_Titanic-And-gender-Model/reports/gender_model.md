Gender Model
================
Pascal Schmidt
September 23, 2018

In this tutorial, I will be showing you how to achieve 82% accuracy with the titanic women-child model.

There are two rules to this model:

-   We predict that all males die except for boys in families where the majority of females and boys live.
-   We predict that all females live except for females in families where the majority of females and boys die.

Last tutorial, we achieved an accuracy of around 79%. We used a random forest algorithm, a logistic regression, k-nearest neighbors, and linear discriminant analysis. All of these models were decent but are not able to reach the women-child model.

The women-child model can be created by only looking at the last names of passengers and also their corresponding ticket numbers. Let's jump into our analysis.

First, we have to load in the data.

``` r
library(tidyverse)
library(here)
library(knitr)

train <- read.csv(here::here("docs", "train.csv"))
test <- read.csv(here::here("docs", "test.csv"))
test$Survived <- NA
titanic <- rbind(train, test) # combining data sets

titanic %>%
  dplyr::mutate(
    Name = as.character(Name),
    Ticket = as.character(Ticket),
    Cabin = as.character(Cabin),
    Survived = as.factor(Survived),
    Pclass = as.factor(Pclass)
  ) -> titanic
```

Again, we are engeneering a new variable called `titles` like last time.

``` r
titanic$titles <- gsub("(.*\\,|\\..*)", "", titanic$Name) %>%
  gsub("[[:space:]]", "", .)

titanic$last_name <- gsub("\\,.*", "", titanic$Name)
```

Sometimes, ticket numbers differ from each other in their last digits. For example, in the table below we can be sure that the first three passengers travelled together. This is because they embarked in Southampton and all are class 3. There ticket number only differs in the last digits.

``` r
titanic %>%
  dplyr::group_by(Ticket, Pclass, Embarked) %>%
  dplyr::summarise() %>%
  tail() %>%
  pander::pandoc.table()
```

<table style="width:46%;">
<colgroup>
<col width="19%" />
<col width="12%" />
<col width="13%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Ticket</th>
<th align="center">Pclass</th>
<th align="center">Embarked</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">W./C. 6607</td>
<td align="center">3</td>
<td align="center">S</td>
</tr>
<tr class="even">
<td align="center">W./C. 6608</td>
<td align="center">3</td>
<td align="center">S</td>
</tr>
<tr class="odd">
<td align="center">W./C. 6609</td>
<td align="center">3</td>
<td align="center">S</td>
</tr>
<tr class="even">
<td align="center">W.E.P. 5734</td>
<td align="center">1</td>
<td align="center">S</td>
</tr>
<tr class="odd">
<td align="center">W/C 14208</td>
<td align="center">2</td>
<td align="center">S</td>
</tr>
<tr class="even">
<td align="center">WE/P 5735</td>
<td align="center">1</td>
<td align="center">S</td>
</tr>
</tbody>
</table>

So what we will be doing is to substitute the last 2 digits or letters of a ticket with "XX". Below is a little working example of how this works. We are grabbing the last 2 characters of the string with `str_sub` from the `stringr` package and then we are substituting it with "XX". This will be done with the `gsub` function.

``` r
x <- "hello"
gsub(stringr::str_sub(x, -1), "X", x)
```

    ## [1] "hellX"

Now, we are applying this method to the entire `Ticket` column in the titanic data set.

``` r
for(i in 1:length(titanic$Ticket)) {
  titanic$ticket_number[[i]] <- gsub(stringr::str_sub(titanic$Ticket[[i]], -2), "XX", titanic$Ticket[[i]])
}
```

Before we are continuing with our analysis, we will be throwing out all rows with passengers who are male, except for children.

``` r
titanic %>%
  dplyr::filter(Sex == "female" | titles == "Master") %>%
  dplyr::group_by(last_name, titles, ticket_number, Sex, Survived, PassengerId) %>%
  dplyr::summarise() %>%
  dplyr::select(last_name, titles, ticket_number, Sex, Survived, PassengerId) -> gender_model

head(gender_model) %>%
  pander::pandoc.table()
```

<table style="width:97%;">
<colgroup>
<col width="16%" />
<col width="12%" />
<col width="22%" />
<col width="12%" />
<col width="15%" />
<col width="18%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">last_name</th>
<th align="center">titles</th>
<th align="center">ticket_number</th>
<th align="center">Sex</th>
<th align="center">Survived</th>
<th align="center">PassengerId</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Abbott</td>
<td align="center">Master</td>
<td align="center">C.A. 26XX</td>
<td align="center">male</td>
<td align="center">NA</td>
<td align="center">1284</td>
</tr>
<tr class="even">
<td align="center">Abbott</td>
<td align="center">Mrs</td>
<td align="center">C.A. 26XX</td>
<td align="center">female</td>
<td align="center">1</td>
<td align="center">280</td>
</tr>
<tr class="odd">
<td align="center">Abelseth</td>
<td align="center">Miss</td>
<td align="center">3481XX</td>
<td align="center">female</td>
<td align="center">NA</td>
<td align="center">1237</td>
</tr>
<tr class="even">
<td align="center">Abelson</td>
<td align="center">Mrs</td>
<td align="center">P/PP 33XX</td>
<td align="center">female</td>
<td align="center">1</td>
<td align="center">875</td>
</tr>
<tr class="odd">
<td align="center">Abrahim</td>
<td align="center">Mrs</td>
<td align="center">26XX</td>
<td align="center">female</td>
<td align="center">NA</td>
<td align="center">900</td>
</tr>
<tr class="even">
<td align="center">Ahlin</td>
<td align="center">Mrs</td>
<td align="center">75XX</td>
<td align="center">female</td>
<td align="center">0</td>
<td align="center">41</td>
</tr>
</tbody>
</table>

After we have ordered the data, we have to identify families now. We do that by comparing the `last_name` and `ticket_number` columns. When either the preceding or following `ticket_number` and `last_name` are identical, then the familyID will be `family`. Otherwise, `no family`. Afterwards, we have to throw out passengers who travelled alone.

``` r
gender_model$familyID <- NA
gender_model$familyID[[1]] <- "family"
for(i in 2:(length(gender_model$last_name) - 1)) {
  if ((gender_model$last_name[[i]] == gender_model$last_name[[i + 1]] |
      gender_model$last_name[[i]] == gender_model$last_name[[i - 1]]) &
      (gender_model$ticket_number[[i]] == gender_model$ticket_number[[i + 1]] |
      gender_model$ticket_number[[i]] == gender_model$ticket_number[[i - 1]])) {
    gender_model$familyID[[i]] <- "family"
  } else {
        gender_model$familyID[[i]] <- "no family"
      }
}

gender_model %>%
  dplyr::filter(familyID == "family") -> gender_model
length(unique(gender_model$last_name))
```

    ## [1] 81

We are ending up with 81 unique families.

``` r
ggplot(gender_model, aes(x = last_name, fill = Survived)) + 
  geom_bar() +
  ylab("Family Name") +
  ggtitle("All 81 Women-Child Groups") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
```

![](gender_model_files/figure-markdown_github/unnamed-chunk-9-1.png)

The plot makes the power of the women-child model visible. Either all family members die or survive. Only family Alison and family Asplund have family members who died and survived. For other families, either all members survived or died.

What we will be doing next is labelling who survived and died. All NA values for families who all survived will be predicted to have survived as well. All NA values for families who all died will be predicted to have died as well. For the family Alison and Asplund we are assiging the survived and died labels to the NA values based on the majority voting. If the majority of the family survived, then the NA values will be substituted with survived (1). If the majority died, then the NA value will be substituted with died (0).

For the families Gibson, Klasen, and Peacock, all values are missing.

``` r
gender_model$Survived <- as.numeric(as.character(gender_model$Survived))
gender_mod <- split(gender_model, gender_model$last_name)


for (i in 1:length(gender_mod)) {
  if (is.nan(mean(gender_mod[[i]]$Survived, na.rm = TRUE))) {
    gender_mod[[i]]$Survived[is.na(gender_mod[[i]]$Survived)] <- NA
  } else if (mean(gender_mod[[i]]$Survived, na.rm = TRUE) == 0) {
    gender_mod[[i]]$Survived[is.na(gender_mod[[i]]$Survived)] <- 0
  } else if (mean(gender_mod[[i]]$Survived, na.rm = TRUE) == 1) {
    gender_mod[[i]]$Survived[is.na(gender_mod[[i]]$Survived)] <- 1
  } else if (mean(gender_mod[[i]]$Survived, na.rm = TRUE) >= 0.5) {
    gender_mod[[i]]$Survived[is.na(gender_mod[[i]]$Survived)] <- 1 
  } else {
    gender_mod[[i]]$Survived[is.na(gender_mod[[i]]$Survived)] <- 0 
  }
}

gender_mod <- do.call(rbind, gender_mod)
```

``` r
plot_pclass <- ggplot(titanic[1:891, ], aes(x = Pclass, fill = Survived)) + 
  geom_bar()
plot_pclass
```

![](gender_model_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
titanic %>%
  dplyr::filter(last_name %in% c("Klasen", "Peacock", "Gibson")) %>%
  dplyr::select(last_name, Pclass) %>%
  pander::pandoc.table()
```

    ## 
    ## --------------------
    ##  last_name   Pclass 
    ## ----------- --------
    ##   Klasen       3    
    ## 
    ##   Klasen       3    
    ## 
    ##   Peacock      3    
    ## 
    ##   Klasen       3    
    ## 
    ##   Peacock      3    
    ## 
    ##   Gibson       1    
    ## 
    ##   Gibson       1    
    ## 
    ##   Peacock      3    
    ## --------------------

``` r
gender_mod$Survived[gender_mod$last_name %in% c("Klasen", "Peacock")] <- 0
gender_mod$Survived[gender_mod$last_name == "Gibson"] <- 1
```

Because the Klasen family and the Peacock family are class 3 passengers, we are predicting that they died. Because the Gibson family travelled in class 1, we are predicting that they survived.

Now, we have to merge our results back into our original titanic data set. We can do that with a double for loop. However, this takes some time.

``` r
system.time(
for(i in 1:nrow(gender_mod)) {
  for(j in 1:nrow(titanic)) {
    if (gender_mod[["PassengerId"]][[i]] == titanic[["PassengerId"]][[j]]) {
      titanic[["Survived"]][[j]] <- gender_mod[["Survived"]][[i]]
    }
  }
}
)
```

    ##    user  system elapsed 
    ##    4.17    0.00    4.17

So, if we want to save time we can also vectorize our operation like in the code below.

``` r
gender_mod_survived <- gender_mod %>%
  filter(Survived == 1)
gender_mod_died <- gender_mod %>%
  filter(Survived == 0)

titanic$Survived[titanic$PassengerId %in% gender_mod_survived$PassengerId] <- gender_mod_survived$Survived
titanic$Survived[titanic$PassengerId %in% gender_mod_died$PassengerId] <- gender_mod_died$Survived
```

Now, that our `gender_model` results are back in our original data frame, we will be predicting that every male passengers who's survival is stil unkown will die and every female who's survival is still unkown will live.

After that, we will be sending in our predictions.

``` r
titanic$Survived[is.na(titanic$Survived) & titanic$Sex == "male"] <- 0
titanic$Survived[is.na(titanic$Survived) & titanic$Sex == "female"] <- 1


sub <- data.frame(PassengerId = 892:1309, Survived = titanic[892:1309, ]$Survived)
write.csv(sub, here::here("docs", "gender_model.csv"), row.names = FALSE)
```

![](gender_model_files/figure-markdown_github/gender_model_submission.png)

Heya! Almost 82%. That is fantastic. The women-child model is way less time consuming than all the model building in part. It is easy, straight forward but still very powerful. So powerful that it gives us around 3% better accuracy on the test set.

I hope you have enjoyed the second part of the tutorial and if you have any questions, you can write them in the comments below.
