Bank Discrimination Experiment - OpenIntro Statistics Book
================
Benay Tomas
2025-06-24

## Introduction

In this notebook we will replicate the experiment seen in the book
OpenIntro Statistics, Chapter 1.

The experiment was originally set in the 70s and consisted of bank
supervisors that were randomly assigned files of 24 males and 24 females
and then were asked to determine if they would promote or not the person
on file.

Now, for the rest of this report i’ll explain how to determine if the
difference in promotion rates is due by chance or not.

The basic idea is as such:

First, we calculate the difference in promotion rate of men and women,
that being the **observed rate**, then we considere the rate result as
being simply by chance - as the promotions choices not having any
discrimination whatsoever. This will be the *Null Hypothesis*.

We then redo the experiment through computation many times, and see how
often does the difference in promotion rates reach at least the
*observed rate*, if throughout the entire experiment there’s less than
5%, then the *Null Hypothesis* can be dismissed and the *Alternative
Hypothesis* (there was discrimination) can be favored.

``` r
# this imports the ggplot2 library, which will be used for the visualization
library(ggplot2)
```

In the original experiment, 21 men were promoted, while 14 women were
promoted.

``` r
# this sets a seed for the randomizations to be replicable
# and we also set the number of times the test will be done
# in this case, 100 times
set.seed(10)
test_length <- 100

males <- 24
females <- 24
total <- males + females

promoted <- 35
not_promoted <- 13

obs_diff <- 21/males - 14/females

# this is the difference in promotion rate between men and women
print(obs_diff)
```

    ## [1] 0.2916667

``` r
# the diffs vector will contain all the rates reached in each loop of the experiment
diffs <- numeric(test_length)

for (i in 1:test_length){
  # assign a vector with 35 1's to represent the promotions
  # and 13 0's to represent the not-promotions
  promos <- c(rep(1, promoted), rep(0, not_promoted))
  # then shuffle the values
  shuffle <- sample(promos)
  
  male_group <- shuffle[1:24]
  female_group <- shuffle[25:total]
  
  # as the promotions are represented with 1s and the not-promotions represented with 0s - when we do the mean we get the rate of promotion
  male_rate <- mean(male_group)
  female_rate <- mean(female_group)
  
  # and then store that difference in promotion in the diffs vector
  diffs[i] <- male_rate - female_rate
  
  # here we can also see which rates are greater than or equal to the observed difference - which are 3 different occasions out of the 100 tests
  if (diffs[i] >= obs_diff){
   cat(diffs[i], "\n") 
  }
}
```

    ## 0.375 
    ## 0.2916667 
    ## 0.375

After all the iterations in the experiment we can analyze the chart
result and then after check what’s the veredict.

``` r
draw_plot <- function(data){
  stripchart(data, method="stack", at=0,
             col="black",
             main="Stacked Dot Plot",
             xlab="Male-Female Promotion Rates")
}
```

In this graph chart, each point represents the difference in promotion
rates between men and women, near zero, almost no difference was seen
and the farther from zero, the more difference was seen. In this
particular case we can technically dismiss negative rates because that
would imply in a third hypothesis (men being discriminated) which we are
not taking in consideration for this experiment.

![](plot.png)<!-- -->

So with the chart we can have a better understanding of the difference
rates.

With this we can calculate the **P-Value**, which is the percentage of
times where the **observed rate** was achieved or surpassed.

``` r
p_value <- round(mean(diffs >= obs_diff), digits=3)
cat("p-value:", p_value)
```

    ## p-value: 0.03

``` r
if (p_value <= 0.05){
  cat("Null Hypothesis Not Favored (<= 0.05) due to low p-value:", p_value, "\n")
}else{
  cat("Null Hypothesis Favored (> 0.05), due to high p-value:", p_value, "\n")
}
```

    ## Null Hypothesis Not Favored (<= 0.05) due to low p-value: 0.03

And at the end we can say if the Null or the Alternative Hypothesis is
favored, depending on the p-value. In the case of this seed, the Null
Hypothesis is Not favored.

This means that for this particular circumstance - it is favored to say
that there was at least some level of discrimination against women.
