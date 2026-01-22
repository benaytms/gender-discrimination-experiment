## -----------------------------------------------------------------------------
# this imports the ggplot2 library, which will be used for the visualization
library(ggplot2)


## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
draw_plot <- function(data){
  stripchart(data, 
             method="stack",
             at=0,
             col="black",
             main="Stacked Dot Plot",
             xlab="Male-Female Promotion Rates",
             ylab="Count"
  )
}


## -----------------------------------------------------------------------------
draw_plot(diffs)
## -----------------------------------------------------------------------------
p_value <- round(mean(diffs >= obs_diff), digits=3)
cat("p-value:", p_value, "\n")
if (p_value <= 0.05){
  cat("Null Hypothesis Not Favored (<= 0.05) due to low p-value:", p_value, "\n")
}else{
  cat("Null Hypothesis Favored (> 0.05), due to high p-value:", p_value, "\n")
}

