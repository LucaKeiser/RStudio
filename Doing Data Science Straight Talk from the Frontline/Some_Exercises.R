# Cathy O'Neil and Rachel Schutt (2013): Doing Data Science: Straight Talk from the Frontline


# Linear Regrassion -------------------------------------------------------

# page 70f.

# set seed
set.seed(02102021)

# Simulating fake data
# all ture_-values are **not** observable in reality! 
true_error <- rnorm(n = 2000, mean = 0, sd = 2)
true_beta_0 <- 1.1
true_beat_1 <- -8.2
true_beta_2 <- 10

x_1 <- rnorm(n = 2000, mean = 5, sd = 7)
x_2 <- rgamma(n = 2000, shape = 5, rate = 2)
y <- true_beta_0 + true_beat_1 * x_1 + true_beta_2 * x_2 + true_error


# take a look
hist(y, col = "grey")
hist(x_1, col = "grey")
hist(x_2, col = "grey")

# creating data set
library(tidyverse)

df <- tibble(
  y = y,
  x_1 = x_1,
  x_2 = x_2
)


### exercise 1

model1 <- summary(lm(y ~ x_1, data = df))
model1

# plot
plot(df$x_1, df$y, pch = 20, col = "red")
abline(model1$coefficients[1], model1$coefficients[2])


#### exercise 2

model2 <- summary(lm(y ~ x_1, data = df))
model2

model3 <- summary(lm(y ~ x_2, data = df))
model3


model4 <- summary(lm(y ~ x_1 + x_2, data = df))
model4


### cross validation
# create training and testing data set with tidymodels

library(tidymodels)

spl <- initial_split(df, prop = 0.8)
train <- training(spl)
test <- testing(spl)


# calculate the mean squared error and compare!
model5 <- summary(lm(y ~ x_1 + x_2, data = train))
mean(model5$residuals^2)


model6 <- summary(lm(y ~ x_1 + x_2, data = test))
mean(model6$residuals^2)




# k-Nearest Neighbors (k-NN) ----------------------------------------------

# page 77ff.
library(tidyverse)

# set seed
set.seed(1234)

# create fake data set
df <- tibble(
  age = runif(n = 1000, min = 18, max = 95),
  income = runif(n = 1000, min = 15, max = 250)
  
)

# create credit variable (not very sophisticated...)
df <- df %>% 
  mutate(credit = case_when(
    income >= 15 & income < 50 ~ "low",
    income >= 50 & income <= 250 ~ "high"))


# create training and testing set
library(tidymodels)

spl <- initial_split(df, prop = 0.8)

training <- training(spl)
testing <- testing(spl)

# pretend you do not know the labels
train <- training %>% 
  select(c(age, income))
test <- testing %>% 
  select(c(age, income))

# how many labels are in the test set?
num.test.set.labels <- 800

# true labels
# subset of labels for the training set
cl <- training$credit
# subset of labels for the test set, we're withholding these
true.labels <- testing$credit

# predict labels with k = 3 (guess)
library(class)

# note: cl are the true labels of the training set (n = 800) 
# with these values we want to predict the labels for the testing set!
# so the output is 200 guessed labels!
knn(train, test, cl, k = 3)


# what is a good choice for k?
for (k in 1:20) {
  predicted.labels <- knn(train, test, cl, k)
  num.incorrect.labels <- sum(predicted.labels != true.labels)
  missclassification.rate <- num.incorrect.labels / num.test.set.labels
  print(c(k, missclassification.rate))
}




# K-Means -----------------------------------------------------------------

# https://www.statology.org/k-means-clustering-in-r/

# load packages
library(factoextra)
library(cluster)
library(tidyverse)


# prepare data 

# use the USArrest data set
df <- USArrests

# remove rows with missing values
df <- na.omit(df)

# scale each variable to have a mean of 0 and sd of 1
# sclae() creates a matrix => is necessary!
# => Scaling and Centering of Matrix-like Objects
df <- scale(df)

# take a look
head(df)



### start with K-Means

# note:
# centers = numer of clusters (k)
# nstart = number of initial configurations. It is possible that different 
# inital starting clusters lead to different results... 
# it is recommended to use several different inital configurations
# K-Means will find the inital configuration wirh the smallest within-cluser variation

### how many clusters?
fviz_nbclust(df, kmeans, method = "wss")

# Typically when we create this type of plot we look for an “elbow” where 
# the sum of squares begins to “bend” or level off. This is typically the 
# optimal number of clusters. Here; 4 clusters


# OR

gap_stat <- clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
fviz_gap_stat(gap_stat)

# again it seems to be 4 (gap statistic is highest...)


### cluster!

set.seed(1234)

# perform k-means clustering with k = 4
km <- kmeans(df, centers = 4, nstart = 25)
km

# K-means clustering with 4 clusters of sizes 8, 13, 16, 13
# numer below the sates = numer of the cluster

# visualize the output
fviz_cluster(km, data = df)

# aggregate the means for each cluster with aggregate()
# per 100'000 citizens | UrbanPop in %!
aggregate(USArrests, by = list(cluster = km$cluster) , FUN = mean)


# create final data set
final_df <- cbind(USArrests, cluster = km$cluster)

# aggregate again
mean(final_df$Murder[final_df$cluster == 1])
mean(final_df$Murder[final_df$cluster == 2])
# etc.

# or

for (i in 1:4) {
  
 x <- final_df %>% 
      filter(cluster == i) %>% 
      summarise(Murder = mean(Murder),
                Assault = mean(Assault),
                UrbanPop = mean(UrbanPop),
                Rape = mean(Rape))
 
  print(as_tibble(x))

  }



