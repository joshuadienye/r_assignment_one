######################################
#Created by Joshua Dienye on 12/04/2020
#Homework coding questions #1
#Cohort: MSBA 4
#Email address: jdienye2019@student.hult.edu
######################################

# calling necessary libraries
library(tensorflow)
library(MASS)

######################################
#Exercise 3.4 question 3
######################################

install.packages("tensorflow")
help(package = tensorflow)
?tensorflow
# the function in tensorflow that trains a model is train
?train
# it trains a model object

######################################
#Exercise 3.4 question 8
######################################

# creating the UDF transformmatrix
transformmatrix <- function(x){
  # creating an empty vector
  b <- c()
  # for index number in row 
  for (i in 1:nrow(x)) {
    # for index number in column
    for (j in 1:ncol(x)) {
      # if index number of row equal to index number of column
      if(i == j){
        # put value in empty vector
        b[i] <- x[i,j]
      }# closing if statement
      # if index number of row not equal to index number of column
      else{
        # go to next iteration
        next
      }# closing else statement
    }# closing minor for loop
  }# closing major for loop
  
  # finding value of mean for the diagonal vector
  vector_mean <- mean(b)
  #finding value of median for the diagonal vector
  vector_median <- median(b)
  
  #creates vector with the mean and median in it
  mean_median <- c(vector_mean, vector_median)
  
  # return vector to global
  return(mean_median)
} #closing UDF

# matrix from exercise 7 in chapter 2
data_one <- c(10, 11, 9, 15, 19, 52, 19, 7, 10, 22, 28, 40, 6, 
              99, 33, 35, 26, 5, 87, 91, 0, 12, 16, 81, 200)
matrix_one <- matrix(data_one ,ncol=5 ,nrow=5)

# matrix from example in chapter 2.1
data_two <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
matrix_two <- matrix(data_two, ncol=3, nrow=3)

# testing the function on the two matrices
transformmatrix(x = matrix_one)
transformmatrix(x = matrix_two)

# matrix_one has a mean of 64.4 and a median of 19.0
# matrix_two has a mean of 5 and a median of 5

######################################
#Exercise 4.4 question 10
######################################

# creating the UDF
dataset_cleanup <- function(x, y){
  # initializing defined dataframe as new_dataset
  new_dataset <- x
  # beginning of for loop
  for (i in y) {
    # creating an empty vector
    z <- c()
    # creating a vector with the number of missing values
    number_of_missing <- length(which(is.na(new_dataset[ , i])))
    # adding the NA row indexes to empty vector
    z[1:number_of_missing] <- which(is.na(new_dataset[ , i]))
    # beginning of if statement
    if(is.null(z)){
      # if z is null, go to iteration
      next
    }else{
      # else remove all rows with index numbers in z from dataframe
      new_dataset <- new_dataset[-z, ]
    } #end of if/else loop
  } #end of for loop
  return(new_dataset)
}# closing the UDF

# creating the new dataframe
dataset <- airquality

# using the UDF on the dataframe
z<- dataset_cleanup(dataset, c(1:6))

# the user ran the code on all columns
# the dataset has no missing data after the function runs
# the new dataset has 111 observations
# the function succesfully removed 42 observations with missing values

# creating function to convert values to binary
binary_converter <- function(x){
  # for each value in dataset
  for (i in x) {
    # for each index in the length of dataset
    for (j in length(x)) {
      # creating empty vector called value
      value <- c()
      # if the value in dataset less than 0.5
      if (i < 0.5) {
        i <- 0
        value[j] <- i
      } # closing if statement
      else{
        i <- 1
        value[j] <- i
      } # closing else statement
    } # closing second for loop
  } # closing first for loop
}