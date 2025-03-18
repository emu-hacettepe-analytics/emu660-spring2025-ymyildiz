
# (b) section of Assignment 1

data(mtcars)
mtcars

# The function returning a named list containing the mean, median, variance, IQR, minimum, and maximum of the input

compute_stats <- function(x){
  if (!is.numeric(x)){
    stop("Input is not numeric vector.") #We make sure that the function takes numeric vector as input.
  }
  
  statistics_of_data <- list(
  mean =  mean(x),
  median =  median(x),
  variance = var(x),
  IQR = IQR(x),
  min = min(x),
  max =  max(x)
  )
  
  statistics_of_data
}

for (column_name in names(mtcars)){
  if(is.numeric(mtcars[[column_name]])){
    computed_statistics <- compute_stats(mtcars[[column_name]])
    cat("\nStatistics for Column:", column_name, "\n")
    print(computed_statistics)
    cat("\n---------------------\n")
  }
}


numeric_stats <- sapply(mtcars[sapply(mtcars, is.numeric)], compute_stats)
numeric_stats

matrix_of_mtcars <- as.matrix(mtcars[sapply(mtcars, is.numeric)])
matrix_of_statistics <- apply(matrix_of_mtcars, MARGIN = 2, compute_stats)
matrix_of_statistics

# (c) section of Assignment 1

library(dslabs)
data(na_example)
na_example

sum(is.na(na_example)) # Count of NA values found within the dataset
which(is.na(na_example)) # Index position of NA values in the dataset

mean_of_naexample <- mean(na_example, na.rm = TRUE) # Mean of the dataset
std_dev_of_naexample <- sqrt(var(na_example, na.rm = TRUE)) # Standard deviation of the dataset

mean_of_naexample
std_dev_of_naexample


nonmissing_naexample <- na_example[!is.na(na_example)] # Let us remove the NA's to find the median of the non-missing values.
nonmissing_naexample
median_of_nonmissing <- median(nonmissing_naexample)

# Version 1 where all NA values are replaced with the median of the non-missing values
na_example_Version1 <- ifelse(is.na(na_example), median_of_nonmissing, na_example)
na_example_Version1

# Version 2 where all NA values are replaced with the a randomly selected non-missing value
set.seed(123)  # Set seed for reproducibility
na_example_Version2 <- na_example
na_example_Version2[is.na(na_example_Version2)] <- sample(nonmissing_naexample, sum(is.na(na_example_Version2)), replace = TRUE)

na_example_Version2


head(na_example_filled)




