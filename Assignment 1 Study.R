data(mtcars)
mtcars

x <- c(1, 3, 7, 5, 2)
x
compute_stats <- function(x){
  mean_of_vector <- mean(x)
  median_of_vector <- median(x)
  variance_of_vecor <- sum(x[1:length(x)] - mean(x))/(length(x)-1)
  IQR_of_vector <- IQR(x)
  minimum_of_vector <- min(x)
  maximum_of_vector <- max(x)
  print(mean_of_vector)
  print(median_of_vector)
  print(variance_of_vecor)
  print(IQR_of_vector)
  print(minimum_of_vector)
  print(maximum_of_vector)
}
compute_stats(x)

