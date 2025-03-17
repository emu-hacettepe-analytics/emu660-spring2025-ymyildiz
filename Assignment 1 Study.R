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
