#' wrapit function to turn long vectors into a data frame
#' @param x is the long vector
#' @param n is the number of finished data frame columns
#' @return a data frame
#' @export
#' @examples
#' x <- c("John","21","2084033117","4/17/2020","Mary","22","2082061476","6/20/2021")
#' n <- 4
#' my_df <- wrapit(x, n)

wrapit <- function(x, n){
  numrow <- length(x) / n
  temp_df <- data.frame(matrix(NA, nrow = 0, ncol = n))
  for(k in c(1:numrow)){
    which_rows <- c(1:n)
    temp_df[k, ] <- x[which_rows + (n*(k-1))]
  }
  return(temp_df)
}
