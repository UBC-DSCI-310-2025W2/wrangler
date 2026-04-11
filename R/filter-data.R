#' Filters out all rows with a numeric or character value of 0 in any column from a given data frame.
#'
#' @param data A data frame with at least one row.
#'
#' @return A data frame where all rows with a numeric or character value of 0 in any column are filtered out.
#' If there are no values of 0, original data frame is returned.
#' Indexes (row names) will contain the same values as the original data frame.
#' Rows containing the string "zero" spelled out will not be filtered.
#'
#' @examples
#' filter_data(data.frame(a = c(0, 0, 1, 2),
#'                        b = c(1, 2, 3, 4),
#'                        c = c("one", "two", "three", "four")))
#'
#' @export

filter_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("Error: Input must be a data frame")
  }
  if (nrow(data) == 0) {
    stop("Error: Data frame must have at least one row")
  }
  if (is.data.frame(data)) {
    filtered_df <- data[apply(data!=0, 1, all), , drop = FALSE] # drop = FALSE to make sure output stays as a data frame
    return(filtered_df)
  }
}

