#' simplify table builder headers
#'
#' @param x data.frame
#'
#' @return data.frame
#' @export
#' @importFrom stringr word
tb_simplify_names <- function(x) {
  assertthat::assert_that(is.data.frame(x))
  simplified_names <- names(x) %>%
    stringr::word(1) %>%
    tolower()
  names(x) <- simplified_names
  return(x)
}

#' remove rows with totals
#'
#' @param x data.frame
#'
#' @return data.frame
#' @export
tb_remove_totals <- function(x) {
  assertthat::assert_that(is.data.frame(x))
  if (!is.data.table(x)) {
    x <- as.data.table(x)
  }
  x <- x[rowSums(x == "Total") == 0]
  return(x)
}
