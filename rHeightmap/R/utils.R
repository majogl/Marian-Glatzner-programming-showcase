#' Stack functionality
#'
#' Push operation on a vector.
#'
#' @param x A vector to push an item into
#' @param value A value to push into the stack (at the end of the vector)

push <- function(x, value) {
  (assign(as.character(substitute(x)), c(x, value), parent.frame()))
}

#' Stack functionality
#'
#' Pop operation on a vector. Edits input vector and returns the item that was pushed in most recently
#'
#' @param x A vector to pop an item out of
#' @return The last item of vector x

pop <- function(x) {
  temp <- x[length(x)]
  assign(as.character(substitute(x)), x[-length(x)], parent.frame())
  return(temp)
}
