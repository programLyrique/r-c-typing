#' Add two integers using .C convention
#' @export
add_ints <- function(x, y) {
  result <- integer(1)
  .C(testpkg_add_ints, as.integer(x), as.integer(y), result = result)$result
}

#' Compute the sum of a double vector using .C convention
#' @export
sum_doubles <- function(x) {
  n <- length(x)
  result <- double(1)
  .C(testpkg_sum_doubles, as.double(x), as.integer(n), result = result)$result
}

#' Create a list from two values using .Call convention
#' @export
make_pair <- function(x, y) {
  .Call(testpkg_make_pair, x, y)
}

#' Get the type of an R value using .Call convention
#' @export
get_type <- function(x) {
  .Call(testpkg_get_type, x)
}

#' Reverse a character vector using .Call convention
#' @export
reverse_strings <- function(x) {
  .Call(testpkg_reverse_strings, x)
}
