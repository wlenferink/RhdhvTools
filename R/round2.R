#' Round numbers
#'
#' @param x A number.
#' @param digits Number of digits to round to.
#'
#' @return A rounded number
#' @export
#'
#' @examples
#' # Example usage
#' number <- 3.14159
#' rounded_number <- round2(number, 2)
#'
#' # Display the result
#' print(rounded_number)
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}
