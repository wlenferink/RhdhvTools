#' Correct values based on limit symbols
#'
#' The `correct_limit_values` function finds symbols such as "<" or "<=" in the `values` column, or a column of choice
#' to correct values. By default, the function looks for lesser than symbols and will divide the corresponding value
#' by 2.
#'
#' @param data A dataframe.
#' @param values The column containing values to correct.
#' @param new_values The column name for corrected values.
#' @param limits OPTIONAL. The column name containing symbols. If this argument is left empty, the function will find
#' lesser than symbols within the `values` column.
#' @param under A vector containing the symbol(s) for lesser than. Defaults to "<" and "<=".
#' @param over A vector containing the symbol(s) for greater than. Defaults to ">" and ">=". Functionality for 'greater
#' than' symbols has not been implemented yet.
#'
#' @return A dataframe with the corrected column.
#' @export
#'
#' @examples
#' # Sample dataframe
#' df <- data.frame(
#'   values = c("10", "<20", "30", "<40", "50")
#' )
#'
#' # Applying the function
#' corrected_df <- correct_limit_values(data = df, values = "values", new_values = "corrected_values")
#'
#' # Display the result
#' print(corrected_df)
correct_limit_values <- function(data,
                                 values,
                                 new_values,
                                 limits = NULL,
                                 under = c("<", "<="),
                                 over = c(">", ">=")){

  # Determine the symbol of 'values' from a column defined by 'limits'
  if (!is.null(limits)) {
    data_corrected <- data %>%
      mutate({{new_values}} := case_when({{limits}} == under ~ {{values}} / 2,
                                         .default = {{values}}))
  }

  # Determine the symbol of 'values' from within the values column
  # This is WRONG and does not take into account {{over}} and {{under}}
  if (is.null(limits)) {
    data_corrected <- data %>%
      mutate(limits99 = str_extract(.[[values]], "(>|<|=)+"),
             truevalue99 = parse_number(.[[values]]),
             {{new_values}} := case_when(limits99 %in% under ~ truevalue99 / 2,
                                         .default = truevalue99)) %>%
      select(-limits99, -truevalue99)
  }

  return(data_corrected)

}
