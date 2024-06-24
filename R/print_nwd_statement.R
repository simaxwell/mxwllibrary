#' Print non-working day statement for work email signature
#'
#' @description
#' I work a 9-day fortnight with every second Friday off.
#'
#' @param block_start Date. The date the two-week block started (must always be a Monday).
#' @param n numeric. The number of Fridays to print.
#'
#' @author Simon Maxwell
#'
#' @return character
#' @importFrom glue glue_collapse
#' @importFrom lubridate wday
#'
#' @export
#'
#' @examples
#' print_nwd_statement(as.Date("2024-06-17"), 3)
print_nwd_statement <- function(block_start, n) {

  # Check block_start is a Date:
  if (!inherits(block_start, c("Date", "POSIXct"))) {
    stop("'block_start' is not a Date.",
         call. = FALSE)
  }

  # Check block_start is a Monday:
  if (lubridate::wday(block_start, week_start = 1) != 1) {
    stop("'block_start' must be a Monday.",
         call. = FALSE)
  }

  # Check n is a numeric:
  if (!inherits(n, "numeric")) {
    stop("'n' is not a numeric.",
         call. = FALSE)
  }

  # Get the date of the first Friday:
  friday <- block_start + 11

  # Create sequence of n fortnights from first non-working day:
  date_vec <- seq.Date(from = friday,
                       by = "14 days",
                       length.out = n)

  # Convert to more readable date format:
  date_vec <- trimws(format(date_vec, "%e %B"))

  # Collapse:
  date_vec <- glue::glue_collapse(date_vec, sep = ", ", last = " and ")

  # Combine:
  statement <- paste0("I work a 9-day fortnight. My next non-working days are: ", date_vec, ".")

  # Return email signature:
  print(statement)

}
