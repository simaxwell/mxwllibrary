#' Convert date to planting year
#'
#' Convert a date to planting year or season. UK tree planting season runs from 1 October to 30 September.
#'
#' @param date A Date or POSIXct vector
#' @param pye A logical. If TRUE returns planting year ending
#' @param separator A character. Symbol to separate planting year starting and ending (forward slash by default)
#'
#' @return Planting year (character by default. Numeric if pye is TRUE).
#' @export
#'
#' @author Simon Maxwell
#'
#' @importFrom cli cli_abort
#'
#' @examples
#' date2py(as.Date("2024-05-07"))
date2py <- function(date, pye = FALSE, separator = "/") {

  # Stop if `date` is not of Date class:
  if (!inherits(date, c("Date", "POSIXct"))) {

    cli::cli_abort("{.arg date} must be a {.cls Date} or {.cls POSIXct} vector and not a {.cls {class(date)}}.")

  }

  # Stop if `pye` is not a logical:
  if (!is.logical(pye)) {

    cli::cli_abort("{.arg pye} must be a {.cls logical} vector and not a {.cls {class(pye)}}.")

  }

  # Stop if `separator` is not a character:
  if (!is.character(separator)) {

    cli::cli_abort("{.arg separator} must be a {.cls character} vector and not a {.cls {class(separator)}}.")

  }

  # Get month number:
  month <- as.numeric(format(date, "%m"))

  # Get 4-digit year:
  year_l <- as.numeric(format(date, "%Y"))

  # Get 2-digit year:
  year_s <- as.numeric(format(date, "%y"))

  # Get years in planting season (1 October to 30 September):
  first_year <- ifelse(month >= 9, year_l, year_l - 1)
  second_year <- ifelse(month >= 9, year_s + 1, year_s)

  # If second year is single digit then add leading zero:
  second_year <- ifelse(nchar(second_year) == 1,
                        paste0("0", second_year, sep = ""),
                        second_year)

  # Create planting year ending:
  if (pye == TRUE) {

    py <- as.numeric(paste0(substr(first_year, 1, 2), second_year, sep = ""))

  }

  # Create planting year:
  if (pye == FALSE) {

    py <- paste0(first_year, separator, second_year)

  }

  # Return planting year:
  return(py)

}
