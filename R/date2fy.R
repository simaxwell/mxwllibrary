#' Convert date to fiscal year
#'
#' Convert a date to fiscal or financial year. UK fiscal year runs from 1 April to 31 March.
#'
#' @param date A Date or POSIXct vector
#' @param fye A logical. If TRUE returns fiscal year ending
#' @param separator A character. Symbol to separate fiscal year starting and ending (forward slash by default)
#'
#' @return fiscal year (character by default. Numeric if fye is TRUE).
#' @export
#'
#' @author Simon Maxwell
#'
#' @importFrom cli cli_abort
#'
#' @examples
#' date2fy(as.Date("2024-05-07"))
date2fy <- function(date, fye = FALSE, separator = "/") {

  # Stop if `date` is not of Date class:
  if (!inherits(date, c("Date", "POSIXct"))) {

    cli::cli_abort("{.arg date} must be a {.cls Date} or {.cls POSIXct} vector and not a {.cls {class(date)}}.")

  }

  # Stop if `fye` is not a logical:
  if (!is.logical(fye)) {

    cli::cli_abort("{.arg fye} must be a {.cls logical} vector and not a {.cls {class(fye)}}.")

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

  # Get years in fiscal season (1 April to 31 March):
  first_year <- ifelse(month >= 4, year_l, year_l - 1)
  second_year <- ifelse(month >= 4, year_s + 1, year_s)

  # If second year is single digit then add leading zero:
  second_year <- ifelse(nchar(second_year) == 1,
                        paste0("0", second_year, sep = ""),
                        second_year)

  # Create fiscal year ending:
  if (fye == TRUE) {

    fy <- as.numeric(paste0(substr(first_year, 1, 2), second_year, sep = ""))

  }

  # Create fiscal year:
  if (fye == FALSE) {

    fy <- paste0(first_year, separator, second_year)

  }

  # Return fiscal year:
  return(fy)

}
