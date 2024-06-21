#' Check if letters in word are in alphabetical order
#'
#' Consecutive identical letters are treated as maintaining alphabetical order.
#'
#' @param word A word.
#'
#' @return A logical.
#' @importFrom cli cli_abort
#'
#' @export
#'
#' @author Simon Maxwell
#'
#' @importFrom cli cli_abort
#'
#' @examples
#' is_alphabetical("aegilops")
is_alphabetical <- function(word) {

  # Stop if `word` is not a character vector:
  if (!is.character(word)) {

    cli::cli_abort(c(
      "i" = "{.var word} must be a character vector",
      "x" = "Not a {.cls {class(word)}} vector."
    ))

  }

  # Convert `word` to lower case:
  word <- tolower(word)

  # Remove whitespace?
  # Check if more than one word supplied?

  # Split `word` into each individual letter:
  word_split <- strsplit(x = word, split = "")[[1]]

  # Get index of matching letters in the alphabet:
  index <- match(word_split, letters)

  # Calculate difference between each index number:
  diffs <- diff(index)

  # Minuend will always be greater than subtrahend for words with letters in alphabetical order. Every difference will therefore be positive:
  if (all(diffs >= 0)) {

    alphabetical_lgl <- TRUE

  } else {

    alphabetical_lgl <- FALSE

  }

  # Return logical output:
  return(alphabetical_lgl)

}
