
#' @describeIn pd_is Checks whether `pd` contains an expression wrapped in
#'   curly brackets.
#' @keywords internal
is_curly_expr <- function(pd) {
  if (is.null(pd)) {
    return(FALSE)
  }
  pd$token[1] == "'{'"
}

is_for_expr <- function(pd) {
  pd$token[1] == "FOR"
}

#' @describeIn pd_is Checks whether `pd` contains is a conditional expression.
#' @keywords internal
is_cond_expr <- function(pd) {
  pd$token[1] == "IF"
}

#' @describeIn pd_is Checks whether `pd` contains is a while loop.
#' @keywords internal
is_while_expr <- function(pd) {
  pd$token[1] == "WHILE"
}

#' @describeIn pd_is Checks whether `pd` is a function call.
#' @keywords internal
is_function_call <- function(pd) {
  if (is.null(pd)) {
    return(FALSE)
  }
  if (is.na(pd$token_before[2])) {
    return(FALSE)
  }
  pd$token_before[2] == "SYMBOL_FUNCTION_CALL"
}

#' @describeIn pd_is Checks whether `pd` is a function declaration.
#' @keywords internal
is_function_dec <- function(pd) {
  if (is.null(pd)) {
    return(FALSE)
  }
  pd$token[1] == "FUNCTION"
}

#' @describeIn pd_is Checks for every token whether or not it is a comment.
#' @keywords internal
is_comment <- function(pd) {
  if (is.null(pd)) {
    return(FALSE)
  }
  pd$token == "COMMENT"
}
