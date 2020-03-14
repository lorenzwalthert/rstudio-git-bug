
#' Check whether a parse table contains a tilde
#'
#'
#' @param pd A parse table.
#' @param tilde_pos Integer vector indicating row-indices that should be
#'   checked for tilde. See 'Details'.
#'
#' @details
#' A tilde is on the top row in the parse table if it is an asymmetric tilde
#' expression (like `~column`), in the second row if it is a symmetric tilde
#' expression (like `a~b`).
#' @keywords internal
is_tilde_expr <- function(pd, tilde_pos = c(1, 2)) {
  if (is.null(pd) || nrow(pd) == 1) {
    return(FALSE)
  }
  any(pd$token[tilde_pos] == "'~'")
}

#' @rdname is_tilde_expr
is_asymmetric_tilde_expr <- function(pd) {
  is_tilde_expr(pd, tilde_pos = 1)
}

#' @rdname is_tilde_expr
is_symmetric_tilde_expr <- function(pd) {
  is_tilde_expr(pd, tilde_pos = 2)
}

is_subset_expr <- function(pd) {
  if (is.null(pd) || nrow(pd) == 1) {
    return(FALSE)
  }
  pd$token[2] == "'['"
}


#' Identify comments that are shebangs
#'
#' Shebangs should be preserved and no space should be inserted between
#' \# and !. A comment is a shebang if it is the first top level token
#' (identified with `pos_id`) and if it starts with `#!`.
#' @param pd A parse table.
#' @examples
#' style_text("#!/usr/bin/env Rscript")
#' @keywords internal
is_shebang <- function(pd) {
  is_first_comment <- is_comment(pd) & (pd$pos_id == 1L)
  is_first_comment[is_first_comment] <- grepl(
    "^#!", pd$text[is_first_comment],
    perl = TRUE
  )
  is_first_comment
}

#' Identify spinning code chunk header
#'
#' See https://yihui.name/knitr/demo/stitch/#spin-comment-out-texts for details.
#' @examples
#' style_text(c(
#'   "# title",
#'   "some_code <- function() {}",
#'   "#+ chunk-label, opt1=value1",
#'   "call(3, 2, c(3:2))"
#' ))
#' @param pd A parse table.
#' @keywords internal
is_code_chunk_header <- function(pd) {
  is_comment <- is_comment(pd)
  is_comment[is_comment] <- grepl(
    "^#[\\+|\\-]", pd$text[is_comment],
    perl = TRUE
  )
  is_comment
}
