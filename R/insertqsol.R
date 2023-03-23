#' Insert question and solution.
#'
#' Call this function as an addin to insert q and sol code at the cursor position.
#' Can add as a keyboard shortcut (I use ctrl + shift + q)
#' @export
insertqsol <- function() {
  rstudioapi::insertText("`r qbegin()`

`r qend()`
`r solbegin(show=params$SHOW_SOLS, toggle=params$TOGGLE)`

`r solend()`
")
}
