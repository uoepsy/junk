#' Insert callout
#'
#' Call this function as an addin to insert callout code at the cursor position.

#' Can add as a keyboard shortcut (I use ctrl + shift + q)
#' @export
insertcallout <- function() {
  rstudioapi::insertText('
::: {.callout-tip collapse="true"}
#### Hints


:::
')
}
