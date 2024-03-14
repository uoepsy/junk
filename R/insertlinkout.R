#' Insert linkout
#'
#' Call this function as an addin to insert blank link at the cursor position.

#' Can add as a keyboard shortcut
#' @export
insertlinkout <- function() {
  rstudioapi::insertText('[](){target="_blank"}')
}
