#' Insert columns
#'
#' Call this function as an addin to insert 2 column code at the cursor position.

#' Can add as a keyboard shortcut (I use ctrl + shift + q or e)
#' @export
insertcolumns <- function() {
  rstudioapi::insertText('
::::{.columns}
:::{.column width="50%"}

:::

:::{.column width="50%"}

:::
::::
')
}
