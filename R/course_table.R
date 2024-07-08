#' Creates html table of course contents with trailing opacity set by week n
#' @param b1 character string for name of block 1
#' @param b2 character string for name of block 2
#' @param b1list vector (5L) of character strings of lecture titles for block 1
#' @param b2list vector (5L) of character strings of lecture titles for block 2
#' @param week week (1 to 10) of semester up to which opacity = 1
#' @export
#' @examples
#' block1_name = "multilevel modelling<br>working with group structured data"
#' block1_lecs = c("regression refresher","introducing multilevel models","more multilevel models","more complex groupings","recap")
#' block2_name = "factor analysis<br>working with multi-item measures"
#' block2_lecs = c("PCA","EFA","EFA 2","CFA","CFA 2")
#'
#' # put this line in a slide chunk and use results = 'asis'
#' # make two column layout for full year courses
#' course_table(block1_name, block2_name, block1_lecs, block2_lecs, week=4)
course_table <- function(b1,b2,b1list,b2list,week){
  leclist = c(b1list, b2list)
  leclist[week] = paste0("<b>",leclist[week],"</b>")
  glue::glue('
  <table style="border: 1px solid black;>
    <tr style="padding: 0 1em 0 1em;">
      <td rowspan="5" style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[1]};text-align:center">
          {b1}</td>
      <td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[1]}">
          {leclist[1]}</td>
    </tr>
    <tr><td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[2]}">
          {leclist[2]}</td></tr>
    <tr><td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[3]}">
          {leclist[3]}</td></tr>
    <tr><td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[4]}">
          {leclist[4]}</td></tr>
    <tr><td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[5]}">
          {leclist[5]}</td></tr>

    <tr style="padding: 0 1em 0 1em;">
      <td rowspan="5" style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[6]};text-align:center">
          {b2}</td>
      <td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[6]}">
          {leclist[6]}</td>
    </tr>
    <tr><td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[7]}">
          {leclist[7]}</td></tr>
    <tr><td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[8]}">
          {leclist[8]}</td></tr>
    <tr><td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[9]}">
          {leclist[9]}</td></tr>
    <tr><td style="border: 1px solid black;padding: 0 1em 0 1em;opacity:{oplist[10]}">
          {leclist[10]}</td></tr>
  </table>
  ',
  b1=paste0("<b>",b1,"</b>"),
  b2=paste0("<b>",b2,"</b>"),
  leclist=leclist,
  oplist=c(rep(1,week),rep(0.4,10-week))
  ) |> cat()
}
