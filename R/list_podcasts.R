#' List of France Inter Podcasts
#' 
#' @description 
#' Lists a selection of France Inter podcasts.
#' 
#' @return A three-columns data frame with:
#'   - `podcast`: the label of the podcast;
#'   - `start_date`: the date of the first episode.
#'   - `end_date`: the date of the last episode (can be `NA`).
#' 
#' @export
#'
#' @examples
#' list_podcasts()

list_podcasts <- function() {
  
  podcasts <- as.data.frame(matrix(c(
    c("tanguy-pastureau-maltraite-l-info", "2017-08-28", NA),
    c("le-moment-meurice",                 "2015-02-09", NA),
    c("la-chronique-de-waly-dia",          "2020-10-05", NA),
    c("sur-les-epaules-de-darwin",         "2011-02-05", "2020-03-14"),
    c("ca-peut-pas-faire-de-mal",          "2011-06-25", "2020-02-08")
  ), ncol = 3, byrow = TRUE))
  
  colnames(podcasts) <- c("podcast", "start_date", "end_date")
  
  return(podcasts)
}
