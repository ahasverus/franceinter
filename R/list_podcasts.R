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
    
    c("tanguy-pastureau-maltraite-l-info",          "2017-08-28", NA,
      "Tanguy Pastureau", "Tanguy Pastureau maltraite l'info"),
    
    c("le-moment-meurice",                          "2015-02-09", NA,
      "Guillaume Meurice", "Le Moment Meurice"),
    
    c("la-chronique-de-waly-dia",                   "2020-10-05", NA,
      "Waly Dia", "La Chronique de Waly Dia"),
    
    c("la-chanson-de-frederic-fromet",              "2015-03-06", NA,
      "Frederic Fromet", "La Chanson de Frederic Fromet"),
    
    c("la-chronique-d-aymeric-lompret",             "2020-02-24", NA,
      "Aymeric Lompret", "La Chronique d'Aymeric Lompret"),
    c("la-chronique-de-constance",                  "2018-08-28", NA,
      "Constance", "La Chronique de Constance"),
    
    c("la-drole-d-humeur-de-guillermo-guiz",        "2016-09-02", NA,
      "Guillermo Guiz", "La Drole d'humeur de Guillermo Guiz"),
    
    c("la-chronique-de-djamil-le-shlag",            "2021-08-30", NA,
      "Djamil Le Shlag", "La Chronique de Djamil Le Shlag"),
    
    c("sur-les-epaules-de-darwin",                  "2011-02-05", "2020-03-14",
     "Jean-Claude Ameisen", "Sur les Epaules de Darwin"),
    
    c("ca-peut-pas-faire-de-mal",                   "2011-06-25", "2020-02-08",
     "Guillaume Gallienne", "Ca peut pas faire de mal"),
    
    c("la-drole-d-humeur-de-pierre-emmanuel-barre", "2015-01-06", "2017-04-19",
     "Pierre Emmanuel Barre", "La Drole d'humeur de Pierre Emmanuel Barre")
    
  ), ncol = 5, byrow = TRUE))

  colnames(podcasts) <- c("label", "start_date", "end_date", "artist",
                          "podcast")

  return(podcasts)
}
