#' @title radiofrance: Get Radio France Podcasts
#'
#' @description
#' Retrieves Radio France (France Inter, France Culture, etc.) podcasts 
#' metadata (date, title, mp3 url, and duration). User can easily download 
#' metadata for any podcast. The name of the podcast must be extracted from the 
#' website URL (e.g. Le Moment Meurice must be written as le-moment-meurice).
#'
#' @author Nicolas Casajus \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 2022/06/22



## Install dependencies (listed in DESCRIPTION) ----

# install.packages(c("here", "pkgload", "remotes"))
remotes::install_deps(upgrade = "never")


## Load project ----

pkgload::load_all(here::here())


## Path to save results ----

path <- here::here("inst")


## Change system locale ----

locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")


## List podcast names ----

podcasts <- c("tanguy-pastureau-maltraite-l-info", 
              "le-moment-meurice", 
              "la-chronique-de-waly-dia", 
              "la-chanson-de-frederic-fromet", 
              "la-chronique-d-aymeric-lompret", 
              "la-drole-d-humeur-de-guillermo-guiz", 
              "le-journal-de-presque-17h17",
              "la-chronique-de-djamil-le-shlag",
              "la-methode-scientifique",
              "carbone-14-le-magazine-de-l-archeologie",
              "fictions-samedi-noir",
              "le-billet-de-matthieu-noel",
              "la-chronique-de-doully",
              "quand-les-dieux-rodaient-sur-la-terre"
             )

radios   <- c("franceinter",
              "franceinter",
              "franceinter",
              "franceinter",
              "franceinter",
              "franceinter",
              "franceinter",
              "franceinter",
              "franceculture",
              "franceculture",
              "franceculture",
              "franceinter",
              "franceinter",
              "franceinter"
             )

for (i in 1:length(podcasts)) {

  cat("\n*** ", podcasts[i], " ***\n")


  ## Retrieve Metadata ----

  get_metadata(podcasts[i], radio = radios[i], path, na_rm = TRUE)


  ## Create M3U Playlist ----

  tab <- read.csv2(file.path(path, "csv", paste0(podcasts[i], ".csv")))

  create_m3u(tab, podcasts[i], path)
}


## Restore system locale ----

Sys.setlocale("LC_TIME", locale)
