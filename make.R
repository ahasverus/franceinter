#' @title franceinter: A Research Compendium
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#'
#' @date 2021/05/07



## Install Dependencies (listed in DESCRIPTION) ----

if (!("remotes" %in% utils::installed.packages()[ , 1])) {
  install.packages("remotes")
}

remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

pkgload::load_all(here::here())


## Path to Save Results ----

path <- here::here("inst")


## System locale ----

locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")


# path_mp3 <- file.path("", "Users", "nicolascasajus", "Nextcloud", "Podcasts")
# cover    <- file.path(path_mp3, "sur-les-epaules-de-darwin.jpg")


## Get Podcast Info ----

podcasts <- c("tanguy-pastureau-maltraite-l-info", "le-moment-meurice", 
              "la-chronique-de-waly-dia", "la-chanson-de-frederic-fromet", 
              "la-chronique-d-aymeric-lompret", "la-chronique-de-constance", 
              "la-drole-d-humeur-de-guillermo-guiz", 
              "la-chronique-de-djamil-le-shlag")

for (i in 1:length(podcasts)) {

  podcast  <- podcasts[i]

  cat("\n*** ", podcast, " ***\n")


  ## Retrieve Metadata ----

  franceinter::get_metadata(podcast, path, na_rm = TRUE)


  ## Create M3U Playlist ----

  tab <- read.csv2(file.path(path, "csv", paste0(podcast, ".csv")))

  franceinter::add_m3u(tab, podcast, path)


  ## Download mp3 ----

  # franceinter::get_mp3(data    = tab,
  #                      podcast = podcast$"podcast",
  #                      path    = path_mp3)


  ## Add Cover ----

  # franceinter::add_cover(path   = file.path(path_mp3, podcast$"podcast"),
  #                        cover  = cover,
  #                        album  = podcast$"podcast",
  #                        artist = podcast$"artist",
  #                        data   = tab)
}


## Restore locale ----

Sys.setlocale("LC_TIME", locale)
