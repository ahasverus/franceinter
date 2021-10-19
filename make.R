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

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all()


## Path(s) to Save Results ----

path <- here::here("inst")


## System locale ----

locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# path_mp3 <- file.path("", "Users", "nicolascasajus", "Nextcloud", "Podcasts")
# cover    <- file.path(path_mp3, "sur-les-epaules-de-darwin.jpg")


## Get Podcast Info ----

podcasts <- franceinter::list_podcasts()

for (i in 1:nrow(podcasts)) {

  podcast  <- podcasts[i, ]

  cat("\n*** ", podcast$"podcast", " ***\n")


  ## Retrieve Metadata ----

  franceinter::get_metadata(podcast    = podcast$"label",
                            start_date = podcast$"start_date",
                            end_date   = podcast$"end_date",
                            path       = path,
                            na_rm      = TRUE)


  ## Create Playlist ----

  tab <- read.csv2(file.path(path, "csv", paste0(podcast$"label", ".csv")))

  franceinter::add_m3u(data    = tab,
                       podcast = podcast$"label",
                       path    = path)


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


## Create MEGA M3U file ----

franceinter::all_m3u(path)


## Restore locale ----

Sys.setlocale("LC_TIME", locale)

print(Sys.time())

