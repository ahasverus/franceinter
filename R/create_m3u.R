#' Create a m3u file from a list of URL
#'
#' @description
#' Creates a m3u (MPEG version 3.0 URL) file from a list of URLs. This file can
#' be read by any multimedia reader (e.g. VLC). These audio/video URLs will be
#' streamed (not downloaded).
#' For more information: \url{https://en.wikipedia.org/wiki/M3U}
#'
#' @param data a `data.frame` created by `get_metadata()`.
#' 
#' @param podcast a character of length 1. The name of the podcast.
#' 
#' @param path a character of length 1. The folder to save the M3U file.
#'
#' @return The content of the M3U file (optional).
#'
#' @export
#' 
#' @seealso get_metadata
#'
#' @examples
#' \dontrun{
#' ## Retrieve episodes metadata ----
#' 
#' tab <- get_metadata("un-ete-avec-homere", radio = "franceinter")
#'
#'
#' ## Create a m3u playlist ----
#' 
#' x <- create_m3u(tab, "un-ete-avec-homere")
#' cat(paste0(x, collapse = "\n"))
#' }

create_m3u <- function(data, podcast, path = ".") {


  ## Check Inputs ----

  check_arg_path(path)
  check_arg_podcast(podcast)
  check_arg_data(data)

  
  ## Create IDs ----
  
  data <- data[order(data$"date", decreasing = FALSE), ]
  data$"id" <- gsub("\\s", "0", format(1:nrow(data)))
  data <- data[order(data$"date", decreasing = TRUE), ]

  
  ## Write M3U Expression ----

  content <- "#EXTM3U"

  for (i in 1:nrow(data)) {

    meta <- paste("\n#EXTINF:",
                  paste0(data$"duration"[i], ","),
                  data$"id"[i],
                  gsub(",", "", data$"title"[i]))

    content <- c(content, meta, data$"file_url"[i])
  }


  ## Export M3U File ----

  cat(paste0(c(content, ""), collapse = "\n"),
      file = file.path(path, "m3u", paste0(tolower(podcast), ".m3u")))

  messages::msg_done("Writing", 
                     messages::msg_value(file.path(path, "m3u",
                                                   paste0(podcast, ".m3u"))), 
                                         "file")

  invisible(content)
}
