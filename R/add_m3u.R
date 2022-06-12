#' Create a M3U File from a list of URL
#'
#' @description
#' Creates a M3U (MPEG version 3.0 URL) file from a list of URLs. This file can
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
#' @param na_rm a logical. If `TRUE` (default) remove episodes with incomplete
#'   information.
#'
#' @return The content of the M3U file (optional).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Create a folder to store results ----
#' 
#' path <- "Podcasts"
#' dir.create(path)
#' 
#' 
#' ## Retrieve episodes information ----
#' 
#' tab <- get_metadata("un-ete-avec-homere", path)
#'
#'
#' ## Create a M3U playlist ----
#' 
#' x <- add_m3u(tab, "un-ete-avec-homere", path)
#'
#' cat(paste0(x, collapse = "\n"))
#' }

add_m3u <- function(data, podcast, path = ".", na_rm = TRUE) {


  ## Check Inputs ----

  if (missing(data))          stop("Argument 'data' is required.")
  if (missing(podcast))       stop("Argument 'podcast' is required.")

  if (is.null(data))          stop("Argument 'data' is required.")
  if (is.null(podcast))       stop("Argument 'podcast' is required.")

  if (!is.data.frame(data))   stop("Argument 'data' must be a data frame.")
  if (!is.character(podcast)) stop("Argument 'podcast' must be a character.")
  if (!is.logical(na_rm))     stop("Argument 'na_rm' must be a boolean.")

  if (sum(c("date", "title", "duration", "file_url") %in% colnames(data)) != 4)
    stop("Argument 'data' must contain the following variables: 'date', ",
         "'title', 'duration', and 'file_url'.")

  if (!dir.exists(path)) stop("The path <", path, "> does not exist.")

  if (na_rm) {
    data <- data[!is.na(data$"date"), ]
    data <- data[!is.na(data$"title"), ]
    data <- data[!is.na(data$"duration"), ]
    data <- data[!is.na(data$"file_url"), ]
  }

  # data <- data[nrow(data):1, ]


  ## Write M3U Expression ----

  content <- "#EXTM3U"

  ids <- gsub("\\s", "0", format(1:nrow(data)))

  for (i in 1:nrow(data)) {

    meta <- paste("\n#EXTINF:",
                  paste0(data$"duration"[i], ","),
                  ids[i],
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
