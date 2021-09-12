#' Create a M3U File from a list of URL
#'
#' @description
#' Creates a M3U (MPEG version 3.0 URL) file from a list of URLs. This file can
#' be read by any multimedia reader (e.g. VLC). These audio/video URLs will be
#' streamed (not downloaded).
#' For more information: \url{https://en.wikipedia.org/wiki/M3U}
#'
#' @param data a data frame created by `get_metadata()`.
#' @param path the path to save the M3U file (must exist).
#' @param na_rm a logical. If `TRUE` (default) remove episodes with incomplete
#'   information.
#'
#' @return The content of the M3U file (optional).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Create a M3U playlist ----
#' x <- all_m3u(path)
#'
#' cat(paste0(x, collapse = "\n"))
#' }

all_m3u <- function(path = ".", na_rm = TRUE) {
  
  
  ## Check Inputs ----
  
  if (!is.logical(na_rm))     stop("Argument 'na_rm' must be a boolean.")
  
  if (sum(c("date", "title", "duration", "file_url") %in% colnames(data)) != 4)
    stop("Argument 'data' must contain the following variables: 'date', ",
         "'title', 'duration', and 'file_url'.")
  
  if (!dir.exists(path)) stop("The path <", path, "> does not exist.")
  
  
  ## Read metadata ----
  
  csvs <- list.files(path, pattern = "\\.csv$")
  csvs <- csvs[-which(csvs %in% c("sur-les-epaules-de-darwin.csv", 
                                  "ca-peut-pas-faire-de-mal.csv"))]
  
  data <- lapply(csvs, function(x) read.csv2(file.path(path, x)))
  data <- do.call(rbind, data)
  
  
  if (na_rm) {
    data <- data[!is.na(data$"date"), ]
    data <- data[!is.na(data$"title"), ]
    data <- data[!is.na(data$"duration"), ]
    data <- data[!is.na(data$"file_url"), ]
  }
  
  data <- data[sample(1:nrow(data), size = nrow(data), replace = FALSE), ]
  
  
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
      file = file.path(path, "all-podcasts.m3u"))
  
  usethis::ui_done(paste0("Writing ",
                          "{usethis::ui_value(file.path(path, 
                            \"all-podcasts.m3u\"))} file"))
  
  invisible(content)
}
