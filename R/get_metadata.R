#' Retrieve podcast metadata
#'
#' @description 
#' Retrieves podcast metadata (i.e. date, title, mp3 duration and URL).
#' Information will be saved in a CSV file. Each time this function is called
#' only new episodes will be added.
#' 
#' @param podcast a character of length 1. The name of the podcast.
#' 
#' @param radio a character of length 1. The name of the radio. Must one among
#'   `franceinter` (default), `franceinfo`, `francebleu`, `franceculture`,
#'   `francemusique`, `fip`, or `mouv`.
#'   
#' @param path a character of length 1. The folder to save metadata as CSV.
#' 
#' @param na_rm a logical. If `TRUE` (default) remove episodes with incomplete 
#'   information.
#'
#' @return A four-columns data frame with: 
#'   - `date`: the date of the episode;
#'   - `title`: the title of the episode;
#'   - `duration`: the duration of the episode (in seconds);
#'   - `file_url`: the URL of the mp3.
#' 
#' @export
#'
#' @seealso [create_m3u()]
#'
#' @examples
#' \dontrun{
#' ## Retrieve episodes metadata ----
#' 
#' tab <- get_metadata("un-ete-avec-homere", radio = "franceinter")
#' }

get_metadata <- function(podcast, radio = "franceinter", path = ".", 
                         na_rm = TRUE) {
  
  
  ## Check Inputs ----
  
  check_arg_path(path)
  check_arg_radio(radio)
  check_arg_na_rm(na_rm)
  check_arg_podcast(podcast)
  
  
  ## Check podcast name (is podcast home page URL exist?) ----
  
  check_podcast_name(podcast, radio)
  
  
  ## Read Previous Metadata ----
  
  data <- read_metadata(path, podcast, na_rm)
  
  n_episodes <- nrow(data)
  
  
  ## Get date of last scrapped episode ----
  
  if (nrow(data) > 0) {
    start_date <- as.character(max(as.Date(data$"date")))
  } else {
    start_date <- NULL
  }
  
  
  ## Check for new episodes ----
  
  podcasts <- check_for_new_episodes(podcast, radio, path, start_date, na_rm)
  
  
  ## Get new episodes ----
  
  podcasts <- get_new_episodes(podcasts, podcast)
  
  
  ## Clean dates ----
  
  podcasts <- convert_dates(podcasts)
  
  
  ## Append new episodes ----
  
  if (nrow(podcasts)) data <- rbind(podcasts, data)
  
  
  ## Remove Episodes with Incomplete Information ----
  
  if (na_rm && nrow(data)) {
    
    data <- data[!is.na(data$"date"), ]
    data <- data[!is.na(data$"title"), ]
    # data <- data[!is.na(data$"duration"), ]
    data <- data[!is.na(data$"file_url"), ]
  }
  
  
  ## Export Metadata ----
  
  if (nrow(data) > n_episodes) {
    
    utils::write.csv2(data, file = file.path(path, "csv",
                                             paste0(podcast, ".csv")),
                      row.names = FALSE)
    
    messages::msg_done("Adding",messages::msg_value(nrow(data) - n_episodes),
                       "new episodes to",
                       messages::msg_value(file.path(path, "csv", 
                                                     paste0(podcast, ".csv"))))
  } else {
    
    messages::msg_oops("No new episode found")
  }
  
  
  invisible(data)
}
