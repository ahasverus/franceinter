#' Retrieve Podcast Information
#'
#' @description 
#' Retrieves podcast information (i.e. date, title, mp3 duration and url).
#' Information will be saved as a CSV file. Each time this function is called
#' only new episodes will be treated.
#' 
#' @param podcast a character of length 1. The name of the M3U file.
#' 
#' @param start_date a character of length 1. The date of the oldest episode 
#'   to retrieve. If `NULL` the `"2010-01-01"` will be used.
#'   
#' @param end_date a character of length 1. The date of the most recent episode 
#'   to retrieve. If `NULL` the current date will be used.
#'   
#' @param path the path to save metadata as CSV (must exist).
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
#' @examples
#' \dontrun{
#' ## Create a folder to store results ----
#' path <- "Podcasts/"
#' dir.create(path)
#' 
#' ## Get podcasts name ----
#' podcasts <- list_podcasts()
#' podcast  <- podcasts[3, ]
#' 
#' ## Retrieve episodes information ----
#' tab <- get_metadata(podcast    = podcast$"podcast", 
#'                     start_date = podcast$"start_date", 
#'                     end_date   = podcast$"end_date", 
#'                     path = path)
#' }

get_metadata <- function(podcast, start_date = NULL, end_date = NULL, 
                         path = ".", na_rm = TRUE) {
  
  ## Check Inputs ----
  
  if (!dir.exists(path)) stop("The path <", path, "> does not exist.")
  
  if (missing(podcast))       stop("Argument 'podcast' is required.")
  if (is.null(podcast))       stop("Argument 'podcast' is required.")
  if (!is.character(podcast)) stop("Argument 'podcast' must be a character.")
  
  page <- rvest::session(paste0(base_url(), podcast))
  if (page$response$status_code != 200) stop("Wrong podcast name.")
  
  if (!is.logical(na_rm))     stop("Argument 'na_rm' must be a boolean.")
  
  
  ## Create Sequence of Dates ----
  
  dates <- get_dates(from = start_date, to = end_date)
  
  
  ## Read Previous Metadata ----
  
  if (file.exists(file.path(path, paste0(podcast, ".csv")))) {
    
    data <- utils::read.csv2(file.path(path, paste0(podcast, ".csv")))
    
    if (sum(c("date", "title", "duration", "file_url") %in% colnames(data)) != 
        4)
      stop("Malformed <", file.path(path, paste0(podcast, ".csv")), "> file.")
    
    if (nrow(data)) {
      dates <- dates[as.Date(dates$short_date) > max(data$date), ]  
    }
    
  } else {
    
    data <- data.frame()
  }
  
  
  ## Remove Episodes with Incomplete Information ----
  
  if (na_rm && nrow(data)) {
    
    data <- data[!is.na(data$"date"), ]
    data <- data[!is.na(data$"title"), ]
    data <- data[!is.na(data$"duration"), ]
    data <- data[!is.na(data$"file_url"), ]
  }
  
  n_episodes <- nrow(data)
  
  
  ## Retrieve New Episodes Metadata ----
  
  if (nrow(dates)) {
    
    warn <- options()$"warn"
    
    episodes <- data.frame()
    
    for (i in 1:nrow(dates)) {
      
      full_url <- paste0(base_url(), podcast, "/", podcast, "-du-", 
                         dates[i, "long_date"])
      
      options(warn = -1)
      page <- rvest::session(full_url)
      options(warn = warn)
      
      if (page$response$status_code == 404) {
        
        full_url <- paste0(base_url(), podcast, "/", podcast, "-", 
                           dates[i, "long_date"])
        
        options(warn = -1)
        page <- rvest::session(full_url)
        options(warn = warn)
      }
      
      if (page$response$status_code == 404) {
        
        full_url <- paste0(base_url(), podcast, "/", podcast, "-du-", 
                           dates[i, "week_day"], "-", dates[i, "long_date"])
        
        options(warn = -1)
        page <- rvest::session(full_url)
        options(warn = warn)
      }
      
      if (page$response$status_code == 404) {
        
        full_url <- paste0(base_url(), podcast, "/", podcast, "-", 
                           dates[i, "week_day"], "-", dates[i, "long_date"])
        
        options(warn = -1)
        page <- rvest::session(full_url)
        options(warn = warn)
      }
      
      if (page$response$status_code == 200) {
        
        cat("Retrieving metadata for episode:", dates[i, "short_date"], "\r")
        
        content <- rvest::html_elements(page, "script")[1]
        content <- jsonlite::fromJSON(rvest::html_text(content))
        
        tmp <- data.frame(
          date     = as.character(as.Date(content$hasPart$datePublished)),
          title    = content$name,
          duration = content$audio$duration,
          file_url = content$audio$contentUrl
        )
        
        episodes <- rbind(episodes, tmp)
      }
    }
    
    data <- rbind(data, episodes)
  }
  
  
  ## Remove Episodes with Incomplete Information ----
  
  if (na_rm && nrow(data)) {
    
    data <- data[!is.na(data$"date"), ]
    data <- data[!is.na(data$"title"), ]
    data <- data[!is.na(data$"duration"), ]
    data <- data[!is.na(data$"file_url"), ]
  }
  
  
  ## Export Metadata ----
  
  if (nrow(data) > n_episodes) {
    
    utils::write.csv2(data, file = file.path(path, paste0(podcast, ".csv")),
                      row.names = FALSE)
    
    usethis::ui_done(paste0("Adding ", 
                            "{usethis::ui_value(nrow(data) - n_episodes)} ",
                            "new episodes to ",
                            "{usethis::ui_value(file.path(path, paste0(podcast, 
                            \".csv\")))} "))
  } else {
    
    usethis::ui_oops("No new episode found.")
  }
  
  
  invisible(data)
}
