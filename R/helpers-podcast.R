#' _Read podcast metadata from a CSV file (if exists)_
#'
#' @noRd

read_metadata <- function(path, podcast, na_rm) {
  
  if (file.exists(file.path(path, "csv", paste0(podcast, ".csv")))) {
    
    data <- utils::read.csv2(file.path(path, "csv", paste0(podcast, ".csv")))
    
    if (sum(c("date", "title", "duration", "file_url") %in% colnames(data)) != 
        4)
      stop("Malformed <", file.path(path, "csv", paste0(podcast, ".csv")), 
           "> file.")
    
    if (nrow(data)) {
      
      if (na_rm) {
        
        data <- data[!is.na(data$"date"), ]
        data <- data[!is.na(data$"title"), ]
        data <- data[!is.na(data$"duration"), ]
        data <- data[!is.na(data$"file_url"), ]
      }
      
      data <- data[order(as.Date(data$"date"), decreasing = TRUE), ]
    }
    
  } else {
    
    data <- data.frame()
  }
  
  data
}



#' _Detect if episodes are new (or already scrapped)_
#'
#' @noRd

check_for_dates <- function(data, limit) {
  
  if (!is.null(limit)) {
    
    dates_dict <- get_dates()
    dates_dict$"long_dates" <- paste(dates_dict$"week_day", 
                                     as.numeric(dates_dict$"day"),
                                     dates_dict$"full_month", dates_dict$"year")
    
    data$"date" <- gsub("\u00FB", "u", data$"date")
    data$"date" <- gsub("\u00E9", "e", data$"date")
    
    dates_dict <- dates_dict[which(dates_dict$"long_dates" %in% data$"date"), ]
    dates_dict <- dates_dict[ , c("short_date", "long_dates")]
    
    data <- merge(data, dates_dict, by.x = "date", by.y = "long_dates")
    data <- data[order(as.Date(data$"short_date"), decreasing = TRUE), ]
    
    data <- data[which(as.Date(data$"short_date") > as.Date(limit)), ]
  }
  
  data[ , c("date", "title", "url")]
}



#' _Retrive new episodes_
#'
#' @noRd

check_for_new_episodes <- function(podcast, radio, path, limit, na_rm) {
  
  go_on <- TRUE
  page  <- 1
  
  pages_to_scrap <- data.frame()
  
  while (go_on) {
    
    ## Go to podcast homepage (page page) ----
    
    full_url  <- paste0(base_url(radio), podcast, "?p=", page)
    html_page <- rvest::session(full_url)
    
    if (html_page$"response"$"status_code" != 200) stop("Error 404")
    
    content <- rvest::html_elements(html_page, ".CardDetails")
    
    if (length(content)) {
      
      dat <- data.frame()
      
      for (k in 1:length(content)) {
        
        card  <- rvest::html_elements(content[k], ".CardDetails-title")
        links <- rvest::html_elements(card, "a")
        
        if (length(links)) {
          
          page_links <- rvest::html_attr(links, "href")
          page_links <- paste0("https://www.radiofrance.fr", page_links)
          
          page_titles <- rvest::html_text(links)
          page_titles <- gsub("^\\s{1,}|\\s{1,}$", "", page_titles)
          page_titles <- gsub("\\s+", " ", page_titles)
          
          page_dates <- rvest::html_elements(content[k], "time")
          page_dates <- rvest::html_text(page_dates)
          page_dates <- gsub("\\n", "", page_dates)
          page_dates <- gsub("^\\s{1,}|\\s{1,}$", "", page_dates)
          page_dates <- gsub("\\s+", " ", page_dates)
          
          tmp <- data.frame("title" = page_titles,
                            "date"  = page_dates,
                            "url"   = page_links)
          dat <- rbind(dat, tmp)
        }
      }
      
      dat <- check_for_dates(dat, limit)
      
      if (nrow(dat)) {
        
        pages_to_scrap <- rbind(pages_to_scrap, dat)
        
        page <- page + 1
        
      } else {
        
        go_on <- FALSE
      }
      
    } else {
      
      go_on <- FALSE
    }
  }
  
  pages_to_scrap
}



#' _Retrieve mp3 URLs_
#'
#' @noRd

get_new_episodes <- function(data, podcast) {
  
  new_episodes <- data.frame()
  
  if (nrow(data)) {
    
    for (i in 1:nrow(data)) {
      
      html_page <- rvest::session(data[i, "url"])
      
      if (html_page$"response"$"status_code" != 200) stop("Error 404")
      
      content <- rvest::html_elements(html_page, "script")
      content <- content[length(content)]
      
      content <- jsonlite::fromJSON(rvest::html_text(content))
      content <- jsonlite::fromJSON(content$"body")
      
      episode_title <- content$"content"$"title"
      if (is.null(episode_title)) episode_title <- NA
      
      episode_duration <- content$"content"$"manifestations"$"duration"
      if (is.null(episode_duration)) episode_duration <- NA
      
      episode_file_url <- content$"content"$"manifestations"$"url"
      if (is.null(episode_file_url)) episode_file_url <- NA
      
      tmp <- data.frame(
        "date"     = data[i, "date"],
        "title"    = episode_title,
        "duration" = episode_duration,
        "file_url" = episode_file_url
      )
      
      new_episodes <- rbind(new_episodes, tmp)
    }
  }
  
  new_episodes
}



#' _Utility to convert date (from long to short format)_
#'
#' @noRd

convert_dates <- function(data) {
  
  if (nrow(data)) {
    
    dates_dict <- get_dates()
    dates_dict$"long_dates" <- paste(dates_dict$"week_day", 
                                     as.numeric(dates_dict$"day"),
                                     dates_dict$"full_month", dates_dict$"year")
    
    dates_dict <- dates_dict[ , c("short_date", "long_dates")]
    
    data$"date" <- gsub("[[:punct:]]", "", 
                        iconv(data$"date", to = "ASCII//TRANSLIT"))
    
    data <- merge(data, dates_dict, by.x = "date", by.y = "long_dates",
                  all.x = TRUE, all.y = FALSE)
    data$"date" <- data$"short_date"
    
    data <- data[ , c("date", "title", "duration", "file_url")]
    
    data <- data[order(as.Date(data$"date"), decreasing = TRUE), ]
  }
  
  data
}
