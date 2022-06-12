#' _Utilities Functions_
#' 
#' @noRd

base_url <- function() "https://www.radiofrance.fr/franceinter/podcasts/"



#' 
#' @noRd

check_podcast_name <- function(podcast) {
  
  owarn <- options()$"warn"
  on.exit(options(warn = owarn))
  options(warn = -1)
  
  html_page <- rvest::session(paste0(base_url(), podcast))
  
  if (html_page$"response"$"status_code" != 200) stop("Wrong podcast name")
  
  invisible(NULL)
}



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



#' _Create a Dates Sequence_
#'
#' @description
#' Creates a sequence of dates between two dates.
#' 
#' @param from a character of length 1. The starting date of the sequence. If
#'   `NULL` the `"2010-01-01"` will be used.
#'   
#' @param to a character of length 1. The ending date of the sequence. If
#'   `NULL` the current date will be used.
#'
#' @return A seven-columns data frame with:
#'   - `short_date`: the date as "YYYY-MM-DD";
#'   - `day`: the day as "DD";
#'   - `month`: the month as "MM";
#'   - `year`: the year as "YYYY";
#'   - `week_day`: the day of the week (i.e. "lundi");
#'   - `full_month`: the month of the year (i.e. "janvier");
#'   - `full_date`: the date as "DD-janvier-YYYY".
#'
#' @noRd
#'
#' @examples
#' podcasts <- list_podcasts()
#' podcast  <- podcasts[1, ]
#' 
#' dates <- get_dates(from = podcast$"start_date")
#' dates <- dates[!(dates$"week_day" %in% c("samedi", "dimanche")), ]

get_dates <- function(from = "1979-01-01", to = NULL) {
  
  if (is.null(from)) from <- "1979-01-01"
  if (is.null(to))   to   <- Sys.Date()
  if (is.na(to))     to   <- Sys.Date()
  
  short_dates <- seq(as.Date(from), as.Date(to), by = 1)
  
  dates <- data.frame(
    short_date = as.character(short_dates),
    day        = format(short_dates, "%d"),
    month      = format(short_dates, "%m"),
    year       = format(short_dates, "%Y"),
    week_day   = tolower(format(short_dates, "%A")),
    full_month = format(short_dates, "%B")
  )
  
  dates$"full_month" <- gsub("[[:punct:]]", "", 
                             iconv(dates$"full_month", to = "ASCII//TRANSLIT"))
  
  dates$"long_date" <- paste(dates$"day", dates$"full_month", dates$"year", 
                             sep = "-")
  
  dates
}



check_arg_path <- function(path) {
  
  if (missing(path)) {
    stop("Argument 'path' is required")
  }
  
  if (!is.character(path)) {
    stop("Argument 'path' must be a character of length 1")
  }
  
  if (length(path) != 1) {
    stop("Argument 'path' must be a character of length 1")
  }
  
  if (!dir.exists(path)) {
    stop("The path '", path, "' does not exist")
  }
  
  invisible(NULL)
}



check_arg_podcast <- function(podcast) {
  
  if (missing(podcast)) {
    stop("Argument 'podcast' is required")
  }
  
  if (!is.character(podcast)) {
    stop("Argument 'podcast' must be a character of length 1")
  }
  
  if (length(podcast) != 1) {
    stop("Argument 'podcast' must be a character of length 1")
  }

  invisible(NULL)
}



check_arg_na_rm <- function(na_rm) {
  
  if (missing(na_rm)) {
    stop("Argument 'na_rm' is required")
  }
  
  if (length(na_rm) != 1) {
    stop("Argument 'na_rm' must be a boolean of length 1")
  }
  
  if (!is.logical(na_rm)) {
    stop("Argument 'na_rm' must be a boolean of length 1")
  }
  
  invisible(NULL)
}



check_for_new_episodes <- function(podcast, path, limit, na_rm) {
  
  go_on <- TRUE
  page  <- 1
  
  pages_to_scrap <- data.frame()
  
  while (go_on) {
    
    ## Go to podcast homepage (page page) ----
    
    full_url  <- paste0(base_url(), podcast, "?p=", page)
    html_page <- rvest::session(full_url)
    
    if (html_page$"response"$"status_code" != 200) stop("Error 404")
    
    content <- rvest::html_elements(html_page, ".CardDetails-title")
    
    if (length(content)) {
      
      links <- rvest::html_elements(content, "a")
      
      page_links <- rvest::html_attr(links, "href")
      page_links <- paste0("https://www.radiofrance.fr", page_links)
      
      page_titles <- rvest::html_text(links)
      page_titles <- gsub("^\\s{1,}|\\s{1,}$", "", page_titles)
      page_titles <- gsub("\\s+", " ", page_titles)
      
      content <- rvest::html_elements(html_page, "time")
      page_dates <- rvest::html_text(content)
      page_dates <- gsub("\\n", "", page_dates)
      page_dates <- gsub("^\\s{1,}|\\s{1,}$", "", page_dates)
      page_dates <- gsub("\\s+", " ", page_dates)
      
      dat <- data.frame("title" = page_titles,
                        "date"  = page_dates,
                        "url"   = page_links)
      
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


check_for_dates <- function(data, limit) {
  
  if (!is.null(limit)) {
    
    dates_dict <- get_dates()
    dates_dict$"long_dates" <- paste(dates_dict$"week_day", 
                                     as.numeric(dates_dict$"day"),
                                     dates_dict$"full_month", dates_dict$"year")
    
    dates_dict <- dates_dict[which(dates_dict$"long_dates" %in% data$"date"), ]
    dates_dict <- dates_dict[ , c("short_date", "long_dates")]
    
    data <- merge(data, dates_dict, by.x = "date", by.y = "long_dates")
    data <- data[order(as.Date(data$"short_date"), decreasing = TRUE), ]
    
    data <- data[which(as.Date(data$"short_date") > as.Date(limit)), ]
  }
  
  data[ , c("date", "title", "url")]
}



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
      
      tmp <- data.frame(
        date     = data[i, "date"],
        title    = content$"content"$"title",
        duration = content$"content"$"manifestations"$"duration",
        file_url = content$"content"$"manifestations"$"url"
      )
      
      new_episodes <- rbind(new_episodes, tmp)
    }
  }
  
  new_episodes
}



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
