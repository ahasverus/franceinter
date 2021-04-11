#' _Utilities Functions_
#' 
#' @noRd

base_url <- function() "https://www.franceinter.fr/emissions/"



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

get_dates <- function(from = "2010-01-01", to = NULL) {
  
  if (is.null(from)) from <- "2010-01-01"
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
