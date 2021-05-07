get_mp3 <- function(data, podcast, path = ".") {
  
  if (missing(data))          stop("Argument 'data' is required.")
  if (missing(podcast))       stop("Argument 'podcast' is required.")
  
  if (is.null(data))          stop("Argument 'data' is required.")
  if (is.null(podcast))       stop("Argument 'podcast' is required.")
  
  if (!is.data.frame(data))   stop("Argument 'data' must be a data frame.")
  if (!is.character(podcast)) stop("Argument 'podcast' must be a character.")
  
  if (sum(c("date", "title", "duration", "file_url") %in% colnames(data)) != 4)
    stop("Argument 'data' must contain the following variables: 'date', ",
         "'title', 'duration', and 'file_url'.")
  
  if (!dir.exists(path)) stop("The path <", path, "> does not exist.")
  
  dir.create(file.path(path, podcast), showWarnings = FALSE)
  
  k <- 0
  
  if (nrow(data)) {
    
    for (i in 1:nrow(data)) {
      
      filename <- paste0(podcast, " - ", gsub("-", "", data$"date"[i]), ".mp3")
      
      if (!file.exists(file.path(path, podcast, filename))) {
        
        utils::download.file(url      = data$"file_url"[i], 
                             destfile = file.path(path, podcast, filename))
        
        k <- k + 1
      }
    }
  }
  
  if (k > 0) {
    
    usethis::ui_done(paste0("New episodes: {usethis::ui_value(k)} ",
                            "mp3 downloaded in ",
                            "{usethis::ui_value(file.path(path, podcast))} "))
  } else {
    
    usethis::ui_oops("No new mp3 downloaded.")
  }
  
  invisible(NULL)
}
