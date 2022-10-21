#' @noRd

add_cover <- function(path, cover, album, artist, data) {
  
  filenames <- list.files(path, pattern = "mp3$")
  
  if (length(filenames)) {
    
    for (i in 1:length(filenames)) {
      
      filename <- filenames[i]
      
      filename <- file.path(path, filename)
      tempname <- file.path(path, "tempname.mp3")
      
      if (file.exists("tempfile")) system("rm tempfile")
      
      system(paste0("ffmpeg", " -i ", "'", filename, "'", 
                    " -f ffmetadata tempfile"))
      
      system(paste0("ffmpeg", 
                    " -i ", "'", filename, "'", 
                    " -vn -codec:a copy -map_metadata -1 ",
                    "'", tempname, "'"))
      
      system(paste0("rm ", "'", filename, "'"))
      
      meta <- readLines("tempfile")[-1]
      meta <- strsplit(meta, "=")
      meta_keys   <- unlist(lapply(meta, function(x) x[1]))
      meta_values <- unlist(lapply(meta, function(x) x[2]))
      
      pos <- which(meta_keys %in% c("date", "title", "album", "artist", "genre", 
                                    "copyright", "encoded_by"))
      
      meta_keys   <- meta_keys[pos]
      meta_values <- meta_values[pos]
      
      pos <- which(meta_keys == "album")
      if (length(pos)) {
        meta_values[pos] <- album
      } else {
        meta_keys   <- c(meta_keys, "album")
        meta_values <- c(meta_values, album)
      }
      
      pos <- which(meta_keys == "artist")
      if (length(pos)) {
        meta_values[pos] <- artist
      } else {
        meta_keys   <- c(meta_keys, "artist")
        meta_values <- c(meta_values, artist)
      }
      
      pos <- which(meta_keys == "copyright")
      if (length(pos)) {
        meta_values[pos] <- "Radio France"
      } else {
        meta_keys   <- c(meta_keys, "copyright")
        meta_values <- c(meta_values, "Radio France")
      }
      
      
      pos <- which(meta_keys == "encoded_by")
      if (length(pos)) {
        meta_values[pos] <- "Radio France"
      } else {
        meta_keys   <- c(meta_keys, "encoded_by")
        meta_values <- c(meta_values, "Radio France")
      }
      
      pos <- which(meta_keys == "genre")
      if (length(pos)) {
        meta_values[pos] <- "Podcast"
      } else {
        meta_keys   <- c(meta_keys, "genre")
        meta_values <- c(meta_values, "Podcast")
      }
      
      short_date <- strsplit(filenames[i], " - |\\.mp3")[[1]][2]
      sop <- which(gsub("-", "", data$date) == short_date)
      
      pos <- which(meta_keys == "date")
      if (length(pos)) {
        meta_values[pos] <- as.character(data[sop, "date"])
      } else {
        meta_keys   <- c(meta_keys, "date")
        meta_values <- c(meta_values, as.character(data[sop, "date"]))
      }
      
      pos <- which(meta_keys == "title")
      if (length(pos)) {
        meta_values[pos] <- as.character(data[sop, "title"])
      } else {
        meta_keys   <- c(meta_keys, "title")
        meta_values <- c(meta_values, as.character(data[sop, "title"]))
      }
      
      expr <- ""
      for (i in 1:length(meta_keys))
        expr <- c(expr, paste0(" -metadata ", meta_keys[i], "=", 
                               "\"", meta_values[i], "\""))
      
      system(paste0("ffmpeg", 
                    " -i ", "'", tempname, "'",
                    " -i ", "'", cover, "'",
                    " -c copy -map 0 -map 1 -metadata:s:v title='Cover (front)'",
                    paste0(expr, collapse = ""), " ",
                    "'", filename, "'"))
      
      system(paste0("rm ", "'", tempname, "'"))
      
    }
    
    messages::msg_done("Covers successfully added!")
  }
  
  if (file.exists("tempfile")) system("rm tempfile")
  
  invisible(NULL)
}



#' @noRd

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
  
  dir.create(file.path(path, "mp3", podcast), showWarnings = FALSE, 
             recursive = TRUE)
  
  k <- 0
  
  if (nrow(data)) {
    
    for (i in 1:nrow(data)) {
      
      filename <- paste0(podcast, " - ", gsub("-", "", data$"date"[i]), ".mp3")
      
      if (!file.exists(file.path(path, "mp3", podcast, filename))) {
        
        utils::download.file(url      = data$"file_url"[i], 
                             destfile = file.path(path, "mp3", podcast, 
                                                  filename))
        
        k <- k + 1
      }
    }
  }
  
  if (k > 0) {
    
    messages::msg_done("New episodes:", messages::msg_value(k),
                       "mp3 downloaded in",
                       messages::msg_value(file.path(path, "mp3")))
  } else {
    
    messages::msg_oops("No new mp3 downloaded.")
  }
  
  invisible(NULL)
}
