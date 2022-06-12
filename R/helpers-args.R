#' _Check argument "path"_
#'
#' @noRd

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



#' _Check argument "podcast"_
#'
#' @noRd

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



#' _Check argument "na rm"_
#'
#' @noRd

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



#' _Check argument "radio"_
#'
#' @noRd

check_arg_radio <- function(radio) {
  
  if (missing(radio)) {
    stop("Argument 'radio' is required")
  }
  
  if (!is.character(radio)) {
    stop("Argument 'radio' must be a character of length 1")
  }
  
  if (length(radio) != 1) {
    stop("Argument 'radio' must be a character of length 1")
  }
  
  valid_radios <- c("franceinter", "franceinfo", "francebleu", "franceculture",
                    "francemusique", "fip", "mouv")
  
  valid_radios_msg <- paste0(valid_radios, collapse = "' '")
  valid_radios_msg <- paste0("'", valid_radios_msg, "'")
  
  if (!(radio %in% valid_radios)) {
    stop("Argument 'radio' must be one among: ", valid_radios_msg)
  }
  
  invisible(NULL)
}



#' _Check argument "data"_
#'
#' @noRd

check_arg_data <- function(data) {
  
  if (missing(data)) {
    stop("Argument 'data' is required")
  }
  
  if (is.null(data)) {
    stop("Argument 'data' is required")
  }
  
  if (!is.data.frame(data)){
    stop("Argument 'data' must be a data.frame")
  }
  
  if (sum(c("date", "title", "duration", "file_url") %in% colnames(data)) != 4)
    stop("Argument 'data' must contain the following variables: 'date', ",
         "'title', 'duration', and 'file_url'")
  
  invisible(NULL)
}
