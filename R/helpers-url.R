#' _Create radio base URL_
#'
#' @noRd

base_url <- function(radio = "franceinter") {
  
  check_arg_radio(radio)
  
  paste0("https://www.radiofrance.fr/", radio, "/podcasts/")
}



#' _Check podcast URL_
#'
#' @noRd

check_podcast_name <- function(podcast, radio) {
  
  check_arg_radio(radio)
  check_arg_podcast(podcast)
  
  owarn <- options()$"warn"
  on.exit(options(warn = owarn))
  options(warn = -1)
  
  html_page <- rvest::session(paste0(base_url(radio), podcast))
  
  if (html_page$"response"$"status_code" != 200) stop("Wrong podcast name")
  
  invisible(NULL)
}
