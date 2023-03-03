## code to prepare `fit_files` dataset goes here

#' Read info or fit data
#'
#' @param path 
#' @param ext 
#'
#' @importFrom fit read.fit stringr str_detect(
#'
#' @return 
#' @export get_fit_files
#'
get_fit_files <- function(path = "/home/jason/practice/gpx/fit_files/",
                          ext = ".fit",
                          return = "data") {
  
  # stopifnot(
  #   stringr::str_detect(
  #     list.files(path, 
  #              pattern = ext),
  #     pattern = ext),
  #   message(paste0("No ", ext, " files found"))
  #   )
  
  fit <- list()
  files <- list.files(path = path,
                      pattern = ext)
  
  if(return == "metadata") {
    
    df <- data.frame(matrix(ncol = 4, nrow = 0))
    
    for(i in 1:length(files)){
      
      fit[[files[i]]] <- fit::read.fit(paste0(path, files[i]))
      
      new_row <-  c(files[i],
                    as.numeric(fit[[files[i]]]$file_id$time_created),
                    paste0(path, files[i]))
      
      df <- rbind(df, new_row)
      
    }
    
    colnames(df) <- c("file", "timestamp", "path")
    df <- df %>%
      dplyr::mutate(date = as.Date(as.POSIXct(as.numeric(df$timestamp), origin="1989-12-31"))) %>%
      dplyr::arrange(date)
    
    out <- df
    
  } else if (return == "data") {
    for (i in 1:length(files)) {
      fit[[files[i]]] <- fit::read.fit(paste0(path, files[i]))
      
      out <- fit
    }
  }
  
  return(out)
}



fit_files <- get_fit_files(path = "/home/jason/practice/gpx/fit_files/",
                           ext = ".fit",
                           return = "data")

usethis::use_data(fit_files, overwrite = TRUE)
