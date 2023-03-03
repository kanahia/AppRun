## code to prepare `metadata_fit` dataset goes here

a_code <- 
  lapply(
    X = fit_files,
    FUN = function(x) {
      x$session$sport}
    ) %>%
  unlist() %>%
  as.data.frame() %>%
  pull()

metadata_fit <-
  lapply(X = fit_files, 
         FUN = function(x) {
    vec <- x[["file_id"]][["time_created"]]
  }) %>% 
  unlist() %>% 
  as.data.frame() %>%
  tibble::rownames_to_column(var = "file") %>%
  dplyr::rename("timestamp" = 2) %>% 
  dplyr::mutate(date = as.Date(as.POSIXct(as.numeric(timestamp), origin="1989-12-31")),
                year = as.numeric(substr(date, 1,4)),
                month = month.name[as.numeric(substr(date, 6,7))],
                activity_code = 
                  dplyr::case_when(a_code == 0 ~ "Navigation",
                                   a_code == 1 ~ "Running",
                                   a_code == 2 ~ "Cycling",
                                   a_code == 5 ~ "Swimming",
                                   a_code == 12 ~ "Ice skating",
                                   a_code == 17 ~ "Hike",
                                   a_code == 41 ~ "Kayaking",
                                   TRUE ~ "Other"
                                   )
                  
                )


usethis::use_data(metadata_fit, overwrite = TRUE)
