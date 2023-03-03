# ## code to prepare `strava_api` dataset goes here
# 
# 
# # read_dates --------------------------------------------------------------
# 
# 
# 
# #based on https://rviews.rstudio.com/2021/11/22/strava-data/
# 
# # libraries ---------------------------------------------------------------
# 
# library("httr")
# library("jsonlite")
# library("dplyr")
# library("targets")
# 
# # res <- httr::GET(url = "https://www.strava.com/api/v3/athlete",
# #                  httr::add_headers(Authorization = "Bearer e30fbd81c175ed2d2d64bade2cc56bbe2fdea3ea"))
# # 
# # res$status_code
# # 
# # base::rawToChar(res$content)
# 
# #jsonlite::fromJSON(base::rawToChar(res$content))
# 
# options(httr_oob_default=TRUE) 
# 
# id <- "102707"
# secret <- "2dc7fc6b21de393e4ffe2c868cb3459e7e69904c"
# url <-
#   glue::glue("http://www.strava.com/oauth/authorize?client_id={id}&response_type=code&redirect_uri=http://localhost/exchange_token&approval_prompt=force&scope=activity:read_all,activity:read,profile:read_all")
# 
# print(url)
# 
# app <-
#   httr::oauth_app(appname = "AppRun",
#                   key = id,
#                   secret = secret)
# 
# endpoint <- httr::oauth_endpoint(request = NULL,
#                                  authorize = "https://www.strava.com/oauth/authorize",
#                                  access = "https://www.strava.com/oauth/token")
# 
# token <- 
#   httr::oauth2.0_token(endpoint = endpoint, 
#                        app = app, 
#                        as_header = FALSE,
#                        scope = "activity:read_all,activity:read,profile:read_all",
#                        use_oob = TRUE,
#                        cache = TRUE)
# 
# 
# 
# res_activities <-
#   httr::POST(url = "https://www.strava.com/oauth/token",
#              body = 
#                list(client_id = id,
#                     client_secret = "2dc7fc6b21de393e4ffe2c868cb3459e7e69904c",
#                     code = "7e52906aafbaae5c81ae60bdb39d010a2f72c083",
#                     grant_type = "authorization_code")
#              )
# 
# # functions ---------------------------------------------------------------
# 
# define_strava_app <- function(s.name, s.key, s.secret) {
#     oauth_app(
#       appname = s.name,
#       key = s.key,
#       secret = s.secret
#       )
# }
# 
# define_strava_endpoint <- function() {
#   oauth_endpoint(request = NULL,
#                  authorize = "https://www.strava.com/oauth/authorize",
#                  access = "https://www.strava.com/oauth/token")
# }
# 
# 
# define_strava_sig <- function(endpoint, app) {
#   oauth2.0_token(
#     endpoint,
#     app,
#     scope = "activity:read_all,activity:read,profile:read_all",
#     type = NULL,
#     use_oob = FALSE,
#     as_header = FALSE,
#     use_basic_auth = FALSE,
#     cache = FALSE
#   )
# }
# 
# read_all_activities <- function(sig) {
#   activities_url <- httr::parse_url("https://www.strava.com/api/v3/athlete/activities")
#   
#   act_vec <- vector(mode = "list")
#   df_act <- tibble::tibble(init = "init")
#   i <- 1L
#   
#   while (nrow(df_act) != 0) {
#     r <- activities_url %>%
#       httr::modify_url(
#         query = list(
#           access_token = sig$credentials$access_token[[1]],
#           page = i
#         )
#       ) %>%
#       GET()
#     
#     df_act <- content(r, as = "text") %>%
#       fromJSON(flatten = TRUE) %>%
#       as_tibble()
#     if (nrow(df_act) != 0)
#       act_vec[[i]] <- df_act
#     i <- i + 1L
#   }
#   
#   df_activities <- act_vec %>%
#     bind_rows() %>%
#     mutate(start_date = ymd_hms(start_date))
# }
# 
# 
# # commands ----------------------------------------------------------------
# 
# app.name <- "AppRun"
# STRAVA_KEY <- "102707"
# STRAVA_SECRET <- "2dc7fc6b21de393e4ffe2c868cb3459e7e69904c"
# 
# my_app <- define_strava_app(s.name = app.name,
#                             s.key = STRAVA_KEY,
#                             s.secret = STRAVA_SECRET)
# 
# 
# my_endpoint <- define_strava_endpoint()
# 
# 
# my_sig <- define_strava_sig(endpoint = my_endpoint,
#                             app = my_app)
# 
# df_act_raw <- read_all_activities(my_sig)
# 
# usethis::use_data(strava_api, overwrite = TRUE)
