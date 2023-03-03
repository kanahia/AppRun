library("dplyr")
library("fit")
library("ggplot2")
library("plotly")
# parsing gpx file --------------------------------------------------------

#strava <- fit::read.fit("/home/jason/practice/gpx/fit_files/Afternoon_Run.fit")


#' Plot leaflet map from garin fit file
#'
#' @param data fit file read with fit::read.fit()
#' @param color route color
#' @param opacity route opacity
#' @param weight route weigth
#' @param provider map provider
#' 
#' @importFrom leaflet leaflet addTiles addProviderTiles addPolylines addMarkers
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @return leaflet map
#' 
#' @export plot_map
#' 
plot_map <- function(data, 
                     color = "red",
                     opacity = 0.7,
                     weight = 3,
                     provider = "OpenStreetMap.Mapnik",
                     quantile = 0.8) {
  
  #data <- fit::read.fit(path)
  records <- 
    data$record %>%
    dplyr::filter(position_lat != position_long)
  
  condition <- var(data$record$position_lat)
  
  if(condition > 1){
    records <- 
      records %>%
      dplyr::filter(position_lat > median(position_lat)-1 & position_lat < median(position_lat)+1) %>%
      dplyr::arrange(timestamp)
    
  } else {
    records <- 
      data$record
  }
  
  map <-
    records %>%
    dplyr::filter(position_lat != position_long) %>%
    dplyr::select(position_lat, position_long) %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addProviderTiles(provider = provider) %>%
    leaflet::addPolylines(., 
                          lat = ~position_lat, 
                          lng = ~position_long, 
                          color = color, 
                          opacity = opacity, 
                          weight = weight) %>%
    leaflet::addMarkers(lng= records$position_long[1], 
                        lat= records$position_lat[1],
                        popup="Start") %>%
    leaflet::addMarkers(lng= records$position_long[length(records$position_long)],
                        lat= records$position_lat[length(records$position_lat)],
                        popup="Finish")
  return(map)

}


#  #https://stackoverflow.com/questions/50806293/add-gradient-color-to-polyline-in-leaflet-r
# test <- t$record
# col <- rainbow(7)[1:6]
# #col[2] <- "orange"
# 
# test <-
#   test %>%
#   dplyr::mutate(speed = ms2kmh(t),
#                 min_km = 
#                   vapply(X = 1:nrow(test),
#                          FUN = function(x) {
#                            
#                            min_km(speed[x])
#                            
#                          },
#                          FUN.VALUE = character(1L)
#                   )
#                 )
# q <- quantile(test$speed)
# 
# test <-
#   test %>% 
#   dplyr::mutate(
#     color = 
#       vapply(
#         X = 1:nrow(test),
#         FUN = function(x) {
#           if (speed[x] <= q[2]) {
#             col[6] 
#           } else if (speed[x] > q[2] & speed[x] <= q[3]) {
#             col[5]
#           } else if (speed[x] > q[3] & speed[x] <= q[4]) {
#             col[4] 
#           } else if (speed[x] > q[4] & speed[x] <= q[5]) {
#             col[2] 
#           } else if (speed[x] > q[5]) {
#             col[1] 
#           }
#         },
#         FUN.VALUE = character(1L)
#       ),
#     nextLat = dplyr::lead(position_lat),
#     nextLng = dplyr::lead(position_long)
#     )
#             
# #test$color <- as.factor(test$color)
# 
# map <-
#   test %>%
#   #dplyr::select(position_lat, position_long, speed, color) %>%
#   leaflet::leaflet() %>%
#   leaflet::addTiles() %>%
#   leaflet::addProviderTiles(provider = "OpenStreetMap.Mapnik") %>%
#   leaflet::addPolylines(.,
#                         lng = ~position_long,
#                         lat = ~position_lat,
#                         color = ~as.list(test$color),
#                         opacity = 0.7,
#                         weight = 3)
# 
# gradient_map <- 
#   leaflet() %>% 
#   addTiles()

# for (i in 1:nrow(test)) {
#   gradient_map <- addPolylines(map = gradient_map,
#                                data = test, 
#                                lng = as.numeric(test[i, c('position_long', 'nextLng')]), 
#                                lat = as.numeric(test[i, c('position_lat', 'nextLat')]), 
#                                color = as.character(test[i, c('color')])
#   )
# }

#' Seconds to H M S
#'
#' @param time_in_seconds 
#'
#' @return dataframe with H M S values
#' @export seconds_to_time
#' 
seconds_to_time <- function(seconds){
  
  stopifnot(is.numeric(seconds))
  message(paste0(seconds, " is not numeric"))
  
  time <-
    tibble(
      "hours" = floor(seconds %/% 3600),
      "minutes" = floor(seconds %/% (60)) - floor(seconds %/% 3600)*60,
      "seconds" = floor(seconds %% 60) 
        
      )
  return(time)
  
}


#' Seconds to formatted time intervals
#'
#' @param seconds 
#'
#' @return vecotr of string in 00:00:00 format
#' @export duration2time
#' 
duration2time <- function(seconds) {
  stopifnot(is.numeric(seconds))
  
  dur <-
    hms::as_hms(seq(0,seconds)) %>%
    substr(., 1, 8)
  
  return(dur)
}


#' Convert meters/s to km/h
#'
#' @param data fit file read with fit::read.fit()
#'
#' @return vector of speed in km/h
#' @export ms2kmh
#'
ms2kmh <- function(data){
  x <- c()
  start <- 0
  for(i in 1:length(data$record$distance)){
    x[i] <- data$record$distance[i] - start
    start <- data$record$distance[i] 
  }
  
  x <- round((x*3600)/1000, digits = 2)
  
  return(x)
}


#' Plot plotly heart rate based on garmin fenix 6x pro
#'
#' @param data fit file read with fit::read.fit()
#'
#' @return plotly object
#' @importFrom plotly plot_ly layout add_lines
#' @importFrom magrittr %>%
#' 
#' @export plotly_HeartRate
#' 
plotly_HeartRate <- function(data,
                             title = "",
                             opacity = 0.8,
                             black = FALSE){
  
  if(is.list(data)){
    run_duration <- duration2time(data$session$total_timer_time)
  } else if(is.numeric(data)) {
    run_duration <- duration2time(data)
  } else {
    message("Not proper .fit format")
  }
  
  loess_hr <- predict(loess(heart_rate ~ timestamp, 
                            data = data$record, 
                            span = 0.005))
  
  fig <- 
    plotly::plot_ly(x = ~data$record$timestamp, 
                    y = loess_hr, #~t$record$heart_rate, 
                    type = 'scatter', 
                    mode = 'lines', 
                    fill = 'tozeroy',
                    fillcolor = glue::glue('rgba(255, 0, 0, {opacity})'), #'rgba(233, 27, 27, 0.4)',
                    line = list(color = 'rgba(205, 12, 24, 0.8)'),
                    hoverinfo = "text",
                    hovertext = paste(paste0(round(loess_hr, digits = 0), " bpm"),
                                      "<br>",
                                      paste0(run_duration)
                    )
    )
  
  fig <-
    fig %>%
    plotly::layout(
      xaxis = list(
        title = title,
        ticktext = as.list(run_duration[c(seq(1, nrow(data$record), by = 300), length(run_duration))]),
        tickvals = as.list(data$record$timestamp[c(seq(1, nrow(data$record), by = 300), length(run_duration))]),
        tickmode = "array",
        spikemode = "across+toaxis",
        showspikes = TRUE,
        spikesnap = 'cursor',
        showline = TRUE,
        showgrid = TRUE,
        spikedash = 'solid'
      ),
      
      hovermode  = 'x',
      spikedistance =  -1,
      yaxis = list(title = "Heart rate (bpm)")
    ) %>%
    plotly::add_lines(y = mean(data$record$heart_rate),
                      x = data$record$timestamp, 
                      line = list(color = "black",
                                  dash = "dash",
                                  width = 1.5),
                      inherit = FALSE,
                      showlegend = FALSE,
                      hoverinfo = "text",
                      hovertext = paste(paste0("Mean: ", round(mean(loess_hr, digits = 0)))
                      ),
                      hoverlabel = 
                        list(bgcolor="white",
                             font = list(color = "black"))
    )
  
  if(black == TRUE) {
    fig <- 
      fig %>%
      plotly::layout(plot_bgcolor  = "rgba(20, 19, 33, 1)",
                     paper_bgcolor = "rgba(18, 17, 31, 0.7)",
                     fig_bgcolor   = "rgba(18, 17, 31, 0.05)", 
                     xaxis = list(color = "white", gridcolor = "ffff"),
                     yaxis = list(color = "white", gridcolor = "ffff"))
  } else {
    fig <- fig
  }
  
  return(fig)
}


#' Get colors for cadence zones
#'
#' @param data fit file read with fit::read.fit()
#'
#' @return vector
#' @export set_colors
#'
set_colors <- function(data){
  
  cadence_color <-
    vapply(
      X = 1:nrow(data$record),
      FUN = function(x) {
        cadence_both <- data$record$cadence * 2
        if (cadence_both[x] <= 153) {
          "#e5042c" #red
        } else if (cadence_both[x] > 153 & cadence_both[x] <= 164) {
          "#e9522c" #orange
        } else if (cadence_both[x] > 164 & cadence_both[x] <= 174) {
          "#71bd4b" #green
        } else if (cadence_both[x] > 174 & cadence_both[x] <= 184) {
          "#53a4d8" #blue
        } else if (cadence_both[x] > 184) {
          "#8052a0" #purple
        }
      },
      FUN.VALUE = character(1L)
    )
  return(cadence_color)
}



#' Plot plotly cadence
#'
#' @param data 
#' @param span span value to loess function
#'
#' @importFrom plotly plot_ly layout add_lines
#' @importFrom magrittr %>%
#' @importFrom fANCOVA loess.as
#' 
#' @return plotly object
#' 
#' @export plotly_cadence
#'
plotly_cadence <- function(data,
                           span = 0.007,
                           black = FALSE) {
  
  if(is.list(data)){
    run_duration <- duration2time(data$session$total_timer_time)
  } else if(is.numeric(data)) {
    run_duration <- duration2time(data)
  } else {
    message("Not proper .fit format")
  }
  
  #get colors
  cadence_color <- set_colors(data = data)
  
  # smooth cadence
  # loess_cadence <- fANCOVA::loess.as(x = data$record$timestamp,
  #                                    y = data$record$cadence*2, 
  #                                    degree = 1, 
  #                                    criterion = c("aicc", "gcv")[2],
  #                                    user.span = NULL, 
  #                                    plot = F)
  # 
  #  loess_cadence <- predict(loess_cadence)
  # 
  loess_cadence <- predict(loess(cadence*2 ~ timestamp,
                                 data = data$record,
                                 span = span))
  loess_cadence[loess_cadence < 0] <- 0
  
  if(sum(data$record$cadence == 0) < 50 | sum(data$record$cadence == 0) == 0) {
    cadence_threshold <- 50
  } else {
    cadence_threshold <- min(data$record$cadence)
  }
  
  
  #plotly
  fig <-
    plotly::plot_ly(data$record, 
                    x = ~data$record$timestamp, 
                    y = loess_cadence,
                    type = 'scatter',
                    mode = "markers", 
                    color = ~ loess_cadence,
                    marker = 
                      list(color = as.factor(cadence_color),
                           opacity = 0.8),
                    hoverinfo = "text",
                    hovertext = paste(paste0(round(loess_cadence, digits = 0), " spm"),
                                      "<br>",
                                      paste0(run_duration)
                    )
    ) %>%
    plotly::layout(
      xaxis = list(
        title = "",
        ticktext = as.list(run_duration[c(seq(1, nrow(data$record), by = 300), length(run_duration))]),
        tickvals = as.list(data$record$timestamp[c(seq(1, nrow(data$record), by = 300), length(run_duration))]),
        tickmode = "array"),
      yaxis = list(title = "Cadence (spm)",
                   range = list(cadence_threshold, max(data$record$cadence)*2 +10))
    ) %>%
    plotly::add_lines(x = data$record$timestamp,
                      y = mean(loess_cadence),
                      line = list(color = "black",
                                  dash = "dash",
                                  width = 1.5),
                      inherit = FALSE,
                      showlegend = FALSE,
                      hoverinfo = "text",
                      hovertext = paste(paste0("Mean: ", round(mean(loess_cadence, digits = 0)))
                      ),
                      hoverlabel = 
                        list(bgcolor="white",
                             font = list(color = "black"))
    ) %>%
    plotly::layout(showlegend = FALSE) %>%
    plotly::hide_colorbar()
  
  if(black == TRUE) {
    fig <- 
      fig %>%
      plotly::layout(plot_bgcolor  = "rgba(20, 19, 33, 1)",
                     paper_bgcolor = "rgba(18, 17, 31, 0.7)",
                     fig_bgcolor   = "rgba(18, 17, 31, 0.05)", 
                     xaxis = list(color = "white", gridcolor = "ffff"),
                     yaxis = list(color = "white", gridcolor = "ffff"))
  } else {
    fig <- fig
  }
  
  return(fig)
  
}


###############


#' Plot plotly heart rate based on garmin fenix 6x pro
#'
#' @param data fit file read with fit::read.fit()
#'
#' @return plotly object
#' @importFrom plotly plot_ly layout add_lines
#' @importFrom magrittr %>%
#' 
#' @export plotly_speed
#' 
plotly_speed <- function(data,
                         title = "",
                         opacity = 0.4,
                         black = FALSE){
  
  if(is.list(data)){
    run_duration <- duration2time(data$session$total_timer_time)
  } else if(is.numeric(data)) {
    run_duration <- duration2time(data)
  } else {
    message("Not proper .fit format")
  }
  
  #speed <- ms2kmh(data)
  data$record <-
    data$record %>%
    dplyr::mutate(speed = ms2kmh(data),
                  min_km = 
                    vapply(X = 1:nrow(data$record),
                           FUN = function(x) {
                             
                             min_km(speed[x])
                             
                           },
                           FUN.VALUE = character(1L)
                    )
                  )
  
  loess_data <- predict(loess(speed ~ timestamp, 
                              data = data$record, 
                              span = 0.005))
  loess_data[loess_data < 0] <- 0
  
  fig <- 
    plotly::plot_ly(x = ~data$record$timestamp, 
                    y = loess_data,
                    type = 'scatter', 
                    mode = 'lines', 
                    fill = 'tozeroy',
                    fillcolor = glue::glue('rgba(37, 150, 190, {opacity})'),
                    line = list(color = 'rgba(37, 150, 190, 0.9)'),
                    hoverinfo = "text",
                    hovertext = paste(paste0(round(loess_data, digits = 2), " km/h"),
                                      "<br>",
                                      data$record$min_km,
                                      "<br>",
                                      paste0(run_duration)
                    )
    )
  
  fig <-
    fig %>%
    plotly::layout(
      xaxis = list(
        title = title,
        ticktext = as.list(run_duration[c(seq(1, nrow(data$record), by = 300), length(run_duration))]),
        tickvals = as.list(data$record$timestamp[c(seq(1, nrow(data$record), by = 300), length(run_duration))]),
        tickmode = "array",
        spikemode = "across+toaxis",
        showspikes = TRUE,
        spikesnap = 'cursor',
        showline = TRUE,
        showgrid = TRUE,
        spikedash = 'solid'
      ),
      
      hovermode  = 'x',
      spikedistance =  -1,
      yaxis = list(title = "Speed (km/h)")
    ) %>%
    plotly::add_lines(x = data$record$timestamp, 
                      y = mean(loess_data),
                      line = list(color = "black",
                                  dash = "dash",
                                  width = 1.5),
                      inherit = FALSE,
                      showlegend = FALSE,
                      hoverinfo = "text",
                      hovertext = paste0("Mean: ", 
                                         round(mean(loess_data), digits = 2), 
                                         " km/h"),
                      hoverlabel = 
                        list(bgcolor="white",
                             font = list(color = "black"))
                      )
  
  if(black == TRUE) {
    fig <- 
      fig %>%
      plotly::layout(plot_bgcolor  = "rgba(20, 19, 33, 1)",
                     paper_bgcolor = "rgba(18, 17, 31, 0.7)",
                     fig_bgcolor   = "rgba(18, 17, 31, 0.05)", 
                     xaxis = list(color = "white", gridcolor = "ffff"),
                     yaxis = list(color = "white", gridcolor = "ffff"))
  } else {
    fig <- fig
  }
  
  return(fig)
}


#' Get min'km from km/h
#'
#' @param x speed in km/h
#'
#' @return vector of elements
#'
#' @export min_km
#' 
min_km <- function(x) {
  
  if(x == 0) {
    "0 min/km"
  } else {
    min <-  1 / (x / 60)
    sec <- min - floor(min)
    if ((min / sec) == Inf) {
      paste0(min, ":00", " min/km", collapse = "")
    } else {
      if(round(60 * sec, digits = 0) < 10){
        paste0(floor(min),
                     ":0",
                     round(60 * sec, digits = 0) ,
                     " min/km",
                     collapse = "")
        
      } else {
        paste0(floor(min),
                     ":",
                     round(60 * sec, digits = 0) ,
                     " min/km",
                     collapse = "")
        
      }
    }
  }
  

}


# plotly::subplot(style(plotly_HeartRate(t), showlegend = FALSE),
#                 style(plotly_speed(t), showlegend = FALSE),
#                 plotly_cadence(t),
#                 nrows = 3,
#                 shareX = TRUE) %>%
#   plotly::layout(hovermode = 'x')


#' Plot heart_rate zones
#'
#' @param data fit file read with fit::read.fit()
#'
#' @return plotly object
#' @export plotly_zones
#'
plotly_zones <- function(data,
                         black = FALSE) {
  
  df <- 
    data.frame(
      zone = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5"),
      n = 0,
      percent = 0)
  
  data <-
    data$record %>% 
    dplyr::select(heart_rate, timestamp) %>%
    dplyr::mutate(
      zone = 
        dplyr::case_when(
          heart_rate <= 110 ~ "Zone 1",
          heart_rate > 110 & heart_rate <= 138 ~ "Zone 2",
          heart_rate > 138 & heart_rate <= 158 ~ "Zone 3",
          heart_rate > 158 & heart_rate <= 166 ~ "Zone 4",
          heart_rate > 166 ~ "Zone 5"
        )
    ) %>% 
    dplyr::count(zone, .drop = FALSE) %>%
    dplyr::summarise(zone, n, percent = round(prop.table(n) * 100, digits = 2))
  
  complete_df <- 
    data %>% 
    dplyr::anti_join(df, ., by = "zone") %>%
    dplyr::bind_rows(data) %>%
    dplyr::arrange(zone) %>%
    dplyr::mutate(filling = 100 - percent,
                  sum = percent + filling)
  
  fig <-
    complete_df %>%
    plot_ly(x = ~percent, 
            y = ~zone, 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = c('rgb(193, 190, 190)',
                                    'rgb(59, 151, 243)',
                                    'rgb(130, 201, 30)',
                                    'rgb(249, 137, 37)',
                                    'rgb(211, 32, 32)')
                          ),
            textposition = "auto",
            hoverinfo = "text",
            hovertext = paste(paste0(complete_df$zone),
                              "<br>",
                              paste0("Time :", hms::as_hms(complete_df$n)),
                              "<br>",
                              paste0("Percent: ", round(complete_df$percent, digits = 0), " %")
                              ),
            showlegend = FALSE
            ) %>% 
    add_trace(x = ~filling, 
              name = 'test',
              marker = list(color = 'rgb(228, 228, 228)'),
              showlegend = FALSE
              ) %>%
    plotly::layout(barmode = 'stack',
                   #hovermode = 'x',
                   showlegend = FALSE,
                   xaxis = list(title = ''),
                   yaxis = list(title = "")
    )
    # plotly::add_annotations( x = ~ sum+2,
    #                          y = ~zone,
    #                          text = ~round(percent, digits = 0),
    #                          xref = "x",
    #                          yref = "y",
    #                          showarrow = FALSE)
  
  if(black == TRUE) {
    fig <- 
      fig %>%
      plotly::layout(plot_bgcolor  = "rgba(20, 19, 33, 1)",
                     paper_bgcolor = "rgba(18, 17, 31, 0.7)",
                     fig_bgcolor   = "rgba(18, 17, 31, 0.05)", 
                     xaxis = list(color = "white", gridcolor = 'rgba(255,255,255, 1)',
                     yaxis = list(color = "white", gridcolor = 'rgba(255,255,255, 1)')))
  } else {
    fig <- fig
  }
    
  
  return(fig)
  
}
