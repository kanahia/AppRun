## code to prepare `summary_year` dataset goes here

summary_data <- function(year) {
  
  year_min <- min(metadata_fit$year)
  year_max <- max(metadata_fit$year)
  years <- seq(from = min(metadata_fit$year), 
               to = max(metadata_fit$year), 
               by = 1)
  
}
  

usethis::use_data(summary_year, overwrite = TRUE)

categories <- unique(sales$category)
sub_categories <- unique(sales$sub_category)
ids <- unique(sales$id)



yearly <-
  metadata_fit %>% 
    group_by(year, month) %>%
    summarize(n = n()) %>%
  as.data.frame()

  plotly::plot_ly(data = yearly, 
                  x = as.factor(yearly$year), 
                  y = ~n, 
                  type = 'bar',
                  marker = list(color = 'rgb(158,202,225)',
                                line = list(color = 'rgb(8,48,107)',
                                            width = 1.5)),
                  hoverinfo = "text",
                  hovertext = paste(paste0("<b>", yearly$year, "</b>"),
                                    "<br>",
                                    paste0("Acitivities: ", yearly$n))
                  ) %>%
  plotly::layout(title = "Number activities per year",
                 xaxis = list(title = "", font = list(size = 14)),
                 yaxis = list(title = "", font = list(size = 14)),
                 hoverlabel = list(font=list(size=16))
                 )
