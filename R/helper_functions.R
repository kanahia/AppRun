# # title page
# makePage <-
#   function (title, subtitle, contents) {
#     tagList(
#       div(
#         class = "page-title",
#         span(title,
#              class = "ms-fontSize-32 ms-fontWeight-semibold",
#              style = "color: #323130"),
#         span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular",
#              style = "color: #605E5C; margin: 14px;")
#         ),
#       contents
#       )
#   }
# 
# # makeCard
# 
# makeCard <- 
#   function(title, 
#            content, 
#            size = 12, 
#            style = "") {
#     
#     div(class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
#         style = style,
#         Stack(
#           tokens = list(childrenGap = 5),
#           Text(variant = "large", title, block = TRUE),
#           content
#           )
#         )
#   }
# 
# filters <- Stack(
#   tokens = list(childrenGap = 10),
#   Stack(
#     horizontal = TRUE,
#     tokens = list(childrenGap = 10),
#     DatePicker.shinyInput("fromDate", value = as.Date('2020/01/01'), label = "From date"),
#     DatePicker.shinyInput("toDate", value = as.Date('2020/12/31'), label = "To date")
#   ),
#   Label("Filter by sales reps", className = "my_class"),
#   NormalPeoplePicker.shinyInput(
#     "selectedPeople",
#     class = "my_class",
#     options = fluentPeople,
#     pickerSuggestionsProps = list(
#       suggestionsHeaderText = 'Matching people',
#       mostRecentlyUsedHeaderText = 'Sales reps',
#       noResultsFoundText = 'No results found',
#       showRemoveButtons = TRUE
#     )
#   ),
#   Slider.shinyInput("slider",
#                     value = 0, min = 0, max = 1000000, step = 100000,
#                     label = "Minimum amount",
#                     valueFormat = JS("function(x) { return '$' + x}"),
#                     snapToStep = TRUE
#   ),
#   Toggle.shinyInput("closedOnly", value = TRUE, label = "Include closed deals only?")
# )
# 
# 
# analysis_page <- 
#   makePage(
#   title = "Sales representatives", 
#   subtitle = "Best performing reps",
#   div(
#     Stack(
#       horizontal = TRUE,
#       tokens = list(childrenGap = 10),
#       makeCard("Filters", filters, size = 4, style = "max-height: 320px"),
#       makeCard("Deals count", plotlyOutput("plot"), size = 8, style = "max-height: 320px")
#     ),
#     uiOutput("analysis")
#   )
# )
