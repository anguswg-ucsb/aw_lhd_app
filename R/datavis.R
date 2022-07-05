rm(list = ls())

library(dplyr)
library(leaflet)
library(sf)
library(rgdal)
library(knitr)
library(ggplot2)
library(plotly)
library(highcharter)
library(treemap)

# LHD Points and scores, wide format
score_pts   <- readRDS("data/app_data/lhd_score_pts.rds")

# LHD Scores in wide and long format, no geometry 
score       <- readRDS("data/app_data/lhd_score.rds")
score_tidy  <- readRDS("data/app_data/lhd_score_tidy.rds")
score_norm  <- readRDS("data/app_data/lhd_score_normal.rds")

cat_lst       <- c("aquatic_health", "watershed_cond", "public_health", "recreation")
clean_cat_lst <- c("Aquatic Health","Watershed Condition", "Public Health", "Recreation")
rev(clean_cat_lst)
unique(score_norm$clean_cat_id)

lhd_score <- 
  score_norm %>% 
  dplyr::filter(new_id %in% c(1, 156, 484, 784, 5, 9, 7)) %>% 
  dplyr::filter(category %in% c(
    "aquatic_health", "recreation", "watershed_cond"
                                # "public_health"
                                )) %>% 
  dplyr::group_by(clean_cat_id) %>% 
  dplyr::summarize(mean_score = mean(score, na.rm = T)) %>% 
  dplyr::ungroup()

empty_score <- tibble::tibble(
  clean_cat_id = clean_cat_lst[!clean_cat_lst %in% unique(lhd_score$clean_cat_id)]
) %>% 
  dplyr::mutate(mean_score = 0)

lhd_mean <- 
  lhd_score %>% 
  dplyr::bind_rows(empty_score) %>% 
  dplyr::mutate(
    pct = 100*(mean_score/sum(mean_score))
  ) %>% 
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  dplyr::mutate(
    # clean_cat_id = factor(clean_cat_id, levels = c("Recreation", "Public Health", 
    #                                                "Watershed Condition", "Aquatic Health")),
    clean_cat_id = factor(clean_cat_id, levels = c("Recreation", "Public Health", 
                                                   "Watershed Condition", "Aquatic Health")),
    pct          = case_when(
      is.nan(pct) ~ 0,
      TRUE ~ pct
    )
    # category = factor(category, levels = c( "recreation", "public_health", "watershed_cond","aquatic_health"))
  ) 


ggplot(lhd_mean, aes(x = clean_cat_id, y = pct)) +
  geom_segment(aes(x = clean_cat_id, xend = clean_cat_id, y = 0, yend = pct, color = clean_cat_id), size = 2) +
  geom_point(aes(color = clean_cat_id), size = 4) +
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100), expand = c(0, 0)) +
  coord_flip() +
  scale_color_manual(values = c("orange", "darkred","forestgreen", "dodgerblue")) +
  labs(
    y = "Percent of Total score",
    x = "Category",
    color = ""
  ) +
  simple_theme
  # ggthemes::theme_clean()
  # ggthemes::theme_()
  # ggplot2::theme_minimal()
# ggplot2::theme_void()
  # scale_color_manual(values = c("dodgerblue", "orange","forestgreen", "darkred"))
# apatheme
lhd_mean <- 
  lhd_score %>% 
  dplyr::group_by(category) %>% 
  dplyr::summarize(mean_score = mean(score, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
   pct = 100*(mean_score/sum(mean_score))
    ) %>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

hchart(
  lhd_mean,
  "treemap",
       hcaes(x = category, name = category, value = mean_score   , color = pct),
       dataLabels = list(enabled = TRUE),
                         # format='{name}<br/>{point.mean_score} pts<br/>{point.pct} fgpct'),
       tooltip = list(pointFormat = "Score: {point.mean_score}<br>Percent: {point.pct}%")) %>%
  hc_colorAxis(stops = color_stops(colors = viridis::mako(4)))
   # hc_colorAxis(stops = color_stops(colors = viridis::viridis(4)))
# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)
library(viridis)
# Heatmap 
ggplot(data = lhd_score, aes(new_id, category, fill= score_std)) + 
  geom_tile() +
  scale_fill_viridis(option = "H", discrete=FALSE) 

 ggplot(data = lhd_score, aes(category, new_id, fill= score_std)) + 
  geom_tile()
# *******************
# ---- Rank plot ----
# *******************
 tot_score <-
   score_norm %>%
   dplyr::filter(new_id == lhd_id()) %>%
   dplyr::filter(category %in% score_buttons()) %>%
   # dplyr::filter(new_id == 1) %>%
   # dplyr::filter(category %in% input$scoreButtons) %>%
   dplyr::select(-category, -comid, -new_id) %>%
   dplyr::summarise(score = sum(score, na.rm = T))
 # Rank LHD scores and plot
 rank_df <-
   score_norm %>%
   dplyr::group_by(new_id) %>%
   dplyr::filter(category %in% score_buttons()) %>%
   dplyr::summarise(total_score = sum(score, na.rm = T)) %>%
   dplyr::ungroup() %>%
   dplyr::arrange(total_score) %>%
   dplyr::mutate(rank = rank(total_score))
 # ***********************************************************************************
 # ***********************************************************************************
 library(highcharter) # chart
 library(purrr)           # map function to make grouped categories argument
 library(dplyr)           # for select function 
 library(htmltools)   # to add the dependencies, like the plugin what we'll use.
  mydf <- data.frame(Category,SubCategory,sales, color = colorize(Category))
 
 categories_grouped <- map(unique(Category), function(x){
   list(
     name = x,
     categories = SubCategory[Category == x]
   )
 })
 
 df_wide <-
   df %>% 
   tidyr::pivot_wider(
     id_cols     = c(new_id),
     names_from  = clean_cat_id,
     values_from = score
     ) %>% 
   dplyr::mutate(rowid = 1:n())
 
 ah <- 
   df_wide %>% 
   dplyr::select(new_id, `Aquatic Health`) %>% 
   dplyr::mutate(
     cat = "Aquatic Health"
   )
 
 df_wide
 highcharter::highchart() %>% 
   highcharter::hc_add_series(
     data = ah, 
     type = "scatter",
     hcaes(
       x = cat,
       y = `Aquatic Health`
     )
   ) %>% 
   highcharter::hc_xAxis(categories = df$clean_cat_id) 
 library(DT)
 m = cbind(matrix(rnorm(120, 1e5, 1e6), 40), runif(40), rnorm(40, 100))
 colnames(m) = head(LETTERS, ncol(m))
 m
 
 # format the columns A and C as currency, and D as percentages
 datatable(m) %>% formatRound(columns=c('A', 'C'), digits=3)
 # ***********************************************************************************
 # ***********************************************************************************
 library(shiny)
 library(ggplot2) # use mpg dataset in ggplot2
 library(data.table)
 
 dt <- as.data.table(mpg)
 manufacturer_list <- dt[, unique(manufacturer)]
 
 ui <- fluidPage(
    selectInput(inputId = "manufacturer",
                label = "Manufacturer",
                choices = manufacturer_list,
                selected = manufacturer_list[1]
    ),
    selectInput(inputId = "model",
                label = "Model",
                choices = NULL
    ),
    tableOutput("table")
 )
 
 server <- function(input, output, session) {
    filtered_models <- reactive({dt[manufacturer == input$manufacturer, unique(model)]})
    
    observeEvent(filtered_models(), {
       updateSelectizeInput(session, 'model', choices = c(filtered_models()), selected = filtered_models()[1])
    })
    
    filtered_mpg <- reactive({
       # str(input$model)
       dt[manufacturer == input$manufacturer & model == input$model]
    })
    
    output$table <- renderTable(filtered_mpg())
    
 }
 
 shinyApp(ui, server)
 # ***********************************************************************************
 # ***********************************************************************************
 
 library(shiny)
 library(leaflet)
 
 ui=shinyUI(fluidPage(
   
   # Application title
   titlePanel("Test tracks"),
   
   sidebarLayout(
     sidebarPanel(
       checkboxGroupInput(inputId="shps.select", label="Select layer",
                          choices=c("tracks" = "tracks", "MPA" = "mpa")),
       DT::dataTableOutput('x3')),
     
     # Show a plot of the generated distribution
     mainPanel(
       leafletOutput("map", width = "100%", height = 400)
     )
   )
 ))
 
 
 # Global variables 
 
 # Example coords data
 coords <- list(
   c(-122.36075812146,  47.6759920119894),
   c(-122.360781646764, 47.6668890126755),
   c(-122.360782108665,  47.6614990696722),
   c(-122.366199035722, 47.6614990696722),
   c(-122.366199035722,  47.6592874248973),
   c(-122.364582509469, 47.6576254522105),
   c(-122.363887331445,  47.6569107302038),
   c(-122.360865528129, 47.6538418253251),
   c(-122.360866157644,  47.6535254473167),
   c(-122.360866581103, 47.6533126275176),
   c(-122.362526540691,  47.6541872926348),
   c(-122.364442114483, 47.6551892850798),
   c(-122.366077719797,  47.6560733960606),
   c(-122.368818463838, 47.6579742346694),
   c(-122.370115159943,  47.6588730808334),
   c(-122.372295967029, 47.6604350102328),
   c(-122.37381369088,  47.660582362063),
   c(-122.375522972109, 47.6606413027949),
   c(-122.376079703095,  47.6608793094619),
   c(-122.376206315662, 47.6609242364243),
   c(-122.377610811371,  47.6606160735197),
   c(-122.379857378879, 47.6610306942278),
   c(-122.382454873022,  47.6627496239169),
   c(-122.385357955057, 47.6638573778241),
   c(-122.386007328104,  47.6640865692306),
   c(-122.387186331506, 47.6654326177161),
   c(-122.387802656231,  47.6661492860294),
   c(-122.388108244121, 47.6664548739202),
   c(-122.389177800763,  47.6663784774359),
   c(-122.390582858689, 47.6665072251861),
   c(-122.390793942299,  47.6659699214511),
   c(-122.391507906234, 47.6659200946229),
   c(-122.392883050767,  47.6664166747017),
   c(-122.392847210144, 47.6678696739431),
   c(-122.392904778401,  47.6709016021624),
   c(-122.39296705153, 47.6732047491624),
   c(-122.393000803496,  47.6759322346303),
   c(-122.37666945305, 47.6759896300663),
   c(-122.376486363943,  47.6759891899754),
   c(-122.366078869215, 47.6759641734893),
   c(-122.36075812146,  47.6759920119894)
 )
 
 coords <- as.data.frame(do.call("rbind", coords))
 colnames(coords)  = c("longitude", "latitude")
 coords$longitude  = round(coords$longitude, 2)
 
 #create unique id as a test
 coords = transform(coords,id=as.numeric(factor(longitude)))
 # add example dates column
 start <- as.POSIXct(Sys.time())
 end <- start - as.difftime(7, units = "days")
 length <- nrow(coords)
 exampledates <- seq(from = start, to = end, length.out = length)
 coords$timestamp <- exampledates
 #dummy ids
 id = data.frame(matrix(c(1,2,3,4),nrow=4,ncol=2)) 
 colnames(id)  = c("id", "test") 
 
 server = shinyServer(function(input, output) {
   
   output$map <- renderLeaflet({ leaflet() %>% addTiles() %>% setView(-122.37, 47.6659, zoom = 14)})
   
   
   datsel =  reactive({coords})
   idx     = reactive({id})
   output$x3 = DT::renderDataTable(idx(), server = FALSE)
   
   observe({
     
     # Create map
     map <- leafletProxy("map")
     map %>% clearShapes()
     
     # Get select inputs
     shps.select <- input$shps.select # the function is triggered when the select option changes
     
     if (length(shps.select) > 0) {
       
       if ('tracks' %in% shps.select) {
         dat=idx()
         alldat = datsel()
         ids <- input$x3_rows_selected
         
         print(paste(ids))
         #dat1 =  dat[ids, , drop = FALSE]
         
         dat1 =  isolate(paste0(dat[ids, , drop = FALSE][[1]], sep = ""))
         
         
         dat2 =subset(alldat, id %in% dat1)
         
         if (nrow(dat2) == 0){
           return(NULL)}
         
         leafletProxy("map")  %>% addCircles(lng=~longitude, lat= ~latitude, group = "track_id",
                                             data = dat2)
       }
     }  
   })
   
   
 })
 shinyApp(ui = ui, server = server)
 require(tidyverse)
 require(shiny)
 require(shinydashboard)
 require(datasets)
 require(DT)
 require(leaflet)
 
 require(shiny)
 require(leaflet)
 require(DT)
 require(tidyverse)
 
 shiny::shinyApp(
   ui = fluidPage(
     column(
       width = 3,
       br(),
       actionButton(
         "select_all_rows_button",
         "Select All Table Rows"
       ),
       br(),
       actionButton(
         "clear_rows_button",
         "Clear Table Selections"
       )
     ),
     column(
       width = 9,
       fluidRow(
         column(
           width = 12,
           solidHeader = TRUE,
           leafletOutput(
             "my_leaflet"
           )
         )
       ),
       fluidRow(
         column(
           width = 12,
           solidHeader = TRUE,
           DTOutput(
             "my_datatable"
           )
         )
       )
     )
   ),
   
   server = function(session, input, output) {
     
     quakes_r <- reactive({ as_tibble(quakes) })
     
     output$my_datatable <- renderDT({
       
       quakes_r() %>% 
         datatable()
       
     })
     
     
     # base map that we will add points to with leafletProxy()
     output$my_leaflet <- renderLeaflet({
       
       leaflet() %>% 
         addProviderTiles(
           provider = providers$CartoDB.Positron,
           options = providerTileOptions(
             noWrap = FALSE
           )
         ) %>% 
         setView(
           lat = -25.5,
           lng = 178.58,
           zoom = 4
         )
       
     })
     
     observeEvent(input$my_datatable_rows_selected, {
       
       selected_lats <- eventReactive(input$my_datatable_rows_selected, {
         as.list(quakes_r()$lat[c(unique(input$my_datatable_rows_selected))])
       })
       
       selected_longs <- eventReactive(input$my_datatable_rows_selected, {
         as.list(quakes_r()$long[c(unique(input$my_datatable_rows_selected))])
       })
       
       selected_depths <- eventReactive(input$my_datatable_rows_selected, {
         as.list(quakes_r()$depth[c(unique(input$my_datatable_rows_selected))])
       })
       
       selected_mags <- eventReactive(input$my_datatable_rows_selected, {
         as.list(quakes_r()$mag[c(unique(input$my_datatable_rows_selected))])
       })
       
       selected_stations <- eventReactive(input$my_datatable_rows_selected, {
         as.list(quakes_r()$stations[c(unique(input$my_datatable_rows_selected))])
       })
       
       # this is the data that will be passed to the leaflet in the addCircleMarkers argument,
       # as well as the popups when the points are hovered over
       map_df <- reactive({
         tibble(lat = unlist(selected_lats()),
                lng = unlist(selected_longs()),
                depth = unlist(selected_depths()),
                mag = unlist(selected_mags()),
                stations = unlist(selected_stations()))
       })
       
       leafletProxy("my_leaflet", session) %>% 
         clearMarkers() %>% 
         addCircleMarkers(
           data = map_df(),
           lng = ~lng,
           lat = ~lat,
           fillColor = "blue",
           stroke = TRUE,
           color = "white",
           radius = 3,
           weight = 1,
           fillOpacity = 0.4,
           popup = paste0("lat: ", map_df()$lat, "<br>",
                          "lng: ", map_df()$lng, "<br>",
                          "depth: ", map_df()$depth, "<br>",
                          "mag: ", map_df()$mag, "<br>",
                          "stations: ", map_df()$stations)
         )
       
     })
     
     # create a proxy to modify datatable without recreating it completely
     DT_proxy <- dataTableProxy("my_datatable")
     
     # clear row selections when clear_rows_button is clicked
     observeEvent(input$clear_rows_button, {
       selectRows(DT_proxy, NULL)
     })
     
     # clear markers from leaflet when clear_rows_button is clicked
     observeEvent(input$clear_rows_button, {
       clearMarkers(leafletProxy("my_leaflet", session))
     })
     
     # select all rows when select_all_rows_button is clicked
     observeEvent(input$select_all_rows_button, {
       selectRows(DT_proxy, input$my_datatable_rows_all)
     })
     
   }
 )







