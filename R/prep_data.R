rm(list = ls())

library(tidyverse)
library(highcharter)

raw_path   <- list.files("data/raw", full.names = T)
final_path <- "data/app_data"
# ****************************************
# ---- Summarize & Clean scoring data ----
# ****************************************

aquatic <- readRDS(raw_path[1])
public  <- readRDS(raw_path[3])
rec     <- readRDS(raw_path[4])
ws_cond <- readRDS(raw_path[5])

# Join Scores data together
scores <-
  aquatic %>%
  dplyr::select(-tot_hci) %>%
  dplyr::left_join(
      dplyr::select(ws_cond, new_id, wc_tot_score),
    by = "new_id"
    ) %>%
  dplyr::left_join(
    dplyr::mutate(
      dplyr::select(sf::st_drop_geometry(public), new_id, ph_tot_score),
      new_id = as.character(new_id)
      ),
    by = "new_id"
    ) %>%
  dplyr::left_join(
    dplyr::mutate(
      dplyr::select(rec, new_id, rec_tot_score),
      new_id = as.character(new_id)
      ),
    by = "new_id"
    )

# LHD points and IDs
lhd_pts <-
  readRDS(raw_path[2]) %>%
  sf::st_as_sf() %>%
  dplyr::select(new_id, comid, geometry)

score_pts <-
  scores %>%
  dplyr::left_join(
    lhd_pts, by = "new_id"
    ) %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
  dplyr::relocate(new_id, comid) %>% 
  setNames(c("new_id", "comid", "connecitivity", "aquatic_health", 
             "watershed_cond", "public_health", "recreation", "geometry")) %>% 
  dplyr::mutate(new_id = as.numeric(new_id)) %>% 
  dplyr::arrange(new_id) %>% 
  dplyr::mutate(
    map_id = paste0("map_id_", as.character(1:n())),
    new_id = as.character(new_id)
  ) %>% 
  dplyr::relocate(new_id, map_id, comid)

saveRDS(score_pts, paste0(final_path, "/lhd_score_pts.rds"))

scores_no_sf <-
  scores %>%
  dplyr::left_join(
    lhd_pts, by = "new_id"
  ) %>%
  sf::st_as_sf() %>%
  sf::st_drop_geometry() %>% 
  dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
  dplyr::relocate(new_id, comid) %>%  
  setNames(c("new_id", "comid", "connecitivity", "aquatic_health", 
             "watershed_cond", "public_health", "recreation")) %>% 
  dplyr::mutate(new_id = as.numeric(new_id)) %>% 
  dplyr::arrange(-new_id) %>% 
  dplyr::mutate(new_id = as.numeric(new_id)) %>% 
  dplyr::arrange(new_id) %>% 
  dplyr::mutate(
    map_id = paste0("map_id_", as.character(1:n())),
    new_id = as.character(new_id)
  ) %>% 
  dplyr::relocate(new_id, map_id, comid)

saveRDS(scores_no_sf, paste0(final_path, "/lhd_score.rds"))

# scores long dataframe 
scores_long <-  
  scores %>%
  dplyr::left_join(
    lhd_pts, by = "new_id"
  ) %>%
  sf::st_as_sf() %>%
  sf::st_drop_geometry() %>%   
  dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
  dplyr::relocate(new_id, comid) %>% 
  setNames(c("new_id", "comid", "connecitivity", "aquatic_health", 
             "watershed_cond", "public_health", "recreation")) %>% 
  dplyr::mutate(new_id = as.numeric(new_id)) %>% 
  dplyr::arrange(-new_id) %>% 
  dplyr::mutate(new_id = as.numeric(new_id)) %>% 
  dplyr::arrange(new_id) %>% 
  dplyr::mutate(
    map_id = paste0("map_id_", as.character(1:n())),
    new_id = as.character(new_id)
  ) %>% 
  dplyr::relocate(new_id, map_id, comid) %>% 
  tidyr::pivot_longer(
    cols      = c(connecitivity:recreation),
    names_to  = "category",
    values_to = "score"
    ) %>% 
  dplyr::mutate(
    clean_cat_id = case_when(
      category == "connecitivity"   ~ "Connecitivity",
      category == "aquatic_health"  ~ "Aquatic Health",
      category == "watershed_cond"  ~ "Watershed Condition",
      category == "public_health"   ~ "Public Health",
      category == "recreation"      ~ "Recreation"
    )
  ) %>% 
  dplyr::relocate(new_id, map_id, comid, category, clean_cat_id, score) 

saveRDS(scores_long, paste0(final_path, "/lhd_score_tidy.rds"))

# Scores normalize
score_norm <- 
  scores %>%
  dplyr::left_join(
    lhd_pts, by = "new_id"
  ) %>%
  sf::st_as_sf() %>%
  sf::st_drop_geometry() %>%   
  dplyr::relocate(new_id, comid) %>% 
  setNames(c("new_id", "comid", "connecitivity", "aquatic_health", 
             "watershed_cond", "public_health", "recreation")) %>% 
  dplyr::mutate(new_id = as.numeric(new_id)) %>% 
  dplyr::arrange(-new_id) %>% 
  dplyr::mutate(new_id = as.numeric(new_id)) %>% 
  dplyr::arrange(new_id) %>% 
  dplyr::mutate(
    map_id = paste0("map_id_", as.character(1:n())),
    new_id = as.character(new_id)
  ) %>% 
  dplyr::relocate(new_id, map_id, comid) %>% 
  tidyr::pivot_longer(
    cols      = c(connecitivity:recreation),
    names_to  = "category",
    values_to = "score"
  ) %>% 
  dplyr::mutate(
    clean_cat_id = case_when(
      category == "connecitivity"   ~ "Connecitivity",
      category == "aquatic_health"  ~ "Aquatic Health",
      category == "watershed_cond"  ~ "Watershed Condition",
      category == "public_health"   ~ "Public Health",
      category == "recreation"      ~ "Recreation"
    )
  ) %>% 
  dplyr::relocate(new_id, map_id, comid, category, clean_cat_id, score) %>% 
  dplyr::group_by(category) %>% 
  dplyr::mutate(score_std = normalize(score)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(across(where(is.numeric), round, 4)) 

saveRDS(score_norm, paste0(final_path, "/lhd_score_normal.rds"))

# ********************************************************
# ********************************************************
#define vector of data values
data <- c(3, 5, 6, 7, 500)

#define vector of weights
weights <- c(.1, .3, .3, .2, 1)
mean(data)
#calculate weighted mean
weighted.mean(x=data, w=weights)


highchart() %>%
  hc_add_series(
    # data        = ws_wide,
    # data        = rank_df,
    data = dplyr::filter(rank_df, new_id != 1),
    name        = "Total shortage",
    type        = 'point',
    tooltip     = list(pointFormat = "LHD ID: {point.new_id}"),
    fillOpacity = 0.3,
    hcaes(
      x  = rank,
      y  = total_score
    )
  ) %>% 
  hc_add_series(
    # data        = ws_wide,
    # data        = rank_df,
    data = dplyr::filter(rank_df, new_id == 1),
    name        = "Total shortage",
    type        = 'point',
    tooltip     = list(pointFormat = "LHD ID: {point.new_id}"),
    fillOpacity = 1,
    hcaes(
      x  = rank,
      y  = total_score
    )
  ) %>% 
  hc_colors(c("black",  "red")) %>%
  hc_chart(
    plotBorderWidth = 0.5,
    plotBorderColor = '#b4b4b4',
    height          = NULL)

# ********************************************************
# ********************************************************
df<-structure(list(filename = c("FINO3_FINO3_MMS_GE.txt", "HKW_A_LFL_NL.txt", 
                                "KFL_KF_MMS_UK.txt", "NRMD_RAMPION_MMS_UK.txt", "THOR_THOR_LFL_DK.txt", 
                                "VHP_Høvsøre_MML_DK.txt", "WEX_WC_FLS_UK.txt"), project = c("FINO3_FINO3_MMS_GE", 
                                                                                            "HKW_A_LFL_NL", "KFL_KF_MMS_UK", "NRMD_RAMPION_MMS_UK", "THOR_THOR_LFL_DK", 
                                                                                            "VHP_Høvsøre_MML_DK", "WEX_WC_FLS_UK"), lat = c(55.1852777777778, 
                                                                                                                                            52.6252777777778, 51.446272, 50.688111, 56.34703, 56.4405, 54.136111
                                                                                            ), lng = c(7.15138888888889, 3.96027777777778, 1.078051, -0.343189, 
                                                                                                       7.60513, 8.1508, -3.919722)), row.names = c(NA, -7L), class = "data.frame")

#load shapefile
# nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
#   st_transform(4326)

shinyApp(
  ui = fluidPage(
    
    "Update selectize input by clicking on the map",
    
    leafletOutput("map"),
    selectizeInput(inputId = "selected_locations",
                   label = "selected",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE)
  ),
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    #selected_ids <- reactiveValues(ids = vector())
    
    #initial map output
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          data = df,
          fillColor = "white",
          fillOpacity = 0.5,
          color = "black",
          stroke = TRUE,
          weight = 1,
          layerId = ~project,
          group = "regions",
          label = ~project)%>%
        addCircleMarkers(
          data = df,
          fillColor = "red",
          fillOpacity = 0.5,
          color = "black",
          stroke = TRUE,
          weight = 1,
          layerId = ~filename,
          group = ~project,
          label = ~filename)%>%
        hideGroup(group = df$project)%>%
        setView(lat = 55,lng = 7,zoom = 5)
    })
    
    proxy <- leafletProxy("map")
    selected <- reactiveValues(groups = vector())
    
    observeEvent(input$map_marker_click, {
      if(input$map_marker_click$group == "regions"){
        selected$groups <- c(selected$groups, input$map_marker_click$id)
        proxy %>% showGroup(group = input$map_marker_click$id)
      } else {
        selected$groups <- setdiff(selected$groups, input$map_marker_click$group)
        proxy %>% hideGroup(group = input$map_marker_click$group)
      }
      updateSelectizeInput(session,
                           inputId = "selected_locations",
                           label = "",
                           choices = df$project,
                           selected = selected$groups)
    }) 
    
    
    observeEvent(input$selected_locations, {
      removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
      added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
      
      if(length(removed_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% hideGroup(group = removed_via_selectInput)
      }
      
      if(length(added_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% showGroup(group = added_via_selectInput)
      }
    }, ignoreNULL = FALSE) 
    
    
  }
  
)




