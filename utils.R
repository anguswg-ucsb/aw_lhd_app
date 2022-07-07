# # ---- LHD basemap ----
# basemap <- function(
#   pts
# )
# {
#  
#   # LEAFLET MAP
#   leaflet::leaflet() %>%
#     leaflet::addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
#     leaflet::addCircleMarkers(
#       data        = pts,
#       fillColor   = "grey",
#       fillOpacity = 0.5,
#       radius      = 4,
#       color       = "black",
#       stroke      = TRUE,
#       weight      = 1,
#       layerId     = ~new_id,
#       group       = "regions",
#       label       = ~new_id
#     ) %>% 
#     leaflet::addCircleMarkers(
#       data        = pts,
#       fillColor   = "red",
#       fillOpacity = 0.5,
#       radius      = 4,
#       color       = "black",
#       stroke      = TRUE,
#       weight      = 1,
#       layerId     = ~map_id,
#       group       = ~new_id
#       # label       = ~map_id
#     ) %>%
#     leaflet::hideGroup(group = pts$new_id)
#  
#   
#     # leaflet::addCircleMarkers(
#     #   data         = pts,
#     #   # data         = score_pts,
#     #   # color        = "black",
#     #   fillColor    = "dodgerblue",
#     #   opacity      = 1,
#     #   radius       = 7,
#     #   fillOpacity  = 0.5,
#     #   weight       = 3,
#     #   stroke       = TRUE,
#     #   layerId      = ~new_id,
#     #   group        = "lhd_base",
#     #   label        = ~new_id
#     # )  
#   
# 
# }
# ---- Color LHD basemap ----
basemap <- function(
  pts
)
{
  

  # pal <- leaflet::colorFactor(
  #   palette = colorRampPalette(RColorBrewer::brewer.pal(length(ulegend), "Paired"))(length(ulegend)), 
  #   domain  = ulegend
  # )
 
  # pts <- score_pts
  # HTML Labels for markers
  labels <- sprintf(
    "<strong>ID:</strong> %s<br/><strong>Stream: </strong>%s<br/><strong>Ownership: </strong> %s",
    pts$new_id, pts$stream_name, pts$legend
  ) %>%
    lapply(htmltools::HTML)

  # LEAFLET MAP
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(
      providers$OpenStreetMap,
      group    = "Topographic",
      options  = leaflet::providerTileOptions(noWrap = FALSE)) %>%
    leaflet::addProviderTiles(
      providers$Esri.WorldImagery, 
      group    = "Imagery",
      options  = leaflet::providerTileOptions(noWrap = FALSE)) %>%
    leaflet::addLayersControl(
      baseGroups = c("Topographic", "Imagery"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>% 
    # leaflet::addProviderTiles(
    #   provider = providers$CartoDB.Positron,
    #   options  = leaflet::providerTileOptions(
    #     noWrap = FALSE
    #   )
    # ) %>% 
    leaflet::addCircleMarkers(
      data        = pts,
      fillColor   = "#B2BEB5",
      fillOpacity = 0.6,
      radius      = 7,
      color       = "black",
      stroke      = TRUE,
      weight      = 1,
      layerId     = ~new_id,
      group       = "regions",
      # label       = ~new_id
      label       = labels,
        labelOptions = labelOptions(
          style     = list("font-weight" = "normal", padding = "3px 8px"),
          textsize  = "15px",
          direction = "auto"
          )
    ) %>% 
    leaflet::addCircleMarkers(
      data        = pts,
      fillColor   = "red",
      fillOpacity = 0.7,
      radius      = 7,
      color       = "black",
      stroke      = TRUE,
      weight      = 1,
      layerId     = ~map_id,
      group       = ~new_id,
      label       = labels,
        labelOptions = labelOptions(
          style     = list("font-weight" = "normal", padding = "3px 8px"),
          textsize  = "15px",
          direction = "auto"
          )
      # label       = ~map_id
    ) %>%
    leaflet::hideGroup(group = pts$new_id) %>% 
    leaflet::setView(-105.5475, 38.99716, zoom = 7)
    # leaflet::addCircleMarkers(
    #   data        = pts,
    #   fillColor   = ~pal(legend),
    #   # fillOpacity = 0.5,
    #   fillOpacity = 0.8,
    #   # radius      = 4,
    #   color       = "black",
    #   opacity     = 1,
    #   weight      = 1,
    #   stroke      = T,
    #   # label       = ~legend
    #   layerId     = ~new_id,
    #   group       = "regions",
    #   label       = labels,
    #   labelOptions = labelOptions(
    #     style     = list("font-weight" = "normal", padding = "3px 8px"),
    #     textsize  = "15px",
    #     direction = "auto"
    #     )
    # ) %>% 
    # leaflet::addLegend(
    #   pal    = pal, 
    #   values = ulegend,
    #   title  = "Owner"
    #   ) %>% 
    # leaflet::addCircleMarkers(
    #   data        = pts,
    #   fillColor   = "black",
    #   fillOpacity = 0.6,
    #   # radius      = 4,
    #   color       = "black",
    #   stroke      = TRUE,
    #   weight      = 1,
    #   layerId     = ~map_id,
    #   group       = ~new_id
    #   # label       = ~map_id
    # ) %>%
    # leaflet::hideGroup(group = pts$new_id)
    # leaflet::addCircleMarkers(
    #   data        = pts,
    #   fillColor   = "grey",
    #   fillOpacity = 0.5,
    #   radius      = 4,
    #   color       = "black",
    #   stroke      = TRUE,
    #   weight      = 1,
    #   layerId     = ~new_id,
    #   group       = "regions",
    #   label       = ~new_id
    # ) %>% 
    # leaflet::addCircleMarkers(
    #   data        = pts,
    #   fillColor   = "red",
    #   fillOpacity = 0.5,
    #   radius      = 4,
    #   color       = "black",
    #   stroke      = TRUE,
    #   weight      = 1,
    #   layerId     = ~map_id,
    #   group       = ~new_id
    #   # label       = ~map_id
    # ) %>%
    # leaflet::hideGroup(group = pts$new_id)
  
  
  
  
}

# ---- Color LHD basemap ----
tablemap <- function(
  pts
)
{
  # pts <- score_pts
  # ulegend <- unique(pts$legend)
  # 
  # 
  # pal <- leaflet::colorFactor(
  #   palette = colorRampPalette(RColorBrewer::brewer.pal(length(ulegend), "Paired"))(length(ulegend)),
  #   domain  = ulegend
  # )
  # pts <- top_rank
  pal <- leaflet::colorNumeric(
    palette = colorRampPalette(
      rev(RColorBrewer::brewer.pal(9,  "YlOrRd")))(length(unique(pts$Rank))),
    domain  = unique(pts$Rank)
  )

  # RColorBrewer::display.brewer.all()
   # pts <- top_rank
   # pts <- score_pts

    # pal <- colorNumeric(
    #   palette = colorRampPalette(viridisLite::magma(length(unique(pts$Rank)), direction = 1))(length(unique(pts$Rank))),
    #   domain  = pts$Rank
    # )
    # pal <- colorNumeric(
    #   palette = colorRampPalette(viridisLite::mako(length(unique(pts$Rank)), direction = -1))(length(unique(pts$Rank))),
    #   domain  = pts$Rank
    # )
   # HTML Labels for markers
   labels <- sprintf(
     "<strong>ID: %s</strong><br/><strong>Rank: </strong>%s<br/><strong>Stream: </strong>%s<br/><strong>Ownership: </strong>%s",
     pts$ID, pts$Rank,pts$Stream, pts$Ownership
   ) %>%
     lapply(htmltools::HTML)
   
   # labels <- sprintf(
   #   "<strong>ID:</strong> %s<br/><strong>Stream: </strong>%s<br/><strong>Ownership: </strong> %s",
   #   pts$new_id, pts$stream_name, pts$legend
   # ) %>%
   #   lapply(htmltools::HTML)
  #   pts  <- top_rank
  # LEAFLET MAP
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(
      providers$OpenStreetMap,
      group    = "Topographic",
      options  = leaflet::providerTileOptions(noWrap = FALSE)) %>%
    leaflet::addProviderTiles(
      providers$Esri.WorldImagery, 
      group    = "Imagery",
      options  = leaflet::providerTileOptions(noWrap = FALSE)) %>%
    leaflet::addLayersControl(
      baseGroups = c("Topographic", "Imagery"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>% 
    leaflet::addCircleMarkers(
      data        = pts,
      # lng         = pts$lng,
      # lat         = pts$lat,
      lng          = ~pts$Longitude, 
      lat          = ~pts$Latitude,
      # fillColor   = "grey",
      fillColor = pal(pts$Rank),
      fillOpacity = 0.7,
      color       = "black",
      opacity     = 1,
      radius      = 7,
      weight      = 1,
      stroke      = T,
      # layerId     = ~pts$ID,
      layerId     = as.character(pts$ID),
      group       = "regions",
      label       = labels,
        labelOptions = labelOptions(
          style     = list("font-weight" = "normal", padding = "3px 8px"),
          textsize  = "15px",
          direction = "auto"
        )
    ) %>% 
    leaflet::addLegend(
      pal    = pal,
      values = pts$Rank,
      title  = "Rank", 
      position = "bottomleft"
      ) %>%
    leaflet::setView(-105.5475, 38.99716, zoom = 7)
    # leaflet::addCircleMarkers(
    #   data        = pts,
    #   # fillColor   = ~pal(legend),
    #   fillColor   = "grey",
    #   fillOpacity = 0.5,
    #   # radius      = 4,
    #   color       = "black",
    #   opacity     = 1,
    #   weight      = 1,
    #   stroke      = T,
    #   # label       = ~legend
    #   layerId     = ~new_id,
    #   group       = "regions",
    #   label       = labels,
    #   labelOptions = labelOptions(
    #     style     = list("font-weight" = "normal", padding = "3px 8px"),
    #     textsize  = "15px",
    #     direction = "auto"
    #   )
    # ) %>% 
  # leaflet::addCircleMarkers(
  #   data        = pts,
  #   fillColor   = "red",
  #   fillOpacity = 0.5,
  #   # radius      = 4,
  #   color       = "black",
  #   stroke      = TRUE,
  #   weight      = 1,
  #   layerId     = ~map_id,
  #   group       = ~new_id,
  #   label       = labels,
  #   labelOptions = labelOptions(
  #     style     = list("font-weight" = "normal", padding = "3px 8px"),
  #     textsize  = "15px",
  #     direction = "auto"
  #   )
  # ) %>%

}
# ---- Flex Table Theme ----
# Flex Table Theme for scoring table

theme_design <- function(x) {
  x <- border_remove(x)
  std_border <- fp_border_default(width = 4, color = "white")
  x <- fontsize(x, size = 16, part = "all")
  # x <- font(x, fontname = "Courier", part = "all")
  x <- align(x, align = "center", part = "all")
  x <- bold(x, bold = TRUE, part = "all")
  x <- bg(x, bg = "#dcdcdc", part = "body")
  x <- bg(x, bg = "#a9a9a9", part = "header")
  x <- bg(x, bg = "#1bbbda", part = "footer")
  x <- color(x, color = "black", part = "all")
  x <- padding(x, padding = 6, part = "all")
  x <- border_outer(x, part="all", border = std_border )
  x <- border_inner_h(x, border = std_border, part="all")
  x <- border_inner_v(x, border = std_border, part="all")
  x <- set_table_properties(x, layout = "fixed")
  x
}


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Robust scalar normalization
robust_scalar<- function(x){
  (x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))
}

# Min-Max Normalization
norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}

# Mean Normalization
mean_norm_minmax <- function(x){
  (x- mean(x)) /(max(x)-min(x))
}

# Makes table of Node IDs
make_score_table <- function(score_data) {
  
  # node_data <- node_table_data %>%
  #   filter(node_id == "7200938"

  score_table <-
    reactable::reactable(
      score_data,
      style    = list(fontFamily = "Work Sans, sans-serif", fontSize = "12px", fontWeight = 600),
      columns  = list(
        clean_cat_id   = colDef(
                        name  = "Category",
                        align = "center"),
        score          = colDef(
                        name  = "Score",
                        align = "center",
                      )
      ),
      highlight = TRUE,
      outlined  = TRUE,
      bordered  = T,
      theme     = reactableTheme(
        borderColor = "#black",
        cellStyle   = list(
          display         = "flex",
          flexDirection   = "column",
          justifyContent  = "center"
        ),
        headerStyle = list(
          backgroundColor = "hsl(207, 16%, 80%)"
        )
      )
    )
  # %>% add_subtitle("Structure Information",align = "center",  font_size = 16, margin = 3)
  
  return(score_table)
}
# Making a theme
apatheme <-
  theme_bw()+
  theme(
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # panel.border     = element_blank(),
    axis.line        = element_line(),
    text             = element_text(family='Helvetica'),
    legend.title     = element_blank(),
    plot.title       = element_text(color = "black", face = "bold"),
    axis.title       = element_text(color = "black", face = "bold"),
    axis.text.y      = element_text(color = "black"),
    axis.text.x      = element_text(color = "black"),
    strip.text.x     = element_text(color = "black",face = "bold"),
    strip.text.y     = element_text(color = "black",face = "bold")
  )
# Making a theme
simple_theme <-
  theme_classic()+
  theme(
    # panel.grid.major = element_line(colour = "grey", linetype = "dashed"),
    # panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # panel.border     = element_blank(),
    axis.line        = element_line(),
    text             = element_text(family='Helvetica'),
    legend.title     = element_blank(),
    legend.position  = "none",
    plot.title       = element_text(color = "black", face = "bold"),
    axis.title       = element_text(color = "black", face = "bold"),
    axis.text.y      = element_text(color = "black", size = 12),
    axis.text.x      = element_text(color = "black", size = 12),
    strip.text.x     = element_text(color = "black",face = "bold"),
    strip.text.y     = element_text(color = "black",face = "bold")
  )

# ---- Category Rank Plot ----
category_rank_plot <- function(
  df, 
  lhd_ids     = selected$groups, 
  interactive = FALSE) {
  
  # plotly font
  font <- list(
    family = "Helvetica",
    size  = 12,
    color = "black"
  )
  
  # plotly label
  label <- list(
    bgcolor = "white",
    bordercolor = "transparent",
    font = font
  )
  
  # df <- category_rank_df
  # lhd_ids <- character(0)
  # lhd_ids <- c("1", "2", "3")
  
  # Category colors
  cat_colors <- c("dodgerblue", "darkred","orange", "forestgreen")
  # df <- category_rank_df
  # Assign colors to plot categories
  names(cat_colors) <- levels(
                            factor(c(levels(df$clean_cat_id)))
                            ) 
 # ggplot2::scale_fill_manual(name = clean_cat_id, values = cat_colors)  
  if(is.null(lhd_ids) | length(lhd_ids) == 0) {
    
    logger::log_info("null/character0 - category plot")
    
    category_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_point(
        data =  df,
        aes(
          x     = clean_cat_id,
          y     = Score, 
          color = clean_cat_id,
          # text  = paste0("Ownership: ", legend)
          text  = paste0("ID: ", new_id, 
                         "\nStream: ", stream_name, 
                         "\nOwnership: ", legend, 
                         "\nScore: ", Score)
          ),
        alpha = 0.5,
        size  = 1.5
        ) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(breaks =  seq(0, (max(df$Score) + 0.1), by = 1), 
                                  limits = c(0, (max(df$Score) + 0.1), 1)) +
      ggplot2::labs(
        y = "Score",
        x = "",       
        color = ""
      )  +
      ggplot2::scale_color_manual(name = "clean_cat_id", values = cat_colors) +
      # ggplot2::scale_color_manual(values = c("orange", "darkred","forestgreen", "dodgerblue")) +
      simple_theme + 
      ggplot2::guides(color =  ggplot2::guide_legend(reverse=F))
    
  } else {
    
    logger::log_info("NOT null/character0 - category plot")
    
     # df <- category_rank_df 
    # dplyr::filter(clean_cat_id == "Recreation")
     
      category_plot <-
        ggplot2::ggplot() +
          ggplot2::geom_point(
            data =  dplyr::filter(df, !new_id %in% lhd_ids),
            # data =  dplyr::filter(df, !new_id %in% c(2, 3)),
            # data =  dplyr::filter(df, !new_id %in% lsty),
            aes(
              x     = clean_cat_id,
              y     = Score, 
              color = clean_cat_id,
              text  = paste0("ID: ", new_id, 
                             "\nStream: ", stream_name, 
                             "\nOwnership: ", legend, 
                             "\nScore: ", Score)
              # text  = paste0("Ownership: ", legend)
            ),
            alpha = 0.5,
            size  = 1.5) +
          ggplot2::geom_point(
            data    =  dplyr::filter(df, new_id %in% lhd_ids),
            # data    =  dplyr::filter(df, new_id %in% c(2, 3)),
            # data    =  dplyr::filter(df, new_id %in% lsty),
            aes(
              x     = clean_cat_id, 
              y     = Score,
              text  = paste0("ID: ", new_id, 
                             "\nStream: ", stream_name, 
                             "\nOwnership: ", legend, 
                             "\nScore: ", Score)
              # text  = paste0("Ownership: ", legend)
            ),
            color = "black", 
            alpha = 0.7,
            size  = 3.5) +
          ggplot2::coord_flip() +
          ggplot2::scale_y_continuous(breaks =  seq(0, (max(df$Score) + 0.1), by = 1), 
                               limits = c(0, (max(df$Score) + 0.1), 1)
                               # expand = c(1, 0)
            ) +
          ggplot2::labs(
            y = "Score",
            x = "",       
            color = ""
            )  +
          ggplot2::scale_color_manual(name = "clean_cat_id", values = cat_colors) +
          # ggplot2::scale_color_manual(values = c("orange", "darkred","forestgreen", "dodgerblue")) +
          simple_theme +
          ggplot2::guides(color =  ggplot2::guide_legend(reverse=F))
      
  }
  
  if (interactive  == FALSE) {
    
    logger::log_info("NOT Interactive - category plot")
    
    return(category_plot)
    
    } else if(interactive == TRUE) {
      
      logger::log_info("Interactive - category plot")
      
      # interactive plot
      category_plot <-
        category_plot %>%
        plotly::ggplotly(tooltip =  c("text")) %>%
        plotly::style(hoverlabel = label) %>%
        plotly::layout(
          font = font,
          legend  = list(
              title   = list(size = 12),
              font    = list(size = 12)),
          xaxis   = list(
              title   = list(
              text    = "Score",
              font    = list(size = 12)),
            tickfont  = list(size = 12)),
          yaxis   = list(
            titlefont = list(size = 12), 
            tickfont  = list(size = 12)
            )
          )
        return(category_plot)
        
        }
}

# ---- Score Rank Plot ----

score_rank_plot <- function(
  df, 
  lhd_ids     = selected$groups, 
  interactive = FALSE) {
  
  # plotly font
  font <- list(
    family = "Helvetica",
    size  = 12,
    color = "black"
  )
  
  # plotly label
  label <- list(
    bgcolor = "white",
    bordercolor = "transparent",
    font = font
  )

  # cat(paste0("ID: ", df$ID[1],   "\nStream: ", df$stream_name[1],   "\nOwnership: ", df$legend[1],  "\nTotal Score: ", df$total_score[1] ))
  if(is.null(lhd_ids) | length(lhd_ids) == 0) {
    
    logger::log_info("null/character0 - rank plot")
    # df <- rank_df
    rank_scores_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_point(
        data = df,
        # data = dplyr::filter(df, !new_id %in% c(2, 3)),
        aes(
          y = total_score,
          x = rank,
          text  = paste0("ID: ", new_id, 
                         "\nStream: ", stream_name, 
                         "\nOwnership: ", legend, 
                         "\nTotal Score: ", total_score)
        ), 
        color = "black",
        size  = 1.5) +
      ggplot2::labs(
        x = "Rank",
        y = "Total Score")  +
      ggplot2::scale_y_continuous(breaks =  seq(0, (max(df$total_score) + 1), by = 1), 
                                  limits = c(0, (max(df$total_score) + 1), 1),
                                  expand = c(0, 0)) +
      apatheme # simple_theme
    
  } else {
    
    logger::log_info("NOT null/character0 - rank plot")
    # df <- rank_df
    rank_scores_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_point(
        data = dplyr::filter(df, !new_id %in% lhd_ids),
        # data = dplyr::filter(df, !new_id %in% c(2, 3)),
        aes(
          y = total_score,
          x = rank,
          text  = paste0("ID: ", new_id, 
                           "\nStream: ", stream_name, 
                           "\nOwnership: ", legend, 
                           "\nTotal Score: ", total_score)
          ), 
        color = "black",
        size  = 1.5) +
      ggplot2::geom_point(
        data = dplyr::filter(df, new_id %in% lhd_ids),
        # data = dplyr::filter(df, new_id %in% c(2, 3)),
        aes(
          y = total_score,
          x = rank,
          text  = paste0("ID: ", new_id, 
                         "\nStream: ", stream_name, 
                         "\nOwnership: ", legend, 
                         "\nTotal Score: ", total_score)
          ),  
        color = "red",
        alpha = 0.7,
        size  = 3.5) +
      ggplot2::labs(
        x = "Rank",
        y = "Total Score")  +
      ggplot2::scale_y_continuous(breaks =  seq(0, (max(df$total_score) + 1), by = 1), 
                         limits = c(0, (max(df$total_score) + 1), 1),
                         expand = c(0, 0)) +
      apatheme # simple_theme
  }
  if (interactive  == TRUE) {
    
    logger::log_info("Interactive rank plot")
    
    # interactive plot
    rank_scores_plot <-
      rank_scores_plot %>%
      plotly::ggplotly(tooltip =  c("text")) %>%
      # plotly::ggplotly(tooltip =  c("y", "x")) %>%
      plotly::style(hoverlabel = label) %>%
      plotly::layout(
        font = font,
        legend = list(
          title = list(size = 12),
          font = list(size = 12)),
        xaxis  = list(
          title=list(
            text = "Rank",
            font = list(size = 12)),
          tickfont = list(size = 12)),
        # titlefont = list(size = 5)
        yaxis  = list(
          title=list(
            text = "Total Score",
            font = list(size = 12)),
          tickfont = list(size = 12))
        # yaxis = list(
        #   titlefont = list(size = 12), 
        #   tickfont  = list(size = 12)
        # )
      )
    
    return(rank_scores_plot)
    
  } else if(interactive == FALSE) {
    
    logger::log_info("NOT Interactive rank plot")
    
    return(rank_scores_plot)

  }
  
}

score_mean_plot <- function(df, interactive = FALSE) {
  # plotly font
  font <- list(
    family = "Helvetica",
    size = 12,
    color = "black"
  )
  
  # plotly label
  label <- list(
    bgcolor = "white",
    bordercolor = "transparent",
    font = font
  )
  
  # mean scores plot
  mean_plot <- 
    ggplot2::ggplot(df, aes(x = clean_cat_id, y = Percent)) +
    ggplot2::geom_segment(aes(x = clean_cat_id, xend = clean_cat_id, 
                     y = 0, yend = Percent, color = clean_cat_id), size = 1) +
    ggplot2::geom_point(aes(color = clean_cat_id), size = 2) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), 
                       limits = c(0, 100), 
                       expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("orange", "darkred","forestgreen", "dodgerblue")) +
    ggplot2::labs(
      y = "Percent of Total score",
      x = "",
      color = ""
    ) +
    simple_theme + 
    ggplot2::guides(color =  ggplot2::guide_legend(reverse=F))
  
  if (interactive  == TRUE) {
    
    # interactive plot
    mean_plot <-
      mean_plot %>%
      plotly::ggplotly(tooltip =  c("y")) %>%
      plotly::style(hoverlabel = label) %>%
      plotly::layout(
        font = font,
        legend = list(
          title = list(size = 12),
          font = list(size = 12)),
        xaxis  = list(
          title=list(
            text = "Percent of Total score",
            font = list(size = 12)),
        # titlefont = list(size = 5), 
        tickfont = list(size = 12)),
        yaxis = list(
          titlefont = list(size = 12), 
          tickfont = list(size = 12)
        )
      )
    
    return(mean_plot)
    
  } else if(interactive == FALSE) {
    
    return(mean_plot)
    
  }
}

