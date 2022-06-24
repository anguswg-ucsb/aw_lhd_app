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
  
  # legend_cols <- 
  #   pts %>% 
  #   dplyr::group_by(legend) %>% 
  #   dplyr::slice(1) %>% 
  #   dplyr::select(new_id, map_id, legend, pt_color)
  
  ulegend <- unique(pts$legend)
  
  # pal <- colorFactor( palette = colorRampPalette(legend_cols$pt_color)(length(legend_cols$pt_color)), 
  #   domain  = legend_cols$legend )
  
  # pal <- colorFactor(palette = colorRampPalette(viridisLite::turbo(nrow(legend_cols)))(length(legend_cols$pt_color)), 
  #   domain  = legend_cols$legend)

  pal <- leaflet::colorFactor(
    palette = colorRampPalette(RColorBrewer::brewer.pal(length(ulegend), "Paired"))(length(ulegend)), 
    domain  = ulegend
  )
  
  # HTML Labels for markers
  labels <- sprintf(
    "<strong>%s</strong><br/>ID: %s",
    pts$legend, pts$new_id
  ) %>%
    lapply(htmltools::HTML)
  
  # LEAFLET MAP
  leaflet::leaflet() %>%
    # leaflet::addProviderTiles(providers$CartoDB.DarkMatter, group = "Topographic") %>%
    leaflet::addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
    leaflet::addCircleMarkers(
      data        = pts,
      fillColor   = ~pal(legend),
      # fillOpacity = 0.5,
      fillOpacity = 0.8,
      # radius      = 4,
      color       = "black",
      opacity     = 1,
      weight      = 1,
      stroke      = T,
      # label       = ~legend
      layerId     = ~new_id,
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
      values = ulegend,
      title  = "Owner"
      ) %>% 
    leaflet::addCircleMarkers(
      data        = pts,
      fillColor   = "black",
      fillOpacity = 0.6,
      # radius      = 4,
      color       = "black",
      stroke      = TRUE,
      weight      = 1,
      layerId     = ~map_id,
      group       = ~new_id
      # label       = ~map_id
    ) %>%
    leaflet::hideGroup(group = pts$new_id)
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
  
  
  # leaflet::addCircleMarkers(
  #   data         = pts,
  #   # data         = score_pts,
  #   # color        = "black",
  #   fillColor    = "dodgerblue",
  #   opacity      = 1,
  #   radius       = 7,
  #   fillOpacity  = 0.5,
  #   weight       = 3,
  #   stroke       = TRUE,
  #   layerId      = ~new_id,
  #   group        = "lhd_base",
  #   label        = ~new_id
  # )  
  
  
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
  
  rank_scores_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_point(data = dplyr::filter(df, !new_id %in% lhd_ids),
                        aes(x = total_score, y = rank), color = "black", size = 1.5) +
    ggplot2::geom_point(data = dplyr::filter(df, new_id %in% lhd_ids),
                        aes(x = total_score, y = rank), color = "red", size = 2) +
    # ggplot2::geom_point(data = dplyr::filter(df, !new_id %in% selected$groups),
    #                     aes(x = rank, y = total_score), color = "black") +
    # ggplot2::geom_point(data = dplyr::filter(df, new_id %in% selected$groups),
    #                     aes(x = rank, y = total_score), color = "red", size = 3) +
    # geom_point(data = dplyr::filter(rank_scores(), !new_id %in% selected$groups),
    #            aes(x = rank, y = total_score), color = "black") +
    # geom_point(data = dplyr::filter(rank_scores(), new_id %in% selected$groups),
    #            aes(x = rank, y = total_score), color = "red", size = 3) +
    ggplot2::labs(
      y = "Rank",
      x = "Total Score"
      # x = "Score Rank",
      # y = "Total Score"
    )  +
    # scale_y_continuous(breaks =  seq(0, (max(df$total_score) + 1), by = 1), 
    #                    limits = c(0, (max(df$total_score) + 1), 1),
    #                    expand = c(0, 0)) +
    scale_x_continuous(breaks =  seq(0, (max(df$total_score) + 1), by = 1), 
                       limits = c(0, (max(df$total_score) + 1), 1),
                       expand = c(0, 0)) +
    # ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10, 2)) +
    apatheme
  
  if (interactive  == TRUE) {
    
    # interactive plot
    rank_scores_plot <-
      rank_scores_plot %>%
      plotly::ggplotly(tooltip =  c("x", "y")) %>%
      plotly::style(hoverlabel = label) %>%
      plotly::layout(
        font = font,
        legend = list(
          title = list(size = 12),
          font = list(size = 12)),
        xaxis  = list(
          title=list(
            text = "Total Score",
            font = list(size = 12)),
          tickfont = list(size = 12)),
        # titlefont = list(size = 5)
        yaxis  = list(
          title=list(
            text = "Rank",
            font = list(size = 12)),
          tickfont = list(size = 12))
        # yaxis = list(
        #   titlefont = list(size = 12), 
        #   tickfont  = list(size = 12)
        # )
      )
    
    return(rank_scores_plot)
    
  } else if(interactive == FALSE) {
    
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

