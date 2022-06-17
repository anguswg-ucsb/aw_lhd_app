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









