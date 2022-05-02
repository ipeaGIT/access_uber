create_context_map_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#aadaff", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = c(1, 0),
      legend.justification = c(1, 0),
      legend.direction = "horizontal",
      legend.box.just = "right",
      legend.background = element_rect(fill = "#aadaff", color = NA),
      legend.title = element_text(hjust = 1),
      legend.text = element_text(size = 9),
      strip.text.x = element_text(color = "grey30"),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
    )
}


# rio_city_path <- tar_read(rio_city)
create_north <- function(rio_city_path) {
  city_border <- readRDS(rio_city_path)
  expanded_city_border <- st_buffer(city_border, 4000)
  less_expanded_city_border <- st_buffer(city_border, 2000)
  
  north_data <- city_border
  north_data_bbox <- st_bbox(north_data)
  north_data_bbox["xmin"] <- st_bbox(expanded_city_border)["xmin"]
  north_data_bbox["ymin"] <- st_bbox(less_expanded_city_border)["ymin"]
  north_data_bbox["ymax"] <- st_bbox(less_expanded_city_border)["ymax"]
  attr(st_geometry(north_data), "bbox") <- north_data_bbox
  
  north <- ggsn::north(
    data = north_data,
    location = "topleft",
    scale = 0.15,
    symbol = 4
  )
  
  return(north)
}


# rio_city_path <- tar_read(rio_city)
create_scalebar <- function(rio_city_path) {
  city_border <- readRDS(rio_city_path)
  expanded_city_border <- st_buffer(city_border, 1500)
  
  scalebar_data <- city_border
  scalebar_data_bbox <- st_bbox(scalebar_data)
  scalebar_data_bbox["ymin"] <- st_bbox(expanded_city_border)["ymin"]
  attr(st_geometry(scalebar_data), "bbox") <- scalebar_data_bbox
  
  scalebar <- ggsn::scalebar(
    data = scalebar_data, 
    dist = 10, 
    dist_unit = "km",
    location = "bottomleft", 
    transform = TRUE, 
    model = "WGS84",
    border.size = 0.3, 
    st.size = 3,
    st.dist = 0.03
  )
  
  return(scalebar)
}


# grid_path <- tar_read(grid_res_9)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# map_theme <- tar_read(context_map_theme)
# north <- tar_read(north)
# scalebar <- tar_read(scalebar)
create_pop_density_map <- function(grid_path,
                                   rio_city_path,
                                   rio_state_path,
                                   map_theme,
                                   north,
                                   scalebar) {
  grid <- setDT(readRDS(grid_path))
  grid <- grid[
    pop_total > 0 |
      renda_total > 0 |
      empregos_total > 0 |
      saude_total > 0 |
      edu_total > 0
  ]
  grid[, area := as.numeric(sf::st_area(sf::st_sf(grid))) / 1000000]
  grid[, pop_density := pop_total / area]
  
  city_border <- readRDS(rio_city_path)
  expanded_city_border <- st_buffer(city_border, 2000)
  state_border <- readRDS(rio_state_path)
  
  xlim <- c(st_bbox(city_border)[1], st_bbox(city_border)[3])
  ylim <- c(st_bbox(expanded_city_border)[2], st_bbox(expanded_city_border)[4])
  
  p <- ggplot(st_sf(grid)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = pop_density), color = NA) +
    geom_sf(data = city_border, color = "black", fill = NA, size = 0.3) +
    north +
    scalebar +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_gradient(
      name = "Pop. density\n(1000/km²)",
      low = "#efeeec",
      high = "firebrick3",
      label = scales::label_number(scale = 1/1000)
    ) +
    map_theme
  
  figure_path <- "../figures/context/pop_density.png"
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 9,
    units = "cm"
  )
  
  return(figure_path)
}


# grid_path <- tar_read(grid_res_9)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# map_theme <- tar_read(context_map_theme)
# north <- tar_read(north)
# scalebar <- tar_read(scalebar)
create_income_decile_map <- function(grid_path,
                                     rio_city_path,
                                     rio_state_path,
                                     map_theme,
                                     north,
                                     scalebar) {
  grid <- setDT(readRDS(grid_path))
  grid <- grid[
    pop_total > 0 |
      renda_total > 0 |
      empregos_total > 0 |
      saude_total > 0 |
      edu_total > 0
  ]
  grid <- grid[decil > 0]
  grid[
    ,
    decil := factor(
      decil,
      levels = 1:10
    )
  ]
  
  city_border <- readRDS(rio_city_path)
  expanded_city_border <- st_buffer(city_border, 2000)
  state_border <- readRDS(rio_state_path)
  
  xlim <- c(st_bbox(city_border)[1], st_bbox(city_border)[3])
  ylim <- c(st_bbox(expanded_city_border)[2], st_bbox(expanded_city_border)[4])
  
  p <- ggplot(st_sf(grid)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = decil), color = NA) +
    geom_sf(data = city_border, color = "black", fill = NA, size = 0.3) +
    north +
    scalebar +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_brewer(
      name = "Income\ndecile",
      palette = "RdBu"
    ) +
    guides(fill = guide_legend(byrow = TRUE)) +
    map_theme
  
  figure_path <- "../figures/context/income_decile.png"
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 9,
    units = "cm"
  )
  
  return(figure_path)
}


# pickup_data_path <- tar_read(pickup_data)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# map_theme <- tar_read(context_map_theme)
# north <- tar_read(north)
# scalebar <- tar_read(scalebar)
create_pickup_density_map <- function(pickup_data_path,
                                      rio_city_path,
                                      rio_state_path,
                                      map_theme,
                                      north,
                                      scalebar) {
  pickup_data <- fread(pickup_data_path)
  pickup_data <- pickup_data[
    date_block_2019 == "mar8_dec20" &
      weekday_weekend == "weekday" &
      time_block == "morning_peak"
  ]
  pickup_data[
    ,
    setdiff(names(pickup_data), c("hex_addr", "num_pickups")) := NULL
  ]
  pickup_data <- h3jsr::h3_to_polygon(pickup_data, simple = FALSE)
  pickup_data <- st_transform(pickup_data, 4674)
  
  city_border <- readRDS(rio_city_path)
  does_intersect <- st_intersects(pickup_data, city_border, sparse = FALSE)
  pickup_data <- pickup_data[does_intersect, ]
  
  setDT(pickup_data)
  pickup_data[, area := as.numeric(st_area(st_sf(pickup_data))) / 1000000]
  pickup_data[, pickup_density := num_pickups / area]
  
  expanded_city_border <- st_buffer(city_border, 2000)
  state_border <- readRDS(rio_state_path)
  
  xlim <- c(st_bbox(city_border)[1], st_bbox(city_border)[3])
  ylim <- c(st_bbox(expanded_city_border)[2], st_bbox(expanded_city_border)[4])
  
  p <- ggplot(st_sf(pickup_data)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = pickup_density), color = NA) +
    geom_sf(data = city_border, color = "black", fill = NA, size = 0.3) +
    north +
    scalebar +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_gradient(
      name = "Pickup density\n(1000/km²)",
      low = "#efeeec",
      high = "mediumorchid3",
      trans = scales::trans_new(
        "cubic root",
        transform = function(x) x^(1/3),
        inverse = function(x) x^3
      ),
      breaks = c(3000, 30000, 300000),
      label = scales::label_number(scale = 1/1000, accuracy = 1)
    ) +
    map_theme
  
  figure_path <- "../figures/context/pickup_density.png"
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 9,
    units = "cm"
  )
  
  return(figure_path)
}


# dropoff_data_path <- tar_read(dropoff_data)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# map_theme <- tar_read(context_map_theme)
# north <- tar_read(north)
# scalebar <- tar_read(scalebar)
create_dropoff_density_map <- function(dropoff_data_path,
                                       rio_city_path,
                                       rio_state_path,
                                       map_theme,
                                       north,
                                       scalebar) {
  dropoff_data <- fread(dropoff_data_path)
  dropoff_data <- dropoff_data[
    date_block_2019 == "mar8_dec20" &
      weekday_weekend == "weekday" &
      time_block == "morning_peak"
  ]
  dropoff_data[
    ,
    setdiff(names(dropoff_data), c("hex_addr", "num_dropoffs")) := NULL
  ]
  dropoff_data <- h3jsr::h3_to_polygon(dropoff_data, simple = FALSE)
  dropoff_data <- st_transform(dropoff_data, 4674)
  
  city_border <- readRDS(rio_city_path)
  does_intersect <- st_intersects(dropoff_data, city_border, sparse = FALSE)
  dropoff_data <- dropoff_data[does_intersect, ]
  
  setDT(dropoff_data)
  dropoff_data[, area := as.numeric(st_area(st_sf(dropoff_data))) / 1000000]
  dropoff_data[, dropoff_density := num_dropoffs / area]
  
  expanded_city_border <- st_buffer(city_border, 2000)
  state_border <- readRDS(rio_state_path)
  
  xlim <- c(st_bbox(city_border)[1], st_bbox(city_border)[3])
  ylim <- c(st_bbox(expanded_city_border)[2], st_bbox(expanded_city_border)[4])
  
  p <- ggplot(st_sf(dropoff_data)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = dropoff_density), color = NA) +
    geom_sf(data = city_border, color = "black", fill = NA, size = 0.3) +
    north +
    scalebar +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_gradient(
      name = "Dropoff density\n(1000/km²)",
      low = "#efeeec",
      high = "mediumorchid3",
      trans = scales::trans_new(
        "cubic root",
        transform = function(x) x^(1/3),
        inverse = function(x) x^3
      ),
      breaks = c(3000, 30000, 300000),
      label = scales::label_number(scale = 1/1000, accuracy = 1)
    ) +
    map_theme
  
  figure_path <- "../figures/context/dropoff_density.png"
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 9,
    units = "cm"
  )
  
  return(figure_path)
}


# uber_data_path <- tar_read(uber_data)
# grid_path <- tar_read(grid_res_8)
create_edge_bundles <- function(uber_data_path, grid_path) {
  grid <- setDT(readRDS(grid_path))
  
  uber_data <- fread(uber_data_path)
  uber_data <- uber_data[
    date_block_2019 == "mar8_dec20" &
      weekday_weekend == "weekday" &
      time_block == "morning_peak"
  ]
  uber_data <- uber_data[
    pickup_hex8 %chin% grid$id_hex & dropoff_hex8 %chin% grid$id_hex
  ]
  uber_data[
    grid,
    on = c(pickup_hex8 = "id_hex"),
    decile := i.decil
  ]
  
  cols_to_keep <- c("pickup_hex8", "dropoff_hex8", "num_trips", "decile")
  uber_data[, setdiff(names(uber_data), cols_to_keep) := NULL]
  
  hex_coordinates <- h3jsr::h3_to_point(grid$id_hex, simple = FALSE)
  hex_coordinates <- setDT(sfheaders::sf_to_df(hex_coordinates, fill = TRUE))
  hex_coordinates[, c("h3_resolution", "sfg_id", "point_id") := NULL]
  
  rich_data <- uber_data[decile >= 9]
  poor_data <- uber_data[decile >= 1 & decile <= 4]
  
  full_network <- igraph::graph_from_data_frame(
    uber_data,
    vertices = hex_coordinates
  )
  rich_network <- igraph::graph_from_data_frame(
    rich_data,
    vertices = hex_coordinates
  )
  poor_network <- igraph::graph_from_data_frame(
    poor_data,
    vertices = hex_coordinates
  )
  
  edges_list <- lapply(
    list(full_network, rich_network, poor_network),
    edgebundle::edge_bundle_path,
    xy = hex_coordinates[, .(x, y)],
    max_distortion = 12,
    weight_fac =  4,
    segments = 20
  )
  edges_list <- lapply(
    edges_list,
    sfheaders::sf_linestring,
    x = "x",
    y = "y",
    linestring_id = "group"
  )
  edges_list <- mapply(
    edges_list,
    list(uber_data, rich_data, poor_data),
    SIMPLIFY = FALSE,
    FUN = function(edges, data) {
      setDT(edges)
      edges[, `:=`(num_trips = data$num_trips, decile = data$decile)]
      edges
    }
  )
  names(edges_list) <- c("full", "rich", "poor")
  edges_list <- rbindlist(edges_list, idcol = "type")
  edges_list[, group := NULL]
  
  data_dir <- "../data/data"
  if (!dir.exists(data_dir)) dir.create(data_dir)
  
  data_basename <- "edge_bundles_list.rds"
  data_path <- file.path(data_dir, data_basename)
  saveRDS(edges_list, data_path)
  
  return(data_path)
}


# library(geobr)
# library(maptiles)
# library(dplyr)
# library(ggplot2)
# library(igraph)
# library(ggforce)
# library(edgebundle) # https://github.com/schochastics/edgebundle
# library(sfheaders)
# library(patchwork)
# library(sf)
# library(data.table)
# library(aopdata)
# library(h3jsr)
# library(dplyr)
# 
# # disable scientific notation
# options(scipen = 99)
# 
# 
# 
# 
# 
# ###### Read spatial context data ------------------
# 
# # download Rio state 
# state <- read_state(code_state = 33)
# 
# # download Rio vector data
# muni <- read_municipality(code_muni = 3304557)
# muni_bbox <- st_bbox(muni)
# 
#     # download tiles and compose raster (SpatRaster)
#     base_tile12 <- get_tiles(muni,
#                              crop = TRUE,
#                              provider = "CartoDB.DarkMatterNoLabels", zoom = 12)
# 
#     base_tile10 <- get_tiles(muni,
#                            crop = TRUE,
#                            provider = "CartoDB.DarkMatterNoLabels", zoom = 10)
#     # display map
#     # plot_tiles(base_tile12)
#     # plot_tiles(base_tile10)
# 
#     # convert tile para df
#     base_df <- terra::as.data.frame(base_tile10, xy = TRUE)  %>%
#       mutate(hex = rgb(red, green, blue, maxColorValue = 255))
# 
#     # plot
#     ggplot() +
#       geom_tile(data = base_df, aes(x, y, fill = hex), color=NA) +
#       scale_fill_identity() +
#       geom_sf(data=muni , fill=NA, color='gray90', size=.5)
# 
# 
#   # # Hex spatial grid of Rio
#   # hex_sf <- aopdata::read_landuse(city = 'Rio de Janeiro', geometry = T)
#   # head(hex_sf)
#   # 
# 
# 
# 
# base_map <- ggplot() +
#       geom_sf(data=state , fill= 'black') +
#       geom_sf(data=muni, color='gray40', fill=NA, size=.5, alpha=.5) +
#       coord_sf(xlim = c(muni_bbox[[1]], muni_bbox[[3]]), ylim = c(muni_bbox[[2]], muni_bbox[[4]])) +
#       theme_void() +
#       theme(panel.background = element_rect(fill='gray10', colour=NA))
#    
# coords_sf_rio <- coord_sf(xlim = c(muni_bbox[[1]], muni_bbox[[3]]), ylim = c(muni_bbox[[2]], muni_bbox[[4]]))
# 
# 
# ###### Read Uber data  ------------------
# 
# # read data
# df_od <- fread('../data/data-raw/rio_origin_destination.csv')
# df_pick <- fread('../data/data-raw/rio_pickups.csv')
# df_drop <- fread('../data/data-raw/rio_dropoffs.csv')
# 
# # filter
# df_pick <- df_pick[ date_block_2019 == "mar8_dec20" &
#                     weekday_weekend == "weekday" &
#                     time_block == "morning_peak"]
#   
# df_drop <- df_drop[ date_block_2019 == "mar8_dec20" &
#                     weekday_weekend == "weekday" &
#                     time_block == "morning_peak"]
# 
# df_od <- df_od[ date_block_2019 == "mar8_dec20" &
#                 weekday_weekend == "weekday" &
#                 time_block == "morning_peak"]
# 
# 
# ### get spatial H3
# 
# # h3 polygons
# grid8 <- readRDS( tar_read(grid_res_8) )
# grid9 <- readRDS( tar_read(grid_res_9) )
# grid <- rbind(grid8, grid9)
# 
# 
# # h3 centroids coordinates
# h3_od_point <- h3jsr::h3_to_point(h3_address = grid8$id_hex, simple = F)
# h3_od_point <- sfheaders::sf_to_df(h3_od_point, fill = T)
# setDT(h3_od_point)
# 
# 
# ## merge
# df_pick <- dplyr::left_join(df_pick, grid, by=c('hex_addr'='id_hex'))
# df_drop <- dplyr::left_join(df_drop, grid, by=c('hex_addr'='id_hex'))
# 
# df_pick <- st_sf(df_pick)
# df_drop <- st_sf(df_drop)
# 
# 
# # KEEP ONLY DATA POINTS IN RIO
# 
# df_pick <- subset(df_pick, hex_addr %in% grid$id_hex)
# df_drop <- subset(df_drop, hex_addr %in% grid$id_hex)
# df_od   <- subset(df_od, pickup_hex8 %in%  grid8$id_hex)
# df_od   <- subset(df_od, dropoff_hex8 %in% grid8$id_hex)
# 
# 
# 
# ######  Map origin-destination flows  ------------------
# 
# # add income info
# setDT(grid8)
# df_od[grid8, on=c('pickup_hex8'='id_hex'),
#       c('quintil', 'decil') := list(i.quintil, i.decil)]
# 
# 
# # add coordinates to od matrix
# df_od[h3_od_point, on=c('pickup_hex8'='h3_address'),
#       c('pickup_x', 'pickup_y') := list(i.x, i.y)]
# 
# df_od[h3_od_point, on=c('dropoff_hex8'='h3_address'),
#       c('dropoff_x', 'dropoff_y') := list(i.x, i.y)]
# 
# 
# 
# 
# 
# ###### build igraph network   ------------------
# 
# df_od[, date_block_2019 := NULL]
# g <- igraph::graph_from_data_frame(d = df_od,
#                                    vertices = h3_od_point[, .(h3_address,x,y)],
#                                    directed = T)
# 
# g_poor <- igraph::graph_from_data_frame(d = subset(df_od, decil < 5), 
#                                          vertices = h3_od_point[, .(h3_address,x,y)],
#                                          directed = T)
# 
# g_rich <- igraph::graph_from_data_frame(d = subset(df_od, decil >8), 
#                                          vertices = h3_od_point[, .(h3_address,x,y)],
#                                          directed = T)
# 
# 
# 
# 
# # coordinates of vertices
# xy <- cbind(V(g)$x, V(g)$y)
# V
# 
# ### Edge Bundling
# 
# # Edge-Path Bundling
# pbundle <- edge_bundle_path(g, xy,
#                             max_distortion = 12,
#                             weight_fac =  4,
#                             segments = 20)
# 
# pbundle_rich <- edge_bundle_path(g_rich , xy, 
#                             max_distortion = 12,
#                             weight_fac =  4,
#                             segments = 20)
# 
# pbundle_poor <- edge_bundle_path(g_poor, xy, 
#                             max_distortion = 12,
#                             weight_fac =  4,
#                             segments = 20)
# 
# 2+2
# 
# # fbundle <- edge_bundle_force(g, xy, compatibility_threshold = 0.6)
# # sbundle <- edge_bundle_stub(g, xy)
# # hbundle <- edge_bundle_hammer(g, xy, bw = 0.7, decay = 0.5)
# 
# 2+2
# 
# 
# setDT(pbundle)[, cat := 'all']
# setDT(pbundle_rich)[, cat := 'rich']
# setDT(pbundle_poor)[, cat := 'poor']
# 
# df_bundle <- rbind(pbundle, pbundle_rich, pbundle_poor)
# 
# ### Figure -----------------
# fig_od_all <- ggplot() +
#           geom_sf(data=muni , fill='gray10', color=NA) +
#           geom_path(data = pbundle, aes(x, y, group = group),
#                     col = "#9d0191", size = 0.009, alpha=.05) +
#            geom_path(data = pbundle, aes(x, y, group = group),
#                      col = "white", size = 0.00009, alpha=.01) +
#           theme_classic() +
#           theme(axis.line=element_blank(),
#                 axis.text=element_blank(),
#                 axis.ticks=element_blank(),
#                 axis.title=element_blank())
# 
# fig_od_rich <- ggplot() +
#                 geom_sf(data=muni , fill='gray10', color=NA) +
#                 geom_path(data = pbundle_rich, aes(x, y, group = group),
#                           col = "#9d0191", size = 0.009, alpha=.05) +
#                 geom_path(data = pbundle_rich, aes(x, y, group = group),
#                           col = "white", size = 0.00009, alpha=.01) +
#                 theme_classic() +
#                 theme(axis.line=element_blank(),
#                       axis.text=element_blank(),
#                       axis.ticks=element_blank(),
#                       axis.title=element_blank())
# 
# fig_od_poor <- ggplot() +
#                 geom_sf(data=muni , fill='gray10', color=NA) +
#                 geom_path(data = pbundle_poor, aes(x, y, group = group),
#                           col = "#9d0191", size = 0.009, alpha=.05) +
#                 geom_path(data = pbundle_poor, aes(x, y, group = group),
#                           col = "white", size = 0.00009, alpha=.01) +
#                 theme_classic() +
#                 theme(axis.line=element_blank(),
#                       axis.text=element_blank(),
#                       axis.ticks=element_blank(),
#                       axis.title=element_blank())
# 
# ggsave(fig_od_all, 
#        file='../figures/context/od_edges_all_05_01.png', 
#        dpi=200,
#        width = 15,
#        height = 13,
#        units = "cm",
#        bg= '#FFFFFF')
# 
# ggsave(fig_od_rich, 
#        file='../figures/context/od_edges_rich_05_01.png', 
#        dpi=200,
#        width = 15,
#        height = 13,
#        units = "cm",
#        bg= '#FFFFFF')
# 
# ggsave(fig_od_poor, 
#        file='../figures/context/od_edges_poor_05_01.png', 
#        dpi=200,
#        width = 15,
#        height = 13,
#        units = "cm",
#        bg= '#FFFFFF')
# 
# 
# 
# 
# 
# fig_od <- ggplot() +
#   geom_tile(data = base_df, aes(x, y, fill = hex), color=NA) +
#   scale_fill_identity() +
#   geom_sf(data=muni , color='gray50', fill=NA) +
#   geom_path(data = df_bundle, aes(x, y, group = group),
#             col = "#019d9d", size = 0.009, alpha=.05) + # verde 019d9d ROSA 9d0191
#   geom_path(data = df_bundle, aes(x, y, group = group),
#             col = "white", size = 0.00009, alpha=.01) +
#   facet_wrap(~cat, nrow = 3) +
#   theme_classic() +
#   theme(axis.line=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title=element_blank())
# 
# ggsave(fig_od, 
#        file='../figures/context/od_edges_05_01_tile_verde.png', 
#        dpi=200,
#        width = 15,
#        height = 23,
#        units = "cm",
#        bg= '#FFFFFF')
