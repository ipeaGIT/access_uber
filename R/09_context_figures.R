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
  pickup_data <- h3jsr::cell_to_polygon(pickup_data, simple = FALSE)
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
  dropoff_data <- h3jsr::cell_to_polygon(dropoff_data, simple = FALSE)
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


# # uber_data_path <- tar_read(uber_data)
# # grid_path <- tar_read(grid_res_8)
# create_edge_bundles <- function(uber_data_path, grid_path) {
#   grid <- setDT(readRDS(grid_path))
#   
#   uber_data <- fread(uber_data_path)
#   uber_data <- uber_data[
#     date_block_2019 == "mar8_dec20" &
#       weekday_weekend == "weekday" &
#       time_block == "morning_peak"
#   ]
#   uber_data <- uber_data[
#     pickup_hex8 %chin% grid$id_hex & dropoff_hex8 %chin% grid$id_hex
#   ]
#   uber_data[
#     grid,
#     on = c(pickup_hex8 = "id_hex"),
#     decile := i.decil
#   ]
#   
#   cols_to_keep <- c("pickup_hex8", "dropoff_hex8", "num_trips", "decile")
#   uber_data[, setdiff(names(uber_data), cols_to_keep) := NULL]
#   
#   hex_coordinates <- h3jsr::h3_to_point(grid$id_hex, simple = FALSE)
#   hex_coordinates <- setDT(sfheaders::sf_to_df(hex_coordinates, fill = TRUE))
#   hex_coordinates[, c("h3_resolution", "sfg_id", "point_id") := NULL]
#   
#   rich_data <- uber_data[decile >= 9]
#   poor_data <- uber_data[decile >= 1 & decile <= 4]
#   
#   full_network <- igraph::graph_from_data_frame(
#     uber_data,
#     vertices = hex_coordinates
#   )
#   rich_network <- igraph::graph_from_data_frame(
#     rich_data,
#     vertices = hex_coordinates
#   )
#   poor_network <- igraph::graph_from_data_frame(
#     poor_data,
#     vertices = hex_coordinates
#   )
#   
#   edges_list <- lapply(
#     list(full_network, rich_network, poor_network),
#     edgebundle::edge_bundle_path,
#     xy = hex_coordinates[, .(x, y)],
#     max_distortion = 12,
#     weight_fac =  4,
#     segments = 20
#   )
#   edges_list <- lapply(
#     edges_list,
#     sfheaders::sf_linestring,
#     x = "x",
#     y = "y",
#     linestring_id = "group"
#   )
#   edges_list <- mapply(
#     edges_list,
#     list(uber_data, rich_data, poor_data),
#     SIMPLIFY = FALSE,
#     FUN = function(edges, data) {
#       setDT(edges)
#       edges[, `:=`(num_trips = data$num_trips, decile = data$decile)]
#       edges
#     }
#   )
#   names(edges_list) <- c("full", "rich", "poor")
#   edges_list <- rbindlist(edges_list, idcol = "type")
#   edges_list[, group := NULL]
#   
#   data_dir <- "../data/data"
#   if (!dir.exists(data_dir)) dir.create(data_dir)
#   
#   data_basename <- "edge_bundles_list.rds"
#   data_path <- file.path(data_dir, data_basename)
#   saveRDS(edges_list, data_path)
#   
#   return(data_path)
# }
# 
# 
# # edge_bundles_path <- tar_read(edge_bundles)
# # rio_city_path <- tar_read(rio_city)
# # rio_state_path <- tar_read(rio_state)
# # map_theme <- tar_read(context_map_theme)
# # north <- tar_read(north)
# # scalebar <- tar_read(scalebar)
# create_edge_maps <- function(edge_bundles_path,
#                              rio_city_path,
#                              rio_state_path,
#                              map_theme,
#                              north,
#                              scalebar) {
#   edge_bundles <- readRDS(edge_bundles_path)
#   edge_bundles[
#     ,
#     type := factor(
#       type,
#       levels = c("full", "rich", "poor"),
#       labels = c("All origins", "Rich origins", "Poor origins")
#     )
#   ]
#   
#   city_border <- readRDS(rio_city_path)
#   expanded_city_border <- st_buffer(city_border, 2000)
#   state_border <- readRDS(rio_state_path)
#   
#   xlim <- c(st_bbox(city_border)[1], st_bbox(city_border)[3])
#   ylim <- c(st_bbox(expanded_city_border)[2], st_bbox(expanded_city_border)[4])
#   
#   p <- ggplot(st_sf(edge_bundles, crs = 4326)) +
#     geom_sf(data = state_border, color = NA, fill = "#efeeec") +
#     geom_sf(aes(size = num_trips), alpha = 0.01, color = "mediumorchid3") +
#     geom_sf(data = city_border, color = "black", fill = NA, size = 0.3) +
#     facet_wrap(~ type, ncol = 1, strip.position = "left") +
#     north +
#     scalebar +
#     coord_sf(xlim = xlim, ylim = ylim) +
#     scale_size(range = c(0.1, 3)) +
#     map_theme +
#     theme(strip.text = element_text(size = 11))
#   
#   figure_path <- "../figures/context/edge_bundles.png"
#   ggsave(
#     figure_path,
#     plot = p,
#     width = 15,
#     height = 25.5,
#     units = "cm"
#   )
#   
#   return(figure_path)
# }


# grid_res_9_path <- tar_read(grid_res_9)
# grid_res_8_path <- tar_read(grid_res_8)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# map_theme <- tar_read(context_map_theme)
# north <- tar_read(north)
# graph_dir_path <- tar_read(graph_dir)
create_paper_context_figure <- function(grid_res_9_path,
                                        grid_res_8_path,
                                        rio_city_path,
                                        rio_state_path,
                                        map_theme,
                                        north,
                                        graph_dir_path) {
  transit_shapes <- generate_transit_shapes(graph_dir_path)
  
  grid_res_9 <- setDT(readRDS(grid_res_9_path))
  grid_res_9 <- grid_res_9[
    pop_total > 0 |
      renda_total > 0 |
      empregos_total > 0 |
      saude_total > 0 |
      edu_total > 0
  ]
  grid_res_9 <- grid_res_9[decil != 0]
  grid_res_9[, decil := factor(decil, levels = 0:10)]
  
  grid_res_8 <- setDT(readRDS(grid_res_8_path))
  grid_res_8 <- grid_res_8[
    pop_total > 0 |
      renda_total > 0 |
      empregos_total > 0 |
      saude_total > 0 |
      edu_total > 0
  ]
  grid_res_8[, area := as.numeric(sf::st_area(sf::st_sf(grid_res_8))) / 1000000]
  grid_res_8[
    ,
    `:=`(
      pop_density = pop_total / area,
      job_density = empregos_total / area
    )
  ]
  
  city_border <- readRDS(rio_city_path)
  expanded_city_border <- st_buffer(city_border, 2000)
  more_expanded_city_border <- st_buffer(city_border, 14500)
  state_border <- readRDS(rio_state_path)
  
  scalebar_data <- city_border
  scalebar_data_bbox <- st_bbox(scalebar_data)
  scalebar_data_bbox["ymax"] <- st_bbox(st_buffer(city_border, 1500))["ymax"]
  attr(st_geometry(scalebar_data), "bbox") <- scalebar_data_bbox
  
  scalebar <- ggsn::scalebar(
    data = scalebar_data, 
    dist = 10, 
    dist_unit = "km",
    location = "topleft", 
    transform = TRUE, 
    model = "WGS84",
    border.size = 0.3, 
    st.size = 3,
    st.dist = 0.04
  )
  
  transit_shapes$lwd <- ifelse(transit_shapes$mode == "Bus", 0.3, 0.5)
  
  xlim <- c(st_bbox(expanded_city_border)[1], st_bbox(city_border)[3])
  ylim <- c(st_bbox(more_expanded_city_border)[2], st_bbox(expanded_city_border)[4])
  
  deciles <- ggplot(st_sf(grid_res_9)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = decil), color = NA) +
    geom_sf(data = city_border, color = "black", fill = NA) +
    north +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_brewer(
      name = "Income\ndecile",
      palette = "RdBu"
    ) +
    guides(fill = guide_legend(byrow = TRUE)) +
    map_theme
  
  pop_density <- ggplot(st_sf(grid_res_8)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = pop_density), color = NA) +
    geom_sf(
      data = subset(transit_shapes, mode %in% c("BRT", "Rail", "Subway")),
      aes(linetype = mode),
      color = "gray65",
      linewidth = 0.4
    ) +
    geom_sf(data = city_border, color = "black", fill = NA) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_gradient(
      name = "Pop. density\n(1000/km²)",
      low = "#efeeec",
      high = "firebrick3",
      label = scales::label_number(scale = 1/1000)
    ) +
    scale_linetype_manual(
      name = "Mode",
      values = c("solid", "solid", "dotted", "twodash", "dashed", "solid"),
      drop = FALSE
    ) +
    guides(linetype = "none", color = "none") +
    map_theme
  
  job_density <- ggplot(st_sf(grid_res_8)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = job_density), color = NA) +
    geom_sf(
      data = subset(transit_shapes, mode %in% c("BRT", "Rail", "Subway")),
      aes(linetype = mode),
      color = "gray65",
      linewidth = 0.4
    ) +
    geom_sf(data = city_border, color = "black", fill = NA) +
    scalebar +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_gradient(
      name = "Job density\n(1000/km²)",
      low = "#efeeec",
      high = "firebrick3",
      label = scales::label_number(scale = 1/1000),
      trans = "sqrt",
      breaks = c(0, 10000, 40000, 60000)
    ) +
    scale_linetype_manual(
      name = "Mode",
      values = c("solid", "solid", "dotted", "twodash", "dashed", "solid"),
      drop = FALSE
    ) +
    guides(linetype = "none", color = "none") +
    map_theme
  
  transit_distribution <- ggplot() +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(
      data = subset(transit_shapes, mode == "Bus"),
      color = "gray85",
      linewidth = 0.3
    ) +
    geom_sf(
      data = subset(transit_shapes, mode != "Bus"),
      aes(linetype = mode, color = mode),
      linewidth = 0.5
    ) +
    geom_sf(data = city_border, color = "black", fill = NA) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_linetype_manual(
      name = "Mode",
      values = c("solid", "solid", "dotted", "twodash", "dashed", "solid"),
      drop = FALSE
    ) +
    scale_color_manual(
      name = "Mode",
      values = c("firebrick3", "gray85", "sienna3", "royalblue4", "seagreen4", "dodgerblue3"),
      drop = FALSE
    ) +
    map_theme +
    theme(legend.key = element_rect(fill = "#efeeec", color = NA))
  
  p <- cowplot::plot_grid(
    deciles,
    pop_density,
    job_density,
    transit_distribution,
    nrow = 2,
    labels = c("A", "B", "C", "D")
  )
  
  figure_path <- "../figures/context/paper_context_figure.png"
  ggsave(
    figure_path,
    plot = p,
    width = 16,
    height = 12,
    units = "cm"
  )
  
  return(figure_path)
}


# graph_dir_path <- tar_read(graph_dir)
generate_transit_shapes <- function(graph_dir_path) {
  gtfs_fet_path <- file.path(
    graph_dir_path,
    "gtfs_fetranspor_fsub_ninter_nfresc_nout_fvlt_times.zip"
  )
  gtfs_sup_path <- file.path(
    graph_dir_path,
    "gtfs_rio_supervia_2019-11_mod.zip"
  )
  
  gtfs_fet <- gtfstools::read_gtfs(
    gtfs_fet_path,
    files = c("trips", "shapes", "routes")
  )
  gtfs_sup <- gtfstools::read_gtfs(
    gtfs_sup_path,
    files = c("trips", "shapes", "routes")
  )
  
  # subway shapes - pavuna and uruguai
  
  subway_trips  <- c("1202_1188_I_1", "15353_16478_I_alt_1")
  subway_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet,
    trip_id = subway_trips,
    file = "shapes"
  )
  subway_shapes$mode <- "subway"
  
  # rail shapes
  
  rail_routes <- gtfs_sup$routes[route_type == 2]$route_id
  rail_trips  <- gtfs_sup$trips[route_id %chin% rail_routes & direction_id == 0]
  rail_trips  <- rail_trips[rail_trips[, .I[1], keyby = .(trip_headsign)]$V1]
  rail_trips  <- rail_trips[! trip_headsign %chin% c("Campo Grande", "Gramacho")]$trip_id
  rail_shapes <- gtfstools::get_trip_geometry(
    gtfs_sup,
    trip_id = rail_trips,
    file = "shapes"
  )
  rail_shapes$mode <- "rail"
  
  # brt shapes
  
  brt_routes <- gtfs_fet$routes[grepl("BRT", route_short_name)]$route_id
  brt_trips  <- gtfs_fet$trips[route_id %chin% brt_routes & direction_id == 1]
  brt_trips  <- brt_trips[brt_trips[, .I[1], keyby = .(route_id)]$V1]$trip_id
  brt_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet,
    trip_id = brt_trips,
    file = "shapes"
  )
  brt_shapes$mode <- "brt"
  
  # municipal buses shapes
  
  bus_routes <- gtfs_fet$routes[route_type == 3 & !grepl("BRT", route_short_name)]$route_id
  bus_trips  <- gtfs_fet$trips[route_id %chin% bus_routes & direction_id == 1]
  bus_trips  <- bus_trips[bus_trips[, .I[1], keyby = .(route_id)]$V1]$trip_id
  bus_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet,
    trip_id = bus_trips,
    file = "shapes"
  )
  bus_shapes$mode <- "bus"
  
  # vlt shapes
  
  vlt_routes <- gtfs_fet$routes[grepl("VLT", route_short_name)]$route_id
  vlt_trips  <- gtfs_fet$trips[route_id %chin% vlt_routes]
  vlt_trips  <- vlt_trips[vlt_trips[, .I[1], keyby = shape_id]$V1]$trip_id
  vlt_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet,
    trip_id = vlt_trips,
    file = "shapes"
  )
  vlt_shapes$mode <- "vlt"
  
  # ferry shapes
  
  ferry_routes <- gtfs_fet$routes[route_type == 4]$route_id
  ferry_trips  <- gtfs_fet$trips[
    route_id %chin% ferry_routes & direction_id == 1
  ]
  ferry_trips <- ferry_trips[ferry_trips[, .I[1], keyby = route_id]$V1]$trip_id
  ferry_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet,
    trip_id = ferry_trips,
    file = "shapes"
  )
  ferry_shapes$mode <- "ferry"
  
  # bind shapes together
  
  transit_shapes <- rbind(
    ferry_shapes, 
    vlt_shapes, 
    bus_shapes, 
    brt_shapes, 
    rail_shapes, 
    subway_shapes
  )
  transit_shapes$mode <- factor(
    transit_shapes$mode,
    labels = c("BRT", "Bus", "Ferry", "Rail", "Subway", "VLT")
  )
  
  return(transit_shapes)
}