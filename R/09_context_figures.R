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


# grid_res_9_path <- tar_read(grid_res_9)
# grid_res_8_path <- tar_read(grid_res_8)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# map_theme <- tar_read(context_map_theme)
# north <- tar_read(north)
# graph_dir_path <- tar_read(graph_dir)
# lang <- tar_read(lang)[1]
create_paper_context_figure <- function(grid_res_9_path,
                                        grid_res_8_path,
                                        rio_city_path,
                                        rio_state_path,
                                        map_theme,
                                        north,
                                        graph_dir_path,
                                        lang) {
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
  
  if (lang == "en") {
    fg_labels <- list(
      decile = "Income\ndecile",
      pop_density = "Pop. density\n(1000/km²)",
      job_density = "Job density\n(1000/km²)",
      transport_mode = "Mode",
      rapid_transit = c("BRT", "Rail", "Subway"),
      bus = "Bus"
    )
  } else {
    transit_shapes$mode <- factor(
      transit_shapes$mode,
      labels = c("BRT", "Ônibus", "Barca", "Trem", "Metrô", "VLT")
    )
    
    fg_labels <- list(
      decile = "Decil de\nrenda",
      pop_density = "Dens. populacional\n(1000/km²)",
      job_density = "Dens. de empregos\n(1000/km²)",
      transport_mode = "Modo",
      rapid_transit = c("BRT", "Trem", "Metrô"),
      bus = "Ônibus"
    )
  }
  
  deciles <- ggplot(st_sf(grid_res_9)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = decil), color = NA) +
    geom_sf(data = city_border, color = "black", fill = NA) +
    north +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_brewer(
      name = fg_labels$decile,
      palette = "RdBu"
    ) +
    guides(fill = guide_legend(byrow = TRUE)) +
    map_theme
  
  pop_density <- ggplot(st_sf(grid_res_8)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = pop_density), color = NA) +
    geom_sf(
      data = subset(transit_shapes, mode %in% fg_labels$rapid_transit),
      aes(linetype = mode),
      color = "gray65",
      linewidth = 0.4
    ) +
    geom_sf(data = city_border, color = "black", fill = NA) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_gradient(
      name = fg_labels$pop_density,
      low = "#efeeec",
      high = "firebrick3",
      label = scales::label_number(scale = 1/1000)
    ) +
    scale_linetype_manual(
      name = fg_labels$transport_mode,
      values = c("solid", "solid", "dotted", "twodash", "dashed", "solid"),
      drop = FALSE
    ) +
    guides(linetype = "none", color = "none") +
    map_theme
  
  job_density <- ggplot(st_sf(grid_res_8)) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = job_density), color = NA) +
    geom_sf(
      data = subset(transit_shapes, mode %in% fg_labels$rapid_transit),
      aes(linetype = mode),
      color = "gray65",
      linewidth = 0.4
    ) +
    geom_sf(data = city_border, color = "black", fill = NA) +
    scalebar +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_gradient(
      name = fg_labels$job_density,
      low = "#efeeec",
      high = "firebrick3",
      label = scales::label_number(scale = 1/1000),
      trans = "sqrt",
      breaks = c(0, 10000, 40000, 60000)
    ) +
    scale_linetype_manual(
      name = fg_labels$transport_mode,
      values = c("solid", "solid", "dotted", "twodash", "dashed", "solid"),
      drop = FALSE
    ) +
    guides(linetype = "none", color = "none") +
    map_theme
  
  transit_distribution <- ggplot() +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(
      data = subset(transit_shapes, mode == fg_labels$bus),
      color = "gray85",
      linewidth = 0.3
    ) +
    geom_sf(
      data = subset(transit_shapes, mode != fg_labels$bus),
      aes(linetype = mode, color = mode),
      linewidth = 0.5
    ) +
    geom_sf(data = city_border, color = "black", fill = NA) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_linetype_manual(
      name = fg_labels$transport_mode,
      values = c("solid", "solid", "dotted", "twodash", "dashed", "solid"),
      drop = FALSE
    ) +
    scale_color_manual(
      name = fg_labels$transport_mode,
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
  
  lang_dir <- file.path("../figures", lang)
  if (!dir.exists(lang_dir)) dir.create(lang_dir)
  
  context_dir <- file.path(lang_dir, "context")
  if (!dir.exists(context_dir)) dir.create(context_dir)
  
  figure_path <- file.path(context_dir, "paper_context_figure.png")
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