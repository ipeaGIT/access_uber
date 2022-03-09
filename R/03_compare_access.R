# access_path <- tar_read(affordability_accessibility)
# grid_path <- tar_read(grid_res_8)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# type <- "affordability"
create_dist_maps <- function(access_path,
                             grid_path,
                             rio_city_path,
                             rio_state_path,
                             type = c("affordability", "absolute")) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  type <- type[1]
  
  travel_time_cutoff <- 90
  monetary_cutoffs <- if(type == "affordability") {
    c(0.2, 0.4, 0.6)
  } else {
    c(5, 10, 15)
  }
  monetary_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  
  access <- access[travel_time == travel_time_cutoff]
  access <- access[get(monetary_column) %in% monetary_cutoffs]
  access[grid, on = c(from_id = "id_hex"), geometry := i.geometry]
  access[
    ,
    mode := factor(
      mode,
      levels = c("only_transit", "uber_fm_transit_combined", "only_uber"),
      labels = c("Only transit", "Uber first mile + Transit", "Only uber")
    )
  ]
  
  if (type == "affordability") {
    access[
      ,
      affordability := factor(
        affordability,
        levels = monetary_cutoffs,
        labels = scales::label_percent()(monetary_cutoffs)
      )
    ]
  } else {
    access[
      ,
      absolute_cost := factor(
        absolute_cost,
        levels = monetary_cutoffs,
        labels = scales::label_dollar(prefix = "R$ ")(monetary_cutoffs)
      )
    ]
  }
  
  access <- st_sf(access)
  
  city_border <- readRDS(rio_city_path)
  state_border <- readRDS(rio_state_path)
  xlim <- c(st_bbox(city_border)[1], st_bbox(city_border)[3])
  ylim <- c(st_bbox(city_border)[2], st_bbox(city_border)[4])
  
  total_jobs <- sum(grid$empregos_total)
  
  p <- ggplot(access) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = access), color = NA) +
    geom_sf(data = city_border, color = "black", fill = NA, size = 0.3) +
    facet_grid(get(monetary_column) ~ mode, switch = "y") +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_viridis_c(
      name = "Accessibility\n(% of total jobs)",
      option = "inferno",
      labels = scales::label_percent(scale = 100 / total_jobs)
    ) +
    labs(y = "Affordability (% of income spent on transport)") +
    guides(fill = guide_colorbar(title.vjust = 1)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#aadaff", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      strip.text.x = element_text(color = "grey30")
    )
  
  figures_dir <- "../../data/access_uber/figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  type_dir <- file.path(figures_dir, monetary_column)
  if (!dir.exists(type_dir)) dir.create(type_dir)
  
  figure_path <- file.path(type_dir, "access_dist_map.jpg")
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 10,
    units = "cm"
  )
  
  return(figure_path)
}