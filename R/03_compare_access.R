create_line_chart_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}


create_map_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#aadaff", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      strip.text.x = element_text(color = "grey30")
    )
}


# access_path <- tar_read(adjusted_accessibility)[1]
# grid_path <- tar_read(grid_res_8)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# type <- tar_read(cost_type)[1]
# travel_time_cutoff <- tar_read(travel_time_thresholds)[1]
# monetary_thresholds_sublist <- tar_read(monetary_thresholds)[1]
# map_theme <- tar_read(map_theme)
create_dist_maps <- function(access_path,
                             grid_path,
                             rio_city_path,
                             rio_state_path,
                             type,
                             travel_time_cutoff,
                             monetary_thresholds_sublist,
                             map_theme) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  
  monetary_thresholds <- monetary_thresholds_sublist[[1]]
  monetary_thresholds <- setdiff(monetary_thresholds, 0)
  
  monetary_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  
  access <- access[travel_time == travel_time_cutoff]
  access <- access[get(monetary_column) %in% monetary_thresholds]
  access[grid, on = c(from_id = "id_hex"), geometry := i.geometry]
  access[
    ,
    mode := factor(
      mode,
      levels = c(
        "only_transit",
        "uber_fm_transit_combined",
        "only_uber"
      ),
      labels = c("Only transit", "Ride-hailing first mile + Transit", "Only uber")
    )
  ]
  
  if (type == "affordability") {
    access[
      ,
      affordability := factor(
        affordability,
        levels = monetary_thresholds,
        labels = scales::label_percent(accuracy = 1)(monetary_thresholds)
      )
    ]
  } else {
    access[
      ,
      absolute_cost := factor(
        absolute_cost,
        levels = monetary_thresholds,
        labels = scales::label_number(suffix = " BRL")(monetary_thresholds)
      )
    ]
  }
  
  access <- st_sf(access)
  
  city_border <- readRDS(rio_city_path)
  state_border <- readRDS(rio_state_path)
  xlim <- c(st_bbox(city_border)[1], st_bbox(city_border)[3])
  ylim <- c(st_bbox(city_border)[2], st_bbox(city_border)[4])
  
  total_jobs <- sum(grid$empregos_total)
  
  y_axis_name <- ifelse(
    type == "affordability",
    "Affordability (% of income spent on transport)",
    "Monetary cost threshold"
  )
  
  p <- ggplot(access) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = access), color = NA) +
    geom_sf(data = city_border, color = "black", fill = NA, linewidth = 0.3) +
    facet_grid(get(monetary_column) ~ mode, switch = "y") +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_viridis_c(
      name = "Accessibility\n(% of total jobs)",
      option = "inferno",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs)
    ) +
    labs(y = y_axis_name) +
    guides(fill = guide_colorbar(title.vjust = 1)) +
    map_theme
  
  figures_dir <- "../figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  type_dir <- file.path(figures_dir, monetary_column)
  if (!dir.exists(type_dir)) dir.create(type_dir)
  
  access_dist_dir <- file.path(type_dir, "access_dist_maps")
  if (!dir.exists(access_dist_dir)) dir.create(access_dist_dir)
  
  figure_basename <- paste0(travel_time_cutoff, "min.jpg")
  figure_path <- file.path(access_dist_dir, figure_basename)
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 13,
    units = "cm"
  )
  
  return(figure_path)
}


# access_path <- tar_read(adjusted_accessibility)[1]
# grid_path <- tar_read(grid_res_8)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# type <- tar_read(cost_type)[1]
# travel_time_cutoff <- tar_read(travel_time_thresholds)[1]
# monetary_thresholds_sublist <- tar_read(monetary_thresholds)[1]
# map_theme <- tar_read(map_theme)
create_diff_dist_maps <- function(access_path,
                                  grid_path,
                                  rio_city_path,
                                  rio_state_path,
                                  type,
                                  travel_time_cutoff,
                                  monetary_thresholds_sublist,
                                  map_theme) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  
  monetary_thresholds <- monetary_thresholds_sublist[[1]]
  monetary_thresholds <- setdiff(monetary_thresholds, 0)
  
  monetary_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  
  access <- access[travel_time == travel_time_cutoff]
  access <- access[get(monetary_column) %in% monetary_thresholds]
  access <- access[mode != "only_uber"]
  access_diff <- dcast(access, ... ~ mode, value.var = "access")
  access_diff[, diff := uber_fm_transit_combined - only_transit]
  access_diff[grid, on = c(from_id = "id_hex"), geometry := i.geometry]
  
  if (type == "affordability") {
    access_diff[
      ,
      affordability := factor(
        affordability,
        levels = monetary_thresholds,
        labels = scales::label_percent(accuracy = 1)(monetary_thresholds)
      )
    ]
  } else {
    access_diff[
      ,
      absolute_cost := factor(
        absolute_cost,
        levels = monetary_thresholds,
        labels = scales::label_dollar(prefix = "R$ ")(monetary_thresholds)
      )
    ]
  }
  
  access_diff <- st_sf(access_diff)
  
  city_border <- readRDS(rio_city_path)
  state_border <- readRDS(rio_state_path)
  xlim <- c(st_bbox(city_border)[1], st_bbox(city_border)[3])
  ylim <- c(st_bbox(city_border)[2], st_bbox(city_border)[4])
  
  total_jobs <- sum(grid$empregos_total)
  
  y_axis_name <- ifelse(
    type == "affordability",
    "Affordability (% of income spent on transport)",
    "Monetary cost threshold"
  )
  
  p <- ggplot(access_diff) +
    geom_sf(data = state_border, color = NA, fill = "#efeeec") +
    geom_sf(aes(fill = diff), color = NA) +
    geom_sf(data = city_border, color = "black", fill = NA, size = 0.3) +
    facet_wrap(~ get(monetary_column), ncol = 2) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_gradient(
      name = "Accessibility diff.\n(% of total jobs)",
      low = "#efeeec",
      high = "firebrick3",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs)
    ) +
    labs(y = y_axis_name) +
    guides(fill = guide_colorbar(title.vjust = 1)) +
    map_theme
  
  figures_dir <- "../figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  type_dir <- file.path(figures_dir, monetary_column)
  if (!dir.exists(type_dir)) dir.create(type_dir)
  
  diff_dist_dir <- file.path(type_dir, "diff_dist_maps")
  if (!dir.exists(diff_dist_dir)) dir.create(diff_dist_dir)
  
  figure_basename <- paste0(travel_time_cutoff, "min.jpg")
  figure_path <- file.path(diff_dist_dir, figure_basename)
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 11,
    units = "cm"
  )
  
  return(figure_path)
}

# 
# access_path <- tar_read(adjusted_accessibility)[1]
# grid_path <- tar_read(grid_res_8)
# line_chart_theme <- tar_read(line_chart_theme)
# travel_time_thresholds <- tar_read(travel_time_thresholds)
# type <- tar_read(cost_type)[1]
create_avg_access_plot <- function(access_path,
                                   grid_path,
                                   line_chart_theme,
                                   travel_time_thresholds,
                                   type) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  
  monetary_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  
  access <- access[travel_time %in% travel_time_thresholds]
  access[grid, on = c(from_id = "id_hex"), population := i.pop_total]
  access[
    ,
    mode := factor(
      mode,
      levels = c(
        "only_transit",
        "uber_fm_transit_combined",
        "only_uber"
      ),
      labels = c("Only transit", "Uber first mile + Transit", "Only uber")
    )
  ]
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(mode, travel_time, cost_cutoff = get(monetary_column))
  ]
  setnames(access, old = "cost_cutoff", new = monetary_column)
  
  total_jobs <- sum(grid$empregos_total)
  
  scale_x_name <- ifelse(
    type == "affordability",
    "Affordability (% of income spent on transport)",
    "Monetary cost threshold"
  )
  scale_x_labels <- if (type == "affordability") {
    scales::label_percent(accuracy = 1)
  } else {
    scales::label_dollar(prefix = "R$ ")
  }
  
  geom <- ifelse(type == "affordability", geom_line, geom_step)
  
  p <- ggplot(access) +
    geom(
      aes(
        get(monetary_column),
        avg_access,
        color = mode,
        group = mode
      )
    ) +
    facet_wrap(~ travel_time, nrow = 2) +
    scale_color_brewer(name = "Scenario", palette = "Paired") +
    scale_x_continuous(name = scale_x_name, labels = scale_x_labels) +
    scale_y_continuous(
      name = "Average accessibility (% of total jobs)",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs)
    ) +
    line_chart_theme
  
  figures_dir <- "../figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  type_dir <- file.path(figures_dir, monetary_column)
  if (!dir.exists(type_dir)) dir.create(type_dir)
  
  figure_path <- file.path(type_dir, "avg_access.jpg")
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 11,
    units = "cm"
  )
  
  return(figure_path)
}


# access_path <- tar_read(adjusted_accessibility)[1]
# grid_path <- tar_read(grid_res_8)
# line_chart_theme <- tar_read(line_chart_theme)
# travel_time_thresholds <- tar_read(travel_time_thresholds)
# type <- tar_read(cost_type)[1]
create_avg_access_per_group_plot <- function(access_path,
                                             grid_path,
                                             line_chart_theme,
                                             travel_time_thresholds,
                                             type) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  
  monetary_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  
  access <- access[travel_time %in% travel_time_thresholds]
  access[
    grid,
    on = c(from_id = "id_hex"),
    `:=`(
      population = i.pop_total,
      decile = i.decil
    )
  ]
  access[decile == 10, group := "richest_10"]
  access[decile >= 1 & decile <= 4, group := "poorest_40"]
  access <- access[!is.na(group)]
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(group, mode, travel_time, cost_cutoff = get(monetary_column))
  ]
  setnames(access, old = "cost_cutoff", new = monetary_column)
  
  access[
    ,
    mode := factor(
      mode,
      levels = c(
        "only_transit",
        "uber_fm_transit_combined",
        "only_uber"
      ),
      labels = c("Only transit", "Uber first mile +\nTransit", "Only uber")
    )
  ]
  access[
    ,
    group := factor(
      group,
      levels = c("richest_10", "poorest_40"),
      labels = c("Richest 10%", "Poorest 40%")
    )
  ]
  
  total_jobs <- sum(grid$empregos_total)
  
  scale_x_name <- ifelse(
    type == "affordability",
    "Affordability (% of income spent on transport)",
    "Monetary cost threshold"
  )
  scale_x_labels <- if (type == "affordability") {
    scales::label_percent(accuracy = 1)
  } else {
    scales::label_dollar(prefix = "R$ ")
  }
  
  geom <- ifelse(type == "affordability", geom_line, geom_step)
  
  p <- ggplot(access) +
    geom(
      aes(
        get(monetary_column),
        avg_access,
        color = mode,
        linetype = group
      )
    ) +
    facet_wrap(~ travel_time, nrow = 2) +
    scale_color_brewer(name = "Scenario", palette = "Paired") +
    scale_linetype_manual(
      name = "Population group",
      values = c("longdash", "solid")
    ) +
    scale_x_continuous(name = scale_x_name, labels = scale_x_labels) +
    scale_y_continuous(
      name = "Average accessibility (% of total jobs)",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs)
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2)
    ) +
    line_chart_theme
  
  figures_dir <- "../figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  type_dir <- file.path(figures_dir, monetary_column)
  if (!dir.exists(type_dir)) dir.create(type_dir)
  
  figure_path <- file.path(type_dir, "avg_access_per_group.jpg")
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 12,
    units = "cm"
  )
  
  return(figure_path)
}


# access_path <- tar_read(adjusted_accessibility)[1]
# grid_path <- tar_read(grid_res_8)
# line_chart_theme <- tar_read(line_chart_theme)
# travel_time_thresholds <- tar_read(travel_time_thresholds)
# monetary_thresholds_sublist <- tar_read(monetary_thresholds)[1]
# type <- tar_read(cost_type)[1]
create_avg_access_by_time_plot <- function(access_path,
                                           grid_path,
                                           line_chart_theme,
                                           travel_time_thresholds,
                                           monetary_thresholds_sublist,
                                           type) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  
  monetary_thresholds <- monetary_thresholds_sublist[[1]]
  monetary_thresholds <- setdiff(monetary_thresholds, 0)
  
  monetary_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  
  access <- access[get(monetary_column) %in% monetary_thresholds]
  access[grid, on = c(from_id = "id_hex"), population := i.pop_total]
  access[
    ,
    mode := factor(
      mode,
      levels = c(
        "only_transit",
        "uber_fm_transit_combined",
        "only_uber"
      ),
      labels = c("Only transit", "Ride-hailing first mile + Transit", "Only uber")
    )
  ]
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(mode, travel_time, cost_cutoff = get(monetary_column))
  ]
  
  # filter the only_uber curve to trim it when it becomes fully horizontal
  
  access[
    ,
    previous_value := shift(avg_access, fill = 0),
    by = .(mode, cost_cutoff)
  ]
  access[, diff_from_prev := (avg_access - previous_value) / previous_value]
  access <- access[
    mode != "Only uber" |
      (mode == "Only uber" & (diff_from_prev > 0.001 | avg_access == 0))
  ]
  access[, c("previous_value", "diff_from_prev") := NULL]
  setnames(access, old = "cost_cutoff", new = monetary_column)
  
  if (type == "affordability") {
    access[
      ,
      affordability := factor(
        affordability,
        levels = monetary_thresholds,
        labels = scales::label_percent(accuracy = 1)(monetary_thresholds)
      )
    ]
  } else {
    access[
      ,
      absolute_cost := factor(
        absolute_cost,
        levels = monetary_thresholds,
        labels = scales::label_number(suffix = " BRL")(monetary_thresholds)
      )
    ]
  }
  
  total_jobs <- sum(grid$empregos_total)
  x_breaks <- c(0, travel_time_thresholds)
  
  p <- ggplot(access) +
    geom_line(
      aes(
        travel_time,
        avg_access,
        color = mode,
        group = mode
      )
    ) +
    facet_wrap(~ get(monetary_column), nrow = 2) +
    scale_color_brewer(name = "Scenario", palette = "Paired") +
    scale_x_continuous(name = "Travel time threshold", breaks = x_breaks) +
    scale_y_continuous(
      name = "Average accessibility (% of total jobs)",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs)
    ) +
    line_chart_theme
  
  figures_dir <- "../figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  type_dir <- file.path(figures_dir, monetary_column)
  if (!dir.exists(type_dir)) dir.create(type_dir)
  
  figure_path <- file.path(type_dir, "avg_access_by_time.jpg")
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 11,
    units = "cm"
  )
  
  return(figure_path)
}


# access_path <- tar_read(adjusted_accessibility)[2]
# grid_path <- tar_read(grid_res_8)
# line_chart_theme <- tar_read(line_chart_theme)
# travel_time_thresholds <- tar_read(travel_time_thresholds)
# monetary_thresholds_sublist <- tar_read(monetary_thresholds)[2]
# type <- tar_read(cost_type)[2]
create_avg_access_by_time_per_group_plot <- function(access_path,
                                                     grid_path,
                                                     line_chart_theme,
                                                     travel_time_thresholds,
                                                     monetary_thresholds_sublist,
                                                     type) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  
  monetary_thresholds <- monetary_thresholds_sublist[[1]]
  monetary_thresholds <- setdiff(monetary_thresholds, 0)
  
  monetary_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  
  access <- access[get(monetary_column) %in% monetary_thresholds]
  access[
    grid,
    on = c(from_id = "id_hex"),
    `:=`(
      population = i.pop_total,
      decile = i.decil
    )
  ]
  access[decile == 10, group := "richest_10"]
  access[decile >= 1 & decile <= 4, group := "poorest_40"]
  access <- access[!is.na(group)]
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(group, mode, travel_time, cost_cutoff = get(monetary_column))
  ]
  
  access[
    ,
    mode := factor(
      mode,
      levels = c(
        "only_transit",
        "uber_fm_transit_combined",
        "only_uber"
      ),
      labels = c("Only transit", "Ride-hailing first mile +\nTransit", "Only uber")
    )
  ]
  access[
    ,
    group := factor(
      group,
      levels = c("richest_10", "poorest_40"),
      labels = c("Richest 10%", "Poorest 40%")
    )
  ]
  
  # filter the only_uber curve to trim it when it becomes fully horizontal
  
  access[
    ,
    previous_value := shift(avg_access, fill = 0),
    by = .(mode, cost_cutoff, group)
  ]
  access[, diff_from_prev := (avg_access - previous_value) / previous_value]
  access <- access[
    mode != "Only uber" |
      (mode == "Only uber" & (diff_from_prev > 0.001 | avg_access == 0))
  ]
  access[, c("previous_value", "diff_from_prev") := NULL]
  setnames(access, old = "cost_cutoff", new = monetary_column)
  
  if (type == "affordability") {
    access[
      ,
      affordability := factor(
        affordability,
        levels = monetary_thresholds,
        labels = scales::label_percent(accuracy = 1)(monetary_thresholds)
      )
    ]
  } else {
    access[
      ,
      absolute_cost := factor(
        absolute_cost,
        levels = monetary_thresholds,
        labels = scales::label_number(prefix = " BRL")(monetary_thresholds)
      )
    ]
  }
  
  total_jobs <- sum(grid$empregos_total)
  x_breaks <- c(0, travel_time_thresholds)
  
  p <- ggplot(access) +
    geom_line(
      aes(
        travel_time,
        avg_access,
        color = mode,
        linetype = group
      )
    ) +
    facet_wrap(~ get(monetary_column), nrow = 2) +
    scale_color_brewer(name = "Scenario", palette = "Paired") +
    scale_x_continuous(name = "Travel time threshold", breaks = x_breaks) +
    scale_y_continuous(
      name = "Average accessibility (% of total jobs)",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs)
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(title = "Group", nrow = 2)
    ) +
    line_chart_theme
  
  figures_dir <- "../figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  type_dir <- file.path(figures_dir, monetary_column)
  if (!dir.exists(type_dir)) dir.create(type_dir)
  
  figure_path <- file.path(type_dir, "avg_access_by_time_per_group.jpg")
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 11,
    units = "cm"
  )
  
  return(figure_path)
}


# access_path <- tar_read(adjusted_accessibility)[1]
# grid_path <- tar_read(grid_res_8)
# line_chart_theme <- tar_read(line_chart_theme)
# travel_time_cutoff <- tar_read(travel_time_thresholds)[1]
# monetary_thresholds_sublist <- tar_read(monetary_thresholds)[1]
# type <- tar_read(cost_type)[1]
create_comparison_scatter_plot <- function(access_path,
                                           grid_path,
                                           line_chart_theme,
                                           travel_time_cutoff,
                                           monetary_thresholds_sublist,
                                           type) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  
  monetary_thresholds <- monetary_thresholds_sublist[[1]]
  monetary_thresholds <- setdiff(monetary_thresholds, 0)
  
  monetary_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  
  access <- access[get(monetary_column) %in% monetary_thresholds]
  access <- access[travel_time == travel_time_cutoff]
  access <- access[mode != "only_uber"]
  access <- dcast(access, ... ~ mode, value.var = "access")
  
  access[
    grid,
    on = c(from_id = "id_hex"),
    `:=`(
      population = i.pop_total,
      decile = i.decil
    )
  ]
  access <- access[population > 0]
  access[
    ,
    decile := factor(
      decile,
      levels = 1:10,
      labels = c("1\n(poorest)", as.character(2:9), "10\n(richest)")
    )
  ]
  
  if (type == "affordability") {
    access[
      ,
      affordability := factor(
        affordability,
        levels = monetary_thresholds,
        labels = scales::label_percent()(monetary_thresholds)
      )
    ]
  } else {
    access[
      ,
      absolute_cost := factor(
        absolute_cost,
        levels = monetary_thresholds,
        labels = scales::label_dollar(prefix = "R$ ")(monetary_thresholds)
      )
    ]
  }
  
  total_jobs <- sum(grid$empregos_total)
  
  p <- ggplot(access) +
    geom_point(
      aes(
        only_transit,
        uber_fm_transit_combined,
        size = population,
        color = decile
      ),
      alpha = 0.4
    ) +
    geom_abline(color = "gray30") +
    facet_wrap(~ get(monetary_column), nrow = 2) +
    scale_color_brewer(
      name = "Income decile",
      palette = "RdBu",
      guide = guide_legend(
        nrow = 1,
        label.position = "bottom",
        title.position = "top",
        override.aes = list(size = 4)
      )
    ) +
    scale_size(
      name = "Population\n(thousands)",
      labels = scales::label_number(scale = 1/1000),
      guide = guide_legend(nrow = 3)
    ) +
    scale_x_continuous(
      name = "Average accessibility (only transit)",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs)
    ) +
    scale_y_continuous(
      name = "Average accessibility (uber first mile + transit)",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs)
    ) +
    line_chart_theme
  
  figures_dir <- "../figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  type_dir <- file.path(figures_dir, monetary_column)
  if (!dir.exists(type_dir)) dir.create(type_dir)
  
  scatter_dir <- file.path(type_dir, "comparison_scatter")
  if (!dir.exists(scatter_dir)) dir.create(scatter_dir)
  
  figure_basename <- paste0(travel_time_cutoff, "min.jpg")
  figure_path <- file.path(scatter_dir, figure_basename)
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 15,
    units = "cm"
  )
  
  return(figure_path)
}


# access_path <- tar_read(adjusted_accessibility)
# grid_path <- tar_read(grid_res_8)
# rio_city_path <- tar_read(rio_city)
# rio_state_path <- tar_read(rio_state)
# monetary_thresholds_list <- tar_read(monetary_thresholds)
# map_theme <- tar_read(map_theme)
create_single_diff_dist_map <- function(access_path,
                                        grid_path,
                                        rio_city_path,
                                        rio_state_path,
                                        monetary_thresholds_list,
                                        map_theme) {
  access <- lapply(access_path, readRDS)
  grid <- setDT(readRDS(grid_path))
  
  monetary_thresholds <- lapply(
    monetary_thresholds_list,
    function(.x) setdiff(.x, 0)
  )
  
  monetary_column_names <- c("absolute_cost", "affordability")
  
  access_diff <- mapply(
    access,
    monetary_thresholds,
    monetary_column_names,
    SIMPLIFY = FALSE,
    FUN = function(access_data, cutoffs, colname) {
      access_data <- access_data[travel_time == 60]
      access_data <- access_data[get(colname) %in% cutoffs]
      access_data <- access_data[mode != "only_uber"]
      
      access_diff <- dcast(access_data, ... ~ mode, value.var = "access")
      access_diff[, diff := uber_fm_transit_combined - only_transit]
      access_diff[grid, on = c(from_id = "id_hex"), geometry := i.geometry]
      
      setnames(access_diff, old = colname, new = "cutoff")
    }
  )
  
  # the text on each panel is different, depending on the type of monetary cost
  # we're looking at (the first list object uses absolute costs, the second uses
  # relative costs)
  
  access_diff[[1]][
    ,
    cutoff := factor(
      cutoff,
      levels = monetary_thresholds[[1]],
      labels = scales::label_number(suffix = " BRL")(monetary_thresholds[[1]])
    )
  ]
  
  access_diff[[2]][
    ,
    cutoff := factor(
      cutoff,
      levels = monetary_thresholds[[2]],
      labels = scales::label_percent(accuracy = 1)(monetary_thresholds[[2]])
    )
  ]
  
  access_diff <- lapply(access_diff, st_sf)
  
  city_border <- readRDS(rio_city_path)
  state_border <- readRDS(rio_state_path)
  xlim <- c(st_bbox(city_border)[1], st_bbox(city_border)[3])
  ylim <- c(st_bbox(city_border)[2], st_bbox(city_border)[4])
  
  total_jobs <- sum(grid$empregos_total)
  max_diff <- max(
    vapply(access_diff, function(.x) max(.x$diff), FUN.VALUE = numeric(1))
  )
  
  y_axis_name <- c(
    "Absolute monetary cost threshold",
    "Relative monetary cost threshold"
  )
  
  plots <- mapply(
    access_diff,
    y_axis_name,
    SIMPLIFY = FALSE,
    FUN = function(access_diff_df, axis_name) {
      ggplot(access_diff_df) +
        geom_sf(data = state_border, color = NA, fill = "#efeeec") +
        geom_sf(aes(fill = diff), color = NA) +
        geom_sf(data = city_border, color = "gray60", fill = NA, linewidth = 0.3) +
        facet_wrap(~ cutoff, ncol = 2) +
        coord_sf(xlim = xlim, ylim = ylim) +
        scale_fill_gradient(
          name = "Accessibility diff.\n(% of total jobs)",
          low = "#efeeec",
          high = "firebrick3",
          labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs),
          limits = c(0, max_diff)
        ) +
        labs(y = axis_name) +
        guides(fill = guide_colorbar(title.vjust = 1)) +
        map_theme
    }
  )
  
  maps_legend <- cowplot::get_legend(plots[[1]])
  
  p <- cowplot::plot_grid(
    plots[[1]] + theme(legend.position = "none"),
    plots[[2]] + theme(legend.position = "none"),
    maps_legend,
    ncol = 1,
    labels = c("A", "B"),
    rel_heights = c(1, 1, 0.15)
  )
  
  figures_dir <- "../figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  figure_basename <- "combine_diff_maps_60min.png"
  figure_path <- file.path(figures_dir, figure_basename)
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 20,
    units = "cm"
  )
  
  return(figure_path)
}


# access_path <- tar_read(adjusted_accessibility)
# grid_path <- tar_read(grid_res_8)
# monetary_thresholds_list <- tar_read(monetary_thresholds)
# line_chart_theme <- tar_read(line_chart_theme)
create_diff_per_group_plot <- function(access_path,
                                       grid_path,
                                       monetary_thresholds_list,
                                       line_chart_theme) {
  access <- lapply(access_path, readRDS)
  grid <- setDT(readRDS(grid_path))
  
  monetary_thresholds <- lapply(
    monetary_thresholds_list,
    function(.x) setdiff(.x, 0)
  )
  
  monetary_column_names <- c("absolute_cost", "affordability")
  
  access_diff <- mapply(
    access,
    monetary_thresholds,
    monetary_column_names,
    SIMPLIFY = FALSE,
    FUN = function(access_data, cutoffs, colname) {
      access_data <- access_data[travel_time == 60]
      access_data <- access_data[get(colname) %in% cutoffs]
      access_data <- access_data[mode != "only_uber"]
      
      access_diff <- dcast(access_data, ... ~ mode, value.var = "access")
      access_diff[, diff := uber_fm_transit_combined - only_transit]
      
      access_diff[
        grid,
        on = c(from_id = "id_hex"),
        `:=`(
          population = i.pop_total,
          decile = i.decil
        )
      ]
      access_diff[decile == 10, pop_group := "richest_10"]
      access_diff[decile >= 1 & decile <= 4, pop_group := "poorest_40"]
      access_diff <- access_diff[!is.na(pop_group)]
      access_diff[
        ,
        pop_group := factor(
          pop_group,
          levels = c("poorest_40", "richest_10"),
          labels = c("Poorest", "Wealthiest")
        )
      ]
      
      setnames(access_diff, old = colname, new = "cutoff")
    }
  )
  
  # the text on each panel is different, depending on the type of monetary cost
  # we're looking at (the first list object uses absolute costs, the second uses
  # relative costs)
  
  access_diff[[1]][
    ,
    cutoff := factor(
      cutoff,
      levels = monetary_thresholds[[1]],
      labels = scales::label_number(suffix = " BRL")(monetary_thresholds[[1]])
    )
  ]
  
  access_diff[[2]][
    ,
    cutoff := factor(
      cutoff,
      levels = monetary_thresholds[[2]],
      labels = scales::label_percent(accuracy = 1)(monetary_thresholds[[2]])
    )
  ]
  
  total_jobs <- sum(grid$empregos_total)
  max_diff <- max(
    vapply(access_diff, function(.x) max(.x$diff), FUN.VALUE = numeric(1))
  )
  
  y_axis_name <- c(
    "Absolute monetary cost threshold",
    "Relative monetary cost threshold"
  )
  
  plots <- lapply(
    access_diff,
    function(access_diff_df, axis_name) {
      ggplot(access_diff_df) +
        geom_boxplot(
          aes(
            pop_group,
            diff,
            weight = population,
            color = pop_group,
            group = pop_group
          )
        ) +
        facet_wrap(~ cutoff, nrow = 1) +
        scale_y_continuous(
          name = "Accessibility difference\n(% of total jobs)",
          labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs),
          limits = c(0, max_diff)
        ) +
        scale_color_manual(values = c("#b2182b", "#2166ac")) + 
        line_chart_theme +
        theme(
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          axis.title.x = element_blank()
        )
    }
  )
  
  p <- cowplot::plot_grid(
    plots[[1]],
    plots[[2]],
    ncol = 1,
    labels = c("A", "B")
  )
  
  figures_dir <- "../figures"
  if (!dir.exists(figures_dir)) dir.create(figures_dir)
  
  figure_basename <- "combined_diff_per_group_60min.png"
  figure_path <- file.path(figures_dir, figure_basename)
  ggsave(
    figure_path,
    plot = p,
    width = 15,
    height = 12,
    units = "cm"
  )
  
  return(figure_path)
}