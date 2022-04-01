# frontier_paths <- tar_read(frontiers_with_accessibility)
# travel_time_thresholds <- tar_read(travel_time_thresholds)
# monetary_thresholds_sublist <- tar_read(monetary_thresholds)[1]
# grid_path <- tar_read(grid_res_8)
# routed_points_path <- tar_read(r5_points)
# type <- tar_read(cost_type)[1]
calculate_access <- function(frontier_paths,
                             travel_time_thresholds,
                             monetary_thresholds_sublist,
                             grid_path,
                             routed_points_path,
                             type) {
  frontiers <- lapply(frontier_paths, readRDS)
  frontiers_names <- gsub(".rds", "", basename(frontier_paths))
  names(frontiers) <- frontiers_names
  frontiers <- rbindlist(frontiers, fill = TRUE, idcol = "mode")
  type <- type[1]
  
  grid <- setDT(readRDS(grid_path))
  frontiers[grid, on = c(to_id = "id_hex"), dest_jobs := i.empregos_total]

  # loop over different combinations of travel time and monetary cutoffs.
  # we generate two distributions, one that smoothly increases the monetary
  # limits with only the specified temporal thresholds and another that smoothly
  # increases the travel time limits with only the specified monetary cutoffs,
  # and bind them together
  
  monetary_thresholds <- monetary_thresholds_sublist[[1]]
  
  monetary_smooth_distribution <- if (type == "affordability") {
    seq(0, max(monetary_thresholds), 0.01)
  } else {
    c(0, seq(3.8, max(monetary_thresholds), 0.05))
  }
  
  specific_tt_all_mc <- expand.grid(
    tt = travel_time_thresholds,
    mc = monetary_smooth_distribution
  ) 
  specific_mc_all_tt <- expand.grid(
    tt = seq(1, max(travel_time_thresholds), 1),
    mc = monetary_thresholds
  )
  iterator <- setDT(rbind(specific_tt_all_mc, specific_mc_all_tt))
  iterator <- unique(iterator)
  
  routed_points <- fread(routed_points_path)
  monetary_column <- ifelse(
    type == "affordability",
    "relative_monthly_cost",
    "monetary_cost"
  )

  future::plan(future::multisession, workers = getOption("N_CORES") / 3)
  
  accessibility <- furrr::future_pmap(
    iterator,
    function(tt, mc) {
      loadNamespace("data.table")
      sum_opp <- frontiers[travel_time <= tt][get(monetary_column) <= mc]
      sum_opp <- sum_opp[sum_opp[, .I[1], by = .(from_id, to_id, mode)]$V1]
      sum_opp <- sum_opp[
        ,
        .(sum_opp = sum(dest_jobs)),
        keyby = .(from_id, mode)
      ]
      
      access <- data.table(
        from_id = rep(routed_points$id, each = 3),
        mode = rep(unique(frontiers$mode), length(routed_points$id))
      )
      access[sum_opp, on = c("from_id", "mode"), access := i.sum_opp]
      access[is.na(access), access := 0]
      access[, `:=`(travel_time = tt, monetary_limit = mc)]
      
      return(access)
    }
  )
  
  future::plan(future::sequential)
  
  accessibility <- rbindlist(accessibility)
  cost_cutoff_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  setnames(accessibility, old = "monetary_limit", cost_cutoff_column)
  
  access_dir <- "../data/data/access"
  if (!dir.exists(access_dir)) dir.create(access_dir)
  
  access_basename <- paste0(cost_cutoff_column, "_access.rds")
  access_path <- file.path(access_dir, access_basename)
  saveRDS(accessibility, access_path)
  
  return(access_path)
}


# access_path <- tar_read(accessibility)[1]
# grid_path <- tar_read(grid_res_8)
# type <- tar_read(cost_type)[1]
calculate_palma <- function(access_path, grid_path, type) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  
  access[
    grid,
    on = c(from_id = "id_hex"),
    `:=`(decile = i.decil, population = i.pop_total)
  ]
  
  # nest each accessibility distribution and calculate its palma ratio
  
  cost_cutoff_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  env <- environment()
  
  palma <- access[
    ,
    .(data = list(.SD)),
    keyby = .(mode, travel_time, cost_cutoff = get(cost_cutoff_column))
  ]
  palma[, palma := vapply(data, palma_calculator, numeric(1))]
  palma[, data := NULL]
  setnames(palma, old = "cost_cutoff", cost_cutoff_column)
  
  palma_basename <- paste0(cost_cutoff_column, "_palma.rds")
  palma_path <- file.path("../data/data/access", palma_basename)
  saveRDS(palma, palma_path)
  
  return(palma_path)
}


# access_dist <- palma$data[[1]]
palma_calculator <- function(access_dist) {
  richest_10 <- access_dist[decile == 10]
  poorest_40 <- access_dist[decile >= 1 & decile <= 4]
  
  numerator <- weighted.mean(
    richest_10$access,
    w = richest_10$population,
    na.rm = TRUE
  )
  
  denominator <- weighted.mean(
    poorest_40$access,
    w = poorest_40$population,
    na.rm = TRUE
  )
  
  palma_ratio <- numerator / denominator
  
  return(palma_ratio)
}


# graph_path <- tar_read(graph_dir)
# points_path <- tar_read(r5_points)
identify_problematic_hexs <- function(graph_path, points_path) {
  points <- fread(points_path)
  r5r_core <- setup_r5(graph_path, verbose = FALSE, use_elevation = TRUE)
  ttm <- travel_time_matrix(
    r5r_core,
    origins = points,
    destinations = points,
    mode = "WALK",
    departure_datetime = as.POSIXct(
      "08-01-2020 07:00:00",
      format = "%d-%m-%Y %H:%M:%S"
    ),
    max_trip_duration = 120,
    walk_speed = 3.6,
    draws_per_minute = 1,
    n_threads = getOption("N_CORES"),
    verbose = FALSE
  )
  
  # we are considering problematic the hexagons whose unitary accessibility is
  # at least 20 hexagons less than their neighbors'
  
  unitary_access <- ttm[, .N, by = from_id]
  unitary_access[
    ,
    avg_neighbor_access := vapply(
      from_id,
      FUN.VALUE = numeric(1),
      FUN = function(hex) {
        neighbors <- h3jsr::get_kring_list(hex)[[1]][[2]]
        neighbors_access <- unitary_access[from_id %chin% neighbors]$N
        avg_neighbors_access <- mean(neighbors_access)
        return(avg_neighbors_access)
      }
    )
  ]
  
  problematic <- unitary_access[avg_neighbor_access - N > 20]$from_id
  
  return(problematic)
}


# access_path <- tar_read(accessibility)[1]
# type <- tar_read(cost_type)[1]
# problematic_hexs <- tar_read(problematic_hexagons)
adjust_transit_access <- function(access_path, type, problematic_hexs) {
  access_dist <- readRDS(access_path)
  
  monetary_column <- ifelse(
    type == "affordability",
    "affordability",
    "absolute_cost"
  )
  
  problematic_hexs_df <- data.table(
    id = problematic_hexs,
    neighbors = lapply(
      problematic_hexs,
      function(hex) h3jsr::get_kring_list(hex)[[1]][[2]]
    )
  )
  
  transit_access_dist <- access_dist[mode == "only_transit"]
  transit_access_dist <- transit_access_dist[! from_id %chin% problematic_hexs]
  transit_access_dist[, mode := NULL]
  setkeyv(
    transit_access_dist,
    c("from_id", "travel_time", monetary_column)
  )
  
  future::plan(future::multisession, workers = getOption("N_CORES"))
  
  access_dist[
    from_id %chin% problematic_hexs & mode == "only_transit",
    neighbors_access := furrr::future_pmap_dbl(
      list(
        hex = from_id,
        tt = travel_time,
        mc = get(monetary_column))
      ,
      function(hex, tt, mc) {
        neighbors <- problematic_hexs_df[id == hex]$neighbors[[1]]
        
        their_access <- transit_access_dist[
          from_id %chin% neighbors &
            travel_time == tt &
            get(monetary_column) == mc
        ]$access
        
        mean(their_access)
      }
    )
  ]
  
  future::plan(future::sequential)
  
  access_dist[!is.na(neighbors_access), access := neighbors_access]
  access_dist[, neighbors_access := NULL]
  setindexv(access_dist, NULL)
  access_dist <- access_dist[
    order(from_id, mode, travel_time, get(monetary_column))
  ]
  
  access_dir <- "../data/data/access"
  if (!dir.exists(access_dir)) dir.create(access_dir)
  
  access_basename <- paste0(monetary_column, "_adjusted_access.rds")
  access_path <- file.path(access_dir, access_basename)
  saveRDS(access_dist, access_path)
  
  return(access_path)
}