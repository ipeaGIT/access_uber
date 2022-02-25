# frontier_paths <- tar_read(affordability_frontiers)
# travel_time_thresholds <- tar_read(travel_time_thresholds)
# affordability_thresholds <- tar_read(affordability_thresholds)
# grid_path <- tar_read(grid_res_8)
# routed_points_path <- tar_read(r5_points)
calculate_access <- function(frontier_paths,
                             travel_time_thresholds,
                             affordability_thresholds,
                             grid_path,
                             routed_points_path) {
  frontiers <- lapply(frontier_paths, readRDS)
  frontiers_names <- gsub(".rds", "", basename(frontier_paths))
  names(frontiers) <- frontiers_names
  frontiers <- rbindlist(frontiers, fill = TRUE, idcol = "mode")
  
  grid <- setDT(readRDS(grid_path))
  frontiers[grid, on = c(to_id = "id_hex"), dest_jobs := i.empregos_total]

  # loop over different combinations of travel time and affordability cutoffs.
  # we generate two distributions, one that smoothly increases the affordability
  # limits with only the specified temporal thresholds and another that smoothly
  # increases the travel time limits with only the specified monetary cutoffs,
  # and bind them together
  
  specific_tt_all_af <- expand.grid(
    tt = travel_time_thresholds,
    af = seq(0, max(affordability_thresholds), 0.01)
  ) 
  specific_af_all_tt <- expand.grid(
    tt = seq(1, max(travel_time_thresholds), 1),
    af = affordability_thresholds
  )
  iterator <- setDT(rbind(specific_tt_all_af, specific_af_all_tt))
  iterator <- unique(iterator)
  
  routed_points <- fread(routed_points_path)

  future::plan(future::multisession, workers = getOption("N_CORES") / 3)
  
  accessibility <- furrr::future_pmap(
    iterator,
    function(tt, af) {
      loadNamespace("data.table")
      sum_opp <- frontiers[travel_time <= tt][relative_monthly_cost <= af]
      sum_opp <- sum_opp[sum_opp[, .I[1], keyby = .(from_id, to_id, mode)]$V1]
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
      access[, `:=`(travel_time = tt, affordability = af)]
      
      return(access)
    }
  )
  
  future::plan(future::sequential)
  
  accessibility <- rbindlist(accessibility)
  
  access_dir <- "../../data/access_uber/access"
  if (!dir.exists(access_dir)) dir.create(access_dir)
  
  access_path <- file.path(access_dir, "access.rds")
  saveRDS(accessibility, access_path)
  
  return(access_path)
}


# access_path <- tar_read(accessibility)
# grid_path <- tar_read(grid_res_8)
calculate_palma <- function(access_path, grid_path) {
  access <- readRDS(access_path)
  grid <- setDT(readRDS(grid_path))
  
  access[
    grid,
    on = c(from_id = "id_hex"),
    `:=`(decile = i.decil, population = i.pop_total)
  ]
  
  # nest each accessibility distribution and calculate its palma ratio
  
  palma <- access[
    ,
    .(data = list(.SD)),
    keyby = .(mode, travel_time, affordability)
  ]
  palma[, palma := vapply(data, palma_calculator, numeric(1))]
  palma[, data := NULL]
  
  palma_path <- "../../data/access_uber/access/palma.rds"
  saveRDS(palma, palma_path)
  
  return(palma_path)
}


# access_dist <- access$data[[1]]
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
  
  palma <- numerator / denominator
  
  return(palma)
}