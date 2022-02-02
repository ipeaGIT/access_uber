# frontier_paths <- tar_read(affordability_frontiers)
# travel_time_thresholds <- tar_read(travel_time_thresholds)
# affordability_thresholds <- tar_read(affordability_thresholds)
# grid_path <- tar_read(grid_res_8)
calculate_access <- function(frontier_paths,
                             travel_time_thresholds,
                             affordability_thresholds,
                             grid_path) {
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

  future::plan(future::multisession, workers = getOption("N_CORES") / 3)
  
  accessibility <- furrr::future_pmap(
    iterator,
    function(tt, af) {
      loadNamespace("data.table")
      access <- frontiers[travel_time <= tt][relative_monthly_cost <= af]
      access <- access[access[, .I[1], keyby = .(from_id, to_id, mode)]$V1]
      access <- access[, .(access = sum(dest_jobs)), keyby = .(from_id, mode)]
      access[, `:=`(travel_time = tt, affordability = af)]
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