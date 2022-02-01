# frontier_path <- tar_read(all_frontiers)[1]
get_frontier_type <- function(frontier_path) {
  path_basename <- basename(frontier_path)
  
  frontier_type <- fcase(
    grepl("^transit", path_basename), "transit",
    grepl("only_uber", path_basename), "only_uber",
    grepl("uber_fm_transit_combined", path_basename), "uber_fm_transit_combined"
  )
  
  return(frontier_type)
}