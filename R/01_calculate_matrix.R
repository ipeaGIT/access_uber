# grid_path <- tar_read(grid)
generate_r5_points <- function(grid_path) {
  grid <- setDT(readRDS(grid_path))
  
  # keep only cells that have either population > 0 or opportunities > 0
  
  grid <- grid[
    pop_total > 0 |
      renda_total > 0 |
      empregos_total > 0 |
      saude_total > 0 |
      edu_total > 0
  ]
  
  # get centroids and save it as csv
  
  centroids <- grid[, .(id_hex, geometry)]
  centroids <- st_sf(centroids)
  centroids <- st_centroid(centroids)
  
  coords <- as.data.table(st_coordinates(centroids))
  
  centroids <- setDT(cbind(centroids, coords))
  centroids[, geometry := NULL]
  setnames(centroids, old = c("id_hex", "X", "Y"), new = c("id", "lon", "lat"))
  
  # save object and return path
  
  file_path <- paste0("../../data/access_uber/points_rio_09_2019.csv")
  fwrite(centroids, file_path)
  
  return(file_path)
}


# uber_data_path <- tar_read(uber_data)
# stations_path <- tar_read(rapid_transit_stations)
# graph_path <- tar_read(graph_dir)
# points_path <- tar_read(r5_points)
calculate_uber_first_mile <- function(uber_data_path,
                                      stations_path,
                                      graph_path,
                                      points_path) {
  stations <- fread(stations_path, encoding = "UTF-8")
  points <- fread(points_path)
  
  # TODO: calculate travel times from the origins to the transit stations,
  # considering Uber's travel times
  
  # calculate the travel time matrix from the rapid transit stations to all
  # possible destinations
  
  r5r_core <- setup_r5(graph_path, verbose = FALSE)
  
  remaining_matrix <- travel_time_matrix(
    r5r_core,
    origins = stations,
    destinations = points,
    mode = c("WALK", "TRANSIT"),
    departure_datetime = as.POSIXct(
      "08-01-2020 07:00:00",
      format = "%d-%m-%Y %H:%M:%S"
    ),
    max_walk_dist = 1000,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  # TODO: bind the remaining_matrix with the uber_first_mile matrix
  
  matrix_dir <- "../../data/access_uber/ttmatrix"
  if (!dir.exists(matrix_dir)) dir.create(matrix_dir)
  
  matrix_basename <- "uber_first_mile.rds"
  matrix_path <- file.path(matrix_dir, matrix_basename)
  saveRDS(remaining_matrix, matrix_path)
  
  return(matrix_path)
}