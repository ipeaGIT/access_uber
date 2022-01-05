# grid_path <- tar_read(grid_res_8)
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
# grid_path <- tar_read(grid_res_8)
calculate_uber_first_mile <- function(uber_data_path,
                                      stations_path,
                                      graph_path,
                                      points_path,
                                      grid_path) {
  uber_data <- readRDS(uber_data_path)
  stations <- fread(stations_path, encoding = "UTF-8")
  points <- fread(points_path)
  grid <- setDT(readRDS(grid_path))
  
  # find inside which hexagon each station is
  
  grid_columns <- c("id_hex", "geometry")
  grid[, setdiff(names(grid), grid_columns) := NULL]
  grid <- st_sf(grid)
  
  stations_sf <- st_as_sf(stations, coords = c("lon", "lat"), crs = 4326)
  
  stations_to_hex <- setDT(st_join(stations_sf, grid))
  stations_to_hex[, geometry := NULL]
  
  # calculate travel times from the origins to the transit stations, considering
  # Uber's travel times
  
  uber_data <- uber_data[
    date_block_2019 == "mar8_dec20" &
      weekday_weekend == "weekday" &
      time_block == "morning_peak"
  ]
  uber_data <- uber_data[
    ,
    mean_travel_time := (mean_distance_km / mean_trip_speed_kmh) * 60
  ]
  desired_uber_columns <- c("pickup_hex8", "dropoff_hex8", "mean_travel_time")
  uber_data[, setdiff(names(uber_data), desired_uber_columns) := NULL]
  
  uber_matrix <- merge(
    uber_data,
    stations_to_hex,
    by.x = "dropoff_hex8",
    by.y = "id_hex"
  )
  setcolorder(
    uber_matrix,
    c("pickup_hex8", "dropoff_hex8", "id", "mean_travel_time")
  )
  setnames(uber_matrix, old = "id", new = "dropoff_station")
  
  # calculate the travel time matrix from the rapid transit stations to all
  # possible destinations. the departure time depends on the time the uber would
  # arrive at the station, so for each unique uber trip length we calculate one
  # matrix
  
  r5r_core <- setup_r5(graph_path, verbose = FALSE)
  
  uber_trip_lengths <- unique(uber_matrix$mean_travel_time)
  
  remaining_matrix <- lapply(
    uber_trip_lengths,
    function(i) {
      departure_datetime <- as.POSIXct(
        "08-01-2020 07:00:00",
        format = "%d-%m-%Y %H:%M:%S"
      )
      departure_datetime <- departure_datetime + 60 * i
      max_trip_duration <- 120L - i
      
      if (max_trip_duration > 0) {
        matrix <- travel_time_matrix(
          r5r_core,
          origins = stations,
          destinations = points,
          mode = c("WALK", "TRANSIT"),
          departure_datetime = departure_datetime,
          max_trip_duration = max_trip_duration,
          max_walk_dist = 1000,
          n_threads = getOption("R5R_THREADS"),
          verbose = FALSE
        )
      } else {
        matrix <- data.table(
          fromId = character(),
          toId = character(),
          travel_time = character()
        )
      }
      
      matrix
    }
  )
  
  names(remaining_matrix) <- uber_trip_lengths
  remaining_matrix <- rbindlist(remaining_matrix, idcol = "departure_minute")
  remaining_matrix[, departure_minute := as.integer(departure_minute)]
  
  # bind remaining_matrix with uber_matrix, calculate the total travel time and
  # keep only the fastest trip between two hexagons
  
  matrix <- merge(
    uber_matrix,
    remaining_matrix,
    by.x = c("dropoff_station", "mean_travel_time"),
    by.y = c("fromId", "departure_minute"),
    allow.cartesian = TRUE
  )
  
  setnames(
    matrix,
    old = c(
      "dropoff_station",
      "mean_travel_time",
      "pickup_hex8",
      "dropoff_hex8",
      "toId",
      "travel_time"
    ),
    new = c(
      "intermediate_station",
      "first_mile_time",
      "from_id",
      "intermediate_hex",
      "to_id",
      "remaining_time"
    )
  )
  setcolorder(
    matrix,
    c(
      "from_id",
      "intermediate_hex",
      "intermediate_station",
      "to_id",
      "first_mile_time",
      "remaining_time"
    )
  )
  matrix[, travel_time := first_mile_time + remaining_time]
  matrix <- matrix[
    matrix[, .I[travel_time == min(travel_time)], by = .(from_id, to_id)]$V1
  ]
  
  # there may be many trips between the same two points whose travel time equals
  # the minimum travel time (e.g. imagine that you can get from point A to point
  # B using 3 stations as possible intermediates and two of those trips have the
  # same total travel time, which is lower than the third). therefore we need to
  # filter 'matrix' to keep only one entry for each pair, otherwise we will
  # double (triple, quadruple, ...) count the opportunities when estimating the
  # accessibility.
  
  matrix <- matrix[matrix[, .I[1], by = .(from_id, to_id)]$V1]
  
  matrix_dir <- "../../data/access_uber/ttmatrix"
  if (!dir.exists(matrix_dir)) dir.create(matrix_dir)
  
  matrix_path <- file.path(matrix_dir, "uber_first_mile_matrix.rds")
  saveRDS(matrix, matrix_path)
  
  return(matrix_path)
}