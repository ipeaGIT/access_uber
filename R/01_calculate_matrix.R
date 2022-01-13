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
fill_uber_matrix <- function(uber_data_path) {
  uber_data <- readRDS(uber_data_path)
  uber_data <- uber_data[
    date_block_2019 == "mar8_dec20" &
      weekday_weekend == "weekday" &
      time_block == "morning_peak"
  ]
  uber_data <- uber_data[
    ,
    mean_travel_time := (mean_distance_km / mean_trip_speed_kmh) * 60
  ]
  
  desired_uber_columns <- c(
    "pickup_hex8",
    "dropoff_hex8",
    "mean_approx_fare_local",
    "mean_travel_time",
    "mean_distance_km",
    "num_trips"
  )
  uber_data[, setdiff(names(uber_data), desired_uber_columns) := NULL]
  setnames(
    uber_data,
    old = desired_uber_columns,
    new = c("from", "to", "cost", "travel_time", "distance", "num_trips")
  )
  
  # dodgr_dists() uses a column name 'dists' to calculate the shortest path
  # between two edges. but if a 'weights' column is present, it calculates the
  # shortest path according to those weights, but return the final distance
  # based on 'dists'.
  # so here, we calculate two matrices: one to calculate the shortest path based
  # on the travel time, and another to calculate the distance of this shortest
  # path. we first calculate a matrix with 'travel_time' as 'dists', and then
  # use these travel times as the weights when calculating the distance matrix
  
  setnames(
    uber_data,
    old = c("travel_time", "distance"),
    new = c("dists", "length")
  )
  travel_time_matrix <- dodgr_dists(uber_data)
  travel_time_matrix <- as.data.table(travel_time_matrix, keep.rownames = TRUE)
  travel_time_matrix <- melt(travel_time_matrix, id.vars = "rn")
  setnames(
    travel_time_matrix,
    c("rn", "variable", "value"),
    c("from_id", "to_id", "travel_time")
  )
  travel_time_matrix <- travel_time_matrix[!is.na(travel_time)]
  
  setnames(uber_data, old = c("dists", "length"), new = c("weights", "dists"))
  distance_matrix <- dodgr_dists(uber_data)
  distance_matrix <- as.data.table(distance_matrix, keep.rownames = TRUE)
  distance_matrix <- melt(distance_matrix, id.vars = "rn")
  setnames(
    distance_matrix,
    c("rn", "variable", "value"),
    c("from_id", "to_id", "distance")
  )
  distance_matrix <- distance_matrix[!is.na(distance)]
  
  full_matrix <- travel_time_matrix[distance_matrix, on = c("from_id", "to_id")]
  
  # dodgr may theoretically return values lower than those found in the uber
  # data. for example, if in the uber data from A to B takes 10 minutes, but
  # from A to C takes 5 and from B to C takes 3, then dodgr will return the
  # shortest path from A to B, which is 8. but we want to use the values found
  # in the uber data whenever possible. so we check for this possibility and
  # substitute the values if appropriate. we just check if travel_time is
  # different than the actual value because this was the variable used to route
  
  setnames(
    uber_data,
    old = c("weights", "dists"),
    new = c("travel_time", "distance")
  )
  
  full_matrix[
    uber_data,
    on = c(from_id = "from", to_id = "to"),
    `:=`(
      actual_distance = i.distance,
      actual_travel_time = i.travel_time
    )
  ]
  
  tol <- sqrt(.Machine$double.eps)
  full_matrix[
    !is.na(actual_travel_time) & abs(actual_travel_time - travel_time) > tol,
    `:=`(distance = actual_distance, travel_time = actual_travel_time)
  ]
  
  full_matrix[, `:=`(actual_distance = NULL, actual_travel_time = NULL)]
  
  # dodgr_dists() always calculates the travel time and distance from a hex to
  # itself as 0. but the uber data contains entries with travel times and
  # distances from hexagons to themselves. so we calculate the average travel
  # time and distance of these cases and use it to fill the values of trips from
  # a hex to itself that are not present in uber_data
  
  from_hex_to_itself <- uber_data[from == to]
  avg_distance <- weighted.mean(
    from_hex_to_itself$distance,
    w = from_hex_to_itself$num_trips
  )
  avg_travel_time <- weighted.mean(
    from_hex_to_itself$travel_time,
    w = from_hex_to_itself$num_trips
  )
  
  full_matrix[
    from_id == to_id & travel_time == 0,
    `:=`(distance = avg_distance, travel_time = avg_travel_time)
  ]
  
  # to calculate the cost of each route, we model the effects of the travel time
  # and the distance on the monetary cost, based on the data from uber_data.
  # then we use this model to calculate the cost of routes that were calculated
  # with dodgr_dists()
  
  cost_lm <- lm(cost ~ distance + travel_time, uber_data)
  coefs <- cost_lm$coefficients
  coefs[is.na(coefs)] <- 0
  
  full_matrix[uber_data, on = c(from_id = "from", to_id = "to"), cost := i.cost]
  full_matrix[
    is.na(cost),
    cost := coefs["(Intercept)"] +
      distance * coefs["distance"] +
      travel_time * coefs["travel_time"]
  ]
  
  # since r5r::travel_time_matrix() always returns travel times as integers, we
  # round travel time to the closest integer. also, we round the costs to the
  # closest 0.05, because that's the lowest monetary unit we will be using
  
  full_matrix[
    ,
    `:=`(travel_time = round(travel_time), cost = round(cost / 0.05) * 0.05)
  ]
  
  # we won't need the distance between two hexagons anymore (we needed to
  # calculate the cost), se we can drop it
  
  full_matrix[, distance := NULL]
  
  matrix_dir <- "../../data/access_uber/ttmatrix"
  if (!dir.exists(matrix_dir)) dir.create(matrix_dir)
  
  matrix_path <- file.path(matrix_dir, "only_uber_full_matrix.rds")
  saveRDS(full_matrix, matrix_path)
  
  return(matrix_path)
}


# uber_matrix_path <- tar_read(full_uber_matrix)
# stations_path <- tar_read(rapid_transit_stations)
# graph_path <- tar_read(graph_dir)
# points_path <- tar_read(r5_points)
# grid_path <- tar_read(grid_res_8)
calculate_uber_first_mile <- function(uber_matrix_path,
                                      stations_path,
                                      graph_path,
                                      points_path,
                                      grid_path) {
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
  
  uber_matrix <- readRDS(uber_matrix_path)
  
  first_mile_matrix <- merge(
    uber_matrix,
    stations_to_hex,
    by.x = "to_id",
    by.y = "id_hex"
  )
  setcolorder(first_mile_matrix, c("from_id", "to_id", "id", "travel_time"))
  setnames(first_mile_matrix, old = "id", new = "to_station")
  
  # calculate the travel time matrix from the rapid transit stations to all
  # possible destinations. the departure time depends on the time the uber would
  # arrive at the station, so for each unique uber trip length we calculate one
  # matrix
  
  r5r_core <- setup_r5(graph_path, verbose = FALSE, use_elevation = TRUE)
  
  uber_trip_lengths <- unique(first_mile_matrix$travel_time)
  uber_trip_lengths <- uber_trip_lengths[order(uber_trip_lengths)]
  
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
          n_threads = getOption("N_CORES"),
          verbose = FALSE
        )

        # capture.output(
        #   matrix <- pareto_frontier(
        #     r5r_core,
        #     origins = stations,
        #     destinations = points,
        #     mode = c("WALK", "TRANSIT"),
        #     departure_datetime = departure_datetime,
        #     max_trip_duration = max_trip_duration,
        #     max_walk_dist = 1000,
        #     monetary_cost_cutoffs = seq(400, 1000, 500),
        #     fare_calculator = "rio-de-janeiro",
        #     n_threads = getOption("N_CORES"),
        #     verbose = FALSE
        #   ),
        #   file = file.path(tempfile("pareto_frontier_log", fileext = ".txt"))
        # )
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
  
  # bind remaining_matrix with first_mile_matrix, calculate the total travel
  # time and keep only the fastest trip between two hexagons
  
  matrix <- merge(
    first_mile_matrix,
    remaining_matrix,
    by.x = c("to_station", "travel_time"),
    by.y = c("fromId", "departure_minute"),
    allow.cartesian = TRUE
  )
  
  setnames(
    matrix,
    old = c(
      "to_station",
      "travel_time",
      "from_id",
      "to_id",
      "toId",
      "travel_time.y"
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