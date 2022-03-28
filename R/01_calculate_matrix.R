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
  
  file_path <- paste0("../data/data/points_rio_09_2019.csv")
  fwrite(centroids, file_path)
  
  return(file_path)
}


# points_path <- tar_read(r5_points)
# graph_path <- tar_read(graph_dir)
calculate_walk_matrix <- function(points_path, graph_path) {
  points <- fread(points_path)
  
  r5r_core <- setup_r5(graph_path, verbose = FALSE, use_elevation = TRUE)
  
  walking_speed <- 3.6
  
  walk_matrix <- travel_time_matrix(
    r5r_core,
    origins = points,
    destinations = points,
    mode = "WALK",
    departure_datetime = as.POSIXct(
      "08-01-2020 07:00:00",
      format = "%d-%m-%Y %H:%M:%S"
    ),
    time_window = 1,
    draws_per_minute = 1,
    max_trip_duration = 30,
    max_walk_dist = 1800,
    walk_speed = walking_speed,
    n_threads = getOption("N_CORES"),
    verbose = FALSE
  )
  
  # as the uber matrix includes trips from the cells to themselves where the
  # travel time and cost are not 0, we also consider that walking trips from the
  # hexagons to themselves are not instantaneous. so we calculate an "average"
  # travel time as the edge length divided by the walking speed.
  # also, very few walking trips from one hexagon to another may be faster than
  # this calculated minimum walking duration, and we substitute the travel time
  # of those by the calculated value as well
  
  edge_length <- 0.461354684
  min_walking_duration <- edge_length / walking_speed * 60
  min_walking_duration <- as.integer(round(min_walking_duration))
  
  walk_matrix[
    from_id == to_id & travel_time < min_walking_duration,
    travel_time := min_walking_duration
  ]
  walk_matrix[
    travel_time < min_walking_duration,
    travel_time := min_walking_duration
  ]
  
  file_path <- paste0("../data/data/pfrontiers/walk_matrix.rds")
  saveRDS(walk_matrix, file_path)
  
  return(file_path)
}


# points_path <- tar_read(r5_points)
# graph_path <- tar_read(graph_dir)
# rio_fare_calculator_path <- tar_read(rio_fare_calculator)
calculate_transit_frontier <- function(points_path,
                                       graph_path,
                                       rio_fare_calculator_path) {
  points <- fread(points_path)
  
  r5r_core <- setup_r5(graph_path, verbose = FALSE, use_elevation = TRUE)
  
  # to calculate the frontier, we have to specify the monetary cost cutoffs. we
  # pick values to use as cutoffs based on rio's fare values (for now we're
  # limiting this values to BRL 15 max).
  
  max_rides <- 4
  
  rio_fare_calculator <- r5r::read_fare_calculator(rio_fare_calculator_path)
  possible_fare_values <- generate_possible_fare_values(
    rio_fare_calculator,
    max_value = 18,
    max_rides = max_rides
  )
  
  walking_speed <- 3.6
  
  frontier <- pareto_frontier(
    r5r_core,
    origins = points,
    destinations = points,
    mode = c("WALK", "TRANSIT"),
    departure_datetime = as.POSIXct(
      "08-01-2020 07:00:00",
      format = "%d-%m-%Y %H:%M:%S"
    ),
    time_window = 60,
    draws_per_minute = 1,
    max_trip_duration = 120,
    max_walk_dist = 1800,
    max_rides = max_rides,
    walk_speed = walking_speed,
    monetary_cost_cutoffs = possible_fare_values,
    fare_calculator_settings = rio_fare_calculator,
    n_threads = getOption("N_CORES"),
    verbose = FALSE
  )
  frontier[, percentile := NULL]
  
  # as the uber matrix includes trips from the cells to themselves where the
  # travel time and cost are not 0, we also consider that walking trips from the
  # hexagons to themselves are not instantaneous. so we calculate an "average"
  # travel time as the edge length divided by the walking speed.
  # also, very few walking trips from one hexagon to another may be faster than
  # this calculated minimum walking duration, and we substitute the travel time
  # of those by the calculated value as well
  
  edge_length <- 0.461354684
  min_walking_duration <- edge_length / walking_speed * 60
  min_walking_duration <- as.integer(round(min_walking_duration))
  
  frontier[
    from_id == to_id & travel_time < min_walking_duration,
    travel_time := min_walking_duration
  ]
  frontier[
    travel_time < min_walking_duration & monetary_cost == 0,
    travel_time := min_walking_duration
  ]
  
  parent_dir <- "../../data/access_uber/pfrontiers"
  if (!dir.exists(parent_dir)) dir.create(parent_dir)
  
  frontier_dir <- file.path(parent_dir, "absolute")
  if (!dir.exists(frontier_dir)) dir.create(frontier_dir)
  
  frontier_path <- file.path(frontier_dir, "only_transit.rds")
  saveRDS(frontier, frontier_path)
  
  return(frontier_path)
}

# pickup_data_path <- tar_read(pickup_data)
# grid_path <- tar_read(grid_res_8)
aggregate_waiting_times <- function(pickup_data_path, grid_path) {
  pickup_data <- fread(pickup_data_path)
  pickup_data <- pickup_data[
    date_block_2019 == "mar8_dec20" &
      weekday_weekend == "weekday" &
      time_block == "morning_peak"
  ]
  
  parent_res_8 <- h3jsr::get_parent(pickup_data$hex_addr, res = 8)
  pickup_data <- cbind(pickup_data, parent_hex = parent_res_8)
  
  aggregated_data <- pickup_data[
    ,
    .(
      num_pickups = sum(num_pickups),
      frac_driver_cancel = weighted.mean(frac_driver_cancel, w = num_pickups),
      mean_wait_time = weighted.mean(mean_wait_time, w = num_pickups)
    ),
    keyby = parent_hex
  ]
  setnames(aggregated_data, old = "parent_hex", new = "hex_addr")
  
  # keeping only hexagons inside the city of rio
  
  grid <- readRDS(grid_path)
  aggregated_data <- aggregated_data[hex_addr %chin% grid$id_hex]
  
  data_dir <- "../data/data/"
  path <- file.path(data_dir, "pickup_anonymized_res_8.rds")
  saveRDS(aggregated_data, path)
  
  return(path)
}


# uber_data_path <- tar_read(uber_data)
# pickup_data_path <- tar_read(pickup_data_res_8)
# grid_path <- tar_read(grid_res_8)
fill_uber_matrix <- function(uber_data_path, pickup_data_path, grid_path) {
  uber_data <- fread(uber_data_path)
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
  travel_time_matrix <- melt(
    travel_time_matrix,
    id.vars = "rn",
    variable.factor = FALSE
  )
  setnames(
    travel_time_matrix,
    c("rn", "variable", "value"),
    c("from_id", "to_id", "travel_time")
  )
  travel_time_matrix <- travel_time_matrix[!is.na(travel_time)]
  
  setnames(uber_data, old = c("dists", "length"), new = c("weights", "dists"))
  distance_matrix <- dodgr_dists(uber_data)
  distance_matrix <- as.data.table(distance_matrix, keep.rownames = TRUE)
  distance_matrix <- melt(
    distance_matrix,
    id.vars = "rn",
    variable.factor = FALSE
  )
  setnames(
    distance_matrix,
    c("rn", "variable", "value"),
    c("from_id", "to_id", "distance")
  )
  distance_matrix <- distance_matrix[!is.na(distance)]
  
  full_matrix <- travel_time_matrix[distance_matrix, on = c("from_id", "to_id")]
  
  # dodgr will always find a route from the origin to itself, even if this
  # origin does not appear as a pickup in the uber data. we remove such entries
  # from the full matrix
  
  pickup_hexs <- unique(uber_data$from)
  full_matrix <- full_matrix[from_id %chin% pickup_hexs]
  
  # up to this point we needed the uber_data including all cities from the
  # metropolitan region to calculate the time and distance between all hexagons
  # (some hexagons at the border of the city may only be connected to hexagons
  # from other cities, so if we filtered the data from the beginning to drop
  # these hexagons we could have a full_matrix not as complete). we don't need
  # these hexs anymore, so we can filter them out.
  
  grid <- setDT(readRDS(grid_path))
  
  uber_data <- uber_data[from %chin% grid$id_hex & to %chin% grid$id_hex]
  full_matrix <- full_matrix[
    from_id %chin% grid$id_hex & to_id %chin% grid$id_hex
  ]
  
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
  # a hex to itself that are not present in uber_data.
  
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
  # with dodgr_dists().
  # uber_data is filtered to include only rio's hexagons, because the cost
  # function may be different out of rio.
  
  cost_lm <- lm(cost ~ distance, uber_data)
  coefs <- cost_lm$coefficients
  coefs[is.na(coefs)] <- 0
  
  full_matrix[uber_data, on = c(from_id = "from", to_id = "to"), cost := i.cost]
  full_matrix[
    is.na(cost),
    cost := coefs["(Intercept)"] + distance * coefs["distance"]
  ]
  
  # we have to add to the travel time the mean waiting time at each origin,
  # after converting it from seconds to minutes.
  # we do this after routing and calculating the cost because we don't want to
  # "propagate" waiting times when routing, and because waiting times do not
  # influence the monetary cost of a trip.
  # some hexagons do not have waiting time data, so we calculate the weighted
  # mean waiting time in the city of rio and substitute the NAs using this value
  
  pickup_data <- readRDS(pickup_data_path)
  pickup_data <- pickup_data[hex_addr %chin% grid$id_hex]
  
  full_matrix[
    pickup_data,
    on = c(from_id = "hex_addr"),
    waiting_time := i.mean_wait_time
  ]
  
  avg_mean_wait_time <- weighted.mean(
    pickup_data$mean_wait_time,
    w = pickup_data$num_pickups
  )
  full_matrix[is.na(waiting_time), waiting_time := avg_mean_wait_time]
  
  full_matrix[, waiting_time := waiting_time / 60]
  full_matrix[, travel_time := travel_time + waiting_time]
  
  # since r5r::travel_time_matrix() always returns travel times as integers, we
  # round travel time to the closest integer. also, we round the costs to the
  # closest 0.05, because that's the lowest monetary unit we will be using
  
  full_matrix[
    ,
    `:=`(travel_time = round(travel_time), cost = round(cost / 0.05) * 0.05)
  ]
  
  # we won't need the distance between two hexagons anymore (we needed to
  # calculate the cost), and neither the waiting time at origin, so we can drop
  # them
  
  full_matrix[, c("distance", "waiting_time") := NULL]
  setnames(full_matrix, old = "cost", new = "monetary_cost")
  
  matrix_dir <- "../data/data/pfrontiers"
  if (!dir.exists(matrix_dir)) dir.create(matrix_dir)
  
  matrix_path <- file.path(matrix_dir, "full_uber_matrix.rds")
  saveRDS(full_matrix, matrix_path)
  
  return(matrix_path)
}

# uber_matrix_path <- tar_read(full_uber_matrix)
# walk_matrix_path <- tar_read(walk_only_matrix)
calculate_walk_uber_frontier <- function(uber_matrix_path, walk_matrix_path) {
  uber_matrix <- readRDS(uber_matrix_path)
  
  walk_only_matrix <- readRDS(walk_matrix_path)
  walk_only_matrix[, monetary_cost := 0]
  
  walk_uber_frontier <- rbind(uber_matrix, walk_only_matrix)
  walk_uber_frontier <- keep_pareto_frontier(walk_uber_frontier)
  
  parent_dir <- "../data/data/pfrontiers"
  if (!dir.exists(parent_dir)) dir.create(parent_dir)
  
  frontier_dir <- file.path(parent_dir, "absolute")
  if (!dir.exists(frontier_dir)) dir.create(frontier_dir)
  
  frontier_path <- file.path(frontier_dir, "only_uber.rds")
  saveRDS(walk_uber_frontier, frontier_path)
  
  return(frontier_path)
}


# uber_matrix_path <- tar_read(full_uber_matrix)
# stations_path <- tar_read(rapid_transit_stations)
# graph_path <- tar_read(graph_dir)
# points_path <- tar_read(r5_points)
# grid_path <- tar_read(grid_res_8)
# rio_fare_calculator_path <- tar_read(rio_fare_calculator)
calculate_uber_first_mile_frontier <- function(uber_matrix_path,
                                               stations_path,
                                               graph_path,
                                               points_path,
                                               grid_path,
                                               rio_fare_calculator_path) {
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
  # Uber's travel times.
  # first filter uber_matrix to only contain trips with travel time lower than
  # 120 minutes and hexagons with population/opportunities data.
  # also, temporarily filtering by the cost as well, but not sure if that makes
  # sense in the long run
  
  uber_matrix <- readRDS(uber_matrix_path)
  uber_matrix <- uber_matrix[
    travel_time < 120 &
      monetary_cost < 30 &
      from_id %chin% points$id &
      to_id %chin% points$id
  ]
  
  first_mile_matrix <- uber_matrix[stations_to_hex, on = c(to_id = "id_hex")]
  setcolorder(
    first_mile_matrix,
    c("from_id", "to_id", "id", "travel_time", "monetary_cost")
  )
  setnames(first_mile_matrix, old = "id", new = "to_station")
  
  # calculate the pareto frontier from the rapid transit stations to all
  # possible destinations. the departure time depends on the time the uber would
  # arrive at the station, so for each unique uber trip length we calculate one
  # frontier.
  # to calculate the frontier, we have to specify the monetary cost cutoffs. we
  # pick values to use as cutoffs based on rio's fare values (for now we're
  # limiting this values to BRL 10 max).
  
  r5r_core <- setup_r5(graph_path, verbose = FALSE, use_elevation = TRUE)
  
  uber_trip_lengths <- unique(first_mile_matrix$travel_time)
  uber_trip_lengths <- uber_trip_lengths[order(uber_trip_lengths)]
  
  max_rides <- 3
  rio_fare_calculator <- read_fare_calculator(rio_fare_calculator_path)
  possible_fare_values <- generate_possible_fare_values(
    rio_fare_calculator,
    max_value = 10,
    max_rides = max_rides
  )
  
  remaining_frontier <- lapply(
    uber_trip_lengths,
    function(i) {
      cat("departure minute:", i, "\n")
      departure_datetime <- as.POSIXct(
        "08-01-2020 07:00:00",
        format = "%d-%m-%Y %H:%M:%S"
      )
      departure_datetime <- departure_datetime + 60 * i
      max_trip_duration <- 120L - i
      
      frontier <- pareto_frontier(
        r5r_core,
        origins = stations,
        destinations = points,
        mode = c("WALK", "TRANSIT"),
        departure_datetime = departure_datetime,
        time_window = 1,
        draws_per_minute = 1,
        max_trip_duration = max_trip_duration,
        max_walk_dist = 1800,
        max_rides = max_rides,
        monetary_cost_cutoffs = possible_fare_values,
        fare_calculator_settings = rio_fare_calculator,
        n_threads = getOption("N_CORES"),
        verbose = FALSE
      )
      frontier[, percentile := NULL]
      
      # we also remove entries where monetary cost is 0, because we don't want
      # to consider walking-only trips after uber first mile. it's important to
      # remove these entries afterwards, instead of simply not providing 0 as
      # a cutoff, because if we didn't provide 0 as a cutoff we could have
      # walking-only trips "hidden" in the upper limits, such as 380 or 405 (and
      # we wouldn't be able to identify them).
      
      frontier <- frontier[monetary_cost != 0]
      
      frontier
    }
  )
  
  names(remaining_frontier) <- uber_trip_lengths
  remaining_frontier <- rbindlist(
    remaining_frontier,
    idcol = "departure_minute"
  )
  remaining_frontier[, departure_minute := as.integer(departure_minute)]
  
  # bind remaining_frontier with first_mile_matrix. calculate the total travel
  # time and cost, and then keep only the pareto frontier of these new total
  # travel time and cost values
  
  frontier <- merge(
    first_mile_matrix,
    remaining_frontier,
    by.x = c("to_station", "travel_time"),
    by.y = c("from_id", "departure_minute"),
    allow.cartesian = TRUE
  )
  
  setnames(
    frontier,
    old = c(
      "to_station",
      "travel_time",
      "from_id",
      "to_id.x",
      "monetary_cost.x",
      "to_id.y",
      "travel_time.y",
      "monetary_cost.y"
    ),
    new = c(
      "intermediate_station",
      "first_mile_time",
      "from_id",
      "intermediate_hex",
      "first_mile_cost",
      "to_id",
      "remaining_time",
      "remaining_cost"
    )
  )
  setcolorder(
    frontier,
    c(
      "from_id",
      "intermediate_hex",
      "intermediate_station",
      "to_id",
      "first_mile_time",
      "remaining_time",
      "first_mile_cost",
      "remaining_cost"
    )
  )
  frontier[
    ,
    `:=`(
      travel_time = first_mile_time + remaining_time,
      monetary_cost = first_mile_cost + remaining_cost
    )
  ]
  
  cols_to_keep <- c(
    "from_id",
    "to_id",
    "intermediate_station",
    "travel_time",
    "monetary_cost"
  )
  frontier[, setdiff(names(frontier), cols_to_keep) := NULL]
  
  frontier <- keep_pareto_frontier(frontier)
  
  parent_dir <- "../data/data/pfrontiers"
  if (!dir.exists(parent_dir)) dir.create(parent_dir)
  
  frontier_dir <- file.path(parent_dir, "absolute")
  if (!dir.exists(frontier_dir)) dir.create(frontier_dir)
  
  frontier_path <- file.path(frontier_dir, "uber_first_mile.rds")
  saveRDS(frontier, frontier_path)
  
  return(frontier_path)
}


generate_possible_fare_values <- function(rio_fare_calculator,
                                          max_value,
                                          max_rides) {
  values <- c(
    0,
    rio_fare_calculator$fares_per_mode$fare,
    rio_fare_calculator$fares_per_transfer$fare
  )
  values <- unique(values)
  values <- lapply(
    1:max_rides,
    function(m) {
      list_of_values <- rep(list(values), m)
      combinations <- do.call(expand.grid, list_of_values)
      combinations <- rowSums(combinations)
    }
  )
  values <- unlist(values)
  values <- unique(values)
  values <- values[values <= max_value]
  values <- values[order(values)]
  
  return(values)
}


# uber_frontier_path <- tar_read(uber_first_mile_pareto_frontier)
# transit_frontier_path <- tar_read(transit_pareto_frontier)
join_uber_fm_transit_frontiers <- function(uber_frontier_path,
                                           transit_frontier_path) {
  transit_frontier <- readRDS(transit_frontier_path)
  uber_frontier <- readRDS(uber_frontier_path)
  
  frontier <- rbind(uber_frontier, transit_frontier, fill = TRUE)
  frontier <- keep_pareto_frontier(frontier)
  
  parent_dir <- "../data/data/pfrontiers"
  if (!dir.exists(parent_dir)) dir.create(parent_dir)
  
  frontier_dir <- file.path(parent_dir, "absolute")
  if (!dir.exists(frontier_dir)) dir.create(frontier_dir)
  
  frontier_path <- file.path(frontier_dir, "uber_fm_transit_combined.rds")
  saveRDS(frontier, frontier_path)
  
  return(frontier_path)
}


# only_uber_path <- tar_read(full_uber_matrix)
# only_transit_path <- tar_read(transit_pareto_frontier)
# uber_transit_comb_path <- tar_read(uber_fm_transit_combined_frontier)
# grid_path <- tar_read(grid_res_8)
# path <- tar_read(full_uber_matrix)
calculate_affordability <- function(only_uber_path,
                                    only_transit_path,
                                    uber_transit_comb_path,
                                    grid_path) {
  paths <- c(only_uber_path, only_transit_path, uber_transit_comb_path)
  grid <- setDT(readRDS(grid_path))
  
  frontier_paths <- vapply(
    paths,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    FUN = function(path) {
      frontier <- readRDS(path)
      
      frontier[
        grid,
        on = c(from_id = "id_hex"),
        origin_income_per_capita := i.renda_capita
      ]
      
      # FIXME: some hexagons have infinite income per capita because they have
      # income, but no population. that's possibly a problem with the data
      # aggregation process. for now, setting their income per capita as the
      # otherwise max value in the grid
      
      max_non_infinite <- max(
        frontier[is.finite(origin_income_per_capita)]$origin_income_per_capita
      )
      frontier[
        is.infinite(origin_income_per_capita),
        origin_income_per_capita := max_non_infinite
      ]
      
      # we use the relative monthly cost of a trip as the affordability measure.
      # this measure estimates how much of an individual's monthly income would
      # be spent on transport if she/he used that trip every business day to
      # commute from/to work. assuming 22 working days per month, that would be
      # 44 trips per month
      
      frontier[
        ,
        relative_monthly_cost := monetary_cost * 44 / origin_income_per_capita
      ]
      frontier[, origin_income_per_capita := NULL]
      
      frontier_dir <- "../data/data/pfrontiers/affordability"
      if (!dir.exists(frontier_dir)) dir.create(frontier_dir)
      
      frontier_basename <- basename(path)
      frontier_path <- file.path(frontier_dir, frontier_basename)
      saveRDS(frontier, frontier_path)
      
      return(frontier_path)
    }
  )
  
  return(frontier_paths)
}


# f <- copy(frontier)
keep_pareto_frontier <- function(f) {
  # for each OD-pair, keep only the fastest trip for each value of cost
  
  f <- f[
    f[
      ,
      .I[travel_time == min(travel_time)],
      by = .(from_id, to_id, monetary_cost)
    ]$V1
  ]
  f <- f[f[, .I[1], by = .(from_id, to_id, monetary_cost)]$V1]
  
  # that filters out a lot of entries already, but it doesn't solve the problem.
  # to keep only the entries that dominate all the others, we look at each entry
  # individually to check if it's dominated by previously seen entries.
  # we can save some time here and do this only to OD pairs that have more than
  # 1 entry (afterall, the ones with only one entry will be kept intact)
  
  index_appearing_once <- f[, .I[.N == 1], by = .(from_id, to_id)]$V1
  
  appear_once <- f[index_appearing_once]
  appear_many_times <- f[!index_appearing_once]
  
  appear_many_times <- appear_many_times[order(from_id, to_id, monetary_cost)]
  appear_many_times <- appear_many_times[
    ,
    .(data = list(.SD)),
    by = .(from_id, to_id)
  ]
  
  appear_many_times[, relevant_entries := lapply(data, find_relevant_entries)]
  
  to_keep <- unlist(appear_many_times$relevant_entries)
  appear_many_times <- appear_many_times[
    ,
    unlist(data, recursive = FALSE),
    by = .(from_id, to_id)
  ]
  appear_many_times <- appear_many_times[to_keep]
  
  frontier <- rbind(appear_once, appear_many_times)
  
  return(frontier)
}


# appears_many_times[, nrows := vapply(data, nrow, integer(1))]
# options <- appears_many_times[appears_many_times[, .I[nrows == max(nrows)]]]
# options <- options$data[[1]]
# appears_many_times[, nrows := NULL]
find_relevant_entries <- function(options) {
  is_relevant <- vector("logical", length = nrow(options))
  
  # the first entry is always relevant, as it's the cheapest.
  # for all other, we have to check if its travel time is lower than the last
  # relevant entry's travel time. if it is, it's relevant. else, it's not
  # (logical vectors values' are FALSE by default)
  
  is_relevant[1] <- TRUE
  last_relevant_travel_time <- options$travel_time[1]
  
  for (i in seq(2, nrow(options))) {
    if (options$travel_time[i] < last_relevant_travel_time) {
      is_relevant[i] <- TRUE
      last_relevant_travel_time <- options$travel_time[i]
    }
  }
  
  return(is_relevant)
}