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
  
  parent_dir <- "../data/data/pfrontiers"
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
  pickup_data[, parent_hex := parent_res_8]
  
  # pickup data includes a few NAs waiting times. we substitute these by the
  # weighted average value
  # OBS: ISSO É ALGO ERRADO JÁ DO DADO QUE NOS FOI DADO
  
  avg_wait_time <- weighted.mean(
    pickup_data[!is.na(mean_wait_time)]$mean_wait_time,
    pickup_data[!is.na(mean_wait_time)]$num_pickups
  )
  pickup_data[is.na(mean_wait_time), mean_wait_time := avg_wait_time]
  
  aggregated_data <- pickup_data[
    ,
    .(
      num_pickups = sum(num_pickups),
      frac_driver_cancel = weighted.mean(frac_driver_cancel, w = num_pickups),
      mean_wait_time = weighted.mean(mean_wait_time, w = num_pickups)
    ),
    by = parent_hex
  ]
  setnames(aggregated_data, old = "parent_hex", new = "hex_addr")
  
  # keeping only hexagons inside the city of rio
  
  grid <- readRDS(grid_path)
  aggregated_data <- aggregated_data[hex_addr %chin% grid$id_hex]
  
  data_dir <- "../data/data/"
  path <- file.path(data_dir, "pickup_res_8.rds")
  saveRDS(aggregated_data, path)
  
  return(path)
}


# uber_data_path <- tar_read(uber_data)
# pickup_data_path <- tar_read(pickup_data_res_8)
# grid_path <- tar_read(grid_res_8)
# car_od_matrix_path <- tar_read(car_od_matrix)
# points_path <- tar_read(r5_points)
fill_uber_matrix <- function(uber_data_path,
                             pickup_data_path,
                             grid_path,
                             car_od_matrix_path,
                             points_path) {
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
  
  # keeping only hexagons inside rio city to fill the matrix based only on the
  # trips that happened inside it (since the other, that happened outside the
  # city, may have very different costs and follow different traffic patterns)
  
  grid <- setDT(readRDS(grid_path))
  
  uber_data <- uber_data[
    from %chin% grid$id_hex & to %chin% grid$id_hex
  ]
  
  # to fill the distance and time values, we find the correlation between uber's
  # values with the car values and calculate estimated values based on these.
  # to fill the cost values, we find the correlation between cost, time and
  # distance in the uber data and then use these values to estimate the cost
  # based on the new estimated time and distance values.
  # we remove the trips from hexagons to themselves from the first two
  # regressions because they seem to be a bit problematic on uber's end. in
  # those cases, we populate the values with the avg time and distance from hexs
  # to themselves.
  
  car_od_matrix <- fread(car_od_matrix_path)
  
  uber_data[
    car_od_matrix,
    on = c(from = "origin_hex", to = "destination_hex"),
    `:=`(
      car_time = i.Total_Time,
      car_distance = i.Total_Distance
    )
  ]
  
  time_lm <- lm(
    travel_time ~ car_time,
    uber_data[from != to & !(is.na(car_time))]
  )
  
  distance_lm <- lm(
    distance ~ car_distance,
    uber_data[from != to & !(is.na(car_distance))]
  )
  
  cost_lm <- lm(cost ~ distance + travel_time, uber_data)
  
  hex_to_itself <- uber_data[from == to]
  avg_time_to_itself <- weighted.mean(
    hex_to_itself$travel_time,
    hex_to_itself$num_trips
  )
  avg_dist_to_itself <- weighted.mean(
    hex_to_itself$distance,
    hex_to_itself$num_trips
  )
  
  # create a full matrix using the points sent to r5r. then first fill it using
  # the actual values found in uber data, and then estimate new ones using the
  # objects above
  
  points <- fread(points_path)
  full_matrix <- CJ(
    from = points$id,
    to = points$id
  )
  
  full_matrix[
    uber_data,
    on = c("from", "to"),
    `:=`(
      cost = i.cost,
      distance = i.distance,
      travel_time = i.travel_time
    )
  ]
  full_matrix[, manually_inputed := fifelse(is.na(cost), TRUE, FALSE)]
  
  full_matrix[
    from == to & is.na(cost),
    `:=`(
      travel_time = avg_time_to_itself,
      distance = avg_dist_to_itself
    )
  ]
  
  # we have to bring the values from the car od matrix to the full matrix in
  # order to estimate the uber values. but some pairs do not appear on the car
  # od matrix, which introduces some NAs to the matrix. we fill these NAs with
  # the average values found in the data, when filtered to keep the routed
  # points
  
  filtered_car_matrix <- car_od_matrix[
    origin_hex %chin% points$id & destination_hex %chin% points$id
  ]
  
  avg_car_time <- mean(filtered_car_matrix$Total_Time)
  avg_car_dist <- mean(filtered_car_matrix$Total_Distance)
  
  full_matrix[
    car_od_matrix,
    on = c(from = "origin_hex", to = "destination_hex"),
    `:=`(
      car_time = i.Total_Time,
      car_distance = i.Total_Distance
    )
  ]
  full_matrix[is.na(car_time), car_time := avg_car_time]
  full_matrix[is.na(car_distance), car_distance := avg_car_dist]
  
  full_matrix[
    is.na(distance),
    distance := distance_lm$coefficients["(Intercept)"] +
      distance_lm$coefficients["car_distance"] * car_distance
  ]
  full_matrix[
    is.na(travel_time),
    travel_time := time_lm$coefficients["(Intercept)"] +
      time_lm$coefficients["car_time"] * car_time
  ]
  
  # estimating time and distance via regression introduces some negative values,
  # because of the negative intercept and low car time/distance values. so we
  # take the minimum values in the actual uber data and use them to substitute
  # these negative and very low values
  # OBS: ISSO DA VALORES MENORES DO QUE OS MÉDIOS DE HEXAGONOS ENTRE SI MESMOS
  
  min_uber_time <- min(uber_data$travel_time)
  min_uber_dist <- min(uber_data$distance)
  
  full_matrix[distance < min_uber_dist, distance := min_uber_dist]
  full_matrix[travel_time < min_uber_time, travel_time := min_uber_time]
  
  # estimating the cost is also done via a regression. this regression may
  # introduce very low costs, because of low distances and travel times, which
  # are much lower than uber's actual minimum fare. in fact, we also have some
  # low values in the actual uber data. so we substitute those by the minimum
  # fare in rio at the time, which was around 5 reais
  # OBS: QUAL VALOR USAR CERTINHO?
  
  full_matrix[
    is.na(cost),
    cost := cost_lm$coefficients["(Intercept)"] +
      distance * cost_lm$coefficients["distance"] +
      travel_time * cost_lm$coefficients["travel_time"]
  ]
  
  minimum_uber_fare <- 5
  full_matrix[cost < minimum_uber_fare, cost := minimum_uber_fare]
  
  # we have to add to the travel time the mean waiting time at each origin,
  # after converting it from seconds to minutes.
  # we do this after calculating the cost because waiting times do not
  # influence the monetary cost of a trip.
  # some hexagons do not have waiting time data, so we calculate the weighted
  # mean waiting time in the city of rio and substitute the NAs using this value
  
  pickup_data <- readRDS(pickup_data_path)
  pickup_data <- pickup_data[hex_addr %chin% grid$id_hex]
  
  full_matrix[
    pickup_data,
    on = c(from = "hex_addr"),
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
  
  # we won't need the distance between two hexagons (we needed to calculate the
  # cost), the waiting time at origin and the car-related columns anymore, so we
  # can drop them
  
  full_matrix[
    ,
    c("distance", "waiting_time", "car_time", "car_distance") := NULL
  ]
  setnames(
    full_matrix,
    old = c("cost", "from", "to"),
    new = c("monetary_cost", "from_id", "to_id")
  )
  setcolorder(full_matrix, setdiff(names(full_matrix), "manually_inputed"))
  
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
  uber_matrix[, manually_inputed := NULL]
  
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
  # Uber's travel times. first filter uber_matrix to only contain trips with
  # travel time lower than 120 minutes and hexagons with
  # population/opportunities data.
  
  uber_matrix <- readRDS(uber_matrix_path)
  uber_matrix <- uber_matrix[
    travel_time < 120 &
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
  # pick values to use as cutoffs based on rio's fare values
  
  r5r_core <- setup_r5(graph_path, verbose = FALSE, use_elevation = TRUE)
  
  uber_trip_lengths <- unique(first_mile_matrix$travel_time)
  uber_trip_lengths <- uber_trip_lengths[order(uber_trip_lengths)]
  
  max_rides <- 4
  rio_fare_calculator <- read_fare_calculator(rio_fare_calculator_path)
  possible_fare_values <- generate_possible_fare_values(
    rio_fare_calculator,
    max_value = 18,
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


# grid_path <- tar_read(grid_res_8)
adjust_grid_income <- function(grid_path) {
  # to adjust the income based on brazilian inflation, we download a data series
  # that shows how prices historically evolved when adjusted by the ipca index
  
  response <- httr::GET(
    "http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='PRECOS12_IPCA12')"
  )
  response <- httr::content(response)$value
  
  values <- rbindlist(response)
  values[, setdiff(names(values), c("VALDATA", "VALVALOR")) := NULL]
  values[, VALDATA := as.Date(VALDATA)]
  
  # the sociodemographic data in our grid object comes from the 2010 census.
  # we want to adjust it to 2019 values, when our uber was collected
  
  value_2010 <- values[VALDATA == as.Date("2010-10-01")]$VALVALOR
  value_2019 <- values[VALDATA == as.Date("2019-10-01")]$VALVALOR
  
  adjustment_rate <- value_2019 / value_2010
  
  grid <- setDT(readRDS(grid_path))
  grid[
    ,
    `:=`(
      renda_total = renda_total * adjustment_rate,
      renda_capita = renda_capita * adjustment_rate
    )
  ]
  grid[
    ,
    setdiff(names(grid), c("id_hex", "renda_total", "renda_capita")) := NULL
  ]
  
  path <- "../data/data/income_adjusted_grid.rds"
  saveRDS(grid, path)
  
  return(path)
}


# only_uber_path <- tar_read(full_uber_matrix)
# only_transit_path <- tar_read(transit_pareto_frontier)
# uber_transit_comb_path <- tar_read(uber_fm_transit_combined_frontier)
# grid_path <- tar_read(income_adjusted_grid)
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