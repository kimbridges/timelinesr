#' Clean Timeline Raw Signals (Plan A)
#'
#' Extracts high-precision GPS points ("Raw Signals") from the Google Timeline JSON.
#' These are the distinct blue dots on a map, useful for linear interpolation.
#'
#' @param raw_json_data The list object returned by jsonlite::fromJSON().
#' @return A tibble with columns: raw_timestamp, lat, lon, accuracy, altitude, timestamp_utc.
#' @export
clean_timeline_signals <- function(raw_json_data) {
  if (!"rawSignals" %in% names(raw_json_data)) return(NULL)

  as_tibble(raw_json_data$rawSignals) |>
    jsonlite::flatten() |>
    filter(!is.na(position.LatLng)) |>
    select(
      raw_timestamp = position.timestamp,
      latlng_string = position.LatLng,
      accuracy      = position.accuracyMeters,
      altitude      = position.altitudeMeters
    ) |>
    mutate(latlng_clean = str_remove_all(latlng_string, "[° ]")) |>
    separate(latlng_clean, into = c("lat", "lon"), sep = ",", convert = TRUE) |>
    mutate(timestamp_utc = ymd_hms(raw_timestamp)) |>
    arrange(timestamp_utc)
}

#' Clean Semantic History (Plan B)
#'
#' Extracts "Place Visits" from the Google Timeline JSON.
#' robustly handles missing place names or addresses.
#'
#' @param raw_json_data The list object returned by jsonlite::fromJSON().
#' @return A tibble with columns: start_utc, end_utc, lat, lon, place_name, place_address.
#' @export
clean_semantic_history <- function(raw_json_data) {
  if (!"semanticSegments" %in% names(raw_json_data)) return(NULL)

  sem_df <- as_tibble(raw_json_data$semanticSegments)
  if (!"visit" %in% names(sem_df)) return(NULL)

  # Extract the nested "topCandidate" dataframe
  candidates <- sem_df$visit$topCandidate

  # --- SAFETY BLOCK START ---
  # Handle cases where placeLocation or its sub-columns are missing entirely

  # 1. Get the 'placeLocation' data frame safely
  if ("placeLocation" %in% names(candidates)) {
    loc_data <- candidates$placeLocation
  } else {
    # Create a dummy DF if missing
    loc_data <- tibble(latLng = rep(NA, nrow(candidates)))
  }

  # 2. Extract columns safely (fill with NA if missing)
  # We check if the column exists; if not, return NA
  safe_lat <- if("latLng" %in% names(loc_data)) loc_data$latLng else NA
  safe_name <- if("name" %in% names(loc_data)) loc_data$name else NA
  safe_addr <- if("address" %in% names(loc_data)) loc_data$address else NA
  # --- SAFETY BLOCK END ---

  sem_df |>
    mutate(
      place_lat     = safe_lat,
      place_name    = safe_name,
      place_address = safe_addr
    ) |>
    filter(!is.na(place_lat)) |>
    mutate(latlng_clean = str_remove_all(place_lat, "[° ]")) |>
    separate(latlng_clean, into = c("lat", "lon"), sep = ",", convert = TRUE) |>
    mutate(
      start_utc = ymd_hms(startTime),
      end_utc   = ymd_hms(endTime),
      duration_m = as.numeric(difftime(end_utc, start_utc, units = "mins"))
    ) |>
    select(start_utc, end_utc, duration_m, lat, lon, place_name, place_address) |>
    arrange(start_utc)
}

#' Get Photo Timestamps
#'
#' Scans a directory for image files and extracts their "DateTimeOriginal" tags.
#'
#' @param folder_path String. Path to the folder containing images.
#' @param tz_offset String. Timezone of the camera clock (e.g., "Pacific/Honolulu").
#' @return A tibble with columns: SourceFile, local_time, timestamp_utc.
#' @export
get_photo_timestamps <- function(folder_path, tz_offset = "Pacific/Honolulu") {
  files <- list.files(folder_path, pattern = "\\.(jpg|jpeg|arw|tif|tiff)$",
                      ignore.case = TRUE, full.names = TRUE)

  message("Scanning ", length(files), " images...")
  read_exif(files, tags = c("SourceFile", "DateTimeOriginal")) |>
    filter(!is.na(DateTimeOriginal)) |>
    mutate(
      local_time = ymd_hms(DateTimeOriginal, tz = tz_offset),
      timestamp_utc = with_tz(local_time, tzone = "UTC")
    ) |>
    select(SourceFile, local_time, timestamp_utc)
}

#' Clean Raw Location History (Plan C)
#'
#' Extracts raw GPS pings from the 'LatLng' string column.
#' Adapted for your specific JSON format where lat/lon are combined.
#'
#' @param raw_json_data The list object returned by jsonlite::fromJSON().
#' @return A tibble with columns: timestamp, lat, lon, accuracy_m.
#' @export
clean_raw_history <- function(raw_json_data) {
  # 1. Check existence
  if (!"rawSignals" %in% names(raw_json_data)) return(NULL)

  raw_df <- as_tibble(raw_json_data$rawSignals)

  # 2. Extract the 'position' dataframe
  if (!"position" %in% names(raw_df) || !is.data.frame(raw_df$position)) {
    return(NULL)
  }
  pos_df <- raw_df$position

  # 3. Check for the specific "LatLng" column we found
  if (!"LatLng" %in% names(pos_df)) {
    return(NULL)
  }

  # 4. Process and Split
  pos_df |>
    transmute(
      # Parse the timestamp inside the position df
      timestamp = ymd_hms(timestamp),

      # Clean the string: remove '°' and spaces, leaving "21.3,-157.8"
      clean_str = str_remove_all(LatLng, "[° ]"),

      accuracy_m = if("accuracyMeters" %in% names(pos_df)) as.numeric(accuracyMeters) else NA
    ) |>
    # Split into two numeric columns
    separate(clean_str, into = c("lat", "lon"), sep = ",", convert = TRUE) |>
    filter(!is.na(lat), !is.na(lon)) |>
    select(timestamp, lat, lon, accuracy_m) |>
    arrange(timestamp)
}
