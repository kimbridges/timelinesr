#' Read Google Timeline JSON
#'
#' High-level wrapper to ingest Google Location History data.
#' Returns both Semantic (Place) visits and Raw GPS signals.
#'
#' @param json_path Path to the JSON file (e.g., "Timelines/History.json").
#' @return A list containing two tibbles: $visits (Semantic) and $raw_locations (GPS).
#' @export
read_timeline <- function(json_path) {
  if (!file.exists(json_path)) stop("File not found: ", json_path)

  message("Reading Timeline JSON (this may take a moment)...")
  raw_json <- jsonlite::fromJSON(json_path)

  # Process Plan B (Semantic Places)
  visits_df <- clean_semantic_history(raw_json)

  # Process Plan C (Raw GPS)
  raw_df <- clean_raw_history(raw_json)

  list(
    visits = visits_df,
    raw_locations = raw_df
  )
}

#' Write Geotags to Image Files
#'
#' Uses ExifTool to write the calculated latitude and longitude back into the
#' original image files.
#'
#' @param tagged_df The dataframe returned by geotag_photos().
#' @export
write_geotags <- function(tagged_df) {

  # 1. Filter to only successful tags
  to_write <- tagged_df |>
    filter(!is.na(lat), !is.na(lon))

  if (nrow(to_write) == 0) {
    message("No geotagged photos to write.")
    return(NULL)
  }

  message("Writing EXIF tags to ", nrow(to_write), " images...")

  # 2. Iterate and Write (using exifr/exiftool)
  # We use a loop here for safety, though batch commands are possible.
  for (i in 1:nrow(to_write)) {
    row <- to_write[i, ]

    # Construct the command directly for exiftool is often safest,
    # but let's use the package wrapper if you prefer.
    # Here is a robust system command approach that works on Mac/Linux/Windows:

    cmd <- sprintf(
      'exiftool -overwrite_original -GPSLatitude="%s" -GPSLongitude="%s" -GPSLatitudeRef="%s" -GPSLongitudeRef="%s" "%s"',
      abs(row$lat), abs(row$lon),
      ifelse(row$lat >= 0, "N", "S"),
      ifelse(row$lon >= 0, "E", "W"),
      row$SourceFile
    )

    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  message("Done!")
}


#' Geotag Photos using Hybrid Track
#'
#' Interpolates coordinates for photos based on the continuous hybrid track.
#'
#' @param track_df The dataframe returned by fuse_layers().
#' @param photos_df A dataframe of photos (must have a 'timestamp' or 'timestamp_utc' column).
#' @return The photos dataframe with new 'lat' and 'lon' columns.
#' @export
geotag_photos <- function(track_df, photos_df) {

  message("Interpolating coordinates for ", nrow(photos_df), " images...")

  # 1. Standardize Column Names
  # If we have 'timestamp_utc', rename it to 'timestamp' for the math
  if ("timestamp_utc" %in% names(photos_df) && !"timestamp" %in% names(photos_df)) {
    photos_df <- photos_df |> rename(timestamp = timestamp_utc)
  }

  # 2. Validation
  if (!all(c("lat", "lon", "timestamp") %in% names(track_df))) {
    stop("Track data missing required columns. Did you run fuse_layers()?")
  }
  if (!"timestamp" %in% names(photos_df)) {
    stop("Photos data missing 'timestamp' column.")
  }

  # 3. Interpolation (The Math)
  track_time <- as.numeric(track_df$timestamp)
  photo_time <- as.numeric(photos_df$timestamp)

  # Interpolate Lat/Lon
  # rule=1: Return NA if photo is outside the track range (e.g., before the trip started)
  # rule=2: Use the closest point (good for photos taken just after the log stopped)
  # Let's use rule=1 for safety (don't fake data).

  results <- photos_df |>
    mutate(
      lat = approx(track_time, track_df$lat, photo_time, rule = 1)$y,
      lon = approx(track_time, track_df$lon, photo_time, rule = 1)$y,
      geotag_status = if_else(!is.na(lat), "geotagged", "out_of_range")
    )

  return(results)
}
