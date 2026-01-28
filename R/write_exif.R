#' Create GPS Manifest
#'
#' Generates a CSV file formatted for ExifTool. Handles Windows path normalization
#' and creates explicit N/S/E/W reference tags.
#'
#' @param fusion_df The dataframe returned by fuse_data().
#' @param csv_path String. The filename for the manifest (default: "gps_update.csv").
#' @return The path to the created CSV file.
#' @export
write_gps_manifest <- function(fusion_df, csv_path = "gps_update.csv") {

  manifest <- fusion_df |>
    filter(!is.na(final_lat)) |>
    transmute(
      # FIX: Force Forward Slashes.
      # Windows backslashes (\) in CSVs often break ExifTool.
      SourceFile = gsub("\\\\", "/", normalizePath(SourceFile, mustWork = FALSE)),

      GPSLatitude  = final_lat,
      GPSLongitude = final_lon,
      # Explicit Reference tags prevent the "East/West" confusion
      GPSLatitudeRef = ifelse(final_lat >= 0, "N", "S"),
      GPSLongitudeRef = ifelse(final_lon >= 0, "E", "W"),
      UserComment  = paste("Geotagged via timelinesr:", method)
    )

  write.csv(manifest, csv_path, row.names = FALSE, quote = FALSE)
  message("Manifest saved: ", csv_path)
  return(csv_path)
}

#' Apply GPS to Images
#'
#' Wrapper for the ExifTool system command.
#' Requires 'exiftool' to be in the system PATH.
#'
#' @param csv_path Path to the CSV manifest created by write_gps_manifest().
#' @param photo_dir Path to the directory containing images.
#' @return NULL. Prints output to console.
#' @export
apply_gps_to_images <- function(csv_path, photo_dir) {

  # Assumes 'exiftool' is in your Windows PATH
  tool_command <- "exiftool"

  # Build the command structure
  # -v0 : verbose level 0 (shows "1 image updated")
  # -overwrite_original : saves space
  cmd <- paste0(tool_command, ' -csv="', csv_path, '" -overwrite_original -v0 "', photo_dir, '"')

  message("Executing Command: ", cmd)

  # Run it and capture output
  output <- system(cmd, intern = TRUE)

  message("--- ExifTool Output ---")
  print(output)
  message("-----------------------")
}
