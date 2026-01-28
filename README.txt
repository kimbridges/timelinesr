Package Name: timelinesr Version: 0.1.0 Purpose: Retroactively adds GPS data to Sony cameras using Google Timeline history.

The Workflow:

Ingest: read_timeline() pulls the messy JSON from your phone.

Logic:

Plan A: Interpolates high-precision "Raw Signals" (Blue dots).

Plan B: Falls back to "Semantic Visits" (Hotels/Parks) if GPS was off (Orange dots).

Action: geotag_photos() matches the times.

Write: write_geotags() burns the coordinates into the .ARW and .JPG files using ExifTool.

HOW TO RUN IT

library(timelinesr)

# 1. Load your history
timeline <- read_timeline("Timelines/History.json")

# 2. Tag your folder
results  <- geotag_photos("Photos/Trip_Name", timeline, tz = "Pacific/Honolulu")
write_geotags(results, "Photos/Trip_Name")