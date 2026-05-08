source("interlight_api.R")

# Load stations configuration
stations <- read.csv("stations_config.csv", stringsAsFactors = FALSE)

# Filter stations that have an api_url defined
stations_to_process <- stations[!is.na(stations$api_url) & stations$api_url != "", ]

# Calculate yesterday's date in DD/MM/YYYY format
yesterday_date <- format(Sys.Date() - 1, "%d/%m/%Y")

# Process each station
if (nrow(stations_to_process) > 0) {
  for (i in 1:nrow(stations_to_process)) {
    station <- stations_to_process[i, ]
    message(paste("Processing station:", station$id))
    
    # Download data for yesterday
    tryCatch({
      download_data(yesterday_date, station$id, station$api_url)
    }, error = function(e) {
      warning(paste("Failed to download data for", station$id, ":", e$message))
    })
  }
} else {
  message("No stations with api_url found in stations_config.csv")
}
