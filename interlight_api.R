library(stringr)
library(httr2)
library(jsonlite)
library(dotenv)

# Create GET request with query params
create_get_request <- function(base_url, query_params) {
  request(base_url) |>
    req_headers("Accept" = "application/json") |>
    req_url_query(!!!query_params)
}

# Create POST request with JSON body
create_post_request <- function(base_url, body_list) {
  request(base_url) |>
    req_headers(
      "Accept" = "application/json",
      "Content-Type" = "application/json; charset=utf-8"
    ) |>
    req_body_json(body_list)
}

# Download and parse JSON response
fetch_json <- function(req) {
  req |>
    req_perform() |>
    resp_body_json()
}

# Save JSON to file
save_json_file <- function(data, file_path) {
  json <- toJSON(data, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json, file_path)
}

download_from_api_post <- function(
  base_url,
  endpoint,
  body_params,
  output_dir = "."
) {
  # Build and execute POST request
  req <- request(base_url) |>
    req_url_path_append(endpoint) |>
    req_headers(
      "Accept" = "application/json",
      "Content-Type" = "application/json; charset=utf-8"
    ) |>
    req_body_json(body_params)

  # Execute and parse response
  data <- fetch_json(req)

  # Extract station name from the JSON response
  station_name <- data$data[[1]]$nombre

  # Extract date components from body_params
  date_str <- as.character(body_params$diaMuestra)
  date_parts <- str_split_1(date_str, "/")
  year_month <- str_c(date_parts[3], "-", date_parts[2])
  year_month_day <- str_c(date_parts[3], "-", date_parts[2], "-", date_parts[1])

  # Build directory path: {output_dir}/stations/{station_name}/year-month
  output_path <- file.path(
    output_dir,
    "stations",
    station_name,
    year_month
  )

  # Create directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Generate filename: year-month-day.json
  file_name <- str_c(year_month_day, ".json")
  file_path <- file.path(output_path, file_name)

  save_json_file(data, file_path)

  file_path
}

download_data <- function(
  date,
  output_dir = "."
) {
  download_from_api_post(
    base_url = Sys.getenv("BASE_URL"),
    endpoint = Sys.getenv("ENDPOINT"),
    body_params = list(
      usuario = Sys.getenv("USER"),
      password = Sys.getenv("PASSWORD"),
      diaMuestra = date
    ),
    output_dir = output_dir
  )
}

# Calculate yesterday's date in DD/MM/YYYY format
yesterday_date <- format(Sys.Date() - 1, "%d/%m/%Y")

# Execute download function with the calculated date
# download_data(yesterday_date)

download_data("04/02/2026")
download_data("05/02/2026")
