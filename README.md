# API Sentinel — API Reliability Monitor

API Sentinel is an R Shiny web application that monitors API endpoint health, measures response times, records HTTP errors, and tracks reliability metrics.

## Features

- API health status monitoring
- HTTP status and timeout error logging
- Response-time history visualization
- Average and p95 latency metrics
- SQLite database for monitoring history
- CSV report download
- Configurable API endpoints
- Request timeouts and limited retries

## Technologies Used

- R Programming
- RStudio
- Shiny
- httr2
- SQLite
- ggplot2

## Installation

Install the required packages in R:

```r
install.packages(c(
  "shiny", "httr2", "DBI", "RSQLite", "ggplot2"
))
```

## How to Run

1. Clone or download this repository.
2. Open `app.R` in RStudio.
3. Run the application using:

```r
shiny::runApp()
```

4. Enter API endpoint URLs and click **Check APIs**.
5. Review health status, latency history, error logs, and exportable reports.

## Project Goal

To demonstrate API reliability monitoring, response-time analysis, error tracking, and persistent storage using R.

## Disclaimer

This is an educational monitoring tool. Results depend on network conditions and the availability of the selected endpoints.
