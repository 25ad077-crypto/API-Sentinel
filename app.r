library(shiny)
library(httr2)
library(DBI)
library(RSQLite)
library(ggplot2)

# Persistent local database
db <- dbConnect(SQLite(), "api_sentinel.db")

dbExecute(db, "
  CREATE TABLE IF NOT EXISTS checks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    endpoint TEXT,
    checked_at TEXT,
    status TEXT,
    http_code INTEGER,
    latency_ms REAL,
    error TEXT
  )
")

# Check one endpoint with a timeout and limited retry
check_api <- function(url) {
  start <- Sys.time()
  
  tryCatch({
    response <- request(url) |>
      req_timeout(5) |>
      req_retry(max_tries = 2) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    
    latency <- as.numeric(
      difftime(Sys.time(), start, units = "secs")
    ) * 1000
    
    code <- resp_status(response)
    
    status <- if (code >= 500 || code == 429) {
      "Failed"
    } else if (code >= 400) {
      "HTTP Error"
    } else if (latency > 1000) {
      "Slow"
    } else {
      "Healthy"
    }
    
    list(
      status = status,
      code = code,
      latency = round(latency, 2),
      error = if (code >= 400) paste("HTTP", code) else ""
    )
  }, error = function(e) {
    list(
      status = "Failed",
      code = NA_integer_,
      latency = NA_real_,
      error = conditionMessage(e)
    )
  })
}

ui <- fluidPage(
  titlePanel("API Sentinel — API Reliability Monitor"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(
        "endpoints",
        "API endpoints (one URL per line)",
        value = paste(
          "https://httpbin.org/status/200",
          "https://httpbin.org/delay/2",
          "https://httpbin.org/status/500",
          sep = "\n"
        ),
        width = "100%"
      ),
      
      actionButton("check", "Check APIs"),
      br(), br(),
      downloadButton("download", "Download CSV Report")
    ),
    
    mainPanel(
      h3("Endpoint Health"),
      tableOutput("health"),
      h3("Response-Time History"),
      plotOutput("history"),
      h3("Reliability Summary"),
      verbatimTextOutput("summary"),
      h3("Recent Error Logs"),
      tableOutput("errors")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$check, {
    urls <- trimws(unlist(strsplit(input$endpoints, "\n")))
    urls <- unique(urls[nzchar(urls)])
    
    valid <- grepl("^https?://", urls, ignore.case = TRUE)
    
    if (!length(urls) || any(!valid)) {
      showNotification(
        "Enter valid HTTP or HTTPS URLs, one per line.",
        type = "error"
      )
      return()
    }
    
    # Prevent accidental excessive monitoring
    if (length(urls) > 10) {
      showNotification(
        "Please check no more than 10 endpoints at once.",
        type = "error"
      )
      return()
    }
    
    results <- lapply(urls, function(url) {
      result <- check_api(url)
      
      record <- data.frame(
        endpoint = url,
        checked_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        status = result$status,
        http_code = result$code,
        latency_ms = result$latency,
        error = result$error
      )
      
      dbWriteTable(
        db, "checks", record,
        append = TRUE, row.names = FALSE
      )
      
      cbind(record, stringsAsFactors = FALSE)
    })
    
    showNotification("API checks completed.", type = "message")
  })
  
  get_history <- reactive({
    dbReadTable(db, "checks")
  })
  
  output$health <- renderTable({
    x <- get_history()
    if (!nrow(x)) return(data.frame(Message = "Click Check APIs to begin."))
    
    x <- x[order(x$id, decreasing = TRUE), ]
    x <- x[!duplicated(x$endpoint), ]
    
    data.frame(
      Endpoint = x$endpoint,
      Status = x$status,
      `HTTP Code` = x$http_code,
      `Latency (ms)` = x$latency_ms,
      check.names = FALSE
    )
  })
  
  output$history <- renderPlot({
    x <- get_history()
    validate(need(nrow(x) > 0, "Run a check to display history."))
    
    x <- x[!is.na(x$latency_ms), ]
    
    validate(need(nrow(x) > 0, "No successful timing measurements yet."))
    
    ggplot(
      x,
      aes(
        x = as.POSIXct(checked_at),
        y = latency_ms,
        color = endpoint
      )
    ) +
      geom_line() +
      geom_point() +
      labs(x = "Check time", y = "Response time (ms)", color = "Endpoint") +
      theme_minimal()
  })
  
  output$summary <- renderPrint({
    x <- get_history()
    if (!nrow(x)) {
      cat("No checks recorded yet.")
      return()
    }
    
    measured <- x$latency_ms[!is.na(x$latency_ms)]
    cat("Total checks:", nrow(x), "\n")
    cat("Healthy:", sum(x$status == "Healthy"), "\n")
    cat("Slow:", sum(x$status == "Slow"), "\n")
    cat("Failed:", sum(x$status == "Failed"), "\n")
    cat("HTTP errors:", sum(x$status == "HTTP Error"), "\n")
    
    if (length(measured)) {
      cat("Average latency:", round(mean(measured), 2), "ms\n")
      cat("p95 latency:", round(
        unname(quantile(measured, 0.95)), 2
      ), "ms\n")
    }
  })
  
  output$errors <- renderTable({
    x <- get_history()
    x <- x[x$status != "Healthy", ]
    
    if (!nrow(x)) {
      return(data.frame(Message = "No errors recorded."))
    }
    
    head(x[order(x$id, decreasing = TRUE),
           c("endpoint", "checked_at", "status",
             "http_code", "error")], 20)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("api-sentinel-report-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(get_history(), file, row.names = FALSE)
    }
  )
  
  session$onSessionEnded(function() {
    # Keep database connection open for app lifetime
  })
}

shinyApp(ui, server)