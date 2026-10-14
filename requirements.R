# API Sentinel - Package Setup

packages <- c(
  "shiny",
  "httr2",
  "DBI",
  "RSQLite",
  "ggplot2"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}
