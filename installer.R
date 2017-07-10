create_app(
  app_name    = "slimDashboard", 
  app_dir     = "D:/Nitin/ROOT/Buurkracht/Dashboard/Git/slimDashboard",
  dir_out     = "wizard",
  pkgs        = c("jsonlite","shiny","scales","ggplot2","data.table","bit64",
                  "dygraphs","xts", "shinythemes","forecast","ggfortify","depmixS4"),  # CRAN-like repo packages
  remotes     = c("talgalili/installr", "daattali/shinyjs"), # GitHub packages
  include_R   = TRUE,   # Download R and install it with your app, if necessary
  default_dir = "pf")   # Install app in to Program Files

compile_iss()
