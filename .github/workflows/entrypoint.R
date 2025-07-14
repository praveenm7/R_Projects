#!/usr/bin/env Rscript

rsconnect::setAccountInfo(name = Sys.getenv("SHINY_USERNAME"),
                          token = Sys.getenv("SHINY_TOKEN"),
                          secret = Sys.getenv("SHINY_SECRET"))

rsconnect::deployApp(appName = Sys.getenv("APP_NAME"),
                     appDir = Sys.getenv("APP_DIR"),
                     launch.browser = FALSE)
