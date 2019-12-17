####
# Optimize MACD params
# nFast and nSlow
###

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    message("Usage: Rscript opt_foreach.R [DIR] [start] [end]")
    stop("Missing args")
  }
  base_dir <- path.expand(args[1])
  start <- as.numeric(args[2])
  end <- as.numeric(args[3])
  
  if (!dir.exists(base_dir)) {
    message(paste("No existe el directorio", base_dir))
    stop()
  }
} else {
  base_dir <- "~/Datasets/datasets_paper"
  start <- end <- NA
}

paramset <- list(
  .nFast = 1:20,
  .nSlow = 15:60
)


library(seer)

out_dir <- file.path("./results_paper/backtest_opt/macd")
if (!dir.exists(out_dir))
  dir.create(out_dir, recursive = TRUE)

log_file <- file("macd-opt-errors.log", open = "wt")

optimize <- function(filename, outputdir) {
  name <- sub(".csv", "", basename(filename))
  print(paste("Optimizing for ", name))
  print(paste("Saving in ", outputdir))
  assign(name, read_ohlcv(filename), envir = .GlobalEnv)
  
  for (nFast in paramset$.nFast) {
    for (nSlow in paramset$.nSlow) {
      if (nFast < nSlow) {
        params <- paste0(c("macd", nFast, nSlow), collapse = "_")
        rdata.name <- file.path(outputdir, paste0(name, params, ".RData"))
        if (file.exists(rdata.name)) {
          print(paste(rdata.name, "already exists. Skipping"))
          next
        }
        
        print(paste("Testing ", name, params))
        a <- Sys.time()
        try(
          invisible(capture.output(results <- backtest.opt(
            symbols = name,
            strategy = macdhist(fastMA = nFast, slowMA = nSlow)
          ))),
          outFile = log_file
        )
        b <- Sys.time()
        dt <- difftime(b, a, units = "secs")
        message(paste("Finished in ", dt, "secs"))
        
        try(save(results, file = rdata.name), outFile = log_file)
      }
    }
  }
  
  rm(list = name, envir = .GlobalEnv)
}

accepted <- "ES35|GERMAN30|US30|EURGBP|EURUSD|EURJPY|EURJPY|GBPUSD"
files <- list.files(base_dir, pattern = accepted, full.names = TRUE, recursive = TRUE)

if (!is.na(start) & !is.na(end)) {
  files <- files[start:min(length(files), end)]
}

for (file in files) {
  optimize(file, out_dir)
}

close(log_file)