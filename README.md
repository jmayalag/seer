# Seer

Seer is a package and collection of scripts to predict the direction of prices in the Forex market.

## Installation
```r
devtools::install_github("jmayalag/seer")
```

## Scripts
Analysis scripts are placed in the `analysis` directory.

- `run_experiment.R`: Trains multiple machine learning models
- `learning`: Defines the machine learning helper functions
- `run_backtest.R`: Runs the experiments using backtest
- `run_combined_experiment.R`: Backtests the hybrid strategies (Technical analysis + ML)
