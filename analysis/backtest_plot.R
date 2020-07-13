library(seer)

d1_dax_2019 <- read_ohlcv("~/datasets/jcr2020/datasets/d1_dax_2019.csv")
back_macd <- backtest.opt("d1_dax_2019", strategy = macd(fees=0, order_size = 1))
back_tema <- backtest.opt("d1_dax_2019", strategy = triple_crossover(1, 18, 72, fees = 0, order_size = 1))

rec_macd <- recreate_strategy(back_macd, fees = 0, order_size = 1)
rec_tema <- recreate_strategy(back_tema, fees = 0, order_size = 1)

plot_backtest_interactive(rec_macd, d1_dax_2019, back_macd, range = "2019-05/2019-11")

plot_backtest_interactive(rec_tema, d1_dax_2019, back_tema)
