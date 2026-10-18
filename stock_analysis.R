
library(quantmod)

load_stock_data <- function(file = "portfolio.txt") {
  symbols <- readLines(file)
  stock_data <- list()
  
  for (symbol in symbols) {
    data <- getSymbols(symbol, src = "yahoo",
                       auto.assign = FALSE)
    stock_data[[symbol]] <- as.data.frame(data)
  }
  
  return(stock_data)
}
stock_data <- load_stock_data()


calculate_statistics <- function(data) {
  prices <- na.omit(data[, 4])
  
  moving_average <- mean(tail(prices, 5))
  mean_value <- mean(prices)
  median_value <- median(prices)
  mode_value <- as.numeric(names(sort(table(prices), decreasing = TRUE)[1]))
  sd_value <- sd(prices)
  
  return(data.frame(
    Moving_Average = as.numeric(moving_average),
    Mean = mean_value,
    Mode = mode_value,
    Median = median_value,
    Standard_Deviation = sd_value
  ))
}

stock_statistics <- lapply(stock_data, calculate_statistics)

display_stock_data <- function(stock_data) {
  for (symbol in names(stock_data)) {
    cat("\nStock:", symbol, "\n")
    print(head(stock_data[[symbol]]))
  }
}

for (symbol in names(stock_statistics)) {
  cat("\nStatistics for:", symbol, "\n")
  print(stock_statistics[[symbol]])
}

chartSeries(stock_data[["AAPL"]], name = "AAPL Stock Price")

chartSeries(stock_data[["MSFT"]], name = "MSFT Stock Price")

chartSeries(stock_data[["GOOGL"]], name = "GOOGL Stock Price")

chartSeries(stock_data[["AMZN"]], name = "AMZN Stock Price")

chartSeries(stock_data[["TSLA"]], name = "TSLA Stock Price")
