
# Libraries
library(quantmod)
library(ggplot2)
library(reshape2)

# info area
stock_name <- c("SPICEJET", "TATASTEEL", "MAGADHSUGAR", "BATAINDIA", "YESBANK")
start_date <- as.Date("2020-08-01")
end_date <- as.Date(Sys.Date())

# Create an empty list to store stock data
combined_stock_data <- list()

# Get a common set of dates (assuming all stocks have the same dates)
common_dates <- NULL

# Loop through stock symbols
for (i in stock_name) {
  # Create the stock symbol with .BO
  stock_symbol <- paste(i, ".BO", sep = "")
  # Use getSymbols to fetch the stock data
  stock_data <- getSymbols(stock_symbol, from = start_date, to = end_date, auto.assign = FALSE)
  # Extract only .close prices
  for_chart <- paste(i, ".BO.Close", sep = "")
  closing_prices <- stock_data[, for_chart]
  
  # Get dates from the first stock (assuming all stocks have the same dates)
  if (is.null(common_dates)) {
    common_dates <- index(closing_prices)
  }
  
  # Interpolate closing prices to match common_dates
  closing_prices <- approx(index(closing_prices), closing_prices, xout = common_dates)$y
  
  # Store the closing prices in the combined_stock_data list
  combined_stock_data[[i]] <- closing_prices
}

# Create a data frame with common dates and closing prices for all stocks
closing_prices_df <- data.frame(Date = common_dates, combined_stock_data)

# Reshape the data to long format using reshape2. It's easy to plot.
closing_prices_long <- melt(closing_prices_df, id.vars = "Date", variable.name = "Stock", value.name = "ClosingPrice")

# Create a line plot using ggplot2
ggplot(closing_prices_long, aes(x = Date, y = ClosingPrice, color = Stock)) +
  geom_line() +
  labs(x = "Dates", y = "Closing Price") +
  theme_minimal()
