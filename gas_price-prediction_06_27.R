
library(readr)
library(dplyr)

# Import the Monthly Gas Price dataset
gas_prices <- read_csv("gasmprice.csv")

# Take a glimpse at the contents
glimpse(gas_prices)
head(gas_prices)
# Import again, only reading specific columns
gas_prices <- read_csv("gasmprice.csv",col_types = cols_only(
  Date = col_character(),
  Price = col_double()
)
)

# Rename the columns to be more informative
gas_prices_renamed <- gas_prices %>%
  rename(month = Date, price_usd = Price)

head(gas_prices_renamed)
# Check the result
glimpse(gas_prices_renamed)

library(lubridate)

# Convert year and month to Date
gas_prices_clean <- gas_prices_renamed %>%
  mutate(date = seq(as.Date("1993/4/1"), as.Date("2021/2/1"), "1 month")) %>%
  select(-month)


# See the result
tail(gas_prices_cleaned)
# the expected format, finally
gas_prices_cleaned <- gas_prices_clean %>%
  relocate(date)

# Load ggplot2
library(ggplot2)

# Draw a line plot of price vs.date
gas_prices_cleaned %>%
  ggplot(aes(x = date, y = price_usd))+
  geom_line(alpha = 0.2)+
  ggtitle("Gas price over time")



# See the result
gas_prices_summarized

# Load magrittr
library(magrittr)

# Extract a time series
gas_time_series <- gas_prices_cleaned %$% 
  ts(
    price_usd,
    start = c(1993, 4), 
    end = c(2021, 2), 
    frequency = 12
  )


# See the result
gas_time_series



# Forecast the gas time series



#forecast with auto.arima#1
library(forecast)
library(fpp2)
fit <- auto.arima(gas_time_series, stepwise = FALSE) #works
checkresiduals(fit)
fit %>% forecast(h = 24) %>% autoplot()
summary(fit)



#forecast with auto.arima#3 

fit2 <- Arima(gas_time_series, order = c(2, 1, 3), include.constant = TRUE)  #works
checkresiduals(fit2)
fit2 %>% forecast(h = 24) %>% autoplot()
summary(fit2)

#forecast with training and test sets
train <- subset(gas_time_series, end = c(2016, 12))
fit3 <- auto.arima(train, stepwise = FALSE)
checkresiduals(fit3)
fit3 %>% forecast(h = 24) %>% autoplot()
summary(fit3)


#making a point forecast
gas_price_forecast <- forecast(gas_time_series)

# View it
gas_price_forecast

# Plot the forecast
autoplot(gas_price_forecast)+
  ggtitle("Gas price forecast")

#another method, Doesn't work

train <- window(gas_time_series, start = c(1994, 1), end = c(2018, 12))
fit4 <- ets(train)
checkresiduals(fit4)

#another method, doesn't work
fit5 <- tbats(gas_time_series) #
checkresiduals(fit5)

















