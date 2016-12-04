# Transform the data to include actual airport and carrier names rather than
# their codes
mapNames <- function() {
  left_join(flights, airlines) %>% 
    select(-carrier) %>% 
    rename(carrier = name) %>% 
    left_join(., airports, by = c("origin" = "faa")) %>% 
    select(-origin, -(lat:tz)) %>% 
    rename(airport = name)
}

# Calculate the number of flights from each airport for each month
numFlightsFromAirport <- function(data) {
  data %>% 
    group_by(airport, month) %>% 
    summarise(num_flights = n()) %>% 
    ungroup()
}

# Plot the number of flights from each airport for each month
plotNumFlights <- function(data) {
  ggplot(data = data, aes(x = month, y = num_flights, fill = airport)) +
    geom_col(position = "dodge", colour = "black") +
    labs(x = "Month", y = "Number of Flights", fill = "Airport") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_fill_brewer(palette = "YlGn") +
    theme_bw()
}

# Calculate the mean departure delay for each month by carrier
meanDepartureDelayByCarrier <- function(data) {
  data %>% 
    group_by(carrier, month) %>% 
    summarise(mean_delay = mean(dep_delay, na.rm = TRUE)) %>% 
    ungroup
}

# Plot the mean departure delay for each month by carrier
plotDepDelay <- function(data) {
  ggplot(data = data, aes(x = month, y = mean_delay)) +
    geom_col() +
    facet_wrap( ~ carrier, nrow = 4) +
    labs(x = "Month", y = "Mean Delay") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
