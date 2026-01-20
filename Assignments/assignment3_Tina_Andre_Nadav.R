#' Almond Yield Anomaly Model
#' Computes yield of a crop based on available metereological data and a specific model equation (currently hardcoded)
#' @param data data frame which includes columns:
#'  -- tmin_c: minimum temperature per day (deg-C)
#'  -- precip: precipitation per day (mm)
#' @author Tina, Andre, Nadav
#' @return y yield of almonds (tons/acre) 
calculate_yield = function(data) {
  ### Pseudocode
  # 1. Aggregate data from daily to monthly timescale
  # 2. Get average February minimum temperature for each year
  # 3. Collect average precipitation data for January for each year
  # 4. Run model to get yield per year
  # 5. Get summary table
  
  ## 1. Aggregate data from daily to monthly timescale
  monthly_data = data |>
    group_by(year, month) |>
    summarize(tmin_mean = mean(tmin_c, na.rm=TRUE),
              precip_mean = sum(precip, na.rm=TRUE))
  
  ## 2. Get Feb mins
  feb_tmin = monthly_data |>
    filter(month == 2) |> select(tmin_mean)

  ## 3. Get Jan precip data
  jan_precip = monthly_data |>
    filter(month == 1) |> select(precip_mean)

  # Merge the precipitation and the temperature data into a data frame
  yearly_table = left_join(feb_tmin, jan_precip, by = "year")

  ## 4. Run the yield calculations to get yield per year
  yearly_table$yield = almond_yield(tmin_c = yearly_table$tmin_mean, precip = yearly_table$precip_mean)
  
  ## 5. Return a table of aggregated values and output
  return(yearly_table)
}

#' Computes almond yield based on aggregated parameters into the calculate_yield function
#' @param tmin_c minimum temperature per day (deg-C)
#' @param precip precipitation per day (mm)
#' @return y yield of almonds (tons/acre) 
almond_yield = function(tmin_c, precip) {
  y = -0.015*tmin_c - 0.0046*tmin_c^2 - 0.07*precip + 0.0043*precip^2 + 0.28
  return(y)
}

### Run the climate anomaly model
# Input data into clim

# Run model
yields = calculate_yield(clim)
