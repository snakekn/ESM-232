# Lecture 5: follow-in-class on varying across parameters
library(tidyverse)
library(here)

source(here("R/solarpv.R"))
load(here("Data/sierraczosolar_clean.rda"))

# build parameters
nsamples <- 300
deviation <- 0.15
base_thresh <- 20000
ethresh <- runif(
  min = base_thresh - deviation * base_thresh,
  max = base_thresh + deviation * base_thresh, n = nsamples
)

eff <- rnorm(mean = 0.6, sd = 0.1, n = nsamples)

# make a dataframe of these values, just two columns together (What we needed in A3)
parms <- cbind.data.frame(eff, ethresh)

# run the model for each value selected. note that parms uses columns with the same name as our mdel parameters
results = parms |> pmap(solarpv,
                        area = 20,
                        solar = sierraczosolar, clr = "green",
                        eunit = "kWhr", g = FALSE, etype = "direct")

# unnest and collect the values we want
mean_elect = map_df(results, `[`, c("mean"))

# put the final values back into a df with the initial params
mean_elect <- cbind.data.frame(mean_elect, parms)

# note: worthwhile to check each parameter & output combo
p1 = ggplot(mean_elect, aes(eff, mean, col=ethresh))+
  geom_point(cex=2)+
  labs(y="Mean Annual Electricity kWhr/yr")


# generate unique ID for each model output
annual_elect = map_df(results, `[`, c("annual"), .id="parm_id")

# turn into a data frame and clean up column names
solar_annual_elect <- tmp %>% unnest(annual) %>% mutate(
  year = as.numeric(year),
  elect = as.numeric(elect),
  parm_id = parm_id
)

p1 = ggplot(solar_annual_elect, aes(year, elect, group = year)) +
  geom_boxplot() +
  labs(y = "Electricity generated in kWhr/yr", x = "Year")
p1

parms$parm_id = as.character(1:nrow(parms))

solar_annual_elect2 <- solar_annual_elect %>%
  left_join(parms, by = "parm_id")

# facet by year to see the relationship across time
p3 = ggplot(solar_annual_elect2, aes(eff, elect, col=ethresh)) +
  geom_point() + facet_wrap(~year) +
  labs(y = "Electricity generated in kWHr", x = "Efficiency")
p3

# saved for later
save(solar_annual_elect, file = here("Data/annual_elect_solar.rda"))

