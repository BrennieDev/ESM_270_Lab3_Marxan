# ESM 270 Lab 3
# Designing Representative Networks

library(here)
library(prioritizr)
library(tidyverse)
library(raster)

read_dat <- function(path) {
  dat <- data.table::fread(path, data.table = FALSE)
  return(dat)
}

# Read in .dat files
pu_data <- read_dat(here("data", "MorroBay_pu.dat"))
sp_data <- read_dat(here("data", "MorroBay_spec.dat"))
puvspr_data <- read_dat(here("data", "MorroBay_puvspr.dat"))
#bound_data <- read_dat(here("data", "MorroBay_bound.dat"))

sp_data <- rename(sp_data, amount = target)

# create problem
prob <- marxan_problem(x = pu_data, spec = sp_data, puvspr = puvspr_data) %>% 
  add_rsymphony_solver(verbose = FALSE)

solution <- solve(prob)

# Join solution with MorroBay_parcels.shp on the pu "id"
mb_shape <- shapefile(here("data", "MorroBay_parcels.shp"))
merged_shape <- merge(mb_shape, solution, by='id')

# write the merged shp to file
shapefile(merged_shape, here("data/output/mb_solution.shp"))

