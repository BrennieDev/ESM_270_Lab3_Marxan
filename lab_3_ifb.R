# ESM 270 Lab 3
# Authors: Ian Brunjes, Teague Tran
# Designing Representative Networks

library(here)
library(prioritizr)
library(tidyverse)
library(raster)
library(stringr)

# Function to read in .dat file as data frame
read_dat <- function(path) {
  dat <- data.table::fread(path, data.table = FALSE)
  return(dat)
}

# Read in .dat files
pu_data <- read_dat(here("data", "MorroBay_pu.dat"))
sp_data <- read_dat(here("data", "MorroBay_spec.dat")) %>% rename(amount = target)
puvspr_data <- read_dat(here("data", "MorroBay_puvspr.dat"))
#bound_data <- read_dat(here("data", "MorroBay_bound.dat"))

# Function to execute marxan analysis and output to file
run_marxan_problem <- function(pu_data, sp_data, puvspr_data, outfile) {
  # Create marxan problem
  prob <- marxan_problem(x = pu_data, spec = sp_data, puvspr = puvspr_data) %>% 
    add_rsymphony_solver(verbose = FALSE)
  
  # Find solution using rsymphony solver
  solution <- solve(prob)
  
  # Join solution with MorroBay_parcels.shp on the pu "id"
  mb_shape <- shapefile(here("data", "MorroBay_parcels.shp"))
  merged_shape <- merge(mb_shape, solution, by='id')
  # Write the merged shp to file
  shapefile(merged_shape, outfile, overwrite = TRUE)
}

# Run analysis using base Morro Bay data with no alterations
run_marxan_problem(pu_data, sp_data, puvspr_data, here("data/output", "mb_solution_base.shp"))

# Run analysis using different inputs
# Read in species status data
sp_status <- readxl::read_xlsx(here("data", "spec_name_status.xlsx"))

# Merge species input data and species status data
sp_merged <- merge(sp_data, sp_status, by='id')

# Set up weighted species penalty scores based on Global rank
sp_g_rank <- sp_merged %>%
  mutate(spf = case_when(
    str_detect(status, "G1") ~ 500,
    str_detect(status, "G2") ~ 400,
    str_detect(status, "G3") ~ 250,
    str_detect(status, "G4") ~ 50,
    TRUE ~ 0
    )) %>% 
  rename(name = name.x) %>% 
  select(colnames(sp_data))

# Run using all eligible parcels and spf based on global ranks
run_marxan_problem(pu_open, sp_g_rank, puvspr_data, here("data/output", "mb_global_solution.shp"))

# Set up weighted species penalty scores based on Subnational rank
sp_s_rank <- sp_merged %>%
  mutate(spf = case_when(
    str_detect(status, "S1") ~ 200,
    str_detect(status, "S2") ~ 150,
    str_detect(status, "S3") ~ 100,
    str_detect(status, "S4") ~ 50,
    TRUE ~ 0
  )) %>% 
  rename(name = name.x) %>% 
  select(colnames(sp_data))

# Run using all eligible parcels and spf based on subnational ranks
run_marxan_problem(pu_open, sp_s_rank, puvspr_data, here("data/output", "mb_subnational_solution.shp"))
