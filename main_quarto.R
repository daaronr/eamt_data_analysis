# one R file to do everything ####

# Setup ####
#bring in data, packages, etc, whatever else you want to do here
# NO -- this doesn't work because it the environment is not reused across chapters!

# Code to run once to switch things over to Quarto ####
library(pacman)
library(devtools)
library(rex)
library(purrr)
source_url("https://raw.githubusercontent.com/daaronr/dr-rstuff/master/functions/parse_rp_bookdown_to_quarto.R")

rmd_files <- c("oftw_upsell_input_first_analysis.Rmd", "gwc_gg.Rmd", "gwwc_fb.Rmd", "tlycs_placeholder.Rmd",  "tlycs_input_simple_analysis.Rmd") 

system("mkdir chapters")

map2(rmd_files, rmd_files,
  ~ rp_rmd_to_quarto(.y, here::here("chapters", .y)))

newName <- sub(".Rmd", ".qmd", here::here("chapters", rmd_files))
file.rename(here::here("chapters", rmd_files), newName)

rp_rmd_to_quarto("index.Rmd", "index.qmd")

# render the book ####
system("quarto render")

