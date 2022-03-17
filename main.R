#### Building the  bookdown ... command line ####

# NOTE: the package loading is redundant for now -- if we use 'David's system' we would remove this from index.Rmd

# Load packages ####
#Note: I am mainly using renv for package management here, so the things below are not necessary 
library(pacman)
p_load(dplyr, knitr, here, bookdown, install = TRUE)
p_load(readxl)
p_load(ggplot2)
p_load(plotly)
p_load(plm)
p_load(pryr)
p_load(stringr)
p_load(stringr)
p_load(effects)
p_load(devtools)
p_load(googledrive)
p_load(googlesheets4)
p_load(esquisse)
p_load(rsconnect) #for rstudio 'private access' pubilshing
devtools::install_github("paulhendricks/anonymizer")
library(anonymizer)

#source(here("code", "packages.R")) # Install and load packages used in build and analysis (note: these could be cleaned)

#source_url("https://raw.githubusercontent.com/daaronr/dr-rstuff/master/functions/baseoptions.R")

devtools::install_github("rethinkpriorities/rp-r-package")
library(rethinkpriorities)
  
devtools::install_github("rethinkpriorities/r-noodling-package")
library(rnoodling)

### Source local model-building tools/functions ####
#source(here::here("code", "hypothesis_test.R")) #this is being moved to the rethinkpriorities r package


knitr::write_bib(
  x = c(.packages(), "bookdown", "knitr", "rmarkdown"),
  file = "assets/bib/packages.bib"
)


# For a list of 'rethink packages' and common packages we install, see: LINK HERE ####


{
  options(knitr.duplicate.label = "allow")
  rmarkdown::render_site(output_format = 'rethinkpriorities::book', encoding = 'UTF-8')
}


# Zip and move to dropbox (latter is not portable code) ####

PASSWORD <- "reinstein"

files2zip <- dir('docs', full.names = TRUE)

zip(zipfile = 'eamt_bookdown',
  files = files2zip,
  flags = paste("--password", PASSWORD)
  )

file.copy(from = "eamt_bookdown.zip",
  to   = "../../Dropbox/ea_marketing_misc_for_sharing/eamt_bookdown.zip", overwrite = TRUE)

#bookdown::publish_book()

