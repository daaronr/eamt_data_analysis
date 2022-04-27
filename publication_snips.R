#### Some publication code snips -- preserved from 'main' ####

# Zip and move to dropbox (latter is not portable code) ####

PASSWORD <- "reinstein"

files2zip <- dir('docs', full.names = TRUE)

zip(zipfile = 'eamt_bookdown',
  files = files2zip,
  flags = paste("--password", PASSWORD)
  )

file.copy(from = "eamt_bookdown.zip",
  to   = "../../Dropbox/ea_marketing_misc_for_sharing/eamt_bookdown.zip", overwrite = TRUE)

bookdown::publish_book(account = "daaronr@gmail.com")

