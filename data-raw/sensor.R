# create sensor vector
library(rwalkr)
sensor <- unique(walk_melb(from = Sys.Date() - 1L)$Sensor)
sensor_abbr <- gsub(" ", "", gsub("[:a-z:]", "", sensor))
sensor_df <- data.frame(
  sensor = sensor, abbr = sensor_abbr, 
  stringsAsFactors = FALSE
)
devtools::use_data(sensor_df, internal = TRUE, overwrite = TRUE)
