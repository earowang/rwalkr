# create sensor vector
library(rwalkr)
sensor <- unique(walk_melb(from = Sys.Date() - 1L)$Sensor)
devtools::use_data(sensor, internal = TRUE, overwrite = TRUE)
