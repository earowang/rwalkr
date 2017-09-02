# create sensor vector
library(rwalkr)
sensor <- unique(walk_melb(from = Sys.Date() - 1L)$Sensor)
sensor_abbr <- gsub(" ", "", gsub("[:a-z:]", "", sensor))
sensor_df <- data.frame(
  sensor = sensor, abbr = sensor_abbr, 
  stringsAsFactors = FALSE
)

# create a dictionary for sensor names between walk_melb and run_melb
run_sensor <- run_melb()
run_sensor <- unique(run_sensor$Sensor)
same_sensor <- intersect(sensor, run_sensor)
diff_sensor <- sensor[!(sensor %in% run_sensor)]
base_sensor <- run_sensor[!(run_sensor %in% sensor)]
diff_df <- data.frame(
  "run" = c(
    "Flinders Street Station Underpass",
    "QV Market-Elizabeth St (West)",
    "Melbourne Convention Exhibition Centre",
    "The Arts Centre",
    "Flinders St-Spark La",
    "Queen St (West)",
    "Lygon St (East)",
    "Lonsdale St-Spring St (West)",
    "St Kilda Rd-Alexandra Gardens",
    "Flinders La-Swanston St (West)",
    "Little Collins St-Swanston St (East)"
  ),
  "walk" = c(diff_sensor, rep("NA", length(base_sensor) - length(diff_sensor))),
  "match" = FALSE,
  stringsAsFactors = FALSE
)
same_df <- data.frame("run" = same_sensor, "walk" = same_sensor, "match" = TRUE,
  stringsAsFactors = FALSE)
sensor_dict <- rbind(same_df, diff_df)

devtools::use_data(sensor_df, sensor_dict, internal = TRUE, overwrite = TRUE)
