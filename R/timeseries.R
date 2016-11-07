epoch_to_timestamp <- function(time_in_seconds,
                               origin="1970-01-01",
                               tz="UTC"){
  as.POSIXct(as.numeric(as.character(time_in_seconds))
             ,origin=origin, tz=tz)
}

