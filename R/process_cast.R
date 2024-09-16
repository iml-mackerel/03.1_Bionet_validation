process_cast <- function(data, precision=1) {
  mindepth <- min(data$Profondeur)
  maxdepth <- max(data$Profondeur)
  
  if (maxdepth > 1 & maxdepth > mindepth) {
    depthrange <- seq(1, floor(maxdepth), by = precision)
    
    valinterp <- approx(data$Profondeur, data$`Vol.#2`, xout = depthrange)
    
    result <- data.frame(
      cast = data$cast[1],
      direction = data$direction[1],
      consecutive = data$consecutive[1],
      station = data$station[1],
      depth = depthrange,
      Volume2 = valinterp$y
    ) %>%
      mutate(Flow = if_else(
        direction == "ascent",
        c(diff(Volume2) * -1, NA),
        c(NA, diff(Volume2))
      ))
    
    return(result)
  } else {
    return(NULL)
  }
}


