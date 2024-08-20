sec_to_chr <- function(total) {

  rest <- total

  second <- rest %% 60
  rest <- rest - second
  second <- round(second)

  minute <- rest %% 3600
  rest <- rest - minute
  minute <- minute / 60

  hour <- rest %% 86400
  rest <- rest - hour
  hour <- hour / 3600

  day <- rest / 86400

  ans <- ''
  ans <- paste0(ans, ifelse(day    == 0, '', paste0(day   , 'd ')))
  ans <- paste0(ans, ifelse(hour   == 0, '', paste0(hour  , 'h ')))
  ans <- paste0(ans, ifelse(minute == 0, '', paste0(minute, 'm ')))
  ans <- paste0(ans, second, 's')

  return(ans)

}
