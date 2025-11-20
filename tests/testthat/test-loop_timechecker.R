test_that("set_loop_timechecker", {

  # basic use (1 digit case)
  iters <- 1:9
  ans <- NULL
  tc <- set_loop_timechecker(length(iters))
  for (i in iters) {
    ans <- c(ans, i)
    Sys.sleep(0.2)
    tc()
  }

  # basic use (2 digit case)
  iters <- 1:99
  ans <- NULL
  tc <- set_loop_timechecker(length(iters))
  for (i in iters) {
    ans <- c(ans, i)
    Sys.sleep(0.02)
    tc()
  }

  # basic use (6 digit case)
  iters <- 1:100000
  ans <- NULL
  tc <- set_loop_timechecker(length(iters))
  for (i in iters) {
    ans <- c(ans, i)
    Sys.sleep(0.0002)
    tc()
  }

  # without time stamp
  iters <- 1:100000
  ans <- NULL
  tc <- set_loop_timechecker(length(iters), show_timestamp = FALSE)
  for (i in iters) {
    ans <- c(ans, i)
    Sys.sleep(0.00002)
    tc()
  }

  # For multiple loops, overwrite and char_pre arguments can be used for readability
  iters1 <- 1:3
  iters2 <- 1:100
  ans <- NULL
  tc1 <- set_loop_timechecker(length(iters1), overwrite = FALSE)
  for (i in iters1) {
    tc2 <- set_loop_timechecker(length(iters2))
    for (j in iters2) {
      ans <- c(ans, i * j)
      Sys.sleep(0.004)
      tc2(char_pre = '-- ')
    }
    tc1()
  }

  # char_pre or char_post can also be used to check name of current process
  iters <- paste0('case', LETTERS[1:10])
  tc <- set_loop_timechecker(length(iters))
  for (i in iters) {
    Sys.sleep(1)
    tc(char_post = paste0('  Processing ', i))
  }

  # verbose
  iters <- 1:9
  ans <- NULL
  tc <- set_loop_timechecker(length(iters), verbose = FALSE)
  for (i in iters) {
    ans <- c(ans, i)
    Sys.sleep(0.2)
    tc()
  }

})
