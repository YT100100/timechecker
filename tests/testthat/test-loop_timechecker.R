test_that("set_loop_timechecker", {

  # basic use
  iters <- 1:100000
  ans <- NULL
  tc <- set_loop_timechecker(length(iters))
  for (i in iters) {
    tc()
    ans <- c(ans, i)
    Sys.sleep(0.0002)
  }

  # without time stamp
  iters <- 1:100000
  ans <- NULL
  tc <- set_loop_timechecker(length(iters), show_timestamp = FALSE)
  for (i in iters) {
    tc()
    ans <- c(ans, i)
    Sys.sleep(0.00002)
  }

  # For multiple loops, overwrite and char_pre arguments can be used for readability
  iters1 <- 1:3
  iters2 <- 1:100
  ans <- NULL
  tc1 <- set_loop_timechecker(length(iters1), overwrite = FALSE)
  for (i in iters1) {
    tc1()
    tc2 <- set_loop_timechecker(length(iters2))
    for (j in iters2) {
      tc2(char_pre = '-- ')
      ans <- c(ans, i * j)
      Sys.sleep(0.004)
    }
  }

  # char_pre or char_post can also be used to check name of current process
  iters <- paste0('case', LETTERS[1:10])
  tc <- set_loop_timechecker(length(iters))
  for (i in iters) {
    tc(char_post = paste0('  Processing ', i))
    Sys.sleep(1)
  }

})
