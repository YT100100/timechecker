test_that("set_loop_timechecker", {

  f <- function() {

    tc <- set_step_timechecker()

    tc('Simulation')
    df <- data.frame(x = 1:10, y = 1:10 + rnorm(10))
    Sys.sleep(2)

    tc('Data augumentation')
    df$x2 <- df$x ^ 2
    df$x3 <- df$x ^ 3
    Sys.sleep(3)

    tc('Regression')
    lmres <- lm(y ~ ., df)
    Sys.sleep(4)

    tc()
    coef(lmres)

  }
  ans <- f()

  # when you place set_loop_timechecker in a loop,
  # argument char_pre can be used for readability
  tcl <- set_loop_timechecker(3, overwrite = FALSE)
  for (i in 1:3) {

    tc <- set_step_timechecker(char_pre = '  ')

    tc('Simulation')
    df <- data.frame(x = 1:10, y = 1:10 + rnorm(10))
    Sys.sleep(2)

    tc('Data augumentation')
    df$x2 <- df$x ^ 2
    df$x3 <- df$x ^ 3
    Sys.sleep(3)

    tc('Regression')
    lmres <- lm(y ~ ., df)
    Sys.sleep(4)

    tc()
    tcl()

  }

  # when you place set_loop_timechecker inside a step in set_step_timechecker,
  # argument print_done can be used for readability
  f2 <- function() {

    tc <- set_step_timechecker()

    tc('Simulation')
    n <- 10
    df <- data.frame(x = 1:n, y = 1:n + rnorm(10))
    Sys.sleep(2)

    tc('Data augumentation', print_done = FALSE)
    tcl <- set_loop_timechecker(n)
    for (i in seq_len(n)) {
      df$x2[i] <- df$x[i] ^ 2
      df$x3[i] <- df$x[i] ^ 3
      Sys.sleep(0.2)
      tcl(char_pre = '   ')
    }

    tc('Regression')
    lmres <- lm(y ~ ., df)
    Sys.sleep(4)

    tc()
    coef(lmres)

  }
  ans <- f2()

  # verbose
  f <- function() {

    tc <- set_step_timechecker(verbose = FALSE)

    tc('Simulation')
    df <- data.frame(x = 1:10, y = 1:10 + rnorm(10))
    Sys.sleep(2)

    tc('Data augumentation')
    df$x2 <- df$x ^ 2
    df$x3 <- df$x ^ 3
    Sys.sleep(3)

    tc('Regression')
    lmres <- lm(y ~ ., df)
    Sys.sleep(4)

    tc()
    coef(lmres)

  }
  ans <- f()

})
