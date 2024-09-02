#' Print elapsed and remaining time in each code block.
#'
#' \code{set_step_timechecker} returns a function that records and prints
#' processing time in each code block.
#'
#' This function creates a function named \code{step_timechecker}
#' which records and prints processing time in code blocks.
#' The created function \code{step_timechecker} should be placed
#' at the head of each code block with its name.
#' Also, this function should be placed at the end of all steps
#' with no argument.
#'
#' @param char_pre String to be added before messages.
#' @param char_post String to be added after messages.
#'
#' @return A function \code{step_timechecker}.
#'   When placed at the head of each code block,
#'   it records and prints the progress and elapsed time.
#'   An argument \code{step_name}, a string representing the name of the steps,
#'   should be provided. Otherwise, the function judges that
#'   it is the end of the measurement.
#'   An argument \code{print_done} can also be used if you do not wish to
#'   add message when the process of the step ends.
#'
#' @seealso \code{\link{set_loop_timechecker}}
#'
#' @examples
#' f <- function() {
#'
#'   tc <- set_step_timechecker()
#'
#'   tc('Simulation')
#'   df <- data.frame(x = 1:10, y = 1:10 + rnorm(10))
#'   Sys.sleep(2)
#'
#'   tc('Increasing explanatory variables')
#'   df$x2 <- df$x ^ 2
#'   df$x3 <- df$x ^ 3
#'   Sys.sleep(3)
#'
#'   tc('Regression')
#'   lmres <- lm(y ~ ., df)
#'   Sys.sleep(4)
#'
#'   tc()
#'   coef(lmres)
#'
#' }
#' ans <- f()
#'
#' # when you place set_loop_timechecker in a loop,
#' # argument char_pre can be used for readability
#' tcl <- set_loop_timechecker(3, overwrite = FALSE)
#' for (i in 1:3) {
#'
#'   tcl()
#'   tc <- set_step_timechecker(char_pre = '  ')
#'
#'   tc('Simulation')
#'   df <- data.frame(x = 1:10, y = 1:10 + rnorm(10))
#'   Sys.sleep(2)
#'
#'   tc('Increasing explanatory variables')
#'   df$x2 <- df$x ^ 2
#'   df$x3 <- df$x ^ 3
#'   Sys.sleep(3)
#'
#'   tc('Regression')
#'   lmres <- lm(y ~ ., df)
#'   Sys.sleep(4)
#'
#'   tc()
#'
#' }
#'
#' # when you place set_loop_timechecker inside a step in set_step_timechecker,
#' # argument print_done can be used for readability
#' f2 <- function() {
#'
#'   tc <- set_step_timechecker()
#'
#'   tc('Simulation')
#'   n <- 10
#'   df <- data.frame(x = 1:n, y = 1:n + rnorm(10))
#'   Sys.sleep(2)
#'
#'   tc('Increasing explanatory variables', print_done = FALSE)
#'   tcl <- set_loop_timechecker(n)
#'   for (i in seq_len(n)) {
#'     tcl()
#'     df$x2[i] <- df$x[i] ^ 2
#'     df$x3[i] <- df$x[i] ^ 3
#'     Sys.sleep(0.2)
#'   }
#'
#'   tc('Regression')
#'   lmres <- lm(y ~ ., df)
#'   Sys.sleep(4)
#'
#'   tc()
#'   coef(lmres)
#'
#' }
#' ans <- f2()
#' @export
set_step_timechecker <- function(char_pre = '', char_post = '') {

  # check arguments
  char_pre  <- as.character(char_pre)
  char_post <- as.character(char_post)
  stopifnot(length(char_pre ) == 1)
  stopifnot(length(char_post) == 1)

  # initial settings
  step <- 0
  start_time <- proc.time()[3]
  prev_message_len <- NA
  prev_print_done <- TRUE

  step_timechecker <- function(step_name = NULL, print_done = TRUE) {

    # check arguments
    if (!is.null(step_name)) {
      step_name <- as.character(step_name)
    }
    print_done <- as.logical(print_done)

    # proceed step number
    step <<- step + 1

    if (step >= 2) {

      if (prev_print_done) {

        # print elapsed time
        elapsed_time <- proc.time()[3] - start_time
        message_done <- paste0(
          'Done. (', sec_to_chr(elapsed_time), ')', char_post, '\n')

        # add spaces to the message
        n_dot <- getOption('width') - prev_message_len - nchar(message_done)
        dots <- paste(rep('.', n_dot), collapse = '')
        message_done <- paste0(dots, message_done)
        cat(message_done)

      }

      # start measuring time of the next step
      start_time <<- proc.time()[3]

    }

    if (!is.null(step_name)) {

      # print name of the next step
      message_start <- paste0(char_pre, step, '. ', step_name)
      if (!print_done) message_start <- paste0(message_start, '\n')
      cat(message_start)

      # save message length
      prev_message_len <<- nchar(message_start)

    }

    prev_print_done <<- print_done

  }
  step_timechecker

}
