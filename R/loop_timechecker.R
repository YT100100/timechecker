#' Print elapsed and remaining time in iterative processes.
#'
#' \code{set_loop_timechecker} returns a function that records and prints
#' processing time in iterative processes.
#'
#' Provided with the number of iterations,
#' this function creates a function named \code{loop_timechecker}
#' which records and prints processing time in iterative process.
#' In actual usage, it is recommended to call this function and
#' creating \code{loop_timechecker} right before the iteration process,
#' and place the \code{loop_timechecker}
#' at the end of the iteration process.
#'
#' If you want to keep all printed records in your console,
#' please set \code{overwrite = FALSE}.
#'
#' The \code{timestep} argument determines the frequency of updating
#' printed information since too fast updates will decrease visibility.
#'
#' @param n_iter The number of iterations.
#' @param overwrite Logical. Should the message be overwritten?
#' @param timestep The smallest time step of the output (sec).
#' @param show_timestamp Logical. Should the time stamp be added to the message?
#'
#' @return A function \code{loop_timechecker}.
#'   When placed at the end of the iterations,
#'   it records and prints the progress of iteration, elapsed time,
#'   and predicted remaining time.
#'   You can add arbitrary strings to the printed messages
#'   by using \code{char_pre} and \code{char_post} arguments.
#'
#' @seealso \code{\link{set_step_timechecker}}
#'
#' @examples
#' \dontrun{
#' # These examples are not set to run automatically
#' # because they were intentionally designed to take time
#' # and fail the test.
#'
#' iters <- 1:1000
#' ans <- NULL
#' tc <- set_loop_timechecker(length(iters))
#' for (i in iters) {
#'   ans <- c(ans, i)
#'   Sys.sleep(0.002)
#'   tc()
#' }
#'
#' # For multiple loops, overwrite and char_pre arguments can be used for readability
#' iters1 <- 1:3
#' iters2 <- 1:100
#' ans <- NULL
#' tc1 <- set_loop_timechecker(length(iters1), overwrite = FALSE)
#' for (i in iters1) {
#'   tc2 <- set_loop_timechecker(length(iters2))
#'   for (j in iters2) {
#'     ans <- c(ans, i * j)
#'     Sys.sleep(0.004)
#'     tc2(char_pre = '-- ')
#'   }
#'   tc1()
#' }
#'
#' # char_pre or char_post can also be used to check name of current process
#' iters <- paste0('case', LETTERS[1:10])
#' tc <- set_loop_timechecker(length(iters))
#' for (i in iters) {
#'   Sys.sleep(1)
#'   tc(char_post = paste0('  Processing ', i))
#' }
#' @export
set_loop_timechecker <- function(
    n_iter, overwrite = TRUE, timestep = 0.5, show_timestamp = TRUE) {

  # check arguments
  n_iter <- as.integer(n_iter)
  stopifnot(is.numeric(n_iter))
  stopifnot(length(n_iter) == 1)
  stopifnot(n_iter >= 1)

  overwrite <- as.logical(overwrite)
  stopifnot(length(overwrite) == 1)

  timestep <- as.numeric(timestep)
  stopifnot(length(timestep) == 1)
  stopifnot(timestep >= 0)

  show_timestamp <- as.logical(show_timestamp)
  stopifnot(length(show_timestamp) == 1)

  # set internal variables
  count <- 0
  prev_message_len <- 0
  start_time <- proc.time()[3]
  prev_print_time <- start_time

  # create message format
  n_digit <- pmax(1, floor(log10(n_iter) + 1)) + 1
  fmt <- sprintf('%%+ %ii / %%i (%%s%%%%)', n_digit)

  loop_timechecker <- function(char_pre = '', char_post = '') {

    # check arguments
    char_pre  <- as.character(char_pre)
    char_post <- as.character(char_post)
    count <<- count + 1

    if (count >= n_iter + 1) {
      warning('Number of iterations exceeded n_iter.')
      return()
    }

    # check time step
    current_time <- proc.time()[3]
    is_too_fast <- (current_time - prev_print_time) < timestep
    if (is_too_fast && count >= 1 && count <= n_iter - 1) {
      return()
    } else {
      prev_print_time <<- current_time
    }

    # estimate finish time
    elapsed_time <- current_time - start_time
    remain_time <- elapsed_time / count * (n_iter - count)

    # create message
    count_per <- formatC(floor(count / n_iter * 100), width = 3)
    message <- sprintf(fmt, count, n_iter, count_per)
    message <- gsub('\\+', '', message)

    # add time information to the message
    message <- sprintf(
      '%s  Elapsed: %s  Remaining: %s',
      message, sec_to_chr(elapsed_time), sec_to_chr(remain_time))

    # add given characters to the message
    message <- paste0(char_pre, message, char_post)

    # add time stamp
    if (show_timestamp) {
      message <- sprintf('[%s] %s', round(Sys.time()), message)
    }

    # reset console to overwrite
    if (overwrite && count <= n_iter - 1) {
      spaces <- paste(rep(' ', prev_message_len), collapse = '')
      cat(spaces, '\r')
    }

    # print message
    cat(message)

    # add a character for line break to the message
    if (overwrite && count <= n_iter - 1) {
      cat('\r')
    } else {
      cat('\n')
    }

    # save message length
    prev_message_len <<- nchar(message)

  }
  loop_timechecker

}

