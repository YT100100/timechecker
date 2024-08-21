#' Print elapsed and remaining time in iterative processes.
#'
#' \code{set_loop_timechecker} returns a function that records and prints processing time in interative processes.
#'
#' Provided with the number of iterations,
#' this function creates a function named \code{loop_timechecker}
#' which records and prints processing time in interative process.
#' In actual usage, it is recommended to call this function and creating \code{loop_timechecker}
#' right before the iteration process, and place the \code{loop_timechecker}
#' at the beginning of the iteration process.
#'
#' If you want to keep all printed records in your console, please set \code{overwrite = FALSE}.
#'
#' The \code{timestep} argument determines the frequency of updating printed information
#' since too fast an update will decrease visibility.
#'
#' @param n_iter The number of iterations.
#' @param overwrite Logical. Should the message be overwritten?
#' @param timestep The smallest time step of the output (sec).
#' @param char_pre A character added to console output.
#'   It will be printed on the left of the default text.
#' @param char_post A character added to console output.
#'   It will be printed on the right of the default text.
#'
#' @return A function \code{loop_timechecker}.
#'   When placed at the head of the iterations with no argument,
#'   it records and prints the progress of interation, elapsed time, and predicted remaining time.
#'
#' @seealso \code{\link{set_step_timechecker}}
#'
#' @examples
#' iters <- 1:1000
#' ans <- NULL
#' tc <- set_loop_timechecker(length(iters))
#' for (i in iters) {
#'   tc()
#'   ans <- c(ans, i)
#'   Sys.sleep(0.002)
#' }
#'
#' # For multiple loops, overwrite and char_pre arguments can be used for readability
#' iters1 <- 1:3
#' iters2 <- 1:100
#' ans <- NULL
#' tc1 <- set_loop_timechecker(length(iters1), overwrite = FALSE)
#' for (i in iters1) {
#'   tc1()
#'   tc2 <- set_loop_timechecker(length(iters2), char_pre = '  ')
#'   for (j in iters2) {
#'     tc2()
#'     ans <- c(ans, i * j)
#'     Sys.sleep(0.004)
#'   }
#' }
#' @export
set_loop_timechecker <- function(n_iter, overwrite = TRUE, timestep = 0.5,
                                 char_pre = '', char_post = '') {

  # check arguments
  stopifnot(is.numeric(n_iter))
  n_iter <- as.integer(n_iter)
  stopifnot(n_iter >= 1)
  stopifnot(is.logical(overwrite))
  stopifnot(is.numeric(timestep))
  stopifnot(timestep >= 0)
  stopifnot(is.character(char_pre))
  stopifnot(is.character(char_post))

  # set internal variables
  count <- -1
  start_time <- proc.time()[3]
  prev_print_time <- start_time

  loop_timechecker <- function() {

    count <<- count + 1

    if (count >= n_iter) {
      warning('Number of iterations exceeded n_iter.')
      return()
    }

    # check time step
    current_time <- proc.time()[3]
    is_too_fast <- (current_time - prev_print_time) < timestep
    if (is_too_fast && count >= 1 && count <= n_iter - 2) {
      return()
    } else {
      prev_print_time <<- current_time
    }

    # estimate finish time
    elapsed_time <- current_time - start_time
    remain_time <- elapsed_time / count * (n_iter - count)

    # create message
    count_chr <- formatC(count, width = nchar(n_iter))
    count_per <- formatC(floor(count / n_iter * 100), width = 3)
    message <- paste0(count_chr, ' / ', n_iter, ' (', count_per, '%)  ')

    # add time information to the message
    if (count >= 1) {
      message <- paste0(
        message,
        'Elapsed: '  , sec_to_chr(elapsed_time), '  ',
        'Remaining: ', sec_to_chr(remain_time))
    }

    # add given characters to the message
    message <- paste0(char_pre, message, char_post)

    # add spaces to the message
    n_space <- getOption('width') - nchar(message)
    n_space <- pmax(n_space, 0)
    spaces <- paste(rep(' ', n_space), collapse = '')
    message <- paste0(message, spaces)

    # add a character for line break to the message
    message <- if (overwrite && count <= n_iter - 2) {
      paste0(message, '\r')
    } else {
      paste0(message, '\n')
    }
    cat(message)

  }
  loop_timechecker

}
