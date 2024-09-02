#' Print elapsed and remaining time in iterative processes.
#'
#' \code{set_loop_timechecker} returns a function that records and prints
#' processing time in interative processes.
#'
#' Provided with the number of iterations,
#' this function creates a function named \code{loop_timechecker}
#' which records and prints processing time in interative process.
#' In actual usage, it is recommended to call this function and
#' creating \code{loop_timechecker} right before the iteration process,
#' and place the \code{loop_timechecker}
#' at the beginning of the iteration process.
#'
#' If you want to keep all printed records in your console,
#' please set \code{overwrite = FALSE}.
#'
#' The \code{timestep} argument determines the frequency of updating
#' printed information since too fast an update will decrease visibility.
#'
#' @param n_iter The number of iterations.
#' @param overwrite Logical. Should the message be overwritten?
#' @param timestep The smallest time step of the output (sec).
#'
#' @return A function \code{loop_timechecker}.
#'   When placed at the head of the iterations,
#'   it records and prints the progress of iteration, elapsed time,
#'   and predicted remaining time.
#'   You can add arbitrary strings to the printed messages
#'   by using \code{char_pre} and \code{char_post} arguments.
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
#'   tc2 <- set_loop_timechecker(length(iters2))
#'   for (j in iters2) {
#'     tc2(char_pre = '-- ')
#'     ans <- c(ans, i * j)
#'     Sys.sleep(0.004)
#'   }
#' }
#'
#' # char_pre or char_post can also be used to check name of current process
#' iters <- paste0('case', LETTERS[1:10])
#' tc <- set_loop_timechecker(length(iters))
#' for (i in iters) {
#'   tc(char_post = paste0('  Processing ', i))
#'   Sys.sleep(1)
#' }
#' @export
set_loop_timechecker <- function(n_iter, overwrite = TRUE, timestep = 0.5) {

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

  # set internal variables
  count <- -1
  start_time <- proc.time()[3]
  prev_print_time <- start_time

  loop_timechecker <- function(char_pre = '', char_post = '') {

    # check arguments
    char_pre  <- as.character(char_pre)
    char_post <- as.character(char_post)
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
    message <- sprintf('%s / %i (%s%%)', count_chr, n_iter, count_per)

    # add time information to the message
    if (count >= 1) {
      message <- sprintf(
        '%s  Elapsed: %s  Remaining: %s',
        message, sec_to_chr(elapsed_time), sec_to_chr(remain_time))
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

