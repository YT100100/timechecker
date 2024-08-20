#' Print predicted finish time.
#' @param n_iter The number of iterations.
#' @param overwrite Logical. Should the message be overwritten?
#' @param timestep The smallest time step of the output (sec).
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
#' @export
set_loop_timechecker <- function(n_iter, overwrite = TRUE, timestep = 0.5,
                                 char_pre = '', char_post = '') {

  count <- -1
  start_time <- proc.time()[3]
  prev_print_time <- start_time

  loop_timechecker <- function() {

    count <<- count + 1

    # check time step
    current_time <- proc.time()[3]
    is_too_fast <- (current_time - prev_print_time) < timestep
    if (is_too_fast && count >= 1) {
      return()
    } else {
      prev_print_time <<- current_time
    }

    # estimate finish time
    ellapsed_time <- current_time - start_time
    rest_time <- ellapsed_time / count * (n_iter - count)

    # create message
    count_chr <- formatC(count, width = nchar(n_iter))
    count_per <- formatC(floor(count / n_iter * 100), width = 3)
    message <- paste0(count_chr, ' / ', n_iter, ' (', count_per, '%)  ')

    # add time information to the message
    if (count >= 1) {
      message <- paste0(
        message,
        'Ellapsed: ', sec_to_chr(ellapsed_time), '  ',
        'Rest: '    , sec_to_chr(rest_time), '')
    }

    # add given characters to the message
    message <- paste0(char_pre, message, char_post)

    # add spaces to the message
    n_space <- getOption('width') - nchar(message)
    n_space <- pmax(n_space, 0)
    spaces <- paste(rep(' ', n_space), collapse = '')
    message <- paste0(message, spaces)

    # add a character for line break to the message
    message <- if (overwrite && count < n_iter) {
      paste0(message, '\r')
    } else {
      paste0(message, '\n')
    }
    cat(message)

  }
  loop_timechecker

}

# iters <- 1:1000
# ans <- NULL
# tc <- set_loop_timechecker(length(iters))
# for (i in iters) {
#   tc()
#   ans <- c(ans, i)
#   Sys.sleep(0.002)
# }
