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
set_loop_timechecker <- function(n_iter, overwrite = TRUE, timestep = 1) {

  count <- 0
  start_time <- proc.time()[3]
  prev_print_time <- start_time

  loop_timechecker <- function() {

    count <<- count + 1

    # check time step
    current_time <- proc.time()[3]
    if ((current_time - prev_print_time) < timestep) {
      return()
    } else {
      prev_print_time <<- current_time
    }

    # estimate finish time
    ellapsed_time <- current_time - start_time
    whole_time <- ellapsed_time / (count - 1) * n_iter
    finish_time <- round(Sys.time() + n_iter)

    # create message
    message <- paste0('Running ', count, ' / ', n_iter, '. ')
    if (count >= 1) {
      message <- paste0(
        message, 'Ellapsed: ', round(ellapsed_time), 's. ',
        'Finish time: ', finish_time, '.')
    }
    message <- if (overwrite && count <= n_iter) {
      paste0(message, '\r')
    } else {
      paste0(message, '\n')
    }
    cat(message)

  }
  loop_timechecker

}

