#' Print predicted finish time.
#'
#' @examples
#' @export
set_step_timechecker <- function(char_pre = '', char_post = '') {

  step <- 0
  start_time <- proc.time()[3]
  prev_message_len <- NA

  step_timechecker <- function(step_name = NULL) {

    step <<- step + 1

    if (step >= 2) {

      # print ellapsed time
      ellapsed_time <- proc.time()[3] - start_time
      message_done <- paste0(
        'Done. (', sec_to_chr(ellapsed_time), ')', char_post, '\n')

      # add spaces to the message
      n_dot <- getOption('width') - prev_message_len - nchar(message_done)
      dots <- paste(rep('.', n_dot), collapse = '')
      message_done <- paste0(dots, message_done)
      cat(message_done)

      # start measuring time of the next step
      start_time <<- proc.time()[3]

    }

    if (!is.null(step_name)) {

      # print name of the next step
      message_start <- paste0(char_pre, step, '. ', step_name)
      prev_message_len <<- nchar(message_start)
      cat(message_start)

    }

  }
  step_timechecker

}

# tcl <- set_loop_timechecker(3, overwrite = FALSE)
# for (i in 1:3) {
#
#   tcl()
#   tc <- set_step_timechecker(char_pre = '  ')
#
#   tc('create data')
#   df <- data.frame(x = 1:10, y = 1:10 + rnorm(10))
#   Sys.sleep(2)
#
#   tc('plot data')
#   plot(df)
#   Sys.sleep(3)
#
#   tc('regression')
#   abline(coef(lm(y ~ x, df)))
#   Sys.sleep(4)
#
#   tc()
# }
