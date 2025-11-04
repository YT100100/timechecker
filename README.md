# timechecker: An R package to visualize processing time with standard output

## Installation

```
devtools::install_github('YT100100/timechecker')
```

## Usage

This package consists of two functions. `set_loop_timechecker` function returns a function, which visualizes the progress of interation process.

```r
iters <- 1:1000
ans <- NULL
tc <- set_loop_timechecker(length(iters))
for (i in iters) {
  ans <- c(ans, i)
  Sys.sleep(0.002)
  tc()
}
```

![Demo movie of set_loop_timechecker function.](https://github.com/user-attachments/assets/1bf7d19e-24da-4cc6-ae03-0bf83fee9c28)

`set_step_timechecker` function also returns a function, which visualizes ellapsed time in each processing step. This function is intended to be placed in a function.

```r
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
```

![Demo movie of set_step_timechecker function.](https://github.com/user-attachments/assets/6cf14dc6-6ce3-49d8-b465-ff5570e15826)
