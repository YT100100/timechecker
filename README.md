# timechecker: An R package to visualize processing time with standard output

## Installation

```
install.packages('timechecker')
```

## Usage

This package consists of two functions. `set_loop_timechecker` function returns a function, which visualizes the progress of iteration process.

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

![Demo movie of set_loop_timechecker function.](https://github.com/user-attachments/assets/f3ef2689-d01c-4af0-99b5-bc4bc2aec90c)

`set_step_timechecker` function also returns a function, which visualizes elapsed time in each processing step. This function is intended to be placed in a function.

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

![Demo movie of set_step_timechecker function.](https://github.com/user-attachments/assets/62df6d82-fe07-44da-ba36-abbd4833787b)
