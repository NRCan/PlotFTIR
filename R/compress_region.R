compress_above <- function(intercept = 2000, ratio = 5) {
  trans_new("compress",
            transform = function(x) { ifelse(x < intercept, x, (x-intercept)/ratio+intercept)},
            inverse = function(x) { ifelse(x < intercept, x, ((x-intercept) * ratio)+intercept)})
  }
