
plot.miss <- function(df, cols = NULL, miss = c(NA),
                      col = c("black"), main = "Plot of missing data",
                      leftmar = 5, ...) {
  # df - a dataframe of values to plot missingness
  # cols - a subset of cols to use (if NULL, plot missings in everything)
  # miss - a vector of values to consider missing. By default this is NA,
  #        but you could add say -9 as well: miss=c(NA,-9)
  # col - colour to plot missing data, one for each missing code in miss
  #       defaults to black
  #       miss=c(-8,-9), col=c("red","blue") would plot all -8
  #       codes in red, and all -9 in blue
  # main - a title
  # leftmar - width of left margin - may need to increase if you have
  #      long varnames
  #  ... pass any unmatched arguments to plot

  # set margins so labels aren't cut off: bottom, left, top,right
  opar <- par(no.readonly = TRUE)
  par(mar = c(2, leftmar, 1, 0.1))

  # Sort out behaviour if there's only one column
  if (is.null(cols)) cols <- 1:ncol(df)
  df <- df[, cols, drop = FALSE]

  # Check miss and col are same length; if not recycle it:
  if (length(miss) > length(col)) {
    col <- rep(col, ceiling(length(miss) / length(col)))
    col <- col[1:length(miss)]
  }
  # if col>miss we just ignore the extra colours

  # plot the missing data
  for (i in 1:length(miss)) {
    if (is.na(miss[i])) {
      image(is.na(df), axes = FALSE, col = c(NA, col[i]), add = (i > 1), ...)
    } else {
      image((df == miss[i]), axes = FALSE, col = c(NA, col[i]), add = (i > 1), ...)
    }
  }

  # add labels
  title(main = main)
  #  row labels are the variable names
  if (ncol(df) > 1) {
    axis(2, at = (0:(ncol(df) - 1)) / (ncol(df) - 1), labels = colnames(df), las = 2)
  } else {
    axis(2, at = 0.5, labels = colnames(df), las = 2)
  }
  # image uses xaxis range (0,1) - label some observations
  axis(1, at = seq(0, 1, by = 0.2), labels = round(seq(0, nrow(df), by = 0.2 * nrow(df))))
  # restore par options
  par(opar)
}
