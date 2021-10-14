#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

#' if argument is NULL, no line output
#'
#' @keywords internal
#' @noRd
cat0 <- function(prefix = "", x){
  if(!is.null(x)){
    cat(prefix, x, "\n")
  }
}

#' base R safe rbind
#'
#' Send in a list of data.fames with different column names
#'
#' Taken from googleCloudStorageR by Mark Edmondson
#'
#' @return one data.frame
#' a safe rbind for variable length columns
#' @noRd
my_reduce_rbind <- function(x){
  classes <- lapply(x, inherits, what = "data.frame")
  stopifnot(all(unlist(classes)))

  # all possible names
  df_names <- Reduce(union, lapply(x, names))

  df_same_names <- lapply(x, function(y){
    missing_names <- setdiff(df_names,names(y))
    num_col <- length(missing_names)
    if(num_col > 0){
      missing_cols <- vapply(missing_names, function(i) NA, NA, USE.NAMES = TRUE)

      new_df <- data.frame(matrix(missing_cols, ncol = num_col))
      names(new_df) <- names(missing_cols)
      y <- cbind(y, new_df, row.names = NULL)
    }

    y[, df_names]

  })

  Reduce(rbind, df_same_names)
}

#' Javascript time to R time
#'
#' Taken from googleCloudStorageR by Mark Edmondson
#'
#' @keywords internal
#' @noRd
js_to_posix <- function(x){
  as.POSIXct(as.numeric(x) / 1000, origin = "1970-01-01")
}

#' Timestamp to R date
#'
#' Taken from googleCloudStorageR by Mark Edmondson
#'
#' @keywords internal
#' @noRd
timestamp_to_r <- function(t){
  as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%S")
}

#' Custom message log level
#'
#' @param ... The message(s)
#' @param level The severity
#'
#' @details 0 = everything, 1 = debug, 2=normal, 3=important
#' @keywords internal
#' @noRd
myMessage <- function(..., level = 2){

  compare_level <- getOption("googleAuthR.verbose")

  if(level >= compare_level){
    message(Sys.time() ,"> ", ...)
  }

}
