.onLoad <- function(libname, pkgname) {

  op <- options()
  op.googleCloudVertexAIR <- list(
    googleCloudVertexAIR.verbose = 3
  )
  toset <- !(names(op.googleCloudVertexAIR) %in% names(op))
  if (any(toset)) options(op.googleCloudVertexAIR[toset])

  invisible()

}
