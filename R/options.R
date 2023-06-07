.onLoad <- function(libname, pkgname) {

  # .auth <<- gargle::init_AuthState(
  #   package = "googleCloudVertexAIR",
  #   auth_active = TRUE
  #   #app = NULL,
  #   #api_key = NULL,
  #   #cred = NULL
  # )

  op <- options()
  op.googleCloudVertexAIR <- list(
    googleCloudVertexAIR.verbose = 3
  )
  toset <- !(names(op.googleCloudVertexAIR) %in% names(op))
  if (any(toset)) options(op.googleCloudVertexAIR[toset])

  invisible()

}

# see here for example:
# https://github.com/MarkEdmondson1234/googleAuthR/blob/master/R/options.R
