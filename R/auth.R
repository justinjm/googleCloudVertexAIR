# Package-level AuthState object (gargle)
# Initialised when the package is loaded (see options.R / .onLoad)
.auth <- gargle::init_AuthState(
  package     = "googleCloudVertexAIR",
  auth_active = TRUE
)

#' Authenticate with Google Cloud Vertex AI
#'
#' Sets credentials for all subsequent calls to the Vertex AI API.
#' Follows the same pattern used by \pkg{bigrquery} and other tidyverse
#' packages that rely on \pkg{gargle} for authentication.
#'
#' @param email   Google account email address.  When \code{NULL} (default)
#'   gargle will choose the most recently-used cached token, or prompt
#'   interactively.
#' @param path    Path to a JSON file that holds service account credentials.
#'   Set the environment variable \code{GCVA_AUTH_FILE} as a convenient
#'   alternative to passing this argument every time.
#' @param scopes  OAuth 2.0 scopes.  Defaults to the cloud-platform scope
#'   which covers all Google Cloud APIs.
#' @param cache   Path to an OAuth token cache directory, or \code{FALSE} to
#'   disable caching, or \code{TRUE} to use the default gargle cache location.
#' @param use_oob Logical.  Use out-of-band (OOB) auth flow instead of a local
#'   server?  Defaults to the gargle global option.
#'
#' @return Invisibly, the credential object.
#' @export
gcva_auth <- function(
    email   = gargle::gargle_oauth_email(),
    path    = NULL,
    scopes  = "https://www.googleapis.com/auth/cloud-platform",
    cache   = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default()
) {
  # Honour the GCVA_AUTH_FILE env var as a convenience shortcut
  if (is.null(path) && nzchar(Sys.getenv("GCVA_AUTH_FILE"))) {
    path <- Sys.getenv("GCVA_AUTH_FILE")
  }

  cred <- gargle::token_fetch(
    scopes  = scopes,
    email   = email,
    path    = path,
    package = "googleCloudVertexAIR",
    cache   = cache,
    use_oob = use_oob
  )

  if (!inherits(cred, "Token2.0")) {
    stop(
      "Unable to obtain Google credentials.\n",
      "Did you run `gcva_auth(path = '/path/to/service-account.json')` ",
      "or set the GCVA_AUTH_FILE environment variable?",
      call. = FALSE
    )
  }

  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)
  invisible(cred)
}

#' Check whether a token is currently available
#'
#' @return Logical scalar.
#' @export
gcva_has_token <- function() {
  isTRUE(.auth$auth_active) && !is.null(.auth$cred)
}

# ---------------------------------------------------------------------------
# Internal helpers (not exported)
# ---------------------------------------------------------------------------

#' Retrieve the current token as an httr config object
#'
#' If no token exists yet, \code{gcva_auth()} is called automatically.
#'
#' @keywords internal
gcva_token <- function() {
  if (!gcva_has_token()) {
    gcva_auth()
  }
  httr::config(token = .auth$cred)
}

#' Execute an authenticated Vertex AI REST request
#'
#' This internal helper replaces the \code{googleAuthR::gar_api_generator()}
#' pattern used throughout the package.  It obtains credentials via
#' \code{gcva_token()}, makes the HTTP request using \pkg{httr}, checks for
#' HTTP errors, parses the JSON response, and applies an optional parse
#' function.
#'
#' @param url      Full request URL (character scalar).
#' @param method   HTTP verb: \code{"GET"}, \code{"POST"}, \code{"DELETE"},
#'   etc.  Defaults to \code{"GET"}.
#' @param body     R object to serialise as the JSON request body, or
#'   \code{NULL} for requests without a body.
#' @param parse_fn Function applied to the parsed response list before
#'   returning.  Defaults to \code{identity}.
#'
#' @return The value returned by \code{parse_fn}.
#' @keywords internal
gcva_api_request <- function(url,
                              method   = "GET",
                              body     = NULL,
                              parse_fn = identity) {
  token_cfg <- gcva_token()

  if (is.null(body)) {
    resp <- httr::VERB(method, url, token_cfg)
  } else {
    resp <- httr::VERB(
      method, url, token_cfg,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::content_type_json()
    )
  }

  httr::stop_for_status(resp)
  parsed <- httr::content(resp, as = "parsed", type = "application/json")
  parse_fn(parsed)
}
