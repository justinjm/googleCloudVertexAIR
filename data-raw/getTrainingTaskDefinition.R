library(withr)
library(glue)

sh <- function(cmd, args = c(), intern = FALSE) {
  with_path(path.expand("~/google-cloud-sdk/bin/"), {
    if (is.null(args)) {
      cmd <- glue(cmd)
      s <- strsplit(cmd, " ")[[1]]
      cmd <- s[1]
      args <- s[2:length(s)]
    }
    ret <- system2(cmd, args, stdout = TRUE, stderr = TRUE)
    if ("errmsg" %in% attributes(attributes(ret))$names) cat(attr(ret, "errmsg"), "\n")
    if (intern) return(ret) else cat(paste(ret, collapse = "\n"))
  })}


library(googleAuthR)
library(googleCloudVertexAIR)

options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform")

gar_auth_service(json_file = Sys.getenv("GAR_SERVICE_JSON"))
sh("gcloud config set project {gcva_project_get()}")

sh("pwd")

sh("gsutil cp -r gs://google-cloud-aiplatform/schema/trainingjob/definition/ ./data-raw")
