% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/endpoints.R
\name{gcva_delete_endpoint}
\alias{gcva_delete_endpoint}
\title{Deletes a Vertex AI Endpoint}
\usage{
gcva_delete_endpoint(
  projectId = gcva_project_get(),
  locationId = gcva_region_get(),
  displayName = NULL,
  endpoint
)
}
\description{
Deletes a Vertex AI Endpoint
}
\seealso{
Other Endpoints: 
\code{\link{gcva_create_endpoint}()},
\code{\link{gcva_deploy}()},
\code{\link{gcva_endpoint}()},
\code{\link{gcva_list_endpoints}()},
\code{\link{gcva_predict}()},
\code{\link{gcva_undeploy}()},
\code{\link{gcva_wait_for_model_deploy}()}
}
\concept{Endpoints}
