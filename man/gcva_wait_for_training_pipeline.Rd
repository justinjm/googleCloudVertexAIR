% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/training_pipeline_jobs.R
\name{gcva_wait_for_training_pipeline}
\alias{gcva_wait_for_training_pipeline}
\title{Wait for a training pipeline operation}
\usage{
gcva_wait_for_training_pipeline(
  projectId = gcva_project_get(),
  locationId = gcva_region_get(),
  trainingPipelineName,
  wait = 120
)
}
\arguments{
\item{locationId}{locationId of training pipeline}

\item{trainingPipelineName}{an object representing a model training pipeline}

\item{wait}{INTEGER number of seconds to wait between checks. Default is 2minutes}
}
\value{
trainingPipeline object
}
\description{
Wait for a training pipeline operation
}
