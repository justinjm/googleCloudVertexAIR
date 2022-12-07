library(googleAuthR)
library(bigQueryR)
library(glue)

options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform")

gar_auth_service(json_file = Sys.getenv("GAR_SERVICE_JSON"))

projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")

bqr_auth(json_file = Sys.getenv("GAR_SERVICE_JSON"))

bqr_list_projects()
bqr_list_datasets(projectId)

# Steps
# create dataset if does not exist
# create external table -  copy of training dataset in BQ (for training job and batch)
# create create permenant table -  batch of examples (25) of random for batch prediction


# Create dataset https://cloud.google.com/bigquery/docs/datasets#sql
query_training_data <- glue("
#standardSQL
CREATE SCHEMA `{projectId}.california_housing`
OPTIONS (
  location = 'us'
)
")

# Create external table: https://cloud.google.com/bigquery/docs/external-data-cloud-storage#sql
#standardSQL
# CREATE OR REPLACE EXTERNAL TABLE california-housing.training_data
# OPTIONS(
#   format = 'CSV',
#   uris = ['gs://cloud-samples-data/ai-platform-unified/datasets/tabular/california-housing-tabular-regression.csv']
# )


# Create table
# #standardSQL
# # CREATE OR REPLACE TABLE california-housing.batch
# SELECT * FROM `california-housing.training_data`

# FARM_FINGERPRINT(ROW_NUMBER())
# https://cloud.google.com/bigquery/docs/reference/standard-sql/numbering_functions#row_number
# https://stackoverflow.com/questions/45444201/using-farm-fingerprint-for-google-big-query
# https://stackoverflow.com/questions/46019624/how-to-do-repeatable-sampling-in-bigquery-standard-sql
