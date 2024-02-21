
# https://cloud.google.com/vertex-ai/docs/generative-ai/model-reference/gemini#sample_requests
gcva_gemini_text <- function(projectId = gcva_project_get(),
                             locationId = gcva_region_get(),
                             modelId=c("gemini-1.0-pro"),
                             stream=TRUE,
                             role=c("user"),
                             prompt,
                             data=NULL,
                             mimeType=NULL,
                             fileUri=NULL,
                             startOffsetSeconds=NULL,
                             startOffsetNanos=NULL,
                             endOffsetSeconds=NULL,
                             endOffsetNanos=NULL,
                             functionDeclarations=NULL,
                             harmCategory=c("HARM_CATEGORY_SEXUALLY_EXPLICIT",
                                            "HARM_CATEGORY_HATE_SPEECH",
                                            "HARM_CATEGORY_HARASSMENT",
                                            "HARM_CATEGORY_DANGEROUS_CONTENT"),
                             harmThreshold=c("BLOCK_NONE",
                                             "BLOCK_LOW_AND_ABOVE",
                                             "BLOCK_MED_AND_ABOVE",
                                             "BLOCK_ONLY_HIGH"),
                             candidateCount=NULL,
                             temperature = 0.5,
                             maxOutputTokens=NULL, #Mmax = 8192
                             topP = 1.0,
                             topK = 10,
                             stopSequences=NULL) {


  modelId <- match.arg(modelId)
  role <- match.arg(role)


  requestBody <- structure(
    rmNullObs(
      list(
        contents = list(

          role = role,

          parts = list(

            ## start one of text or fileData
            text = prompt

            ## OR
            # ,inlineData = list(
            #   mimeType = mimeType,
            #   data = data
            # ),
            ## OR
            # fileData = list(
            #   mimeType = mimeType,
            #   fileUri = fileUri
            # ),
            ## end one of text or fileData

            # videoMetadata = list(
            #   startOffset = list(
            #     seconds = startOffsetSeconds,
            #     nanos = startOffsetNanos
            #   ),
            #   endOffset = list(
            #     seconds = endOffsetSeconds,
            #     nanos = endOffsetNanos
            #   )
            # )

          )

        ),
        # tools = list(
        #   list(
        #     functionDeclarations = functionDeclarations
        #   )
        # ),
        safetySettings = list(
          # list(
          category = match.arg(harmCategory),
          threshold = match.arg(harmThreshold)
          # )
        ),
        generationConfig = list(
          temperature = temperature,
          topP = topP,
          topK = topK,
          candidateCount = candidateCount,
          maxOutputTokens = maxOutputTokens,
          stopSequences = stopSequences
        )
      )
    ), class = c("gcva_gemini", "list")
  )

  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)



  # POST https://{REGION}-aiplatform.googleapis.com/v1/projects/{PROJECT_ID}/locations/{REGION}/publishers/google/models/gemini-pro:streamGenerateContent
  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/publishers/google/models/%s:streamGenerateContent",
                 locationId,
                 parent,
                 modelId)



  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f(the_body = requestBody)

  # save candidates object from response to make parsing easier
  candidates <- response$candidates

  # Define a function to safely extract the text
  extract_text <- function(candidate) {
    if (!is.null(candidate[["content"]]) &&
        !is.null(candidate[["content"]][["parts"]]) &&
        length(candidate[["content"]][["parts"]]) >= 1 &&
        !is.null(candidate[["content"]][["parts"]][[1]][["text"]])) {
      return(candidate[["content"]][["parts"]][[1]][["text"]])
      # return(gsub("\n", " ", candidate[["content"]][["parts"]][[1]][["text"]]))
    } else {
      return(NA)  # Return NA if the path does not exist or is NULL
    }
  }

  # Use lapply to apply the function to each element of candidates
  texts <- lapply(candidates, extract_text)

  # Concatenate all texts into a single string for output
  all_texts <- paste(unlist(texts), collapse = " ")

  all_texts

}

## TODO: add streaming processing? outline below
# library(httr)
# library(jsonlite)
#
# # Define the API endpoint
# url <- "http://api.example.com/stream"
#
# # Function to process each JSON object
# process_data <- function(json_data) {
#   # Assuming json_data is a list similar to rg_candidates
#   # Extract and concatenate text as before
#   texts <- lapply(json_data, function(candidate) {
#     if (!is.null(candidate[["content"]]) &&
#         !is.null(candidate[["content"]][["parts"]]) &&
#         length(candidate[["content"]][["parts"]]) >= 1 &&
#         !is.null(candidate[["content"]][["parts"]][[1]][["text"]])) {
#       return(candidate[["content"]][["parts"]][[1]][["text"]])
#     } else {
#       return(NA)  # Assign NA if the path does not exist or is NULL
#     }
#   })
#
#   # Concatenate all texts into a single string for Markdown
#   all_texts <- paste(unlist(texts), collapse = "\n\n")
#   return(all_texts)
# }
#
# # Set up a streaming connection
# response <- GET(url, stream_with = function(con) {
#   while(length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
#     # Parse the JSON data
#     data <- fromJSON(line)
#
#     # Process the data
#     result <- process_data(data)
#     print(result)  # Or do other processing as needed
#   }
# })
#
# # Close the connection
# close(response)
