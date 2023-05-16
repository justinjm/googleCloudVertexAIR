#' Generate Text from Text Prompts
#' https://cloud.google.com/vertex-ai/docs/generative-ai/text/test-text-prompts#generative-ai-test-text-prompt-python
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#' @param prompt A prompt is a natural language request submitted to a language model to receive a response back. Prompts can contain questions, instructions, contextual information, examples, and text for the model to complete or continue. (Don't add quotes around the prompt here.)
#' @param modelId
#' @param temperature The temperature is used for sampling during response generation, which occurs when topP and topK are applied. Temperature controls the degree of randomness in token selection. Lower temperatures are good for prompts that require a more deterministic and less open-ended or creative response, while higher temperatures can lead to more diverse or creative results. A temperature of 0 is deterministic, meaning that the highest probability response is always selected.
#' @param maxOutputTokens  Maximum number of tokens that can be generated in the response. A token is approximately four characters. 100 tokens correspond to roughly 60-80 words. Specify a lower value for shorter responses and a higher value for longer responses.
#' @param topP Top-P changes how the model selects tokens for output. Tokens are selected from the most (see top-K) to least probable until the sum of their probabilities equals the top-P value. For example, if tokens A, B, and C have a probability of 0.3, 0.2, and 0.1 and the top-P value is 0.5, then the model will select either A or B as the next token by using temperature and excludes C as a candidate.
#' @param topK Top-K changes how the model selects tokens for output. A top-K of 1 means the next selected token is the most probable among all tokens in the model's vocabulary (also called greedy decoding), while a top-K of 3 means that the next token is selected from among the three most probable tokens by using temperature. For each token selection step, the top-K tokens with the highest probabilities are sampled. Then tokens are further filtered based on top-P with the final token selected using temperature sampling. Specify a lower value for less random responses and a higher value for more random responses. The default top-K is 40.
#' @param rawResponse print raw response or only text response, default FALSE
#'
#' @return Prediction response from Vertex AI API
#'
#' @export
gcva_text_gen_predict <- function(projectId = gcva_project_get(),
                                  locationId = gcva_region_get(),
                                  prompt,
                                  modelId,
                                  temperature=0.2,
                                  maxOutputTokens=256,
                                  topP=0.8,
                                  topK=40,
                                  rawResponse=FALSE) {

  textPrompt <- structure(
    rmNullObs(
      list(
        instances = list(
          list(
            prompt = prompt
          )
        ),
        parameters = list(
          temperature = temperature,
          maxOutputTokens = maxOutputTokens,
          topK = topK,
          topP = topP
        )
      )), class = c("gcva_textPrompt", "list")
  )

  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/publishers/google/models/%s:predict",
                 locationId,
                 parent,
                 modelId)

  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f(the_body = textPrompt)

  if(rawResponse == FALSE) {
    # return pretty output, content only (default)
    printPretty <- function(text) {
      pretty_text <- paste("===================================================\n",
                           "        Response:       \n",
                           "===================================================\n",
                           text,
                           "\n===================================================\n")
      return(pretty_text)
    }

    printPretty(response$predictions$content)

  }  else if(rawResponse == TRUE) {
    # print raw result
    response
  }

}
