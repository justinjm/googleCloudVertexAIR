% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/large_language_models.R
\name{gcva_chat_predict}
\alias{gcva_chat_predict}
\title{TODO
TODO
Chat Prompt}
\usage{
gcva_chat_predict(
  projectId = gcva_project_get(),
  locationId = gcva_region_get(),
  context,
  examples,
  messages,
  modelId,
  temperature = 0.2,
  maxOutputTokens = 256,
  topP = 0.95,
  topK = 40
)
}
\arguments{
\item{projectId}{GCP project id}

\item{locationId}{location of GCP resources}

\item{temperature}{The temperature is used for sampling during response generation, which occurs when topP and topK are applied. Temperature controls the degree of randomness in token selection. Lower temperatures are good for prompts that require a more deterministic and less open-ended or creative response, while higher temperatures can lead to more diverse or creative results. A temperature of 0 is deterministic, meaning that the highest probability response is always selected.}

\item{maxOutputTokens}{Maximum number of tokens that can be generated in the response. A token is approximately four characters. 100 tokens correspond to roughly 60-80 words. Specify a lower value for shorter responses and a higher value for longer responses.}

\item{topP}{Top-P changes how the model selects tokens for output. Tokens are selected from the most (see top-K) to least probable until the sum of their probabilities equals the top-P value. For example, if tokens A, B, and C have a probability of 0.3, 0.2, and 0.1 and the top-P value is 0.5, then the model will select either A or B as the next token by using temperature and excludes C as a candidate.}

\item{topK}{Top-K changes how the model selects tokens for output. A top-K of 1 means the next selected token is the most probable among all tokens in the model's vocabulary (also called greedy decoding), while a top-K of 3 means that the next token is selected from among the three most probable tokens by using temperature. For each token selection step, the top-K tokens with the highest probabilities are sampled. Then tokens are further filtered based on top-P with the final token selected using temperature sampling. Specify a lower value for less random responses and a higher value for more random responses. The default top-K is 40.}
}
\description{
TODO
TODO
Chat Prompt
}
