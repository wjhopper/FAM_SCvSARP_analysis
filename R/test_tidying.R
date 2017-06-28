test_tidying <- function(data) {
  
  # Quit if stringdist package isn't loaded, we need it
  stopifnot(all(c("stringdist") %in% (.packages()) ))

  # Convert the recalled column to logical if it isn't already.
  if (!is.logical(data$recalled)) {
    data$recalled <- as.logical(data$recalled)
  }
  
  # If the latency time (i.e., the timestamp for when they pressed remember/dont remember)
  # is missing, they didn't decide in time. This is useful for filtering the data later on
  data$decided <- !is.na(data$latency)
  
  # The advance timestamps for trials where no free response was given are 0, because of the way the
  # MATLAB code for running the experiment was written. NA is a better value to indicate this timestamo
  # was not recorded, so replace the 0 value on non-recalled trials with NA.
  if (all(data$advance[!data$recalled] == 0)) {
      data$advance[!data$recalled] <- NA_real_
  } else {
    stop("An 'advance' timestamp is non-zero where the 'recalled' variable is 0. This should not be possible.")
  }

  #### Scoring the tests for accuracy ####
  
  # hard_match indicates whether the free response given is an exact match to the target.
  # If the response does not exactly match, or the participant indcated they could not recall the target,
  # hard_match is FALSE. If no decision was reached by the 10s. deadline, hard_match is NA (missing).
  
  data$hard_match[data$recalled] <- data$target[data$recalled] == data$response[data$recalled]
  data$hard_match[!data$recalled & data$decided] <- FALSE
  
  # fuzzy_match indicates whether the free response given is an inexact match (similar) to the target,
  # as determined by having a sufficiently low jaro-winkler string distance.
  # If the response is above the string distance criterion, or the participant indcated they could
  # not recall the target,fuzzy_match is FALSE. If no decision was reached by the 10s. deadline,
  # fuzzy_match is NA (missing).

  data$fuzzy_match[data$recalled] <- mapply(stringdist::ain,
                                            data$target[data$recalled],
                                            data$response[data$recalled],
                                            MoreArgs = list(method = "jw", maxDist = 0.1))
  data$fuzzy_match[!data$recalled & data$decided] <- FALSE
  
  #### Scoring the tests for RT ####

  # Create RT variable as the difference between latency and onset variable
  # Onset is when the cue is displayed
  # latency is when the memory decision is given by pressing 'z' or 'm' key
  data$RT <- data$latency - data$onset

  data$pre_typing_lag <- data$fp - data$latency
  data$typing_time <- data$lp - data$fp
  data$post_typing_lag <- data$advance - data$lp
  
  data <- data[-match(c("onset", "latency", "fp", "lp", "advance"), names(data))]
  data
}