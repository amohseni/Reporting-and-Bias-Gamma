# REPORTING AND BIAS
# by Aydin Mohseni

# Load packages
library(ggplot2)
library(ggthemes)
library(reshape2)

# Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Determine parameter values to test
SampleSpace <- seq(from = -10, to = 10, by = 0.1) # The sample space of possible states true state of the world
Runs <- 1000 # The number of runs of the simulation
TrueStateSD <- 1 # The true mean of the world
TrueStateMeanVEC <- seq(from = -1, to = 1, by = 1)
InitialBeliefVEC <- seq(from = -1, to = 1, by = 1)
BiasStrengthVEC <- c(10 ^ -1, 1)
HyperboleVEC <- seq(from = 1, to = 2, by = 1)
ExtermityBiasVEC <- seq(from = 0, to = 1, by = 1)
FairAndBalancedVEC <- c(0, 1)
QuantityOfEvidence <- 3

# Calculate the number of distinct parameter settings required
TotalNumberOfCases <-
  length(TrueStateMeanVEC) * length(InitialBeliefVEC) * length(BiasStrengthVEC) * length(ExtermityBiasVEC) * length(HyperboleVEC) * length(FairAndBalancedVEC)
SampleSize <- 10 ^ (QuantityOfEvidence + 2)

# Create the data frame in which to save the data
Df <- data.frame(matrix(
  data = NA,
  ncol = 12,
  nrow = TotalNumberOfCases,
  byrow = TRUE
))
colnames(Df) <-
  c(
    "TrueMean",
    "PriorMean",
    "BiasStrength",
    "Hyperbole",
    "ExtermityBias",
    "FaB",
    "BeliefMean",
    "BeliefVar",
    "BeliefMeanError",
    "BeliefVarError",
    "BeliefMeanMSE",
    "BeliefVarMSE"
  )
Df[, 1] <- rep(TrueStateMeanVEC,
               times = 1,
               each = TotalNumberOfCases / length(TrueStateMeanVEC))
Df[, 2] <- rep(InitialBeliefVEC,
               times = length(TrueStateMeanVEC),
               each = TotalNumberOfCases / (length(TrueStateMeanVEC) * length(InitialBeliefVEC)))
Df[, 3] <- rep(
  BiasStrengthVEC,
  times = length(TrueStateMeanVEC) * length(InitialBeliefVEC),
  each = length(HyperboleVEC) * length(ExtermityBiasVEC) * length(FairAndBalancedVEC)
)
Df[, 4] <- rep(
  HyperboleVEC,
  times = TotalNumberOfCases / (
    length(HyperboleVEC) * length(ExtermityBiasVEC) * length(FairAndBalancedVEC)
  ),
  each = length(ExtermityBiasVEC) * length(FairAndBalancedVEC)
)
Df[, 5] <- rep(
  ExtermityBiasVEC,
  times = TotalNumberOfCases / (length(ExtermityBiasVEC) * length(FairAndBalancedVEC)),
  each = length(FairAndBalancedVEC)
)
Df[, 6] <- rep(FairAndBalancedVEC,
               times = TotalNumberOfCases / length(FairAndBalancedVEC),
               each = 1)

# Set up the for loops for the PARAMETER SWEEP over the total number of cases
for (i in 1:TotalNumberOfCases) {
  # Set model parameters
  TrueStateMean <- Df[i, 1]
  InitialBelief <- Df[i, 2]
  BiasStrength <- Df[i, 3]
  Hyperbole <- Df[i, 4]
  ExtermityBias <- Df[i, 5]
  FairAndBalanced <- Df[i, 6]
  
  ### Create the OBJECTIVE distribution of events for the world
  WorldDistribution <-
    sapply(SampleSpace, dnorm, mean = TrueStateMean, sd = TrueStateSD)
  NormalizingFactor <- sum(WorldDistribution)
  
  ### Determine the REPORTED distribution of events by news media
  NewsMean <- TrueStateMean
  NewsSD <- TrueStateSD
  # Apply hyperbole distortion
  NewsDistribution <-
    sapply(SampleSpace,
           dnorm,
           mean = Hyperbole * NewsMean,
           sd = Hyperbole * NewsSD)
  # Apply extremity-bias distortion
  NewsDistribution[which(-ExtermityBias < SampleSpace &
                           SampleSpace < ExtermityBias)] <- 0
  # Apply fair-and-balanced distortion
  if (FairAndBalanced == 1) {
    NewsDistribution[which(SampleSpace < 0)] <-
      NewsDistribution[which(SampleSpace < 0)] * (sum(NewsDistribution[which(SampleSpace > 0)]) / (sum(NewsDistribution[which(SampleSpace < 0)])))
  }
  # Re-normalize distribution
  NewsDistribution <-
    NewsDistribution * (NormalizingFactor / sum(NewsDistribution)) # renormalize
  NewsData <-
    sample(SampleSpace,
           size = 10000,
           prob = NewsDistribution,
           replace = TRUE)
  # Output summary statistics
  ReportedMean <- mean(NewsData)
  ReportedSD <- sd(NewsData)
  
  # The Bayesian update function which describes how agents learn from news reports
  # It take reports and prior beliefs about the mean and SD as inputs,
  # and has posterior beliefs about the population mean and SD as outputs.
  update <- function(mu_0, n_0, alpha, beta, report) {
    # Get data x_i | µ, τ ∼ N (µ, τ ) i.i.d.
    x_bar <- report
    # Compute posterior distiributions parameters
    # First, precision τ | x ∼ Ga (α + n / 2, β + (Σ (x_i − x_bar)^2) / 2 + (x_bar − µ_0)^2 * n * n0 / 2(n + n0))
    # where now n = 1
    alpha_post <- alpha + 1 / 2
    beta_post <-
      beta + (x_bar - mu_0) ^ 2 * n_0 / (2 * (1 + n_0))
    # Convert the gamma hyperparameters into precision & variance for further calculations
    tau_post <- alpha_post / beta_post
    sd_post <- tau_post ^ -(1 / 2)
    # Next, mean µ | x, τ ∼ N(x_bar * nτ / (nτ + n0τ) x_bar +  µ_0 * n_0τ / (nτ + n0τ) , τ(nτ + n_0) )
    mean_post <-
      (x_bar * tau_post) / (tau_post + n_0 * tau_post) + (mu_0 * n_0 * tau_post) / (tau_post + n_0 * tau_post)
    # Outupt the posterior parameters (to function as the new prior)
    return(c(mean_post, alpha_post, beta_post))
  }
  
  # Create a vector in which to save the outcomes of each run of the learning process
  meanPerceptionVec <- rep(NA, Runs)
  sdPerceptionVec <- rep(NA, Runs)
  
  # Run the learning process 'Runs'-many times
  for (r in 1:Runs) {
    # Sample data points from the News Distribution to be observed by the agent
    SampleOfNewsReports <-
      sample(SampleSpace,
             SampleSize,
             prob = NewsDistribution,
             replace = T)
    
    # Now, we determine the SUBJECTIVE distribution of events perceived by an individual
    # First, set the initial values for the agent's beliefs.
    # The initial precision of the agent's prior of  τ ∼ Ga(α, β)
    priorSD <- TrueStateSD
    alpha <- 2
    beta <- 1
    # The initial mean of the agent's prior µ ~ N(µ_0, τn_0)
    priorMean <- InitialBelief
    
    # And create a vector in which to store the means and SDs of the belief state
    # Create a vector in which to store the means and SDs of the belief state
    priorMeanVector <- rep(NA, SampleSize)
    priorAlphaVector <- rep(NA, SampleSize)
    priorBetaVector <- rep(NA, SampleSize)
    priorSDVector <- rep(NA, SampleSize)
    acceptRejectVector <- rep(NA, SampleSize)
    
    # Given our updating function, individiual learning now proceeds as follows:
    # For each news report,
    # (1) the agent decides whether to accept or reject it
    # which is determined by how close the report is to her current belief;
    # (2) if she rejects the report, then her view remains unchanged,
    # and if she accepts the report, then she updates her beliefs via Bayes rule;
    # (3) the process begins again with a new report and her new beliefs.
    
    # But first, set the initial state of the counters
    # for the number of pieces of evidence considered
    # and the number of pieces of evdience update upon.
    reportsConsidered <- 0
    reportsUpdatedOn <- 0
    
    # Now, keep exposing the agent to evidence
    # until she has updated the required number of times
    while (reportsUpdatedOn < (10 ^ QuantityOfEvidence) &
           reportsConsidered < SampleSize) {
      # Increment the count for pieces of evidence considered
      reportsConsidered <- reportsConsidered + 1
      # Determine the index of the current data point to consider
      obs <- reportsConsidered
      
      # Consider the news report
      report <- SampleOfNewsReports[obs]
      
      # First, check if strength of bias = 0.
      # If so, the agent simply accepts the data.
      if (BiasStrength == 0) {
        # Record that the report was accepted,
        acceptRejectVector[obs] <- 1
        
        # Increment the count for pieces of evidence updated upon
        reportsUpdatedOn <- reportsUpdatedOn + 1
        
        # And update the belief state via Bayes' rule.
        updatedParams <-
          update(priorMean, reportsUpdatedOn, alpha, beta, report)
        
      } else {
        # If there is confirmation bias, then
        # (1) Decide whether confirmation bias allows the agent to update.
        
        # If confirmation bias makes it so that the report is rejected,
        # Then simply leave the agent belief state as is.
        if (abs(priorMean - report) > abs(priorMean - rnorm(1, mean = priorMean, BiasStrength ^ -1))) {
          # Record that the report was rejected
          acceptRejectVector[obs] <- 0
          
          # And leave belief state as is
          #updatedParams <- c(priorMean, priorSD)
          updatedParams <- c(priorMean, alpha, beta)
          
        } else {
          # If conformation bias does not make it so that the report is rejected,
          # record that the report was accpeted.
          acceptRejectVector[obs] <- 1
          
          # Increment the count for pieces of evidence updated upon.
          reportsUpdatedOn <- reportsUpdatedOn + 1
          
          # Then update the belief state via Bayes' rule.
          updatedParams <-
            update(priorMean, reportsUpdatedOn, alpha, beta, report)
        }
      }
      # Update the belief means and SD for the next round of learning.
      priorMean <- updatedParams[1]
      alpha <- updatedParams[2]
      beta <- updatedParams[3]
      
      # Store these in their respective vectors.
      priorMeanVector[obs] <- priorMean
      priorAlphaVector[obs] <- alpha
      priorBetaVector[obs] <- beta
      priorSDVector[obs] <- (alpha / beta) ^ -(1 / 2)
    }
    
    # Determine which reports were accepted,
    # and use these to calculate the Bayesian estimator of the SD of the accpeted reports
    whichReportsAccepted <- which(acceptRejectVector %in% c(1))
    
    # Record the final resulting posterior mean and SD for the run
    # and save these to their respective data vectors
    meanPerceptionVec[r] <- priorMeanVector[reportsConsidered]
    sdPerceptionVec[r] <- priorSDVector[reportsConsidered]
  }
  # Save the average across all runs for the case into the data frame
  Df[i, 7] <- mean(meanPerceptionVec)
  Df[i, 8] <- mean(sdPerceptionVec)
  # Print the progress of the algorithm
  paste("Run number ", i, " of ", TotalNumberOfCases, " completed.", sep = "")
}

# Calculate the errors in belief given the true states and agent beliefs
Df$BeliefMeanError <- (Df$BeliefMean - Df$TrueMean)
Df$BeliefVarError <- (Df$BeliefVar - TrueStateSD)
Df$BeliefMeanMSE <- Df$BeliefMeanError ^ 2
Df$BeliefVarMSE <- Df$BeliefVarError ^ 2
Df <- round(Df, digits = 3)

# Print all the results as a CSV file
write.csv(Df, file = "AllData.csv", row.names = FALSE)
