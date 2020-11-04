# NEWS, CONFIRMATION BIAS, AND BELIEF POLARIZATION
# << SERVER >>
# by Aydin Mohseni


# Load packages
library(shiny)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(Bolstad)

# Create the sample space of possible states true state of the world
x <- seq(from = -10,
         to = 10,
         by = 0.1) 

# Define server logic
shinyServer(function(input, output, session) {
  computeDynamics <- reactive({
    
    # Re-run the simulation when 'RERUN SIMULATION' button is pressed
    simulationResetVariable <- input$RerunSimulation
    
    # Get the simulation parameter values from the GUI sliders:
    # 1. The true distribution
    # The mean of the true distribution
    TrueStateMean <- as.numeric(input$trueStateMean) 
    # The standard deviation of the true distribution
    TrueStateSD <- as.numeric(input$trueStateSD) 
    # A sampling distributon for the true distribution
    WorldDistribution <- 
      sapply(x, dnorm, mean = TrueStateMean, sd = TrueStateSD)
    # The total mass of the sampling distribution to be used in (re-)normalization
    NormalizingFactor <- sum(WorldDistribution) 
    
    # 3. The reporting practices as play
    # The degree of hypoerbole in reports
    Hyperbole <- as.numeric(input$hyperbole)
    
    # Next, create the distribution of events for the appearance portrayed by news media
    # Apply the hyperbole distortion
    NewsDistribution <-
      sapply(x,
             dnorm,
             mean = Hyperbole * TrueStateMean,
             sd = Hyperbole * TrueStateSD)
    # Apply the extremity bias distortion
    CherryPicking <- as.numeric(input$cherryPicking)
    NewsDistribution[which(-CherryPicking < x &
                             x < CherryPicking)] <- 0
    # Apply the fair-and-balanced distortion
    if (as.numeric(input$fairAndBalanced) == 1) {
      NewsDistribution[which(x < 0)] <-
        NewsDistribution[which(x < 0)] * (sum(NewsDistribution[which(x > 0)]) / (sum(NewsDistribution[which(x < 0)])))
    }
    # Re-normalize distribution
    NewsDistribution <-
      NewsDistribution * (NormalizingFactor / sum(NewsDistribution)) 
    NewsData <-
      sample(x,
             size = 10000,
             prob = NewsDistribution,
             replace = TRUE)
    # Output summary statistics
    ReportedMean <- mean(NewsData)
    ReportedSD <- sd(NewsData)
    
    # Create the prior & posterior distributions of beliefs of an individual
    IndividualPrior  <-
      sapply(x, dnorm, mean = InitialBelief, sd = TrueStateSD)
    # Update the number of reports observed by individuals
    quantityOfEvidence <- as.numeric(input$quantityOfEvidence)
    
    # Sample (10^q + k) reportss from the News Distribution
    SampleOfNewsReports <-
      sample(x,
             10 ^ (quantityOfEvidence + 2),
             prob = NewsDistribution,
             replace = T)
    
    # The Bayesian update function which describes how agents learn from news reports
    # It take reports and prior beliefs about the mean and SD as inputs,
    # and has posterior beliefs aobut the population mean and SD as outputs.
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
    
    # Create a vector in which to store the means and SDs of the belief state
    priorMeanVector <- rep(0, length(SampleOfNewsReports))
    priorAlphaVector <- rep(0, length(SampleOfNewsReports))
    priorBetaVector <- rep(0, length(SampleOfNewsReports))
    priorSDVector <- rep(0, length(SampleOfNewsReports))
    acceptRejectVector <- rep(0, length(SampleOfNewsReports))
    
    # Initialize progress loader
    # withProgress(message = 'Computing:', value = 0, {
    
    # Invdividiual learning now proceeds as follows.
    # For each news report, the agent (1) decides whether to accept or reject it
    # which is determined by how close it is to her current belief.
    # (2) If she rejects the report, then her view remains unchanged.
    # If she accepts the report, then she updates her beliefs via Bayes rule.
    # (3) The process begins again with a new report and her new beliefs.
    
    # But first, set the initial state of the counters 
    # for the number of pieces of evidence considered
    # and the number of pieces of evdience update upon.
    PiecesOfEvidenceConsidered <- 0
    PiecesOfEvidenceUpdatedOn <- 0
    
    # Now, keep exposing the agent to evidence 
    # until she has updated the required number of times
    while (PiecesOfEvidenceUpdatedOn < 10 ^ quantityOfEvidence &
           PiecesOfEvidenceConsidered < length(SampleOfNewsReports)) {
      
      # Set agent's initial beliefs
      if (PiecesOfEvidenceUpdatedOn == 0) {
        # 2. The agent's bias & prior beliefs
        # The strength of confirmation bias of agents
        StrengthOfBias <- as.numeric(input$strengthOfBias) 
        # The initial position of confirmation of agents
        InitialBelief <- as.numeric(input$initialBelief) 
        # The initial mean of the prior of agents µ ~ N(µ_0, τn_0)
        priorMean <- InitialBelief
        n_0 <- 1
        # mu_0 <- 1
        # The precision of the prior of agents τ ∼ Ga(α, β)
        alpha <- 2
        beta <- 1
      }
      
      # Increment the count for pieces of evidence considered
      PiecesOfEvidenceConsidered <- PiecesOfEvidenceConsidered + 1
      # Determine the index of the current data point to consider
      i <- PiecesOfEvidenceConsidered
      # Consider the news report
      report <- SampleOfNewsReports[i]
      # First, check if strenght of bias = 0.
      # If so, simply accepted the data.
      if (StrengthOfBias == 0) {
        # record that the report was accepted,
        acceptRejectVector[i] <- 1
        # Increment the count for pieces of evidence updated upon
        PiecesOfEvidenceUpdatedOn <- PiecesOfEvidenceUpdatedOn + 1
        # and update the belief state via Bayes' rule.
        updatedParams <-
          update(priorMean, PiecesOfEvidenceUpdatedOn, alpha, beta, report)
      } else {
        # And (1) Decide whether confirmation bias will allow you to update
        # If confirmation bias makes it so that the report is rejected,
        # Then simply leave the belief state as is
        if (abs(priorMean - report) > abs(priorMean - rnorm(1, mean = priorMean, StrengthOfBias ^ -1))) {
          # Record that the report was rejected
          acceptRejectVector[i] <- 0
          # Leave belief state as is
          updatedParams <- c(priorMean, alpha, beta)
        } else {
          # If conformation bias does not make it so that the report is rejected,
          # record that the report was accepted,
          acceptRejectVector[i] <- 1
          # Increment the count for pieces of evidence updated upon
          PiecesOfEvidenceUpdatedOn <- PiecesOfEvidenceUpdatedOn + 1
          # Then update the belief state via Bayes' rule.
          updatedParams <- update(priorMean, PiecesOfEvidenceUpdatedOn, alpha, beta, report)
        }
      }
      # Update the belief means and SD for the next round of learning
      priorMean <- updatedParams[1]
      alpha <- updatedParams[2]
      beta <- updatedParams[3]
      # Store these in their respective vectors
      priorMeanVector[i] <- priorMean
      priorAlphaVector[i] <- alpha
      priorBetaVector[i] <- beta
      priorSDVector[i] <- (alpha / beta) ^ -(1 / 2)
      
    }
    
    # Determine which reports were accepted with whcih to calculate the Bayesian estimator of the SD of the accpeted reports
    whichReportsAccepted <- which(acceptRejectVector %in% c(1))
    
    # Record the posterior mean and SD of individual belief
    meanPerception <- priorMeanVector[PiecesOfEvidenceConsidered]
    sdPerception <- priorSDVector[PiecesOfEvidenceConsidered]
    # And produce the normal distribution corresponding to that belief
    IndividualPerception <-
      sapply(x, dnorm, mean = meanPerception, sd = sdPerception)
    IndividualPerceptionParam <- IndividualPerception
    
    # OUTPUT the data for the plots
    h <-
      list(
        WorldDistribution,
        NewsDistribution,
        IndividualPrior ,
        IndividualPerception,
        IndividualPerceptionParam,
        c(ReportedMean, ReportedSD),
        c(meanPerception, sdPerception)
      )
    return(h)
  })
  
  # PLOT 1: State of the world distribution
  output$trueStatePlotOutput <- renderPlot({
    # Import computed distribution
    World <- computeDynamics()[[1]]
    # Format and label the imported data
    WorldPlot <- melt(data.frame(x, World), id.vars = 'x')
    colnames(WorldPlot) <-
      c("Evidence",  "Distribution", "Probability")
    # Create the ggplot
    X <- ggplot(WorldPlot) +
      geom_area(
        data = WorldPlot,
        size = 1,
        aes(
          x = Evidence,
          y = Probability,
          fill = Distribution,
          color = Distribution
        ),
        alpha = 0.5
      ) +
      theme_minimal() +
      ggtitle("True Distribution of Evidence") +
      labs(x = "Evidence", y = "Objective Frequency") +
      scale_x_continuous(limits = c(-10, 10)) +
      scale_y_continuous(limits = c(0, 0.75)) +
      scale_fill_manual(values = c("orange2")) +
      scale_color_manual(values = c("orange2")) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(X)
  })
  
  # PLOT 2: News media distribution
  output$newsAppearancePlotOutput <- renderPlot({
    # Import computed distribution
    News <- computeDynamics()[[2]]
    # Format and label the imported data
    NewsPlot <- melt(data.frame(x, News), id.vars = 'x')
    colnames(NewsPlot) <-
      c("Evidence",  "Distribution", "Probability")
    # Create the ggplot
    Y <- ggplot(NewsPlot) +
      geom_area(
        data = NewsPlot,
        size = 1,
        aes(
          x = Evidence,
          y = Probability,
          fill = Distribution,
          color = Distribution
        ),
        alpha = 0.5
      ) +
      theme_minimal() +
      ggtitle("Reported Distribution of Evidence") +
      labs(x = "Evidence", y = "Reported Frequency") +
      scale_x_continuous(limits = c(-10, 10)) +
      scale_y_continuous(limits = c(0, 0.75)) +
      scale_fill_manual(values = c("darkorange3")) +
      scale_color_manual(values = c("darkorange3")) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(Y)
  })
  
  # PLOT 1: State of the world distribution
  output$IndividualBeliefPlotOutput <- renderPlot({
    # Import computed distribution
    Prior <- computeDynamics()[[3]]
    Posterior <- computeDynamics()[[5]]
    # Format and label the imported data
    IndividualBeliefPlot <-
      melt(data.frame(x, Prior, Posterior), id.vars = 'x')
    colnames(IndividualBeliefPlot) <-
      c("Evidence",  "Distribution", "Probability")
    # Create the ggplot
    Z <- ggplot(IndividualBeliefPlot) +
      geom_area(
        data = IndividualBeliefPlot,
        size = 1,
        aes(
          x = Evidence,
          y = Probability,
          fill = Distribution,
          color = Distribution
        ),
        alpha = 0.5,
        position = "identity"
      ) +
      coord_cartesian(ylim = c(0, 0.75)) +
      theme_minimal() +
      ggtitle("Individual Perception of Evidence") +
      labs(x = "Evidence", y = "Subjective Probability") +
      scale_x_continuous(limits = c(-10, 10)) +
      scale_fill_manual(values = c("pink", "firebrick2")) +
      scale_color_manual(values = c("pink", "firebrick2")) +
      guides(fill = guide_legend(
        keywidth = 0.4,
        keyheight = 0.4,
        default.unit = "inch"
      )) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.6),
        legend.background = element_rect(
          colour = 'white',
          fill = 'white',
          size = 3
        ),
        legend.text = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(Z)
  })
  
  # Print to UI: statistics of objective distribution
  output$ui1params <- renderUI({
    withMathJax(HTML(
      paste(
        "<h5>\\(\\mu_{true}=\\) ",
        as.numeric(input$trueStateMean),
        ", \\(\\quad \\sigma_{true}=\\) ",
        as.numeric(input$trueStateSD),
        "</h5>",
        sep = ""
      )
    ))
  })
  
  # Print to UI: statistics of reported distribution
  output$ui2params <- renderUI({
    meanNews <- round(computeDynamics()[[6]][1], digits = 1)
    sdNews <- round(computeDynamics()[[6]][2], digit = 1)
    withMathJax(HTML(
      paste(
        "<h5>\\(\\mu_{reported}=\\) ",
        meanNews,
        ", \\(\\quad \\sigma_{reported}=\\) ",
        sdNews,
        "</h5>",
        sep = ""
      )
    ))
  })
  
  # Print to UI: statistics of belief distribution
  output$ui3params <- renderUI({
    meanPerception <- round(computeDynamics()[[7]][1], digits = 1)
    sdPerception <- round(computeDynamics()[[7]][2], digit = 1)
    withMathJax(HTML(
      paste(
        "<h5>\\(\\mu_{0}=\\) ",
        as.numeric(input$initialBelief),
        ", \\(\\quad \\mu_{posterior}=\\) ",
        meanPerception,
        ", \\(\\quad \\sigma_{posterior}=\\) ",
        sdPerception,
        "</h5>",
        sep = ""
      )
    ))
  })
  
  
})

### EOD ###