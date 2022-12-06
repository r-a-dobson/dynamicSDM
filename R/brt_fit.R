#' Fit boosted regression tree models to species distribution or abundance data.
#'
#' Fit gradient boosting boosted regression tree models to species distribution and abundance data and associated dynamic explanatory variables.
#' @param occ.data a data frame, the data to fit boosted regression tree models to, containing columns for model response and explanatory variable data. If required, occ.data should contain block.col and weights.col columns too.
#' @param response.col a character string, the name of the column in occ.data containing response variable column.
#' @param varnames a character vector, the names of the columns containing model explanatory variables in occ.data.
#' @param distribution a character string, the model distribution family to use, such as "Gaussian", "Poisson" or "Bernoulli".
#' @param block.col optional; a character string, the name of the column in occ.data containing spatiotemporal block numbers for occ.data splitting. See details for more information.
#' @param weights.col a character string, the name of the column in occ.data containing spatiotemporal sampling effort weights to be used in fitting process.
#' @param test.data optional; a data frame, the testing dataset for optimising interaction.depth when blocking is not used.
#' @param n.trees optional; an integer, the number of trees in boosted regression tree models. Default is 5000.
#' @param shrinkage optional; an integer, the shrinkage parameter applied to each tree in the boosted regression tree expansion. Also known as learning rate. Default is 0.001.
#' @param interaction.depth optional; an integer specifying the maximum depth of each tree (i.e. highest level of variable interactions allowed). Default optimises depth between 1 and 4.
#' @details This function calculates a gradient boosting “gbm” object for the response and explanatory variable data provided, using the gbm R package (Greenwell et al., 2019). dynamicSDM adds extra functionality for optimising the gbm interation.depth parameter and splitting training and testing data by spatiotemporal blocks to account for spatial and temporal autocorrelation.
#'
#' If interaction.depth is not given, then brt_fit will vary the interaction.depth parameter between 1 (an additive model) and 4 (four-way interaction model). For each interaction.depth value, model performance is measured by calculating the root-mean-square error of model predictions compared to actual values in the testing data. The interaction.depth value that results in the lowest root-mean-square error is used for the returned fitted model.
#'
#' The model testing dataset can either be given using argument test.data or block.col given. In the latter situation, each unique block.col block is excluded in a jack-knife approach following Bagchi et al., (2013). This approach uses each block as the model testing dataset in numerical order, whilst all other block.col blocks are used as training data for the boosted regression tree model. Therefore, the function returns a list of fitted boosted regression tree models equal to the length of unique blocking categories in block.col. If block.col is not given, models are fit to all occ.data and a single gbm model is returned.
#'
#' If weights.col given, records are weighted by their associated value in the weights.col when model fitting. For instance, the user may wish to down weigh the importance of records collected at oversampled sites and times when fitting models, and vice versa, to account for spatiotemporal biases in occurrence records.
#'
#' @return Returns a “gbm” model object or list of “gbm” model objects.
#' @references
#'Bagchi, R., Crosby, M., Huntley, B., Hole, D. G., Butchart, S. H. M., Collingham, Y., Kalra, M., Rajkumar, J., Rahmani, A. & Pandey, M. 2013. Evaluating the effectiveness of conservation site networks under climate change: accounting for uncertainty. Global Change Biology, 19, 1236-1248.
#'Greenwell, B., Boehmke, B., Cunningham, J., & GBM Developers. 2019. Package ‘gbm’. R package version, 2.
#' @examples
#'
#'data("sample_model_data")
#'split <- sample(c(TRUE, FALSE),
#'                replace=TRUE,
#'                nrow(sample_model_data),
#'                prob = c(0.75, 0.25))
#'training <- sample_model_data[split, ]
#'testing <- sample_model_data[!split, ]

#'brt_fit(
#'  occ.data = training,
#'  test.data = testing,
#'  response.col = "presence.absence",
#'  distribution = "bernoulli",
#'  weights.col = "sampling_weights",
#'  varnames = colnames(training)[9:12]
#')
#'
#'training <- sample_model_data
#'brt_fit(
#'  occ.data = training,
#'  response.col = "presence.absence",
#'  distribution = "bernoulli",
#'  block.col = "blockno",
#'  weights.col = "sampling_weights",
#'  varnames = colnames(training)[9:12]
#')
#'@export

brt_fit <-
  function(occ.data,
           response.col,
           varnames,
           distribution,
           block.col = NULL,
           weights.col = NULL,
           test.data = NULL,
           interaction.depth = NULL,
           n.trees = 5000,
           shrinkage = 0.001) {


    # Check column names for response and explanatory variables are within data
    if (!response.col %in% colnames(occ.data)) {
      stop("response.col column not found in occ.data.")
    }

    if (!any(varnames %in% colnames(occ.data))) {
      stop("one of varnames column not found in occ.data.")
    }

    # Inform user of the defauls being used in model fitting.
    if (missing(block.col)) {
      message("block.col not specified. Model will be fit on all occ.data.")
    }

    if (missing(weights.col)) {
      message("weights.col not specified. All records equally weighted.")
    }

    if (missing(distribution)) {
      stop("Distribution not specified. Please set model distribution")
    }

    if (!missing(block.col)) {
      if (!block.col %in% colnames(occ.data)) {
        stop(
          "block.col column not found in occ.data."
        )
      }
    }

    if (missing(interaction.depth)) {
      message("interaction.depth not set. Optimisation taking place.")
      if (missing(block.col) && missing(test.data)) {
        stop("No test.data provided. Required to optimise interaction.depth")
      }
    }


  # Create formula using response and explanatory variables specified
formula <-
  as.formula(paste(response.col, paste(varnames, collapse = " + "), sep = " ~ "))

    # Set response variable as correct class for "bernoulli" distribution
    if (distribution == "bernoulli") {
      if (!is.character(occ.data[, response.col])) {
        occ.data[, response.col] <- as.character(occ.data[, response.col])
      }
    }



    # Remove rows that contain NA where applicable.
    occ.data <- occ.data[!is.na(occ.data[, response.col]), ]
    for (v in 1:length(varnames)) {
      occ.data <- occ.data[!is.na(occ.data[, varnames[v]]), ]
    }

    if (!missing(block.col)) {
      occ.data <- occ.data[!is.na(occ.data[, block.col]), ]
    }
    if (!missing(weights.col)) {
      occ.data <- occ.data[!is.na(occ.data[, weights.col]), ]
    }

    # Fit models on standard training/testing data split
    if (missing(block.col)) {
      # Create empty list to bind fitted models too
      modelvector <- vector("list", 1)

      # All parameters specified, fit the model.
      if (!missing(interaction.depth)) {
        if (missing(weights.col)) {
          modelvector[[1]] <-
            gbm::gbm(
              formula = formula,
              distribution = distribution,
              data = occ.data,
              n.trees = n.trees,
              shrinkage = shrinkage,
              interaction.depth = interaction.depth
            )
        }
        if (!missing(weights.col)) {
          modelvector[[1]] <-
            gbm::gbm(
              formula = formula,
              distribution = distribution,
              data = occ.data,
              weights = occ.data[, weights.col],
              n.trees = n.trees,
              shrinkage = shrinkage,
              interaction.depth = interaction.depth
            )
        }
      }

      # No interaction.depth specified so optimises between 1 and 4 using RMSE.
      if (missing(interaction.depth)) {
        try.depth <- c(1, 2, 3, 4)
        OPTIMAL <- NULL # Empty vector to bind RMSE from each loop too.

        # Loops through each interaction.depth and measures model performance.

        for (x in 1:length(try.depth)) {

          if (missing(weights.col)) {
            model <-
              gbm::gbm(
                formula = formula,
                distribution = distribution,
                data = occ.data,
                n.trees = n.trees,
                shrinkage = shrinkage,
                interaction.depth = try.depth[x]
              )
          }
          if (!missing(weights.col)) {
            model <-
              gbm::gbm(
                formula = formula,
                distribution = distribution,
                data = occ.data,
                weights = occ.data[, weights.col],
                n.trees = n.trees,
                shrinkage = shrinkage,
                interaction.depth = try.depth[x]
              )
          }

          prediction <-
            gbm::predict.gbm(model, newdata = test.data,  type = "response")

          RMSE <-
            sum((as.numeric(as.character(test.data[, response.col])) - prediction) ^
                  2)  # Calculate RMSE between model predicted and actual values

          OPTIMAL <- rbind(OPTIMAL, RMSE)
        }

        # Extract interaction.depth that resulted in the lowest model RMSE
        complexno <- which.min(OPTIMAL[, 1])

        # Fit the final model with the optimal interaction.depth
        if (missing(weights.col)) {
          modelvector[[1]] <-
            gbm::gbm(
              formula = formula,
              distribution = distribution,
              data = occ.data,
              n.trees = n.trees,
              shrinkage = shrinkage,
              interaction.depth = complexno
            )
        }
        if (!missing(weights.col)) {
          modelvector[[1]] <-
            gbm::gbm(
              formula = formula,
              distribution = distribution,
              data = occ.data,
              weights = occ.data[, weights.col],
              n.trees = n.trees,
              shrinkage = shrinkage,
              interaction.depth = complexno
            )
        }
      }
    }

    # Each block excluded in-turn and used as the test dataset
    if (!missing(block.col)) {
      # Create empty list to bind fitted models too
      modelvector <- vector("list", length(unique(occ.data[, block.col])))

      # For every unique block
      for (blocks in 1:length(unique(occ.data[, block.col]))) {
        # Extract unique block name
        blockname <- sort(unique(occ.data[, block.col]))[blocks]
        # Train model on all blocks except the named one
        trainocc <- occ.data[!occ.data[, block.col] == blockname, ]
        # Named block becomes the test dataset
        testocc <- occ.data[occ.data[, block.col] == blockname, ]

        # All parameters specified, fit the model.
        if (!missing(interaction.depth)) {
          if (missing(weights.col)) {
            modelvector[[blocks]] <-
              gbm::gbm(
                formula = formula,
                distribution = distribution,
                data = trainocc,
                n.trees = n.trees,
                shrinkage = shrinkage,
                interaction.depth = interaction.depth
              )
          }
          if (!missing(weights.col)) {
            modelvector[[blocks]] <-
              gbm::gbm(
                formula = formula,
                distribution = distribution,
                data = trainocc,
                weights = trainocc[, weights.col],
                n.trees = n.trees,
                shrinkage = shrinkage,
                interaction.depth = interaction.depth
              )
          }
        }

        # No interaction.depth specified so optimises between 1 and 4 using RMSE.
        if (missing(interaction.depth)) {
          try.depth <- c(1, 2, 3, 4)
          OPTIMAL <- NULL # Empty vector to bind RMSE from each loop too.

          # Loops through each interaction.depth and measures RMSE.
          for (x in 1:length(try.depth)) {

            if (missing(weights.col)) {
              model <-
                gbm::gbm(
                  formula = formula,
                  distribution = distribution,
                  data = trainocc,
                  n.trees = n.trees,
                  shrinkage = shrinkage,
                  interaction.depth = try.depth[x]
                )
            }
            if (!missing(weights.col)) {
              model <-
                gbm::gbm(
                  formula = formula,
                  distribution = distribution,
                  data = trainocc,
                  weights = trainocc[, weights.col],
                  n.trees = n.trees,
                  shrinkage = shrinkage,
                  interaction.depth = try.depth[x]
                )
            }

    prediction <- gbm::predict.gbm(model, newdata = testocc,  type = "response")

    # Calculate RMSE between model predicted and actual values
    RMSE <- sum((as.numeric(as.character(testocc[, response.col])) - prediction) ^ 2)

            OPTIMAL <- rbind(OPTIMAL, RMSE)}

          # Extract interaction.depth that resulted in the lowest model RMSE
          complexno <- which.min(OPTIMAL[, 1])

          if (missing(weights.col)) {
            modelvector[[blocks]]  <-
              gbm::gbm(
                formula = formula,
                distribution = distribution,
                data = trainocc,
                n.trees = n.trees,
                shrinkage = shrinkage,
                interaction.depth = complexno
              )
          }
          # Fit the final model with the optimal interaction.depth
          if (!missing(weights.col)) {
            modelvector[[blocks]]  <-
              gbm::gbm(
                formula = formula,
                distribution = distribution,
                data = trainocc,
                weights = trainocc[, weights.col],
                n.trees = n.trees,
                shrinkage = shrinkage,
                interaction.depth = complexno
              )
          }
        }
      }
    }

    return(modelvector)
  } # Return list of fitted model(s)
