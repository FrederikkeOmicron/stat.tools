#' Create a table of interaction models
#'
#' @param data A dataframe in wide format
#' @param outcome A column in data with two levels to be predicted
#' @param predictor A column in data with two levels to predict outcome
#' @param covariates A vector of column names in data to model their interaction with predictor on outcome
#'
#' @returns A dataframe of the table of interaction models
#' @export
#'
#' @examples
#' data <- default (from ISLR package)
#' interaction_model(data, outcome = "default", predictor = "student", covariates = c("income", "balance"))

interaction_model <- function(data, outcome, predictor, covariates){

  data_longer <- data %>%
    pivot_longer(
      cols = all_of(covariates),
      names_to = "parameter",
      values_to = "value"
    )

  level1 = levels(data[[predictor]])[2]
  plevel_str = paste0(predictor, level1)

  results = list()

  #all
  formula_str <- paste(outcome, "~", predictor)
  model <- glm(formula = as.formula(formula_str), family = "binomial", data = data)

  coeff = coefficients(model)
  vcov_matrix = vcov(model)

  IRR = exp(coeff[plevel_str])
  std_error = sqrt(vcov_matrix[plevel_str, plevel_str])
  IRR_lower = IRR * exp(-qnorm(0.975) * std_error)
  IRR_upper = IRR * exp(qnorm(0.975) * std_error)

  results[["overall"]] = data.frame(parameter = "overall", value = "all", IRR = IRR, IRR_lower = IRR_lower, IRR_upper = IRR_upper)

  #covariates
  for (covar in covariates){
    data_temp = data_longer %>% filter(parameter == covar)

    formula_str <- paste(outcome, "~", predictor, "* value")
    model <- glm(formula = as.formula(formula_str), family = "binomial", data = data_temp)

    coeff = coefficients(model)
    vcov_matrix = vcov(model)

    IRR = exp(coeff[plevel_str])
    std_error = sqrt(vcov_matrix[plevel_str, plevel_str])
    IRR_lower = IRR * exp(-qnorm(0.975) * std_error)
    IRR_upper = IRR * exp(qnorm(0.975) * std_error)

    IRRs = IRR
    IRR_lowers = IRR_lower
    IRR_uppers = IRR_upper

    value_levels = sort(unique(data_temp$value))
    for (level in value_levels[2:length(value_levels)]){
      IRR = exp(coeff[plevel_str] + coeff[paste0(plevel_str, ":value", level)])
      std_error = sqrt(vcov_matrix[plevel_str, plevel_str]
                       + vcov_matrix[paste0(plevel_str, ":value", level), paste0(plevel_str, ":value", level)]
                       + 2 * vcov_matrix[plevel_str, paste0(plevel_str, ":value", level)])
      IRR_lower = IRR * exp(-qnorm(0.975) * std_error)
      IRR_upper = IRR * exp(qnorm(0.975) * std_error)

      IRRs = c(IRRs, IRR)
      IRR_lowers = c(IRR_lowers, IRR_lower)
      IRR_uppers = c(IRR_uppers, IRR_upper)
    }
    results[[covar]] = data.frame(parameter = covar, value = value_levels, IRR = IRRs, IRR_lower = IRR_lowers, IRR_upper = IRR_uppers)
  }

  results = rbindlist(results)

  return(results)

}
