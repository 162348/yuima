rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#' @title adaptive Bayesian estimation via Stan
#' @description convert yuima object into Stan code and estimate parameters via Stan
#' @param yuima an yuima object
#' @param start initial values of parameters
#' @param priors prior distributions of parameters
#' @param upper upper bounds of parameters
#' @param lower lower bounds of parameters
#' @param iter number of Monte Carlo samples
#' @param chains number of parallel chains
#' @param refresh progress display interval
#' @param rstan if FALSE, cmdstanr will be used
#' @export
adaStan <- function(yuima, start, priors, upper, lower,
                    iter = 4000, chains = 4,
                    refresh = 1000, rstan = TRUE) {

  .check_slots(yuima)
  sde_data <- list(N =  yuima@sampling@n,
                   x = as.numeric(yuima@data@original.data),
                   T = yuima@sampling@Terminal,
                   h = yuima@sampling@Terminal / yuima@sampling@n)

  stan_code <- .yuima_to_stan(yuima)  # generating Stan code

  if (rstan) {
    fit <- rstan::stan(model_code = stan_code,
                       data = sde_data,
                       iter = iter,
                       chains = chains)
  } else {
    stan_file_variables <- cmdstanr::write_stan_file(stan_code)
    model <- cmdstanr::cmdstan_model(stan_file_variables)
    fit <- model$sample(
      data = sde_data,
      iter_sampling = iter,
      chains = chains,
      refresh = refresh
    )
  }

  return(new("adastan", fit = fit, code = stan_code))
}

.check_slots <- function(yuima) {
  # error when yuima object itself is missing
  if (missing(yuima)) {
    yuima.stop("yuima object is missing.")
  }
  # To Do: check other slots
  if (is.null(yuima@data@original.data)) {
    yuima.stop("Data is not available")
  }
}

stan_templates <- "
data {{
{data}
}}
parameters {{
{parameters}
}}
model {{
  x[1] ~ normal(0, 1);
  x[2:(N + 1)] ~ normal(x[1:N] + h .* {drift}, sqrt(h) .* {diffusion});
}}
"

.yuima_to_stan <- function(yuima) {
  .initialize_yuima(yuima)

  # Construct Stan code by combining snippets
  # for each section (data, parameters, model)
  snippets <- list(
    data = .fetch_data(yuima),
    parameters = .fetch_parameters(yuima)
  )
  snippets <- append(snippets, .fetch_model(yuima))

  return(glue::glue_data(snippets, stan_templates, .trim = FALSE))
}

.initialize_yuima <- function(yuima) {
  # To Do: change the state variable to x
  if ("x" != yuima@model@state.variable) {
    yuima.warn("State variable is not 'x'.
               They will be converted to 'x' internally.")
  }
}

# sample size N, state variable x, time T, time step h, will be always used inside,
# no matter what the variable names are in `yuima` object received as an argument.
.fetch_data <- function(yuima) {
  variables <- c(
    "int<lower=1> N;",  # Stan will check N != 0
    "vector[N+1] x;",
    "real<lower=0> T;",
    "real<lower=0> h;"
  )
  snippet <- paste(" ", variables, collapse = "\n")
  return(snippet)
}

.fetch_parameters <- function(yuima) {
  params <- yuima@model@parameter@all
  snippet <- paste(" ", "real", params, ";", collapse = "\n")
  return(snippet)
}

.fetch_model <- function(yuima) {

  # convert parameter names to Stan syntax
  # and vectorize them
  # e.g. yuima@model@state.variable -> x[1:N]
  # e.g. mu -> rep_vector(mu, N)
  drift <- .standardize_parameter_names(yuima)$drift
  diffusion <- .standardize_parameter_names(yuima)$diffusion

  return(list(drift = drift, diffusion = diffusion))
}

.standardize_parameter_names <- function(yuima) {
  sv <- yuima@model@state.variable
  vr <- yuima@model@parameter@all

  drift <- gsub(sv, "x[1:N]", yuima@model@drift)
  drift <- .vectorize_operation(drift)
  drift <- Reduce(function(text, var) {
    gsub(var, sprintf("rep_vector(%s, N)", var), text)
  }, vr, init = drift)

  diffusion <- gsub(sv, "x[1:N]", yuima@model@diffusion[[1]])
  diffusion <- .vectorize_operation(diffusion)
  diffusion <- Reduce(function(text, var) {
    gsub(var, sprintf("rep_vector(%s, N)", var), text)
  }, vr, init = diffusion)

  return(list(drift = drift, diffusion = diffusion))
}

.vectorize_operation <- function(text) {
  text <- gsub("\\*", ".*", text)
  text <- gsub("\\/", "./", text)
  text <- gsub("\\^", ".^", text)
  return(text)
}

.fetch_parameters_with_constraints <- function(yuima, parameter_name) {
  # To Do: not completed: specify parameters that take positive real values, and return the corresponding Stan code
  params <- yuima@model@parameter@all
  if (parameter_name %in% params) {
    params <- params[params != parameter_name]
    snippet <- paste(" ", "real", params, ";", collapse = "\n")
    return(snippet)
  }
  return(.fetch_parameters(yuima))
}
