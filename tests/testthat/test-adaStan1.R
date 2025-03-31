test_that("adaStan test: simple diffusion model w/o jumps", {
  model <- setModel(drift = "theta*(mu-x)", diffusion = "sigma",
                    state.variable = "x", solve.variable = "x")
  sampling <- setSampling(Initial = 0, Terminal = 3, n = 1000)
  yuima <- setYuima(model = model, sampling = sampling)
  simulation <- simulate(yuima, true.parameter =
                           c(theta = 1, mu = 0, sigma = 0.5),
                         xinit = rnorm(1))

  output <- adaStan(simulation, iter = 2000, rstan = FALSE)

  print(output@code)
})

test_that("similar as above, but with a tricky state variable name", {
  model <- setModel(drift = "-theta * y", diffusion = "1 / (1 + y^gamma)",
                    state.variable = "y", solve.variable = "y")
  sampling <- setSampling(Initial = 0, Terminal = 3, n = 1000)
  yuima <- setYuima(model = model, sampling = sampling)
  simulation <- simulate(yuima, true.parameter =
                           c(theta = 1, gamma = 3),
                         xinit = rnorm(1))

  output <- adaStan(simulation, iter = 2000, rstan = FALSE)

  print(output@code)
})