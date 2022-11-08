test_that("", {

  prm_create(pname = c("LSPHS", "LSPHE_latent"),
             pfile = c("WHCER048.ECO", ""),
             pmin = c(4.0, 0.0), #5.5
             pmax = c(5.7, 1.0), #6.5
             pmu = c(5.5, 0.8), #6.3
             psigma = c(0.28, 0.1)) %>%
    prm_add_transform(LSPHE~LSPHE_latent*(6.5 - LSPHS - 0.1) + LSPHS + 0.1,
                      "WHCER048.ECO")

})
