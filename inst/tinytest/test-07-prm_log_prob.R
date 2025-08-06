library(tinytest)

obs_df <-
  structure(list(
    EXPERIMENT = c("INST0001", "INST0001", "INST0001",
                   "INST0001", "INST0001", "INST0001", "INST0001", "INST0001", "INST0001",
                   "INST0001", "INST0001", "INST0001", "INST0001", "INST0001", "INST0001",
                   "INST0001"),
    TRNO = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
             2, 2, 2),
    DATE = structure(c(-62135575764, -62135575764, -62135575764,
                       -62135575764, -62135575764, -62135575764, -62135575764, -62135575764,
                       -62135575764, -62135575764, -62135575764, -62135575764, -62135575764,
                       -62135575764, -62135575764, -62135575764),
                     tzone = "",
                     class = c("POSIXct", "POSIXt")),
    variable = c("HWAM", "HWAM", "HWAM", "HWAM", "HIAM",
                 "HIAM", "HIAM", "HIAM", "HWAM", "HWAM", "HWAM", "HWAM", "HIAM",
                 "HIAM", "HIAM", "HIAM"),
    obs = c(4107.38354877589, 4462.91936435583,
            4805.7209036414, 3156.00427469679, 0.379659650233301, 0.365621605702139,
            0.410991015867936, 0.327320307607068, 3621.5451300748, 4185.27705846846,
            3446.66269484243, 4013.76122686646, 0.416825152214178, 0.291264519301968,
            0.326243098042931, 0.334623913857399)),
    class = "data.frame",
    row.names = c(NA, -16L))

sim_df <-
  obs_df |>
  csmparameter:::df_rename(sim = "obs") |>
  aggregate(sim ~ EXPERIMENT + TRNO + DATE + variable,
            FUN = mean)

pmu <- c(25, 32, 3.8)

prm_df <-
  csmparameter::prm_create_prm_df(pname = c("G1", "G2", "G3"),
                                  pmin = c(16, 1.2, 0.5),
                                  pmax = c(36, 43, 23),
                                  pmu = c(25, 32, 3.8),
                                  psigma = c(5, 11, 6.2),
                                  pdist = "normal",
                                  pfile = "WHCER048.CUL",
                                  ptier = NA,
                                  pkey = "IB0001") |>
  csmparameter::prm_add_variances(psigma = c(250, 0.025),
                                  variable = c("HWAM", "HIAM"))

pval <- c(pmu, 250, 0.025)

prior_lp <- sum(
  sapply(
    seq_along(prm_df$pdensity),
    function(i) prm_df$pdensity[[i]](pval[i])
  )
)

sd_tmp <- sapply(obs_df$variable, switch, "HWAM" = 250, "HIAM" = 0.025)

log_likelihood <-
  obs_df |>
  merge(sim_df, all = TRUE) |>
  with(
    sum(dnorm(obs, mean = sim, sd = sd_tmp, log = TRUE))
  )

expect_identical(
  csmparameter:::prm_prior_log_density(pval, prm_df),
  prior_lp,
  info = "prm_prior_log_density()")

expect_identical(
  csmparameter::prm_log_prob(obs_df, sim_df, prm_df, pval),
  prior_lp + log_likelihood,
  info = "prm_log_prob()")
