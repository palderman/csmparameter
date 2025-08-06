library(tinytest)

expect_identical(csmparameter:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3"),
                 "function(p1, p2, p3){p1 + p2 + p3}",
                 info = "ptrans_internal")

expect_identical(csmparameter:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3/p2"),
                 "function(p1, p2, p3){p1 + p2 + p3/p2}",
                 info = "ptrans_internal")

set.seed(1234)

prm_df <-
  csmparameter::prm_create_prm_df(pname = c("p1_latent", "p2", "p3"),
                                  pfile = "",
                                  pmu = 0,
                                  psigma = 1,
                                  pdist = "normal")

set.seed(1234)

actual <- csmparameter::prm_add_transform(prm_df, p1~p1_latent+p2+p3/p2)

expect_equivalent_to_reference(actual,
                               file = "prm_add_transform_with_latent.rds")

expect_error(
  csmparameter::prm_add_transform(prm_df, p1~p1+p2+p3/p2),
)

prm_df <- prm_df[-1,]

expect_error(
  csmparameter::prm_add_transform(prm_df, p1~p1+p2+p3/p2),
  info = "prm_add_transform without latent")
