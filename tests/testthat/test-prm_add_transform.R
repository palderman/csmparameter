test_that("ptrans_internal",{

  expect_identical(parametR:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3"),
                   "function(p1, p2, p3){p1 + p2 + p3}")

  expect_identical(parametR:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3/p2"),
                   "function(p1, p2, p3){p1 + p2 + p3/p2}")

})

test_that("prm_add_transform with latent",{

  expected <- tibble::tibble(
    pname = c("p1_latent", "p2", "p3", "p1")
    )

  expected$pdensity <- lapply(1:4,
                             function(x) parametR:::prm_prior_density(NA, NA, 0, 1, "normal"))

  expected$psampler <- lapply(1:4,
                            function(x) parametR:::prm_prior_sampler(NA, NA, 0, 1, "normal"))

  expected$pnum <- c(1L, 2L, 3L, NA)

  expected$ptransform <- vector("list", 4)
  expected$ptransform[[4]] <- function(prm){
    do.call(
      function(p1_latent, p2, p3){
        p1_latent + p2 + p3/p2
      },
      as.list(prm[c(1, 3, 4)])
    )
  }

  expected$pdensity[4] <- list(NULL)

  expected$psampler[4] <- list(NULL)

  prm_tbl <- expected[1:3,]

  expect_identical(
    parametR::prm_add_transform(prm_tbl, p1~p1_latent+p2+p3/p2),
    expected,
    ignore_function_env = TRUE
    )

  expect_error(
    parametR::prm_add_transform(prm_tbl, p1~p1+p2+p3/p2),
  )

})

test_that("prm_add_transform without latent",{

  prm_tbl <- tibble::tibble(pname = c("p1_latent", "p1", "p3", "p2"))

  prm_tbl$pdensity <- lapply(1:4,
                             function(x) parametR:::prm_prior_density(NA, NA, 0, 1, "normal"))

  prm_tbl$psampler <- lapply(1:4,
                             function(x) parametR:::prm_prior_sampler(NA, NA, 0, 1, "normal"))

  expected <- prm_tbl

  prm_tbl <- prm_tbl[-1,]

  expected$pnum <- c(1L, NA, 2L, 3L)

  expected$ptransform <- vector("list", 4)
  expected$ptransform[[2]] <- function(prm){
    do.call(
      function(p1_latent, p2, p3){
        p1_latent + p2 + p3/p2
      },
      as.list(prm[c(1, 3, 4)])
    )
  }

  expected$pdensity[2] <- list(NULL)

  expected$psampler[2] <- list(NULL)


  expect_error(
    parametR::prm_add_transform(prm_tbl, p1~p1+p2+p3/p2),
  )

})
