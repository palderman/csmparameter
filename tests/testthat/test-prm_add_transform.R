test_that("ptrans_internal",{

  expect_identical(parametR:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3"),
                   "function(p1, p2, p3){p1 + p2 + p3}")

  expect_identical(parametR:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3/p2"),
                   "function(p1, p2, p3){p1 + p2 + p3/p2}")

})

test_that("prm_add_transform",{

  prm_tbl <- tibble::tibble(pname = c("p1", "p3", "p2"))

  expected <- tibble::tibble(pname = c("p1", "p3", "p2"),
                             pnum = 1:3,
                             ptrans = vector("list", 3))

  expected$ptrans[[1]] <- function(prm){
    do.call(
      function(p1, p2, p3){
        p1 + p2 + p3/p2
      },
      as.list(prm[c(1, 2, 3)])
    )
  }

  expect_identical(
    parametR::prm_add_transform(prm_tbl, p1~p1+p2+p3/p2),
    expected,
    ignore_function_env = TRUE
    )
})


