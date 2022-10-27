test_that("ptrans_internal",{

  expect_identical(parametR:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3"),
                   "function(p1, p2, p3){p1 + p2 + p3}")

  expect_identical(parametR:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3/p2"),
                   "function(p1, p2, p3){p1 + p2 + p3/p2}")

})

test_that("prm_add_transform",{

  prm_tbl <- tibble::tibble(pname = c("p1", "p3", "p2"))

  expect_identical(
    parametR::ptrans_create(p1~p1+p2+p3/p2, prm_tbl),
    function(prm){
      do.call(
        function(p1, p2, p3){
          p1 + p2 + p3/p2
          },
        as.list(prm[c(1, 2, 3)])
      )
      },
    ignore_function_env = TRUE
    )
})


