# test_that("ptrans_arg_list",{
#
#   test <- function(expr){
#     parametR:::ptrans_arg_list(expr)
#   }
#
#   expect_identical(
#     test(p1 + p2 + p3),
#     c("p1", "p2", "p3")
#   )
# })

test_that("ptrans_create",{

  expect_identical(deparse(parametR::ptrans_create(expr = p1 + p2 + p3)),
                   deparse(function(p1, p2, p3){
                     p1 + p2 + p3
                   }))

  expect_identical(deparse(parametR::ptrans_create(expr = p1 + p2 + p3/p2)),
                   deparse(function(p1, p2, p3){
                     p1 + p2 + p3/p2
                   }))

})

test_that("ptrans_get_ind",{

  expect_identical(parametR:::ptrans_get_ind(c("p1", "p3", "p2"),
                                             p1 + p2 + p3/p2),
                   c(1L, 3L, 2L))

})
