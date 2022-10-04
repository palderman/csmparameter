test_that("ptrans_internal",{

  expect_identical(parametR:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3"),
                   "function(p1, p2, p3){p1 + p2 + p3}")

  expect_identical(parametR:::ptrans_internal(c("p1", "p2", "p3"), "p1 + p2 + p3/p2"),
                   "function(p1, p2, p3){p1 + p2 + p3/p2}")

})

test_that("ptrans_create",{
  expect_identical(deparse(
                     parametR::ptrans_create(c("p1", "p3", "p2"), p1+p2+p3/p2)
                   ),
                   deparse(
                     function(prm){
                       do.call(
                         function(p1, p2, p3){
                           p1 + p2 + p3/p2
                         },
                         as.list(prm[c(1, 3, 2)])
                       )
                     }
                   )
  )
})


