test_that("dssHmisc works", {

 with(opals$local1$envir,
      {
      set.seed(123)
      src <- data.frame(age = rnorm(400, 50, 10))
      src$bp <- rnorm(400,120, 15)
      src$bp[1] <- NA
      src$d.time <- rexp(400)
      src$cens <- runif(400,.5,2)
      src$death <- src$d.time <= src$cens
      src$d.time <- pmin(src$d.time, src$cens)
      })

    r<- dssHmisc('rcorrcens', 'survival::Surv(d.time, death) ~ age + bp', data = 'src', async = FALSE, datasources = opals["local1"])
    expect_lte(r$local1[1,1] - 0.4753143, 1e-05)
})
