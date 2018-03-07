context("Quiz Function Tests")

test_that("matx computes (x)(matrix)(x^T)", {
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
    inv <- solve(a)
    final<- (t(x) %*% inv %*% x)
     expect_identical(final,matx(a,x))
})

test_that("std computes column standarization of a matrix ",{
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
    aa<- a
    count = 1
    rows <- nrow(aa)
    cols <- ncol(aa)
    while (count <= cols){
      singleColumn <- aa[,count]
      columnMean <- mean(singleColumn)
      columnSd <- sd(singleColumn)
      aa[,count] = ((singleColumn-columnMean)/columnSd)
      count = count + 1
    }
    final2<-(aa)
    expect_identical(final2,std(a))
})
