
rm(list=ls())


context("unpackVars")


test_that("Data formats are persisted", {
  x <- data.frame(x=seq(1:10))
  y <- c(y=10)
  z <- 15
  stringer<- 'hello'
  
  temp<- gatherVars()

  rm(x,y,z) #Remove them. 
  unpackVars(temp) #Re-unpack them. 

  expect_true(class(x) == 'data.frame')
  expect_true(class(y) == 'numeric')
  expect_true(class(stringer) == 'character')
  
})



test_that("Environment encapsulation works", {
  x <- 5
  y <- 10
  z <- 15
  
  temp<- gatherVars()
  adder<- function(stuff){
    unpackVars(stuff, keep=c('x', 'y'))
    if(!exists('z', inherits=FALSE )) z<- 100
    #' this shows that Z was not unpacked. 
    x+y+z
  }
  
  results<- adder(temp)
  expect_true(results == 115)
  
})

