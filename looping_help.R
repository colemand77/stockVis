trims <- c(0, 0.1, 0.2, 0.5)
x <-rcauchy(100)

lapply(trims, function (trim) mean(x, trim = trim))
lapply(trims, mean, x=x)

formulas <-list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt)

formulas
lm(mpg~disp, data=mtcars)

lapply(formulas, FUN = function(x) lm(x,data=mtcars))

models <- c()
for (n in formulas) {
  print(n)
  models <- c(models,lm(n, data=mtcars))
}
models


bootstraps <- lapply(1:10, function(i){
  rows <- sample(1:nrow(mtcars), rep=TRUE)
  mtcars[rows, ]
})
bootstraps

#using lapply
lapply(bootstraps,FUN=function(x) lm(mpg~disp, data=x))

#using loop
out <- vector("list",length(bootstraps))
for (i in seq_along(bootstraps)){
  out[[i]]<-lm(mpg~disp,data=bootstraps[[i]])
}
out
