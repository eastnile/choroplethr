library(ggplot2)
ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length )) +
  scale_color_gradient2(low = 'green', mid = 'white', high = 'blue', midpoint = 4)


ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length )) +
  scale_color_gradient2(low = 'green', mid = 'white', high = 'blue', midpoint = 4)
#+ scale_color_gradient(low = 'yellow', high = 'green')

# Testing cut functions

a = 1e10
b = 1e10+100
x = c(a:b)
ncat = 5
cut3 = function(x, ncat) {
  scipen_orig = getOption("scipen")
  options(scipen=999)
  x = cut2(x, g = ncat)
  left = stringr::str_extract(levels(x), "(\\d+),\\s*(\\d+)", group = 1)
  right = stringr::str_extract(levels(x), "(\\d+),\\s*(\\d+)", group = 2)
  lnew = paste0(left, ' to <', right)
  levels(x) = lnew
  options(scipen=scipen_orig)
  return(x)
}

cut3(x, 5)

