library(ggplot2)
ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length )) +
  scale_color_gradient2(low = 'green', mid = 'white', high = 'blue', midpoint = 4)


ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length )) +
  scale_color_gradient2(low = 'green', mid = 'white', high = 'blue', midpoint = 4)
#+ scale_color_gradient(low = 'yellow', high = 'green')
#+ 

iris$Petal.Length
iris$jomama = sample(letters[1:4], nrow(iris), replace = T)
irisn = iris
colors = 4
irisn$Petal.Length=cut3(irisn$Petal.Length, ncat = colors)

mycolors = colorRampPalette(c("blue", "red"))(colors)

ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = jomama )) 

+
  scale_color_manual(values = mycolors)


# Testing cut functions

a = 1e10
b = 1e10+100
x = c(a:b)
x = iris$Petal.Length
ncat = 5

Hmisc::cut2(c(.1, .1, .1, .1, .1, .2, .3, .4, .5), g = 4, m = 1, oneval = F, minmax = F)


cut(c(.1, .1, .1, .1, .1, .2, .3, .4, .5), g = 5)


x = .1

cut3 = function(x, ncat) {
  scipen_orig = getOption("scipen")
  options(scipen=999)
  x = Hmisc::cut2(x, g = ncat)
  regex = '(-?\\d+(\\.\\d+)?),\\s*(-?\\d+(\\.\\d+)?)'
  left = stringr::str_extract(levels(x), regex, group = 1)
  right = stringr::str_extract(levels(x), regex, group = 3)
  lnew = paste0(left, ' to <', right)
  levels(x) = lnew
  options(scipen=scipen_orig)
  return(x)
}

cut3(x, 5)

