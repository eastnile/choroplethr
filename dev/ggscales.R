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

ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = jomama )) + scale_color_manual(values = mycolors)


# Testing cut functions

a = 1e10
b = 1e10+100
x = c(a:b)
x = iris$Petal.Length
ncat = 5

x = c(.1, .1, .1, .1, .1, .2, .3, .4, .5)


z = Hmisc::cut2(c(.1, .1, .1, .1, .1, .2, .3, .4, .5), g = 4, m = 1, oneval = T, minmax = F)

levels(z)

cut(c(.1, .1, .1, .1, .1, .2, .3, .4, .5), breaks = 2, include_lowest = T, labels = F)

cut(c(.1, .1, .1, .1, .1, .2, .3, .4, .5), g = 5)

str_split('[0.3,0.5', pattern = ',')

str_split('0.1', pattern = ',')


x = .1

cut3 = function(x, ncat) {
  scipen_orig = getOption("scipen")
  options(scipen=999)
  x_cut = Hmisc::cut2(x, g = ncat, m = 1, minmax = T)
  labelgood = character()
  for (i in seq_along(levels(x_cut))) {
    # i = 1
    str = levels(x_cut)[i]
    strsplit = unlist(str_split(str, pattern = ','))
    if (length(strsplit) == 1) {
      labelgood[i] = str
    } else {
      left = str_replace_all(strsplit[1], "[\\[\\]\\(\\)]", "")
      right = str_replace_all(strsplit[2], "[\\[\\]\\(\\)]", "")
      labelgood[i] = paste0(left, ' to <', right)
    }
  }
  stopifnot(length(labelgood) == length(levels(x_cut)))
  levels(x_cut) = labelgood
  options(scipen=scipen_orig)
  return(x_cut)
}

cut3(x, 5)

