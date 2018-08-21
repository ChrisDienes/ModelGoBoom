# This file creates an xkcd-styled title image

# --------------------- #
# load packages
# --------------------- #
library(xkcd)
library(extrafont)
library(ggplot2)

# --------------------------------------------------------------- #
# First time using xkcd requires font install and proper loading
# --------------------------------------------------------------- #
# download.file("http://simonsoftware.se/other/xkcd.ttf",dest="xkcd.ttf", mode="wb")
# In windows: right click on .ttf file and then select install.
# font_import(pattern = "[X/x]kcd", prompt=FALSE)
# fonts()
# fonttable()
# if(.Platform$OS.type != "unix") {
#   loadfonts(device="win")
# } else {
#   loadfonts()
# }

font_import(pattern = "[X/x]kcd", prompt=FALSE)
loadfonts()

xrange <- c(0,1)
yrange <- c(0,1)
mainlabel <- "Let's Go Fast"
mainsize <- 22
sublabel <- "Using Rcpp to Speedup Rowwise Operations"
subsize <- 8

p <- ggplot() +
  annotate("text", x=0.5, y=0.65, label = mainlabel, family="xkcd", size=mainsize, color = "#1E90FF") +
  annotate("text", x=0.5, y=0.45, label = sublabel, family="xkcd", size=subsize) +
  ylim(0,1) +
  theme_void()
p

