
# This file creates an xkcd-styled stickman gif "movie" 

# --------------------- #
# load packages
# --------------------- #
library(xkcd)
library(extrafont)
library(ggplot2)
# Note: animation may require additional steps to setup
library(animation) 
# --------------------------------------------------------------- #
# First time using xkcd requires font install and proper loading
# --------------------------------------------------------------- #
# download.file("http://simonsoftware.se/other/xkcd.ttf",dest="xkcd.ttf", mode="wb")
# system("mkdir ~/.fonts")
# system("cp xkcd.ttf ~/.fonts")
# font_import(pattern = "[X/x]kcd", prompt=FALSE)
# fonts()
# fonttable()
# if(.Platform$OS.type != "unix") {
# ## Register fonts for Windows bitmap output
#   loadfonts(device="win")
# } else {
#   loadfonts()
# }
font_import(pattern = "[X/x]kcd", prompt=FALSE)
loadfonts()

# ------------------------------------------------- #
# Creating xkcd objects for later, 
# refer to xkcd package for syntax details 
# ------------------------------------------------- #
xrange <- c(0,1)
yrange <- c(0,1)
ratioxy <- diff(xrange) / diff(yrange)
datalines_1 <- data.frame(xbegin=0.17,ybegin=0.65,xend=0.30, yend=0.8)
datalines_2 <- data.frame(xbegin=0.65,ybegin=0.65,xend=0.55, yend=0.75)
datalines_axes_jitter <- (1-0)/50
datalines_right <- data.frame(xbegin = 1, xend = 1, ybegin = 0, yend = 1)
datalines_top   <- data.frame(xbegin = 0, xend = 1 ,ybegin = 1, yend = 1)
datalines_box <- data.frame(xbegin=c(0.38,0.38,0.54,0.38),ybegin=c(0.02,0.02,0.02,0.18),
                            xend=c(0.54,0.38,0.54,0.54), yend=c(0.02,0.18,0.18,0.18),
                            group = factor(rep("grroupA",4)))
datalines_box_end <- data.frame(xbegin=c(0.38,0.38,0.54,0.38),ybegin=c(0.02,0.02,0.02,0.04),xend=c(0.54,0.38,0.54,0.54), yend=c(0.02,0.04,0.04,0.04))
boom_rect = data.frame(xmin=0.38,ymin=0.02,xmax=0.54,ymax=0.18)
mapping <- aes(x, y, scale, ratioxy, angleofspine,
                 anglerighthumerus, anglelefthumerus,
                 anglerightradius, angleleftradius,
                 anglerightleg, angleleftleg, angleofneck)
dataman_0 <- data.frame( x= c(0.15,0.7), y=c(0.7, 0.68),
                         scale = 0.26 ,
                         ratioxy = ratioxy,
                         angleofspine = -pi/2 ,
                         anglerighthumerus = c(-pi/2 + pi/24, -pi/3),
                         anglelefthumerus = c(-pi/2 - pi/12, -pi/2 - pi/12),
                         anglerightradius = c(-pi/3, -pi/3 - pi/6),
                         angleleftradius = c(-pi/2 + pi/6, -2*pi/3),
                         angleleftleg = c(3*pi/2 + pi / 12, 3*pi/2 + pi / 12),
                         anglerightleg = c(3*pi/2 - pi / 12, 3*pi/2 - pi / 12),
                         angleofneck = c(3*pi/2, 3*pi/2) + runif(2, -0.05, 0.05))
dataman_1 <- data.frame( x= c(0.15,0.7), y=c(0.7, 0.68),
                         scale = 0.26 ,
                         ratioxy = ratioxy,
                         angleofspine = -pi/2 ,
                         anglerighthumerus = c(-pi/6, -pi/3),
                         anglelefthumerus = c(-pi/2 - pi/12, -pi/2 - pi/12),
                         anglerightradius = c(pi/5, -pi/3 - pi/6),
                         angleleftradius = c(-pi/2 + pi/6, -2*pi/3),
                         angleleftleg = c(3*pi/2 + pi / 12, 3*pi/2 + pi / 12),
                         anglerightleg = c(3*pi/2 - pi / 12, 3*pi/2 - pi / 12),
                         angleofneck = c(3*pi/2, 3*pi/2) + runif(2, -0.05, 0.05))
dataman_2 <- data.frame( x= c(0.15,0.7), y=c(0.7, 0.68),
                         scale = 0.26 ,
                         ratioxy = ratioxy,
                         angleofspine = -pi/2 ,
                         anglerighthumerus = c(-pi/2 + pi/24, -pi/3),
                         anglelefthumerus = c(-pi/2 - pi/12, -pi/2 - pi/8),
                         anglerightradius = c(-pi/3, -pi/3 - pi/6),
                         angleleftradius = c(-pi/2 + pi/6, -2*pi/3 - pi/2),
                         angleleftleg = c(3*pi/2 + pi / 12, 3*pi/2 + pi / 12),
                         anglerightleg = c(3*pi/2 - pi / 12, 3*pi/2 - pi / 12),
                         angleofneck = c(3*pi/2, 3*pi/2) + runif(2, -0.05, 0.05))
dataman_kick1 = dataman_0
dataman_kick1$anglerightleg[2] = 3*pi/2
dataman_kick2 = dataman_0
dataman_kick2$anglerightleg[2] = 3*pi/2 - pi/5
dataman_headdown1 = dataman_0
dataman_headdown1$angleofneck[2] = -pi/6 + runif(1, -0.05, 0.05)
dataman_headdown1$x[2] = 0.65
dataman_headdown2 = dataman_0
dataman_headdown2$angleofneck = c( pi+pi/6,- pi/6) + runif(2, -0.05, 0.05)
dataman_headdown2$x = c(0.25,0.65)
dataman_ohno1 = dataman_headdown1
dataman_ohno1$anglelefthumerus[1] = -pi - pi/6
dataman_ohno1$angleleftradius[1] =  pi/4
dataman_ohno1$anglerighthumerus[1] =  pi/8
dataman_ohno1$anglerightradius[1] =  pi - pi/3
dataman_ohno2 = dataman_0
dataman_ohno2$anglelefthumerus = c(-pi - pi/6,-pi - pi/3)
dataman_ohno2$angleleftradius =  c(pi/4,pi/8)
dataman_ohno2$anglerighthumerus =  c(pi/8,pi/12)
dataman_ohno2$anglerightradius =  c(pi - pi/3,pi-pi/12)
checklist <- data.frame(index=1:4)
checklist$xmin <- rep(0.20,4) + runif(4, -0.01,0.01)
checklist$xmax <- rep(0.24,4) + runif(4, -0.01,0.01)
checklist$ymin <- c(0.8, 0.6, 0.4, 0.2) - 0.07
checklist$ymax <- c(0.8, 0.6, 0.4, 0.2) - 0.03
checklist_mapping <- aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax)
# ----------------------------------------- #
# Creating functions to generate different 
# scenes for the animation, note that 
# suppresWarnings() are used to cleanup 
# unwanted console messages.
# ----------------------------------------- #
xkcd_blank = function(label, size){
  suppressWarnings(
    p <- ggplot() +
      xkcdaxis(xrange,yrange) +
      annotate("text", x=0.5, y=0.5, label = label, family="xkcd",size=size) +
      xkcdline(aes(x=xbegin,y=ybegin,xend=xend, yend=yend), datalines_right, xjitteramount = datalines_axes_jitter) +
      xkcdline(aes(x=xbegin,y=ybegin,xend=xend, yend=yend), datalines_top, yjitteramount = datalines_axes_jitter) +
      theme(axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
  )
  return(p)
}


xkcd_open = function(dataman, talkingline, talkingtext, textx, texty){
  if(is.null(talkingline)){
    suppressWarnings(
      p <- ggplot() +
        xkcdaxis(xrange,yrange) +
        xkcdman(mapping, dataman) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend, yend=yend), datalines_right, xjitteramount = datalines_axes_jitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend, yend=yend), datalines_top, yjitteramount = datalines_axes_jitter) +
        ggtitle("Dramatization") +
        theme(axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
    )
  }else{
    suppressWarnings(
      p <- ggplot() +
        xkcdaxis(xrange,yrange) +
        xkcdman(mapping, dataman) +
        annotate("text", x=textx, y=texty, label = talkingtext, family="xkcd",size=7) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),talkingline, xjitteramount = 0.07) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend, yend=yend), datalines_right, xjitteramount = datalines_axes_jitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend, yend=yend), datalines_top, yjitteramount = datalines_axes_jitter) +
        ggtitle("Dramatization") +
        theme(axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
    )
  }
  return(p)
}

xkcd_checklist = function(kk){
  if(kk == 0){
    suppressWarnings(
      p <- ggplot() +
        xkcdaxis(xrange,yrange) +
        annotate("text", x=0.5, y=0.92, label = "Chris' To Do List", family="xkcd",size=7) +
        annotate("text", x=0.40, y=0.75, label = "Get Data", family="xkcd",size=6) +
        annotate("text", x=0.42, y=0.55, label = "Train Model", family="xkcd",size=6) +
        annotate("text", x=0.42, y=0.35, label = "Test Model", family="xkcd",size=6) +
        annotate("text", x=0.51, y=0.15, label = "Make A Presentation", family="xkcd",size=6) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_right, xjitteramount = datalines_axes_jitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_top, yjitteramount = datalines_axes_jitter) +
        ggtitle("Dramatization") +
        theme(axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(hjust = 0.5))
    )
  }else{
    suppressWarnings(
      p <- ggplot() +
        xkcdaxis(xrange,yrange) +
        xkcdrect(checklist_mapping,checklist[1:kk,]) +
        annotate("text", x=0.5, y=0.92, label = "Chris' To Do List", family="xkcd",size=7) +
        annotate("text", x=0.40, y=0.75, label = "Get Data", family="xkcd",size=6) +
        annotate("text", x=0.42, y=0.55, label = "Train Model", family="xkcd",size=6) +
        annotate("text", x=0.42, y=0.35, label = "Test Model", family="xkcd",size=6) +
        annotate("text", x=0.51, y=0.15, label = "Make A Presentation", family="xkcd",size=6) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_right, xjitteramount = datalines_axes_jitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_top, yjitteramount = datalines_axes_jitter) +
        ggtitle("Dramatization") +
        theme(axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
    )
  }
  return(p)
}

xkcd_prod = function(dataman, talkingline, talkingtext, textx, texty, boxjitter){
  if(is.null(talkingline)){
    suppressWarnings(
      p <- ggplot() +
        xkcdaxis(xrange,yrange) +
        xkcdman(mapping, dataman) +
        annotate("text", x=0.46, y=0.10, label = "Model", family="xkcd",size=6) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_box, xjitteramount = boxjitter, yjitteramount = boxjitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_right, xjitteramount = datalines_axes_jitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_top, yjitteramount = datalines_axes_jitter) +
        ggtitle("Dramatization") +
        theme(axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(hjust = 0.5))
    )
  }else{
    suppressWarnings(
      p <- ggplot() +
        xkcdaxis(xrange,yrange) +
        xkcdman(mapping, dataman) +
        annotate("text", x=textx, y=texty, label = talkingtext, family="xkcd",size=7) +
        annotate("text", x=0.46, y=0.10, label = "Model", family="xkcd",size=6) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),talkingline, xjitteramount = 0.07) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_box, xjitteramount = boxjitter, yjitteramount = boxjitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_right, xjitteramount = datalines_axes_jitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_top, yjitteramount = datalines_axes_jitter) +
        ggtitle("Dramatization") +
        theme(axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(hjust = 0.5))
    )
  }
  return(p)
}

xkcd_misc = function(){
  suppressWarnings(p <- ggplot() +
                     xkcdaxis(xrange,yrange) +
                     xkcdman(mapping, dataman_ohno2) +
                     xkcdrect(checklist_mapping,boom_rect, fill="red", colour="black") +
                     xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend,colour=group,size=2),datalines_box, xjitteramount = 0.1, yjitteramount = 0.1) +
                     xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_right, xjitteramount = datalines_axes_jitter) +
                     xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_top, yjitteramount = datalines_axes_jitter) +
                     annotate("text", x=0.46, y=0.1, label = "BOOM", family="xkcd",size=24) +
                    scale_colour_manual(values=c("#FFB914")) +
                     ggtitle("Dramatization") +
                     theme(axis.ticks = element_blank(),
                           axis.title.x = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.position="none",
                           plot.title = element_text(hjust = 0.5))
  )
  return(p)
}

xkcd_end = function(dataman, talkingline, talkingtext, textx, texty, boxjitter){
  if(is.null(talkingline)){
    suppressWarnings(
      p <- ggplot() +
        xkcdaxis(xrange,yrange) +
        xkcdman(mapping, dataman) +
        annotate("text", x=0.46, y=0.10, label = "Model", family="xkcd",size=6) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_box_end, xjitteramount = boxjitter, yjitteramount = boxjitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_right, xjitteramount = datalines_axes_jitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_top, yjitteramount = datalines_axes_jitter) +
        ggtitle("Dramatization") +
        theme(axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(hjust = 0.5))
    )
  }else{
    suppressWarnings(
      p <- ggplot() +
        xkcdaxis(xrange,yrange) +
        xkcdman(mapping, dataman) +
        annotate("text", x=textx, y=texty, label = talkingtext, family="xkcd",size=7) +
        annotate("text", x=0.46, y=0.10, label = "Model", family="xkcd",size=6) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),talkingline, xjitteramount = 0.07) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_box_end, xjitteramount = boxjitter, yjitteramount = boxjitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_right, xjitteramount = datalines_axes_jitter) +
        xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),datalines_top, yjitteramount = datalines_axes_jitter) +
        ggtitle("Dramatization") +
        theme(axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(hjust = 0.5))
    )
  }
  return(p)
}


# ---------------------------------- #
# Create function for animation
# ---------------------------------- #
makeplot <- function(scene_selection){
for(ss in 1:length(scene_selection)){
  if(scene_selection[ss] == 35){p <- xkcd_blank(label = " ", size = 20)}
  if(scene_selection[ss] == 36){p <- xkcd_blank(label = "Cube N1F02 Production\nPresents", size = 14)}
  if(scene_selection[ss] == 1){p <- xkcd_blank(label = "Model Go\nBoom", size = 36)}
  if(scene_selection[ss] == 2){p <- xkcd_blank(label = "Starring", size = 20)}
  if(scene_selection[ss] == 3){p <- xkcd_blank(label = "Michael\nand\nChris", size = 20)}
  if(scene_selection[ss] == 4){p <- xkcd_open(dataman = dataman_0, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL)}
  if(scene_selection[ss] == 5){p <- xkcd_open(dataman = dataman_1, talkingline = datalines_1, talkingtext = "Hi Chris!", textx = 0.44, texty = 0.83) }
  if(scene_selection[ss] == 6){p <- xkcd_open(dataman = dataman_2, talkingline = datalines_2, talkingtext = "Hi Michael!", textx = 0.44, texty = 0.79)}
  if(scene_selection[ss] == 7){p <- xkcd_open(dataman = dataman_1, talkingline = datalines_1, talkingtext = "I need you\nto make a\nmodel.", textx = 0.44, texty = 0.83) }
  if(scene_selection[ss] == 8){p <- xkcd_open(dataman = dataman_2, talkingline = datalines_2, talkingtext = "I'm on it!", textx = 0.44, texty = 0.79)}
  if(scene_selection[ss] == 9){p <- xkcd_checklist(kk = 0)}
  if(scene_selection[ss] == 10){p <- xkcd_checklist(kk = 1)}
  if(scene_selection[ss] == 11){p <- xkcd_checklist(kk = 2)}
  if(scene_selection[ss] == 12){p <- xkcd_checklist(kk = 3)}
  if(scene_selection[ss] == 13){p <- xkcd_checklist(kk = 4)}
  if(scene_selection[ss] == 14){p <- xkcd_prod(dataman = dataman_0, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.001)}
  if(scene_selection[ss] == 15){p <- xkcd_prod(dataman = dataman_1, talkingline = datalines_1, talkingtext = "Wow Chris!\nYou're fast.", textx = 0.44, texty = 0.83, boxjitter = 0.001)}
  if(scene_selection[ss] == 16){p <- xkcd_prod(dataman = dataman_2, talkingline = datalines_2, talkingtext = "It's only a\nDramatization.", textx = 0.44, texty = 0.85, boxjitter = 0.001)}
  if(scene_selection[ss] == 17){p <- xkcd_prod(dataman = dataman_1, talkingline = datalines_1, talkingtext = "Oh . . . Right . . .", textx = 0.42, texty = 0.83, boxjitter = 0.001)}
  if(scene_selection[ss] == 18){p <- xkcd_prod(dataman = dataman_2, talkingline = datalines_2, talkingtext = "Time to deploy.", textx = 0.44, texty = 0.83, boxjitter = 0.001)}
  if(scene_selection[ss] == 19){p <- xkcd_prod(dataman = dataman_kick1, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.001)}
  if(scene_selection[ss] == 20){p <- xkcd_prod(dataman = dataman_kick2, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.03)}
  if(scene_selection[ss] == 21){p <- xkcd_prod(dataman = dataman_2, talkingline = datalines_2, talkingtext = "Deployed!", textx = 0.44, texty = 0.83, boxjitter = 0.001)}
  if(scene_selection[ss] == 22){p <- xkcd_prod(dataman = dataman_headdown1, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.001)}
  if(scene_selection[ss] == 23){p <- xkcd_prod(dataman = dataman_headdown2, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.001)}
  if(scene_selection[ss] == 24){p <- xkcd_prod(dataman = dataman_headdown2, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.01)}
  if(scene_selection[ss] == 25){p  <- xkcd_prod(dataman = dataman_ohno1, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.03)}
  if(scene_selection[ss] == 26){p  <- xkcd_prod(dataman = dataman_ohno2, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.03)}
  if(scene_selection[ss] == 27){p <- xkcd_misc()}
  if(scene_selection[ss] == 28){p <- xkcd_end(dataman = dataman_ohno2, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.01)}
  if(scene_selection[ss] == 29){p <- xkcd_end(dataman = dataman_ohno1, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.01)}
  if(scene_selection[ss] == 30){p <- xkcd_end(dataman = dataman_headdown2, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.01)}
  if(scene_selection[ss] == 31){p <- xkcd_end(dataman = dataman_headdown1, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.01)}
  if(scene_selection[ss] == 32){p <- xkcd_end(dataman = dataman_0, talkingline = NULL, talkingtext = NULL, textx = NULL, texty = NULL, boxjitter = 0.01)}
  if(scene_selection[ss] == 33){p <- xkcd_end(dataman = dataman_1, talkingline = datalines_1, talkingtext = "You're fired!", textx = 0.44, texty = 0.83, boxjitter = 0.01)}
  if(scene_selection[ss] == 34){p <- xkcd_blank(label = "The End", size = 36)}
  print(p)
}
}
# ----------------------------------------------- #
#   Create scene order for gif, repeats will have
#   random jitter to make it look like things are
#   moving.
# ----------------------------------------------- #
scene_selection = c(35,35,
                    36,36,36,36,36,36,
                    1,1,1,1,1,1,
                    2,2,2,2,
                    3,3,3,3,
                    4,4,
                    5,5,5,5,
                    6,6,6,6,
                    7,7,7,7,
                    8,8,8,8,
                    9,9,9,9,
                    10,10,10,
                    11,11,11,
                    12,12,12,
                    13,13,13,
                    14,14,
                    15,15,15,15,
                    16,16,16,16,
                    17,17,17,17,
                    18,18,18,18,
                    14,14,14,14,
                    19,20,14,19,20,14,19,20,14,
                    21,21,21,21,
                    14,14,14,14,
                    22,22,22,
                    23,23,23,
                    24,24,24,
                    25,25,25,
                    26,26,26,
                    27,27,27,
                    28,28,28,
                    29,29,29,
                    30,30,30,
                    31,31,31,
                    32,32,32,
                    33,33,33,33,
                    34,34,34,34,34,34,34,34)
# -------------------------------------------- #
#  Create gif
# -------------------------------------------- #
### setwd("....path to where you want to save gif...")
N = length(scene_selection)
oopt = ani.options(interval = 0, nmax = N)
# This may take a minute to run, then launches in browser
saveGIF(makeplot(scene_selection = scene_selection),
        movie.name = "model_go_boom.gif", 
        interval = 0.5, 
        width = 580, 
        height = 400)
ani.options(oopt)

