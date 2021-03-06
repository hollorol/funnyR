source("https://raw.githubusercontent.com/hollorol/funnyR/master/installAndLoad.R")

installAndLoad("imager")
library(imager)
library(png)

biColorImage <- function(baseColor=NULL,altColor="brown", alpha=1, inputPicture=NULL, outputPicture=NULL, selectedColor=NULL, hateMouse=FALSE){

    colorTransform <- function(colorName,alpha){
        splitedColor <- unlist(strsplit(colorName,""))
        if(splitedColor[1]!="#"){
            colorName <- c(as.vector(col2rgb(colorName))/255,alpha)
        } else {
            splitedColor<-splitedColor[-1]
            colorName<-tryCatch(c(strtoi(paste0("0x",paste(splitedColor[1:2], collapse = "")))/255,
                        strtoi(paste0("0x",paste(splitedColor[3:4], collapse = "")))/255,
                        strtoi(paste0("0x",paste(splitedColor[5:6], collapse = "")))/255,
                        alpha),error = function (e) {stop("Wrong color name")})
        }
        return(colorName)
    }

    if(is.null(inputPicture)){
        inputPicture <- file.choose()
    }
    if(is.null(outputPicture)){
        outputPicture <- file.choose()
    }

    img <- tryCatch(readPNG(inputPicture), error = function (e) {stop(paste0("Cannot find ",inputPicture))})
    if(!hateMouse){
    implot <- load.image(inputPicture)
    plot(implot)
    sPoint <- locator(1)
    sPoint <- lapply(sPoint,floor)
    dev.off()
    colorOfChoosenPoint <- img[sPoint$y,sPoint$x,]} else {
                                                      colorOfChoosenPoint <- tryCatch(colorTransform(selectedColor, alpha=1),
                                                                                      error = function (e){
                                                                                          stop("Invalid or missing selectedColor parameter, when hateMouse is not set to true")
                                                                                      })
                                                  }
    if(is.null(baseColor)){
        baseColor <- colorOfChoosenPoint
    } else {
        baseColor <- colorTransform(baseColor,alpha=alpha)
    }
    altColor <- colorTransform(altColor,alpha=alpha)
    xres <- dim(img)[2]
    yres <- dim(img)[1]
    nLayers <- dim(img)[3]
    for(i in 1:xres){
        for(j in 1:yres){
            ## if(length(img[j,i,])!=0){
            ##     myArray[j,i,] <- img[j,i,]
            ## }
            img[j,i,][4] <- 1
            if(!identical(img[j,i,],colorOfChoosenPoint)){
                img[j,i,]<- altColor
            } else {
                img[j,i,]<- baseColor
            }
        }
    }
    tryCatch(writePNG(image=img,target=outputPicture), error = function (e) {stop(paste0("Cannot find ", outputPicture))})
    plot(load.image(outputPicture))
}

## europa <- readPNG("/home/hollorol/Documents/tmp/EuroBase.png")
## im <- load.image("/home/hollorol/Documents/tmp/EuroBase.png")
## print(europa)
## plot(im)
## test <- matrix(c(0,0,0,200,200,0),nrow=2,byrow = TRUE)
## points(test[,1],test[,2])
## identify(test[,1],test[,2])

## points(x=sort(rep(rep(1:600),820)),y=rep(rep(1:600),820),
##        col=rgb(0,0,0,0))
## identify(x=sort(rep(rep(1:600),820)),y=rep(rep(1:600),820))
## points(x=1:820,y=europa[1,,1],)
## plot(x=1:5,y=5:9)
## identify(x=1:5,y=5:9)
## identify(im)
## head(europa[,,1])

## locator(1)

## data.frame(x=sort(rep(rep(1:600),820)),y=rep(rep(1:600),820))

## dim(europa)
## sort(rep(rep(1:600),820))

## sort(c(5,6,1))
## colorOfCoord <- function(){
##     europa <- readPNG("/home/hollorol/Documents/tmp/EuroBase.png")
##     im <- load.image("/home/hollorol/Documents/tmp/EuroBase.png")
##     plot(im)
##     sPoint <- locator(1)
##     sPoint <- lapply(sPoint,floor)
##     dev.off()
##     return(europa[sPoint$y,sPoint$x,])
## }

## myArray <- array(numeric(820*600*4),dim=c(600,820,4))

## Eurannya <- colorOfCoord()

## for(i in 1:820){
##     for(j in 1:600){
##         if(length(europa[j,i,])!=0){
##             myArray[j,i,] <- europa[j,i,]
##         }
##         myArray[j,i,][4] <- 1
##         if(!identical(myArray[j,i,],Eurannya)){
##             myArray[j,i,]<-c(0,1,0,1)
##        }
##     }
## }

## writePNG(image=myArray,target="jancsika.png")
## dim(europa)
## im2 <- load.image("/home/hollorol/Documents/tmp/jancsika.png")
## plot(im2)
## for(j in 385:400){
##     for(i in 1:10){
##         print(identical(europa[i,j,], Eurannya))
##     }
## }


## neighbourFinder <- function(center, radius, xres, yres,
##                            dfun =     distanceR <- function(a,b){
##                                sqrt(sum((a-b)^2))
##                            }){

##     radius <- floor(radius)
##     resMatrix <- matrix(ncol = 2)
##     baseCorn <- center-rep(radius,2)

##     for(i in 1:radius){
##         for(j in 1:radius){
##             pontocska<-baseCorn+c(i,j)
##             if(radius>=dfun(pontocska,center)){
##                 p1 <- pontocska+c(2*(radius-i),0)
##                     p2 <- pontocska+c(0,2*(radius-j))
##                     p3 <- pontocska+c(2*(radius-i),2*(radius-j))
##                     resMatrix <-rbind(resMatrix,pontocska,p1,p2,p3)
##                 }
##         }
##     }

##     return(resMatrix[-1,])
## }

## neighbourFinder2 <- function(center, radius, xres, yres,
##                            dfun =     distanceR <- function(a,b){
##                                sqrt(sum((a-b)^2))
##                            }){

##     radius <- floor(radius)
##     resMatrix <- matrix(ncol = 2)
##     radiusE <- radius + 1

##     if(radius==0){
##         return(center)
##     }
    
##     for(i in 1:radius){
##         j=1
##         while(j < radiusE){
##             pontocska <- center + c(i,-j)
##             if(radius>=dfun(pontocska,center)){

##                 pv <- pontocska+c(-2*i,0)
##                 ph <- pontocska+c(0,2*j)
##                 phv <- pontocska+c(-2*i,2*j)
##                 resMatrix <-rbind(resMatrix,pontocska,pv,ph,phv)
##                 j <- j + 1
               
##             } else {
##                 j <- radiusE
##             }
##         }
##     }
##     vertx <- (center[1]-radius):(center[1]+radius)
##     verty <- rep(center[2],(2*radius+1))
##     horx <- rep(center[1],(2*radius))
##     hory <- c(((center[2]-radius):(center[2]-1)),((center[2]+1):(center[2]+radius)))
##     vert <- matrix(c(vertx,verty),ncol=2)
##     hor <- matrix(c(horx,hory),ncol=2)
##     resMatrix <- rbind(resMatrix,hor,vert)[-1,]
##     resMatrix <- resMatrix[apply(resMatrix,1,function(x) {
##         (0<=x[1]) & (x[1]<=xres) &
##         (0<=x[2]) & (x[2]<=yres)
##     }),]
##     return(resMatrix)
## }

## plot(neighbourFinder2(center = c(200,40),
##                      radius =100,
##                      xres = 300,
##                      yres = 100))
## resMatrix <- neighbourFinder2(center = c(300,100),
##                      radius =100,
##                      xres = 300,
##                      yres = 400)
## microbenchmark(neighbourFinder2(center = c(300,100),
##                      radius = 30,
##                      xres = 300,
##                      yres = 400),neighbourFinder(center = c(300,100),
##                      radius = 30,
##                      xres = 300,
##                      yres = 400))

## resMatrix <- matrix(ncol = 2)

## dMat <- matrix(numeric(600*820),nrow=600,ncol=820)


## distanceR <- function(a,b){
##    sqrt(sum((a-b)^2))
## }
## pontjaink <- matrix(numeric(2),ncol=2)
## startPoint <- c(500,200)
## baseCorn <- startPoint-c(100,100)
## for(i in 1:100){
##     for(j in 1:100){
##         pontocska<-baseCorn+c(i,j)
##         if(100>=distanceR(pontocska,startPoint)){
##             p1 <- pontocska+c(2*(100-i),0)
##             p2 <- pontocska+c(0,2*(100-j))
##             p3 <- pontocska+c(2*(100-i),2*(100-j))
##             pontjaink <-rbind(pontjaink,pontocska,p1,p2,p3)
##         }
##     }
## }


## nrow(pontjaink)
## plot(pontjaink[-1,])
## bambaMarha <- locator(1)

##     matrica <- neighbourFinder2(center = unlist(bambaMarha), radius = 0,xres=820,yres=600)
##     npoint <- nrow(matrica)
## color <- c(0,0,0,0)
##     for(i in 1:npoint){
##         color <- color + europa[matrica[i,1],matrica[i,2],]
##     }
##     color/npoint
