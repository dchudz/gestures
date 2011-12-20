#Feel free to copy/modify for any purpose, as long as you:
# --attribute me by name (David Chudzicki)
# --attribute me by URL (www.learnfromdata.com)
# --leave this notice attached


library("plyr")
#library("emdist")
library("Matrix")
library("colorspace")
library(gridExtra)



#this is my convenient function for plotting to a file on my webserver for viewing in a browser
media = "/home/ubuntu/dj/mysite/media/gestures/"
file = paste(media, "plot.jpeg",sep="")
splot <- function(arg) {
  jpeg(file, width=960, height=960)
  eval(arg)
  dev.off()
}
  
#package for loading MATLAB saved objects
library("R.matlab")

#load the MATLAB saved object
Ks47 <- readMat("Ks47.mat")
K <- Ks47[[1]]

#Number of movies:
length(K)
#Number of total frames:
sum(sapply(K,length))

#Number of movies to include in PCA:
NumMovies = 20

NumTrainingFrames <- sum(sapply(K[1:10],length))
NumTrainingFrames
NumTTSFrames <- sum(sapply(K[1:NumMovies], length))
NumTTSFrames

NumFrames <- sum(sapply(K,length))
NumFrames

#decide on the width (FrameSize) and length (NumFrames) of my matrix for holding the frames
FrameSize <- prod( dim(K[[1]][[1]]) )

#move data from frame matrices to one big matrix
KM <- matrix(nrow = NumFrames, ncol = FrameSize + 1)
index = 1
for (videoNum in 1:length(K) ) {
  video <- K[[videoNum]]
  for (frameNum in 1:length(video)) {
    frame <- video[[frameNum]]
    KM[index,] = c(frame,videoNum)
    index = index + 1
  }
}

#do PCA
system.time( KpTrain <- prcomp(KM[1:NumTrainingFrames,1:FrameSize]) )
system.time( KpTTS <- prcomp(KM[1:NumTTSFrames,1:FrameSize]) )

save(file="gestures.Rdata")



#get answers:
test_answers <- read.csv("./data/devel01/devel01_test.csv",header=F)
train_answers <- read.csv("./data/devel01/devel01_train.csv",header=F)

answers <- rbind(train_answers, test_answers)
answers$videoNum <- sapply(strsplit(as.character(answers$V1),"_"),function(l) as.numeric(l[2]))
answers <- answers[,-1]
names(answers)[1] <- "answer"
answers

#make plots

FrameSet = 1:NumTTSFrames
PCObject = KpTTS

plotdata <- data.frame(PCObject$x[FrameSet,1:2], videoNum = KM[FrameSet,FrameSize+1])
plotdata <- merge(plotdata,answers)
head(plotdata)
plotdata

names(plotdata)

MakeTestVideoPlot <- function(testVideo, testFrame="all") {
  testdata <- subset(plotdata,videoNum==testVideo)
  if (testFrame != "all") testdata = testdata[1:testFrame,]
  p <- ggplot(data=subset(plotdata,videoNum <= 10)) +
    geom_segment(aes(x=PC1,y=PC2, group=videoNum,color=answer,
                     xend=c(tail(PC1,n=-1),NA), yend=c(tail(PC2,n=-1),NA)),
                 arrow=arrow(length=unit(.15,"cm"))) #+

                        # scale_shape_manual(value=1:100)
  p = p + geom_segment(aes(x=PC1,y=PC2, group=videoNum, linetype=1,
    xend=c(tail(PC1,n=-1),NA), yend=c(tail(PC2,n=-1),NA)),
    arrow=arrow(length=unit(.15,"cm")) ,    
    data=testdata) +
      opts(title = answers$answer[testVideo])
  print(p)
  5
}


#ShowThisMany = 10
#pdffile = "/home/ubuntu/dj/mysite/media/gestures/devel01test11to20.pdf"
#pdf(pdffile)
#for (i in 11:(10+ShowThisMany)) MakeTestVideoPlot(i)
#dev.off()

?as.character

testVideo = 11

testGIF <- function(testVideo) {
  nrows = nrow(subset(plotdata,videoNum==testVideo))
  frame=10
  directory = paste("/home/ubuntu/dj/mysite/media/gestures/gif",testVideo,sep="")
  try(  system(paste("mkdir",directory)  ))
  try(  system(paste("rm ", directory, "/*.png",sep=""))  )
  for ( frame in seq(1,nrows,10) ) {
    file = paste("/home/ubuntu/dj/mysite/media/gestures/gif",testVideo,"/","frame",sprintf('%05d', frame),".png",sep="")
    png(file)
    MakeTestVideoPlot(testVideo, testFrame=frame)
    dev.off()
  }
  system(paste("convert -delay 60 ", directory, "/*.png ", directory, "/test11.gif",sep=""))
}

testGIF(15)
testGIF(17)

https://github.com/hadley/ggplot2/wiki/Using-ggplot2-animations-to-demonstrate-several-parallel-numerical-experiments-in-a-single-layout
saveMovie({
      for (i in unique(bm.data$step)) {
                print(qplot(x, y, facets = ~group, geom = "line", colour = id, alpha = I(0.1),
                                                           data = subset(bm.data, step <= i), main = paste("step", i)) +
                                    xlim(range(bm.data$x)) + ylim(range(bm.data$x)) +
                                    geom <- point(aes(x = x, y = y, facets = ~group, size = I(rep(3, n * grp))),
                                                                                 data = subset(bm.data, step == i)) +
                                    theme <- bw() + opts(legend.position = "none"))
              }
    }, interval = 0.2, movie.name = "ggplot2-brownian-motion.gif", ani.width = 600, ani.height = 600)
