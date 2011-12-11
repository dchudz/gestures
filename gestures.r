#Feel free to copy/modify for any purpose, as long as you:
# --attribute me by name (David Chudzicki)
# --attribute me by URL (www.learnfromdata.com)
# --leave this notice attached


library("plyr")

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

#what's here?
names(Ks47[[1]][[1]])
dim(K[[1]][[1]])

#decide on the width (FrameSize) and length (NumFrames) of my matrix for holding the frames
NumFrames <- sum( sapply(K[1:10], length) )
FrameSize <- prod( dim(K[[1]][[1]]) )

#move data from frame matrices to one big matrix
KM <- matrix(nrow = NumFrames, ncol = FrameSize + 1)
index = 1
for (videoNum in 1:length(K[1:10]) ) {
  video <- K[[videoNum]]
  for (frameNum in 1:length(video)) {
    frame <- video[[frameNum]]
    KM[index,] = c(frame,videoNum)
    index = index + 1
  }
}

#do PCA
Kpc <- prcomp(KM[,1:FrameSize])

#what do I have?
names(Kpc)
Kpc$sdev[1:100]

#make plots
splot(
      {
        plotpcs <- function(inds) {
          plot(Kpc$x[,inds], col = KM[,FrameSize+1],pch=20, cex=2)
          df <- data.frame(Kpc$x[,inds], KM[,FrameSize+1])
          names(df)[3] = "videoNum"
          l <- dlply(df, .(videoNum), function(d) lines(d[,1:2], type="l", col=d[,3],lwd=1))
        }
#        par(mfrow = c(2,2))
        plotpcs(1:2)
#        plotpcs(c(1,3))
#        plotpcs(c(1,3))
#        plotpcs(2:3)            
      })


