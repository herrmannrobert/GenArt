if(F){
  #Packages
  .rs.restartR()
  packages  <- c("devtools", "roxygen2")
  if(length(packages[!(packages %in% installed.packages()[,"Package"])])){
    install.packages(packages[!(packages %in% installed.packages()[,"Package"])])}
  suppressWarnings(loaded <- lapply(packages, require, character.only = TRUE)); names(loaded) <- packages; loaded
  
  #PackageName
  pn <- "GenArt"
  
  #DescriptionFile
  custdes <- list("Package" = as.character(pn),
                  "Title" = "Genetic algorithm for image reproduction",
                  "Maintainer" = "Robert Herrmann <robertherrmann@mail.de>",
                  "Author" = "Robert Herrmann",
                  "Version" = "0.1.0",
                  "Description" =  "R package provides a genetic algorithm for image reproduction. It is based on forcing one or more errors during DNA replication (mutation) to keep the best candidate for another mutation. DNA consists of coordinates, degree of transparency, and RGB codes or gray scales for a given number of polygons.",
                  "Depends"= "R (>= 3.5.1)",
                  "Encoding" = "UTF-8",
                  "LazyData" = "true",
                  "License" = "MIT + file LICENSE",
                  "RoxygenNote" = "6.1.0",
                  "Import" = "png")
  
  #CreatePackage
  setwd(paste(Sys.getenv("HOME"), "Dropbox/GitHub", sep = "/"))
  create(pn, custdes)
  
  #CreateDocumentation
  setwd(paste("./", pn, sep = ""))
  document()
  
  #TestingPackage
  setwd("..")
  install(pn)
  library(GenArt)
  help(package = as.character(pn))
  
  #WritingManual
  check(pn, check_dir = getwd(), manual = TRUE)
  
  #LoadFromGitHub
  .rs.restartR()
  remove.packages(pn)
  install_github(paste("herrmannrobert", pn, sep = "/"), force = TRUE)
  library(GenArt)
  help(package = as.character(pn))
}

#------------------------------------
#---Picture comparison-------------
#------------------------------------

#' @title Calculate image fitness
#' @description Function to compare two images to calculate fitness.
#' @param imgONE first array to compare with second array.
#' @param imgTWO second array to compare with first array.
#' @details ...
#' @export

pic_compare <- function(imgONE, imgTWO){
  
  (1 - mean(abs(imgONE - imgTWO)/abs(0 - imgTWO))) * 100
  
}

#------------------------------------
#---Random image tiling--------------
#------------------------------------

#' @title Randomly crop an image
#' @description Function to tile an image into 4 different pieces.
#' @param w numeric, width of the original picture.
#' @param h numeric, height of the original picture.
#' @param plot logic, function plots layout if TRUE (default).
#' @details Try multiple times to get the required image layout. Assign data frame to an object for subsequent use in gen_art. See example.
#' @export
#' @examples
#' ## Image width and height
#' h <- 100
#' w <- 120
#' 
#' ## Three trials as an example
#' for(i in 1:3){
#'    pic_randomtiles(w, h, plot = TRUE)
#'    title("Split your image for better results...")
#'    text(w/2, h/2, paste("Trial", i), cex = 4, col = "white")
#'    Sys.sleep(1.5)
#' }

pic_randomtiles <- function(w, h, plot = TRUE){

  if(sample(c(TRUE, FALSE), 1)){
    tl <- c(0, sample(10:w-10, 1), 0, sample(round(h/10):h-round(h/10), 1))
    bl <- c(0, sample(tl[2]:w-round(w/10), 1), tl[4], h)
    tr <- c(tl[2], w, 0, tl[4])
    br <- c(bl[2], w, tl[4], h)
  }else{
    tl <- c(0, sample(10:w-10, 1), 0, sample(round(h/10):h-round(h/10), 1))
    tr <- c(tl[2], w, 0, sample(tl[4]:h-round(h/10), 1))
    bl <- c(0, tl[2], tl[4], h)
    br <- c(tl[2], w, tr[4], h)
  }
  
  if(TRUE){
    tileFrame <- data.frame(xleft = c(tl[1], bl[1], tr[1], br[1]),
                            ybottom = c(tl[3], bl[3], tr[3], br[3]),
                            xright = c(tl[2], bl[2], tr[2], br[2]),
                            ytop = c(tl[4], bl[4], tr[4], br[4]),
                            col = rgb(runif(4), runif(4), runif(4)))
    plot(h, xlim = c(0, w), ylim = c(h, 0), type = "n", xlab = "WIDTH", ylab = "HIGHT")
    rect(tileFrame[,1], tileFrame[,2], tileFrame[,3], tileFrame[,4], col = tileFrame[,5])
  }
  
  entire <- rbind(tl, tr, bl, br)
  entire <- entire[, c(3, 4, 1, 2)]
  entire[entire == 0] <- 1
  
  return(entire)
  
}

#------------------------------------
#---Uniform image tiling--------------
#------------------------------------

#' @title Uniformly crop an image
#' @description Function to uniformly crop an image into n pieces.
#' @param wh vector, width and height of the original image.
#' @param nn vector, number of columns and rows to split the image in.
#' @param plot logic, function plots layout if TRUE (default).
#' @details Function cuts remaining pixels if width/height is indivisible by columns/rows. Assign data frame to an object for subsequent use in gen_art. See example.
#' @export
#' @examples
#' ## Image width/height and number of colums/rows
#' wh <- c(89, 70)
#' nn <- c(6, 4)
#' 
#' ## Calculation and plotting
#' pic_unitiles(wh, nn)

pic_unitiles <- function(wh, nn, plot = TRUE){

  wn <- cbind(
    c(head(round(seq(1, wh[1], length.out = nn[1])), -1)[1],
      head(round(seq(1, wh[1], length.out = nn[1])), -1)[-1] + 1),
    tail(round(seq(1, wh[1], length.out = nn[1])), -1)
  )
  
  hn <- cbind(
    c(head(round(seq(1, wh[2], length.out = nn[2])), -1)[1],
      head(round(seq(1, wh[2], length.out = nn[2])), -1)[-1] + 1),
    tail(round(seq(1, wh[2], length.out = nn[2])), -1)
  )
  
  for(i in 1:nrow(hn)){
    block <- cbind(rep(hn[i, 1], nrow(wn)),
                   rep(hn[i, 2], nrow(wn)),
                   wn)
    ifelse(i == 1,
           entire <- block,
           entire <- rbind(entire, block))
  }
  
  if(plot){
    plot(x = c(1, wh[1]), y = c(1, wh[2]), ylim = c(wh[2], 1), xlab = "WIDTH", ylab = "HEITH", type = "n")
    abline(h = c(0, unique(entire[,2])), lty = 2)
    abline(v = c(0, unique(entire[,4])), lty = 2)
  }
  
  return(entire)

}

#-----------------------------------------------
#---Converts pic to grayscale-------------------
#-----------------------------------------------

#' @title Convert RGB code to gray scale
#' @description Function to convert RGB to gray scale.
#' @param pic array including RGB. 
#' @details ...
#' @export

pic_convert <- function(pic){
  
  bar <- pic[,,1] + pic[,,2] + pic[,,3]
  array(bar/max(bar), dim = c(dim(pic)[1:2], 3))
  
}

#---------------------------------------------------
#---Reads an already created DNA saved in Log.txt---
#---------------------------------------------------

#' @title Read image DNA
#' @description Function to read file.txt including a tangled or untangled image DNA.
#' @param log filepath. 
#' @param tangle logic, TRUE if tangled (default), FALSE if untangled.
#' @details After your calculation is done, you may want to improve the results by reloading the existing image DNA, and start the calculation again.
#' @export

dna_read <- function(log, tangle = TRUE){
  
  logbook <- read.table(log)[,c(-1,-2)]
  ifelse(tangle,
         dna <- paste(as.character(logbook[,1]), paste(logbook[1,-1], collapse = " "), collapse = " "),
         dna <- dna_untangle(paste(as.character(logbook[,1]), paste(logbook[1,-1], collapse = " "), collapse = " ")))
  
  return(dna)
  
}

#-------------------------------------------
#---Creates initial DNA---------------------
#-------------------------------------------

#' @title Generate image DNA
#' @description Function to generate a random image DNA.
#' @param n numeric, number of polygons to be drawn to your image, Default = 10. 
#' @param vertex numeric, number of vertices each polygen should be constructed of, Default = 3.
#' @param maxXY vector, sets the pixels given by the original image, Default = c(200, 200).
#' @param rgb logic, if TRUE, generates an RGB code (default), if FALLS, generates a gray scale.
#' @details Is generally the first step of starting an image reproduction. Use maxXY = dim(pic)[2:1] to get the number of pixels in the original image.
#' @export
#' @examples
#' dna_in(n = 5, vertex = 2, rgb = FALSE)

dna_in <- function(n = 10, vertex = 3, maxXY = c(200, 200), rgb = TRUE){
  
  ifelse(rgb == TRUE,
         secONE <- "nVertexRGBaXY",
         secONE <- "nVertexGaXY")
  
  secTWO <- paste(n, vertex)
  
  ifelse(rgb == TRUE,
         cols <- paste(round(runif(n, 0, 255)), round(runif(n, 0, 255)),
                       round(runif(n, 0, 255)), rep(0, n)),
         cols <- paste(round(runif(n, 0, 255)), rep(0, n)))
  
  coordsmatrix <- matrix(paste(round(runif(n*vertex, 1, maxXY[1])),
                               round(runif(n*vertex, 1, maxXY[2]))), nrow = vertex)
  coords <- apply(coordsmatrix, 2, paste0, collapse = " ")
  secTHREE <- paste(cols, coords, sep = " ")
  
  paste(secONE, secTWO, paste(secTHREE, collapse = " "), sep = " ")
  
}

#-------------------------------------------
#---Untangle DNA----------------------------
#-------------------------------------------

#' @title Untangle image DNA
#' @description Function to untangle an image DNA to get color code and coordinates of each polygon.
#' @param dna character, tangled image DNA of any size.
#' @details See example...
#' @export
#' @examples
#' dna_untangle(dna_in())
#' dna_untangle(dna_in(rgb = FALSE))

dna_untangle <- function(dna){
  
  if(is.matrix(dna))stop("DNA untangled already...")
  
  DNAsplit <- strsplit(dna, " ")
  secONE <- DNAsplit[[1]][1]
  n <- as.numeric(DNAsplit[[1]][2])
  vertex <- as.numeric(DNAsplit[[1]][3])
  vertexname <- paste0(rep(c("x", "y"), vertex),
                       rep(seq(vertex), rep(2, vertex)))
  ifelse(secONE == "nVertexRGBaXY",
         nRGB <- c("R", "G", "B", "a"),
         nRGB <- c("G", "a"))
  
  DNAmatrix <- matrix(as.numeric(DNAsplit[[1]][-1:-3]),
                      ncol = length(nRGB) + vertex*2,
                      byrow = TRUE)
  colnames(DNAmatrix) <- c(nRGB, vertexname)
  return(DNAmatrix)
  
}

#------------------------------
#---Tangle DNA-----------------
#------------------------------

#' @title Tangle image DNA
#' @description Function to tangle an untangled image DNA.
#' @param dna matrix, untangled image DNA of any size.
#' @details See example...
#' @export
#' @examples
#' dna <- dna_untangle(dna_in(n = 2, vertex = 3, rgb = FALSE))
#' dna_tangle(dna)

dna_tangle <- function(dna){
  
  if(is.character(dna))stop("DNA tangled already...")
  
  ifelse(which(colnames(dna) == "a") == 4,
         secONE <- "nVertexRGBaXY",
         secONE <- "nVertexGaXY")
  vertex <- length(colnames(dna)[grep("x|y", colnames(dna))])/2
  paste(secONE, nrow(dna), vertex, paste(apply(dna, 1, c), collapse = " "))
  
}

#------------------------------
#---Mutation of DNA------------
#------------------------------

#' @title Mutate image DNA
#' @description Function to mutate an image DNA.
#' @param dna matrix or character, untangled or tangled image DNA of any size.
#' @param degree numeric, number nucleotides to be modified per mutation, Default = 1.
#' @details It is recommended to use the softest mutation rate as given by default.
#' @export
#' @examples
#' dna <- dna_untangle(dna_in(rgb = FALSE))
#' test <- dna_mutate(dna, degree = 20)
#' pic_compare(dna, test)

dna_mutate <- function(dna, degree = "soft", maxXY){
  
  if(!is.matrix(dna)){
    dna <- dna_untangle(dna)
  }
  
  if(missing(maxXY)){
    maxXY <- c(max(dna[,grep("x", colnames(dna))]), max(dna[,grep("y", colnames(dna))])) 
  }
  
  ifelse(is.numeric(degree),
         n <- ceiling(degree/100 * length(dna)),
         n <- 1)
  
  ind <- arrayInd(sample(seq_along(dna), n), dim(dna))
  indname <- colnames(dna)[ind[,2]]
  
  dna[matrix(ind[grep("R|G|B|a", indname),], ncol = 2)] <- sample(0:255, length(grep("R|G|B|a", indname)))
  dna[matrix(ind[grep("x", indname),], ncol = 2)] <- sample(1:maxXY[1], length(grep("x", indname)))
  dna[matrix(ind[grep("y", indname),], ncol = 2)] <- sample(1:maxXY[1], length(grep("y", indname)))
  
  return(dna)
  
}

#------------------------------
#---Draw DNA-------------------
#------------------------------

#' @title Plot image DNA
#' @description Function to plot an image DNA.
#' @param dna matrix or character, untangled or tangled image DNA of any size.
#' @details Are comming up soon...
#' @export
#' @examples
#' #Is comming up soon...

dna_print <- function(dna, maxXY){
  
  if(!is.matrix(dna)){
    dna <- dna_untangle(dna)
  }
  
  if(missing(maxXY)){
    maxXY <- c(max(dna[,grep("x", colnames(dna))]), max(dna[,grep("y", colnames(dna))])) 
  }
  
  ifelse(which(colnames(dna) == "a") == 4,
         farbe <- rgb(dna[,1], dna[,2], dna[,3], dna[,4], maxColorValue = 255),
         farbe <- rgb(dna[,1], dna[,1], dna[,1], dna[,2], maxColorValue = 255))
  
  for(i in seq(nrow(dna))){
    listforplot <- list(RGBa = farbe[i],
                        xx = dna[i, grep("x", colnames(dna))],
                        yy = dna[i, grep("y", colnames(dna))])
    ifelse(i == 1,
           lfp <- list(listforplot),
           lfp[[i]] <- listforplot)
  }
  
  lapply(lfp, function(x){polygon(x$xx, x$yy, col = x$RGBa, border = x$RGBa)})
  
}

#------------------------------
#---Convert DNA to PNG---------
#------------------------------

#' @title Convert image DNA to PNG format
#' @description Function converts image DNA to array object including RGB or gray scale for each pixel.
#' @param dna matrix or character, untangled or tangled image DNA of any size.
#' @param tempf temporate file generated by default or given as file path.
#' @param pngWH vector, width and height of reconstructed image. If missing, width and height of original image are used.
#' @param bg character, color or RGB code indicating the background color of PNG.
#' @details See example...
#' @export
#' @examples
#' dna <- dna_untangle(dna_in(rgb = FALSE))
#' for(i in 1:20){
#' dna <- dna_mutate(dna)
#' }
#' test <- dna_convert(dna)
#' grid::grid.raster(test)
#' test[1,1,]

dna_convert <- function(dna, maxXY, tempf, pngWH, bg = "white"){
  
  if(missing(tempf)){
    tempf <- tempfile(fileext = ".png")
  }
  
  if(!is.matrix(dna)){
    dna <- dna_untangle(dna)
  }
  
  if(missing(maxXY)){
    maxXY <- c(max(dna[,grep("x", colnames(dna))]), max(dna[,grep("y", colnames(dna))])) 
  }
  
  ifelse(which(colnames(dna) == "a") == 4,
         farbe <- rgb(dna[,1], dna[,2], dna[,3], dna[,4], maxColorValue = 255),
         farbe <- rgb(dna[,1], dna[,1], dna[,1], dna[,2], maxColorValue = 255))
  
  for(i in seq(nrow(dna))){
    listforplot <- list(RGBa = farbe[i],
                        xx = dna[i, grep("x", colnames(dna))],
                        yy = dna[i, grep("y", colnames(dna))])
    ifelse(i == 1,
           lfp <- list(listforplot),
           lfp[[i]] <- listforplot)
  }
  
  png(filename = tempf, 
      width = ifelse(missing(pngWH), maxXY[1], pngWH[1]), 
      height = ifelse(missing(pngWH), maxXY[2], pngWH[2]),
      bg = bg)
  par(mar = rep(0, 4))
  plot(c(0,maxXY[1]), c(0,maxXY[2]), type = 'n')
  lapply(lfp, function(x){polygon(x$xx, x$yy, col = x$RGBa, border = x$RGBa)})
  dev.off()
  
  return(png::readPNG(tempf)[,,1:3])
  
}

#------------------------------------------------------
#---Genetic algorithm----------------------------------
#------------------------------------------------------

#' @title Genetic algorithm
#' @description Algorithm that continues to mutate (improve) the image DNA until manually stoped or a given maximum of iterations is reached
#' @param pic object of class array, original image in PNG format.
#' @param dna matrix or character, untangled or tangled image DNA of any size.
#' @param iter numeric, number of iterations. Default = Inf.
#' @param fname character, filename for the reconstructed image. Default = "ReconPic.png".
#' @param fanmelog character, filename for file.txt including the tangled image DNA. Default = "Log.txt".
#' @param tempf  temporate file generated by default or given as file path. See ...
#' @param degree numeric, how many nucleotides should be modified per one iteration, Default = 1.
#' @details It is recommended to use the softest mutation rate as given by default.
#' @details See example...
#' @export
#' @examples
#' #Is coming up soon...

gen_art <- function(pic, dna, iter = Inf, fname = "ReconPic.png", fnamelog = "Log.txt", tempf, degree = "Soft"){
  
  if(missing(tempf)){
    tempf <- tempfile()
  }
  
  png(filename = "OriginalPic.png", width = dim(pic)[2], height = dim(pic)[1])
  grid::grid.raster(pic)
  dev.off()
  
  pic <- pic[,,1:3]
  i <- 1
  fitold <- 1
  
  while(i < iter){
    dnanew <- dna_mutate(dna, degree = degree)
    convertedDNA <- dna_convert(dnanew, tempf = tempf, maxXY = dim(pic)[2:1])
    fitnew <- pic_compare(pic, convertedDNA)
    if(is.na(fitnew)){fitnew <- 0}
    if(i == 1 | fitold < fitnew){
      png(filename = fname, width = dim(pic)[2], height = dim(pic)[1])
      grid::grid.raster(convertedDNA)
      dev.off()
      dna <- dnanew
      fitold <- fitnew
      write(paste(i, fitnew, dna_tangle(dna)), file = fnamelog, append = FALSE)
    }
    i <- i + 1
  }
}

#--------------------------------------------
#---FurtherExamples--------------------------
#--------------------------------------------

if(FALSE){
  
  #---CompositionTwo----------
  
  setwd(paste0(Sys.getenv("HOME"), "/Desktop"))
  pic <- png::readPNG(paste0(Sys.getenv("HOME"), "/Desktop/Compos.png"))[,43:564,]
  dim(pic)
  graphics.off()
  grid::grid.raster(pic)
  dna <- dna_in(n = 50, vertex = 6, maxXY = dim(pic)[2:1], rgb = TRUE)
  dim(dna_convert(dna, maxXY = dim(pic)[2:1]))
  system.time(gen_art(pic = pic, dna = dna, iter = 100000, degree = 3))
  
  fpath <- paste0(Sys.getenv("HOME"), "/Desktop/TrialThree.txt")
  dna <- dna_read(fpath, tangle = TRUE)
  system.time(gen_art(pic = pic, dna = dna, iter = 1000))
  
  dna_convert(dna, tempf = paste0(Sys.getenv("HOME"), "/Desktop/forphone.png"),
              pngWH = c(dim(dna_convert(dna))[2]*15, dim(dna_convert(dna))[1]*15))
  
  dna_convert(dna, tempf = paste0(Sys.getenv("HOME"), "/Desktop/forphone.png"),
              pngWH = c(2000, 3000), bg = "azure4")
  
  #---Hundertwasser--------
  
  #ReadEverythingIn
  pic <- png::readPNG(paste0(Sys.getenv("HOME"), "/Dropbox/GitHub/GenArt.Raw/Hundertwasser/Hundertwasser.png"))[1:1100,,]
  dim(pic)
  
  #CreateTiles
  h <- seq(1, dim(pic)[1]+1, 100)
  w <- seq(1, dim(pic)[2]+1, 100)
  for(i in 2:length(h)){
    for(j in 2:length(w)){
      a <- c(h[i]-100, h[i]-1, w[j]-100, w[j]-1)
      ifelse(j == 2,
             b <- a,
             b <- rbind(b, a))
    }
    ifelse(i == 2,
           d <- b,
           d <- rbind(d, b))
  }
  
  #Calculations
  for(j in 1:nrow(d)){
    setwd(paste0(Sys.getenv("HOME"), "/Dropbox/GitHub/GenArt.Raw/Hundertwasser"))
    tilesDone <- unlist(strsplit(list.files(getwd(), pattern = "00.txt"), ".txt"))
    tilesList <- apply(d, 1, paste, collapse = "_")
    tilesToDo <- tilesList[!(tilesList %in% tilesDone)]
    if(length(tilesToDo) == 0) break
    tilesToDo <- na.omit(tilesToDo[1:3])
    
    #SetMultipleCores
    no_cores <- detectCores()
    cl <- makeCluster(no_cores - 1)
    registerDoParallel(cl)
    getDoParWorkers()
    
    #Calculations
    foreach(i = seq_along(tilesToDo)) %dopar% {
      
      #AreasToCalculate
      tile <- tilesToDo[i]
      fn <- paste0(tile, ".png")
      fnlog <- paste0(tile, ".txt")
      
      #PictureAndDNA
      pix <- unlist(strsplit(tile, "_"))
      pic <- png::readPNG("Hundertwasser.png")[pix[1]:pix[2],pix[3]:pix[4],]
      dna <- dna_in(n = 75, vertex = 6, maxXY = dim(pic)[2:1], rgb = TRUE)
      
      #Calculations
      st <- system.time({gen_art(pic = pic, dna = dna, iter = 60000, fname = fn, fnamelog = fnlog)})
      write(st, file = "TimeNeeded.txt", append = TRUE)
    }
    
    #StopWorkers
    stopCluster(cl)
    
    #TileFitting
    fpathONE <- paste0(Sys.getenv("HOME"), "/Dropbox/GitHub/GenArt.Raw/Hundertwasser/")
    fpathTWO <- paste0(d[,1], "_" ,d[,2], "_", d[,3], "_", d[,4], ".txt")
    fpathALL <- paste0(fpathONE, fpathTWO)
    whatever <- strsplit(tilesDone, "_")
    whatever <- lapply(whatever, "[", c(2,4))
    dnas <- lapply(paste0(tilesDone, ".txt"), dna_read, tangle = FALSE)
    
    for(zz in seq_along(tilesDone)){
      dnas[[zz]][,grep("x", colnames(dnas[[zz]]))] <- dnas[[zz]][,grep("x", colnames(dnas[[zz]]))]+as.numeric(whatever[[zz]][2])-100
      dnas[[zz]][,grep("y", colnames(dnas[[zz]]))] <- dnas[[zz]][,grep("y", colnames(dnas[[zz]]))]-as.numeric(whatever[[zz]][1])
    }
    
    size <- c(1100, 1600)
    png(filename = "EntirePic.png", width = size[2]*4, height = size[1]*4, bg = "white")
    par(mar = rep(0, 4))
    plot(1, xlim = c(1,size[2]), ylim = c(-size[1], 1), type = "n", xaxs = "i", yaxs = "i")
    lapply(dnas, dna_print)
    dev.off()
  }
  
  #Improvements
  setwd(paste0(Sys.getenv("HOME"), "/Dropbox/GitHub/GenArt.Raw/Hundertwasser"))
  tilesDone <- unlist(strsplit(list.files(getwd(), pattern = "00.txt"), ".txt"))
  lists <- lapply(paste0(tilesDone, ".txt"), read.table)
  ns <- cbind(paste0(tilesDone, ".txt"), unlist(lapply(lists, "[", 1)))
  ns <- ns[as.numeric(ns[,2]) < 59000, ]      
  
  #SetMultipleCores
  no_cores <- detectCores()
  cl <- makeCluster(no_cores - 1)
  registerDoParallel(cl)
  getDoParWorkers()
  
  #ParallelizedLoop
  foreach(i = 1:nrow(ns)) %dopar% {
    nstodo <- 60000-as.numeric(ns[i,2])
    fn <- paste0(unlist(strsplit(ns[i,1], ".txt")), ".png")
    fnlog <- ns[i,1]
    
    #PictureAndDNA
    pix <- unlist(strsplit(unlist(strsplit(ns[i,1], ".txt")), "_"))
    pic <- png::readPNG("Hundertwasser.png")[pix[1]:pix[2],pix[3]:pix[4],]
    dna <- dna_read(fnlog)
    
    #Calculations
    st <- system.time({gen_art(pic = pic, dna = dna, iter = nstodo, fname = fn, fnamelog = fnlog)})
    write(paste(unlist(strsplit(ns[i,1], ".txt")), st[3], nstodo), file = "TimeNeeded.txt", append = TRUE)
  }
  
  stopCluster(cl)
  
  #---PictureForGitHub-----------
  
  #ReadAndManipulateData
  setwd("/Users/Robert/Desktop")
  pic <- png::readPNG(paste0(Sys.getenv("HOME"), "/Desktop/MyOwn.png"))[1:530,1:440,]
  dim(pic)
  
  #Calculations
  dna <- dna_in(n = 150, vertex = 6, maxXY = dim(pic)[2:1], rgb = FALSE)
  gen_art(pic = pic, dna = dna, iter = 500000, 
          fname = paste0("pic", ".png"), fnamelog = paste0("pic", ".txt"))
  
  #Improvements
  pic <- png::readPNG(paste0(Sys.getenv("HOME"), "/Desktop/MyOwn.png"))[1:530,1:440,]
  dna <- dna_read("/Users/Robert/Desktop/pic.txt")
  gen_art(pic = pic, dna = dna, iter = 100000, 
          fname = paste0("betterpic", ".png"), fnamelog = paste0("betterpic", ".txt"))
  
  pic <- png::readPNG(paste0(Sys.getenv("HOME"), "/Desktop/pic.png"))
  graphics.off()
  grid::grid.raster(pic)
  
  #CreateTiles
  h <- seq(1, dim(pic)[1]+1, 265)
  w <- seq(1, dim(pic)[2]+1, 220)
  for(i in 2:length(h)){
    for(j in 2:length(w)){
      a <- c(h[i]-265, h[i]-1, w[j]-220, w[j]-1)
      ifelse(j == 2,
             b <- a,
             b <- rbind(b, a))
    }
    ifelse(i == 2,
           d <- b,
           d <- rbind(d, b))
  }
  
  #Calculations
  for(i in 1:4){
    pix <- d[i,]
    picz <- pic[pix[1]:pix[2], pix[3]:pix[4], ]
    dna <- dna_in(n = 75, vertex = 6, maxXY = dim(picz)[2:1], rgb = TRUE)
    dim(dna_convert(dna, maxXY = dim(picz)[2:1]))
    gen_art(pic = picz, dna = dna, iter = 100000, 
            fname = paste0(i, ".png"), fnamelog = paste0(i, ".txt"))
  }
  
}
