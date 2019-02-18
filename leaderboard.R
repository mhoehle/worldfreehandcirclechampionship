library(purrr)
library(dplyr)

##Directory containing the image and .csv files with the seed points
path <- file.path("round-1")
img_list <- list.files( path=path, pattern="*.jpg")

##Common scale factor for scoring all images
scaleFactor <- 50


##Compute the circularity scores for the entire batch of jpg files located in a certain directory.
leaderboard <- map_df(img_list, ~ {
  cat("Processing: ", .x, "\n")
  #Load image and coordinates
  file_path <- file.path(file_path, .x)
  img <- imager::load.image(file_path)
  seedPoints <- read.csv(file=gsub("\\.jpg$","\\.csv", file_path))
  names(seedPoints) <- NULL

  ##Scale
  img <- imager::resize(img, -scaleFactor, -scaleFactor)
  seedPoints[,1:2] <- as.matrix(seedPoints[,1:2]) * scaleFactor/100

  ##Measure
  res <- perfectcircle::circularity(warp=img, seedPoints=seedPoints, progress=NULL)
  res$fileName <- gsub("\\.csv","", .x)

  res
})

#Leaderboard
leaderboard %>% arrange(desc(score)) %>% select(score, fileName, ratio_area)

