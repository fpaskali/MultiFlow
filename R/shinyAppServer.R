#' Shiny app server function
#' @title MultiFlow Extended
#' @param input provided by shiny
#' @param output provided by shiny
#' @importFrom EBImage imageData display otsu readImage
#' @importFrom fs path_home dir_create
#' @import shiny stats
#' @importFrom shinyFiles shinyDirChoose
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom rmarkdown render
#' @importFrom ggplot2 ggplot geom_point geom_abline ylab annotate
#' @importFrom shinyjs useShinyjs hidden hide show reset
#'

## required packages
library(shiny)
library(shinyjs)
library(EBImage)
library(fs)
library(shinyFiles)
library(DT)
library(rmarkdown)
library(ggplot2)

shinyAppServer <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2) #file can be up to 50 mb; default is 5 mb
  shinyImageFile <- reactiveValues(shiny_img_origin = NULL, Threshold = NULL)

  #checks radio for file input
  observe({
    #default: upload image
    if(input$radio == 1){
      reset('file1') #allows plot1 to be null when radio is clicked
      # creates a warning to upload correct file
      # otherwise outputs image
      output$plot1 <- renderPlot({
        validate(need(!is.null(input$file1), "Must upload a valid jpg, png, or tiff"))
        if(is.null(input$file1))
          return(NULL)
      })
    }
    if(input$radio == 2){
      # using sample image
      img <- readImage(system.file("images", "sample.TIF", package="MultiFlowExt"))
      shinyImageFile$shiny_img_origin <- img

      shinyImageFile$filename <- "sample.TIF"
      #outputs image to plot1 -- main plot
      output$plot1 <- renderPlot({ display(img, method = "raster") })
    }
  }) # end of observe

  observe({
    input$file1
    input$radio

    # reset plot brush
    session$resetBrush("plot_brush")
  })

  #the datapath is different from the one needed to properly recognize photo
  #so this function renames the file
  renameUpload <- function(inFile){
    if(is.null(inFile))
      return(NULL)

    oldNames <- inFile$datapath
    newNames <- file.path(dirname(inFile$datapath), inFile$name)
    file.rename(from = oldNames, to = newNames)
    inFile$datapath <- newNames

    return(inFile$datapath)
  }

  #if they enter a new file, their file will become the new imageFile
  observeEvent(input$file1, {
    # reseting plots and text messages
    output$plot3 <- renderPlot({})
    output$plot4 <- renderPlot({})
    if(!is.null(shinyImageFile$Threshold))
      shinyImageFile$Threshold <- NULL
    if(!is.null(shinyImageFile$Mean_Intensities))
      shinyImageFile$Mean_Intensities <- NULL
    if(!is.null(shinyImageFile$Median_Intensities))
      shinyImageFile$Median_Intensities <- NULL

    shinyImageFile$filename <- input$file1$name
    img <- readImage(renameUpload(input$file1))
    shinyImageFile$shiny_img_origin <- img
    output$plot1 <- renderPlot({display(img, method = "raster")})
  })

  #prompts shiny to look at recursive crop
  observe({recursiveCrop()})

  #only executes when keep is clicked
  recursiveCrop <- eventReactive(input$keep,{
    isolate({
      p <- input$plot_brush
      img <- shinyImageFile$shiny_img_origin
      if(length(dim(img)) == 2)
        shinyImageFile$shiny_img_origin <- img[p$xmin:p$xmax, p$ymin:p$ymax, drop = FALSE]
      if(length(dim(img)) == 3)
        shinyImageFile$shiny_img_origin <- img[p$xmin:p$xmax, p$ymin:p$ymax, , drop = FALSE]
      output$plot1 <- renderPlot({
        display(shinyImageFile$shiny_img_origin, method = "raster")

        MAX <- dim(shinyImageFile$shiny_img_origin)[1:2]
        colcuts <- seq(1, MAX[1], length.out = input$strips + 1)
        rowcuts <- seq(1, MAX[2], length.out = 2*input$bands) # bands + spaces between bands

        for (x in colcuts) {
          lines(x = rep(x, 2), y = c(1, MAX[2]), col="red")
        }
        for (y in rowcuts) {
          lines(x = c(1, MAX[1]), y = rep(y, 2), col="red")
        }
      })
    })
    session$resetBrush("plot_brush")
  })

  #hides the keep button in the instance in which the user highlighted the plot
  #then clicks on the side so that the brush plot disappears
  #if user clicks keep without a valid brush, there will be errors
  #so we need to hide the button
  observeEvent(is.null(input$plot_brush), {
    shinyjs::hide("keep")
  })

  #creates a clone of the image in the main image viewer
  #shows the user a preview of the cropped image
  #since shinyimg saves every image that is edited, we use a clone
  #so that we aren't saving any of the previews
  #till the user clicks keep
  croppedShiny <- function(image){
    p <- input$plot_brush
    validate(need(p != 'NULL', "Highlight a portion of the photo to see a cropped version!"))
    validate(need(p$xmax <= dim(shinyImageFile$shiny_img_origin)[1],
                  "Highlighted portion is out of bounds on the x-axis of your image 1"))
    validate(need(p$ymax <= dim(shinyImageFile$shiny_img_origin)[2],
                  "Highlighted portion is out of bounds on the y-axis of your image 1"))
    validate(need(p$xmin >= 0,
                  "Highlighted portion is out of bounds on the x-axis of your image 2"))
    validate(need(p$ymin >= 0,
                  "Highlighted portion is out of bounds on the y-axis of your image 2"))
    preview <- shinyImageFile$shiny_img_origin
    if(length(dim(preview)) == 2)
      preview <- preview[p$xmin:p$xmax, p$ymin:p$ymax, drop = FALSE]
    if(length(dim(preview)) == 3)
      preview <- preview[p$xmin:p$xmax, p$ymin:p$ymax, , drop = FALSE]
    display(preview, method = "raster")

    MAX <- dim(preview)[1:2]
    colcuts <- seq(1, MAX[1], length.out = input$strips + 1)
    rowcuts <- seq(1, MAX[2], length.out = 2*input$bands)

    for (x in colcuts) {
      lines(x = rep(x, 2), y = c(1, MAX[2]), col="red")
    }
    for (y in rowcuts) {
      lines(x = c(1, MAX[1]), y = rep(y, 2), col="red")
    }
  }

  #shows a preview of the cropped function
  #shows the keep button (originally hiding)
  output$plot2 <- renderPlot({
    p <- input$plot_brush
    validate(need(p != 'NULL', "Highlight a portion of the photo to see a cropped version!"))
    validate(need(p$xmax <= dim(shinyImageFile$shiny_img_origin)[1],
                  "Highlighted portion is out of bounds on the x-axis of your image 1"))
    validate(need(p$ymax <= dim(shinyImageFile$shiny_img_origin)[2],
                  "Highlighted portion is out of bounds on the y-axis of your image 1"))
    validate(need(p$xmin >= 0,
                  "Highlighted portion is out of bounds on the x-axis of your image 2"))
    validate(need(p$ymin >= 0,
                  "Highlighted portion is out of bounds on the y-axis of your image 2"))

    croppedShiny(shinyImageFile$shiny_img_origin)

    shinyjs::show("keep")
    shinyjs::show("segmentation")
  })

  observe({recursiveSegmentation()})

  #only executes when Apply Segmentation is clicked
  recursiveSegmentation <- eventReactive(input$segmentation,{
    isolate({
      MAX <- dim(shinyImageFile$shiny_img_origin)[1:2]
      colcuts <- seq(1, MAX[1], length.out = input$strips + 1)
      rowcuts <- seq(1, MAX[2], length.out = 2*input$bands)

      segmentation.list <- vector("list", length = input$strips)
      count <- 0
      for(i in 1:input$strips){
        tmp.list <- vector("list", length = 2*input$bands-1)
        for(j in 1:(2*input$bands-1)){
          img <- shinyImageFile$shiny_img_origin
          if(length(dim(img)) == 2)
            img <- img[colcuts[i]:colcuts[i+1], rowcuts[j]:rowcuts[j+1]]
          if(length(dim(img)) == 3)
            img <- img[colcuts[i]:colcuts[i+1], rowcuts[j]:rowcuts[j+1], , drop = FALSE]
          tmp.list[[j]] <- img
        }
        segmentation.list[[i]] <- tmp.list
      }
      shinyImageFile$cropping_grid <- list("columns" = colcuts, "rows" = rowcuts)
      shinyImageFile$segmentation_list <- segmentation.list
      updateTabsetPanel(session, "tabs", selected = "tab2")
    })
  })

  observe({
    input$thresh
    updateNumericInput(session, "selectStrip", max=input$strips)
  })

  observe({input$channel})

  observe({recursiveThreshold()})

  recursiveThreshold <- eventReactive(input$threshold,{
    isolate({
      seg.list <- shinyImageFile$segmentation_list
      i <- input$selectStrip
      if(input$thresh == 2){
        Background <- vector(mode = "list", length = input$bands)
        for(j in 1:input$bands){
          img <- seg.list[[i]][[j]]
          if(colorMode(img) > 0){
            if(input$channel == 1)
              img <- 1-channel(img, "luminance")
            if(input$channel == 2)
              img <- 1-channel(img, "gray")
            if(input$channel == 3)
              img <- 1-channel(img, "red")
            if(input$channel == 4)
              img <- 1-channel(img, "green")
            if(input$channel == 5)
              img <- 1-channel(img, "blue")
          }
          Background[[j]] <- as.numeric(imageData(img))
        }
        Background.Threshold <- quantile(unlist(Background),
                                         probs = input$quantile1/100)
        shinyImageFile$Threshold <- Background.Threshold
        output$plot3 <- renderPlot({
          par(mfcol = c(1, input$bands))
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count <- 0
          for(j in Bands){
            count <- count + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              if(input$channel == 1)
                img <- 1-channel(img, "luminance")
              if(input$channel == 2)
                img <- 1-channel(img, "gray")
              if(input$channel == 3)
                img <- 1-channel(img, "red")
              if(input$channel == 4)
                img <- 1-channel(img, "green")
              if(input$channel == 5)
                img <- 1-channel(img, "blue")
            }
            signal <- imageData(img) > Background.Threshold
            imageData(img) <- signal
            plot(img)
            title(paste0("Line ", count))
          }
        })
        shinyImageFile$Mean_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        shinyImageFile$Median_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        output$plot4 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count <- 0
          for(j in Bands){
            count <- count + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              if(input$channel == 1)
                img <- 1-channel(img, "luminance")
              if(input$channel == 2)
                img <- 1-channel(img, "gray")
              if(input$channel == 3)
                img <- 1-channel(img, "red")
              if(input$channel == 4)
                img <- 1-channel(img, "green")
              if(input$channel == 5)
                img <- 1-channel(img, "blue")
            }
            signal <- imageData(img) > Background.Threshold
            imageData(img) <- (imageData(img) - Background.Threshold)*signal
            shinyImageFile$Mean_Intensities[1,count] <- mean(imageData(img)[signal])
            shinyImageFile$Median_Intensities[1,count] <- median(imageData(img)[signal])
            plot(img)
            title(paste0("Line ", count))
          }
        })
      }
      if(input$thresh == 1){
        Background.Threshold <- numeric(input$bands)
        output$plot3 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              if(input$channel == 1)
                img <- 1-channel(img, "luminance")
              if(input$channel == 2)
                img <- 1-channel(img, "gray")
              if(input$channel == 3)
                img <- 1-channel(img, "red")
              if(input$channel == 4)
                img <- 1-channel(img, "green")
              if(input$channel == 5)
                img <- 1-channel(img, "blue")
            }
            Background.Threshold[count1] <- otsu(img)
            signal <- imageData(img) > Background.Threshold[count1]
            imageData(img) <- signal
            plot(img)
            title(paste0("Line ", count2))
          }
          shinyImageFile$Threshold <- Background.Threshold
        })
        shinyImageFile$Mean_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        shinyImageFile$Median_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        output$plot4 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              if(input$channel == 1)
                img <- 1-channel(img, "luminance")
              if(input$channel == 2)
                img <- 1-channel(img, "gray")
              if(input$channel == 3)
                img <- 1-channel(img, "red")
              if(input$channel == 4)
                img <- 1-channel(img, "green")
              if(input$channel == 5)
                img <- 1-channel(img, "blue")
            }
            thr <- otsu(img)
            signal <- imageData(img) > thr
            imageData(img) <- (imageData(img) - thr)*signal
            shinyImageFile$Mean_Intensities[1,count1] <- mean(imageData(img)[signal])
            shinyImageFile$Median_Intensities[1,count1] <- median(imageData(img)[signal])
            plot(img)
            title(paste0("Line ", count2))
          }
        })
      }
      if(input$thresh == 3){
        Background.Threshold <- numeric(input$bands)
        output$plot3 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              if(input$channel == 1)
                img <- 1-channel(img, "luminance")
              if(input$channel == 2)
                img <- 1-channel(img, "gray")
              if(input$channel == 3)
                img <- 1-channel(img, "red")
              if(input$channel == 4)
                img <- 1-channel(img, "green")
              if(input$channel == 5)
                img <- 1-channel(img, "blue")
            }
            Background.Threshold[count1] <- MultiFlowExt::triangle(img)
            signal <- imageData(img) > Background.Threshold[count1]
            imageData(img) <- signal
            plot(img)
            title(paste0("Line ", count2))
          }
          shinyImageFile$Threshold <- Background.Threshold
        })
        shinyImageFile$Mean_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        shinyImageFile$Median_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        output$plot4 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              if(input$channel == 1)
                img <- 1-channel(img, "luminance")
              if(input$channel == 2)
                img <- 1-channel(img, "gray")
              if(input$channel == 3)
                img <- 1-channel(img, "red")
              if(input$channel == 4)
                img <- 1-channel(img, "green")
              if(input$channel == 5)
                img <- 1-channel(img, "blue")
            }
            thr <- MultiFlowExt::triangle(img)
            signal <- imageData(img) > thr
            imageData(img) <- (imageData(img) - thr)*signal
            shinyImageFile$Mean_Intensities[1,count1] <- mean(imageData(img)[signal])
            shinyImageFile$Median_Intensities[1,count1] <- median(imageData(img)[signal])
            plot(img)
            title(paste0("Line ", count2))
          }
        })
      }
    })
  })

  observe({recursiveData()})

  recursiveData <- eventReactive(input$data,{
    isolate({
      AM <- shinyImageFile$Mean_Intensities
      colnames(AM) <- paste0("Mean", 1:input$bands)
      Med <- shinyImageFile$Median_Intensities
      colnames(Med) <- paste0("Median", 1:input$bands)
      if(input$thresh == 1){
        BG.method <- matrix(c("Otsu", NA), nrow = 1,
                            ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      if(input$thresh == 2){
        BG.method <- matrix(c("quantile", input$quantile1),
                            nrow = 1, ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      seg.list <- shinyImageFile$segmentation_list
      img <- seg.list[[1]][[1]]
      if(colorMode(img) > 0){
        if(input$channel == 1) MODE <- "luminance"
        if(input$channel == 2) MODE <- "gray"
        if(input$channel == 3) MODE <- "red"
        if(input$channel == 4) MODE <- "green"
        if(input$channel == 5) MODE <- "blue"
        DF <- data.frame("File" = shinyImageFile$filename,
                         "Mode" = MODE,
                         "Strip" = input$selectStrip,
                         BG.method, AM, Med,
                         check.names = FALSE)
      }else{
        DF <- data.frame("File" = shinyImageFile$filename,
                         "Mode" = NA,
                         "Strip" = input$selectStrip,
                         BG.method, AM, Med,
                         check.names = FALSE)
      }
      if(inherits(try(IntensData, silent = TRUE), "try-error"))
        IntensData <<- DF
      else
        IntensData <<- rbind(IntensData, DF)

      output$intens <- renderDT({
        DF <- IntensData
        datatable(DF)
      })
      output$plot3 <- renderPlot({})
      output$plot4 <- renderPlot({})
      if(!is.null(shinyImageFile$Threshold))
        shinyImageFile$Threshold <- NULL
      if(!is.null(shinyImageFile$Mean_Intensities))
        shinyImageFile$Mean_Intensities <- NULL
      if(!is.null(shinyImageFile$Median_Intensities))
        shinyImageFile$Median_Intensities <- NULL
    })
  })

  observe({recursiveShowIntensData()})
  recursiveShowIntensData <- eventReactive(input$showIntensData,{
    isolate({
      updateTabsetPanel(session, "tabs", selected = "tab3")
    })
  })

  observe({recursiveDelete()})
  recursiveDelete <- eventReactive(input$deleteData,{
    isolate({
      suppressWarnings(rm(IntensData, pos = 1))
    })
  })

  observe({recursiveDelete2()})
  recursiveDelete2 <- eventReactive(input$deleteData2,{
    isolate({
      suppressWarnings(rm(ExpInfo, pos = 1))
      suppressWarnings(rm(MergedData, pos = 1))
    })
  })

  observe({recursiveDelete3()})
  recursiveDelete3 <- eventReactive(input$deleteData3,{
    isolate({
      suppressWarnings(rm(MergedData, pos = 1))
      suppressWarnings(rm(CalibrationData, pos = 1))
    })
  })

  observe({recursiveRefresh()})
  recursiveRefresh <- eventReactive(input$refreshData,{
    isolate({
      output$intens <- renderDT({
        DF <- IntensData
        datatable(DF)
      })
    })
  })

  observe({recursiveRefresh2()})
  recursiveRefresh2 <- eventReactive(input$refreshData2,{
    isolate({
      output$experiment <- renderDT({
        DF <- MergedData
        datatable(DF)
      })
    })
  })

  observe({recursiveRefresh3()})
  recursiveRefresh3 <- eventReactive(input$refreshData3,{
    isolate({
      output$calibration <- renderDT({
        DF <- CalibrationData
        datatable(DF)
      })
    })
  })
  observeEvent(input$intensFile,{
    output$intens <- renderDT({})
    suppressWarnings(rm(IntensData, pos = 1))
  })
  observeEvent(input$expFile,{
    output$experiment <- renderDT({})
    suppressWarnings(rm(ExpInfo, pos = 1))
    suppressWarnings(rm(MergedData, pos = 1))
  })
  observeEvent(input$prepFile,{
    output$calibration <- renderDT({})
    suppressWarnings(rm(IntensData, pos = 1))
    suppressWarnings(rm(ExpInfo, pos = 1))
    suppressWarnings(rm(MergedData, pos = 1))
  })

  observe({recursiveExpInfo()})

  recursiveExpInfo <- eventReactive(input$expInfo,{
    updateTabsetPanel(session, "tabs", selected = "tab4")
  })

  observe({recursiveUploadIntens()})
  recursiveUploadIntens <- eventReactive(input$intensFile,{
    isolate({
      req(input$intensFile)
      tryCatch(
        DF <- read.csv(input$intensFile$datapath, header = TRUE,
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      IntensData <<- DF
      output$intens <- renderDT({
        datatable(DF)
      })
    })
  })

  observe({recursiveUploadExpFile()})
  recursiveUploadExpFile <- eventReactive(input$expFile,{
    isolate({
      req(input$expFile)
      tryCatch(
        DF <- read.csv(input$expFile$datapath, header = TRUE,
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      ExpInfo <<- DF
      MergedData <<- DF
      suppressWarnings(rm(CalibrationData, pos = 1))
      output$calibration <- renderDT({})

      output$experiment <- renderDT({
        datatable(DF)
      })
    })
  })

  observe({recursiveUploadPrepFile()})
  recursiveUploadPrepFile <- eventReactive(input$prepFile,{
    isolate({
      req(input$prepFile)
      tryCatch(
        DF <- read.csv(input$prepFile$datapath, header = TRUE,
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      CalibrationData <<- DF
      output$calibration <- renderDT({
        datatable(DF)
      })
    })
  })

  observe({recursiveMerge()})
  recursiveMerge <- eventReactive(input$merge,{
    isolate({
      DF <- merge(ExpInfo, IntensData,
                  by.x = input$mergeExp,
                  by.y = input$mergeIntens, all = TRUE)

      MergedData <<- DF
      CalibrationData <<- DF

      output$experiment <- renderDT({
        datatable(DF)
      })
    })
  })

  observe({recursivePrepare()})
  recursivePrepare <- eventReactive(input$prepare,{
    DF <- MergedData
    CalibrationData <<- DF

    output$calibration <- renderDT({
      datatable(DF)
    })

    updateTabsetPanel(session, "tabs", selected = "tab5")
  })

  observe({recursiveCombReps()})
  recursiveCombReps <- eventReactive(input$combReps,{
    isolate({
      Cols <- c(grep("Mean", colnames(MergedData)),
                grep("Median", colnames(MergedData)))
      RES <- NULL
      if(input$colorsBands > 1){
        DF <- MergedData[,c(input$combRepsColSI, input$combRepsColCL)]
        DFuni <- DF[!duplicated(DF),]
        for (i in 1:nrow(DFuni)) {
          sel <- DF[,1] == DFuni[i,1] & DF[,2] == DFuni[i,2]
          tmp <- MergedData[sel, ]
          tmp2 <- tmp[1, ]
          if (input$radioReps == 1) #mean
            tmp2[, Cols] <- colMeans(tmp[, Cols], na.rm = TRUE)
          if (input$radioReps == 2) #median
            tmp2[, Cols] <- apply(tmp[, Cols], 2, median, na.rm = TRUE)
          RES <- rbind(RES, tmp2)
        }
      }else{
        DF <- MergedData[,input$combRepsColSI]
        for (spl in unique(MergedData[, input$combRepsColSI])) {
          tmp <- MergedData[DF == spl, ]
          tmp2 <- tmp[1, ]
          if (input$radioReps == 1) #mean
            tmp2[, Cols] <- colMeans(tmp[, Cols], na.rm = TRUE)
          if (input$radioReps == 2) #median
            tmp2[, Cols] <- apply(tmp[, Cols], 2, median, na.rm = TRUE)
          RES <- rbind(RES, tmp2)
        }
      }
      rownames(RES) <- 1:nrow(RES)
      RES <- RES[order(RES[,input$combRepsColSI]),]
      CalibrationData <<- RES

      output$calibration <- renderDT({
        datatable(RES)
      })
    })
  })

  observe({recursiveReshapeWide()})

  recursiveReshapeWide <- eventReactive(input$reshapeWide,{
    isolate({
      rm.file <- (colnames(CalibrationData) != colnames(MergedData)[1] &
                    colnames(CalibrationData) != input$reshapeCol)
      DF.split <- split(CalibrationData[,rm.file], CalibrationData[,input$reshapeCol])

      N <- length(unique(CalibrationData[,input$reshapeCol]))
      if(N > 1){
        DF <- DF.split[[1]]
        Cols <- c(grep("Mean", colnames(DF)),
                  grep("Median", colnames(DF)))
        Cols <- c(Cols, which(colnames(DF) == input$combRepsColSI))
        for(i in 2:N){
          DF <- merge(DF, DF.split[[i]][,Cols], by = input$combRepsColSI,
                      suffixes = paste0(".", names(DF.split)[c(i-1,i)]))
        }
        CalibrationData <<- DF
      }else{
        DF <- CalibrationData
      }

      output$calibration <- renderDT({
        datatable(DF)
      })
    })
  })

  observe({recursiveRunCali()})

  recursiveRunCali <- eventReactive(input$runCali,{
    isolate({
      if(is.integer(input$folder)){
        PATH.OUT <- paste0(fs::path_home(), "/MultiFlowExt")
        fs::dir_create(PATH.OUT)
        output$folder <- renderPrint({
          paste0("Folder for Results: ", PATH.OUT)
        })
      }else
        PATH.OUT <- parseDirPath(c(wd=fs::path_home()), input$folder)

      FORMULA <- input$formula

      if(inherits(try(as.formula(FORMULA), silent = TRUE), "try-error")){
        output$modelSummary <- renderPrint({ as.formula(FORMULA) })
        updateTabsetPanel(session, "tabs", selected = "tab6")
        return(NULL)
      }

      SUBSET <- input$subset
      save(CalibrationData, FORMULA, SUBSET, PATH.OUT,
           file = paste0(PATH.OUT,"/CalibrationData.RData"))

      file.copy(from = system.file("markdown", "CalibrationAnalysis.Rmd",
                                   package="MultiFlowExt"),
                to = paste0(PATH.OUT, "/CalibrationAnalysis.Rmd"))
      rmarkdown::render(input = paste0(PATH.OUT, "/CalibrationAnalysis.Rmd"),
                        output_file = paste0(PATH.OUT, "/CalibrationAnalysis.html"))

      load(file = paste0(PATH.OUT, "/CalibrationResults.RData"))

      AIC.fit.sum <- summary(AIC.fit)
      R2 <- round(AIC.fit.sum$r.squared, 3)
      adj.R2 <- round(AIC.fit.sum$adj.r.squared, 3)
      DF <- data.frame(Observed = AIC.fit$model[,1],
                       Fitted = fitted(AIC.fit))
      output$modelSummary <- renderPrint({ AIC.fit })

      output$plot5 <- renderPlot({
        ggplot(DF, aes(x = Observed, y = Fitted)) +
          geom_point() + geom_abline(slope = 1, intercept = 0) +
          xlab("Observed value") + ylab("Fitted values") +
          annotate("text",  x=min(DF$Observed), y = max(DF$Fitted),
                   label = substitute(paste(R^2, " = ", R2, ", adj. ", R^2, " = ", adj.R2),
                                      list(R2 = R2, adj.R2 = adj.R2)),
                   vjust=1, hjust=0, size = 5)

      })
      output$LOB <- renderText({
        paste0("Limit of Blank (LOB): ", signif(LOB, 3))
      })
      output$LOD <- renderText({
        paste0("Limit of Detection (LOD): ", signif(LOD, 3))
      })
      output$LOQ <- renderText({
        paste0("Limit of Quantification (LOQ): ", signif(LOQ, 3))
      })

      updateTabsetPanel(session, "tabs", selected = "tab6")
    })
  })

  observe({recursiveOpenReport()})

  recursiveOpenReport <- eventReactive(input$openReport,{
    isolate({
      PATH.OUT <- parseDirPath(c(wd=fs::path_home()), input$folder)
      browseURL(paste0(PATH.OUT, "/CalibrationAnalysis.html"),
                browser = getOption("browser"))
    })
  })

  #creates the textbox below plot2 about the plot_brush details and etc
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  output$thresh <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Threshold(s): ", paste0(signif(shinyImageFile$Threshold, 4), collapse = ", "))
  })
  output$meanIntens <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Mean intensities: ", paste0(signif(shinyImageFile$Mean_Intensities, 4), collapse = ", "))
  })
  output$medianIntens <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Median intensities: ", paste0(signif(shinyImageFile$Median_Intensities, 4), collapse = ", "))
  })
  output$intens <- renderDT({
    DF <- IntensData
    datatable(DF)
  })
  output$folder <- renderPrint({
    paste0("Folder for Results: ", parseDirPath(c(wd=fs::path_home()), input$folder))
  })

  #allows user to download data
  output$downloadData <- downloadHandler(
    filename = "IntensityData.csv",
    content = function(file) {
       write.csv(IntensData, file, row.names = FALSE)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = "MergedData.csv",
    content = function(file) {
       write.csv(MergedData, file, row.names = FALSE)
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = "CalibrationData.csv",
    content = function(file) {
      write.csv(CalibrationData, file, row.names = FALSE)
    }
  )
  shinyDirChoose(input, 'folder',
                 roots=c(wd=fs::path_home()),
                 filetypes=c(''))


  #When user clicks the return to command line button
  #stops the shiny app
  # prevents user from quitting shiny using ^C on commandline
  observe({recursiveStop()})

  recursiveStop <- eventReactive(input$stop,{
    isolate({
      suppressWarnings(rm(IntensData, pos = 1))
      suppressWarnings(rm(ExpInfo, pos = 1))
      suppressWarnings(rm(MergedData, pos = 1))
      suppressWarnings(rm(CalibrationData, pos = 1))
      stopApp()
    })
  })
  #//////// END OF CODE FOR STOP SHINY APP ///////////////
}
