library(shiny)
library(shinyjs)
library(jpeg)
library(png)


url <- "https://twitter.com/intent/tweet?text=arka%20planmis%20artik%20yok&url=https://batu6.shinyapps.io/app_bgremover/"

ui <- fluidPage(
    
  useShinyjs(), # allows nice stuff like disabling and enabling buttons.
  
  # App title -----
  titlePanel("Remove Backgrounds with bgRemoveR"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
             
  # Sidebar to demonstrate various slider options ----
  sidebarPanel(width = 3,
                          
  #bg color of side panel
  tags$style(".well {background-color:#039f98;}"),

              # file upload
              fileInput("files" , tags$h4(tags$strong("Upload an image")),
                        accept = c(
                          "image/jpeg",
                          "image/png"),
                        multiple = T
                      ),
  
     
      #Show pixel color hex number
      h4("pixel color:"),
      verbatimTextOutput("pixelColor"),
      
      #Show pixel color
      plotOutput("plotColor", height = "1%"),
      
      tags$hr(),
      
      h5("Works 10% of the time:"),
      h5("Seçtiğiniz renge yakın renkleri de kaldırmak için bunu deneyin:"),
      sliderInput("degree", label = "Remove percentage", min = 0 , max = 200, value = 0 ), 
      
      tags$hr(),
      
                #Düğmeler
                disabled(actionButton("remove", "Remove the selected color")),
                disabled(downloadButton("downloadData", "Download")),
                disabled(downloadButton("downloadZIP", "Download zip")),
          
      tags$hr(),
      
      # notes about app
      tags$p(tags$em("-- PNG/JPEG uzantılı resimlerdeki istenilen renkleri saydam yap", style = "color:lightgray;", style = "font-size:14px;")),
      
      tags$p(tags$em("-- Resimdeki bir rengin renk kodunu öğren", style = "color:lightgray;", style = "font-size:14px;")),
          
      tags$p(tags$em("-- Birden fazla resmi aynı anda düzenle", style = "color:lightgray;", style = "font-size:14px;")),
      
             # Create url with the 'twitter-share-button' class
             tags$a(href=url, "Tweet", class="twitter-share-button" ),
             # Copy the script from https://dev.twitter.com/web/javascript/loading into your app
             # You can source it from the URL below. It must go after you've created your link
             includeScript("http://platform.twitter.com/widgets.js"),
      
      
      tags$footer(tags$em("-Batuhan Akçabozan tarafından hayata geçirildi.- Packages Used: 'shiny', 'shinyjs', 'jpeg' and 'png' "), 
                  align = "right", style = "font-size:12px;" )
      ),
  
    
    
      mainPanel(
        
        h4("After removal your image will appear here"),
        #Output image
        
        fluidRow(
          column(12,uiOutput(outputId = "img2") ,div(style = "height:90px;"))),
        
        hr(),
        h4("Click on image to define the colors to be removed"),
        #Input image
        
        fluidRow(
          uiOutput(outputId = "img1"))
         
      
      )
    
  )
)




server <- function(input, output, session){

  #Gets the input file info to a table
  output$files <- renderTable(input$files)
  
  #input files as reactive objects
  files <- reactive({
    files <- input$files
    files$datapath <- gsub("\\\\", "/", files$datapath)
    files
  })
  
  # Input image
  # 1st part
  observe({  

  output$img1 <- renderUI({
    
    if(is.null(input$files))return(NULL)
    
    #print(c(width = dim(rv$y[[1]])[2]/8, height = dim(rv$y[[1]])[1]/8 ) )
    #print("a")
    
    image_output_list <- 
      lapply(1:nrow(files()),
             function(i)
             {
               imagename = paste0("image", i)
               imageOutput(imagename, click = "img_click",width = dim(rv$y[[1]])[2], height = dim(rv$y[[1]])[1]) 
               
             })
  
    do.call(tagList, image_output_list)
    #https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny
    
  })
  
  },priority = 0)
  
  # 2nd part, this part plots the image
  observe({
    if(is.null(input$files)) return(NULL)
    
    for (i in 1:1)
    {
      #print(i)
      local({
        my_i <- i
        imagename = paste0("image", my_i)
        #print(imagename)
        output[[imagename]] <- 
          renderImage({
            
            list(src = files()$datapath[my_i],
                 alt = "Image failed to render",width = dim(rv$y[[1]])[2], height = dim(rv$y[[1]])[1])
          }, deleteFile = FALSE)
        

      })
    }
  }, priority = -1)
  
  
  # Show pixel hex code
  output[["pixelColor"]] <- renderPrint({
    rv$colordisplay 
  })
  
  
  # 2nd image
  # plotting 2nd plot after removal
  observeEvent(input$remove,{
    #print("selam")
    # 1st part
    output$img2 <- renderUI({
      if(is.null(input$files)) return(NULL)
      if(is.null(input$remove)) return(NULL)

      
      image_outputA_list <- 
        lapply(1,
               function(i)
               {
                 imagename = paste0("imageA", i)
                 imageOutput(imagename)
                 
               })
      #print("selam2")
      
      do.call(tagList, image_outputA_list)
      #https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny
      
    })
  }, priority = -2)
    
  
  observe({
    
    #print("buraaaaaaaaaaa211111111")
    #print(rv$count2)
    
    # 2nd part, this part plots the image
      if(is.null(input$files)) return(NULL)
     if(input$remove == 0) return(NULL)
    if(rv$count2 >= 1) {
      
      #print("buraaaaaaaaaaa")
      
      for (i in 1:1)
      {
        print(i)
        local({
          my_i <- i
          imagename = paste0("imageA", my_i)
          print(imagename)
          output[[imagename]] <- 
            renderImage({
              
              
              #print(c(width = dim(rv$y[[1]])[2]/8, height = dim(rv$y[[1]])[1]/8 ) )
              
              
              
              outfile <- tempfile(fileext = '.png')
              # Generate the PNG
                

              png(outfile,width = round( (dim(rv$y[[1]])[2]/dim(rv$y[[1]])[1])*550 ), height = 550 , units = "px",res = 300, bg = "transparent" )
            
              
              par(mar=c(1,1,1,1)) # for figure margins too large error
              plot.new() 
              rasterImage(rv$myImg[[1]],0,0,1,1)
              dev.off() 
              
              
              list(src = outfile,
                   alt = "Image failed to render")
            }, deleteFile = FALSE)
          
          
        })
      }
    }
    }, priority = -5)


  
  
  # Object for image parameters
  rv <- reactiveValues()
  
          # counter resets after each input.
          observeEvent(input$files,{
            rv$count <- 0
            rv$count <- rv$count + 1
            
            print("first")
            
            rv$count2 <- 0
          }, priority = 6)
          
          # counter for observing change in remove button
          observeEvent(input$remove,{
            rv$count2 <- rv$count2 + 1
          }, priority = 6)
  
          
  #Removal of bg
  observeEvent(input$files,{
    
    
    # First time a file loaded and remove should be not equal to 0
    if(rv$count == 1){
      
          rv$count <- 2
          
          rv$y <- list()
          rv$y2 <- list()
          rv$val <- list()
  
      
      
          for (i in 1:nrow(files())){
        
      
                 if (grepl("png", x = files()[i,1], ignore.case = T)){
                   
                   
                   # I re-adjusted the directory for readPNG
                   rv$y[[i]] <- readPNG(gsub("\\/", "//",  files()[i,4]))
                   
                 } else {
                 
                   
                   rv$y[[i]]  <- readJPEG(gsub("\\/", "//",  files()[i,4]) )
                 }
      
      
                   # For images that dont have "A" in RGBA. I add that dimension.
                    # If it is only RGB there are 3 dimensions in image array.
                     # I make all Alphas 1 for those kind of files
                 if(dim(rv$y[[i]])[3] == 3){
                   rv$y2[[i]] <- array(NA, dim=c(dim(rv$y[[i]] )[1], dim(rv$y[[i]] )[2], dim(rv$y[[i]] )[3] + 1))
                   rv$y2[[i]][,,-4] <- rv$y[[i]] 
                   rv$y2[[i]][,,4] <- 1
                   rv$y[[i]] <- rv$y2[[i]]
                 }
      
                  # Converting to RGB
                  rv$val[[i]] <- rgb( rv$y[[i]][,,1], rv$y[[i]][,,2], rv$y[[i]][,,3], rv$y[[i]][,,4])
                  }
                  print("sec")
          
          }
      
  }, priority = 6)
      
      
      
    observeEvent(input$remove,{
        
        rv$myImg <- list()
        
        if(rv$count2 >= 1){
          
          for (i in 1:nrow(files())){
            
            
            if(isolate({input$degree}) == 0){
            # Making transparent the given color
            rv$y[[i]][,,4][which(rv$val[[i]] == rv$colordisplay  ) ] <- 0
            }
            
            if(isolate({input$degree}) != 0){
              # Making transparent the given color
              rv$y[[i]][,,4][which(rv$val[[i]] %in% isolate({rv$data})  ) ] <- 0
              
              #print(rv$y[[i]][,,4][which(rv$val[[i]] %in% isolate({rv$data})  ) ] )
            }
            
            # Regenerating RGB
            rv$val[[i]] <- rgb( rv$y[[i]][,,1], rv$y[[i]][,,2], rv$y[[i]][,,3], rv$y[[i]][,,4])
            
            # Final image matrix
            rv$myImg[[i]] <- matrix( rv$val[[i]], dim(rv$y[[i]])[1], dim(rv$y[[i]])[2] )
            
          }
        
        }
        
        
      },priority = 5)
      
  
                # renk kaldırıldı bildirimi
                observeEvent(input$remove,{
                  showNotification("renk kaldırıldı yaşasın!!",duration = 2, type = "message")
                })
                        
                                       # Düğme aktivasyonu ya da deaktivasyonu
                                       observeEvent(input$files,{
                                         
                                       if (nrow(files()) > 1){
                                         
                                         shinyjs::disable("downloadData")
                                         shinyjs::enable("downloadZIP")
                                       }
                                       },ignoreInit = T)
                                       
                                       observeEvent(input$files,{
                                         
                                         if (nrow(files()) == 1){
                                           
                                           shinyjs::enable("downloadData")
                                           shinyjs::disable("downloadZIP")
                                           
                                         }
                                       },ignoreInit = T)
                                       
                                       observeEvent(input$img_click,{

                                         
                                         if (nrow(files()) >= 1){
                                           
                                           shinyjs::enable("remove")
                                         
                                           
                                         }
                                       },ignoreInit = T, priority = 5)

            
                # Download button single
                output$downloadData <- downloadHandler(
                  
                  filename = function(){
                    paste0("new_", stringr::str_replace(files()[1,1], "(?i).JP.+", ".png")  , sep = "") # if it is jpeg it changes it to png.
                    # If png nothing changes.
                  },
                  
                  content = function(file) {  # gets the image width info but somehow dont work.
                    png(file,width = dim(rv$y[[1]])[2]*4, height = dim(rv$y[[1]])[1]*4 , units = "px",res = 300,  bg = "transparent" )
                    par(mar=c(1,1,1,1)) # for figure margins too large error
                    plot.new() 
                    rasterImage(rv$myImg[[1]],0,0,1,1)
                    dev.off() 
                  }
                  
                )
               
               
               # Download button multiple
               output$downloadZIP <- downloadHandler(
                 
                 filename = function(){
                   "images.zip" 
                 },
                 
                 content = function(file){
                   #go to a temp dir to avoid permission issues
                   owd <- setwd(tempdir())
                   on.exit(setwd(owd))
                   filesZip <- NULL;
                   
                   #loop through the sheets
                   for (i in 1:nrow(files())){
                     
                     fileName <- paste0("new_", stringr::str_replace(files()[i,1], "(?i).JP.+", ".png")  , sep = "")
                     
                     png(fileName,width = dim(rv$y[[i]])[2]*4, height = dim(rv$y[[i]])[1]*4 , units = "px",res = 300, bg = "transparent" )
                     par(mar=c(1,1,1,1)) # for figure margins too large error
                     plot.new() 
                     rasterImage(rv$myImg[[i]],0,0,1,1)
                     dev.off() 
                     
                     
                     filesZip <- c(fileName,filesZip)
                   }
                   #create the zip file
                   zip(file,filesZip)
                 }
               )
     

     
    # Mouse click and color info retrival
    observeEvent(c(input$files, input$img_click),{
      
      
     if(is.null(input$img_click)) return(NULL)
    
      if (grepl("png", x = files()[1,1], ignore.case = T) ){
       
        # Read image
        rv$ydisplay <- readPNG(gsub("\\/", "//",  files()[1,4]))
        
        
      } else {
        #print(gsub("\\/", "//",  files()[1,4]))
        #print(files()[1])
        # Read image
        rv$ydisplay <- readJPEG(gsub("\\/", "//",  files()[1,4]))
        
      }
    
      
      
      
            # For images that dont have "A" in RGBA. I add that dimension.
            if(dim(rv$ydisplay)[3] == 3){
              rv$ydisplay2 <- array(NA, dim=c(dim(rv$ydisplay)[1], dim(rv$ydisplay)[2], dim(rv$ydisplay)[3] + 1))
              rv$ydisplay2[,,-4] <- rv$ydisplay 
              rv$ydisplay2[,,4] <- 1
              rv$ydisplay <- rv$ydisplay2
            }
      
      # Get RGB
      rv$valdisplay <- rgb( rv$ydisplay[,,1], rv$ydisplay[,,2], rv$ydisplay[,,3], rv$ydisplay[,,4])
  
     
      
      # Image Matrix
      rv$valdisplay <- matrix( rv$valdisplay, dim(rv$ydisplay)[1],dim(rv$ydisplay)[2])
      
      # According to the displayed image's pixel info I upscale the image, by the ratio to original pixel size.
        # I first duplicate each row with the given ratio.
          # Then I do that for each column.
      
      # This part is now unnecessary as I used dim(rv$y[[1]])[1] which kind of matched the pixel size. But making the image smaller was still not possible
     #print(c(input$img_click$range$bottom, dim(rv$ydisplay)[1])) 
     ##
     #print(c(input$img_click$range$right, dim(rv$ydisplay)[2])) 
     #rv$valdisplay <- apply(rv$valdisplay, 1, FUN = function(x)rep(x, each = round((input$img_click$range$bottom / dim(rv$ydisplay)[1]))  ))
     # 
     #rv$valdisplay <- apply(rv$valdisplay, 2, FUN = function(x)rep(x, each = round((input$img_click$range$right / dim(rv$ydisplay)[2]))  ))

      
      
      # Display clicked color.
      output$plotColor <- renderPlot({
        
      #  print(dim(rv$valdisplay)[1])
      #  print(c(input$img_click$range$right, input$img_click$x))
      #  
        #if(is.null(input$img_click)) return(NULL)
        
        # This part is for if one wants to change image. Then x,y coordinates can cause issue. 
        # Still it sometimes gives a warning. but it is ok I think. It is due to latency.
       if (dim(rv$valdisplay)[1] >= round(input$img_click$y) & dim(rv$valdisplay)[2] >= round(input$img_click$x)  ){

          # Get the color value for the clicked coordinate
          rv$colordisplay <- rv$valdisplay[round(input$img_click$y), round(input$img_click$x)]
        }
        
        # Plot an empty region and rectangle with the clicked color
        par(mar=c(1,1,1,1))
        plot(1:10, type= "n", xlab = "", ylab = "", axes = F)
        rect(1, 1, 10, 10,col = rv$colordisplay, border = "black", lty = 2)
        
        
        
      },bg = "transparent",  height = 100, width = 100) # to make the background transparent.
      
    },priority = 6)
    
    
  

                    observeEvent(input$remove ,{
                    ###
                    
                    if(input$degree == 0) return(NULL)
                    
                    
                    # tried to gather as much as color as ı could
                    colfuncw <- colorRampPalette(c( rv$colordisplay,"#FFFFFFFF"))
                    colfuncbl <- colorRampPalette(c( rv$colordisplay,"#000000FF"))
                    colfuncr <- colorRampPalette(c( rv$colordisplay,"red"))
                    colfuncg <- colorRampPalette(c( rv$colordisplay,"green"))
                    colfuncb <- colorRampPalette(c( rv$colordisplay,"blue"))
                    colfuncm <- colorRampPalette(c( rv$colordisplay,"magenta"))
                    colfuncc <- colorRampPalette(c( rv$colordisplay,"cyan"))
                    colfuncy <- colorRampPalette(c( rv$colordisplay,"yellow"))
                    
                    
                    
                    rv$data <- unique(c(colfuncw(500)[1:isolate({input$degree})], colfuncbl(500)[1:isolate({input$degree})],
                                 colfuncr(500)[1:isolate({input$degree})],
                                 colfuncg(500)[1:isolate({input$degree})],
                                 colfuncb(500)[1:isolate({input$degree})],
                                 colfuncm(500)[1:isolate({input$degree})],
                                 colfuncc(500)[1:isolate({input$degree})],
                                 colfuncy(500)[1:isolate({input$degree})]))
                    
                    rv$data <- paste0(rv$data, "FF")
                    
              
                    },ignoreInit = T, priority = 6)
                  
  
}

shinyApp(ui, server)



