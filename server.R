#
# Copyright © 2017, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2017 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.


# This is the server side of the Shiny app to transform CellSet output files (XLS)
# into one single RSML file that can be read by PAGE-Root



library(shiny)

options(shiny.maxRequestSize=30*1024^2) 

shinyServer(
  
  function(input, output, clientData, session) {  
    
    rs <- reactiveValues(global=NULL, 
                         local=NULL, 
                         direction=NULL, 
                         shape=NULL, 
                         genotypes=NULL, 
                         dates=NULL,
                         rois = NULL,
                         profiles = NULL,
                         histogram = NULL)
    
    # For the root paramerers
    observe({
 
      
    })
       
 
    
    observe({
      if(is.null(rs$global)){return()}
      sel = unique(rs$global$genotype)
      updateSelectInput(session, "genotype", choices = sel, selected = sel[1])
    })  
    
    observe({
      if(is.null(rs$global)){return()}
      sel = unique(rs$global$root[rs$global$genotype == input$genotype])
      updateSelectInput(session, "root", choices = sel, selected = sel[1])
    })  
    # observe({
    #   if(is.null(rs$global)){return()}
    #   sel = unique(rs$global$variable)
    #   updateSelectInput(session, "variable", choices = sel, selected = sel[1])
    # })      
    
    observe({
      if(is.null(rs$dates)){return()}
      updateSliderInput(session, "date", min=1, max=length(rs$dates), value=rs$dates[1])
    }) 
    
    
    observeEvent(input$load_data, {
            
        #------------------------------------------------------
        # LOAD THE USER DATA
        #------------------------------------------------------

        # Load datafiles
        pathData <- paste0(input$dirPath, "/")
        
        withProgress(message = 'Loading data', {

          # # LOAD THE DIFFERENT DATAFILE GENERATED WITH GLORIA
          # global <- fread("/Users/g.lobet/Desktop/Work/HL_Exp6//root-data-global.csv")
          # local1 <- fread("/Users/g.lobet/Desktop/Work/HL_Exp6/root-data-local.csv")
          # rois <- fread("/Users/g.lobet/Desktop/Work/HL_Exp6/root-data-roi.csv")
          # genotypes <- read.csv("/Users/g.lobet/Desktop/Work/HL_Exp6/genotypes.csv")
          # 
          inGlobal <- input$global_file
          inLocal <- input$local_file
          # inDir <- input$dir_file
          inROI <- input$roi_file
          inGen <- input$genotype_file
          
          if(!is.null(inGlobal)) global <- fread(inGlobal$datapath)
          if(!is.null(inLocal)) local1 <- fread(inLocal$datapath)
          if(!is.null(inGen)) genotypes <- read.csv(inGen$datapath)
          if(!is.null(inROI)) rois <- fread(inROI$datapath)
          # if(!is.null(inDir)) direction <- fread(inDir$datapath)
          
        })

        withProgress(message = 'Processing data', {
          
          # PROCESS THE NAMES AND GENOTYPES
          for(i in c(1:nrow(genotypes))){
            id <- as.character(genotypes$id[i])
            global$genotype[grepl(id, global$image)] <- as.character(genotypes$genotype[i])
            local1$genotype[grepl(id, local1$image)] <- as.character(genotypes$genotype[i])
          }
          

          
          # PROCESS THE DATES
          sps <- strsplit(global$image, "-")
          global$date <- paste(unlist(lapply(sps, `[[`, 3)),unlist(lapply(sps, `[[`, 4)),unlist(lapply(sps, `[[`, 5)),sep="-")
          global$root <- unlist(lapply(sps, `[[`, 2))
          sps <- strsplit(local1$image, "-")
          local1$date <- paste(unlist(lapply(sps, `[[`, 3)),unlist(lapply(sps, `[[`, 4)),unlist(lapply(sps, `[[`, 5)),sep="-")
          local1$root <- unlist(lapply(sps, `[[`, 2))

          dates <- unique(global$date)
          
          local1$rangle <- round(local1$angle)
          hist1 <- ddply(local1, .(date, genotype, rangle), summarize, n=sum(length/100))
          
          histogram <- NULL
          for(i in c(1:nrow(hist1))){
            histogram <- rbind(histogram, data.frame(angle=rep(hist1$rangle[i], hist1$n[i]),
                                                     genotype=rep(hist1$genotype[i], hist1$n[i]),
                                                     date=rep(hist1$date[i], hist1$n[i])))
          }
          
          # Here, the angle is ponderated by the length of the segment, to account for longer semgent in the mean computation
          local2 <- ddply(local1, .(image, type), summarise, angle=sum(angle*length)/sum(length), n_segments=length(length), length=sum(length))
          # length2 <- ddply(local1, .(image, type), summarise, local_length=sum(length))
          
          
          # Create the profile database
          profiles <- local2[local2$type != "total",]
          sps <- strsplit(profiles$image, "-")
          profiles$date <- paste(unlist(lapply(sps, `[[`, 3)),unlist(lapply(sps, `[[`, 4)),unlist(lapply(sps, `[[`, 5)),sep="-")
          profiles$root <- unlist(lapply(sps, `[[`, 2))
          for(i in c(1:nrow(genotypes))){
            id <- as.character(genotypes$id[i])
            profiles$genotype[grepl(id, profiles$image)] <- as.character(genotypes$genotype[i])
          }
          profiles$depth <- as.numeric(gsub("layer_", "", profiles$type))
          profiles <- melt(profiles, id.vars = c("image", "genotype", "date", "root", "type", "depth"))
          
          
          # global <- merge(global, direction1, by="image")
          global <- merge(global, local2[local2$type == "total",], by="image")
          # global <- merge(global, length2, by="image")
          
          global <- global[,-c("type", "lenght")]
          global <- melt(global, id.vars = c("image", "genotype", "date", "root"))
          
          rs$global <- global
          rs$local <- local1[local1$type == "total",]
          # rs$direction <- direction
          rs$dates <- dates
          rs$genotypes <- genotypes
          rs$rois <- rois
          rs$profiles <- profiles
          rs$histogram <- histogram
        })
    })
        
    
    output$global_data <- renderTable({
      if(is.null(rs$global)){return()}
      rs$global
    })    
    output$download_global_data <- downloadHandler(
      filename = function() {"global_results.csv"},
      content = function(file) {
        write.csv(rs$global, file)
      }
    ) 
    

    output$profile_data <- renderTable({
      if(is.null(rs$profiles)){return()}
      rs$profiles
    })    
    output$download_profile_data <- downloadHandler(
      filename = function() {"profile_results.csv"},
      content = function(file) {
        write.csv(rs$profiles, file)
      }
    )  
        
    output$local_data <- renderTable({
      if(is.null(rs$local)){return()}
      rs$local
    })    
    output$download_local_data <- downloadHandler(
      filename = function() {"local_results.csv"},
      content = function(file) {
        write.csv(rs$local, file)
      }
    )  
    
    
    output$hist_data <- renderTable({
      if(is.null(rs$histogram)){return()}
      rs$histogram
    })    
    output$download_hist_data <- downloadHandler(
      filename = function() {"histogram_results.csv"},
      content = function(file) {
        write.csv(rs$histogram, file)
      }
    )      
    
    
    # ----------------------------------------------------------------
    # Plot the root systems
    # ----------------------------------------------------------------
    output$rootPlot1 <- renderPlot({
      plot <- ggplot() +  theme_classic()
      if(is.null(rs$local)){return(plot)}
      
      temp2 <- rs$rois[grepl(input$root, rs$rois$image),]
      temp2 <- temp2[grepl(rs$dates[input$date], temp2$image),]
      temp3 <- temp2[temp2$type == "convex",]
      # temp2 <- temp2[temp2$type == "shape",]
      temp <- rs$local[grepl(input$root, rs$local$image),]
      temp1 <- temp[temp$date == rs$dates[input$date],]

      temp3$atan <- atan2(temp3$y - mean(temp3$y), temp3$x - mean(temp3$x))
      temp3 <- temp3[order(temp3$atan),]
      
      
      plot <- ggplot(data=temp1, aes(x, -y)) +
        geom_spoke(aes(angle=angle, radius = 0.5)) +
        # geom_polygon(data=temp2, aes(x, -y), colour="green", fill=NA) + 
        geom_polygon(data=temp3, aes(x, -y), colour="blue", fill=NA) + 
        xlim(range(0,20)) + 
        ylim(range(0,-35)) + 
        theme_classic() + 
        coord_fixed()
      
      plot    
    })  
    
    
    
    # ----------------------------------------------------------------
    # Plot the length
    # ---------------------------------------------------------------- 
    
    output$sizePlot <- renderPlotly({
      plot <- ggplot() +  theme_classic()
      if(is.null(rs$global)){return(plot)}
      
    
      temp <- rs$global[rs$global$root == input$root & 
                          rs$global$date == rs$dates[input$date] &
                          rs$global$variable == input$variable,]
      temp1 <- rs$global[rs$global$variable == input$variable,]
      # temp1 <- global[global$variable == input$variable,]
      
      if(!input$mean){
      plot <- ggplot(data=temp1, aes(date, value, colour=factor(genotype), group=root)) +
        geom_line() +
        geom_vline(xintercept=input$date+1, lty=2) +
        geom_point(data=temp, aes(input$date+1, value), size=4) + 
        geom_point(data=temp, aes(input$date+1, value), size=2, colour="white") + 
        geom_line() +
        xlab("Date") + 
        ylab(input$variable) + 
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }else{
        plot <- ggplot(data=temp1, aes(date, value, colour=factor(genotype), group=genotype)) +
          stat_smooth() +
          geom_vline(xintercept=input$date, lty=2) +
          geom_point(data=temp, aes(input$date, value), size=4) + 
          geom_point(data=temp, aes(input$date, value), size=2, colour="white") + 
          xlab("Date") + 
          ylab(input$variable) + 
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }


      
      ggplotly(plot)
      })  
    
    output$histPlot <- renderPlotly({
      plot <- ggplot() +  theme_classic()
      if(is.null(rs$histogram)){return(plot)}
      
      temp <- rs$histogram[rs$histogram$date == rs$dates[input$date],]
      
      plot <- ggplot(temp, aes(angle, colour=genotype)) + 
        geom_density(size=1.2) + 
        theme_classic() 
      
      ggplotly(plot)
    })
    
    output$profilePlot <- renderPlotly({
      plot <- ggplot() +  theme_classic()
      if(is.null(rs$profiles)){return(plot)}
      
      
      # temp1 <- global[global$variable == input$variable,]
      
      if(!input$mean){
        temp <- rs$profiles[rs$profiles$root == input$root & rs$profiles$variable == input$variable2,]
        plot <- ggplot(data=temp, aes(-depth, value, colour=factor(date), group=date)) +
          geom_line() +
          geom_point(size=2) + 
          # geom_point(size=2, colour="white") + 
          ylab(input$variable2) + 
          xlab("Depth layer") + 
          theme_classic() +
          coord_flip() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      }else{
        temp <- rs$profiles[rs$profiles$date == rs$dates[input$date] & rs$profiles$variable == input$variable2,]
        
        plot <- ggplot(data=temp, aes(-depth, value, colour=factor(genotype), group=genotype)) +
          stat_smooth() +
          ylab(input$variable2) + 
          xlab("Depth layer") + 
          theme_classic() +
          coord_flip() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      
      
      ggplotly(plot)
    })     
    
    
    
    
})
