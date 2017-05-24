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

# This is the user interface of the Shiny app to transform CellSet output files (XLS)
# into one single RSML file that can be read by PAGE-Root

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Friend of GLORIA"),
  
  fluidRow(
    column(3, 
      helpText("Analyse data extracted from Rhizotron image with GLORIA"),
      tabsetPanel(
        tabPanel("Your data",
                 fileInput('global_file', 'Choose global data File', accept=c('text/comma-separated-values', '.csv')),
                 fileInput('local_file', 'Choose local data File', accept=c('text/comma-separated-values', '.csv')),
                 # fileInput('dir_file', 'Choose directionality data File', accept=c('text/comma-separated-values', '.csv')),
                 fileInput('roi_file', 'Choose ROI data File', accept=c('text/comma-separated-values', '.csv')),
                 fileInput('genotype_file', 'Choose genotype datafile', accept=c('text/comma-separated-values', '.csv')),
                 actionButton(inputId = "load_data", label="Start analysis",icon("paper-plane"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 
        ),
        tabPanel("Plotting options",
                 selectInput("genotype", label = "Select the genotype", choices = c("Load data"), selected = 1),
                 selectInput("root", label = "Select the root", choices = c("Load data"), selected = 1),
                 sliderInput("date", "Select date:", min=0, max=1, value=1, step = 1),
                 checkboxInput("mean", "Plot average", value = F)
        )
      )  
    ),
    
    column(3,
           h4("3. View the root system"),
           tags$hr(),
           plotOutput("rootPlot1")
    ), 
  
    column(6,
           tabsetPanel(
             tabPanel("Global data",
                      tags$hr(),
                      selectInput("variable", label = "Select variable", choices = c("length", "area", "n_segments", "convexhull", "angle", "depth", "width"), selected = 1),
                      plotlyOutput("sizePlot")
             ),
             tabPanel("Profile data",
                      tags$hr(),
                      selectInput("variable2", label = "Select variable", choices = c("length", "angle"), selected = 1),
                      plotlyOutput("profilePlot")
             ),
             tabPanel("Download processed data",
                      tabsetPanel(
                        
                        tabPanel("Global results",
                                 downloadButton('download_global_data', 'Download full table'),
                                 tags$hr(),
                                 tableOutput('global_data'),
                                 value=2
                        ),
                        
                        tabPanel("Local results",
                                 downloadButton('download_local_data', 'Download full table'),
                                 tags$hr(),
                                 tableOutput('local_data'),
                                 value=2
                        ),
                        tabPanel("Profile results",
                                 downloadButton('download_profile_data', 'Download full table'),
                                 tags$hr(),
                                 tableOutput('profile_data'),
                                 value=2
                        )
                      )
             )
           )
    )
    

    
    )
))