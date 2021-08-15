
navbarPage(
    
    # load theme
    theme = shinytheme("united"),
    
    # logo of navigation bar
    img(src = 'img.png', style = " float:left; 
                                   width:30px; 
                                   height:30px; "),
    
    tabPanel("Data",
             sidebarPanel(
                 
                 # Style
                 width = 3,
                 style = "overflow-y:scroll; 
                          max-height: 600px; 
                          position:relative;",
                 
                 # Plot type
                 selectInput(inputId  = "ptype", 
                             label    = strong("Plot type:"), 
                             choices  = list("Barplot"      = 1, 
                                             "Distribution" = 2,
                                             "Boxplot"      = 3), 
                             selected = 1),
                 
                 # Radio button for position
                 conditionalPanel(
                     "input.ptype == 1",
                     radioButtons( "radio", 
                                   label = strong("Position of bars:"),
                                   choices = list("ident" = 1,
                                                  "fill"     = 2,
                                                  "dodge"    = 3,
                                                  "stack"    = 4), 
                                   selected = 1,
                                   inline = TRUE),
                 ),
                 
                 # Sidebar for alpha
                 sliderInput(inputId = "alpha",
                             label   = strong("Transparency:"), 
                             min     = 0, 
                             max     = 1, 
                             value   = 0.5),
                 
                 # X variables
                 selectInput(inputId  = "xvar", 
                             label    = strong("X variable:"), 
                             choices  = list("Age"                 = 1,
                                             "Sex"                 = 2,
                                             "Race"                = 3,
                                             "Autopsy"             = 4,
                                             "Education"           = 5,
                                             "MonthOfDeath"        = 6,
                                             "InjuryAtWork"        = 7,
                                             "MannerOfDeath"       = 8,
                                             "MaritalStatus"       = 9,
                                             "ResidentStatus"      = 10, 
                                             "DayOfWeekOfDeath"    = 11,
                                             "MethodOfDisposition" = 12), 
                             selected = 1,
                             multiple = TRUE, 
                             selectize = FALSE),
                 
                 # Sidebar for Age
                 conditionalPanel(
                     "input.xvar == 1",
                     sliderInput(inputId = "sbar",
                                 label   = strong("Age range:"), 
                                 min     = 0, 
                                 max     = 110, 
                                 value   = c(10, 50)),
                 ),
                 
                 # Fill
                 selectInput(inputId  = "fill", 
                             label    = strong("Fill:"), 
                             choices  = list("Sex"                 = 2,
                                             "Race"                = 3,
                                             "Autopsy"             = 4,
                                             "Education"           = 5,
                                             "MonthOfDeath"        = 6,
                                             "InjuryAtWork"        = 7,
                                             "MannerOfDeath"       = 8,
                                             "MaritalStatus"       = 9,
                                             "ResidentStatus"      = 10, 
                                             "DayOfWeekOfDeath"    = 11,
                                             "MethodOfDisposition" = 12), 
                             selected = 2),
                 
                 # Row facet
                 selectInput(inputId  = "rfacet", 
                             label    = strong("Facet row:"), 
                             choices  = list("None"                = 1,
                                             "Sex"                 = 2,
                                             "Race"                = 3,
                                             "Autopsy"             = 4,
                                             "Education"           = 5,
                                             "MonthOfDeath"        = 6,
                                             "InjuryAtWork"        = 7,
                                             "MannerOfDeath"       = 8,
                                             "MaritalStatus"       = 9,
                                             "ResidentStatus"      = 10, 
                                             "DayOfWeekOfDeath"    = 11,
                                             "MethodOfDisposition" = 12), 
                             selected = 1),

                 
                 # Column Facet
                 selectInput(inputId  = "cfacet", 
                             label    = strong("Facet column:"), 
                             choices  = list("None"                = 1,
                                             "Sex"                 = 2,
                                             "Race"                = 3,
                                             "Autopsy"             = 4,
                                             "Education"           = 5,
                                             "MonthOfDeath"        = 6,
                                             "InjuryAtWork"        = 7,
                                             "MannerOfDeath"       = 8,
                                             "MaritalStatus"       = 9,
                                             "ResidentStatus"      = 10, 
                                             "DayOfWeekOfDeath"    = 11,
                                             "MethodOfDisposition" = 12), 
                             selected = 1),
                 
             ),
             
             mainPanel(
                 
                 # Tabset ----
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot", 
                                      br(),
                                      plotOutput(outputId = "myplot")),
                             
                             tabPanel("Table", 
                                      br(),
                                      DT::dataTableOutput("mytable")),
                             
                             tabPanel("Summary", 
                                      br(),
                                      htmlOutput("text"))

                             )
                 )
             ),
    
    
    tabPanel("Predict",
             
             sidebarPanel(
                 
                 # style
                 width = 3,
                 style = "height:   600px; 
                          position: relative;",
                 
                 # Input: Select a file ----
                 fileInput("datafile", 
                           "Choose CSV file:",
                           accept = c( "csv",
                                       "comma-separated-values",
                                       ".csv" )),
                 
                 # Download button ----
                 uiOutput("get_the_item")
                     
             ),
             
             mainPanel(
                 # table of prediction
                 DT::dataTableOutput('table'),
             )
    )
    
)
