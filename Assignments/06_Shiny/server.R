
function(input, output) {
  
  ##########################    Data    ################################
  # clean data
  DF <- reactive({
    df <- data %>%
      # filter Age based on slidebar
      filter( Age > min(input$sbar) & Age < max(input$sbar)) %>%
      # filter irrelevant DayOfWeekOfDeath
      filter( DayOfWeekOfDeath < 8 ) %>%
      # MannerOfDeath
      mutate( MannerOfDeath = ifelse( MannerOfDeath == 2, 
                                      'Suicide', 'Homicide'),
              Sex = ifelse( Sex == 'F', 'Female', 'Male')) # Sex
    
    # MethodOfDisposition
    from_dis = c('B', 'C', 'O', 'U', 
                 'D', 'E', 'R') 
    to_dis = c('Burial','Cremation','Other','Unknown',
               'Other','Other','Other')
    df$MethodOfDisposition <- plyr::mapvalues(df$MethodOfDisposition,
                                              from = from_dis, 
                                              to = to_dis)
    
    # Race 
    from_rac = c(1, 2, 3, 4, 
                 5, 6, 7, 18, 
                 28, 38, 48, 58, 
                 68, 78) 
    to_rac = c('White', 'Black', 'Amric-Ind', 'Asian', 
               'Asian', 'Hawaiian', 'Asian', 'Asia-Ind',
               'Asian', 'Other', 'Asian', 'Other',
               'Other', 'Other')
    df$Race <- plyr::mapvalues(df$Race, 
                               from = from_rac, to = to_rac)
    
    # Education
    from_edu = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,99) 
    to_edu   = c(1,1,1,1,1,1,1,1,1,2,2,2,2,3,4,5,6,7,9)
    df$Education1989Revision <- plyr::mapvalues( df$Education1989Revision, 
                                                 from = from_edu,
                                                 to = to_edu )
    df <- df %>%
      mutate( Education = Education2003Revision + Education1989Revision ) %>%
      filter( Education < 8 )
    
    return(df)
  })
  
  # plot type
  PT <- reactive({ 
    pt <- input$ptype
    if      ( pt == 1 ) { out <- 'bar' } 
    else if ( pt == 2 ) { out <- 'dist'} 
    else if ( pt == 3 ) { out <- 'box' } 
    return(out)
  })
  
  # radio button
  RB <- reactive({ 
    rb <- input$radio
    if      ( rb == 1 ) { out <- 'identity'} 
    else if ( rb == 2 ) { out <- 'fill'} 
    else if ( rb == 3 ) { out <- 'dodge' } 
    else if ( rb == 4 ) { out <- 'stack' } 
    return(out)
  })
  
  # x variable
  X <- reactive({ 
    x <- input$xvar
    if      ( x == 1 )  { out <- list( v1 = DF()$Age, 
                                       v2 = 'Age' ) } 
    else if ( x == 2 )  { out <- list( v1 = DF()$Sex, 
                                       v2 = 'Sex' ) } 
    else if ( x == 3 )  { out <- list( v1 = DF()$Race, 
                                       v2 = 'Race' ) } 
    else if ( x == 4 )  { out <- list( v1 = DF()$Autopsy, 
                                       v2 = 'Autopsy' ) } 
    else if ( x == 5 )  { out <- list( v1 = DF()$Education, 
                                       v2 = 'Education' ) } 
    else if ( x == 6 )  { out <- list( v1 = DF()$MonthOfDeath, 
                                       v2 = 'MonthOfDeath' ) } 
    else if ( x == 7 )  { out <- list( v1 = DF()$InjuryAtWork, 
                                       v2 = 'InjuryAtWork' ) } 
    else if ( x == 8 )  { out <- list( v1 = DF()$MannerOfDeath, 
                                       v2 = 'MannerOfDeath' ) } 
    else if ( x == 9 )  { out <- list( v1 = DF()$MaritalStatus, 
                                       v2 = 'MaritalStatus' ) } 
    else if ( x == 10 ) { out <- list( v1 = DF()$ResidentStatus, 
                                       v2 = 'ResidentStatus' ) } 
    else if ( x == 11 ) { out <- list( v1 = DF()$DayOfWeekOfDeath, 
                                       v2 = 'DayOfWeekOfDeath' ) } 
    else if ( x == 12 ) { out <- list( v1 = DF()$MethodOfDisposition, 
                                       v2 = 'MethodOfDisposition' ) } 
    return(out )
  })
  
  # fill
  FL <- reactive({ 
    x <- input$fill
    if      ( x == 2 )  { out <- list( v1 = DF()$Sex, 
                                       v2 = 'Sex' ) } 
    else if ( x == 3 )  { out <- list( v1 = DF()$Race, 
                                       v2 = 'Race' ) } 
    else if ( x == 4 )  { out <- list( v1 = DF()$Autopsy, 
                                       v2 = 'Autopsy' ) } 
    else if ( x == 5 )  { out <- list( v1 = DF()$Education, 
                                       v2 = 'Education' ) } 
    else if ( x == 6 )  { out <- list( v1 = DF()$MonthOfDeath, 
                                       v2 = 'MonthOfDeath' ) } 
    else if ( x == 7 )  { out <- list( v1 = DF()$InjuryAtWork, 
                                       v2 = 'InjuryAtWork' ) } 
    else if ( x == 8 )  { out <- list( v1 = DF()$MannerOfDeath, 
                                       v2 = 'MannerOfDeath' ) } 
    else if ( x == 9 )  { out <- list( v1 = DF()$MaritalStatus, 
                                       v2 = 'MaritalStatus' ) } 
    else if ( x == 10 ) { out <- list( v1 = DF()$ResidentStatus, 
                                       v2 = 'ResidentStatus' ) } 
    else if ( x == 11 ) { out <- list( v1 = DF()$DayOfWeekOfDeath, 
                                       v2 = 'DayOfWeekOfDeath' ) } 
    else if ( x == 12 ) { out <- list( v1 = DF()$MethodOfDisposition, 
                                       v2 = 'MethodOfDisposition' ) } 
    return(out)
  })
  
  # row facet
  RF <- reactive({ 
    x <- input$rfacet
    if      ( x == 1 )  { out <- NULL } 
    else if ( x == 2 )  { out <- vars(DF()$Sex) } 
    else if ( x == 3 )  { out <- vars(DF()$Race) }
    else if ( x == 4 )  { out <- vars(DF()$Autopsy) }
    else if ( x == 5 )  { out <- vars(DF()$Education) }
    else if ( x == 6 )  { out <- vars(DF()$MonthOfDeath) } 
    else if ( x == 7 )  { out <- vars(DF()$InjuryAtWork) }
    else if ( x == 8 )  { out <- vars(DF()$MannerOfDeath) } 
    else if ( x == 9 )  { out <- vars(DF()$MaritalStatus) }
    else if ( x == 10 ) { out <- vars(DF()$ResidentStatus) } 
    else if ( x == 11 ) { out <- vars(DF()$DayOfWeekOfDeath) }
    else if ( x == 12 ) { out <- vars(DF()$MethodOfDisposition) }
    return( out )
  })
  
  # column facet
  CF <- reactive({ 
    x <- input$cfacet
    if      ( x == 1 )  { out <- NULL } 
    else if ( x == 2 )  { out <- vars(DF()$Sex) } 
    else if ( x == 3 )  { out <- vars(DF()$Race) } 
    else if ( x == 4 )  { out <- vars(DF()$Autopsy) } 
    else if ( x == 5 )  { out <- vars(DF()$Education) } 
    else if ( x == 6 )  { out <- vars(DF()$MonthOfDeath) } 
    else if ( x == 7 )  { out <- vars(DF()$InjuryAtWork) } 
    else if ( x == 8 )  { out <- vars(DF()$MannerOfDeath) } 
    else if ( x == 9 )  { out <- vars(DF()$MaritalStatus) } 
    else if ( x == 10 ) { out <- vars(DF()$ResidentStatus) } 
    else if ( x == 11 ) { out <- vars(DF()$DayOfWeekOfDeath) } 
    else if ( x == 12 ) { out <- vars(DF()$MethodOfDisposition) } 
    return( out )
  })
  
  # Generate a plot of the data ----
  output$myplot <- renderPlot({
    
    g <- ggplot( data = DF(),
                 aes( x = X()$v1,
                      fill = factor(FL()$v1) )) +
      facet_grid( rows = RF(), 
                  cols = CF(),
                  scales = "free") +
      labs( x = X()$v2, 
            fill = FL()$v2 ,
            title = paste( 'Plot of', X()$v2 , 'for different', FL()$v2)) + 
      scale_fill_brewer( palette = "Set1" ) +
      theme( axis.text    = element_text( size = 13 ),
             axis.title   = element_text( size = 14 ),
             legend.title = element_text( size = 14 ), 
             legend.text  = element_text( size = 12 ),
             strip.text   = element_text( size = 14 ),
             plot.title   = element_text( size = 20, face = "bold", 
                                          margin = margin(0,0,30,0)))
    
    if (PT() == 'bar'){
      g <- g + geom_bar( position = RB() ,
                         alpha    = input$alpha ) 
    } 
    
    else if (PT() == 'box'){
      g <- g + geom_boxplot( alpha = input$alpha ) 
    }
    
    else if (PT() == 'dist'){
      g <- g + geom_density_ridges2( aes( y = factor(FL()$v1)),
                                     scale = 2,
                                     alpha = input$alpha,
                                     rel_min_height = 0.01) + 
        ylab( FL()$v2 )
    }
    
    return(g)
    
  })
  ##########################    Data/Summary    ############################
  # Summary of the data ----
  output$text <- renderUI({
    s1 <- paste(h2('Mortality Data'),
                h5('Information of cause-of-death of mortality 
                   in U.S.A in 2014'))
    s2 <- paste(h3('Description'),
                h5('A dataset containing the manner of death 
                   (homicide or suicide) and other features of a 
                   sample of 60000 deaths.'))
    s3 <- paste(h3('Variables'),
                HTML('<li><strong>Age: </strong>
                     0-103 years</li>'),
                HTML('<li><strong>Sex: </strong>
                     Female(F) or Male(M)</li>'),
                HTML('<li><strong>Race: </strong>
                     White(1), Black(2), American-Indian(3),
                     Asian(4,5,7,28,48), Hawaiian(6), Asian-Indian(18), 
                     Other(38,58,68,78)</li>'),
                HTML('<li><strong>Autopsy: </strong>
                     Yes(Y), No(N), Unknown(U)</li>'),
                HTML('<li><strong>Education: </strong>
                     1-8</li>'),
                HTML('<li><strong>MonthOfDeath: </strong>
                     1-12</li>'),
                HTML('<li><strong>InjuryAtWork: </strong>
                     Yes(Y), No(N), Unknown(U)</li>'),
                HTML('<li><strong>MannerOfDeath: </strong>
                     Suicide(0), Homicide(1)</li>'),
                HTML('<li><strong>MaritalStatus: </strong>
                     Divorced(D), Married(M), Single(S), 
                     Widowed(W), Unknown(U)</li>'),
                HTML('<li><strong>ResidentStatus: </strong>
                     Residents(1), Intrastate Nonresidents(2),
                     Interstate Nonresidents(3),
                     Foreign Residents(4)</li>'),
                HTML('<li><strong>DayOfWeekOfDeath: </strong>
                     1-7</li>'),
                HTML('<li><strong>MethodOfDisposition: </strong>
                     Burial(B), Cremation(C), Other(O), Unknown(U)</li>'))
    HTML(paste(s1, s2, s3, sep = '<br/>'))
    
  })
  
  
  ##########################    Data/Table    ################################
  # Generate an table view of the data ----
  output$mytable = renderDataTable({
    datatable(data, 
              filter = "top",
              options = list( searching = T,
                              pageLength = 10,
                              lengthMenu = c(10, 15, 20), 
                              scrollX = T))
  })
  
  ##########################    Predict    ################################
  # uploaded file ----
  UF <- reactive({
    
    # check if csv is not empty
    if (is.null(input$datafile))
      return(NULL)                
    
    # read data
    df <- read.csv(file = input$datafile$datapath)
    
    # separate id of each data
    ID <- df$Id
    
    # make these columns numeric
    df$Sex                 <- as.numeric(factor(df$Sex))
    df$Autopsy             <- as.numeric(factor(df$Autopsy))
    df$MaritalStatus       <- as.numeric(factor(df$MaritalStatus))
    df$InjuryAtWork        <- as.numeric(factor(df$InjuryAtWork))
    df$MethodOfDisposition <- as.numeric(factor(df$MethodOfDisposition))
    
    # Education
    from_edu = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,99) 
    to_edu   = c(1,1,1,1,1,1,1,1,1,2,2,2,2,3,4,5,6,7,9)
    df$Education1989Revision <- plyr::mapvalues(df$Education1989Revision, 
                                                from=from_edu,
                                                to=to_edu)
    df <- df %>%
      mutate(Education = Education2003Revision + Education1989Revision)
    
    # Keep these columns
    cols <- c('ResidentStatus','Education','MonthOfDeath','Sex','Age',
              'MaritalStatus','DayOfWeekOfDeath','MethodOfDisposition',
              'Autopsy','InjuryAtWork','Race','MannerOfDeath')
    df <- df[ , (names(df) %in% cols)]
    
    # Predict based on saved model
    hpred <- h2o.predict(saved_model, as.h2o(df))
    
    # turn it to dataframe
    hpred <- as.data.frame(hpred)
    
    # turn predictions to strings
    from_pred = c(0, 1) 
    to_pred   = c('Suicide', 'Homicide')
    hpred$predict <- plyr::mapvalues(hpred$predict, 
                                     from=from_pred,
                                     to=to_pred)
    # bind predictions with saved ids
    out <- data.frame(cbind(ID, 
                            'Prediction' = hpred$predict))
    
    # active download button for download predictions
    output$get_the_item <- renderUI({
      downloadButton('download_item', 
                     'Download')
    })
    
    return(out)
    
  })
  
  # Display uploaded file ----
  output$table <- renderDataTable({
    datatable(UF(), 
              filter = "top",
              options = list( searching = T,
                              pageLength = 10,
                              lengthMenu = c(10, 20, 50), 
                              scrollX = T))
  })
  
  
  # Downloadable csv of selected dataset ----
  output$download_item <- downloadHandler(
    filename = function() {
      paste('prediction', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(UF(), file, row.names = FALSE)
    }
  )
  
  
}

