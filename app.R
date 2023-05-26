

###############################################################################################
#
# Description: 
#
# Location: 
#
# Program name: global.R
# 
# Source code: 
# 
# Author: Jason Watts
#
# Sys.info: 
#
# R version: >=3.2.3
# 
############################################################################################################################################################
source("global.R")
#source("www/custom.css")
#########################################################################################################################################################

body <- dashboardBody(theme,
                      
                      h1("Loudspeaker Enclosure Design Dashboard", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; color: #8FBC8F;"),
                      
 tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    "))),

 


  
tabItems(
  
  
  tabItem("vented", 
          
       box(title = h2("Vented and Extended Bass Shelf Response", style = "font-family: 'Lobster', cursive;
                  font-weight: 500; line-height: 1.1; 
                             color: #8FBC8F;"),
                  solidHeader = TRUE,
                  #status = "primary",
                  background=Box_Color, 
                  collapsible = TRUE,
                  selectInput("AL", label = h3("Vented Box Alignment", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; color: #8FBC8F;"), 
                         choices = list("QB3", "SBB4", "SC4"), selected = "SBB"), 
                  textInput("aFs", h4("Low Frequency Extension Point", style = "font-family: 'Lobster', cursive;
                             font-weight: 500; line-height: 1.1; 
                             color: #8FBC8F;"), "29"), 
                  h2("Vented Frequency Response / EBS Vented Frequency Response", style = "font-family: 'Lobster', cursive;
                             font-weight: 500; line-height: 1.1; 
                             color: #8FBC8F;"), plotlyOutput(outputId = "ebs_response"),
                  h2("Vented Max SPL Response / EBS Vented Max SPL Response", style = "font-family: 'Lobster', cursive;
                             font-weight: 500; line-height: 1.1; 
                             color: #8FBC8F;"),plotlyOutput(outputId = "ebs_max_spl"),
                 h2("Vented Max Power Response / EBS Vented Max Power Response", style = "font-family: 'Lobster', cursive;
                             font-weight: 500; line-height: 1.1; 
                             color: #8FBC8F;"), plotlyOutput(outputId = "max_power_ebs")),
                  box(title = h2("Vented and Extended Bass Shelf Response", style = "font-family: 'Lobster', cursive;
                             font-weight: 500; line-height: 1.1; 
                             color: #8FBC8F;"),
              solidHeader = TRUE,
              #status = "primary",
              background=Box_Color, 
              collapsible = TRUE,
              img(src = "bmp/ported.bmp"),
              column(8, verbatimTextOutput("vented_summary"))),
              column(6, box(title = h2("EBS Vented Response", style = "font-family: 'Lobster', cursive;
                             font-weight: 500; line-height: 1.1; 
                             color: #8FBC8F;"),
                        solidHeader = TRUE,
                        background=Box_Color, 
                        collapsible = TRUE,
                        plotOutput("ebs_response_static")),
               box(title = h2("Vented Response", style = "font-family: 'Lobster', cursive;
                             font-weight: 500; line-height: 1.1; 
                             color: #8FBC8F;"),
                        solidHeader = TRUE,
                       background=Box_Color, 
                       collapsible = TRUE,
                       plotOutput("vented_response_static"))),
          
       #   column(6, box(title = h2("EBS Max SPL Response", style = "font-family: 'Lobster', cursive;
        #                     font-weight: 500; line-height: 1.1; 
        #                     color: #8FBC8F;"),
         #     solidHeader = TRUE,
              #status = "primary",
          #    background=Box_Color, 
          #    collapsible = TRUE,
           #   plotOutput("ebs_max_spl_static")
           #   ),
          
           #  box(title = h2("Vented Max SPL Response", style = "font-family: 'Lobster', cursive;
            #                 font-weight: 500; line-height: 1.1; 
           #                  color: #8FBC8F;"),
             # solidHeader = TRUE,
              #status = "primary",
            #  background=Box_Color, 
             # collapsible = TRUE,
             #   plotOutput("vented_max_spl_static"))),
      
    #  column(6, box(title = h2("Vented Max Power Response", style = "font-family: 'Lobster', cursive;
     #                        font-weight: 500; line-height: 1.1; 
     #                        color: #8FBC8F;"),
     #                       solidHeader = TRUE,
     #                       #status = "primary",
     #                       background=Box_Color, 
     #                       collapsible = TRUE,
      #                      plotOutput("max_power_ebs_static")),
                     
       #              box(title = h2("EBS Max Power Response", style = "font-family: 'Lobster', cursive;
        #                     font-weight: 500; line-height: 1.1; 
        #                     color: #8FBC8F;"),
         #                solidHeader = TRUE,
                         #status = "primary",
        #                 background=Box_Color, 
         #                collapsible = TRUE,
          #               plotOutput("max_power_ebs_static_vented"))),
      
      h2(HTML("<span style=\"text-align:center; display:inline-block; width: 100%; font-family: 'Lobster', cursive; font-weight: 500; line-height: 1.1; color: #8FBC8F; padding: 0px;\">Copyright @ Computational Analytics 2023</span>"))
),

tabItem("bandpass", 
        #if S = 0.7, then  b = 0.7206, passband ripple = 0.00 dB
        #if S = 0.6, then b = 0.9560, passband ripple = 0.35 dB
        #if S = 0.5, then b = 1.2712, passband ripple = 1.25 dB
        box(title = h2("4th Order Response", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),
            img(src = "bmp/4thorder.bmp"),verbatimTextOutput("bandpass_summary"),
            solidHeader = TRUE,
            #status = "primary",
            width=12,
            background=Box_Color, 
            collapsible = TRUE,
            selectInput("rip", label = h3("Passband Ripple", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; color: #8FBC8F;"), 
                        choices = list("0.00(dB)", "0.35(dB)",
                                       "1.25(dB)"), selected = "0.00(dB)"), textInput("cFl", h4("Low Frequency Cutoff Point", style = "font-family: 'Lobster', cursive;
                             font-weight: 500; line-height: 1.1; 
                             color: #8FBC8F;"), "29"),
            h2("4th Order Response", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),
                    plotlyOutput(outputId = "fourth_order_response"),
            h2("4th Order Max Power Response", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),
            plotlyOutput(outputId = "fourth_order_response_Pmax")), 
        
        #box(title = h2("6th Order Response", style = "font-family: 'Lobster', cursive;
           ##            font-weight: 500; line-height: 1.1; 
            #           color: #8FBC8F;"),
            #img(src = "bmp/6thorder.bmp"),
            #solidHeader = TRUE,
            #status = "primary",
            #width=12,
            #background=Box_Color, 
           # collapsible = TRUE,
           # plotlyOutput(outputId = "sixth_order_response")
           # ),
      #  wellPanel(dataTableOutput('sixth_order_bp_table')),
       
        
        
        
         h2(HTML("<span style=\"text-align:center; display:inline-block; width: 100%; font-family: 'Lobster', cursive; font-weight: 500; line-height: 1.1; color: #8FBC8F; padding: 0px;\">Copyright @ Computational Analytics 2023</span>"))),

tabItem("tools", 
        box( h2("Vent Calculator", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),img(src = "bmp/dual.bmp"),
           solidHeader = TRUE,
            #status = "primary",
            background=Box_Color, 
            collapsible = TRUE,
           textInput("Fbb", h4("Box Tunning Frequency", style = "font-family: 'Lobster', cursive;
                                       font-weight: 500; line-height: 1.1; 
                                       color: #8FBC8F;"), "45"),
                               textInput("Npp", h4("Number of Vents", style = "font-family: 'Lobster', cursive;
                                       font-weight: 500; line-height: 1.1; 
                                       color: #8FBC8F;"), "1"),
                               textInput("Vbb", h4("Box Voulme in Liters", style = "font-family: 'Lobster', cursive;
                                       font-weight: 500; line-height: 1.1; 
                                       color: #8FBC8F;"), "4.5"),
                               textInput("Dv", h4("Vent Diameter(cm)", style = "font-family: 'Lobster', cursive;
                                       font-weight: 500; line-height: 1.1; 
                                       color: #8FBC8F;"), "4"),
            selectInput("ALL", label = h3("Flare Type", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; color: #8FBC8F;"), 
                        choices = list("Both Ends Flared" = 1, "One End Flared" = 2,
                                       "No Flare Correction" = 3), selected = 1),
          #  selectInput("ALL", label = h3("Square or Round Port", style = "font-family: 'Lobster', cursive;
                   #      font-weight: 500; line-height: 1.1; color: #8FBC8F;"), 
                    #   choices = list("Square" = 1, "Round" = 2), selected = 2)),
            verbatimTextOutput("vent")),
  h2(HTML("<span style=\"text-align:center; display:inline-block; width: 100%; font-family: 'Lobster', cursive; font-weight: 500; line-height: 1.1; color: #8FBC8F; padding: 0px;\">Copyright @ Computational Analytics 2023</span>"))),

tabItem("sealed", 
        box(title = h2("Sealed Response", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),
            solidHeader = TRUE,
            #status = "primary",
            background=Box_Color, 
            collapsible = TRUE,
           h2("Sealed Box Alignment", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; color: #8FBC8F;"),
           h4("Qtc=.5 Critically Damped - Transient Perfect", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; color: #8FBC8F;"),
           h4("Qtc=.577 Bessel Response - Max Flat Delay", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; color: #8FBC8F;"),
           h4("Qtc=.707 Butterworth Response - Max Flat Amplitude", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; color: #8FBC8F;"),
           h4("Qtc=.5 Chebychev Equal Ripple Response", style = "font-family: 'Lobster', cursive;
                         font-weight: 500; line-height: 1.1; color: #8FBC8F;"),
               numericInput("Qtc", label =  h2("Qtc", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"), round(1/(2)^.5,4), min=.1, max = 1, step=.001),
            h2("Sealed Frequency Response", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),
            plotlyOutput(outputId = "sealed_response")),
           # h2("Sealed Max Power Response - Power Required to Produce SPLd at w in Watts", style = "font-family: 'Lobster', cursive;
                      # font-weight: 500; line-height: 1.1; 
                      # color: #8FBC8F;"),
           # plotlyOutput(outputId = "sealed_max_power")
          # h2("Thermally Limited SPL in db", style = "font-family: 'Lobster', cursive;
          #             font-weight: 500; line-height: 1.1; 
           #            color: #8FBC8F;"),
          # plotlyOutput(outputId = "Displacement Limited SPL at w in db"),
          # h2("Thermally Limited SPL", style = "font-family: 'Lobster', cursive;
           #            font-weight: 500; line-height: 1.1; 
           #            color: #8FBC8F;"),
           #plotlyOutput(outputId = "displacement_limited_SPL")
       # ),
        
        
   
       box(title = h2("Sealed Response", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),
            
            solidHeader = TRUE,
            #status = "primary",
            background=Box_Color, 
            collapsible = TRUE,
            img(src = "bmp/sealed.bmp"),
           br(),
           br(),
            verbatimTextOutput("sealed_summary")),
       column(6, box(title = h2("Sealed Frequency Response", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),
           solidHeader = TRUE,
           #status = "primary",
           background=Box_Color, 
           collapsible = TRUE,
           plotOutput("sealed_response_static"))
       ),
        h2(HTML("<span style=\"text-align:center; display:inline-block; width: 100%; font-family: 'Lobster', cursive; font-weight: 500; line-height: 1.1; color: #8FBC8F; padding: 0px;\">Copyright @ Computational Analytics 2023</span>"))),
            
        


tabItem("parameters", 
        
        
        box(title = h2("Driver Parameters - All Parameters are Required", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),
            width=8,
            solidHeader = TRUE,
            #status = "primary",
            background=Box_Color, 
            collapsible = TRUE,
            column(3,textInput("Re", h4("DC Resistance - Ohms", style = "font-family: 'Lobster', cursive;
                                         font-weight: 500; line-height: 1.1; 
                                         color: #8FBC8F;"), "6.68"),
                   textInput("Fs", h4("Fs - Hz", style = "font-family: 'Lobster', cursive;
                                      font-weight: 500; line-height: 1.1; 
                                       color: #8FBC8F;"), "39.442"),
                   textInput("Mms", h4("Mms - gram", style = "font-family: 'Lobster', cursive;
                                       font-weight: 500; line-height: 1.1; 
                                       color: #8FBC8F;"), "1.63285"),
                  # textInput("Zmax", h4("Zmax - Ohms", style = "font-family: 'Lobster', cursive;
                          #              font-weight: 500; line-height: 1.1; 
                           #             color: #8FBC8F;"), "44.09585"),
                   textInput("Qes", h4("Qes", style = "font-family: 'Lobster', cursive;
                                       font-weight: 500; line-height: 1.1; 
                                       color: #8FBC8F;"), "0.4386"),
                   textInput("Qms", h4("Qms", style = "font-family: 'Lobster', cursive;
                                       font-weight: 500; line-height: 1.1; 
                                       color: #8FBC8F;"), "2.4777")),
            column(3, textInput("Qts", h4("Qts", style = "font-family: 'Lobster', cursive;
                                          font-weight: 500; line-height: 1.1; 
                                          color: #8FBC8F;"), "0.3721"),
                   textInput("Le", h4("Le - mH @1kHz", style = "font-family: 'Lobster', cursive;
                                      font-weight: 500; line-height: 1.1; 
                                      color: #8FBC8F;"), "1.63285"),
                   textInput("Diam", h4("Dia - cm", style = "font-family: 'Lobster', cursive;
                                        font-weight: 500; line-height: 1.1; 
                                        color: #8FBC8F;"), "13.1"),
                   textInput("Sd", h4("Sd - mm^2", style = "font-family: 'Lobster', cursive;
                                      font-weight: 500; line-height: 1.1; 
                                      color: #8FBC8F;"), "13966.1245"),
                   textInput("Vas", h4("Vas - L", style = "font-family: 'Lobster', cursive;
                                       font-weight: 500; line-height: 1.1; 
                                       color: #8FBC8F;"), "22.0639")),
              column(3, 
                   textInput("BL", h4("BL(Tm)", style = "font-family: 'Lobster', cursive;
                                         font-weight: 500; line-height: 1.1; 
                                         color: #8FBC8F;"), "8.8152"),
#textInput("Efficiency", h4("Efficiency - %", style = "font-family: 'Lobster', cursive;
                                            #     font-weight: 500; line-height: 1.1; 
                                               #  color: #8FBC8F;"), "0.2843"),
                   textInput("Sensitivity", h4("Sensitivity - @2.83Vrms/1m ", style = "font-family: 'Lobster', cursive;
                                               font-weight: 500; line-height: 1.1; 
                                               color: #8FBC8F;"), "86.5158"),
                  textInput("Ov", h4("Occupied Volume - ft^3", style = "font-family: 'Lobster', cursive;
                                      font-weight: 500; line-height: 1.1; 
                                      color: #8FBC8F;"), ".045"),
                   textInput("Xmax", h4("Xmax - mm", style = "font-family: 'Lobster', cursive;
                                        font-weight: 500; line-height: 1.1; 
                                        color: #8FBC8F;"), "4.5")
                   
                   )
               ),
        box(title = h2("Import Data Sheet", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"),
            width=4,
            solidHeader = TRUE,
            #status = "lime",
            background=Box_Color, 
            collapsible = TRUE,
           fileInput('file1', 'Select Data Sheet', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.pdf')),
                      downloadButton('downloadData', 'Download')),
       #############Data Table Output
       column(4, DT::dataTableOutput('i_table')),
        h2(HTML("<span style=\"text-align:center; display:inline-block; width: 100%; font-family: 'Lobster', cursive; font-weight: 500; line-height: 1.1; color: #8FBC8F; padding: 0px;\">Copyright @ Computational Analytics 2023</span>"))
        )



))







server <- function(input, output, session) {
  
  output$sixth_order_response <- renderPlotly({ 
    
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      
      
      AAA <- c("Qts", "Vf/Vas", "Ff/Fs", "Vr/Vas", "Fr/Fs", "Fh/Fs", "Fl/Fs", "Gain")
      BBB <- c(0.18, 0.190, 1.950, 0.440, 1.000, 2.370, 1.052, -2.3)
      CCC <- c(0.19, 0.200, 1.960, 0.460, 1.000, 2.410, 1.013, -1.9)
      DDD <- c(0.20, 0.212, 1.960, 0.465, 1.000, 2.410, 1.070, -1.4)
      EEE <- c(0.21, 0.215, 1.980, 0.470, 1.000, 2.460, 1.076, -1.1)
      FFF <- c(0.22, 0.217, 2.020, 0.510, 1.000, 2.590, 1.060, -0.9)
      GGG <- c(0.23, 0.223, 2.032, 0.530, 1.000, 2.640, 1.060, -0.6)
      HHH <- c(0.24, 0.230, 2.040, 0.550, 1.000, 2.680, 1.060, -0.3)
      III <- c(0.25, 0.252, 2.010, 0.580, 1.000, 2.620, 1.060, 0.2)
      JJJ <- c(0.26, 0.270, 1.988, 0.600, 1.000, 2.570, 1.060, 0.6)
      KKK <- c(0.27, 0.294, 1.960, 0.630, 1.000, 2.510, 1.064, 1.1)
      LLL <- c(0.28, 0.308, 1.950, 0.660, 1.000, 2.500, 1.060, 1.0)
      DTT <- data.frame(BBB, CCC, DDD, EEE, FFF, GGG, HHH)
      DTT <- t(DTT)
      colnames(DTT) <- AAA
      DT<-DTT
      
      
      
      
      
      print(input$sixth_order_bp_table_rows_selected)
      
      #Vr = net rear volume
      #Fr = rear tuning frequency (Hz)
      #Fl = lower -3dB cutoff frequency (Hz)
      #Vf = net front volume
      #Ff = front tuning frequency (Hz)
      #Fh = upper -3dB cutoff frequency (Hz)edit(DT)
      Vf <- (DT[1,2])*Vas
      Ff <- (DT[1,3])*Fs
      Fh <- (DT[1,4])*Fs
      Vr <- (DT[1,5])*Vas
      Fr <- (DT[1,6])*Fs
      Fl <- (DT[1,7])*Fs
      
      #Vf <- (Vf/Vas)*Vas
      #Ff <- (Ff/Fs)*Fs
      #Fh <- (Fh/Fs)*Fs
      #Vr <- (Vr/Vas)*Vas
      #Fr <- (Fr/Fs)*Fs
      #Fl <- (Fl/Fs)*Fs
      
      a <- abs(Ff^2-Fr^2)*F^4
      b <- F^6
      c <- (Fr^2/Ff/Ql+Fs/Qts+Ff/Ql)*F^5
      d <- (Ff^2+Fr^2+Fs*(Fr^2/Ff/Qts/Ql+Ff/Qts/Ql) + Fs^2*(Vas/Vf+Vas/Vr+1))*F^4
      e <- (Fs^2*(Ff/Ql*(Vas/Vr+1)+Fr^2/Ff/Ql*(Vas/Vf+1)) + Fs/Qts*(Fr^2+Ff^2)+2*Fr^2*Ff/Ql)*F^3
      f <- (Fs^2*(Fr^2*(Vas/Vf+1)+Ff^2*(Vas/Vr+1)) + 2*Fr^2*Ff*Fs/Qts/Ql+ Fr^2*Ff^2)*F^2
      g <- (Fr^2*Ff*(Ff*Fs/Qts+2*Fs^2/Ql))*F
      h <- Fs^2*Fr^2*Ff^2
      i <- -b+d-f+h
      j <- c-e+g
      dBmag <- 20*log(a/(i^2+j^2)^.5)
      
      
      
      
      
      DF <- data.frame(dBmag)
      fg <- ggplot(data=DF, aes(x=w, y=dBmag, group=1)) + geom_line(aes(group=1), size=1.2, color="red") + geom_line(aes(x=w,y=exdBmag, group=1, color="blue"), size=1.2) + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black",size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"),axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size =11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + theme(legend.position="none")
      
      ggplotly(fg, tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)
      
      
    })
  })
  
  

  
  #####################################
  # Generate Data Table
  #####################################
  output$sixth_order_bp_table = DT::renderDataTable({
   
    BBB <- c(0.18, 0.190, 1.950, 0.440, 1.000, 2.370, 1.052, -2.3)
    CCC <- c(0.19, 0.200, 1.960, 0.460, 1.000, 2.410, 1.013, -1.9)
    DDD <- c(0.20, 0.212, 1.960, 0.465, 1.000, 2.410, 1.070, -1.4)
    EEE <- c(0.21, 0.215, 1.980, 0.470, 1.000, 2.460, 1.076, -1.1)
    FFF <- c(0.22, 0.217, 2.020, 0.510, 1.000, 2.590, 1.060, -0.9)
    GGG <- c(0.23, 0.223, 2.032, 0.530, 1.000, 2.640, 1.060, -0.6)
    HHH <- c(0.24, 0.230, 2.040, 0.550, 1.000, 2.680, 1.060, -0.3)
    III <- c(0.25, 0.252, 2.010, 0.580, 1.000, 2.620, 1.060, 0.2)
    JJJ <- c(0.26, 0.270, 1.988, 0.600, 1.000, 2.570, 1.060, 0.6)
    KKK <- c(0.27, 0.294, 1.960, 0.630, 1.000, 2.510, 1.064, 1.1)
    LLL <- c(0.28, 0.308, 1.950, 0.660, 1.000, 2.500, 1.060, 1.0)
    DTT <- data.frame(BBB, CCC, DDD, EEE, FFF, GGG, HHH)
    DTT <- t(DTT)
    AAA <- c("Qts", "Vf/Vas", "Ff/Fs", "Vr/Vas", "Fr/Fs", "Fh/Fs", "Fl/Fs", "Gain")
    colnames(DTT) <- AAA
    DT::datatable(data = DTT, selection = list(mode = 'single'),
                  rownames = FALSE,
                  width="100%",
                  extensions = c("TableTools", "FixedColumns", 
                                 "ColVis"), escape = FALSE, options = list(preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"), 
                                                                           drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "), 
                                                                           searching = TRUE, exporting = TRUE, pageLength = 20, dom = "CT<\"clear\">lfrtip", 
                                                                           oTableTools = list(sSwfPath = "images/js/copy_csv_xls_pdf.swf", 
                                                                                              aButtons = list("copy", "csv", "pdf")), aoColumnDefs = list(list(sClass = "alignCenter", 
                                                                                                                                                               aTargets = c(list(1), list(2)))))
    )

    
  })
 
  output$vented_summary <- renderPrint({
    w<-as.numeric(input$w)
    Xmax<-as.numeric(input$Xmax)
    Re<-as.numeric(input$Re)
    Fs<-as.numeric(input$Fs)
    Zmax<-as.numeric(input$Zmax)
    Qes<-as.numeric(input$Qes)
    Qms<-as.numeric(input$Qms)
    Qts<-as.numeric(input$Qts)
    Le<-as.numeric(input$Le)
    Diam<-as.numeric(input$Diam)
    Sd<-as.numeric(input$Sd)
    Vas<-as.numeric(input$Vas)
    BL<-as.numeric(input$BL)
    Mms<-as.numeric(input$Mms)
    Cms<-as.numeric(input$Cms)
    Kms<-as.numeric(input$Kms)
    Rms<-as.numeric(input$Rms)
    Efficiency<-as.numeric(input$Efficiency)
    Sensitivity<-as.numeric(input$Sensitivity)
    Ov <- as.numeric(input$Ov)
    
    #ifelse(is.null(Qts)==TRUE, Qts<-2, Qts<-Qts)
    
    SPL <- Sensitivity
    PEmax <- 100
    c<-345
    Ro <- 1.18
    
    Ql <- 7
    
    AL <- as.character(input$AL)
    
    vented_response_tables <- vented_response_tables(AL, round(Qts,2))
    
    alpha<-vented_response_tables$alpa
    helm<-vented_response_tables$helm
    ratio<-vented_response_tables$ratio
    
    Vb <- Vas/alpha
    Fb <- helm * Fs
    F3Fs <- ratio
    F3 <- (F3Fs)*Fs
    
   
    
    
    Sd <- pi*(Diam/100)^2/4
    Vd <- (Sd*Xmax)/1000
    n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
    SPL <- 112 + 10*log10(n0)
    K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
    K2 <- 112+10*log10(K1)
    Par <- 3*F3^4*Vd^2
    Per <- Par/n0
    
    PeakSPL <- SPL+10*log10(PEmax)
    
    aFs<-as.numeric(input$aFs)
    
    dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
    mx <- max(dBmag)
    
    for(i in seq(from = 1, to = 5, by=.05)){
      Vbf <- Vb*i
      exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
      if(max(exdBmag) > (mx + mx*margin)) break
    }
    
    DF <- data.frame(exdBmag, dBmag, aFs, Vbf*0.035315, mx)
 
    print(paste("Free Air Reference Efficiency: ", round(n0,3)))
    print(paste("Acoustic Power Output(RMS): ", round(Par,3)))
    print(paste("Maximum Electric Input(Watts): ", round(Per,3)))
    
    print(paste("Vented Box Volume(Litter): ", round(Vb,2)))
    print(paste("Vented -3db Down(db): ", round(F3,2)))
    print(paste("Vented Box Tunning Frequency(Hz): ", round(Fs,2)))
    print(paste("-3db Down Point(Hz): ", round(F3,2)))
    print(paste("Vented Box Peak SPL(db): ", round(PeakSPL,2)))
    
     print(paste("EBS Box Volume(Litters): ", round(Vbf,2)))
     print(paste("EBS Box Tunning Frequency(Hz): ", round(aFs,2)))
    
  })  
  
  
  output$ebs_response <- renderPlotly({ 
    
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
 
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
    
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      AL <- as.character(input$AL)
      
      vented_response_tables <- vented_response_tables(AL, round(Qts,2))
      
      alpha<-vented_response_tables$alpa
      helm<-vented_response_tables$helm
      ratio<-vented_response_tables$ratio
      
      Vb <- Vas/alpha
      Fb <- helm * Fs
      F3Fs <- ratio
      F3 <- (F3Fs)*Fs
      
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- (Sd*Xmax)/1000
      n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL <- 112 + 10*log10(n0)
      K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
      K2 <- 112+10*log10(K1)
      Par <- 3*F3^4*Vd^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      aFs<-as.numeric(input$aFs)
    
  dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
  mx <- max(dBmag)
  for(i in seq(from = 1, to = 5, by=.05)){
    Vbf <- Vb*i
    exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
    if(max(exdBmag) > (mx + mx*margin)) break
  }
  DF <- data.frame(exdBmag, dBmag, aFs, Vbf*0.035315, mx)
  fg <- ggplot(data=DF, aes(x=w, y=dBmag, group=1)) + geom_line(aes(group=1), size=1.2, color="red") + geom_line(aes(x=w,y=exdBmag, group=1, color="blue"), size=1.2) + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black",size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"),axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size =11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + theme(legend.position="none")

  ggplotly(fg, tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)

  
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$ebs_response_static <- renderPlot({ 
    
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
    
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      AL <- as.character(input$AL)
      
      vented_response_tables <- vented_response_tables(AL, round(Qts,2))
      
      alpha<-vented_response_tables$alpa
      helm<-vented_response_tables$helm
      ratio<-vented_response_tables$ratio
      
      Vb <- Vas/alpha
      Fb <- helm * Fs
      F3Fs <- ratio
      F3 <- (F3Fs)*Fs
      
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- (Sd*Xmax)/1000
      n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL <- 112 + 10*log10(n0)
      K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
      K2 <- 112+10*log10(K1)
      Par <- 3*F3^4*Vd^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      aFs<-as.numeric(input$aFs)

      dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
      mx <- max(dBmag)
      for(i in seq(from = 1, to = 5, by=.05)){
        Vbf <- Vb*i
        exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
        if(max(exdBmag) > (mx + mx*margin)) break
      }
      DF <- data.frame(exdBmag, dBmag, aFs, Vbf*0.035315, mx)
      ggplot(data=DF, aes(x=w, y=dBmag, group=1)) + geom_line(aes(x=w,y=exdBmag, group=1, color="red"), size=1.2) + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black",size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"),axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size =11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + theme(legend.position="none") + coord_polar(theta = "x")
      
   
      
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$vented_response_static <- renderPlot({ 
    
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
    
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      AL <- as.character(input$AL)
      
      vented_response_tables <- vented_response_tables(AL, round(Qts,2))
      
      alpha<-vented_response_tables$alpa
      helm<-vented_response_tables$helm
      ratio<-vented_response_tables$ratio
      
      Vb <- Vas/alpha
      Fb <- helm * Fs
      F3Fs <- ratio
      F3 <- (F3Fs)*Fs
      
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- (Sd*Xmax)/1000
      n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL <- 112 + 10*log10(n0)
      K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
      K2 <- 112+10*log10(K1)
      Par <- 3*F3^4*Vd^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      aFs<-as.numeric(input$aFs)

      dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
      mx <- max(dBmag)
      for(i in seq(from = 1, to = 5, by=.05)){
        Vbf <- Vb*i
        exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
        if(max(exdBmag) > (mx + mx*margin)) break
      }
      DF <- data.frame(exdBmag, dBmag, aFs, Vbf*0.035315, mx)
      ggplot(data=DF, aes(x=w, y=dBmag, group=1)) + geom_line(aes(group=1), size=1.2, color="blue") + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black",size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"),axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size =11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + theme(legend.position="none") + coord_polar(theta = "x")
      

    })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  output$bandpass_summary <- renderPrint({
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      Ql<-7
      if(input$rip=="0.00(dB)"){s<-.7}
      if(input$rip=="0.35(dB)"){s<-.6}
      if(input$rip=="1.25(dB)"){s<-.5}
      if(input$rip=="0.00(dB)"){b<-0.7206}
      if(input$rip=="0.35(dB)"){b<-0.9560}
      if(input$rip=="1.25(dB)"){b<-1.2712}
      #if S = 0.7, then  b = 0.7206, passband ripple = 0.00 dB
      #if S = 0.6, then b = 0.9560, passband ripple = 0.35 dB
      #if S = 0.5, then b = 1.2712, passband ripple = 1.25 dB
      Fl <- as.numeric(input$cFl)
      Fls <- (Fl*Qts)/Fs
      Fh  <- (Fls+b)*Fs/Qts
      Qbp <- (Fls*(Fls+b))^0.5
      Fb  <- Qbp*Fs/Qts
      Vf  <- (2*s*Qts)^2*Vas
      Vr  <- Vas/((Qbp/Qts)^2-1)
      Pa  <- -40*log10(1/(Qbp*2*s))
      F<-w
      #Fl  = lower -3dB cutoff frequency (Hz)
      #Fh  = upper -3dB cutoff frequency (Hz)
      #Qbp = Qtc of sealed chamber
      #Fb  = resonance frequency of vented chamber(Hz)
      #Vf  = net volume of vented chamber (litres)
      #Vr  = net volume of sealed chamber (litres)
      #Pa  = gain (dB)
      #SPLmax = displacement-limited SPL at F (dB/1M)
      #SPLtherm = thermally-limited SPL at  F (dB/1M)
      #PeakSPL = Thermally-limited SPL in passband
      
      #4th order bandpass system with desired gain
      #Choose a value for Pa, the gain in efficiency,
      
      #Qbp <- ((10^(-Pa/40))*2*s)^-1
      #Fl  <- ((-b+(b^2+4*Qbp^2)^0.5)/2)*(Fs/Qts)
      #Fh  <- Fl+(b*Fs/Qts)
      #Fb  <- Qbp*Fs/Qts
      #Vf  <- (2*s*Qts)^2*Vas
      #Vr  <- Vas/((Qbp/Qts)^2-1)
      
      Qtss <- Qts*((Vas+Vr)/Vr)^0.5
      Fss <- Fs*Qtss/Qts
      Qess <- Qes*Qtss/Qts
      Vass <- Vas*Vr/(Vas+Vr)
      Vbs <- Vf
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- Sd*Xmax/1000
      n0 <- 9.64*10^(-10)*Fss^3*Vass/Qess
      SPL <- 112 + 10*log10(n0)
      PeakSPL <- SPL+10*log10(PEmax)
    
    print(paste("Box Volume Front(Litters): ", round(Vf,2)))
    print(paste("Box Volume Rear(Litters): ", round(Vr,2)))
    print(paste("Vented Chmber Box Tunning Frequency(Hz): ", round(Fb,2)))
    print(paste("Lower -3dB Cutoff Frequency(Hz): ", round(Fl,2)))
    print(paste("Upper -3dB Cutoff Frequency(Hz): ", round(Fh,2)))
    })
  }) 
  
  
  output$fourth_order_response <- renderPlotly({ 
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      Ql<-7
       if(input$rip=="0.00(dB)"){s<-.7}
      if(input$rip=="0.35(dB)"){s<-.6}
      if(input$rip=="1.25(dB)"){s<-.5}
      if(input$rip=="0.00(dB)"){b<-0.7206}
      if(input$rip=="0.35(dB)"){b<-0.9560}
      if(input$rip=="1.25(dB)"){b<-1.2712}
      #if S = 0.7, then  b = 0.7206, passband ripple = 0.00 dB
      #if S = 0.6, then b = 0.9560, passband ripple = 0.35 dB
      #if S = 0.5, then b = 1.2712, passband ripple = 1.25 dB
      Fl <- as.numeric(input$cFl)
      Fls <- (Fl*Qts)/Fs
      Fh  <- (Fls+b)*Fs/Qts
      Qbp <- (Fls*(Fls+b))^0.5
      Fb  <- Qbp*Fs/Qts
      Vf  <- (2*s*Qts)^2*Vas
      Vr  <- Vas/((Qbp/Qts)^2-1)
      Pa  <- -40*log10(1/(Qbp*2*s))
      F<-w
      #Fl  = lower -3dB cutoff frequency (Hz)
      #Fh  = upper -3dB cutoff frequency (Hz)
      #Qbp = Qtc of sealed chamber
      #Fb  = resonance frequency of vented chamber(Hz)
      #Vf  = net volume of vented chamber (litres)
      #Vr  = net volume of sealed chamber (litres)
      #Pa  = gain (dB)
      #SPLmax = displacement-limited SPL at F (dB/1M)
      #SPLtherm = thermally-limited SPL at  F (dB/1M)
      #PeakSPL = Thermally-limited SPL in passband
      
      #4th order bandpass system with desired gain
      #Choose a value for Pa, the gain in efficiency,
      
      #Qbp <- ((10^(-Pa/40))*2*s)^-1
      #Fl  <- ((-b+(b^2+4*Qbp^2)^0.5)/2)*(Fs/Qts)
      #Fh  <- Fl+(b*Fs/Qts)
      #Fb  <- Qbp*Fs/Qts
      #Vf  <- (2*s*Qts)^2*Vas
      #Vr  <- Vas/((Qbp/Qts)^2-1)
      
      Qtss <- Qts*((Vas+Vr)/Vr)^0.5
      Fss <- Fs*Qtss/Qts
      Qess <- Qes*Qtss/Qts
      Vass <- Vas*Vr/(Vas+Vr)
      Vbs <- Vf
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- Sd*Xmax/1000
      n0 <- 9.64*10^(-10)*Fss^3*Vass/Qess
      SPL <- 112 + 10*log10(n0)
      
      
      #Par <- 3*F3^4*Vd^2
      
     # Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      K1 <- (4*pi^3*Ro/c)*Fss^4*Vd^2
      Fn2 <- (w/Fss)^2
      Fn4 <- Fn2^2
      A <- (Fb/Fss)^2
      B <- A/Qtss+Fb/(Ql*Fss)
      C <- 1+A+(Vass/Vbs)+Fb/(Fss*Qtss*Ql)
      D <- 1/Qtss+Fb/(Fss*Ql)
      E <- (97/49)*A
      dBmag <- 10*log10((A*Fn2)^2/((Fn4-C*Fn2+A)^2+Fn2*(D*Fn2-B)^2))
      
      Pmax <- (K1/n0)*((Fn4-C*Fn2+A)^2+Fn2*(D*Fn2-B)^2)/(Fn4-E*Fn2+A^2)
     # SPLmax <- SPL+dBmag+10*log10(SPLmax)
     # SPLtherm <- PeakSPL+dBmag
      
      #dat = data.frame(F, dBmag, SPLmax, Pmax)
            dat = data.frame(w, dBmag)
      
      fp <- ggplot(data=dat, aes(x=w, y=dBmag, group=1)) + geom_line(aes(group=1), size=1.2) + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"),panel.border= element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + scale_fill_manual(values = c("indianred", "deepskyblue1", "lightyellow", "darkslategray3"))

    ggplotly(fp, tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)
    
      
    })
  }) 
  
  output$fourth_order_response_Pmax <- renderPlotly({ 
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      Ql<-7
      if(input$rip=="0.00(dB)"){s<-.7}
      if(input$rip=="0.35(dB)"){s<-.6}
      if(input$rip=="1.25(dB)"){s<-.5}
      if(input$rip=="0.00(dB)"){b<-0.7206}
      if(input$rip=="0.35(dB)"){b<-0.9560}
      if(input$rip=="1.25(dB)"){b<-1.2712}
      #if S = 0.7, then  b = 0.7206, passband ripple = 0.00 dB
      #if S = 0.6, then b = 0.9560, passband ripple = 0.35 dB
      #if S = 0.5, then b = 1.2712, passband ripple = 1.25 dB
      Fl <- as.numeric(input$cFl)
      Fls <- (Fl*Qts)/Fs
      Fh  <- (Fls+b)*Fs/Qts
      Qbp <- (Fls*(Fls+b))^0.5
      Fb  <- Qbp*Fs/Qts
      Vf  <- (2*s*Qts)^2*Vas
      Vr  <- Vas/((Qbp/Qts)^2-1)
      Pa  <- -40*log10(1/(Qbp*2*s))
     
      #Fl  = lower -3dB cutoff frequency (Hz)
      #Fh  = upper -3dB cutoff frequency (Hz)
      #Qbp = Qtc of sealed chamber
      #Fb  = resonance frequency of vented chamber(Hz)
      #Vf  = net volume of vented chamber (litres)
      #Vr  = net volume of sealed chamber (litres)
      #Pa  = gain (dB)
      #SPLmax = displacement-limited SPL at F (dB/1M)
      #SPLtherm = thermally-limited SPL at  F (dB/1M)
      #PeakSPL = Thermally-limited SPL in passband
      
      #4th order bandpass system with desired gain
      #Choose a value for Pa, the gain in efficiency,
      
      #Qbp <- ((10^(-Pa/40))*2*s)^-1
      #Fl  <- ((-b+(b^2+4*Qbp^2)^0.5)/2)*(Fs/Qts)
      #Fh  <- Fl+(b*Fs/Qts)
      #Fb  <- Qbp*Fs/Qts
      #Vf  <- (2*s*Qts)^2*Vas
      #Vr  <- Vas/((Qbp/Qts)^2-1)
      
      Qtss <- Qts*((Vas+Vr)/Vr)^0.5
      Fss <- Fs*Qtss/Qts
      Qess <- Qes*Qtss/Qts
      Vass <- Vas*Vr/(Vas+Vr)
      Vbs <- Vf
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- Sd*Xmax/1000
      
      n0 <- 9.64*10^(-10)*Fss^3*Vass/Qess
      SPL <- 112 + 10*log10(n0)
      
      
      #Par <- 3*F3^4*Vd^2
      
      # Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      K1 <- (4*pi^3*Ro/c)*Fss^4*Vd^2
      Fn2 <- (w/Fss)^2
      Fn4 <- Fn2^2
      A <- (Fb/Fss)^2
      B <- A/Qtss+Fb/(Ql*Fss)
      C <- 1+A+(Vass/Vbs)+Fb/(Fss*Qtss*Ql)
      D <- 1/Qtss+Fb/(Fss*Ql)
      E <- (97/49)*A
      dBmag <- 10*log10((A*Fn2)^2/((Fn4-C*Fn2+A)^2+Fn2*(D*Fn2-B)^2))
      
      Pmax <- (K1/n0)*((Fn4-C*Fn2+A)^2+Fn2*(D*Fn2-B)^2)/(Fn4-E*Fn2+A^2)
      # SPLmax <- SPL+dBmag+10*log10(SPLmax)
      # SPLtherm <- PeakSPL+dBmag
      
      #dat = data.frame(F, dBmag, SPLmax, Pmax)
      dat = data.frame(w, Pmax)
      
      fp <- ggplot(data=dat, aes(x=w, y=Pmax, group=1)) + geom_line(aes(group=1), size=1.2, color="cornflowerblue") + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"),panel.border= element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + scale_fill_manual(values = c("indianred", "deepskyblue1", "lightyellow", "darkslategray3"))
      
      ggplotly(fp, tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)
      
    })
  }) 
  
  output$ebs_max_spl <- renderPlotly({ 
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
  
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
   
      AL <- as.character(input$AL)
   
      vented_response_tables <- vented_response_tables(AL, round(Qts,2))
    
      alpha<-vented_response_tables$alpa
      helm<-vented_response_tables$helm
      ratio<-vented_response_tables$ratio
      
      Vb <- Vas/alpha
      Fb <- helm * Fs
      F3Fs <- ratio
      F3 <- (F3Fs)*Fs
      
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- (Sd*Xmax)/1000
      n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL <- 112 + 10*log10(n0)
      K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
      K2 <- 112+10*log10(K1)
      Par <- 3*F3^4*Vd^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      aFs<-as.numeric(input$aFs)

   
      dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
     
       mx <- max(dBmag)
      
      for(i in seq(from = 1, to = 5, by=.05)){
        Vbf <- Vb*i
        exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
        if(max(exdBmag) > (mx + mx*margin)) break
      }
      
      SPLmax <- K2+10*log10((((w/Fs)^2)^2)^2/((((w/Fs)^2)^2)-((97/49)*((Fb/Fs)^2))*((w/Fs)^2)+((Fb/Fs)^2)^2))
      ESPLmax <- K2+10*log10((((w/Fs)^2)^2)^2/((((w/Fs)^2)^2)-((97/49)*((aFs/Fs)^2))*((w/Fs)^2)+((aFs/Fs)^2)^2))
      DF <- data.frame(SPLmax, ESPLmax)
      fp <- ggplot(data=DF, aes(x=w, y=SPLmax, group=1)) + geom_line(aes(group=1), size=1.2, color="red") + geom_line(aes(x=w, y=ESPLmax, group=1), size=1.2, color="cornflowerblue") + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + theme(legend.position="none")

      ggplotly(fp, tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)
      
    })
  })
  
  
  output$ebs_max_spl_static <- renderPlot({ 
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      AL <- as.character(input$AL)
      
      vented_response_tables <- vented_response_tables(AL, round(Qts,2))
      
      alpha<-vented_response_tables$alpa
      helm<-vented_response_tables$helm
      ratio<-vented_response_tables$ratio
      
      Vb <- Vas/alpha
      Fb <- helm * Fs
      F3Fs <- ratio
      F3 <- (F3Fs)*Fs
      
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- (Sd*Xmax)/1000
      n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL <- 112 + 10*log10(n0)
      K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
      K2 <- 112+10*log10(K1)
      Par <- 3*F3^4*Vd^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      aFs<-as.numeric(input$aFs)

      
      dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
      mx <- max(dBmag)
      for(i in seq(from = 1, to = 5, by=.05)){
        Vbf <- Vb*i
        exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
        if(max(exdBmag) > (mx + mx*margin)) break
      }
      SPLmax <- K2+10*log10((((w/Fs)^2)^2)^2/((((w/Fs)^2)^2)-((97/49)*((Fb/Fs)^2))*((w/Fs)^2)+((Fb/Fs)^2)^2))
      ESPLmax <- K2+10*log10((((w/Fs)^2)^2)^2/((((w/Fs)^2)^2)-((97/49)*((aFs/Fs)^2))*((w/Fs)^2)+((aFs/Fs)^2)^2))
      DF <- data.frame(SPLmax, ESPLmax)
      ggplot(data=DF, aes(x=w, y=SPLmax, group=1)) + geom_line(aes(group=1), size=1.2, color="red") + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + theme(legend.position="none") + coord_polar(theta = "x")
      
   
      
    })
  })
  
  output$vented_max_spl_static <- renderPlot({ 
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
       Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      AL <- as.character(input$AL)
      
      vented_response_tables <- vented_response_tables(AL, round(Qts,2))
      
      alpha<-vented_response_tables$alpa
      helm<-vented_response_tables$helm
      ratio<-vented_response_tables$ratio
      
      Vb <- Vas/alpha
      Fb <- helm * Fs
      F3Fs <- ratio
      F3 <- (F3Fs)*Fs
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- (Sd*Xmax)/1000
      n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL <- 112 + 10*log10(n0)
      K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
      K2 <- 112+10*log10(K1)
      Par <- 3*F3^4*Vd^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      aFs<-as.numeric(input$aFs)

      
      dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
      mx <- max(dBmag)
      for(i in seq(from = 1, to = 5, by=.05)){
        Vbf <- Vb*i
        exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
        if(max(exdBmag) > (mx + mx*margin)) break
      }
      SPLmax <- K2+10*log10((((w/Fs)^2)^2)^2/((((w/Fs)^2)^2)-((97/49)*((Fb/Fs)^2))*((w/Fs)^2)+((Fb/Fs)^2)^2))
      ESPLmax <- K2+10*log10((((w/Fs)^2)^2)^2/((((w/Fs)^2)^2)-((97/49)*((aFs/Fs)^2))*((w/Fs)^2)+((aFs/Fs)^2)^2))
      DF <- data.frame(SPLmax, ESPLmax)
      ggplot(data=DF, aes(x=w, y=ESPLmax, group=1)) + geom_line(aes(group=1), size=1.2, color="blue") + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + theme(legend.position="none") + coord_polar(theta = "x")
      
      
      
    })
  })
  
  
  
  output$max_power_ebs <- renderPlotly({ 
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      AL <- as.character(input$AL)
      
      vented_response_tables <- vented_response_tables(AL, round(Qts,2))
      
      alpha<-vented_response_tables$alpa
      helm<-vented_response_tables$helm
      ratio<-vented_response_tables$ratio
      
      Vb <- Vas/alpha
      Fb <- helm * Fs
      F3Fs <- ratio
      F3 <- (F3Fs)*Fs
      
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- (Sd*Xmax)/1000
      n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL <- 112 + 10*log10(n0)
      K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
      K2 <- 112+10*log10(K1)
      Par <- 3*F3^4*Vd^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)

      aFs<-as.numeric(input$aFs)

      dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
      mx <- max(dBmag)
      for(i in seq(from = 1, to = 5, by=.05)){
        Vbf <- Vb*i
        exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
        if(max(exdBmag) > (mx + mx*margin)) break
      }
      EPmax <- (K1/n0)*(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2)+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2)/Qts+aFs/(Ql*Fs)))^2)/((((w/Fs)^2)^2)-((97/49)*((aFs/Fs)^2))*((w/Fs)^2)+((aFs/Fs)^2)^2)
      Pmax <- (K1/n0)*(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2)+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2)/Qts+Fb/(Ql*Fs)))^2)/((((w/Fs)^2)^2)-((97/49)*((Fb/Fs)^2))*((w/Fs)^2)+((Fb/Fs)^2)^2)
      DF <- data.frame(Pmax, EPmax)
      
      fp <- ggplot(data=DF, aes(x=w, y=EPmax, group=1)) + geom_line(aes(group=1), size=1.2, color="red") + geom_line(aes(x=w, y=Pmax, group=1), size=1.2, color="cornflowerblue") + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold"))
   
      ggplotly(fp,  tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)
      
    })
  })
  
  
  output$max_power_ebs_static <- renderPlot({ 
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
       Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      AL <- as.character(input$AL)
      
      vented_response_tables <- vented_response_tables(AL, round(Qts,2))
      
      alpha<-vented_response_tables$alpa
      helm<-vented_response_tables$helm
      ratio<-vented_response_tables$ratio
      
      Vb <- Vas/alpha
      Fb <- helm * Fs
      F3Fs <- ratio
      F3 <- (F3Fs)*Fs
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- (Sd*Xmax)/1000
      n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL <- 112 + 10*log10(n0)
      K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
      K2 <- 112+10*log10(K1)
      Par <- 3*F3^4*Vd^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      aFs<-as.numeric(input$aFs)

      dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
      mx <- max(dBmag)
      for(i in seq(from = 1, to = 5, by=.05)){
        Vbf <- Vb*i
        exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
        if(max(exdBmag) > (mx + mx*margin)) break
      }
      EPmax <- (K1/n0)*(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2)+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2)/Qts+aFs/(Ql*Fs)))^2)/((((w/Fs)^2)^2)-((97/49)*((aFs/Fs)^2))*((w/Fs)^2)+((aFs/Fs)^2)^2)
      Pmax <- (K1/n0)*(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2)+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2)/Qts+Fb/(Ql*Fs)))^2)/((((w/Fs)^2)^2)-((97/49)*((Fb/Fs)^2))*((w/Fs)^2)+((Fb/Fs)^2)^2)
      DF <- data.frame(Pmax, EPmax)
      
      ggplot(data=DF, aes(x=w, y=EPmax, group=1)) + geom_line(aes(group=1), size=1.2, color="red") + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + coord_polar(theta = "x")
      
    })
  })
   
  output$max_power_ebs_static_vented <- renderPlot({ 
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      AL <- as.character(input$AL)
      
      vented_response_tables <- vented_response_tables(AL, round(Qts,2))
      
      alpha<-vented_response_tables$alpa
      helm<-vented_response_tables$helm
      ratio<-vented_response_tables$ratio
      
      Vb <- Vas/alpha
      Fb <- helm * Fs
      F3Fs <- ratio
      F3 <- (F3Fs)*Fs
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- (Sd*Xmax)/1000
      n0 <- 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL <- 112 + 10*log10(n0)
      K1 <- (4*pi^3*Ro/c)*Fs^4*Vd^2
      K2 <- 112+10*log10(K1)
      Par <- 3*F3^4*Vd^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      aFs<-as.numeric(input$aFs)

      
      dBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2 )+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2 ))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2 )/Qts+Fb/(Ql*Fs)))^2))
      mx <- max(dBmag)
      
      for(i in seq(from = 1, to = 5, by=.05)){
        Vbf <- Vb*i
        exdBmag <- 10*log10((((w/Fs)^2)^2)^2/(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2 )+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2 )/Qts+aFs/(Ql*Fs)))^2))
        if(max(exdBmag) > (mx + mx*margin)) break
      }
      
      EPmax <- (K1/n0)*(((((w/Fs)^2)^2)-(1+((aFs/Fs)^2)+(Vas/Vbf)+aFs/(Fs*Qts*Ql))*((w/Fs)^2)+((aFs/Fs)^2))^2+((w/Fs)^2)*((1/Qts+aFs/(Fs*Ql))*((w/Fs)^2)-(((aFs/Fs)^2)/Qts+aFs/(Ql*Fs)))^2)/((((w/Fs)^2)^2)-((97/49)*((aFs/Fs)^2))*((w/Fs)^2)+((aFs/Fs)^2)^2)
      Pmax <- (K1/n0)*(((((w/Fs)^2)^2)-(1+((Fb/Fs)^2)+(Vas/Vb)+Fb/(Fs*Qts*Ql))*((w/Fs)^2)+((Fb/Fs)^2))^2+((w/Fs)^2)*((1/Qts+Fb/(Fs*Ql))*((w/Fs)^2)-(((Fb/Fs)^2)/Qts+Fb/(Ql*Fs)))^2)/((((w/Fs)^2)^2)-((97/49)*((Fb/Fs)^2))*((w/Fs)^2)+((Fb/Fs)^2)^2)
      DF <- data.frame(Pmax, EPmax)
      
      ggplot(data=DF, aes(x=w, y=Pmax, group=1)) + geom_line(aes(group=1), size=1.2, color="blue") + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line(colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold")) + coord_polar(theta = "x")
      
    })
  })

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$sealed_summary <- renderPrint({
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      
            w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
            
        Xmax<-as.numeric(input$Xmax)
        Re<-as.numeric(input$Re)
        Fs<-as.numeric(input$Fs)
        Zmax<-as.numeric(input$Zmax)
        Qes<-as.numeric(input$Qes)
        Qms<-as.numeric(input$Qms)
        Qts<-as.numeric(input$Qts)
        Le<-as.numeric(input$Le)
        Diam<-as.numeric(input$Diam)
        Sd<-as.numeric(input$Sd)
        Vas<-as.numeric(input$Vas)
        BL<-as.numeric(input$BL)
        Mms<-as.numeric(input$Mms)
        Cms<-as.numeric(input$Cms)
        Kms<-as.numeric(input$Kms)
        Rms<-as.numeric(input$Rms)
        Efficiency<-as.numeric(input$Efficiency)
        Sensitivity<-as.numeric(input$Sensitivity)
        Ov <- as.numeric(input$Ov)
        
        SPL <- Sensitivity
        PEmax <- 100
        c<-345
        Ro <- 1.18
        
        Ql <- 7
        
        Qtc<-as.numeric(input$Qtc)
        Qr <- Qtc/Qts
        Vr <- Qr^2-1
        Vb <- Vas/Vr
        
        Fb <- Qr*Fs
        
        ifelse(Qtc>(1/2)^.5, dBpeak<-20*log10(Qtc^2/(Qtc^2-0.25)^0.5), dBpeak<-0)
        
        ifelse(Qtc>(1/2)^.5, Amax<-(Qtc^2)/(Qtc^2-0.25)^(1/2), Amax<-1)
        
        Sd <- pi*(Diam/100)^2/4
        Vd <- Sd*Xmax/1000
        n0 = 9.64*10^(-10)*Fs^3*Vas/Qes
        SPL = 112 + 10*log10(n0)
        K1 = (4*pi^3*Ro/c)*Fb^4*Vd^2
        K2 = 112+10*log10(K1)
        Par <- K1/Amax^2
        Per <- Par/n0
        PeakSPL <- SPL+10*log10(PEmax)
        
        print(paste("Free Air Reference Efficiency: ", round(n0,3)))
        print(paste("Acoustic Power Output(RMS): ", round(Par,3)))
        print(paste("Maximum Electric Input(Watts): ", round(Per,3)))
        
        print(paste("Sealed Box Volume(Litter): ", round(Vb,2)))
        #print(paste("Sealed -3db Down(db): ", round(F3,2)))
        print(paste("Sealed Box Tunning Frequency(Hz): ", round(Fs,2)))

        print(paste("Sealed Box Peak SPL(db): ", round( PeakSPL,2)))
      
    })
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$sealed_response <- renderPlotly({ 
    
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
         Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      Qtc<-as.numeric(input$Qtc)
      Qr <- Qtc/Qts
      Vr <- Qr^2-1
      Vb <- Vas/Vr
      
      Fb <- Qr*Fs
  
      ifelse(Qtc>(1/2)^.5, dBpeak<-20*log10(Qtc^2/(Qtc^2-0.25)^0.5), dBpeak<-0)
      
      ifelse(Qtc>(1/2)^.5, Amax<-(Qtc^2)/(Qtc^2-0.25)^(1/2), Amax<-1)
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- Sd*Xmax/1000
      n0 = 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL = 112 + 10*log10(n0)
      K1 = (4*pi^3*Ro/c)*Fb^4*Vd^2
      K2 = 112+10*log10(K1)
      Par <- K1/Amax^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
   
      
      F3 <- Fb*((1/Qtc^2-2+((1/Qtc^2-2)^2+4)^0.5)/2)^0.5
      
      Fr <- (w/Fb)^2
      
      dBmag <- 10*log10(Fr^2/((Fr-1)^2+Fr/Qtc^2))

      
      SPLd <- K2+40*log10(F/Fb)
      Pmax <- K1*((Fr-1)^2+Fr/Qtc^2)/n0
      SPLt <- dBmag + PeakSPL
      
      DF <- data.frame(dBmag)

      p <- ggplot(data=DF, aes(x=w, y=dBmag, group=1)) + geom_line(aes(group=1), size=1.2) + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background =
                element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line
              (colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold"))
      ggplotly(p,  tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)
      
  })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$sealed_max_power <- renderPlotly({ 
    
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      Qtc<-as.numeric(input$Qtc)
      Qr <- Qtc/Qts
      Vr <- Qr^2-1
      Vb <- Vas/Vr
      
      Fb <- Qr*Fs
      
      ifelse(Qtc>(1/2)^.5, dBpeak<-20*log10(Qtc^2/(Qtc^2-0.25)^0.5), dBpeak<-0)
      
      ifelse(Qtc>(1/2)^.5, Amax<-(Qtc^2)/(Qtc^2-0.25)^(1/2), Amax<-1)
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- Sd*Xmax/1000
      n0 = 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL = 112 + 10*log10(n0)
      
      k1 = (4*pi^3*Ro/c)*Fb^4*Vd^2

      k2 = 112+10*log10(k1)
      
    
      
      Par <- k1/Amax^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)


      F3 <- Fb*((1/Qtc^2-2+((1/Qtc^2-2)^2+4)^0.5)/2)^0.5
      
      Fr <- (w/Fb)^2
     
      dBmag <- 10*log10(Fr^2/((Fr-1)^2+Fr/Qtc^2))
      
      SPmax <- k1*((Fr-1)^2+Fr/Qtc^2)/n0
      
      SPLd <- k2+40*log10(F/Fb)
      
      Pmax <- k1*((Fr-1)^2+Fr/Qtc^2)/n0
      
      SPLt <- dBmag + PeakSPL
      
      #SPLd = displacement-limited SPL at F (dB)
     # Pmax = power required to produce SPLd at F (W)
      #SPLt = thermally-limited SPL at F (dB)
      
      DF <- data.frame(dBmag, SPLd, Pmax, SPLt, SPmax)
      
      p <- ggplot(data=DF, aes(x=w, y=Pmax, group=1)) + geom_line(aes(group=1), size=1.2) + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line
              (colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold"))
      
  
      ggplotly(p,  tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)
      
    })
  })
  
  
  output$thermally_limited_SPL <- renderPlotly({ 
    
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      Qtc<-as.numeric(input$Qtc)
      Qr <- Qtc/Qts
      Vr <- Qr^2-1
      Vb <- Vas/Vr
      
      Fb <- Qr*Fs
      
      ifelse(Qtc>(1/2)^.5, dBpeak<-20*log10(Qtc^2/(Qtc^2-0.25)^0.5), dBpeak<-0)
      
      ifelse(Qtc>(1/2)^.5, Amax<-(Qtc^2)/(Qtc^2-0.25)^(1/2), Amax<-1)
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- Sd*Xmax/1000
      n0 = 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL = 112 + 10*log10(n0)
      
      k1 = (4*pi^3*Ro/c)*Fb^4*Vd^2
      
      k2 = 112+10*log10(k1)
      
      
      
      Par <- k1/Amax^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      
      F3 <- Fb*((1/Qtc^2-2+((1/Qtc^2-2)^2+4)^0.5)/2)^0.5
      
      Fr <- (w/Fb)^2
      
      dBmag <- 10*log10(Fr^2/((Fr-1)^2+Fr/Qtc^2))
      
      SPmax <- k1*((Fr-1)^2+Fr/Qtc^2)/n0
      
      SPLd <- k2+40*log10(F/Fb)
      
      Pmax <- k1*((Fr-1)^2+Fr/Qtc^2)/n0
      
      SPLt <- dBmag + PeakSPL
      
      #SPLd = displacement-limited SPL at F (dB)
      # Pmax = power required to produce SPLd at F (W)
      #SPLt = thermally-limited SPL at F (dB)
      
      DF <- data.frame(dBmag, SPLd, Pmax, SPLt, SPmax)
      
      p <- ggplot(data=DF, aes(x=w, y=SPLt, group=1)) + geom_line(aes(group=1), size=1.2) + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line
                                                                                                  (colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold"))
      
      
      ggplotly(p,  tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)
      
    })
  })
  
  output$displacement_limited_SPL <- renderPlotly({ 
    
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      Qtc<-as.numeric(input$Qtc)
      Qr <- Qtc/Qts
      Vr <- Qr^2-1
      Vb <- Vas/Vr
      
      Fb <- Qr*Fs
      
      ifelse(Qtc>(1/2)^.5, dBpeak<-20*log10(Qtc^2/(Qtc^2-0.25)^0.5), dBpeak<-0)
      
      ifelse(Qtc>(1/2)^.5, Amax<-(Qtc^2)/(Qtc^2-0.25)^(1/2), Amax<-1)
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- Sd*Xmax/1000
      n0 = 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL = 112 + 10*log10(n0)
      
      k1 = (4*pi^3*Ro/c)*Fb^4*Vd^2
      
      k2 = 112+10*log10(k1)
      
      
      
      Par <- k1/Amax^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      
      F3 <- Fb*((1/Qtc^2-2+((1/Qtc^2-2)^2+4)^0.5)/2)^0.5
      
      Fr <- (w/Fb)^2
      
      dBmag <- 10*log10(Fr^2/((Fr-1)^2+Fr/Qtc^2))
      
      SPmax <- k1*((Fr-1)^2+Fr/Qtc^2)/n0
      
      SPLd <- k2+40*log10(F/Fb)
      
      Pmax <- k1*((Fr-1)^2+Fr/Qtc^2)/n0
      
      SPLt <- dBmag + PeakSPL
      
      #SPLd = displacement-limited SPL at F (dB)
      # Pmax = power required to produce SPLd at F (W)
      #SPLt = thermally-limited SPL at F (dB)
      
      DF <- data.frame(dBmag, SPLd, Pmax, SPLt, SPmax)
      
      p <- ggplot(data=DF, aes(x=w, y=SPLd, group=1)) + geom_line(aes(group=1), size=1.2) + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background = element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line
                                                                                                  (colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold"))
      
      
      ggplotly(p,  tooltip = c("x","y"))%>%config(displaylogo = FALSE)%>%config(showLink = FALSE)

    })
  })
  
  output$sealed_response_static <- renderPlot({ 
    
    withProgress(message = 'Analyzing - Please Wait', value = 0.1, {
      w<-as.numeric(input$w)[1]:as.numeric(input$w)[2]
      Xmax<-as.numeric(input$Xmax)
      Re<-as.numeric(input$Re)
      Fs<-as.numeric(input$Fs)
      Zmax<-as.numeric(input$Zmax)
      Qes<-as.numeric(input$Qes)
      Qms<-as.numeric(input$Qms)
      Qts<-as.numeric(input$Qts)
      Le<-as.numeric(input$Le)
      Diam<-as.numeric(input$Diam)
      Sd<-as.numeric(input$Sd)
      Vas<-as.numeric(input$Vas)
      BL<-as.numeric(input$BL)
      Mms<-as.numeric(input$Mms)
      Cms<-as.numeric(input$Cms)
      Kms<-as.numeric(input$Kms)
      Rms<-as.numeric(input$Rms)
      Efficiency<-as.numeric(input$Efficiency)
      Sensitivity<-as.numeric(input$Sensitivity)
      Ov <- as.numeric(input$Ov)
      
      SPL <- Sensitivity
      PEmax <- 100
      c<-345
      Ro <- 1.18
      
      Ql <- 7
      
      Qtc<-as.numeric(input$Qtc)
      Qr <- Qtc/Qts
      Vr <- Qr^2-1
      Vb <- Vas/Vr
      
      Fb <- Qr*Fs
      
      ifelse(Qtc>(1/2)^.5, dBpeak<-20*log10(Qtc^2/(Qtc^2-0.25)^0.5), dBpeak<-0)
      
      ifelse(Qtc>(1/2)^.5, Amax<-(Qtc^2)/(Qtc^2-0.25)^(1/2), Amax<-1)
      
      Sd <- pi*(Diam/100)^2/4
      Vd <- Sd*Xmax/1000
      n0 = 9.64*10^(-10)*Fs^3*Vas/Qes
      SPL = 112 + 10*log10(n0)
      K1 = (4*pi^3*Ro/c)*Fb^4*Vd^2
      K2 = 112+10*log10(K1)
      Par <- K1/Amax^2
      Per <- Par/n0
      PeakSPL <- SPL+10*log10(PEmax)
      
      
      F3 <- Fb*((1/Qtc^2-2+((1/Qtc^2-2)^2+4)^0.5)/2)^0.5
      
      Fr <- (w/Fb)^2
      
      dBmag <- 10*log10(Fr^2/((Fr-1)^2+Fr/Qtc^2))
      
      
      SPLd <- K2+40*log10(F/Fb)
      Pmax <- K1*((Fr-1)^2+Fr/Qtc^2)/n0
      SPLt <- dBmag + PeakSPL
      
      DF <- data.frame(dBmag)
      
     ggplot(data=DF, aes(x=w, y=dBmag, group=1)) + geom_line(aes(group=1), size=1.2) + theme(legend.background = element_rect(colour = 'black', fill = 'grey92', size = 1.5, linetype='solid'), plot.background =
                                                                                                                                    element_rect(fill="indianred", color="black", size=2.5, linetype="solid"), panel.border = element_rect(fill=NA,color="black", size=2.5, linetype="solid"), panel.grid.major = element_line(colour= "indianred", size=1), panel.grid.minor = element_line
                                                                                                                                  (colour= "indianred", size=1), axis.ticks = element_blank(), axis.title.x = element_text(colour = "black", face = "bold"), axis.title.y = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "black", angle = 0, size = 11, face = "bold"), axis.text.y = element_text(colour = "black", angle = 0, size = 11, face = "bold"))+ coord_polar(theta = "x")
 
    })
  })
  output$vent <- renderPrint({
    Diam<-as.numeric(input$Diam)
    Sd<-as.numeric(input$Sd)
    Xmax<-as.numeric(input$Xmax)
    #(Xmax mm)
  
    #(Sd cm^2)
  
    #Vd cm^3
    Sd <- pi*(Diam/100)^2/4
    Vd <- Sd*Xmax/1000
    

  #flare<-1 #AL
 # round or square #ALL
   
    if(input$ALL == 3) {k <- 0.307 + 0.307}
    if(input$ALL == 2) {k <- 0.425 + 0.307}
    if(input$ALL == 1) {k <- 0.425 + 0.425}

    #if both ends were flanged,
    #k = 0.425 + 0.425 = 0.850
    #if one flanged, one free,
    #k = 0.425 + 0.307 = 0.732
    #if both ends were free,
    #k = 0.307 + 0.307 = 0.614
    
    #Dv = round port diameter (cm)
    #Dv = square port = 2*((H * W)/pi)^.5
    #Lv = length of each port (cm)
    #Np = number of ports
    #Dmin = minimum port diameter (cm)
    
    Lv <- (23562.5*as.numeric(input$Dv)^2*as.numeric(input$Npp)/(as.numeric(input$Fbb)^2*as.numeric(input$Vbb)))-(k*as.numeric(input$Dv))
    
    Dmin <- 100*(20.3*(Vd^2/as.numeric(input$Fbb))^0.25)/as.numeric(input$Npp)^.5
    print(paste("Vent Length (cm)", round(Lv,2)))
    #print(paste("Driver Volume Displacement (cm^3)",round(Vd,6)))
    print(paste("Minimum Vent Diameter (cm)",round(Dmin,2)))

  })  
  
  ##############################Begin Importer Code##################################
  
  #############Open File and Perform Functions
  datasetInput <- reactive({
    #############Test File to See if Empty
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    
    
    
    uri <- inFile$datapath
    if(all(file.exists(Sys.which(c("pdfinfo", "pdftotext"))))) {
      pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = uri), language = "en", id = "id1")
      C <- content(pdf)
      print(C<-C[1:23])
    }
    VCorpus(URISource(uri, mode = ""), readerControl = list(reader = readPDF(engine = "ghostscript")))
    MC_tokenizer(C)
    scan_tokenizer(C)
    
    print(C)
    strsplit_space_tokenizer <- function(x)
      unlist(strsplit(as.character(x), "[[:space:]]+"))
    C <- strsplit_space_tokenizer(C)
                                                                                                                                                                      
    D <- t(data.frame(str_extract(C[9], "\\d+\\.*\\d*"), str_extract(C[12], "\\d+\\.*\\d*"), str_extract(C[15], "\\d+\\.*\\d*"), str_extract(C[18], "\\d+\\.*\\d*"), str_extract(C[21], "\\d+\\.*\\d*"), str_extract(C[24], "\\d+\\.*\\d*"), str_extract(C[27], "\\d+\\.*\\d*"), str_extract(C[30], "\\d+\\.*\\d*"), str_extract(C[33], "\\d+\\.*\\d*"), str_extract(C[36], "\\d+\\.*\\d*"), str_extract(C[39], "\\d+\\.*\\d*"),str_extract(C[42], "\\d+\\.*\\d*"), str_extract(C[45], "\\d+\\.*\\d*"), str_extract(C[48], "\\d+\\.*\\d*"), str_extract(C[54], "\\d+\\.*\\d*"), str_extract(C[57], "\\d+\\.*\\d*")))
    rownames(D)<-c("Impedance(Ohms)", "DC Resistance(Ohms)", "Inductance(mH @ 1 kHz)", "Free Air Resonance (Hz)", "Qms", "Qes", "Qts", "Mms(gram)", "Cms(mm/N)", "Sd(cm^2)", "Vd(cm^3)", "BL(Tm)", "Vas(Liters)", "Xmax(mm)", "SPL(dB @ 2.83V/1m)", "Power")
    colnames(D)<-C[2]
     updateTextInput(session, "Re", label = NULL, value = D[2])
     updateTextInput(session, "Fs", label = NULL, value = D[4])
     
    # updateTextInput(session, "Zmax", label = NULL, value = D[3])
     
     updateTextInput(session, "Qes", label = NULL, value = D[6])
     updateTextInput(session, "Qms", label = NULL, value = D[5])
     
     updateTextInput(session, "Qts", label = NULL, value = D[7])
     
     updateTextInput(session, "Le", label = NULL, value = D[3])
     updateTextInput(session, "Diam", label = NULL, value = D[2])
     
     updateTextInput(session, "Sd", label = NULL, value = D[10])
     updateTextInput(session, "Vas", label = NULL, value = D[13])
     
     updateTextInput(session, "BL", label = NULL, value = D[12])
     updateTextInput(session, "Mms", label = NULL, value = D[8])
     updateTextInput(session, "Cms", label = NULL, value = D[9])
   #  updateTextInput(session, "Kms", label = NULL, value = D[3])
    # updateTextInput(session, "Rms", label = NULL, value = D[3])
     
    # updateTextInput(session, "Efficiency", label = NULL, value = D[3])
     updateTextInput(session, "Sensitivity", label = NULL, value = D[15])
    # updateTextInput(session, "Ov", label = NULL, value = D[2])
     updateTextInput(session, "Xmax", label = NULL, value = D[14])
     

     print(D)
    #write.csv(D, file = "/home/ffa/Documents/DailySales.csv")
   
    
  })
  
  
  #############Insert into Data Table
  output$i_table <- DT::renderDataTable(datasetInput(), options=list(pageLength = 10, orderClasses = TRUE, searching = FALSE, paging = TRUE))
  #############Download Data
  output$downloadData <- downloadHandler(
    ##############Assign File Name and Type
    filename = function() {paste('data-', Sys.Date(), '.csv', sep='')},
    #############Write Data to File
    content = function(file) {write.csv(datasetInput(), file)}
  )
  #####################################################Importer Code#################################
}
shinyApp(
  ui = dashboardPage(header, sidebar, body, skin = Skin),
  server = server
)
