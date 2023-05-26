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
###############################################################################################

###############################################################################################
# Asethtics
###############################################################################################
Title <- "Computational Analytics"
TitleWidth <- 350 
######################################################################################################Font Color############################################################################
font_color<-"yellow"
# #00688B
######################################################################################################Skin Color############################################################################
Skin<-NULL
#blue
#black
#purple
#green
#red
#yellow

#######################################################################################################BoxColors###########################################################################
Box_Color<-"navy"
#red
#yellow
#aqua
#blue
#light-blue
#green
#navy
#teal
#olive
#lime
#orange
#fuchsia
#purple
#maroon
#black

###################################################################################################Status Colors###########################################################################
#primary - blue
#succsess - green
#info - light blue
#warning - orange
#danger - red
#SideBar_Width<-60 #Icon Only Sidebar
SideBar_Width<-320 #Full Sidebar
####################################################################################################Custom Theme############################################################################

        tags$head(
          tags$style(HTML('a[data-title="Save and edit plot in cloud"]{display:none;}'))
        )

tags$head(
  tags$style(HTML(""))
)
#skin must be set to NULL
theme<-tags$head(tags$style(HTML('

                           .selectize-input.input-active, 
                           .selectize-input.input-active:hover, 
                           .selectize-control.multi 
                          .selectize-input.focus {border-color: red !important;}
                          .selectize-dropdown .active {background: indianred !important;}

                         /* tab color */
                           .nav-tabs {
                            background-color: indianred;
                            }

                           .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                            background-color: white;
                            border-color: black;
                            border-top-color: black;
                            }

                           /* logo font */
                          .skin-blue .main-header .logo {
                          font-family: "Lobster", cursive;
                          font-weight: bold;
                          font-size: 24px;
                          color: #8FBC8F; }
 
                          /* logo */
                          .skin-blue .main-header .logo {
                          background-color: indianred;
                          }

                          /* logo when hovered */
                          .skin-blue .main-header .logo:hover {
                          background-color: indianred;
                          }
                          
                          /* navbar (rest of the header) */
                         .skin-blue .main-header .navbar {
                          background-color: indianred;
                          }        
                          
                          /* main sidebar */
                          .skin-blue .main-sidebar {
                          background-color: indianred;
                          }
                          
                          /* active selected tab in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: #C1FFC1;
                          }
                          
                          /* other links in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                          background-color: #C1FFC1;
                          color: indianred;
                          }
                          
                          /* other links in the sidebarmenu when hovered */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: #8FBC8F;
                          }
                          
                          /* add scroll bar to sidebar*/
                          .sidebar {
                          height: 210vh; overflow-y: auto;
                          }"

                          ')))

##################################################################################
# load necessary libraries


library(ggplot2)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(shiny)
library(DT)


####################################################################Define Functions######################################################################
unit <- "cm"
VENTP <- function(Vd, Fb, Np, Vb, Dv, flare, unit) {
  if(flare == 1) {k <- 0.307 + 0.307}
  if(flare == 2) {k <- 0.425 + 0.307}
  if(flare == 3) {k <- 0.425 + 0.425}
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
  
  Lv <- (23562.5*Dv^2*Np/(Fb^2*Vb))-(k*Dv)
  
  Dmin <- 100*(20.3*(Vd^2/Fb)^0.25)/Np^.5
  
  if(unit == "cm") {Lv <- Lv} else {Lv <- Lv / 2.54}
  if(unit == "cm") {Dmin <- Dmin} else {Dmin <- Dmin / 2.54}
  DF <- data.frame(Lv, Dmin)
  return(DF)
}




margin<-0
vented_response_tables <- function(AL, Qts) {
  
  if(AL == "QB3"){
    if (Qts == 0.1) {
      alpa <- 34.3925
      helm <- 3.8416
      ratio <- 5.2233
    }
    if (Qts == 0.11) {
      alpa <- 28.2341
      helm <- 3.4947
      ratio <- 4.7386
    }
    if (Qts == 0.12) {
      alpa <- 23.5499
      helm <- 3.2058
      ratio <- 4.3337
    }
    if (Qts == 0.13) {
      alpa <- 19.9046
      helm <- 2.9615
      ratio <- 3.9902
    }
    if (Qts == 0.14) {
      alpa <- 17.015
      helm <- 2.7525
      ratio <- 3.6949
    }
    if (Qts == 0.15) {
      alpa <- 14.6784
      helm <- 2.5712
      ratio <- 3.4381
    }
    if (Qts == 0.16) {
      alpa <- 12.7685
      helm <- 2.4129
      ratio <- 3.2126
    }
    if (Qts == 0.17) {
      alpa <- 11.1855
      helm <- 2.2743
      ratio <- 3.0128
    }
    if (Qts == 0.18) {
      alpa <- 9.8589
      helm <- 2.1495
      ratio <- 2.8345
    }
    if (Qts == 0.19) {
      alpa <- 8.7361
      helm <- 2.0388
      ratio <- 2.6741
    }
    if (Qts == 0.2) {
      alpa <- 7.7775
      helm <- 1.9393
      ratio <- 2.5289
    }
    if (Qts == 0.21) {
      alpa <- 6.9524
      helm <- 1.8494
      ratio <- 2.3968
    }
    if (Qts == 0.22) {
      alpa <- 6.2372
      helm <- 2.2759
      ratio <- 1.12
    }
    if (Qts == 0.23) {
      alpa <- 5.6132
      helm <- 1.6935
      ratio <- 2.1647
    }
    if (Qts == 0.24) {
      alpa <- 5.0655
      helm <- 1.6254
      ratio <- 2.062
    }
    if (Qts == 0.25) {
      alpa <- 4.5822
      helm <- 1.5629
      ratio <- 1.9667
    }
    if (Qts == 0.26) {
      alpa <- 4.1535
      helm <- 1.5054
      ratio <- 1.8778
    }
    if (Qts == 0.27) {
      alpa <- 3.7714
      helm <- 1.4522
      ratio <- 1.7946
    }
    if (Qts == 0.28) {
      alpa <- 3.4295
      helm <- 1.4029
      ratio <- 1.7165
    }
    if (Qts == 0.29) {
      alpa <- 3.1223
      helm <- 1.3571
      ratio <- 1.6429
    }
    if (Qts == 0.3) {
      alpa <- 2.8421
      helm <- 1.3145
      ratio <- 1.5732
    }
    if (Qts == 0.31) {
      alpa <- 2.5944
      helm <- 1.2748
      ratio <- 1.507
    }
    if (Qts == 0.32) {
      alpa <- 2.3667
      helm <- 1.2376
      ratio <- 1.4439
    }
    if (Qts == 0.33) {
      alpa <- 2.1594
      helm <- 1.2028
      ratio <- 1.3836
    }
    if (Qts == 0.34) {
      alpa <- 1.9699
      helm <- 1.1702
      ratio <- 1.3258
    }
    if (Qts == 0.35) {
      alpa <- 1.7964
      helm <- 1.1395
      ratio <- 1.2702
    }
    if (Qts == 0.36) {
      alpa <- 1.6371
      helm <- 1.1106
      ratio <- 1.2167
    }
    if (Qts == 0.37) {
      alpa <- 1.4905
      helm <- 1.0834
      ratio <- 1.1651
    }
    if (Qts == 0.38) {
      alpa <- 1.3552
      helm <- 1.0578
      ratio <- 1.1153
    }
    if (Qts == 0.39) {
      alpa <- 1.23
      helm <- 1.0335
      ratio <- 1.0674
    }
    if (Qts == 0.4) {
      alpa <- 1.1141
      helm <- 1.0106
      ratio <- 1.0214
    }
    if (Qts == 0.41) {
      alpa <- 1.0065
      helm <- 0.9889
      ratio <- 0.9776
    }
    if (Qts == 0.42) {
      alpa <- 0.9064
      helm <- 0.9683
      ratio <- 0.9362
    }
    if (Qts == 0.43) {
      alpa <- 0.8131
      helm <- 0.9488
      ratio <- 0.8975
    }
    if (Qts == 0.44) {
      alpa <- 0.726
      helm <- 0.9303
      ratio <- 0.8618
    }
    if (Qts == 0.45) {
      alpa <- 0.6445
      helm <- 0.9128
      ratio <- 0.8294
    }
    if (Qts == 0.46) {
      alpa <- 0.5682
      helm <- 0.8961
      ratio <- 0.8001
    }
    if (Qts == 0.47) {
      alpa <- 0.4966
      helm <- 0.8802
      ratio <- 0.7741
    }
    if (Qts == 0.48) {
      alpa <- 0.4294
      helm <- 0.8651
      ratio <- 0.751
    }
    if (Qts == 0.49) {
      alpa <- 0.3661
      helm <- 0.8507
      ratio <- 0.7307
    }
    if (Qts == 0.5) {
      alpa <- 0.3065
      helm <- 0.837
      ratio <- 0.7129
    }
    if (Qts == 0.51) {
      alpa <- 0.2503
      helm <- 0.824
      ratio <- 0.6972
    }
    if (Qts == 0.52) {
      alpa <- 0.1971
      helm <- 0.8116
      ratio <- 0.6835
    }
    if (Qts == 0.53) {
      alpa <- 0.1468
      helm <- 0.7998
      ratio <- 0.6715
    }
    if (Qts == 0.54) {
      alpa <- 0.0992
      helm <- 0.7886
      ratio <- 0.661
    }
    if (Qts == 0.55) {
      alpa <- 0.054
      helm <- 0.7779
      ratio <- 0.6518
    }
    if (Qts == 0.56) {
      alpa <- 0.0111
      helm <- 0.7677
      ratio <- 0.6438
    }
  }
  
  #'SBB4(Super Fourth-Order Boom Box)
  if(AL == "SBB4") {
    if (Qts == 0.2) {
      alpa <- 5.89
      helm <- 1
      ratio <- 3.3686
    }
    if (Qts == 0.21) {
      alpa <- 5.339
      helm <- 1
      ratio <- 3.1518
    }
    if (Qts == 0.22) {
      alpa <- 4.8457
      helm <- 1
      ratio <- 2.9521
    }
    if (Qts == 0.23) {
      alpa <- 4.4204
      helm <- 1
      ratio <- 2.7674
    }
    if (Qts == 0.24) {
      alpa <- 4.0478
      helm <- 1
      ratio <- 2.596
    }
    if (Qts == 0.25) {
      alpa <- 3.7114
      helm <- 1
      ratio <- 2.4366
    }
    if (Qts == 0.26) {
      alpa <- 3.4286
      helm <- 1
      ratio <- 2.2883
    }
    if (Qts == 0.27) {
      alpa <- 3.1699
      helm <- 1
      ratio <- 2.1503
    }
    if (Qts == 0.28) {
      alpa <- 2.9388
      helm <- 1
      ratio <- 2.022
    }
    if (Qts == 0.29) {
      alpa <- 2.7315
      helm <- 1
      ratio <- 1.9031
    }
    if (Qts == 0.3) {
      alpa <- 2.5448
      helm <- 1
      ratio <- 1.7932
    }
    if (Qts == 0.31) {
      alpa <- 2.3761
      helm <- 1
      ratio <- 1.6922
    }
    if (Qts == 0.32) {
      alpa <- 2.2233
      helm <- 1
      ratio <- 1.6
    }
    if (Qts == 0.33) {
      alpa <- 2.0843
      helm <- 1
      ratio <- 1.5162
    }
    if (Qts == 0.34) {
      alpa <- 1.9576
      helm <- 1
      ratio <- 1.4406
    }
    if (Qts == 0.35) {
      alpa <- 1.8419
      helm <- 1
      ratio <- 1.3728
    }
    if (Qts == 0.36) {
      alpa <- 1.7357
      helm <- 1
      ratio <- 1.3122
    }
    if (Qts == 0.37) {
      alpa <- 1.6392
      helm <- 1
      ratio <- 1.2583
    }
    if (Qts == 0.38) {
      alpa <- 1.5484
      helm <- 1
      ratio <- 1.204
    }
    if (Qts == 0.39) {
      alpa <- 1.4656
      helm <- 1
      ratio <- 1.1679
    }
    if (Qts == 0.4) {
      alpa <- 1.389
      helm <- 1
      ratio <- 1.1302
    }
    if (Qts == 0.41) {
      alpa <- 1.3181
      helm <- 1
      ratio <- 1.0966
    }
    if (Qts == 0.42) {
      alpa <- 1.2523
      helm <- 1
      ratio <- 1.0667
    }
    if (Qts == 0.43) {
      alpa <- 1.1911
      helm <- 1
      ratio <- 1.0399
    }
    if (Qts == 0.44) {
      alpa <- 1.1341
      helm <- 1
      ratio <- 1.016
    }
    if (Qts == 0.45) {
      alpa <- 1.0809
      helm <- 1
      ratio <- 0.9944
    }
    if (Qts == 0.46) {
      alpa <- 1.0313
      helm <- 1
      ratio <- 0.975
    }
    if (Qts == 0.47) {
      alpa <- 0.9849
      helm <- 1
      ratio <- 0.9574
    }
    if (Qts == 0.48) {
      alpa <- 0.9414
      helm <- 1
      ratio <- 0.9415
    }
    if (Qts == 0.49) {
      alpa <- 0.9006
      helm <- 1
      ratio <- 0.927
    }
    if (Qts == 0.5) {
      alpa <- 0.8622
      helm <- 1
      ratio <- 0.9137
    }
    if (Qts == 0.51) {
      alpa <- 0.8262
      helm <- 1
      ratio <- 0.9015
    }
    if (Qts == 0.52) {
      alpa <- 0.7923
      helm <- 1
      ratio <- 0.8904
    }
    if (Qts == 0.53) {
      alpa <- 0.7603
      helm <- 1
      ratio <- 0.8801
    }
    if (Qts == 0.54) {
      alpa <- 0.7302
      helm <- 1
      ratio <- 0.8706
    }
    if (Qts == 0.55) {
      alpa <- 0.7017
      helm <- 1
      ratio <- 0.8619
    }
    if (Qts == 0.56) {
      alpa <- 0.6747
      helm <- 1
      ratio <- 0.8537
    }
    if (Qts == 0.57) {
      alpa <- 0.6493
      helm <- 1
      ratio <- 0.8462
    }
    if (Qts == 0.58) {
      alpa <- 0.6251
      helm <- 1
      ratio <- 0.8391
    }
    if (Qts == 0.59) {
      alpa <- 0.6022
      helm <- 1
      ratio <- 0.8325
    }
    if (Qts == 0.6) {
      alpa <- 0.5805
      helm <- 1
      ratio <- 0.8264
    }
    if (Qts == 0.61) {
      alpa <- 0.5599
      helm <- 1
      ratio <- 0.8206
    }
    if (Qts == 0.62) {
      alpa <- 0.5403
      helm <- 1
      ratio <- 0.8152
    }
    if (Qts == 0.63) {
      alpa <- 0.5216
      helm <- 1
      ratio <- 0.8102
    }
    if (Qts == 0.64) {
      alpa <- 0.5038
      helm <- 1
      ratio <- 0.8054
    }
    if (Qts == 0.65) {
      alpa <- 0.4869
      helm <- 1
      ratio <- 0.8009
    }
    if (Qts == 0.66) {
      alpa <- 0.4708
      helm <- 1
      ratio <- 0.7967
    }
    if (Qts == 0.67) {
      alpa <- 0.4554
      helm <- 1
      ratio <- 0.7926
    }
    if (Qts == 0.68) {
      alpa <- 0.4407
      helm <- 1
      ratio <- 0.7889
    }
    if (Qts == 0.69) {
      alpa <- 0.4267
      helm <- 1
      ratio <- 0.7853
    }
    if (Qts == 0.7) {
      alpa <- 0.4133
      helm <- 1
      ratio <- 0.7819
    }
  }
  
  # 'SC4(Fourth-Order Sub-Chebychev) Alignment'
  if(AL == "SC4"){
    if (Qts == 0.25) {
      alpa <- 3.8961
      helm <- 1.0338
      ratio <- 2.3949
    }
    if (Qts == 0.26) {
      alpa <- 3.6755
      helm <- 1.0534
      ratio <- 2.2282
    }
    if (Qts == 0.27) {
      alpa <- 3.4551
      helm <- 1.0703
      ratio <- 2.0784
    }
    if (Qts == 0.28) {
      alpa <- 2360
      helm <- 1.0842
      ratio <- 1.9439
    }
    if (Qts == 0.29) {
      alpa <- 2.0193
      helm <- 1.0951
      ratio <- 1.8229
    }
    if (Qts == 0.3) {
      alpa <- 2.8062
      helm <- 1.1028
      ratio <- 1.7137
    }
    if (Qts == 0.31) {
      alpa <- 2.5977
      helm <- 1.1073
      ratio <- 1.6149
    }
    if (Qts == 0.32) {
      alpa <- 2.3952
      helm <- 1.1086
      ratio <- 1.5251
    }
    if (Qts == 0.33) {
      alpa <- 2.1997
      helm <- 1.1065
      ratio <- 1.4431
    }
    if (Qts == 0.34) {
      alpa <- 1.0125
      helm <- 1.1012
      ratio <- 1.3679
    }
    if (Qts == 0.35) {
      alpa <- 1.8347
      helm <- 1.0926
      ratio <- 1.2986
    }
    if (Qts == 0.36) {
      alpa <- 1.6672
      helm <- 1.081
      ratio <- 1.2345
    }
    if (Qts == 0.37) {
      alpa <- 1.5109
      helm <- 1.0667
      ratio <- 1.1751
    }
    if (Qts == 0.38) {
      alpa <- 1.3665
      helm <- 1.0498
      ratio <- 1.12
    }
    if (Qts == 0.39) {
      alpa <- 1.2343
      helm <- 1.0309
      ratio <- 1.0689
    }
    if (Qts == 0.4) {
      alpa <- 1.1146
      helm <- 1.0103
      ratio <- 1.0215
    }
    if (Qts == 0.41) {
      alpa <- 1.007
      helm <- 0.9886
      ratio <- 0.9777
    }
    if (Qts == 0.42) {
      alpa <- 0.9113
      helm <- 0.9662
      ratio <- 0.9373
    }
    if (Qts == 0.43) {
      alpa <- 0.8266
      helm <- 0.9436
      ratio <- 0.9001
    }
    if (Qts == 0.44) {
      alpa <- 0.7521
      helm <- 0.9212
      ratio <- 0.866
    }
    if (Qts == 0.45) {
      alpa <- 0.6868
      helm <- 0.8992
      ratio <- 0.8348
    }
    if (Qts == 0.46) {
      alpa <- 0.6297
      helm <- 0.878
      ratio <- 0.8064
    }
    if (Qts == 0.47) {
      alpa <- 0.5798
      helm <- 0.8578
      ratio <- 0.7804
    }
    if (Qts == 0.48) {
      alpa <- 0.5361
      helm <- 0.8385
      ratio <- 0.7567
    }
    if (Qts == 0.49) {
      alpa <- 0.4978
      helm <- 0.8203
      ratio <- 0.7351
    }
    if (Qts == 0.5) {
      alpa <- 0.4642
      helm <- 0.8031
      ratio <- 0.7155
    }
    if (Qts == 0.51) {
      alpa <- 0.4345
      helm <- 0.787
      ratio <- 0.6975
    }
    if (Qts == 0.52) {
      alpa <- 0.4083
      helm <- 0.7719
      ratio <- 0.681
    }
    if (Qts == 0.53) {
      alpa <- 0.3849
      helm <- 0.7578
      ratio <- 0.6659
    }
    if (Qts == 0.54) {
      alpa <- 0.364
      helm <- 0.7445
      ratio <- 0.652
    }
    if (Qts == 0.55) {
      alpa <- 0.3453
      helm <- 0.7321
      ratio <- 0.6393
    }
    if (Qts == 0.56) {
      alpa <- 0.3284
      helm <- 0.7205
      ratio <- 0.6275
    }
    if (Qts == 0.57) {
      alpa <- 0.3131
      helm <- 0.7096
      ratio <- 0.6166
    }
    if (Qts == 0.58) {
      alpa <- 0.2992
      helm <- 0.6993
      ratio <- 0.6065
    }
    if (Qts == 0.59) {
      alpa <- 0.2865
      helm <- 0.6896
      ratio <- 0.5971
    }
    if (Qts == 0.6) {
      alpa <- 0.2749
      helm <- 0.6805
      ratio <- 0.5883
    }
    if (Qts == 0.61) {
      alpa <- 0.2641
      helm <- 0.6719
      ratio <- 0.5802
    }
    if (Qts == 0.62) {
      alpa <- 0.2542
      helm <- 0.6638
      ratio <- 0.5726
    }
    if (Qts == 0.63) {
      alpa <- 0.2449
      helm <- 0.6561
      ratio <- 0.5654
    }
    if (Qts == 0.64) {
      alpa <- 0.2363
      helm <- 0.6488
      ratio <- 0.5587
    }
    if (Qts == 0.65) {
      alpa <- 0.2283
      helm <- 0.6418
      ratio <- 0.5524
    }
    if (Qts == 0.66) {
      alpa <- 0.2208
      helm <- 0.6353
      ratio <- 0.5465
    }
    if (Qts == 0.67) {
      alpa <- 0.2136
      helm <- 0.6289
      ratio <- 0.5409
    }
    if (Qts == 0.68) {
      alpa <- 0.2069
      helm <- 0.6229
      ratio <- 0.5355
    }
    if (Qts == 0.69) {
      alpa <- 0.2006
      helm <- 0.6171
      ratio <- 0.5305
    }
    if (Qts == 0.7) {
      alpa <- 0.1946
      helm <- 0.6116
      ratio <- 0.5258
    }
    
  }  
  
  vented_response_table_output<-data.frame(alpa, helm, ratio)
  return(vented_response_table_output)
}






#######################################################################Header Menus#########################################################################

header <- dashboardHeader(title = Title, titleWidth = TitleWidth)
########################################################################################################################################################################################
######################################################################################Side Bar Menus####################################################################################
sidebar <- dashboardSidebar(width = SideBar_Width, 
                            
                    sidebarMenu(
                    
                      menuItem(h4("Driver Parameters", style = "font-family: 'Lobster', cursive;
                              font-weight: 500; line-height: 1.1; color:#00688B;"), tabName = "parameters", icon = icon("bar-chart-o", "fa-2x")),
                      
                      menuItem(h4("Enclosure Design", style = "font-family: 'Lobster', cursive;
                              font-weight: 500; line-height: 1.1; color:#00688B;"), icon = icon("bar-chart-o", "fa-2x"),  
                      menuSubItem(h4("Vented and Extended Bass Shelf", style = "font-family: 'Lobster', cursive;
                              font-weight: 500; line-height: 1.1; color:#00688B;"),  tabName = "vented", icon = icon("line-chart", "fa-2x")),
                      menuSubItem(h4("Sealed", style = "font-family: 'Lobster', cursive;
                              font-weight: 500; line-height: 1.1; color:#00688B;"), tabName = "sealed", icon = icon("line-chart", "fa-2x")),
                      menuSubItem(h4("Bandpass", style = "font-family: 'Lobster', cursive;
                              font-weight: 500; line-height: 1.1; color:#00688B;"), tabName = "bandpass", icon = icon("line-chart", "fa-2x"))
                      ),
                       menuItem(h4("Box Tools", style = "font-family: 'Lobster', cursive;
                              font-weight: 500; line-height: 1.1; color:#00688B;"), tabName = "tools", icon = icon("line-chart", "fa-2x")),
                      sliderInput("w", label =  h2("Frequency Range", style = "font-family: 'Lobster', cursive;
                       font-weight: 500; line-height: 1.1; 
                       color: #8FBC8F;"), value=c(1,250), min=1, max = 1000, step=1),
                      menuItem(h4("Computational Analytics Website", style = "font-family: 'Lobster', cursive;
                              font-weight: 500; line-height: 1.1; color:#00688B;"), icon = icon("user", "fa-2x"), href = "https://companalytics.net")
                              )

)



dashboardSidebar(width = SideBar_Width, 
                            
                            sidebarMenu(
                              menuItem(icon = icon("dashboard", "fa-2x"), "", tabName="tab2"),
                              menuItem("", tabName = "tab", icon = icon("bar-chart-o", "fa-2x"))
                           
                             
                              )
                            
                              )





######################################################################################################################################################################################
