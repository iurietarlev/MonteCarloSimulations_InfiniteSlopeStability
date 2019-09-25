# rm(list=ls())
# 
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

library(shiny)          #shiny allows tranlation of R script into HTML
library(ggplot2)        #ggplot for plotting graphs and histograms
library(evd)            #for extreme value distributions (gumbel)
library(ggpubr)         #used for ggarrange
library(pracma)         #practical math for deg2rad
library(expss)          #used for count_if
library(shinydashboard) #shiny dashboard builder
library(plotly)         #for plotting interactive plots


#IMPORTING ALL THE NECESSARY FUNCTIONS CREATED IN ANOTHER FILE
source("Monte_Carlo_Functions_2.R")

#SOME STYLING PARAMTERS
css_style_head = "font-size: 20px; color: white" #font-weight: bold
choice_label <- "Specify distribution using:"
choice_csv <- "CSV file"
choice_m_sd <- "Mean and Standard Deviation"
choice_const <- "Constant"

mean_label <- "Mean"
sd_label <- "Standard Deviation"

tabs_panel_title <- "Probability Paper"
width_tab_box_prob_paper <- 12
#height_tab_box_prob_paper <- "350px"

distributions_list <- c("", "Uniform", "Normal", "Lognormal", "Gumbel")
sidebarpanel_width = 12



#UI PART OF THE SCRIPT
ui <- dashboardPage(skin = "yellow", # yellow color of the header
  dashboardHeader(title = "Monte Carlo Simulation of Slope Failure Probability", titleWidth = 480,
                  tags$li(a(href="https://www.bath.ac.uk", img(src = 'Shiny_Monte_Carlo.png',
                                  title = "Company Home", height = "40px"),
                              style = "padding-top:5px; padding-bottom:5px;
                              margin-right:0px;"),
                          class = "dropdown")),  # title width and name
    dashboardSidebar(width = 350,
      tags$style(HTML(".sidebar-menu li a { font-size: 20px; }",
                      " .skin-blue .sidebar-menu > li.active > a {
                        border-left-color: #ff0000;
                      }")),
      #adds scroll bar only for the sidebar panel
      tags$head(
        tags$style(HTML(".sidebar {
                        height: 100vh; overflow-y: auto;
                        }"
               ) # close HTML       
        )            # close tags$style
        ), 
      sidebarMenu(
        #----------------- number of iterations ----------------
        menuItem(startExpanded = TRUE, text = tags$p(style = css_style_head,"Number of Iterations"), tabName = "iter_no",
                 radioButtons(inputId="iter_choice", label="Specify input type", choices=c("Slider input", "Numeric input")),
                 uiOutput("iter_param_choice"), h5(HTML("&nbsp;"), align = "left")),
        
        #------------------- soil parameters -------------------
        menuItem(startExpanded = F, text = tags$p(style = css_style_head, HTML("Soil Weight (&gamma;)")), tabName = "soil_weight", 
        radioButtons(inputId="soil_choice", label=choice_label, choices=c(choice_const, choice_m_sd, choice_csv), 
                     selected = choice_const),
        uiOutput("soil_param_choice"), h5(HTML("&nbsp;"), align = "left")),
        
        #------------------- angle of slope --------------------
        menuItem(startExpanded = F, text = tags$p(style = css_style_head, HTML("Slope angle (&beta;)")), tabName = "slope_angle", 
        radioButtons(inputId="slope_choice", label=choice_label, choices=c(choice_const, choice_m_sd, choice_csv),
                     selected = choice_const),
        uiOutput("slope_param_choice"), h5(HTML("&nbsp;"), align = "left")),
        
        #----------- angle of shearing resistance- -------------
        menuItem(startExpanded = F, text = tags$p(style = css_style_head, HTML("Shear resistance angle (&phi;')")), tabName = "phi_angle",
        radioButtons(inputId="phi_choice", label=choice_label, choices=c(choice_const, choice_m_sd, choice_csv),
                     selected = choice_m_sd),
        uiOutput("phi_param_choice"), h5(HTML("&nbsp;"), align = "left")),
        
        #-------------------- cohesion of soil -----------------
        menuItem(startExpanded = F, text = tags$p(style = css_style_head, HTML("Cohesion of soil (c')")), tabName = "cohesion",
        radioButtons(inputId="c_choice", label=choice_label, choices=c(choice_const, choice_m_sd, choice_csv),
                     selected = choice_m_sd),
        uiOutput("c_param_choice"), h5(HTML("&nbsp;"), align = "left")),
        
        #------------ ground-water fraction height -------------
        menuItem(startExpanded = FALSE,text = tags$p(style = css_style_head,HTML("Groundwater fraction height (m)")), tabName = "depth_d",
                 radioButtons(inputId="m_choice", label=choice_label, choices=c(choice_const, choice_m_sd, choice_csv),
                              selected = choice_m_sd),
                 uiOutput("m_param_choice"), h5(HTML("&nbsp;"), align = "left")),
        
        #------------------------- depth -----------------------
        menuItem(startExpanded = FALSE, text = tags$p(style = css_style_head, HTML("Depth (H)")), tabName = "depth_d",
        radioButtons(inputId="H_choice", label=choice_label, choices=c(choice_const, choice_m_sd, choice_csv),
                     selected = choice_const),
        uiOutput("H_param_choice"), h5(HTML("&nbsp;"), align = "left")),
        
        #------------------- water unit weight -----------------
        menuItem(startExpanded = FALSE, text = tags$p(style = css_style_head,HTML("Water Unit Weight (&gamma;<sub>w</sub>)")), tabName = "water_unit_weight",
        sliderInput(inputId = "gamma_w", label = "", value = 9.81, min = 9.78, max = 9.83, step = 0.01), h5(HTML("&nbsp;"), align = "left")),
        #numericInput(inputId = "gamma_w", "", value = 9.81),
      
        #------------------ equation used for the simulations ------------------
        h5(HTML("&nbsp;"), align = "left"),
        h4(HTML("&nbsp;&nbsp;&nbsp;&nbsp; Infinite Slope Stability Equation:")),
        h5(HTML("&nbsp;"), align = "left"),
        h5(withMathJax("$$FOS=\\frac{c' + (\\gamma - m\\gamma_w)H\\cos^{2}(\\beta)\\tan(\\phi')}
                    {\\gamma H\\sin(\\beta)cos(\\beta)}$$"), align = "left"),
        
        #------------------ authors names and link to documentation ------------------
        h5(withMathJax("$$$$"), align = "left"), # create a gap between the formula and what's below
        h5(HTML("&nbsp;"), align = "left"),
        tags$div(class="Header", checked=NA,
                 tags$a(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Author: Iurie Tarlev"))
                  ),
        tags$div(class="Header", checked=NA,
                 tags$a(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Co-author: Davyd Tamrazov"))
        ),
        tags$div(class="Header", checked=NA,
                 tags$a(href="https://github.com/osk849/Monte-Carlo-Simulations---Infinite-Slope-Stability.git", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Click here to access the documentation"))
        ))
      ),
      
    dashboardBody(style = "height: 40vh",
      fluidRow(column(width = 6, uiOutput("ui")),
               column(width = 6, uiOutput("ui1"))),
      fluidRow( style = "height: 40vh",
                column(width = 8,
                          box(width = 12,
                          title = "Monte Carlo Factor of Safety Histogram",
                          closable = FALSE,
                          collapsible = FALSE,
                          plotlyOutput("myPlot"))),
                column(width = 4, valueBoxOutput("sim_no_box", width = 12),
                       valueBoxOutput("mean_prob_box", width = 12),
                       valueBoxOutput("sd_prob_box", width = 12),
                       valueBoxOutput("fail_prob_box", width = 12)
                       ),
      column(width=12, wellPanel(textOutput("comments"))))
      
      
  ))

#SERVER PART OF THE SCRIPT
server <- function(input, output, session) {
  
  ##### -----------> RENDERING THE INPUT DISPLAY BASED ON SELECTED INPUT CHOICE <--------------------######
  output$iter_param_choice = renderUI({
    input_type_selector_iter(input_choice <- input$iter_choice, slid = "Slider input", num = "Numeric Input", value = "iter_n")
  })
  

  
  
  
  # ----- soil weight (gamma) -------
  output$soil_param_choice = renderUI({
      input_type_selector(input_choice <- input$soil_choice, csv = choice_csv, m_sd = choice_m_sd,
                        csv_tag = "soil_csv", csv_dist_tag = "soil_csv_dist_type",
                        m_tag = "soil_m", sd_tag = "soil_sd", m_val = 1, sd_val = 1,
                        m_sd_dist_tag = "soil_m_sd_dist_type", const_tag = "soil_const", const_value = 17,
                        dist_choices = distributions_list , selected_m_sd_distr = distributions_list[1],
                        selected_csv_distr = distributions_list[4])
  })
  
  # ----- slope angle (beta) -------
  output$slope_param_choice = renderUI({
    input_type_selector(input_choice <- input$slope_choice, csv = choice_csv, m_sd = choice_m_sd,
                        csv_tag = "slope_csv", csv_dist_tag = "slope_csv_dist_type",
                        m_tag = "slope_m", sd_tag = "slope_sd", m_val = "", sd_val = "", 
                        m_sd_dist_tag = "slope_m_sd_dist_type", const_tag = "slope_const", const_value = 18, 
                        dist_choices = distributions_list , selected_m_sd_distr = distributions_list[1], 
                        selected_csv_distr = distributions_list[4])
  })
  
  # ----- shear resistance angle (phi) -------
  output$phi_param_choice = renderUI({
    input_type_selector(input_choice <- input$phi_choice, csv = choice_csv, m_sd = choice_m_sd,
                        csv_tag = "phi_csv", csv_dist_tag = "phi_csv_dist_type",
                        m_tag = "phi_m", sd_tag = "phi_sd", m_val = 22.5, sd_val = 4, 
                        m_sd_dist_tag = "phi_m_sd_dist_type", const_tag = "phi_const", const_value = "", 
                        dist_choices = distributions_list , selected_m_sd_distr = distributions_list[2], 
                        selected_csv_distr = distributions_list[1])
  })
  
  # ----- cohesion (c) -------
  output$c_param_choice = renderUI({
    input_type_selector(input_choice <- input$c_choice, csv = choice_csv, m_sd = choice_m_sd,
                        csv_tag = "c_csv", csv_dist_tag = "c_csv_dist_type",
                        m_tag = "c_m", sd_tag = "c_sd", m_val = 20, sd_val = 10, 
                        m_sd_dist_tag = "c_m_sd_dist_type", const_tag = "c_const", const_value = "", 
                        dist_choices = distributions_list , selected_m_sd_distr = distributions_list[4], 
                        selected_csv_distr = distributions_list[1])
  })
  
  # ----- depth (H) -------
  output$H_param_choice = renderUI({
    input_type_selector(input_choice <- input$H_choice, csv = choice_csv, m_sd = choice_m_sd,
                        csv_tag = "H_csv", csv_dist_tag = "H_csv_dist_type",
                        m_tag = "H_m", sd_tag = "H_sd", m_val = "", sd_val = "", 
                        m_sd_dist_tag = "H_m_sd_dist_type", const_tag = "H_const", const_value = 9.2, 
                        dist_choices = distributions_list , selected_m_sd_distr = distributions_list[1], 
                        selected_csv_distr = distributions_list[1])
  })
  
  # ----- ground water height ratio (m) -------
  output$m_param_choice = renderUI({
    input_type_selector_gwh(input_choice <- input$m_choice, csv = choice_csv, m_sd = choice_m_sd,
                        csv_tag = "m_csv", csv_dist_tag = "m_csv_dist_type",
                        m_tag = "m_m", sd_tag = "m_sd", m_val = 0.5, sd_val = 1/sqrt(12), 
                        m_sd_dist_tag = "m_m_sd_dist_type", const_tag = "m_const", const_value = "", 
                        dist_choices = distributions_list, selected_m_sd_distr = distributions_list[2], 
                        selected_csv_distr = distributions_list[1])
  })
  
  
  ###### -------------------------> PROBABILITY PAPER PLOTS <----------------------------------#####
  
  #---> soil weight <----
  output$soil_gf <- renderPlot({
    infile <- input$soil_csv
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    infile <- read.csv(infile$datapath, header = F)$V1
    var <- goodness_of_fit(original_df = infile, line_col = "red")
    return(var$four_plots)
   
  })
  
  #---> slope angle <----
  output$slope_gf <- renderPlot({
    infile <- input$slope_csv
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    infile <- read.csv(infile$datapath, header = F)$V1
    var <- goodness_of_fit(original_df = infile, line_col = "red")
    return(var$four_plots)
  })
  
  #---> phi angle <----
  output$phi_gf <- renderPlot({
    infile <- input$phi_csv
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    infile <- read.csv(infile$datapath, header = F)$V1
    var <- goodness_of_fit(original_df = infile, line_col = "red")
    return(var$four_plots)
  })
  
  #---> cohesion angle <----
  output$c_gf <- renderPlot({
    infile <- input$c_csv
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    infile <- read.csv(infile$datapath, header = F)$V1
    var <- goodness_of_fit(original_df = infile, line_col = "red")
    return(var$four_plots)
  })
  
  #---> depth <----
  output$H_gf <- renderPlot({
    infile <- input$H_csv
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    infile <- read.csv(infile$datapath, header = F)$V1
    var <- goodness_of_fit(original_df = infile, line_col = "red")
    return(var$four_plots)
  })
  
  #---> GW depth ratio <----
  output$m_gf <- renderPlot({
    infile <- input$m_csv
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    infile <- read.csv(infile$datapath, header = F)$V1
    var <- goodness_of_fit(original_df = infile, line_col = "red")
    return(var$four_plots)
    
  })
  
  ########### -------------> CREATION OF PROBABILITY PAPER TABS <-------------------------  ######### 
  output$ui <- renderUI({

    check1 <- input$soil_choice == choice_csv
    check2 <- input$slope_choice == choice_csv
    check3 <- input$phi_choice == choice_csv
    check4 <- input$c_choice == choice_csv
    check5 <- input$H_choice == choice_csv
    check6 <- input$m_choice == choice_csv
    
    if(length(check1)==0){check1 <- F}
    if(length(check2)==0){check2 <- F}
    if(length(check3)==0){check3 <- F}
    if(length(check4)==0){check4 <- F}
    if(length(check5)==0){check5 <- F}
    if(length(check6)==0){check6 <- F}
    
    tabs <- list()
    k <- 0
    
    if (check1 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel(HTML("&gamma;"), div( plotOutput("soil_gf")) ) #style = 'overflow-y:scroll;',
    }
    
    if (check2 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel(HTML("&beta;"), div( plotOutput("slope_gf")))#style = 'overflow-y:scroll;',
    }
    
    if (check3 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel(HTML("&phi;'"), div(plotOutput("phi_gf")))#style = 'overflow-y:scroll;',
    }
    
    if (check4 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel("c'", div(plotOutput("c_gf")))#style = 'overflow-y:scroll;',
    }
    
    if (check5 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel("H", div(plotOutput("H_gf")))#style = 'overflow-y:scroll;',
    }
    
    if (check6 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel("m", div(plotOutput("m_gf")))#style = 'overflow-y:scroll;',
    }
    
    do.call(tabBox, c(tabs, #height = height_tab_box_prob_paper,
                      width = width_tab_box_prob_paper, title = "Probability Paper - for csv type data",
                      id = "prob_paper_tabs"))
  })
  
  output$ui1 <- renderUI({
    
    check1 <- input$soil_choice == choice_csv || input$soil_choice == choice_m_sd
    check2 <- input$slope_choice == choice_csv || input$slope_choice == choice_m_sd
    check3 <- input$phi_choice == choice_csv || input$phi_choice == choice_m_sd
    check4 <- input$c_choice == choice_csv || input$c_choice == choice_m_sd
    check5 <- input$H_choice == choice_csv || input$H_choice == choice_m_sd
    check6 <- input$m_choice == choice_csv || input$m_choice == choice_m_sd
    
    if(length(check1)==0){check1 <- F}
    if(length(check2)==0){check2 <- F}
    if(length(check3)==0){check3 <- F}
    if(length(check4)==0){check4 <- F}
    if(length(check5)==0){check5 <- F}
    if(length(check6)==0){check6 <- F}
    
    tabs <- list()
    k <- 0
    
    if (check1 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel(HTML("&gamma;"), div(plotlyOutput("soil_dist"))) #style = 'overflow-y:scroll;', 
    }
    
    if (check2 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel(HTML("&beta;"), div(plotlyOutput("slope_dist")))#style = 'overflow-y:scroll;', 
    }
    
    if (check3 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel(HTML("&phi;'"), div(plotlyOutput("phi_dist")))#style = 'overflow-y:scroll;', 
    }
    
    if (check4 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel("c'", div(plotlyOutput("c_dist")))#style = 'overflow-y:scroll;', 
    }
    
    if (check5 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel("H", div(plotlyOutput("H_dist")))#style = 'overflow-y:scroll;', 
    }
    
    if (check6 == T){ 
      k <- k+1
      tabs[[k]] <- tabPanel("m", div(plotlyOutput("m_dist")))#style = 'overflow-y:scroll;', 
    }
    
    #do.call(tabBox, tabs)
    do.call(tabBox, c(tabs, #height = height_tab_box_prob_paper,
                      width = width_tab_box_prob_paper, title = "Parameter Distributions",
                      id = "param_dist_tabs"))
  })
  
  
  
  ####### ---------------> GENERATION OF RANDOM VARIABLES BASED ON THEIR DISTRIBUTION TYPE <------------- #########
  
  # water unit weight
  gamma_w <- reactive(input$gamma_w)

  #soil unit weight
  gamma <- reactive(dist_choice_final(n = input$iter_n, df = input$soil_csv, dist_type_csv = input$soil_csv_dist_type,
                             m = input$soil_m, sd = input$soil_sd, dist_type_m_sd = input$soil_m_sd_dist_type,
                             constant_value = input$soil_const, input_choice = input$soil_choice,
                             csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const))
  #slope angle
  slope <- reactive(dist_choice_final(n = input$iter_n, df = input$slope_csv, dist_type_csv = input$slope_csv_dist_type,
                             m = input$slope_m, sd = input$slope_sd, dist_type_m_sd = input$slope_m_sd_dist_type,
                             constant_value = input$slope_const, input_choice = input$slope_choice,
                             csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const))
  # cohesion value
  c_c <- reactive(dist_choice_final(n = input$iter_n, df = input$c_csv, dist_type_csv = input$c_csv_dist_type,
                                      m = input$c_m, sd = input$c_sd, dist_type_m_sd = input$c_m_sd_dist_type,
                                      constant_value = input$c_const, input_choice = input$c_choice,
                                      csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const))

  #soil shear resistance angle
  phi <- reactive(dist_choice_final(n = input$iter_n, df = input$phi_csv, dist_type_csv = input$phi_csv_dist_type,
                           m = input$phi_m, sd = input$phi_sd, dist_type_m_sd = input$phi_m_sd_dist_type,
                           constant_value = input$phi_const, input_choice = input$phi_choice,
                           csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const))
  
  #depth 
  H <- reactive(dist_choice_final(n = input$iter_n, df = input$H_csv, dist_type_csv = input$H_csv_dist_type,
                         m = input$H_m, sd = input$H_sd, dist_type_m_sd = input$H_m_sd_dist_type,
                         constant_value = input$H_const, input_choice = input$H_choice,
                         csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const))
  #ground water height ratio
  m <- reactive(dist_choice_final(n = input$iter_n, df = input$m_csv, dist_type_csv = input$m_csv_dist_type,
                         m = input$m_m, sd = input$m_sd, dist_type_m_sd = input$m_m_sd_dist_type,
                         constant_value = input$m_const, input_choice = input$m_choice,
                         csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const))
  
  # --------------> CALCULATION OF FACTOR OF SAFETY POINTS, BASED ON THE ABOVE CALCULATED PARAMETERS <--------#
  FOS <- reactive((c_c()+(gamma()-m()*gamma_w())*H()*cos(deg2rad(slope()))^2
          *tan(deg2rad(phi())))/(gamma()*H()*sin(deg2rad(slope()))*cos(deg2rad(slope()))))
  
  
  
  
  # ------> RENDERING THE DISTRIBUTION HISTOGRAMS FOR EVERY ONE OF THE VARIABLES (apart from constants) <------#
  
  # soil weight
  output$soil_dist <- renderPlotly({
    req(input$iter_n)
    p <- hist_plot(param_df = gamma(), input_choice = input$soil_choice, dist_type_csv = input$soil_csv_dist_type
              ,dist_type_m_sd = input$soil_m_sd_dist_type, csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const, 
              x_axis = "Soil Weight Distribution")
    ggplotly(p)
  })
  
  # slope angle
  output$slope_dist <- renderPlotly({
    req(input$iter_n)
    p <- hist_plot(param_df = slope(), input_choice = input$slope_choice, dist_type_csv = input$slope_csv_dist_type
              ,dist_type_m_sd = input$slope_m_sd_dist_type, csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const, 
              x_axis = "Slope Angle Distribution")
    ggplotly(p)
  })
  
  # soil shear resistance angle
  output$phi_dist <- renderPlotly({
    req(input$iter_n)
    p <- hist_plot(param_df = phi(), input_choice = input$phi_choice, dist_type_csv = input$phi_csv_dist_type
              ,dist_type_m_sd = input$phi_m_sd_dist_type, csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const, 
              x_axis = "Shear Angle Distribution")
    ggplotly(p)
  })
  
  # soil cohesion
  output$c_dist <- renderPlotly({
    req(input$iter_n)
    p <- hist_plot(param_df = c_c(), input_choice = input$c_choice, dist_type_csv = input$c_csv_dist_type
              ,dist_type_m_sd = input$c_m_sd_dist_type, csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const, 
              x_axis = "Cohesion Distribution")
    ggplotly(p)
  })
  
  # depth
  output$H_dist <- renderPlotly({
    req(input$iter_n)
    p <- hist_plot(param_df = H(), input_choice = input$H_choice, dist_type_csv = input$H_csv_dist_type
              ,dist_type_m_sd = input$H_m_sd_dist_type, csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const, 
              x_axis = "Depth Distribution")
    ggplotly(p)
  })
  
  #ground water height ratio
  output$m_dist <- renderPlotly({
    req(input$iter_n)
    p <- hist_plot(param_df = m(), input_choice = input$m_choice, dist_type_csv = input$m_csv_dist_type
              ,dist_type_m_sd = input$m_m_sd_dist_type, csv = choice_csv,  m_sd = choice_m_sd, constant = choice_const, 
              x_axis = "Ratio “m” Distribution")
    ggplotly(p)
  })
  
  
  # --------------------------> RENDER FACTOR OF SAFETY HISTOGRAM PLOT <-----------------------###
  output$myPlot <- renderPlotly({
    req(input$iter_n)
    req(FOS())
    FOS_df <- as.data.frame(FOS())
    FOS_df <- data.frame(x=FOS_df, above=FOS_df>=1 )
  
    colnames(FOS_df) <- c("FOS", "above")
    
    p <- ggplot(FOS_df, aes(x=FOS, fill = above)) +
      geom_histogram(color="black", binwidth = 0.1, breaks = seq(0, max(FOS()), by=0.1)) + 
      ylab("Count") + xlab("Factor of Safety")
      
    ggplotly(p) %>% add_annotations(text="Above 1:", xref="paper", yref="paper",
                                    x=0.4, xanchor="right",
                                    y=-0.3, yanchor="bottom",    # Same y as legend below
                                    legendtitle=TRUE, showarrow=FALSE ) %>%
      layout(legend=list(x = 0.4, y = -0.2, orientation = "h",  yanchor="top") )
    
    
  })
  
  ##### ---------> VALUE BOX DISPLAYING NUMBER OF SIMULATIONS PERFORMED <-----------------------####
 
  output$sim_no_box <- renderValueBox({
    req(input$iter_n)
    req(FOS())
    iter_n <- formatC(input$iter_n, format = "d", big.mark = ",")
    valueBox(
      paste0(iter_n), "Simulations", icon = icon("ffas fa-bars", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  #### -------------->  FAILURE PROBABILITY VALUE BOX <-------------------- #########
  output$fail_prob_box <- renderValueBox({
    req(input$iter_n)
    req(FOS())
    FOS_df <- as.data.frame(FOS())
    FOS_df <- data.frame(x=FOS_df, above=FOS_df>=1 )
    

    colnames(FOS_df) <- c("FOS", "above")
    df <- FOS_df
    
    P_failure <- round(count_if("FALSE", df$above)/length(df$above)*100, digits = 1)
    P_failure <- signif(P_failure,3)
    
  
    valueBox(
      paste0(P_failure, "%"), "Failure Probability", icon = icon("ffas fa-window-close", lib = "font-awesome"),#icon = icon("remove-sign", lib = "glyphicon"),
      color = "red"
    )
  })
  
  #### -------------->  FOS MEAN VALUE BOX <-------------------- #########
  output$mean_prob_box <- renderValueBox({
    req(input$iter_n)
    req(FOS())
    FOS_mean <- mean(FOS())
    FOS_mean <- signif(FOS_mean, 3)

    valueBox(
      paste0(FOS_mean), "Average Factor of Safety", icon = icon("ffas fa-toggle-down", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  #### -------------->  FOS SD VALUE BOX <-------------------- #########
  output$sd_prob_box <- renderValueBox({
    req(input$iter_n)
    req(FOS())
    FOS_sd <- sd(FOS())
    FOS_sd <- signif(FOS_sd,3)
    
    valueBox(
      paste0(FOS_sd), "Standard Deviation of Factor of Safety", icon = icon("ffas fa-bar-chart-o", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  #### -------------->  BOX WITH COMMENT BASED ON THE OUTPUT PROBABILITY OF FAILURE <-------------------- ######
  output$comments <- renderText({
    req(input$iter_n)
    req(FOS())
    FOS_df <- as.data.frame(FOS())
    FOS_df <- data.frame(x=FOS_df, above=FOS_df>=1 )
    
    
    colnames(FOS_df) <- c("FOS", "above")
    
    df <- FOS_df
    P_failure <- count_if("FALSE", df$above)/length(df$above)*100
    P_failure <- signif(P_failure, 3)
    
    iter_n <- formatC(input$iter_n, format = "d", big.mark = ",")
    comment <- paste("Using infinite slope stability equation and based on", iter_n, " Monte Carlo simulations, there is a", P_failure, "% chance that the slope will fail under the given input parameters.")
   
  })

  
}

# -------- > RUN THE APPLICATION < ---------- 
shinyApp(ui = ui, server = server)

