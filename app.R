## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
# library(dplyr)
library(janitor)
source("equations.R")
source("simLoop.R")

## Sidebar content ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("play")),
    menuItem("Ecologic threshold", tabName = "ecology", icon = icon("fish")),
    menuItem("Economic threshold", tabName = "economy", icon = icon("money-bill")),
    menuItem("Equations", tabName = "equation", icon = icon("magnifying-glass")),
    menuItem("Data input", tabName = "data", icon = icon("th")),
    menuItem("Money tables", tabName = "money", icon = icon("wallet")),
    menuItem("Effort sliders", tabName = "effortslider", icon = icon("align-center")),
    menuItem("Details - Calculations", tabName = "middle", icon = icon("calculator")),
    menuItem("Prints / trouble shooting", tabName = "prints", icon = icon("bug"))
  )
)
## Body content
body <- dashboardBody(
  tabItems(
    # tab 0 dasboard ----
    tabItem(tabName = "about",
            fluidRow(
              h3("title of the app"),
              HTML("blabla"),
              wellPanel(
                HTML("First you need to define the path of the fisheries object"),
                fileInput("fileInput", "Choose an RDS file to load"),
                actionButton("loadObject", "Load Object")
                
                # verbatimTextOutput("objectDetails")
              )
            )
    ),
    # tab 4.1 effort ----
    tabItem(tabName = "effortslider",
            fluidRow(
              wellPanel(
                h3("Effort multipliers"),
                HTML("Here you can manually adjust the effort to check out one specific combination. 
                   The sliders will multiply the current effort loaded with the app. Values from 2019 at the moment. 
                   Careful, starting the new projection with the action button will overwrite the currently loaded object.")
              ),
              wellPanel(
                actionButton("start_new_sim", "Run the projection")
              ),
              wellPanel(
                sliderInput("FLTDS102030_mult", "FLTDS102030", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("FLTDS60_mult", "FLTDS60", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("FLTT10_mult", "FLTT10", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("FLTT2060_mult", "FLTT2060", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("FLTT30_mult", "FLTT30", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("HK_LIG_mult", "HK_LIG", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("Mixed_mult", "Mixed", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("MWOT102060_mult", "MWOT102060", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("MWOT30_mult", "MWOT30", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("TLIG_mult", "TLIG", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("TRED_mult", "TRED", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("WHSDS20_mult", "WHSDS20", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("WHSDS60_mult", "WHSDS60", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1),
                sliderInput("WHST1020_mult", "WHST1020", 
                            min = 0, max = 5, 
                            value = 1, step = 0.1)
              )
            )
            
    ),
    # tab 1 ecology ----
    tabItem(tabName = "ecology",
            fluidRow(
              column(3,
                     wellPanel(
                       actionButton("run", "Run the projection")
                     ),
                     wellPanel(
                       sliderInput("biom_thresh", "biomass threshold", min = 0, max = 1, value = 0.55,
                                   step = 0.01))
              ),
              column(7,
                     plotOutput("plotBiomass", width = 900, height = 600)
              )
            ),
            fluidRow(
              textOutput("surviving_sp")
            )
    ),
    
    # tab 1.1 equations ----
    tabItem(tabName = "equation",
            fluidRow(
              h3("equations used to calculate the ecologic threshold"),
              HTML("no equations there, just a >= or not"),
              h3("equations used to calculate the economic threshold"),
              HTML("CMS, catch per metier. Freight cost"),
              h3("equations used to calculate the social threshold"),
              HTML("blabla"),
              h3("your own equations"),
              HTML("If you decide to update your own equations, they are all present as functions in the XXX.R file. 
                   Updating the code there will change the behavior of the app. Be mindful to respect the format of inputs and outputs.")
            )
    ),
    
    ## tab 2 data input ----
    tabItem(tabName = "data",
            h2("Parameter input"),
            fluidRow(
              column(4,
                     h3("Danish Seine fleet parameters"),
                     numericInput("DS_var", "Variable cost per shot in aud",
                                  value =  85.64 + 27),
                     numericInput("DS_fre", "Freight cost in aud per kg of freight",
                                  value =  0.207),
                     numericInput("DS_crew", "Average share of the crew as a proportion (e.g. if 50% write .5)",
                                  value = 0.402636533),
                     numericInput("DS_fix", "Fixed costs, annual per vessel",
                                  value = 85536),
                     numericInput("DS_cap", "Capital costs per vessel, used at 7.9% a year",
                                  value = 305987),
                     numericInput("DS_dep", "Deprecation cost as a proportion",
                                  value = .02),
                     numericInput("DS_noc", "Average number of crew member per vessel",
                                  value = 3.5),
                     numericInput("DS_day", "Average number of days at sea, annual",
                                  value = 111),
                     # prettyCheckbox(
                     #   inputId = "fix_rate", label = "Are the fixed costs entered as a proportion instead? Check if yes", icon = icon("check")
                     # ) # default are from Sean Paul and its abares 2015 data
                     # average crew humber is Fulton parametrisation of Atlantis (extremly hard to find any number)
              ),
              column(4,
                     h3("Trawler fleet parameters"),
                     numericInput("TW_var", "Variable cost per shot in aud",
                                  value = 969.76 + 388),
                     numericInput("TW_fre", "Freight cost in aud per kg of freight",
                                  value =  0.175),
                     numericInput("TW_crew", "Average share of the crew as a proportion (e.g. if 50% write .5)",
                                  value = 0.229013145),
                     numericInput("TW_fix", "Fixed and capital costs, annual per vessel",
                                  value = 144128),
                     numericInput("TW_cap", "Capital costs per vessel, used at 7.9% a year",
                                  value = 506098),
                     numericInput("TW_dep", "Deprecation cost as a proportion",
                                  value = .03),
                     numericInput("TW_noc", "Average number of crew member per vessel",
                                  value = 4),
                     numericInput("TW_day", "Average number of days at sea, annual",
                                  value = 145)
              ),
              column(4,
                     h3("GHK fleet parameters"),
                     numericInput("GH_var", "Variable cost per shot in aud",
                                  value = 385.48 + 380.33),
                     numericInput("GH_fre", "Freight cost in aud per kg of freight",
                                  value =  0.043669),
                     numericInput("GH_crew", "Average share of the crew as a proportion (e.g. if 50% write .5)",
                                  value = .428),
                     numericInput("GH_fix", "Fixed and capital costs, annual per vessel",
                                  value = 114604.4),
                     numericInput("GH_cap", "Capital costs per vessel, used at 7.9% a year",
                                  value = 331460.6),
                     numericInput("GH_dep", "Deprecation cost as a proportion",
                                  value = .06),
                     numericInput("GH_noc", "Average number of crew member per vessel",
                                  value = 2),
                     numericInput("GH_day", "Average number of days at sea, annual",
                                  value = 80)
              )
            ),
            fluidRow(
              column(4,
                     h3("Full time equivalent vessels per metier"),  # values from Sean
                     numericInput("FLTDS102030_ves", "FLTDanishSeiene102030",
                                  value = 9.72),
                     numericInput("FLTDS60_ves", "FLTDanishSeiene60",
                                  value = 6.71),
                     numericInput("FLTT10_ves", "FLTTrawl10",
                                  value = 6.53)),
              numericInput("FLTT2060_ves", "FLTTrawl2060",
                           value = 9.94),
              numericInput("FLTT30_ves", "FLTTrawl30",
                           value = 3.46),
              numericInput("HK_LIG_ves", "HK_LIG",
                           value = 2),
              numericInput("Mixed_ves", "Mixed",
                           value = 10),
              numericInput("MWOT102060_ves", "MWOTrawl102060",
                           value = 0.86),
              numericInput("MWOT30_ves", "MWOTrawl30",
                           value = 0.34),
              numericInput("TLIG_ves", "TrawlLIG",
                           value = 6.5),
              numericInput("TRED_ves", "TrawlRED",
                           value = 0.25),
              numericInput("WHSDS20_ves", "WHSDanishSeine20",
                           value = 0.5),
              numericInput("WHSDS60_ves", "WHSDanishSeine60",
                           value = 6.69),
              numericInput("WHST1020_ves", "WHSTrawl1020",
                           value = 1.02),
              #estimating values of mixed, hk lig, trawl red and trawl lig from shots number
              column(4,
                     h3("Number of shots per metier per year"),
                     numericInput("FLTDS102030_shot", "FLTDanishSeiene102030",
                                  value = 4653),
                     numericInput("FLTDS60_shot", "FLTDanishSeiene60",
                                  value = 2118),
                     numericInput("FLTT10_shot", "FLTTrawl10  ",
                                  value = 820),
                     numericInput("FLTT2060_shot", "FLTTrawl2060",
                                  value = 2931),
                     numericInput("FLTT30_shot", "FLTTrawl30",
                                  value = 1214),
                     numericInput("HK_LIG_shot", "HK_LIG",
                                  value = 214),
                     numericInput("Mixed_shot", "Mixed",
                                  value = 5574),
                     numericInput("MWOT102060_shot", "MWOTrawl102060",
                                  value = 128),
                     numericInput("MWOT30_shot", "MWOTrawl30",
                                  value = 161),
                     numericInput("TLIG_shot", "TrawlLIG",
                                  value = 854),
                     numericInput("TRED_shot", "TrawlRED",
                                  value = 46),
                     numericInput("WHSDS20_shot", "WHSDanishSeine20",
                                  value = 103),
                     numericInput("WHSDS60_shot", "WHSDanishSeine60",
                                  value = 2278),
                     numericInput("WHST1020_shot", "WHSTrawl1020",
                                  value = 218),
              ),
              column(4,
                     h3("Fish price in aud per kg"),
                     numericInput("FLT_price", "Cost of FLT", # flathead
                                  value = 6.20),
                     numericInput("GRE_price", "Cost of GRE", # blue grenadier
                                  value = 2.40),
                     numericInput("LIG_price", "Cost of LIG", # pink ling
                                  value = 6.58),
                     numericInput("MOW_price", "Cost of MOW", # jackass morwong
                                  value = 3.42),
                     numericInput("RED_price", "Cost of RED", # red fish
                                  value = 3.34),
                     numericInput("SQD_price", "Cost of SQD", # squid
                                  value = 3.79),
                     numericInput("TRS_price", "Cost of TRS", # Silver Warehou
                                  value = 2.03),
                     numericInput("TRT_price", "Cost of TRT", # Blue Warehou
                                  value = 3.30),
                     numericInput("WHS_price", "Cost of WHS", # school whiting 
                                  value = 2.55)
              ) # default are taken from Abares fisheries statistics year 2018-2019
            ),
            fluidRow(
              column(4,
                     h3("Cost per day per vessel per metier in aud if already calculated"),
                     numericInput("FLTDS102030_cost", "FLTDanishSeiene102030",
                                  value = 1754),
                     numericInput("FLTDS60_cost", "FLTDanishSeiene60",
                                  value = 1711),
                     numericInput("FLTT10_cost", "FLTTrawl10  ",
                                  value = 4795),
                     numericInput("FLTT2060_cost", "FLTTrawl2060",
                                  value = 5360),
                     numericInput("FLTT30_cost", "FLTTrawl30",
                                  value = 5461),
                     numericInput("HK_LIG_cost", "HK_LIG",
                                  value = NA),
                     numericInput("Mixed_cost", "Mixed",
                                  value = NA),
                     numericInput("MWOT102060_cost", "MWOTrawl102060",
                                  value = 5119),
                     numericInput("MWOT30_cost", "MWOTrawl30",
                                  value = 5223),
                     numericInput("TLIG_cost", "TrawlLIG",
                                  value = NA),
                     numericInput("TRED_cost", "TrawlRED",
                                  value = NA),
                     numericInput("WHSDS20_cost", "WHSDanishSeine20",
                                  value = 1591),
                     numericInput("WHSDS60_cost", "WHSDanishSeine60",
                                  value = 1655),
                     numericInput("WHST1020_cost", "WHSTrawl1020",
                                  value = 4248))
            )
    ),
    ## tab 3 troubleshooting ----
    tabItem(tabName = "prints",
            fluidPage(
              tableOutput("ecoParam"),
              tableOutput("fishPrice"),
              tableOutput("CMS"),
              # tableOutput("vesselNumber"),
              tableOutput("shotNumber"),
              #tableOutput("GOS_threshold"),
            )
    ),
    # tab 4 economy ----
    tabItem(tabName = "economy",
            fluidPage(
              column(3,
                     wellPanel(
                       actionButton("runEco", "Run the projection")
                     ),
                     wellPanel(
                       numericInput("eco_thresh", "economic threshold, which minimum value should GOS be? in AUD, can be negative",
                                    value = -10000000)),
                     wellPanel(
                       numericInput("humie_thresh", "crew wages, which minimum value should the wages be? in AUD, minimum is 0 (free labor)",
                                    min = 0, value = 1))
              ),
              column(7,
                     plotOutput("eco_plot", width = 900, height = 600),
                     # plotOutput("plot2", width = 600, height = 600),
                     # plotOutput("plot3", width = 600, height = 600),
                     # if(!is.null(dat)) plotOutput("plot4", width = 600, height = 600)
                     # textOutput("text")
              )
            ),
            fluidPage(
              tableOutput("final_effort")
            )
    ),
    #tab 5 money prints ----
    tabItem(tabName = "money",
            fluidPage(
              tableOutput("GOS_threshold"),
              tableOutput("wage_threshold")
            )
    ),
    #tab 6 intermediate steps tables ----
    tabItem(tabName = "middle",
            fluidPage(
              tableOutput("sp_alive"),
              tableOutput("qflt_df"),
              tableOutput("CMS_table"),
              tableOutput("GVL_df"),
              fluidRow(
                column(4, tableOutput("fre_df")),
                column(4, tableOutput("varUE_df")),
                column(4, tableOutput("GVL"))
              ),
              tableOutput("GVL_total"),
              tableOutput("rtbs"),
              fluidRow(
                column(4,tableOutput("GOS")),
                column(4,tableOutput("GOS_Sean"))
              ),
              tableOutput("wage")
              
            )
    )
  ) #end of tabItems
)# end of dashboardbody

ui <- #fluidPage(
  #includeCSS("romain_shiny/www/mycss.css"),
  dashboardPage(
    dashboardHeader(title = "Eco-viability App"),
    sidebar,
    body)
## server ----
server = function(input, output) {
  
  ## Loading simulation object ----
  loaded_object <- reactiveVal(NULL)
  
  observeEvent(input$loadObject, {
    # Check if a file was selected
    if (!is.null(input$fileInput)) {
      file_path <- input$fileInput$datapath
      # Load the object from the selected file
      loaded_object_data <- readRDS(file_path)
      loaded_object(loaded_object_data)
      print("Object loaded successfully.")
    } else {
      # Handle the case when no file is selected
      loaded_object(NULL)
    }
  })
  
  # output$objectDetails <- renderText({
  #   # Display details of the loaded object
  #   if (!is.null(loaded_object())) {
  #     # Access and display information about the loaded object
  #     object_info <- capture.output(str(loaded_object()))
  #     paste("Loaded Object Details:\n", object_info, collapse = "\n")
  #   } else {
  #     "No object loaded."
  #   }
  # })
  
  ## running simulation ----
  # new_sim <- eventReactive(input$start_new_sim, {
  #   mult_vec <- c(input$FLTDS102030_mult,
  #                 input$FLTDS60_mult,
  #                 input$FLTT10_mult,
  #                 input$FLTT2060_mult,
  #                 input$FLTT30_mult,
  #                 input$HK_LIG_mult,
  #                 input$Mixed_mult,
  #                 input$MWOT102060_mult,
  #                 input$MWOT30_mult,
  #                 input$TLIG_mult,
  #                 input$TRED_mult,
  #                 input$WHSDS20_mult,
  #                 input$WHSDS60_mult,
  #                 input$WHST1020_mult)
  #   
  #   res <- simLoop(effortMatrix = mult_vec, estimation.MSP = estimation.MSP,
  #                  groups = groups, P.prey = P.prey, E.Fleet = E.Fleet)
  #   return(res)
  # })
  
  observeEvent(input$start_new_sim, {
    mult_vec <- c(input$FLTDS102030_mult,
                  input$FLTDS60_mult,
                  input$FLTT10_mult,
                  input$FLTT2060_mult,
                  input$FLTT30_mult,
                  input$HK_LIG_mult,
                  input$Mixed_mult,
                  input$MWOT102060_mult,
                  input$MWOT30_mult,
                  input$TLIG_mult,
                  input$TRED_mult,
                  input$WHSDS20_mult,
                  input$WHSDS60_mult,
                  input$WHST1020_mult)
    
    res <- simLoop(effortMatrix = mult_vec, estimation.MSP = estimation.MSP,
                   groups = groups, P.prey = P.prey, E.Fleet = E.Fleet)
    
    biom0 <- simLoop(effortMatrix = rep(0,14), estimation.MSP = estimation.MSP, 
                     groups = groups, P.prey = P.prey, E.Fleet = E.Fleet)$biomass

    res <- res %>% 
      mutate(biom0 = biom0,
             biom_thresh = biomass/biom0,
             effortID = rep(1:(dim(res)[1]/9), each = 9))
    # 
    # res$biom0 <- rep(biom0,(dim(res)[1]/9))
    # res$effortID <-  rep(1:(dim(res)[1]/9), each = 9)
    loaded_object(res)
    print("Simulation ran successfully.")
  })
  
  # observe({print(new_sim())})
  
  ## biomass threshold ----
  biom_list <- eventReactive(input$run, {
    
    # calculating viable effort combination for threshold
    # threshold value selected by user in biom_thresh
    
    min_threshold <- input$biom_thresh 
    
    fish_thresh <- filter(loaded_object(), biom0 >= min_threshold)
    
    biom_threshold <- fish_thresh
    
    no_sp <- length(unique(biom_threshold$species)) # how many species survive the treatment? used for duplicates
    # print(no_sp)
    if(no_sp>1)
    {
      # print(biom_threshold)
      biom_possible <- janitor::get_dupes(biom_threshold[,c(4:17,19)]) # includes effort ID so I know which ones are staying
      # print(biom_possible)
      if(dim(biom_possible)[1]){# if there is at least one duplicate 
        # get_dupes is cool as it adds the number of duplicates but it also keep all duplicate which I don't want.
        biom_viable <- filter(biom_possible, dupe_count == no_sp) # select combination with the maximum number of duplicates
        biom_viable$dupe_count <- NULL
        # saving the vector of viable effort combinations elsewhere
        viable_effort <- biom_viable$effortID
        biom_viable$effortID <- NULL
        biom_viable<- biom_viable[-which(duplicated(biom_viable)),]
        # print(biom_viable)
        rownames(biom_viable) <- unique(viable_effort) # best way to keep effortID and group results with it
        # print(biom_viable)
        plot_dat <- reshape2::melt(as.matrix(biom_viable)) # to keep rownames
        plot_dat$threshold <- input$biom_thresh
        # print(viable_effort)
        # print(dim(plot_dat))
        # print(plot_dat)
      }
      # save also the number of species surviving per threshold
      surviving_species <- c(input$biom_thresh,no_sp,paste(unique(biom_threshold$species), collapse = " "))
      surv_names <- c("biomass_fraction","species_number","species")
      if(is.null(dim(surviving_species))) names(surviving_species) <- surv_names else colnames(surviving_species) <- surv_names
      #print(surviving_species)
      
      # saveRDS(surviving_species, file = "species_eco_threshold.RDS")
      colnames(plot_dat)[1:3] <- c("effortID","metiers", "effort")
      # print(plot_dat)
      return(list(plot_dat,surviving_species,viable_effort))
    } else (print("no surviving species!"))
    
  })
  # biomass threshold plot on ecologic threshold tab
  output$plotBiomass <- renderPlot({
    ggplot(biom_list()[[1]])+
      geom_line(aes(x = metiers, y = effort, group = effortID), alpha = .15) +
      # facet_wrap(.~threshold) +
      scale_y_continuous(name = "effort scalar") +
      theme_bw()
  })
  
  # text below plot showing the names of the highest number of surviving species
  output$surviving_sp <- renderText({ 
    paste0("The maximum number of species surviving at this threshold is ",
           biom_list()[[2]]["species_number"],". They are ",
           biom_list()[[2]]["species"],". ",
           length(unique(biom_list()[[3]])), " unique effort's combination make this threshold.")
  })
  
  
  ## input parameters ----
  
  # contains user input to calculate mosts costs
  ecoParam <- reactive({
    # cout variable per unit of effort per day in aud
    DS_var <- as.numeric(input$DS_var)
    TW_var <- as.numeric(input$TW_var)
    GH_var <- as.numeric(input$GH_var)
    
    # share of the crew average in fraction
    DS_crew <- as.numeric(input$DS_crew)
    TW_crew <- as.numeric(input$TW_crew)
    GH_crew <- as.numeric(input$GH_crew)
    
    # fixed and capital costs , annual per vessel, with the "no west coast" adjustment and the yearly rate for capital costs
    DS_fix <- 0.6354664 * (as.numeric(input$DS_fix) + .079 * as.numeric(input$DS_cap))  
    TW_fix <- 0.6354664 * (as.numeric(input$TW_fix) + .079 * as.numeric(input$TW_cap)) 
    GH_fix <- 0.6354664 * (as.numeric(input$GH_fix) + .079 * as.numeric(input$GH_cap)) 
    
    # deprecation cost from FLo's paper in fraction
    DS_dep <- as.numeric(input$DS_dep)
    TW_dep <- as.numeric(input$TW_dep)
    GH_dep <- as.numeric(input$GH_dep)
    
    # number of crew per vessel on average
    DS_noc <- as.numeric(input$DS_noc)
    TW_noc <- as.numeric(input$TW_noc)
    GH_noc <- as.numeric(input$GH_noc)
    
    # number of days at sea per year on average
    DS_day <- as.numeric(input$DS_day)
    TW_day <- as.numeric(input$TW_day)
    GH_day <- as.numeric(input$GH_day)
    
    # number of vessels on average
    # DS_ves <- as.numeric(input$DS_ves)
    # TW_ves <- as.numeric(input$TW_ves)
    
    # freight cost in aud per kg of catch
    DS_fre <- as.numeric(input$DS_fre)
    TW_fre <- as.numeric(input$TW_fre)
    GH_fre <- as.numeric(input$GH_fre)
    
    
    ecoParam <- data.frame("cVarUE" = c(rep(DS_var,2),rep(TW_var,3),rep(GH_var,2),rep(TW_var,4),
                                        rep(DS_var,2),TW_var),
                           "cShr" = c(rep(DS_crew,2),rep(TW_crew,3),rep(GH_crew,2),rep(TW_crew,4),
                                      rep(DS_crew,2),TW_crew),
                           "cFix" = c(rep(DS_fix,2),rep(TW_fix,3),rep(GH_fix,2),rep(TW_fix,4),
                                      rep(DS_fix,2),TW_fix),
                           "cDep" = c(rep(DS_dep,2),rep(TW_dep,3),rep(GH_dep,2),rep(TW_dep,4),
                                      rep(DS_dep,2),TW_dep),
                           "noCrew" = c(rep(DS_noc,2),rep(TW_noc,3),rep(GH_noc,2),rep(TW_noc,4),
                                        rep(DS_noc,2),TW_noc),
                           "noDay" = c(rep(DS_day,2),rep(TW_day,3),rep(GH_day,2),rep(TW_day,4),
                                       rep(DS_day,2),TW_day),
                           # "noVessel" = c(rep(DS_ves,2),rep(TW_ves,3),DS_ves,rep(TW_ves,4),
                           #             rep(DS_ves,2),TW_ves),
                           "cFre" = c(rep(DS_fre,2),rep(TW_fre,3),rep(GH_fre,2),rep(TW_fre,4),
                                      rep(DS_fre,2),TW_fre))
    ecoParam
  })
  
  output$ecoParam <- renderTable(ecoParam())
  
  fishPrice <- reactive({
    FLT_price <- as.numeric(input$FLT_price)
    GRE_price <- as.numeric(input$GRE_price)
    LIG_price <- as.numeric(input$LIG_price)
    MOW_price <- as.numeric(input$MOW_price)
    RED_price <- as.numeric(input$RED_price)
    SQD_price <- as.numeric(input$SQD_price)
    TRS_price <- as.numeric(input$TRS_price)
    TRT_price <- as.numeric(input$TRT_price)
    WHS_price <- as.numeric(input$WHS_price)
    
    fishPrice<- c(FLT_price,GRE_price,LIG_price,MOW_price,RED_price,SQD_price,
                  TRS_price,TRT_price,WHS_price)*1000 # convert to tons
    names(fishPrice) <- c("FLT", "GRE", "LIG", "MOW", "RED", "SQD", "TRS", "TRT", "WHS")
    fishPrice
  })
  
  output$fishPrice <- renderTable(fishPrice())
  
  vesselNumber <- reactive({
    FLTDS102030_ves <- as.numeric(input$FLTDS102030_ves)
    FLTDS60_ves <- as.numeric(input$FLTDS60_ves)
    FLTT10_ves <- as.numeric(input$FLTT10_ves)
    FLTT2060_ves <- as.numeric(input$FLTT2060_ves)
    FLTT30_ves <- as.numeric(input$FLTT30_ves)
    HK_LIG_ves <- as.numeric(input$HK_LIG_ves)
    Mixed_ves <- as.numeric(input$Mixed_ves)
    MWOT102060_ves <- as.numeric(input$MWOT102060_ves)
    MWOT30_ves <- as.numeric(input$MWOT30_ves)
    TLIG_ves <- as.numeric(input$TLIG_ves)
    TRED_ves <- as.numeric(input$TRED_ves)
    WHSDS20_ves <- as.numeric(input$WHSDS20_ves)
    WHSDS60_ves <- as.numeric(input$WHSDS60_ves)
    WHST1020_ves <- as.numeric(input$WHST1020_ves)
    
    vesselNumber <- c(FLTDS102030_ves,FLTDS60_ves,FLTT10_ves,FLTT2060_ves,FLTT30_ves,
                      HK_LIG_ves,Mixed_ves,MWOT102060_ves,MWOT30_ves,TLIG_ves,
                      TRED_ves,WHSDS20_ves,WHSDS60_ves,WHST1020_ves)
    names(vesselNumber) <- c("FLTDS102030","FLTDS60","FLTT10","FLTT2060","FLTT30",
                             "HK_LIG","Mixed","MWOT102060","MWOT30","TLIG",
                             "TRED","WHSDS20","WHSDS60","WHST1020")
    vesselNumber
  })
  
  output$vesselNumber <- renderTable(vesselNumber())
  
  shotNumber <- reactive({
    FLTDS102030_shot <- as.numeric(input$FLTDS102030_shot)
    FLTDS60_shot <- as.numeric(input$FLTDS60_shot)
    FLTT10_shot <- as.numeric(input$FLTT10_shot)
    FLTT2060_shot <- as.numeric(input$FLTT2060_shot)
    FLTT30_shot <- as.numeric(input$FLTT30_shot)
    HK_LIG_shot <- as.numeric(input$HK_LIG_shot)
    Mixed_shot <- as.numeric(input$Mixed_shot)
    MWOT102060_shot <- as.numeric(input$MWOT102060_shot)
    MWOT30_shot <- as.numeric(input$MWOT30_shot)
    TLIG_shot <- as.numeric(input$TLIG_shot)
    TRED_shot <- as.numeric(input$TRED_shot)
    WHSDS20_shot <- as.numeric(input$WHSDS20_shot)
    WHSDS60_shot <- as.numeric(input$WHSDS60_shot)
    WHST1020_shot <- as.numeric(input$WHST1020_shot)
    
    shotNumber <- c(FLTDS102030_shot,FLTDS60_shot,FLTT10_shot,FLTT2060_shot,FLTT30_shot,
                    HK_LIG_shot,Mixed_shot,MWOT102060_shot,MWOT30_shot,TLIG_shot,
                    TRED_shot,WHSDS20_shot,WHSDS60_shot,WHST1020_shot)
    names(shotNumber) <- c("FLTDS102030","FLTDS60","FLTT10","FLTT2060","FLTT30",
                           "HK_LIG","Mixed","MWOT102060","MWOT30","TLIG",
                           "TRED","WHSDS20","WHSDS60","WHST1020")
    shotNumber
  })
  
  output$shotNumber <- renderTable(shotNumber())
  
  # this table bypass the calculations if provided. For now both methods are displayed in the app
  costDay <- reactive({
    FLTDS102030_cost <- as.numeric(input$FLTDS102030_cost)
    FLTDS60_cost <- as.numeric(input$FLTDS60_cost)
    FLTT10_cost <- as.numeric(input$FLTT10_cost)
    FLTT2060_cost <- as.numeric(input$FLTT2060_cost)
    FLTT30_cost <- as.numeric(input$FLTT30_cost)
    HK_LIG_cost <- as.numeric(input$HK_LIG_cost)
    Mixed_cost <- as.numeric(input$Mixed_cost)
    MWOT102060_cost <- as.numeric(input$MWOT102060_cost)
    MWOT30_cost <- as.numeric(input$MWOT30_cost)
    TLIG_cost <- as.numeric(input$TLIG_cost)
    TRED_cost <- as.numeric(input$TRED_cost)
    WHSDS20_cost <- as.numeric(input$WHSDS20_cost)
    WHSDS60_cost <- as.numeric(input$WHSDS60_cost)
    WHST1020_cost <- as.numeric(input$WHST1020_cost)
    
    costDay <- c(FLTDS102030_cost,FLTDS60_cost,FLTT10_cost,FLTT2060_cost,FLTT30_cost,
                 HK_LIG_cost,Mixed_cost,MWOT102060_cost,MWOT30_cost,TLIG_cost,
                 TRED_cost,WHSDS20_cost,WHSDS60_cost,WHST1020_cost)
    names(costDay) <- c("FLTDS102030","FLTDS60","FLTT10","FLTT2060","FLTT30",
                        "HK_LIG","Mixed","MWOT102060","MWOT30","TLIG",
                        "TRED","WHSDS20","WHSDS60","WHST1020")
    costDay
  })
  
  output$costDay <- renderTable(costDay())
  
  
  ## Gross Operative Surplus calculations ----
  
  GOS_reactive <- eventReactive(input$runEco, {
    # the dataframe of effort combinations is filtered to keep only viable ecological values
    object_alive <- filter(loaded_object(), effortID %in% biom_list()[[3]])
    
    # catch per metier
    CMS <- computeCMS(object_alive = object_alive,
                      qflt = qflt, 
                      currentF = currentF)
    # freight cost
    cFre <- computeFRE(CMS = CMS, 
                       object_alive = object_alive, 
                       ecoParam = ecoParam())
    
    # gross value per species
    GVL_list <- computeGVL(CMS = CMS, 
                           object_alive = object_alive,
                           fishPrice = fishPrice())
    GVL <- GVL_list[[1]]
    
    # gross operative surplus
    GOS <- computeGOS(GVL = GVL, 
                      cFre = cFre, 
                      object_alive = object_alive, 
                      ecoParam = ecoParam(), 
                      shotNumber = shotNumber(),
                      vesselNumber = vesselNumber())
    
    # wages
    wage <- computeWage(GVL = GVL, 
                             ecoParam = ecoParam(), 
                             vesselNumber = vesselNumber())

    ### Output checks in the app (just using first effort combination to not overflow)
    CMS_sp <- apply(CMS[,1:9], 2, sum)
    names(CMS_sp) <- c("FLT", "GRE", "LIG", "MOW", "RED", "SQD", "TRS", "TRT", "WHS")
    CMS_df <- as.data.frame(CMS[,1:9])
    colnames(CMS_df) <- c("FLT", "GRE", "LIG", "MOW", "RED", "SQD", "TRS", "TRT", "WHS")
    CMS_df <- tibble::add_column(CMS_df, "Metier" = rownames(CMS_df), .before = "FLT")
    qflt_df <- as.data.frame(qflt)
    colnames(qflt_df) <- c("FLT", "GRE", "LIG", "MOW", "RED", "SQD", "TRS", "TRT", "WHS")
    qflt_df <- tibble::add_column(qflt_df, "Metier" = rownames(qflt_df), .before = "FLT")
    cFre_df <- ecoParam()$cFre * 1000 * rowSums(CMS) #  not working if more than one combination I think
    GVL_df <- GVL_list[[2]][1:9,]
    cVarUE_df <- ecoParam()$cVarUE * shotNumber()
    names(cVarUE_df) <- rownames(qflt_df)
    GOS <- as.data.frame(t(GOS))
    GOS$effortID <- GVL$effortID
    
    # fix to remove plot not found error at start, need to understand it a bit more
    #TODO what is going on here? Not even sure I am plotting the right thing
    output$eco_plot <- renderPlot({
      #req(GOS_thresh) # maybe
      # print(biom_list()[[1]])
      # print(GOS_thresh())
      
      plot_dat <- filter(biom_list()[[1]], effortID %in% effortID)
      
      ggplot(plot_dat)+
        geom_line(aes(x = metiers, y = effort, group = effortID), alpha = .05) +
        scale_y_continuous(name = "effort scalar") +
        theme_bw()
    })
    
    
    ## Alternate calculation, using cost per day from Sean
    
    # print("cost per Day")
    # print(costDay())
    
    totalCosts <- costDay() * vesselNumber() * ecoParam()$noDay
    # print("total costs")
    # print(totalCosts)
    # print(1.4)
    
    NVL <- GVL
    NVL[,-1] <- NVL[,-1] * (1 - ecoParam()$cShr)
    NVL[,-1] <- NVL[,-1] - t(cFre)
    
    GOS_Sean <- NVL[,-1] - totalCosts
    
    GOS_Sean <- as.data.frame(t(GOS_Sean))
    colnames(GOS_Sean) <- unique(GVL$effortID)
    
    # print(GOS)
    # print(dim(GOS))
    
    return(list(GOS,wage,CMS_df,qflt_df,GVL_df,
                GVL,NULL,NULL,object_alive, cFre_df,
                cVarUE_df,GOS_Sean))
    
    
  })
  
  ## All the tables linked to the economic threshold and appearing in "intermediate step tables" tab
  ## Most tables are formatted to look at one effort combination only such as "current effort" and will look
  ## confusing when used with the simulations from HPC
  
  output$GOS<- renderTable({
    
    res <- data.frame("Metier" = colnames(GOS_reactive()[[1]][,-dim(GOS_reactive()[[1]])[2]]), 
                      "Value" = t(GOS_reactive()[[1]][,-dim(GOS_reactive()[[1]])[2]]))
    res
    
  }, caption = "Gross operating surplus per metier. Units: $AUD per year. GOS is GVL minus proportion for the crew and fixed costs")
  
  output$GOS_Sean<- renderTable({
    
    res <- GOS_reactive()[[12]]
    res$Metier <- rownames(GOS_reactive()[[12]])
    res <- select(res, Metier, everything())
    res
    
  }, caption = "Gross operating surplus per metier using daily costs. Units: $AUD per year.")
  
  # output$GOS_ag<- renderTable({
  #   
  #   res <- data.frame("Gear type" = rownames(GOS_reactive()[[12]]), 
  #                     "Value" = (GOS_reactive()[[12]]))
  #   res
  #   
  # }, caption = "Gross operating surplus aggregated per gear type. Units: $AUD per year. GOS is GVL minus proportion for the crew and fixed costs")
  
  output$wage<- renderTable({
    res <- data.frame("Metier" = colnames(GOS_reactive()[[2]][,-dim(GOS_reactive()[[2]])[2]]),
                      "Value" = t(GOS_reactive()[[2]][,-dim(GOS_reactive()[[2]])[2]]))
    # res <- GOS_reactive()[[2]]
    # res$Metier <- rownames(GOS_reactive()[[2]])
    # res <- select(res, Metier, everything())
    res
  }, caption = "Wage per metier per crew member. Units: $AUD per year. 
  Calculated from the average number of crew member per vessel multiplied by the proportion allocated for crew.")
  
  output$CMS_table<- renderTable({
    GOS_reactive()[[3]]},
    caption = "Catch per metier per species. Determined as effort x biomass x qflt. Units: Tonnes per year",
    digits = 10)
  
  output$qflt_df<- renderTable({
    GOS_reactive()[[4]]
  }, caption = "qflt: catchability per metier and species. Units: ?", digits = 10)
  
  output$GVL_df<- renderTable({
    res <- data.frame("Metier" = colnames(GOS_reactive()[[5]][,-dim(GOS_reactive()[[5]])[2]]), 
                      "Value" = t(GOS_reactive()[[5]][,-dim(GOS_reactive()[[5]])[2]]))
    res
  }, caption = "Gross value per metier and species. Units: $AUD per year")
  
  output$GVL<- renderTable({
    res <- data.frame("Metier" = colnames(GOS_reactive()[[6]][,-1]), 
                      "Value" = t(GOS_reactive()[[6]][,-1]))
    res
  }, caption = "Gross value per metier aggregated. Units: $AUD per year")
  
  output$GVL_total<- renderTable({
    res <- sum(GOS_reactive()[[6]][,-1])
    
  }, caption = "Total value issue from fisheries. Units: $AUD per year")
  
  output$rtbs<- renderTable({
    res <- data.frame("Metier" = rownames(GOS_reactive()[[7]]), 
                      "Value" = GOS_reactive()[[7]])
    res
  }, caption = "Return to be shared (GVL - variable costs - freight costs). Units: $AUD per year. 
  Variable cost is shots per day x fuel per shot x days at sea x number of vessels x effort scalar")
  
  output$wage_FTE<- renderTable({
    GOS_reactive()[[8]]
  }, caption = "wage per crew member")
  
  output$sp_alive<- renderTable({
    res <- data.frame("Species" = GOS_reactive()[[9]]$sp,  "Biomass" = GOS_reactive()[[9]]$biomass)
  }, caption = "Species surviving the biomass threshold and their biomass available to be fished. Units: tonnes")
  
  output$varUE_df<- renderTable({
    res <- data.frame("Metier" = names(GOS_reactive()[[11]]), 
                      "Value" = GOS_reactive()[[11]])
    res
  }, caption = "Variable costs per metier. Units: $AUD per year")
  
  output$fre_df<- renderTable({
    res <- data.frame("Metier" = names(GOS_reactive()[[10]]), 
                      "Value" = GOS_reactive()[[10]])
    res
  }, caption = "Freight costs per metier. Units: $AUD per year")
  
  ## economic threshold ----
  
  # Now that we calculated the GOS, let's check what makes a profit
  GOS_thresh <- reactive({ # observeEvent(GOS_reactive(),{#
    # req(GOS_reactive()) # seems that this req doesn't work (internet examples show with input$, not shiny generated object)
    # need one though otherwise getting errors graphics
    # print(GOS_reactive()[[1]])
    # print(10)
    GOS_thresh <- filter(GOS_reactive()[[1]], FLTDanishSeiene102030 >= input$eco_thresh, FLTDanishSeiene60 >= input$eco_thresh,
                         FLTTrawl10 >= input$eco_thresh, FLTTrawl2060  >= input$eco_thresh, FLTTrawl30  >= input$eco_thresh,
                         HK_LIG  >= input$eco_thresh, Mixed  >= input$eco_thresh, MWOTrawl102060 >= input$eco_thresh,
                         MWOTrawl30  >= input$eco_thresh, TrawlLIG >= input$eco_thresh, TrawlRED >= input$eco_thresh,
                         WHSDanishSeine20 >= input$eco_thresh, WHSDanishSeine60 >= input$eco_thresh, 
                         WHSTrawl1020  >= input$eco_thresh)
    
    # GOS_thresh <-  GOS_reactive()[(GOS_reactive() >= input$eco_thresh).all(axis=1)]
    
    # GOS_thresh <- filter_all(GOS_reactive, all_vars(.>= input$eco_thresh))
    # print(GOS_thresh)
    
    GOS_thresh
    
  })
  
  output$GOS_threshold <- renderTable({
    GOS_thresh()
  }, caption = "Gross operative surplus per fleet per year in aud")
  
  
  
  ## Printing viable effort combinations ----
  
  output$final_effort <- renderTable({
    # print(GOS_thresh()$effortID)
    # print(11)
    final_effort <- filter(loaded_object(), effortID %in% GOS_thresh()$effortID)
    final_effort <- final_effort[,4:17]
    final_effort <- final_effort[!duplicated(final_effort),]
    
  })
  
  # wage section --------
  
  wage_thresh <- reactive({ # observeEvent(GOS_reactive(),{#
    # req(GOS_reactive()) # seems that this req doesn't work (internet examples show with input$, not shiny generated object)
    # need one though otherwise getting errors graphics
    
    wage_thresh <- filter(GOS_reactive()[[2]], FLTDanishSeiene102030 >= input$humie_thresh, FLTDanishSeiene60 >= input$humie_thresh,
                          FLTTrawl10 >= input$humie_thresh, FLTTrawl2060  >= input$humie_thresh, FLTTrawl30  >= input$humie_thresh,
                          HK_LIG  >= input$humie_thresh, Mixed  >= input$humie_thresh, MWOTrawl102060 >= input$humie_thresh,
                          MWOTrawl30  >= input$humie_thresh, TrawlLIG >= input$humie_thresh, TrawlRED >= input$humie_thresh,
                          WHSDanishSeine20 >= input$humie_thresh, WHSDanishSeine60 >= input$humie_thresh, 
                          WHSTrawl1020  >= input$humie_thresh, effortID %in% GOS_thresh()$effortID)
    
    # GOS_thresh <-  GOS_reactive()[(GOS_reactive() >= input$eco_thresh).all(axis=1)]
    
    # GOS_thresh <- filter_all(GOS_reactive, all_vars(.>= input$eco_thresh))
    # print(GOS_thresh)
    
    wage_thresh
    
  })
  
  output$wage_threshold <- renderTable({
    wage_thresh()
  }, caption = "wage per crew per fleet per year in aud")
  
  
  # FTE_dat <- data.frame("fleet" = c("DS","TW"), "crew_size" = c(3.5,4))
  # FTE_adjusted <- c(rep(FTE_dat$crew_size[1],2),rep(FTE_dat$crew_size[2],3))
  
  
}

shinyApp(ui, server)