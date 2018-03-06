rm(list=ls())

library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(mapproj)
library(fiftystater)
library(shinythemes)
library(BAMMtools)

#### to do
## add 2016 pop data for map denominators
## add rate adjustment to plots, maps
## add ncands
## format for load Rdata rather than reading csv, quicker startup


dat<-read_csv("afcars-long-round.csv")

dat$RaceEthn<-factor(dat$RaceEthn, 
                     levels = c("Total",
                                "African American, Non-Hispanic",
                                "American Indian or Alaska Native, Non-Hispanic",
                                "Asian, Non-Hispanic",
                                "Hispanic (Any Race)",
                                "Native Hawaiian or Other Pacific Islander, Non-Hispanic",
                                "White, Non-Hispanic",
                                "Two or More Races, Non-Hispanic",
                                "Race/Ethnicity Unknown"))

data(state)
st<-data.frame(stname=state.abb, 
               state=state.name)
st$stname<-as.character(st$stname); st$state<-as.character(st$state)

dat<-dat%>%
  rename(stname=state)%>%
  left_join(st)

pop<-read_csv("seer-pop.csv")

pop<-pop%>%
  rename(stname=state)

pop<-pop%>%
  left_join(st)

pop$race<-recode(pop$race,
             "Total" = "Total",
             "African American" = "African American, Non-Hispanic",
             "American Indian/Alaska Native" = "American Indian or Alaska Native, Non-Hispanic",
             "Asian/Pacific Islander" = "Asian, Non-Hispanic",
             "Latino/a" = "Hispanic (Any Race)",
             "White" = "White, Non-Hispanic")

pop<-pop%>%
  rename(RACE=race)

state_pop<-pop%>%
  group_by(state, stname, RACE, year)%>%
  summarise(pop = sum(pop))%>%
  rename(fy = year)%>%
  filter(!(is.na(state)))

race.labels<-levels(dat$RaceEthn)
i<-which(race.labels%in%c("Total"))

dat<-dat%>%
  rename(RACE=RaceEthn)
################## CHECK ON THIS - MAYBE FLIPPING DRUG AB PARENT DRUG AB CHILD
rem.labels<-list("All removals",
                 "Alcohol abuse: child"="AACHILD",
                 "Alcohol abuse: parent"="AAPARENT",
                 "Abandonment"="ABANDMNT",
                 "Child behavior problem"="CHBEHPRB",
                 "Child disability"="CHILDIS",
                 "Drug abuse: child"="DACHILD",
                 "Drug abuse: parent"="DAPARENT",
                 "Inadequate housing"="HOUSING",
                 "Neglect"="NEGLECT",
                 "Caretaker inability to cope"="NOCOPE",
                 "Physical abuse"="PHYABUSE",
                 "Parent death"="PRTSDIED",
                 "Parent incarcerated"="PRTSJAIL",
                 "Relinquishment"="RELINQSH",
                 "Sexual abuse"="SEXABUSE")

dat<-dat%>%
  filter(!(is.na(state)))

dat$state<-factor(as.character(dat$state))

# curplset.labels<-c("Pre-adoptive home", 
#                    "Relative foster home", 
#                    "Non-relative foster home", 
#                    "Group home", 
#                    "Institution",
#                    "Supervised independent living",
#                    "Runaway",
#                    "Trial home visit")
# 
# curplset.codes<-function(x){
#   code.out<-ifelse(x=="Pre-adoptive home", "1",
#                    ifelse(x=="Relative foster home", "2",
#                           ifelse(x=="Non-relative foster home", "3",
#                                  ifelse(x=="Group home", "4",
#                                         ifelse(x=="Institution", "5", 
#                                                ifelse(x=="Supervised independent living", "6",
#                                                       ifelse(x=="Runaway", "7", 
#                                                              ifelse(x=="Trial home visit", "8", "NULL"))))))))
#   return(code.out)
# }

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("flatly"),
  
  #TITLE
  titlePanel("Child Welfare Data Portal"),
  
  #INPUT
  sidebarLayout(
      sidebarPanel(
        selectInput("type",
                       "Visual type",
                       c(Plot = "tsplot",
                         Table = "table",
                         Map = "map")),

    ############################################
    #### TSPLOT    
    ############################################
    conditionalPanel(
      condition="input.type == 'tsplot'",
      selectInput("state",
                  "Place",
                    c("US total" = "US",
                      unique(as.character(dat$state)))),
      selectInput("cl_type",
                  "Caseload type",
                  c("Foster care entries",
                  "Total served"="Served",
                  "Exits")),
      selectInput("rem_reason",
                  "Removal reason",
                  rem.labels),
      selectInput("race",
                  "Child race/ethnicity",
                  c(race.labels)),
      selectInput("rate",
                  "Output type",
                  c("Count of cases" = "count",
                    "Rate per 1,000 child population" = "rate")),
      downloadButton('downloadData_plot', 'Download data')
      ),
    
    ############################################
    #### TABLE    
    ############################################
    
    conditionalPanel(
      condition="input.type == 'table'",
      selectInput("fy1",
                  "Year",
                  c("All years" = "All",
                    unique(as.character(dat$fy)))),
      selectInput("state1",
                  "State",
                  c("US total" = "US",
                    "All states" = "All",
                    unique(as.character(dat$state)))),
      selectInput("cl_type1",
                  "Caseload type",
                  c("Foster care entries",
                  "Total served"="Served",
                  "Exits")),
      selectInput("rem_reason1",
                  "Removal reason",
                  rem.labels),
      selectInput("race1",
                  "Child race/ethnicity",
                  c(race.labels)),
      # selectInput("rate1",
      #             "Output type",
      #             c("Count of cases" = "count",
      #               "Rate per 1,000 child population" = "rate")),
      downloadButton('downloadData_table', 'Download data')
      ),
    
    ############################################
    #### MAP    
    ############################################
    
    conditionalPanel(
      condition="input.type == 'map'",
      selectInput("fy2",
                  "Year",
                  c(unique(as.character(dat$fy)))),
      selectInput("cl_type2",
                  "Caseload type",
                  c("Foster care entries",
                  "Total served"="Served",
                  "Exits")),
      selectInput("rem_reason2",
                  "Removal reason",
                  rem.labels),
      selectInput("race2",
                  "Child race/ethnicity",
                  c(race.labels)),
      downloadButton('downloadData_map', 'Download data')
      )
  ),
  
  #OUTPUT
  mainPanel(
      conditionalPanel(
        condition = "input.type == 'table'",
      DT::dataTableOutput("table")),
      
      conditionalPanel(
        condition = "input.type == 'tsplot'",
      plotOutput("tsplot")),
      
      conditionalPanel(
        condition = "input.type == 'map'",
      plotOutput("map")
      )
    )
  ),
  hr(),
  print("This is a prototype developed by the National Data Archive on Child Abuse and Neglect using data from the Adoption and Foster Care Data Analysis and Reporting System. 
        Figures may have errors and are presented for demonstration purposes only. 
        Data are subject to random rounding and may not identically match other published figures.
        Do not interpret for research purposes. 
        For best results, please use Firefox or Chrome. 
        All questions/comments can be directed to fedwards@cornell.edu")
  
)


######################################################################################################
## SERVER
######################################################################################################

server <- function(input, output){

############################################################################################
# Make temp data files based on user selections
############################################################################################  
  
  ##############################################
  ###################### TSPLOT
  
  newDat_plot<-reactive({ #### rewrite for reactive data for download
        temp<-dat
        
        pop_temp<-state_pop
        if(input$race!="Total"){
          temp<-temp%>%filter(RACE==input$race)
          pop_temp<-pop_temp%>%filter(RACE==input$race)
        }
        if(input$race=="Total"){
          temp<-temp%>%filter(RACE=="Total")
          pop_temp<-pop_temp%>%filter(RACE=="Total")
          
        }
        
        if(input$rem_reason!="All removals"){
          index<-which(names(dat)==input$rem_reason)
          temp$Placements<-temp[, index]
        } 
        if(input$state!="US"){
          temp<-temp%>%
            dplyr::filter(state==input$state)%>%
            dplyr::filter(cl_type==input$cl_type)%>%
            group_by(fy, state, cl_type)%>%
            select(Placements)%>%
            summarise(Placements=sum(Placements, na.rm=TRUE))%>%
            ungroup()
          
          pop_temp<-pop_temp%>%
            dplyr::filter(state == input$state)%>%
            group_by(fy, state)%>%
            summarise(pop = sum(pop, na.rm=TRUE))%>%
            ungroup()
          } 
        if(input$state=="US"){
          temp<-temp%>%
            dplyr::filter(cl_type==input$cl_type)%>%
            group_by(fy, cl_type)%>%
            select(Placements)%>%
            summarise(Placements=sum(Placements, na.rm=TRUE))%>%
            ungroup()
          
          pop_temp<-pop_temp%>%
            group_by(fy)%>%
            summarise(pop = sum(pop, na.rm=TRUE))
        }
        if(input$rate == "rate"){
          temp<-left_join(temp, 
                          pop_temp)%>%
            mutate(Placements = 1000 * Placements / pop,
                   Type = "Rate per 1,000 child population")
        } else{temp$Type = "Count of cases"}
    return(temp)
  })

  ##############################################
  ###################### TABLE
  newDat_table<-reactive({
      temp<-dat
      pop_temp<-state_pop
      
      temp<-temp%>%
        filter(cl_type==input$cl_type1)
      
      if(input$rem_reason1!="All removals"){
        index<-which(names(dat)==input$rem_reason1)
        temp$Placements<-temp[, index]
      }
      if(input$race1!="Total"){
        temp<-temp%>%filter(RACE==input$race1)
        pop_temp<-pop_temp%>%filter(RACE==input$race1)
      }
      if(input$race1=="Total"){
        temp<-temp%>%filter(RACE=="Total")
        pop_temp<-pop_temp%>%filter(RACE=="Total")
      }
      if(input$fy1!="All"){
        temp<- temp%>%
          dplyr::filter(fy==as.numeric(input$fy1))
        pop_temp<- pop_temp%>%
          dplyr::filter(fy==as.numeric(input$fy1))
      }
      if(input$state1!="All" & input$state1!="US"){
        temp<- temp%>%
          dplyr::filter(state==input$state1)
        pop_temp<- pop_temp%>%
          dplyr::filter(state==input$state1)
      }
      if(input$state1!="US"){
        temp<-temp%>%
          group_by(fy, state, cl_type)%>%
          select(Placements)%>%
          summarise(Placements=sum(Placements, na.rm=TRUE))%>%
          select(-cl_type)%>%
          rename(Year=fy, State=state)%>%
          ungroup()
        pop_temp<-pop_temp%>%
          group_by(fy, state)%>%
          summarise(pop = sum(pop, na.rm=TRUE))%>%
          rename(Year=fy, State=state)%>%
          ungroup()
      } else{
        temp<-temp%>%
          group_by(fy, cl_type)%>%
          select(Placements)%>%
          summarise(Placements=sum(Placements, na.rm=TRUE))%>%
          select(-cl_type)%>%
          rename(Year=fy)%>%
          ungroup()
        pop_temp<-pop_temp%>%
          group_by(fy)%>%
          summarise(pop = sum(pop, na.rm = TRUE))%>%
          rename(Year=fy)%>%
          ungroup()
      }
      temp<-left_join(temp, 
                        pop_temp)%>%
        mutate('Rate per 1,000 child population' = round(1000 * Placements / pop,2))%>%
        rename('Count of cases' = Placements)%>%
        select(-pop)
      
      return(temp)
  })
    
  ##############################################
  ###################### MAP
    
   newDat_map<-reactive({
     
     temp<-dat
     
     if(input$race2!="Total"){
       temp<-temp%>%filter(RACE==input$race2)
       # racelabel<-input$race2
       # poplab<-input$race2
     } 
     
     if(input$rem_reason2!="All removals"){
       index<-which(names(dat)==input$rem_reason2)
       temp$Placements<-temp[, index]}
     
     if(input$race2=="Total"){
       temp<-temp%>%filter(RACE=="Total")
       # poplab<-input$race2
     }
     
     temp<-temp%>%
       filter(fy==input$fy2)%>%
       filter(cl_type==input$cl_type2)%>%
       group_by(state)%>%
       summarise(Placements=sum(Placements, na.rm=TRUE))%>%
       left_join(pop%>%
                   filter(year==input$fy2, RACE==input$race2)%>%
                   group_by(state, stname)%>%
                   summarise(pop=sum(pop)))%>%
       mutate(rate=Placements/pop * 1000)%>%
       filter(!(is.na(state)))
     
     rate.jenks<-assignColorBreaks(temp$rate, NCOLORS=6, method="jenks")
     temp<-temp%>%
       mutate(rate.jenks = ifelse(rate<=rate.jenks[1],1,
                                  ifelse(rate<=rate.jenks[2],2,
                                         ifelse(rate<=rate.jenks[3],3,
                                                ifelse(rate<=rate.jenks[4],4,
                                                       ifelse(rate<=rate.jenks[5],5,
                                                              ifelse(rate<=rate.jenks[6],6,
                                                                     7)))))))
      
     rate.jenks<-round(rate.jenks,2)
     if(length(unique(rate.jenks))==7){
       temp$rate.jenks<-ordered(temp$rate.jenks, levels=unique(temp$rate.jenks)[order(unique(temp$rate.jenks))],
                                labels=c(paste("0-",rate.jenks[1], sep=""),
                                         paste(rate.jenks[1],"-",rate.jenks[2],sep=""),
                                         paste(rate.jenks[2],"-",rate.jenks[3],sep=""),
                                         paste(rate.jenks[3],"-",rate.jenks[4],sep=""),
                                         paste(rate.jenks[4],"-",rate.jenks[5], sep=""),
                                         paste(rate.jenks[5],"-",rate.jenks[6],sep=""),
                                         paste(rate.jenks[6], "-", sep="")
                                ))} else{
       temp$rate.jenks<-"Insufficient Data"
     }
       temp$state<-tolower(temp$state)
       return(temp)
   }) 


  
  #################################################################################
  ## TS PLOTS, var
  #################################################################################
  
  output$tsplot<-renderPlot({
    temp<-newDat_plot()
    
    p<-ggplot(temp) + 
      aes(x=fy, y=Placements)+
      geom_line()+
      ylab(temp$Type[1]) + 
      xlab("Year") + 
      scale_x_continuous(breaks=unique(temp$fy))+
      # ggtitle(paste(input$cl_type, " in ", 
      #               placelab, ", ", 
      #               #tolower(input$curplset), ", ", 
      #               racelabel, 
      #               sep=""))+
      theme(text=element_text(size=14))
    
    p
      
  })
  

  #################################################################################
  ## TABLE, var1
  #################################################################################
  
  output$table <- DT::renderDataTable(DT::datatable({
    
    temp<-newDat_table()
    temp%>%
      arrange(desc(Year))
    
  }, options=list(lengthChange=TRUE, pageLength=15), rownames = FALSE))
  
  #################################################################################
  ## MAP, var2
  #################################################################################
  
  output$map <- renderPlot({
    
    temp<-newDat_map()

    if("Insufficient Data"%in%temp$rate.jenks){
      df <- data.frame()
      ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
      p<-ggplot(df) + 
        geom_blank() + 
        ggtitle("Insufficient data to generate map, please select different variables")
    } else{
    # map_id creates the aesthetic mapping to the state name column in your dat
    p <- ggplot(temp, aes(map_id = state)) +
      # map points to the fifty_states shape dat
      geom_map(aes(fill = rate.jenks), map = fifty_states,
               color = "black") +
      expand_limits(x = fifty_states$long, y = fifty_states$lat) +
      coord_map() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(x = "", y = "") +
      theme(legend.position = "right",
            panel.background = element_blank()) +
      scale_fill_brewer(type="seq", palette = "OrRd", name="Rate per 1,000\nchild population") + 
      coord_map("albers", lat0 = 39, lat1 = 45)
    }
    p
})
    
    
 #### downloads

    output$downloadData_plot <- downloadHandler(
      filename = function(){
        paste('ndacan-portal-', 
              Sys.Date(), 
              '.csv', 
              sep='')
      },
      content = function(file) {
        temp_out<-newDat_plot()
        temp_out<-temp_out%>%
          rename(foster_care_pop=Placements,
                 year=fy)
        temp_out$state<-input$state
        temp_out$race_ethn<-input$race
        temp_out$foster_care_type<-input$cl_type
        temp_out$removal_reason<-input$rem_reason
        
        temp_out<-temp_out[c("state",
                             "year",
                             "race_ethn",
                             "removal_reason",
                             "foster_care_type",
                             "foster_care_pop")]
        
        write.csv(temp_out, 
                  file,
                  row.names=FALSE)
      })
    
    output$downloadData_table <- downloadHandler(
      filename = function(){
        paste('ndacan-portal-', 
              Sys.Date(), 
              '.csv', 
              sep='')
      },
      content = function(file) {
        temp_out<-newDat_table()
        temp_out<-temp_out%>%
          rename(foster_care_pop=Placements,
                 year=Year)
        if(input$state1=="US"){
        temp_out$state<-input$state1}else{
          temp_out$state<-temp_out$State
        }
        
        temp_out$race_ethn<-input$race1
        temp_out$foster_care_type<-input$cl_type1
        temp_out$removal_reason<-input$rem_reason1

        temp_out<-temp_out[c("state",
                             "year",
                             "race_ethn",
                             "removal_reason",
                             "foster_care_type",
                             "foster_care_pop")]
        write.csv(temp_out, 
                  file,
                  row.names=FALSE)
      })
    
    output$downloadData_map <- downloadHandler(
      filename = function(){
        paste('ndacan-portal-', 
              Sys.Date(), 
              '.csv', 
              sep='')
      },
      content = function(file) {
        temp_out<-newDat_map()
        print(temp)
        temp_out<-temp_out%>%
          select(-state)%>%
          rename(foster_care_pop=Placements,
                 state=stname,
                 child_pop=pop,
                 foster_care_per_thousand=rate)
        temp_out$race_ethn<-input$race2
        temp_out$foster_care_type<-input$cl_type2
        temp_out$year<-input$fy2
        temp_out$removal_reason<-input$rem_reason2
        
        temp_out<-temp_out%>%
          select(-rate.jenks)
        temp_out<-temp_out[c("state", 
                 "year", 
                 "race_ethn",
                 "removal_reason",
                 "foster_care_type", 
                 "foster_care_pop",
                 "foster_care_per_thousand",
                 "child_pop")]
        
        write.csv(temp_out, 
                  file,
                  row.names=FALSE)
      })
     
    
      
    

}

# Run the application 
shinyApp(ui = ui, server = server)

