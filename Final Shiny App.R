library(shiny)
library(fable)
library(fabletools)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(colourpicker)
library(regclass)
library(ggeasy)
#install.packages("shinydashboard")
#install.packages("shinydashboardPlus")
library(fpp3)
data(souvenirs)
tsibble(souvenirs)
#?souvenirs

Plot1 = souvenirs %>%
  gg_season(Sales, labels = "both") +
  labs(y = "Total Souvenir Sales (millions)",
       title = "Seasonal plot: Souvenir sales")+theme_update()+easy_center_title()
Plot2 =  souvenirs %>%
  ACF(Sales) %>%
  autoplot() + labs(title="Autocorrelation: Souvenir Sales", x= "1 Month Lag")+theme_update()+easy_center_title()
Plot3 =  souvenirs %>%
  model(
    classical_decomposition(Sales, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposition: Total Sales of Souvenirs")+theme_update()+easy_center_title()
#######################
Plot5 =  souvenirs %>%
  model(
    classical_decomposition(Sales, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposition Date: Total Sales of Souvenirs")+theme_update()+easy_center_title()
##############################
Souvenir_fc <- souvenirs %>%
  select(Sales) %>%
  model(
    LinearModel = TSLM(Sales~trend()),
    Naive= NAIVE(Sales),
    SNaive = SNAIVE(Sales),
    Arima = ARIMA(Sales),
    Drift = RW(Sales ~ drift()),
    Mean = MEAN(Sales),
    SES = ETS(Sales ~ error("A") + trend("N") + season("A")),
  )
########## Makes the graph (plot 9)
MODELNAMES<- c("LinearModel","Naive","SNaive","Drift","Mean","SES")
library(shiny)
Souvenir_fc %>%
  forecast(h=12)%>%
  autoplot(souvenirs)+theme_update()+labs(title= "12 Month Souvenir Sales Forecast",y= "Souvenir Sales")+theme_update()+easy_center_title()
#######################################
MODELNAMES2<- c("Holt","HoltWinters")
HOLT<-souvenirs %>%
  select(Sales) %>%
  model(
    Holt = ETS(Sales ~ error("A") + trend("A") + season("N")),
    HoltWinters = ETS(Sales ~ error("A") + trend("A") + season("A")),
  )

HOLT %>%
  forecast(h=12)%>%
  autoplot(souvenirs)+theme_update()+labs(title= "12 Month Souvenir Sales Forecast",y= "Souvenir Sales")+theme_update()+easy_center_title()
##################################
MODELNAMES3<-c("Fourier_K_1","Fourier_K_2","Fourier_K_3","Fourier_K_4","Fourier_K_5","Fourier_K_6","Default")
Fourier<- souvenirs %>%
  select(Sales) %>%
  model(Fourier_K_1 = ARIMA(log(Sales)~ fourier(K=1)+PDQ(0,0,0)),
        Fourier_K_2 = ARIMA(log(Sales)~ fourier(K=2)+PDQ(0,0,0)),
        Fourier_K_3 = ARIMA(log(Sales)~ fourier(K=3)+PDQ(0,0,0)),
        Fourier_K_4 = ARIMA(log(Sales)~ fourier(K=4)+PDQ(0,0,0)),
        Fourier_K_5 = ARIMA(log(Sales)~ fourier(K=5)+PDQ(0,0,0)),
        Fourier_K_6 = ARIMA(log(Sales)~ fourier(K=6)+PDQ(0,0,0)),
        Default = ARIMA(log10(Sales)~  pdq(1,1,2) + PDQ(0,1,1)))

Fourier %>%
  forecast(h=12)%>%
  autoplot(souvenirs)+theme_update()+labs(title= "12 Month Souvenir Sales Forecast",y= "Souvenir Sales")+theme_update()+easy_center_title()


FORECAST<- forecast(Souvenir_fc)
ui <- dashboardPage(
  dashboardHeader(title = "Final Project"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", tabName = "in"),
      menuItem("Full Time Series", tabName = "sy"),
      menuItem("Plot Choices", tabName = "his"),
      menuItem("Date Range Decomposition", tabName = "date"),
      menuItem("Interpretation", tabName = "comp"),
      menuItem("Simple Models", tabName = "ten"),
      menuItem("Exponential Smoothing", tabName = "smooth"),
      menuItem("ARIMA", tabName = "Arima")
    )
  ),
  controlbar=dashboardControlbar(collapsed=TRUE, skinSelector()),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "in", 
              includeMarkdown("Personal Project.rmd")
      ),
      
      # Second tab content
      tabItem(tabName = "sy",
              span(tags$h1("Full Time Series: Souvenir Sales"),style= "color:purple"),
              plotOutput("Trend")),
  
      # Third tab content
      tabItem(tabName = "his",
              span(tags$h1("Choose Your Plot: Souvenir Sales"),style= "color:purple"),
              span(textOutput("text2"), style="color:purple"),
              sidebarLayout(sidebarPanel(
                selectInput("Plot", "Please select a plot to view:",
                            choices = c("Seasonality", "Autocorrelation","Decompisition")),
                actionButton("submit", "Submit")),
                plotOutput(outputId = "Plots")
              )),
      # Fourth tab content
      tabItem(tabName = "ten",
              span(tags$h1("Forecasts"),style= "color:purple"),
              prettyCheckboxGroup(
                inputId = "Id032",
                label = "Choose Simple Forecasting Model:", 
                choices = MODELNAMES
              ), 
              sliderInput("ForecastMonths2",
                             label = "Select Forecast Time (Months):",
                             min=1,
                             max = 48,
                             value = 12),
              plotOutput("Plot9")
      ),
      
      #Fifth Tab Content
      tabItem(tabName = "comp",
              includeMarkdown("Interpretations.rmd")
      ),
      #Sixth Tab Content
      tabItem(tabName = "date",
              span(tags$h1("Date Decomposition: Souvenir Sales"),style= "color:purple"),
              span(textOutput("text6"), style="color:purple"),
              plotOutput("plot5"),
              numericInput("Year",
                           label = "Input Starting Year",
                           value = 1989),
              numericInput("Year2",
                           label = "Input Ending Year",
                           value = 1993)
      ),
      ####### Tab 7
      tabItem(tabName = "smooth",
      span(tags$h1("Exponential Smoothing "),style= "color:purple"),
      prettyCheckboxGroup(
        inputId = "Id033",
        label = "Choose Exponential Smoothing Model:", 
        choices = MODELNAMES2
      ),
      sliderInput("ForecastMonths",
                  label = "Select Forecast Time (Months):",
                  min=1,
                  max = 48,
                  value = 12),
      plotOutput("Plot10")),
      
      ######## Tab 8
      tabItem(tabName = "Arima",
              span(tags$h1("Exponential Smoothing "),style= "color:purple"),
              pickerInput(
                inputId = "Id086",
                label = "Placeholder", 
                choices = MODELNAMES3),
              sliderInput("ForecastMonths3",
                          label = "Select Forecast Time (Months):",
                          min=1,
                          max = 48,
                          value = 12),
              plotOutput("Plot11")
              )
    ) 
  )
)
server <- function(input, output) {
  #############################TAB 2
  output$Trend <- renderPlot({
    souvenirs %>%
      autoplot(Sales)+
      labs(title = "Full Time Series: Total Sales of Souvenirs",
           x= "Month", y= "Souvenir Sales")+theme_update()+easy_center_title()
  })
  ########################### TAB 3 PLOTS
  output$text2 <- renderText({ paste("Instructions: Select the Plot from the drop down then click the 'Submit Button' for the plot to appear")})
  observeEvent(input$submit,{
    output$Plots = renderPlot({
      switch(isolate(input$Plot),
             "Seasonality" = Plot1,
             "Autocorrelation" = Plot2,
             "Decompisition" = Plot3)
    })
  })
    
  ########################################## Tab 4
  output$Plot9<-renderPlot({
    Souvenir_fc %>%
      forecast(h=input$ForecastMonths2)%>%
      filter(.model %in% input$Id032)%>%
      autoplot(souvenirs)+theme_update()+labs(title= "12 Month Souvenir Sales Forecast",y= "Souvenir Sales")+theme_update()+easy_center_title()
    
  })  
 
  ######################################## Tab 6
  output$text6 <- renderText({ paste("***Collected Data Years Begin 1988 Ends 1993***")})
  output$plot5<-renderPlot({
    souvenirs %>%
      filter(year(Month)>= input$Year) %>%
      filter(year(Month)<= input$Year2) %>%
      model(stl=STL(Sales))%>%
      components()%>%
      autoplot()
  })
  ######################################
  output$Plot10<-renderPlot({
    HOLT%>%
      forecast(h=input$ForecastMonths)%>%
      filter(.model %in% input$Id033)%>%
      autoplot(souvenirs)+theme_update()+labs(title= "12 Month Souvenir Sales Forecast",y= "Souvenir Sales")+theme_update()+easy_center_title()
    
  }) 
  #######################################
  output$Plot11<-renderPlot({
    Fourier %>%
      forecast(h=input$ForecastMonths3)%>%
      filter(.model %in% input$Id086)%>%
      autoplot(souvenirs)+theme_update()+labs(title= "12 Month Souvenir Sales Forecast",y= "Souvenir Sales")+theme_update()+easy_center_title()
    
  }) 
}

shinyApp(ui = ui, server = server)
