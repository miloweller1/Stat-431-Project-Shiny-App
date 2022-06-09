#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(devtools)
library(spotifyr)
library(shinydashboard)
library(data.table)
library(shinyWidgets)
library(spotapi)
library(ggplot2)
library(ggrepel)
library(plotly)
library(viridis)
library(hrbrthemes)
library(dashboardthemes)
library(shinyalert)
library(tidyverse)
library(readr)

data <- read_csv("artist-uris.csv.csv", col_names=c("Artist", "Info"), show_col_types = F)
data<-data$Artist



header <- dashboardHeader(



  title = shinyDashboardLogo(
    theme = "purple_gradient",
    boldText = "Artist Info",
    mainText = "",
    badgeText = "v1"))

sidebar <- dashboardSidebar(
  # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
  # label = "Search Artists", icon = shiny::icon("search"))


  fluidPage(
    title = "Search Bar",
    fluidRow(

      pickerInput(
        inputId = "searchme",
        label = "Search Artist",
        choices = data,
        selected="Pitbull",
        multiple = FALSE,
        options = pickerOptions(title = "Search Artist",
                                `live-search` = TRUE)

        #buttonId = "searchButton", icon = shiny::icon("search"),

      ))))
options(shiny.sanitize.errors = FALSE)




body <- dashboardBody( fluidPage(
  shinyDashboardThemes(theme = "purple_gradient"),
  verbatimTextOutput( "res", placeholder=F),
  box(tableOutput("similar"), title="Related Artists"),
  box(tableOutput("songs"), title="Top Songs"),
  box(plotOutput("plot"),title="Song Duration Vs Popularity For Top 10 Songs"),
  box(plotlyOutput("plot2"), title="Charting Songs Overtime"),
  box(plotlyOutput("plot3"), title="Artist's Average Audio Composition")
))

ui <- dashboardPage(title = 'Search', header, sidebar, body)



server <- function(input, output, session) {

  shinyalert(
    title = "Welcome to the spotapi shiny app. In the upper left, search for an artist of your choice to learn more about them.",
    text = "Allow a few seconds for your input to load. Some artists are not available.",
    size = "l",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )

  updatePickerInput(
    session,
    "searchme",
    selected = "Pitbull")


  #selected artist name print in console. REMOVE LATER
  observe({
    print(input$searchme)
  })


  #artist name selected
  output$res<- renderText({
    input$searchme
  })


  #top artists output table
  output$similar<- renderTable({

    t <- try(related_artists(input$searchme), silent=T)
    if("try-error" %in% class(t)){
      noSimilar<-as.character("no similar")
      results<-as.data.frame(noSimilar)
      colnames(results) <- "Similar Artists Not Available, Please Select A Different Artist"
    }
    else{
      related_artists(input$searchme)
    }
  })




  #top songs output table
  output$songs<- renderTable({

    tt <- try(top_songs(input$searchme), silent=T)
    if("try-error" %in% class(tt)){
      noSimilar<-as.character("no similar")
      results<-as.data.frame(noSimilar)
      colnames(results) <- "Top Songs Not Available, Please Select A Different Artist"
    }
    else{
      top_songs(input$searchme)
    }
  })


  #plot top 10 songs pop vs duration

  output$plot <- renderPlot({

    ttt <- try(top_songs(input$searchme), silent=T)
    if("try-error" %in% class(ttt)){
      noSimilar<-as.character("no similar")
      results<-as.data.frame(noSimilar)
      colnames(results) <- "Top Songs Not Available, Please Select A Different Artist"
    }
    else{
      ggplot(top_songs(input$searchme), aes(x = top_songs(input$searchme)$Duration, y = top_songs(input$searchme)$Popularity))+ geom_point(color = "#00AFBB", size = 4)+xlab("Song Duration (Seconds)")+ylab("Popularity Index Score")+
        geom_label_repel(aes(label = Song), size = 3)+
        scale_color_manual("#00AFBB")+theme_classic()+theme(plot.background = element_rect(fill = "darkslategray3"), panel.background = element_rect(fill = "darkslategray3"))
    }
  })


  output$plot2<- renderPlotly({
    tester<-artist_charts(input$searchme)
    validate(
      need(tester, 'No data available, please select a new artist')
    )

    pplotly<-artist_charts(input$searchme)
    pplotly<-pplotly%>%select(date, rank, date, song, artist,"last-week", "peak-rank", "weeks-on-board" )
    colnames(pplotly)<- c("date", "rank", "song","artist", "last_week","peak_rank","weeks_on_board")

    pplotly<-pplotly%>%mutate(date=as.Date(date), rank=as.numeric(rank), last_week=as.numeric(last_week), peak_rank=as.numeric(peak_rank), weeks_on_board=as.numeric(weeks_on_board))

    pplotly<-pplotly%>%mutate(text = paste("Song: ", song, "\nRank: ", rank, "\nArtist: ", artist, "\nPeak Rank: ", peak_rank, "\nWeeks on Board ", weeks_on_board, sep=""))


    plot2<-ggplot(pplotly, aes(x=date, y=rank, size=-peak_rank, color=song, text=text)) +scale_y_reverse()+geom_point(alpha=0.7)+xlab("Date")+ylab("Position on Top 100 Chart (US)")+
      scale_color_viridis(discrete=T, guide=T) +
      theme_ipsum() +
      theme(legend.position="none", plot.background = element_rect(fill = "darkslategray3"), panel.background = element_rect(fill = "darkslategray3"))

    ggplotly(plot2, tooltip="text")

  })

  output$plot3<- renderPlotly({

    t4 <- try(top_songs(input$searchme), silent=T)
    if("try-error" %in% class(t4)){
      noSimilar<-as.character("no similar")
      results<-as.data.frame(noSimilar)
      colnames(results) <- "Top Songs Not Available, Please Select A Different Artist"
    }
    else{

      audio<-artist_audio_features(input$searchme)

      audio<-audio%>%select(danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, mode)
      audio<-audio%>%mutate(Danceability=danceability*100, Energy=energy*100, Speechiness=speechiness*100, Acousticness=acousticness*100, Instrumentalness=instrumentalness*100, Liveness=liveness*100, Valence=valence*100, Mode=mode*100)%>%select(Danceability, Energy, Speechiness, Acousticness, Instrumentalness, Liveness, Valence, Mode)%>%pivot_longer(cols = everything())

      audio<-audio%>%mutate(text = paste("Audio Feature: ", name, "\nScore Out Of 100: ", value, sep=""))

      g<-ggplot(audio, aes(x=name, y=value, text=text ))+geom_bar(aes(fill=name), stat="identity") +theme(legend.position="none", plot.background = element_rect(fill = "darkslategray3"), panel.background = element_rect(fill = "darkslategray3"), axis.text.x = element_text(angle = 45, hjust = 1))+scale_color_viridis( discrete=T, guide=F)+ylab("Average Score Out Of 100")+xlab("Audio Feature")
      ggplotly(g, tooltip="text")


    }


  })



}

# Run in a dialog within R Studio
#runGadget(ui, server, viewer = dialogViewer("Spotify Info", width = 2000, height = 1000))

# Run in Viewer pane
#runGadget(ui, server, viewer = paneViewer(minHeight = 500))

# Run in browser
#runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))


shinyApp(ui = ui, server = server)
