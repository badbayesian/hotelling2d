library(shiny) # Shiny!
library(uuid) # For Unique id Createion
library(deldir) # Does the heavy lifting on Voronoi diagram. 
library(shinyalert) # Does model popups for scores and instructions. 
library(uuid) # Generates random ids for game and session.
library(RMySQL) # Database Stuff
library(dbplyr) # Database Stuff
library(dplyr) # Database Stuff
library(DBI) # Database Stuff
library(stringr) # For Regex
library(shinyjs) # For Cookies and Stuff
library(shinyWidgets) # For Alerts


#################################################################
##               JS Code to Make and Get Cookies               ##
#################################################################
jsCode <- '
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    } else {
      var cookie = "";
      Shiny.onInputChange("jscookie", cookie);
    }
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: 30 });
    Shiny.onInputChange("jscookie", params);
  }

'

#################################################################
##                       SQL Server Info                       ##
#################################################################
source(file="dbcredentials.R")

# File above should have format:

# options(mysql = list(
#   "host" = "xxx.xxx.xxx.xxx",
#   "port" = 3306,
#   "user" = "user",
#   "password" = "password"
# ))
# databaseName <- "dbname"

# SQL Server Structure
# clicks table: gameid (varchar), x (float), y (float)
# games table: gameid (varchar), id (varchar), time (varchar)
# scores table: score (float), gameid (varchar), name (varchar)
# sessions table: ip (varchar), start (varchar), name (varchar)

#################################################################
##                       Game Parameters                       ##
#################################################################
firms <- 10 #This includes the player.
totalCustomers <- 10000 #This is multiplied by the area to get profit.

startText <- "Click the question mark for instructions.<br><br>
Click the bar chart for today's top 5 players.<br><br>
The map changes every day.<br><br>
Come back tomorrow for another challenge.<br><br>
How would you like to show up on the leaderboard?<br>
You can use 1-10 alphanumeric characters."

instructionsText1 <- "In this game you are a firm.\n
In each round you choose a location for your firm.\n
Click anywhere on the map to place your firm.\n
The other firms stay put."

instructionsText2 <- "10000 consumers live on the map.\n
Each will pay $1 to the closest firm.\n
The consumers that buy from you are shaded in red.\n
The number shown below each firm is that firm's cumulative profit.\n
In each round, the firm with the lowest cumulative profit goes out of business.\n
That firm's customers are shaded in blue."

cookieText <- "This site uses cookies to store your name for the leaderboard.<br><br> To opt-out, check the box below."

##################################################################
##                      Database Functions                      ##
##                     Append Data to Table                     ##
##################################################################
appendData <- function(data,table) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password) #Connect
  dbWriteTable(db, table, data, append = TRUE,row.names=FALSE,header=TRUE) #Write Data to Table
  dbDisconnect(db) #Disconnect
}

#################################################################
##                      Database Functions                     ##
##                Get the Top 5 Players for Map                ##
#################################################################
getTopPlayers <- function(map){
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password) #Connect
  scores <- tbl(db,"scores") %>% filter(map== !!map) %>% group_by(name) %>% summarize(max_score = max(score)) %>% slice_max(max_score,n=5) %>% select(max_score,name) #Get top 5 Players and Score
  scores <- as.data.frame(scores) #Turn Player, Score Table to Data Frame
  dbDisconnect(db) #Disconnect
  return(scores)
}

#################################################################
##                      Database Functions                     ##
##                    Get all Scores on Map                    ##
#################################################################
getScores <- function(map){
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password) #Connect
  scores <- tbl(db,"scores") %>% filter(map==!!map) %>% arrange(-score) %>% select(score,name) #Get scores and names
  scores <- as.data.frame(scores) #Turns scores and names to data frame
  dbDisconnect(db) # Disconnect
  return(scores)
}

##################################################################
##                      Database Functions                      ##
##                    Get Clicks From GameId                    ##
##                       Not Used in Game                       ##
##################################################################
getClicks <- function(game){
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  clicks <- tbl(db,"clicks") %>% filter(gameid==!!game) %>% arrange(-round) %>% select(x,y)
  clicks <- as.data.frame(clicks)
  dbDisconnect(db)
  return(clicks)
}

#################################################################
##                        Game Functions                       ##
##          Get Profits from Current Locations Matrix          ##
#################################################################
getProfits <- function(locations) {
  x <- locations[, 1]
  y <- locations[, 2]
  customers <- deldir(x, y, rw = c(0, 1, 0, 1))
  customerAreas <- tileInfo(customers, bndry = TRUE)$Areas
  profit <- customerAreas * totalCustomers
  return(profit)
}

#################################################################
##                        Game Functions                       ##
##     Get Tiling for Visulization from Current Locatiions     ##
#################################################################
getTiles <- function(locations) {
  x <- locations[, 1] 
  y <- locations[, 2]
  customers <- deldir(x, y, rw = c(0, 1, 0, 1))
  tileCustomers <- tile.list(customers)
  tileCustomers
}

##################################################################
##                        Plotting Helper                       ##
##                  Make Vector of Tile Colors                  ##
##################################################################
makeColorVector <- function(locations,profits){
  colors <- c("#BF98A0", rep("#312F2F", dim(locations)[1]))
  minProfitIndex <- which(profits == min(profits))
  if (all(profits == 0)) {
    colors <- c(rep("#312F2F", dim(locations)[1]))
    shapes <- rep(20, dim(locations)[1])
  }
  if (!all(profits == 0)) {
    colors[minProfitIndex] <- "#A6D8F7"
  }
  return(colors)
}

##################################################################
##                        Plotting Helper                       ##
##                  Make Vector of Plot Shapes                  ##
##################################################################
makeShapesVector <- function(locations,profits){
  shapes <- c(17, rep(20, dim(locations)[1] - 1))
  if (all(profits == 0)) {
    shapes <- rep(20, dim(locations)[1])
  }
  return(shapes)
}

##################################################################
##                        Game Functions                        ##
##                     Create Customer Plot                     ##
##################################################################
plotCustomers <- function(locations, profits, text = "") {
  colors <- makeColorVector(locations,profits)
  shapes <- makeShapesVector(locations,profits)
  tileCustomers <- locations |> getTiles()
  par(mar = c(0, 0, 0, 0) + 0.1) #Set up the base plot
  par(bg = "#312F2F") #Set up the base plot
  plot(c(),c(),xlim = c(0, 1),ylim = c(0, 1),axes = FALSE,lwd = 3,xlab = "",col="white",ylab = "") #Set up the base plot
  plot.tile.list(tileCustomers,number = FALSE,border = "white",lwd = 3,col.pts="white",fillcol = colors,close = TRUE,showpoints = TRUE,pch = shapes,add = TRUE) #Add Game Plot!
  text(locations[, 1], locations[, 2] - 0.03, paste0("$", profits),col="white",cex=1,,family="mono") #Add profit text
  text(0.5, 0.5, text, col = "white", cex=3,family="mono") #Add input text.
}


#################################################################
##                         Plot Clicks                         ##
#################################################################
plotClicks <- function(clicks, text = "Test") {
  par(mar = c(0, 0, 0, 0) + 0.1) #Set up the base plot
  par(bg = "#312F2F") #Set up the base plot
  plot(c(),c(),xlim = c(0, 1),ylim = c(0, 1),axes = TRUE,lwd = 3,xlab = "",col="white",ylab = "") #Set up the base plot
  points(clicks$x,clicks$y,pch=19,col='white')
  box(col='white')
  text(clicks$x, clicks$y - 0.08, 1:length(clicks$x),col="white",cex=1,,family="mono") #Add profit text
}


##################################################################
##                              UI                              ##
##################################################################
ui <- fluidPage(
  shinyjs::useShinyjs(), #Turn on Javascript
  shinyjs::extendShinyjs(text = jsCode,functions=c("setcookie","getcookie")), # Add the javascript cookies functions to Shiny.
  tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js")), # Get cookie helpers.
  tags$script('$(document).on("shiny:sessioninitialized",function(){$.get("https://api.ipify.org", function(response) {Shiny.setInputValue("getIP", response);});})'), # Javascript to get user IP
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")), # Add the stylesheet.
  
  # Header
  fluidRow(
    # Title
    column(12, style = "vertical-align: middle; text-align: center; background-color:#312F2F; height: 35px", 
           h2("HOTELLING",style="color: white; padding: 0; margin: 0")
    ), # End Title
    
    # Icons
    column(12, style = "padding-top: 0; padding-bottom: 0;  vertical-align: middle; text-align: center; background-color:#312F2F; height: 35px", 
           actionLink("instructions", "", icon = icon("question", style="color: white; padding-left: 15px; padding-right: 15px",lib = "font-awesome")),
           actionLink("scores", "", icon("bar-chart", style="color: white; padding-left: 15px; padding-right: 15px",lib = "font-awesome")),
           actionLink("settings", "", icon("cog", style="color: white; padding-left: 15px; padding-right: 15px",lib = "font-awesome"))
           # actionLink("login", "", icon("user", style="color: white; padding-left: 15px; padding-right: 15px",lib = "font-awesome"))
    ), # End Icons
  ), # End Header
  
  # Game Map
  fluidRow(
    column(12, align="center", style = "margin-top: -10px; background-color:#312F2F; padding: 0 0 0 0",
           plotOutput("distPlot",click = "plot_click",width="350px",height="350px")
    )
  ), #End Game Map
  
  # Game Over Text
  fluidRow(
    column(12, align="center", style = "background-color:#312F2F; padding: 0 0 0 0",div(verbatimTextOutput('gameOver'),style="background-color:#312F2F; width: 200px"))
  ), #End Game Score
  
  # Click Map
  fluidRow(
    column(12, align="center", style = "margin-top: 0px; background-color:#312F2F; padding: 0 0 0 0",
           plotOutput("clickPlot",width="150px",height="150px")
    )
  ), #End Click Map
  
  # Game Score
  fluidRow(
    column(12, align="center", style = "background-color:#312F2F; padding: 0 0 0 0",div(verbatimTextOutput('score'),style="background-color:#312F2F; width: 200px"))
  ), #End Game Score
  
)

server <- function(input, output, session) {
  
  js$getcookie() # See if there is a name cookie.
  js$getcookie() # Need a second check to make sure cookie is found. Not sure why.
  
  
  ##################################################################
  ##                  Setup Reactive Data Frames                  ##
  ##                             User                             ##
  ##################################################################
  user <- reactiveValues()
  user$name<-""
  user$sessionId <- UUIDgenerate()
  user$gameId <- ""
  user$map <- ""
  user$sessionStartTime <- Sys.time()
  if(length(isolate(input$getIP))==1){user$ip<-isolate(input$getIP)}else{user$ip<-"null"} # Reactive ip address.
  
  ##################################################################
  ##                         Cookie Event                         ##
  ##################################################################
  observeEvent(input$jscookie,{
    #If Cookie Found, Set name and write session info to database. 
    if (!is.null(input$jscookie) && input$jscookie != "") {
      user$name <- input$jscookie
      df <- data.frame(ip=user$ip,start=user$sessionStartTime,id=user$sessionId,name=user$name,stringsAsFactors = FALSE)
      appendData(df,"sessions")
    } 
    #If No Cookie, Ask Name
    else {
      inputSweetAlert(session = session,title="New Here?", inputId="name",text=startText,input="text",allowOutsideClick=FALSE,html=startText)
    }
  })
  
  #################################################################
  ##                    After Name Submission                    ##
  ##                      Ask Cookie Opt-Out                     ##
  #################################################################
  observeEvent(input$name,{
    inputSweetAlert(session = session,inputId = "nocookie",html = cookieText,input = "checkbox",title = "Opt Out?",inputPlaceholder = "opt-out",allowOutsideClick = FALSE,showCloseButton = FALSE)
  })
  
  #################################################################
  ##               After Cookie Opt-Out Submission               ##
  ##                          Check Name                         ##
  #################################################################
  observeEvent(input$nocookie,{
    if(str_detect(input$name, "^[a-zA-Z0-9]{1,10}$")){
      if(input$nocookie==0){
        js$setcookie(input$name)
      }
      user$name <- input$name
      df <- data.frame(ip=user$ip,start=user$sessionStartTime,id=user$sessionId,name=user$name,stringsAsFactors = FALSE)
      appendData(df,"sessions")
    }else{
      inputSweetAlert(session = session, inputId="name2",html="<p style='color:red'>Please check your input.</p>How would you like to show up on the leaderboard?<br>You can use 1-10 alphanumeric characters.",input="text",allowOutsideClick=FALSE)
      
    }
  })
  
  #################################################################
  ##                       If Name was Bad                       ##
  ##                        Ask Name Again                       ##
  #################################################################
  observeEvent(input$name2,{
    if(str_detect(input$name2, "^[a-zA-Z0-9]{1,10}$")){
      if(input$nocookie==0){
        js$setcookie(input$name2)
      }
      user$name<-input$name2
      df <- data.frame(ip=user$ip,start=user$sessionStartTime,id=user$sessionId,name=user$name,stringsAsFactors = FALSE)
      appendData(df,"sessions")
    }else{
      inputSweetAlert(session = session, inputId="name2",html="<p style='color:red'>Please check your input.</p>How would you like to show up on the leaderboard?<br>You can use 1-10 alphanumeric characters.",input="text",allowOutsideClick=FALSE)
      
    }
  })
  
  
  ##################################################################
  ##                      Initial Game Setup                      ##
  ##################################################################
  game <- reactiveValues() # Make the game reactive dataframe
  clicks <- reactiveValues(x=c(),y=c()) # Make the clicks reactive
  set.seed(as.numeric(Sys.Date())) # For Daily Map
  initialLocations <- matrix(runif(firms * 2), firms, 2) #Make Daily Map Locations
  game$locations <- initialLocations #Add initial locations to game reactive.
  set.seed(as.numeric(Sys.time())) #Reset seed to get unique game id.
  game$gameId <- UUIDgenerate() #Update game id
  game$startTime <- Sys.time() #Update game strt time 
  game$map <- Sys.Date() #Update game map 
  game$profits <- rep(0, firms) #Set initial profits to 0
  game$gameOver <- 0 #Game is not over... yet.
  game$round <- 0 #Set round to 0
  # Write Game Info
  df <- data.frame(gameid=isolate(game$gameId),id=isolate(user$sessionId),time=isolate(game$startTime),stringsAsFactors = FALSE)
  appendData(df,"games")
  # Make the initial map
  output$distPlot <- renderPlot(plotCustomers(game$locations[-1, ], game$profits[-1]))

  
  #################################################################
  ##                     Scores Icon Clicked                     ##
  #################################################################
  observeEvent(input$scores,{
    scores <- getTopPlayers(Sys.Date())
    names <- scores[,2]
    scores <- scores[,1]
    scoreString <- ""
    for(i in 1:5){
      scoreString<-paste0(c(scoreString,i,": ",scores[i]," -- ",as.character(names[i]),"\n"),collapse="")
    }
    shinyalert("Top 5 Players Today",scoreString,size="xs")
  })
  
  ##################################################################
  ##                     Instructions Clicked                     ##
  ##################################################################
  observeEvent(input$instructions,{
    shinyalert("Instructions",instructionsText1,size="s",callbackR = function(value){ 
      shinyalert("Instructions",instructionsText2,size="s")})
  })
  
  #################################################################
  ##                         Game Logic!                         ##
  ##                          New Action                         ##
  #################################################################
  observeEvent(input$plot_click, {  
    
    
    #################################################################
    ##                     If the Game is Over                     ##
    ##                      Setup a New Game                       ##
    #################################################################
    if (game$gameOver == 1) {
      clicks$x <- c() #Reset clicks reactive
      clicks$y <- c() #Reset clicks reactive
      output$clickPlot <- renderText("") #Reset Click Plot
      output$score <- renderText("") #Reset the score display
      output$gameOver <- renderText("") # Reset the game over display
      set.seed(as.numeric(Sys.Date())) # For Daily Map
      initialLocations <- matrix(runif(firms * 2), firms, 2) #Make Daily Map Locations
      game$locations <- initialLocations #Add initial locations to game reactive.
      set.seed(as.numeric(Sys.time())) #Reset seed to get unique game id.
      game$gameId <- UUIDgenerate() #Update game id
      game$startTime <- Sys.time() #Update game strt time 
      game$map <- Sys.Date() #Update game map 
      game$profits <- rep(0, firms) #Set initial profits to 0
      game$gameOver <- 0 #Game is not over... yet.
      game$round <- 0 #Set round to 0
      # Write Game Info
      df <- data.frame(gameid=isolate(game$gameId),id=isolate(user$sessionId),time=isolate(game$startTime),stringsAsFactors = FALSE)
      appendData(df,"games")
      # Make the map
      output$distPlot <- renderPlot(plotCustomers(game$locations[-1, ], game$profits[-1]))
    } 
    
    #################################################################
    ##                      If Game Continues                      ##
    #################################################################
    else{
      
      #Update clicks reactive and database table. 
      clicks$x <- c(clicks$x,input$plot_click$x) 
      clicks$y <- c(clicks$y,input$plot_click$y)
      df <- data.frame(gameid=game$gameId, x=input$plot_click$x, y=input$plot_click$y,round=game$round,stringsAsFactors = FALSE)
      appendData(df,"clicks")
      
      #Remove lowest profit firm from last round.
      if (game$round > 0) {
        firmToRemove <- which(game$profits == min(game$profits))
        game$locations <- game$locations[-firmToRemove, ]
        game$profits <- game$profits[-firmToRemove]
      }
      
      game$locations[1, ] <- c(input$plot_click$x, input$plot_click$y) # Change player's location to click.
      game$profits <- game$profits + getProfits(game$locations) # Update profits
      
      #Check if the game should end.
      firmToRemove <- which(game$profits == min(game$profits)) #Check if I am the firm to remove. 
      
      
      #################################################################
      ##                          Game Over                          ##
      #################################################################
      if (firmToRemove == 1) {
        output$distPlot <- renderPlot(plotCustomers(game$locations, game$profits)) #Update Plot
        df <- data.frame(gameid=user$gameId,score=game$profits[1],map=game$map,name=user$name,stringsAsFactors = FALSE) #Set up score for database.
        appendData(df,"scores") # Write score to database. 
        scores <- getScores(game$map)[,1] #Pull map's scores.
        # percentile <- 100-base::round(100*mean(game$profits[1]<=scores),0) #Calculate my percentile from game map's scores.
        output$clickPlot <- renderPlot(plotClicks(isolate(clicks))) #Update Click Plot
        output$score <- renderText(paste0(isolate(user$name),"  ",game$map,"  ",game$profits[1],collapse = "")) # Make score display.
        output$gameOver <- renderText("Game Over") # Make game over display.
        game$gameOver <- 1 #Set gameover.
      } 
      
      
      
      #################################################################
      ##                          Game Win!                          ##
      #################################################################i
      else if (length(game$profits) == 2 && firmToRemove != 1) {
        output$distPlot <- renderPlot(plotCustomers(game$locations, game$profits)) #Update Plot
        df <- data.frame(gameid=user$gameId,score=game$profits[1],map=game$map,name=user$name,stringsAsFactors = FALSE) #Set up score for database.
        appendData(df,"scores") # Write score to database. 
        scores <- getScores(game$map)[,1] #Pull map's scores.
        # percentile <- 100-base::round(100*mean(game$profits[1]<=scores),0) #Calculate my percentile from game map's scores.
        output$clickPlot <- renderPlot(plotClicks(isolate(clicks))) #Update Click Plot
        output$score <- renderText(paste0(isolate(user$name),"  ",game$map,"  ",game$profits[1],collapse = "")) # Make score display.
        output$gameOver <- renderText("Win!") # Make game over display.
        game$gameOver <- 1 #Set gameover.
        
      } 
      
      
      ##################################################################
      ##                        Game Continues                        ##
      ##################################################################
      else{
        output$distPlot <- renderPlot(plotCustomers(game$locations, game$profits)) #Update Plot
        game$round <- game$round + 1 #Increment Round
      }
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
