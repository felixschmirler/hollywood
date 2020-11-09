#shiny app as submission for the fith data visualisation challenge
#interactive scatterplot, circle packing graph and network graph
#to explore commercial success, audience vs critics ratings and genres

#load necessary packages
library(shiny)
library(shinydashboard)
library(magrittr) #only for convenient %<>% operator
library(plotly) #for interactive scatter plot
library(RCurl) #to load picture for scatterplot background
library(ggallin) #for pseudo log transformation on scatter plot axis
library(visNetwork) #for network graph
library(igraph) #for network graph
library(data.tree) #to create data for circle packing graph
library(circlepackeR) #interactive packed circle chart
library(tidyverse) #function for data manipulation

#load data
hollywood <- read_csv("hollywood2019.csv", 
                      na = c("", "NA", "-"),
                      locale = readr::locale(encoding = "WINDOWS-1252")) %>%
  select(-Year)

#base64 encoding to add picture to scatterplot background
image_file <- "www/hollywood.png"
txt <- RCurl::base64Encode(
  readBin(
    image_file, "raw", file.info(image_file)[1, "size"]
    ), "txt"
  )

#create genre list
genres <- hollywood %>%
  pull(Genre) %>% 
  paste(collapse = ",") %>% 
  str_split(",") %>% 
  as_vector() %>%
  str_squish() %>%
  unique()

#create genre variables
for (i in genres) {
  hollywood %<>%
    mutate(!!i := str_detect(Genre, as.character(i)))
}

#data preparation
hollywood %<>%
  filter(!str_detect(Film, "Los Domirri"))%>% #Excluded because lack of ratings 
  mutate(
    across(matches("%"), function(x) str_remove(x, "%") %>% as.numeric())
    ) %>% 
  rename(`Percentage of Budget Recovered` = `% of budget recovered`) %>%
  mutate(
    `Proportion of Budget Recovered` = `Percentage of Budget Recovered`/100,
    `Script Type` = str_to_title(`Script Type`),
    `Average Critics` = ifelse(is.na(`Metacritic % critics`), 
                               `Rotten Tomatoes % critics`, 
                               `Average critics %`),
    `Average Audience` = ifelse(is.na(`Metacritic Audience %`), 
                                `Rotten Tomatoes % critics`, 
                                `Average audience %`),
    Audience = ifelse(`Average Audience`>= 78, "Loved by the Audience",
                      ifelse(`Average Audience`<= 60, "Hated by the Audience", 
                             "Average")),
    
  ) %>%
  select(-Genre) 

#prepare data for packed circle chart-------------------------------------------
data_nested <- 
  hollywood %>%
  transmute(
    root = "Hollywood 2019",
    group = `Script Type`,
    subgroup = ifelse(Audience != "Hated by the Audience", 
                      "Appreciated by the Audience", 
                      Film),
    subsubgroup = ifelse(Audience != "Loved by the Audience", 
                         Film, 
                         "Loved by the Audience"),
    subsubsubgroup = Film, 
    value = `Average Critics`^2,
    pathString = paste(root, group, subgroup, subsubgroup, Film, Film, sep = "/")
  )

data_Node <- as.Node(data_nested)

#prepare data for genre network graph-------------------------------------------
#create co-occurence matrix of genres
long_data <- hollywood %>% 
  select(Film , genres) %>% 
  pivot_longer(cols = -Film, names_to = "Genre", values_to = "values") %>%
  filter(values == TRUE) 

cooccurence <- crossprod(table(long_data[1:2]))
diag(cooccurence) <- 0

#create edges data frame for genre network graph
edges <- cooccurence %>% 
  as_tibble(rownames = "from") %>% 
  pivot_longer(cols = -1, names_to = "to", values_to = "width") %>% 
  filter(from != to, width != 0) %>% 
  rowwise() %>%
  mutate(filter_var = list(paste(sort(c(from, to))))) %>% 
  distinct(filter_var, .keep_all = TRUE) %>% 
  select(-filter_var) %>%
  as.data.frame()

#create nodes data frame for genre network graph
node_value <- hollywood %>%
  select(genres) %>% 
  map_df(sum) %>% 
  t()

nodes <- tibble(id = genres, label = genres, value = node_value) %>% 
  arrange(value) %>%
  mutate(
    title = paste0(round(value/1.54, 1), "% ", label," films in 2019")
  ) 

#Louvain Comunity Detection
graph <- graph_from_data_frame(edges, directed = FALSE)

cluster <- cluster_louvain(graph)

cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)
cluster_df %<>%
  mutate(label = ifelse(label == "Sci.Fi", "Sci-Fi", label))%>%
  rename(group = V1)

#Create group column
nodes <- left_join(nodes, cluster_df, by = "label")

nodes %<>%
  mutate(
    color = ifelse(group == 1,"#12336D",
                   ifelse(group == 2, "#6D83B5", 
                          "#000000")),
    group = ifelse(group == 1, "Dark",
                   ifelse(group == 2, "Entertaining",
                          "Heartbreaking")),
  )

#create data frame for network legend
legendNodes <- data.frame(
  label = c("Dark","Entertaining", "Heartbreaking"),
  color.background = c("#12336D","#6D83B5", "#000000"),
  color.border = c("#00000000", "#00000000","#00000000"),
  font.color = "#000000",
  shape = "square",
  size = 35
)

# create the UI ----------------------------------------------------------------
ui <- navbarPage(title = "Hollywood 2019",
                 tabPanel(title = "Intro",
                          tags$img(
                            width = "97.5%",
                            src = "oscar.jpg",
                            style = 
                              "position: absolute; 
                              height:85vh !important; 
                              margin-left: 0px; 
                              margin-right: 0px; 
                              padding: 0px !important;"
                          ),
                          fluidRow(),
                          fluidRow(
                            column(width = 4), 
                            column(width = 4,
                            ),
                            column(width = 4,
                                   wellPanel(style = 
                                               "background: #00000000;
                                               border: 0px; 
                                               color: white;",
                                             br(),
                                             h2("Hollywood 2019"),
                                             p("I'm sure everyone watched at 
                                               least one or two of the hollywood 
                                               films of 2019. But watching the 
                                               films is obviously not enough. We 
                                               want some data."),
                                             p("In the three tabs above you can 
                                               find 3 visualisations that allow 
                                               you to explore"),
                                             p("1. the commercial success"),
                                             p("2. the reception by the audience
                                               and critics"),
                                             p("3. the genres"),
                                             p("I hope you find some of your 
                                               favourite films from last year. 
                                               Have fun!")
                                   )
                            )
                          ),
                          fluidRow()
                 ),
                 tabPanel(title = "Commercial Success",
                          fluidRow(
                            tags$head(tags$style("#commercial1
                                                 {height:88vh !important; 
                                                 margin-left:-7px; 
                                                 margin-right: 0px; 
                                                 padding: 0px !important;}")),
                            plotlyOutput("commercial1", width = "100%")
                          )
                 ),
                 tabPanel(title = "Critics and Audience Ratings",
                          fluidRow(
                            column(width = 3,
                                   h5(strong("Audience and Critics Ratings")),
                                   br(),
                                   p("While Audience and Critics often agree on 
                                     what is a good film and what not, it is 
                                     sometimes more interesting to see where 
                                     they disagree."),
                                   p("On the right you can explore which films 
                                     were rated the highest by the audience on 
                                     rotten tomatoes and meta critics by Script 
                                     Type. The darker the circle the better the 
                                     audience rating."),
                                   p("Both websites also give critics ratings 
                                     which are indicated by the size of the 
                                     white film bubbles. A very small bubble 
                                     means it has received a low ratings by the 
                                     critics and a big one that the critics 
                                     didn't really enjoy this film."),
                                   p("Have a look and see if you agree with the 
                                     audience and critics on the films you 
                                     watched. I hope no fans of 'No Manches 
                                     Frida 2' or 'Replicas' are seeing this.")
                                   
                            ),
                            column(width = 1),
                            column(width = 6,
                                   tags$head(
                                   tags$style("#circle{height:85vh !important;}")
                                   ),
                                   circlepackeROutput("circle")
                            ),
                            column(width = 2,
                                   tags$img(
                                     width = "100%",
                                     src = "legend.jpg")
                            )
                          )),
                 tabPanel(title = "Genres",
                          fluidRow(
                            tags$head(tags$style("#network
                                                 {height:80vh !important;}")),
                            column(width = 3,
                                   h5(strong("Relationship between Film Genres")),
                                   br(),
                                   p("Often Hollywood films don't fall into one 
                                     genre alone. Unsurprisingly, some genres 
                                     co-occurr more often than others. In the 
                                     network on the right you can explore which 
                                     genres have been listed most often together 
                                     in 2019 and also see what percentage of the 
                                     154 films of 2019 fall into each genre."),
                                   p("The colours represent three clusters of 
                                     genres that were more or less 
                                     creatively labelled as 'Dark' (Dark Blue), 
                                     'Entertaining' (Light Blue), and 
                                     'Heartbreaking' (Black). Hover over or 
                                     click on the individual genres to explore 
                                     their individual relationships with other 
                                     genres. For example, Fantasy is the only 
                                     Genre that never appeared alongside Drama. 
                                     Probably there are reasons for it or are 
                                     Fantasy Dramas a gap in the market?"
                                   ),
                                   p("While it does not add any informational 
                                     value, I think it's also fun to drag the 
                                     entire network around and and watch it 
                                     behave like flubber.")
                            ),
                            column(width = 9,
                                   visNetworkOutput("network")
                            )
                          )
                 )
) # end of page

# create the server ------------------------------------------------------------
server <- function( input, output, session ){
  
  output$commercial1 <- renderPlotly({
    plot <- hollywood %>% 
      ggplot(aes(`Budget ($million)`, 
                 `Worldwide Gross ($million)`, 
                 colour = `Script Type`, 
                 text = paste0("Film: ", 
                               Film, 
                               "\nBudget: $", 
                               `Budget ($million)`, 
                               " Million\nWorldwide Gross: $", 
                               `Worldwide Gross ($million)`, 
                               " Million\nBudget recovered: ", 
                               round(`Proportion of Budget Recovered`,1), 
                               " Times"))) +
      geom_point() +
      geom_abline(slope = 1, colour = "white", size = 0.1) +
      scale_colour_manual(values = rep("white", 154)) +
      scale_x_continuous(trans = pseudolog10_trans) +
      scale_y_continuous(trans = pseudolog10_trans) +
      coord_cartesian(ylim = c(-5, 10000)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            plot.margin = margin(0, 0, 0, 0, "cm"),
            panel.background = element_blank(),
            axis.ticks.length = unit(0, "pt")
      )  +
      ggtitle("Commercial Success of Hollywood Films in 2019") +
      labs(x = NULL, y = NULL)
    
    plotly_plot <- ggplotly(plot,  tooltip = "text")
    
    plotly_plot %>%
      layout(
        images = list(
          list(
            source = paste('data:image/png;base64', txt, sep=','),
            xref = "x",
            yref = "y",
            x = 0,
            y = 5.5,
            sizex = 2.70,
            sizey = 6.5,
            opacity = 1,
            sizing= "stretch",
            layer = "below"
          )
        ),
        annotations = list(
          text = "100% Budget Recovered",
          font = list(size = 10, color = "white"),
          bgcolor = 'rgba(0,0,0,0)',
          x = 2.50,
          y = 2.6,
          xref = "x",
          yref = "y",
          ax = 0,
          ay = 0,
          textangle= -11
        ),
        margin = list(
          l = 0,
          r = 0,
          pad = 0
        )
      ) %>%
      add_annotations(
        text = "Worldwide Gross in Millions ($)",
        font = list(size = 14, color = "white"),
        bgcolor = 'rgba(0,0,0,0)',
        x = 0.125,
        y = 2,
        xref = "x",
        yref = "y",
        ax = 0,
        ay = 0,
        textangle= -90
      )%>%
    add_annotations(
      text = "Budget in Millions ($)",
      font = list(size = 14, color = "white"),
      bgcolor = 'rgba(0,0,0,0)',
      x = 1.4,
      y = -0.75,
      xref = "x",
      yref = "y",
      ax = 0,
      ay = 0
    ) 
  })
  
  output$circle <- renderCirclepackeR({
    circlepackeR(data_Node, 
                 size = "value", 
                 color_max = "#013d7e", 
                 color_min = "#e1e5f0")
  })
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges, width = "100%") %>%
      visNodes(
        shape = "dot",
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#d3d3d3", highlight = "#d3d3d3")
      ) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                 selectedBy = "group") %>% 
      visLayout(randomSeed = 11) %>%
      visLegend(position = "right",
                useGroups = FALSE,
                addNodes = legendNodes,)
  })
  
} # end of server

## run shinyApp ##
shiny::shinyApp( ui = ui, server = server)

# end of script #

