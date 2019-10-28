# shinyProject

    #ui 
    shinyUI(dashboardPage(
        dashboardHeader(title = "Apple Store"),
        dashboardSidebar(

            sidebarUserPanel("Tabs"),
            sidebarMenu(
                menuItem("Analysis", tabName = "genre", icon = icon("apple"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "genre",
                        fluidRow(box(plotlyOutput("graph1"), width = 12),
                                 box(selectizeInput("yvar",
                                                    label = "Select a variable",
                                                    choices = graph1.yvar,
                                                    selected = "price")
                        )))),
                        fluidRow(box(plotlyOutput("graph2"), width = 12),
                                 box(selectizeInput("yvar2",
                                                    label = "Select a variable",
                                                    choices = graph2.yvar,
                                                    selected = "Mean Rating"))
                                 ),
                        fluidRow(box(plotlyOutput("graph3"), width = 12)

                        ),
                        fluidRow(box(plotlyOutput("graph4"), width = 12) )
        ) 

             )

    )

    #server
    shinyServer(function(input, output,session){

        reactive_graph1 <- reactive({
            df.graph1 %>%
                select(prime_genre, y = input$yvar) %>%
                arrange(desc(y)) 
        })
        output$graph1 <- renderPlotly({
            ggplot(reactive_graph1(),aes(x=reorder(prime_genre,-y),y=y)) + 
                geom_bar(aes(fill = prime_genre),stat="identity") +
                theme(axis.text.x=element_text(family="Montserrat",size=12),text=element_text(family="Montserrat",size=12),legend.title=element_blank()) +
                ggtitle("Genre Analysis") +
                xlab("Genre") +
                ylab(input$yvar) + 
                coord_flip()
        })

        reactive_graph2 <- reactive({
            df.graph2 %>%
                select(prime_genre, y = input$yvar2) %>%
                arrange(desc(y)) 
        })
        output$graph2 <- renderPlotly({
            ggplot(reactive_graph2(),aes(x=reorder(prime_genre,-y),y=y)) + 
                geom_bar(aes(fill = prime_genre),stat="identity") +
                theme(axis.text.x=element_text(family="Montserrat",size=12),text=element_text(family="Montserrat",size=12),legend.title=element_blank()) +
                ggtitle("Ratings By Genre") +
                xlab("Genre") +
                ylab(input$yvar2) +
                coord_flip()
        })
         output$graph3 <- renderPlotly({
             ggplot(df.app,aes(x=rating_count_tot, y = user_rating)) +
                 geom_point(position = "jitter",size = 0.5,color = "#0000FF") +
                 geom_smooth(method='lm', formula = y~x, col = "Red") + 
                 stat_cor(method='pearson',label.x.npc = "center",label.y.npc = "bottom") + 
                 ggtitle("Number of Reviews vs. Average Rating") +
                 xlab("Number of Reviews") + 
                 ylab("Average Rating") + 
                 ylim(1,5) + 
                 xlim(100,2500)


         })

         output$graph4 <- renderPlotly({
             ggplot(data = df.app, aes(x = (((user_rating_ver-user_rating)/(user_rating))*100))) + 
                 geom_histogram(binwidth = 10,color='#003399',fill='#66CCFF') + 
                 xlim(-50,50)+
                 xlab("Percent Change") + 
                 ylab("Frequency") + 
                 ggtitle("Percent Change in Average Review From Version to New Version")


         })
    })

    #global
    library(ggplot2)
    library(plotly)
    library(shiny)
    library(dplyr)
    library(shinydashboard)
    library(ggpubr)

    df.app <- read.csv('AppleStore copy.csv')

    df.app <- df.app %>%
      select(size_bytes, price, rating_count_tot, rating_count_ver, user_rating, user_rating_ver,cont_rating,
             prime_genre,sup_devices.num,lang.num,vpp_lic) 

    df.graph1 <- df.app %>%
      group_by(prime_genre) %>%
      summarise("Frequency" = n(),
                "Size in MB" = mean(size_bytes/1000000),
                "Price" = mean(price),
                "Supported Device" = mean(sup_devices.num),
                "Supported Languages" = mean(lang.num))

    graph1.yvar= c("Frequency",
                   "Size in MB",
                   "Price",
                   "Supported Device",
                   "Supported Languages")

    df.graph2 <- df.app %>%
      group_by(prime_genre) %>%
      summarise("Average Rating" = mean(user_rating),
                "Number of Ratings" = mean(rating_count_tot),
                "Average Number of Ratings" = mean(rating_count_tot)/(n())
      )

    graph2.yvar = c("Average Rating",
                    "Number of Ratings",
                    "Average Number of Ratings")
