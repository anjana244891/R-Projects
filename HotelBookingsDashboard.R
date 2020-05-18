
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)

mydata = read.csv("/Users/anjanaananthraman/Downloads/hotel_bookings.csv")

ui <- fluidPage(

    # Application title
    titlePanel(
        h2("Term of Stay by Booking source", align = "center")
    ),
    sidebarLayout(
        sidebarPanel(
            selectInput("channel","Booking Source :", choices = list("Corporate", "Direct", "Global Distribution System", "Travel Agent/Tour Operator","Other"), selected = "Corporate"),
            radioButtons("dist", "When Stayed:",c("Weekend" = "weekend","WeekNight" = "weeknight")),
        ),

        # Show a plot of the generated distribution
       
        mainPanel(
            
                  column(12,plotOutput(outputId="distPlot", width="500px",height="500px")),
                
            )
        
    )
)

server <- function(input, output) {

    
        
    output$distPlot <- 
        renderPlot({
            channel = input$channel
            print(channel)
            if (channel== "Global Distribution System")
            channel="GDS"
            else if (channel== "Travel Agent/Tour Operator")
                channel="TA/TO"
            else if (channel== "Other")
                channel="Undefined"
            df = mydata[mydata$distribution_channel==channel,]
            
            dist = input$dist
            if(dist == "weekend") {            
            newdf = df[c(5,8)]
            x= newdf$stays_in_weekend_nights
            w <- ifelse((x >= 0) & (x<=5), "short", ifelse((x >= 6) & (x <= 10), "medium", "long"))
            newdf = data.frame(newdf,w)
            
         #plot bar1 for weekends
            
            bar = ggplot(newdf, aes(arrival_date_month, stays_in_weekend_nights, fill =w)) +
            geom_bar(stat="identity", position=position_dodge())
            bar + scale_fill_brewer(palette="Paired",name = "Term of Stay", 
                                     labels = c("Short", "Medium","Long")) + theme_minimal()+ 
            labs(x = "Months", y = "Number of Nights", title = "Term of Stay on Weekends")+
            theme(axis.text.x  = element_text(angle = 45,vjust = 1))
            } # weekend ends
            
            else if(dist == "weeknight") {
                
                newdf = df[c(5,9)]
                x= newdf$stays_in_week_nights
                w <- ifelse((x >= 0) & (x<=5), "short", ifelse((x >= 6) & (x <= 10), "medium", "long"))
                newdf = data.frame(newdf,w)
                
                bar = ggplot(newdf, aes(arrival_date_month, stays_in_week_nights, fill = w)) +
                    geom_bar(stat="identity", position=position_dodge())
                bar + scale_fill_brewer(palette="Paired",name = "Term of Stay", 
                                         labels = c("Short", "Medium","Long")) + theme_minimal()+
                    labs(x = "Months", y = "Number of Nights", title = "Term of Stay on Weeknights")+
                    theme(axis.text.x  = element_text(angle = 45,vjust = 1))
                
                
            }
            
        })
    
        
    }

# Run the application 
shinyApp(ui = ui, server = server)
