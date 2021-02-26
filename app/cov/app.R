#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(cowplot)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covariance"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("obs", "Observations:", 4, min = 0, max = 60, step=1),
            selectInput("sign", "Sign:",
                        choices=c("negative", "positive")),
            selectInput("strength", "Strength:",
                        choices=c("weak", "moderate", "strong"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("info"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    buildData <- reactive({
        if(input$strength == "weak") B <- 60
        if(input$strength == "moderate") B <- 20
        if(input$strength == "strong") B <- 5
        x <- runif(200, min=0, max=100)
        if(input$sign == "negative"){
            y <- rnorm(200, mean=-1*x, sd=B)
        }
        if(input$sign == "positive"){
            y <- rnorm(200, mean=x, sd=B)
        }
        data.frame(x,y)
    })


    inputdat <- reactive({
        dat <- buildData()[1:input$obs,]
        colnames(dat) <- c("X", "Y")
        dat
    })

    output$distPlot <- renderPlot({
        dat <- inputdat()
        ymean <- mean(dat[,2])
        xmean <- mean(dat[,1])
        x1 <- x2 <- y1 <- y2 <- sign <- c()
        for(i in 1:nrow(dat)){
            if(dat[i,1] < xmean){
                x1[i] <- dat[i,1]
                x2[i] <- xmean
            }else{
                x1[i] <- xmean
                x2[i] <- dat[i,1]
            }
            if(dat[i,2] < ymean){
                y1[i] <- dat[i,2]
                y2[i] <- ymean
            }else{
                y1[i] <- ymean
                y2[i] <- dat[i,2]
            }
            if((dat[i,1] > xmean) == (dat[i,2] > ymean)){
                sign[i] <- "positive"
            }
            if((dat[i,1] > xmean) != (dat[i,2] > ymean)){
                sign[i] <- "negative"
            }
        }

        dat <- data.frame(dat, x1, x2, y1, y2, sign)
        areas <- abs((dat$x1-dat$x2) * (dat$y1-dat$y2))
        posedge <- sqrt(sum(areas[dat$sign == "positive"]))
        negedge <- sqrt(sum(areas[dat$sign == "negative"]))
        d <- data.frame(x1=c(0,0),
                        x2=c(posedge,negedge),
                        y1=c(0,0),
                        y2=c(posedge,negedge),
                        sign=c("positive","negative"))
        e <- ggplot(dat, aes(X, Y))
        plot1 <- e + geom_point() +
                 theme_bw() +
                 geom_hline(yintercept= ymean) +
                 geom_vline(xintercept = xmean) +
            geom_rect(data=dat,
                      mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=sign),
                      alpha=0.1)

        plot2 <- ggplot() +
            theme_bw() +
            geom_rect(data=d,
                      mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=sign),
                      alpha=0.3) +
            annotate(geom="text", x=max(d$x2)*.2, y=d$y2[1],
                     label=paste("+ Area =",round(posedge^2,digits=2)),
                     color = "blue") +
            annotate(geom="text", x=max(d$x2)*.2, y=d$y2[2],
                     label=paste("- Area =",round(negedge^2,digits=2)),
                     color = "red")+
        annotate(geom="text", x=max(d$x2)*.5, y=max(d$y2)*.1,
                 label=paste(round(posedge^2, digits=2), "-",
                             round(negedge^2,digits=2),
                             "/(",nrow(dat),"-1)\n",
                             round((posedge^2 - negedge^2)/(nrow(dat)-1),digits=2), sep=""),
                 color = "black")


        plot_grid(plot1, plot2)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
