#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)



#preparation

embs <- fromJSON(file = "embeddings.txt")

data <- rbind(
    embs$Borchev$Borchev_main, 
    embs$`Grant Pepoyan`$`Grant Pepoyan_main`, 
    embs$Galaev$Galaev_main, 
    embs$Berezin$Berezin_main, 
    embs$Voloshin$Voloshin_main, 
    embs$Roznov$Roznov_main, 
    embs$Nikitina$Nikitina_main, 
    embs$andron$andron_main, 
    embs$Nikitina2$Nikitina2_main
)

names_all <- c("Borchev_main", "Pepoyan_main", "Galaev_main", "Berezin_main", "Voloshin_main", "Roznov_main", 'Nikitina_main', 'andron_main', 'Nikitina2_main')



for(i in 1:9){
    
    for(j in 1:length(embs[[i]][[2]])){
        
        data <- rbind(data, embs[[i]][[2]][[j]])
        
        names_all <- c(names_all, names(embs[[i]][[2]])[j])
        
        
    }
    
    
}

data <- cbind(data, names_all)

data <- 
    data %>%
    as.data.frame() %>%
    mutate(names= names_all) %>%
    separate(names_all, into = c("mark", "add_info"))


for(k in 1:512){
    data[,k] = as.numeric(data[,k])
}












# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
