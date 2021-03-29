library(shiny)
library(bs4Dash)

shinyApp(
    ui = dashboardPage(
        title = "Basic Dashboard",
        header = dashboardHeader(),
        sidebar = dashboardSidebar(),
        controlbar = dashboardControlbar(),
        footer = dashboardFooter(),
        body = dashboardBody()
    ),
    server = function(input, output) {}
)