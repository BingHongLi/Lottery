shinyUI(fluidPage(
  titlePanel("Changing the values of inputs from the server"),
  fluidRow(
    
    column(3,
           wellPanel(

             
             checkboxGroupInput("inCheckboxGroup",
                                "Enter the Number:",
                                c(1:49),
                                inline=T),
             submitButton("Submit")

           )

    ),
    column(9,
           plotOutput("plot1", width = 400, height = 300),
           verbatimTextOutput("text")
    )
  )
))