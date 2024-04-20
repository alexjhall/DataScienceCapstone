#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# ## Load functions and packages
# lapply(
#     grep(
#         list.files('./R', full.names = TRUE),
#         # pattern = '_scratchpad\\.R', invert = TRUE, value = TRUE
#         pattern = 'scratchpad', invert = TRUE, value = TRUE
#     ),
#     source
# )
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Next word prediction - Data Science Capstone"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             "This is where there will be basic instructions on entering text",
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             ## Input text
#             textInput(
#                 inputId = "textinput",
#                 label = "Start of the sentance",
#                 value = "Please write something"
#             ),
#             ## Outputted predicted words
#             
#             
#         )
#     )
# )
