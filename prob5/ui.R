library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Poll Analytics",

  tabPanel("Introduction",
          
          #plotOutput("background"), 
            
          column(10, offset=3, h1(strong("Twitter Data Analytics"))),
          
          hr(),hr(),hr(),
          
          helpText("Click on the tabs above to check out the statistics."),
          hr(),
          column(8,offset=2,
                 
                 plotOutput("word_cloud"),
                            hr(),
                            column(5,offset=3,
                                   textInput("search","Search",value="",placeholder = "Type to see trend.."))
                            )
   ),                   
  tabPanel("Candidate Popularity",
           
           h2("Who is more Popular?"),
           
           plotOutput("popularity_plot"),
    
           hr(),
          
           h4("We have analyzed thousands of tweets over a range of days to figure out who is being talked about the most."),
           h5("\nClick on the radio button below to see the variation of each Candidates popularity."),
           
           column(12,offset=1,radioButtons("radio_button", label = h3("Candidates"),
                        choices = list("All"=1,"Hillary Clinton"=2,"Bernie Sanders"=3,
                                       "Donald Trump"=4,"Marco Rubio"=5,"Ted Cruz"=6,"Ben Carson"=7),
                        selected=1, inline=T))
           ),
  
  tabPanel("Always on Twitter",
           
           #!!!For this, you need to have the indiviual json files for each
           #candidates. This is provided in the candidate files in the
           #tar archive.!!!
           h2("Who is most Active on Twitter?"),
           
           helpText("In the digital age that we live in, being active on Social Media can make or break a
                    campaign. By accessing the Presidential candidates user timelines, we can determine who
                    among the top 6 use their Twitter handle the most."),
           
           plotOutput("most.active"),
  
           hr(),hr(),
    
          column(12,offset=4,
                  helpText("Have a closer look by choosing your candidate."), 
                  selectInput("select",label=h3("Candidate"),
                      choices = list("Select"=1,"Hillary Clinton"=2,"Bernie Sanders"=3,
                                     "Donald Trump"=4,"Marco Rubio"=5,"Ted Cruz"=6,"Ben Carson"=7),
                      selected=1))
          
  ),
  tabPanel("Trending Hashtags",
           
           column(12,offset=4,h2("Trending Hashtags")),
           hr(), br(),
           
           column(12,offest=4,helpText("Select the date to find the top five trending hashtags."))  ,
              
           column(12,offset=4,dateInput("date","Date", value="2016-02-28",min="2016-02-28",
                     max=Sys.Date()),
                
                  column(12, offset=1,tableOutput("hashtag_table")))
)))
