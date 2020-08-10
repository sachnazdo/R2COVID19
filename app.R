library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(ghibli)
library(scales)
library(dplyr)
# source our helper script into our file
source("scripts/reader.R")
source("scripts/preprocess.R")
corona <- read_github()
corona_latest <- subset(corona, date == max(corona$date))
corona_latest$date <- as.character(corona_latest$date)
melted_latest <- melt_all(corona_latest)
inc<- corona %>% dplyr::mutate(diff_lag_con=confirmed-lag(confirmed),
                               diff_lag_rec=recovered-lag(recovered),
                               diff_lag_dea=deaths-lag(deaths)) %>%
    group_by(date) %>%
    summarise(sum(diff_lag_con), sum(diff_lag_rec), sum(diff_lag_dea))
options(scipen = 999)
# navigational bar page
ui <- navbarPage(
    title="Covid-19 Real Time Dashboard",
    # find more themes on: https://rstudio.github.io/shinythemes/
    theme=shinytheme("flatly"),
    tabPanel("Global",
             sidebarLayout(mainPanel(
                 fluidRow(
                     column(4,
                            h5("Total Cases"),
                            wellPanel(
                                h3(textOutput("totalCases")),
                                p(img(src='up_red.png', width=15),textOutput("totalCasesInc",inline = TRUE))
                            )),
                     column(4,
                            h5("Total Recovered"),
                            wellPanel(
                                h3(textOutput("totalRecovered")),
                                p(img(src='up_green.png', width=15),textOutput("totalRecoveredInc",inline = TRUE))
                            )),
                     column(4,
                            h5("Total Deaths"),
                            wellPanel(
                                h3(textOutput("totalDeaths")),
                                p(img(src='up_red.png', width=15),textOutput("totalDeathInc",inline = TRUE))
                            ))
                 ),
                 plotOutput("globalPlot")
             ),
             sidebarPanel(h2("Covid-19 Updates"),
                          img(src="logor2.png", width=100),
                          hr(),
                          tags$head(
                              tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                          ),
                          helpText(
                              textOutput("dateToday"),
                              "Data source: Johns Hopkins University. View the details in the Credits."),
                          selectInput("topN",
                                      label="Countries to display",
                                      choices = c("Top 10 by cases", "Top 20 by cases", "Top 30 by cases"), selected = "Top 20 by cases"),
                          radioButtons("plotType",
                                       label="Plot type",
                                       choices=c("Bars", "Points"),
                                       selected = "Bars" # ini tadi belum ada
                          ),
                          selectInput("sortBy",
                                      label="Order Criteria",
                                      choices = c("Default (Alphabetical)","Confirmed Cases", "Recovered", "Deaths"), 
                                      selected = "Confirmed Cases")
                          # ini tadi belum ada
             )
             )
    ),
    tabPanel("Latest Data",
             fluidRow(
                 column(12, dataTableOutput("todayTable"))
             ),
             fluidRow(
                 column(12, downloadButton("downloadTable", " Download", style="float:right;"))
             )
    ),
    tabPanel("Credits",  
             h3("The creator received invaluable supports from:"),
             p("1. Samuel Chan (Algoritma), who has given the hints in how to polish a good web app. You guys can follow his excellent works through his", a("GitHub", href = "https://github.com/onlyphantom?tab=repositories"), "repos."),
             p("2. Data attribution of COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University. One might", em("fork"), "their repo for detail raw data or so on their", a("GitHub", href = "https://github.com/CSSEGISandData/COVID-19"), "as well."),
             p("3. Package developers of:", code('shiny'),",", code('shinythemes'),",", code('dplyr'),",", code('ggplot2'),",", code('DT'),",", code('ghibli'),",", "and", code('scales'), "which has empowered the whole system of ui and server codes."),
             p("4. Icon attribution of", a("Freepik.com", href="https://www.freepik.com/"), "and", a("Flaticon.com", href="https://www.flaticon.com/"), "who have been helping in beautifying appearance of this work."),
    ),
    tabPanel("About Us",
             p("The", a("R-square analytica", href= "https://www.google.com/maps/place/R+square+Analytica/@36.0679744,140.101196,15z/data=!4m5!3m4!1s0x0:0xe64e802306031a77!8m2!3d36.0679744!4d140.101196"),  "is a non-affiliated platform that aimed  to promote dissemination of knowledge in science, agriculture, economics, environment and their nexus to achieve sustainable development goal", a("(SDG).", href="https://www.un.org/sustainabledevelopment/sustainable-development-goals/")),
             p("Documentation of project and article related to the aforementioned dimensions convey messages that are expected to benefit the whole society worldwide. Thus it does not monetize data, findings, and anaysis that displayed within this means."),
             p("It was firstly founded in 2018 in Tsukuba, Japan. Since its first establishment, it was owned and operated by", span("Sachnaz Desta Oktarina Ph.D,", style = "color:blue"), "The", a("University of Tsukuba", href= "http://www.tsukuba.ac.jp/en/"), "graduates and", a("MEXT scholarship", href="https://www.mext.go.jp/en/policy/education/highered/title02/detail02/sdetail02/1373897.htm"),"awardee who is now working as a Socio Techno Economist in", a("Indonesian Oil Palm Research Institute.", href= "https://www.iopri.org/"), "Her research interest covers oil palm market, climate change, palm oil data science, biodiesel, and text analytics."),
             p("The commentary, views, and ideas within this platform are all her own and not reflecting the institution where she is currently working in."),
             br(),
             p("Reach her out through:"),
             p(img(src = "gmail.png", height = 20, width = 20), code('r2analytica@gmail.com')),
             p(img(src = "github.png", height = 21, width = 21), a("sachnazdo", href="https://github.com/sachnazdo")),
             p(img(src = "instagram.png", height = 20, width = 20), a("sachnaz_des", href="https://www.instagram.com/sachnaz_des/?hl=id"))
    )
)
server <- function(input, output){
    output$totalCases <- renderText({
        prettyNum(sum(corona_latest$confirmed), big.mark=",")
    })
    output$totalCasesInc <- renderText({
        prettyNum(as.numeric(inc[nrow(inc),2]), big.mark = ",")
    })
    output$totalRecovered <- renderText({
        prettyNum(sum(corona_latest$recovered), big.mark=",")
    })
    output$totalRecoveredInc <- renderText({
        prettyNum(as.numeric(inc[nrow(inc),3]), big.mark = ",")
    })
    output$totalDeaths <- renderText({
        prettyNum(sum(corona_latest$deaths), big.mark=",")
    })
    output$totalDeathInc <- renderText({
        prettyNum(as.numeric(inc[nrow(inc),4]), big.mark = ",")
    })
    output$todayTable <- renderDataTable({
        corona_latest
    }, style = "bootstrap4")
    output$downloadTable <- downloadHandler(
        filename="covid-19-latest.csv",
        content = function(file){
            write.csv(corona_latest, file, row.names = FALSE)
        }
    )
    output$dateToday <- renderText({
        paste("Updated as of: ", as.character(Sys.Date()))
    })
    output$globalPlot <- renderPlot({
        # pull the countries that are among the top-n by confirmed column
        cntToPlot <- switch(input$topN,
                            "Top 10 by cases"=corona_latest %>% top_n(10, confirmed) %>% pull(country),
                            "Top 20 by cases"=corona_latest %>% top_n(20, confirmed) %>% pull(country),
                            "Top 30 by cases"=corona_latest %>% top_n(30, confirmed) %>% pull(country)
        )
        # subset data to only use a portion of the data frame
        dat <- subset(melted_latest, country %in% cntToPlot)
        print(input$sortBy)
        # geoms <- ifelse(as.character(input$plotType) == "Bars", geom_col, geom_point)
        geoms <- switch(input$plotType,
                        "Bars" = geom_col(),
                        "Points"=geom_point(aes(size=value)))
        
        sortByCrit <- switch(input$sortBy,
                             "Default (Alphabetical)" = "default",
                             "Confirmed Cases"="confirmed",
                             "Recovered"="recovered",
                             "Deaths"="deaths"
        )
        
        if(sortByCrit != "default"){
            or <- dat$country[order(dat[dat$variable == sortByCrit, "value"])]
            ggplot(data=dat,
                   aes(x=value, y=factor(country, levels=or), fill=variable, col=variable)) +
                geoms +
                labs(
                    x="Number of cases",
                    y="Country", 
                    title = paste("Plot of", input$topN), 
                    subtitle = "--- line: Median of confirmed cases",
                    caption = "Data Source: Johns Hopkins University") +
                theme_light() +
                scale_colour_ghibli_d("LaputaMedium", direction = -1) +
                scale_fill_ghibli_d("LaputaMedium", direction = -1) +
                scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
                geom_vline(xintercept=median(dat[dat$variable=="confirmed","value"]),
                           linetype="dashed") +
                theme(legend.position = c(0.8, 0.3))
        } else {
            ggplot(data=dat,
                   aes(x=value, y=factor(country), fill=variable, col=variable)) +
                geoms +
                labs(
                    x="Number of cases",
                    y="Country", title = paste("Plot of", input$topN), subtitle = "The fight against COVID-19 is not yet over",
                    caption = "Data Source: Johns Hopkins University"
                ) +
                theme_light() +
                scale_colour_ghibli_d("LaputaMedium", direction = -1) +
                scale_fill_ghibli_d("LaputaMedium", direction = -1) +
                scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
                geom_vline(xintercept=median(dat[dat$variable=="confirmed","value"]),
                           linetype="dashed") +
                theme(legend.position = c(0.8, 0.3))
        }
        
        
    })
}
shinyApp(ui=ui, server=server)