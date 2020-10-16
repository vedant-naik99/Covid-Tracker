# update data with automated script
source("data.R") # run locally to update numbers, but not live on Rstudio server (to avoid possible errors on auto-updates)

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_active = "#0282cc"
covid_normal = "#0c0d0d"
covid_recovered = "#0eed28"

# import data
cv_cases = read.csv("input_data/coronavirus.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")
cv_states = read.csv("input_data/coronavirus_states.csv")

### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
    plot_df = subset(cv_aggregated, date<=plot_date)
    g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
        ylab("cumulative cases") + theme_bw() + 
        scale_colour_manual(values=c(covid_normal)) +
        scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
    plot_df_new = subset(cv_aggregated, date<=plot_date)
    g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
        geom_bar(position="stack", stat="identity") + 
        ylab("new cases") + theme_bw() + 
        scale_fill_manual(values=c(covid_normal)) +
        scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g1
}

# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"), plot_start_date) {
    if (start_point=="Date") {
        g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, group = 1,
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
            xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
    }
    
    if (start_point=="Day of 100th confirmed case") {
        cv_cases = subset(cv_cases, days_since_case100>0)
        g = ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region, group = 1,
                                 text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
            xlab("Days since 100th confirmed case")
    }
    
    if (start_point=="Day of 10th death") {
        cv_cases = subset(cv_cases, days_since_death10>0)
        g = ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region, group = 1,
                                 text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
            xlab("Days since 10th death")
    }
    
    g1 = g +
        geom_bar(position="stack", stat="identity") + 
        ylab("new") + theme_bw() + 
        scale_fill_manual(values=country_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}


# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"), plot_start_date) {
    if (start_point=="Date") {
        g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
            xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
    }
    
    if (start_point=="Day of 100th confirmed case") {
        cv_cases = subset(cv_cases, days_since_case100>0)
        g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                                 text = paste0("Day ", days_since_case100,"\n", region, ": ",outcome))) +
            xlab("Days since 100th confirmed case")
    }
    
    if (start_point=="Day of 10th death") {
        cv_cases = subset(cv_cases, days_since_death10>0)
        g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                                 text = paste0("Day ", days_since_death10,"\n", region, ": ",outcome))) +
            xlab("Days since 10th death")
    }
    
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
        ylab("cumulative") + theme_bw() + 
        scale_colour_manual(values=country_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}


# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"), plot_start_date)  {
    if (start_point=="Date") {
        g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
            xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
    }
    
    if (start_point=="Day of 100th confirmed case") {
        cv_cases = subset(cv_cases, days_since_case100>0)
        g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                                 text = paste0("Day ",days_since_case100, "\n", region, ": ",outcome))) +
            xlab("Days since 100th confirmed case")
    }
    
    if (start_point=="Day of 10th death") {
        cv_cases = subset(cv_cases, days_since_death10>0)
        g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                                 text = paste0("Day ",days_since_death10, "\n", region, ": ",outcome))) +
            xlab("Days since 10th death")
    }
    
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
        ylab("cumulative (log10)") + theme_bw() +
        scale_y_continuous(trans="log10") +
        scale_colour_manual(values=country_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
    cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)
cv_cases$deathsper100k = as.numeric(format(round(cv_cases$deaths/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newdeathsper100k = as.numeric(format(round(cv_cases$new_deaths/(cv_cases$population/100000),1),nsmall=1))

# add variable for days since 100th case and 10th death
cv_cases$days_since_case100 = cv_cases$days_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
    country_name = as.character(unique(cv_cases$country))[i]
    country_db = subset(cv_cases, country==country_name)
    country_db$days_since_case100[country_db$cases>=100] = 0:(sum(country_db$cases>=100)-1)
    country_db$days_since_death10[country_db$deaths>=10] = 0:(sum(country_db$deaths>=10)-1)
    cv_cases$days_since_case100[cv_cases$country==country_name] = country_db$days_since_case100
    cv_cases$days_since_death10[cv_cases$country==country_name] = country_db$days_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset of state data for today's data
if (any(grepl("/", cv_states$date))) { 
    cv_states$date = format(as.Date(cv_states$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_states$date = as.Date(cv_states$date, format="%Y-%m-%d") }
cv_states_today = subset(cv_states, date==max(cv_states$date))

# create subset for countries with at least 1000 cases
cv_today_reduced = subset(cv_today, cases>=1000)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
                                per100k, newper100k, activeper100k,
                                deathsper100k, newdeathsper100k,
                                days_since_case100, days_since_death10)), "input_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for days since 100th case and 10th death
cv_cases_continent$days_since_case100 = cv_cases_continent$days_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
    continent_name = as.character(unique(cv_cases_continent$continent))[i]
    continent_db = subset(cv_cases_continent, continent==continent_name)
    continent_db$days_since_case100[continent_db$cases>=100] = 0:(sum(continent_db$cases>=100)-1)
    continent_db$days_since_death10[continent_db$deaths>=10] = 0:(sum(continent_db$deaths>=10)-1)
    cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
    cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
}

# add continent populations
cv_cases_continent$pop = NA
cv_cases_continent$pop[cv_cases_continent$continent=="Africa"] = 1.2e9
cv_cases_continent$pop[cv_cases_continent$continent=="Asia"] = 4.5e9
cv_cases_continent$pop[cv_cases_continent$continent=="Europe"] = 7.4e8
cv_cases_continent$pop[cv_cases_continent$continent=="North America"] = 5.8e8
cv_cases_continent$pop[cv_cases_continent$continent=="Oceania"] = 3.8e7
cv_cases_continent$pop[cv_cases_continent$continent=="South America"] = 4.2e8

# add normalised counts
cv_cases_continent$per100k =  as.numeric(format(round(cv_cases_continent$cases/(cv_cases_continent$pop/100000),1),nsmall=1))
cv_cases_continent$newper100k =  as.numeric(format(round(cv_cases_continent$new_cases/(cv_cases_continent$pop/100000),1),nsmall=1))
cv_cases_continent$deathsper100k =  as.numeric(format(round(cv_cases_continent$deaths/(cv_cases_continent$pop/100000),1),nsmall=1))
cv_cases_continent$newdeathsper100k =  as.numeric(format(round(cv_cases_continent$new_deaths/(cv_cases_continent$pop/100000),1),nsmall=1))
write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$days_since_case100 = cv_cases_global$days_since_death10 = 0:(nrow(cv_cases_global)-1)

# add normalised counts
cv_cases_global$pop = 7.6e9
cv_cases_global$per100k =  as.numeric(format(round(cv_cases_global$cases/(cv_cases_global$pop/100000),1),nsmall=1))
cv_cases_global$newper100k =  as.numeric(format(round(cv_cases_global$new_cases/(cv_cases_global$pop/100000),1),nsmall=1))
cv_cases_global$deathsper100k =  as.numeric(format(round(cv_cases_global$deaths/(cv_cases_global$pop/100000),1),nsmall=1))
cv_cases_global$newdeathsper100k =  as.numeric(format(round(cv_cases_global$new_deaths/(cv_cases_global$pop/100000),1),nsmall=1))
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(cv_large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,1,5,10,50,100,Inf)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
    addTiles() %>% 
    addLayersControl(
        position = "bottomright",
        overlayGroups = c("2019-COVID (new)", "2019-COVID (cumulative)", "2019-COVID (active)", "2019-COVID (recovered)"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("2019-COVID (cumulative)","2019-COVID (active)", "2019-COVID (recovered)")) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-50,~80,80) %>%
    addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$deathsper100k,
              title = "<small>Deaths per 100,000</small>") 

#fitBounds(0,-25,90,65) # alternative coordinates for closer zoom

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
    if (i==1) { cv_aggregated$new[i] = 0 }
    if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)), as.character(unique(cv_states$state)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names



### SHINY UI ###
ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("slate"), collapsible = TRUE,
               "COVID-19 tracker", id="nav",
               
               tabPanel("COVID-19 mapper",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mymap", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, left = 55, width = 250, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          
                                          span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                          h3(textOutput("reactive_case_count"), align = "right", style="color:#cc4c02"),
                                          h4(textOutput("reactive_death_count"), align = "right"),
                                          span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                          span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#0282cc"),
                                          h6(textOutput("clean_date_reactive"), align = "right"),
                                          h6(textOutput("reactive_country_count"), align = "right"),
                                          plotOutput("cumulative_plot", height="130px", width="100%"),
                                          
                                          sliderInput("plot_date",
                                                      label = h5("Select mapping date"),
                                                      min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                      max = as.Date(current_date,"%Y-%m-%d"),
                                                      value = as.Date(current_date),
                                                      timeFormat = "%d %b", 
                                                      animate=animationOptions(interval = 3000, loop = FALSE))
                            ),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                          actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                       onclick = sprintf("window.open('%s')", 
                                                                         "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                            
                            
                        )
               ),
               
               tabPanel("Region plots",
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
                                
                                pickerInput("level_select", "Level:",   
                                            choices = c("Global", "Continent", "Country", "US state"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select", "Country/Region:",   
                                            choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country)[1:10],
                                            multiple = TRUE), 
                                
                                pickerInput("outcome_select", "Outcome:",   
                                            choices = c("Deaths per 100,000", "Cases per 100,000", "Cases (total)", "Deaths (total)"), 
                                            selected = c("Deaths per 100,000"),
                                            multiple = FALSE),
                                
                                pickerInput("start_date", "Plotting start date:",   
                                            choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"), 
                                            options = list(`actions-box` = TRUE),
                                            selected = "Date",
                                            multiple = FALSE), 
                                
                                sliderInput("minimum_date",
                                            "Minimum date:",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value=as.Date(cv_min_date),
                                            timeFormat="%d %b"),
                                
                                "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 1000 confirmed cases are included."
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                                    tabPanel("New", plotlyOutput("country_plot")),
                                    tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
                                )
                            )
                        )
               ),
               
                              tabPanel("Data",
                        numericInput("maxrows", "Rows to show", 25),
                        verbatimTextOutput("rawtable"),
                        downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                        "Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                           "Johns Hopkins Center for Systems Science and Engineering.")
               ),
               
               tabPanel("About this site",
                        tags$div(
                            tags$h4("Last update"), 
                            h6(paste0(update)),
                            "This site is updated once daily. At this time of rapid escalation of the COVID-19 pandemic, the following resources offer the latest numbers of known cases:",tags$br(),
                            tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                            tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),tags$br(),
                            "The aim of this site is to complement the above resources by providing several interactive features like the timeline function",tags$br(),
                            tags$br(),tags$h4("Background"), 
                            "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                        These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                        The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                        This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                            tags$br(),tags$br(),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/vedant-naik99/Covid-Tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("The shiny app was built referring to the code of Dr Edward Parker and Quentin Leclerc: "), tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),tags$br(),
                            tags$b("Country mapping coordinates: "), tags$a(href="https://github.com/martynafford/natural-earth-geojson", "Martyn Afford's Github repository"),tags$br(),
                            tags$br(),tags$br(),
                            tags$h4("This site was made by: "),tags$a(href="https://github.com/vedant-naik99/Covid-Tracker", "Vedant Naik"),
                            tags$br(),
                        )
               )
               
    )          
)



### SHINY SERVER ###

server = function(input, output, session) {
    
    # covid tab 
    output$clean_date_reactive <- renderText({
        format(as.POSIXct(input$plot_date),"%d %B %Y")
    })
    
    reactive_db = reactive({
        cv_cases %>% filter(date == input$plot_date)
    })
    
    reactive_db_last24h = reactive({
        cv_cases %>% filter(date == input$plot_date & new_cases>0)
    })
    
    reactive_db_large = reactive({
        large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        #large_countries = reactive %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
        large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
        large_countries
    })
    
    reactive_db_large_last24h = reactive({
        large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        large_countries = large_countries[order(large_countries$alpha3),]
        large_countries
    })
    
    reactive_polygons = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
    })
    
    reactive_polygons_last24h = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large_last24h()$alpha3, ]
    })
    
    output$reactive_case_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
    })
    
    output$reactive_death_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$deaths), big.mark=","), " deaths")
    })
    
    output$reactive_recovered_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " recovered")
    })
    
    output$reactive_active_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " active cases")
    })
    
    output$reactive_country_count <- renderText({
        paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
    })
    
    output$reactive_new_cases_24h <- renderText({
        paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
    })
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$plot_date, {
        leafletProxy("mymap") %>% 
            clearMarkers() %>%
            clearShapes() %>%
            
            addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                             label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g<br/>Deaths per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths, reactive_db()$per100k, reactive_db()$deathsper100k) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%  
            
            addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deathsper100k)) %>%
            
            addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                             label = sprintf("<strong>%s (past 24h)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g<br/>Deaths per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$newper100k, reactive_db_last24h()$newdeathsper100k) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%
            
             addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
                              fillOpacity = 0.1, color = covid_active, group = "2019-COVID (active)",
                              label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                              labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_active),
                                textsize = "15px", direction = "auto")) %>% 
             
             addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(recovered_cases)^(1/5), 
                             fillOpacity = 0.1, color = covid_recovered, group = "2019-COVID (recovered)",
                             label = sprintf("<strong>%s (recovered)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$recovered_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                               style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_recovered),
                               textsize = "15px", direction = "auto")) 
        
                })
    
    output$cumulative_plot <- renderPlot({
        cumulative_plot(cv_aggregated, input$plot_date)
    })
    
    
    
    
    # add note for cfr
    output$epi_notes_3 <- renderText({
        if(input$comparison_metric=="cfr") { 
            paste0("For COVID-19, this displays the proportion of confirmed cases who have subsequently died. When factoring in mild or asymptomatic infections that are not picked up by case surveillance efforts, current estimates place the case fatality rate in the range of 0.3-1%.")
        }
    })
    
    # update region selections
    observeEvent(input$level_select, {
        if (input$level_select=="Global") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = "Global", selected = "Global")
        }
        
        if (input$level_select=="Continent") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                              selected = c("Africa", "Asia", "Europe", "North America", "South America"))
        }
        
        if (input$level_select=="US state") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = as.character(cv_states_today[order(-cv_states_today$cases),]$state), 
                              selected = cv_states_today$state)
        }
        
        if (input$level_select=="Country") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
                              selected = cv_today_reduced$country)
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db = reactive({
        if (input$level_select=="Global") { 
            db = cv_cases_global
            db$region = db$global_level
        }
        if (input$level_select=="Continent") { 
            db = cv_cases_continent 
            db$region = db$continent
        }
        if (input$level_select=="Country") { 
            db = cv_cases
            db$region = db$country
        }
        if (input$level_select=="US state") { 
            db = cv_states
            db$region = db$state
        }
        
        if (input$outcome_select=="Cases (total)") { 
            db$outcome = db$cases
            db$new_outcome = db$new_cases
        }
        
        if (input$outcome_select=="Deaths (total)") { 
            db$outcome = db$deaths 
            db$new_outcome = db$new_deaths 
        }
        
        if (input$outcome_select=="Cases per 100,000") { 
            db$outcome = db$per100k 
            db$new_outcome = db$newper100k 
        }
        
        if (input$outcome_select=="Deaths per 100,000") { 
            db$outcome = db$deathsper100k 
            db$new_outcome = db$newdeathsper100k 
        }
        
        db %>% filter(region %in% input$region_select)
    })
    
    # country-specific plots
    output$country_plot <- renderPlotly({
        country_cases_plot(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative <- renderPlotly({
        country_cases_cumulative(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative_log <- renderPlotly({
        country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # output to download data
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste("COVID_data_", cv_today$date[1], ".csv", sep="")
        },
        content = function(file) {
            write.csv(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                            recovered, new_recovered, active_cases, 
                                            per100k, newper100k, activeper100k, deathsper100k, newdeathsper100k)), file)
        }
    )
    
    output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                         recovered, new_recovered, active_cases, 
                                         per100k, newper100k, activeper100k, deathsper100k, newdeathsper100k)), input$maxrows), row.names = FALSE)
        options(orig)
    })
    
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
