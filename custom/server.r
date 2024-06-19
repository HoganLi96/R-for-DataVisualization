#Add color arguments
plot_choropleth <- function(df, attr, plot_title, legend_title) {
  fig <- plot_ly(
    df,
    type = 'choropleth',
    locations = df$CODE,
    z =  attr,
    text = df$CODE,
    #check
    frame = ~ Year#,
    #colorscale = list(c(22, 83), c("#f9ca8a", '#bd5868'))
  ) %>%
    layout(
      title = plot_title,
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      #fig_bgcolor = "rgb(255, 255, 255)", 
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      geo = list(
        scope = "world",
        showocean = TRUE,
        oceancolor = "#abd3df",
        showland = TRUE,
        landcolor = "#f1efe8"
      )
    ) %>%
    colorbar(title = legend_title) 
  
  return(fig)
}

plot_line <- 
  function ( df, attr1, attr2, plot_title,legend_title,xtitle, ytitle){
    fig <- plot_ly(df()) %>%
      add_lines(
        x=~attr1,
        y=~attr2,
        frame=~year,
        color =~factor(region),
        line=list(simplify=F)) %>%
      layout(
        title=plot_title,
        xaxis=list(title=xtitle),
        yaxis=list(title=ytitle),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)'
      )
    return(fig)
  }

plot_lineYear <- 
  function ( df, attr1, attr2, plot_title,legend_title,xtitle, ytitle){
    fig <- plot_ly(df) %>%
      add_lines(
        x=~year,
        y=~attr2,
        frame=NULL,
        color =~factor(continent),
        line=list(simplify=F)) %>%
      layout(
        title=plot_title,
        xaxis=list(title=xtitle),
        yaxis=list(title=ytitle),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)'
      )
    return(fig)
  }

plot_bubbleChart <- 
  function(df,x,y,color,country,frame, plot_title,xtitle, ytitle,legend_title){
    fig <- plot_ly(df,
                   x = ~x, 
                   y = ~y, 
                   size = ~x, 
                   color = ~color, 
                   frame = ~frame ,
                   text = ~country, 
                   hoverinfo = "text",
                   type = 'scatter',
                   mode = 'markers',
                   fill=~''
                   
    )
    
    fig <- fig %>% layout(
      title = plot_title,
      xaxis = list(title=xtitle,type = "log" ),
      yaxis=list(title=ytitle),
      legend = list(title=list(text=legend_title)),
      paper_bgcolor='rgba(0,0,0,0)',
      plot_bgcolor='rgba(0,0,0,0)'
    )
    return(fig)
  }

plot_bar<-function(df,region,y1,y2, plot_title, legend_title,xtitle, ytitle){
  fig <- plot_ly(df)
  fig <- fig %>% add_bars(
    x = ~region,
    y = ~y1,
    base=0,
    marker = list(
      color = '#bcbd22'
    ),
    name = 'Import'
  )
  fig <- fig %>% add_bars(
    x = ~region,
    y = ~y2,
    base = 0,
    marker = list(
      color = '#e377c2'
    ),
    name = 'Export'
  )
  return(fig)
}

plot_globe<- function(df, attr1, attr2)
{
  line <- list(color = toRGB("#d1d1d1"), width = 0.2)
  geo <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'orthographic'),
    resolution = '100',
    showcountries = TRUE,
    countrycolor = 'black',
    showocean = TRUE,
    oceancolor = '#429ca6',
    bgcolor = '#30353b',
    landcolor= 'grey',
    showland=T)
  
  plot_geo(text = ~attr2, 
           hoverinfo = "text") %>%
    layout(geo = geo,
           title = "World Regions",
           paper_bgcolor='rgba(0,0,0,0)',
           plot_bgcolor='rgba(0,0,0,0)',
           font = list(color = 'white')) %>%
    add_trace(data = df,
              z = attr1,
              color = ~attr1,
              #colors = 'grey',
              #text = attr3,
              locations = attr2#,
              #marker = list(line = line)
    )
}   


plot_densityChart<- function(df,x,y,z,plot_title,xtitle,legend_title, ytitle){
  fig<- ggplot(df, aes(x = x, fill = factor(y))) + 
    geom_density(alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_log10()+
    transition_time(z) +
    labs(title = plot_title,
         x = xtitle,
         y = ytitle,
         fill = NULL)+
    guides(fill=guide_legend(title=legend_title))+
    theme(panel.grid.major.x = element_blank())+
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.background =element_blank())
  
  anim_save("outfile.gif", animate(fig,width=400,height=400,duration = 5, 
                                   fps = 30,bg = 'transparent', renderer = gifski_renderer()))
  list(src = "outfile.gif", contentType = "image/gif")
}



Plot_BoxChart<-function(df,y,color,plot_title,xtitle, ytitle,legend_title){
  fig<- plot_ly(df, y =y, color =color, type = "box")
  fig <- fig %>% layout(
    title = plot_title,
    xaxis = list(title=xtitle),
    yaxis=list(title=ytitle),
    legend = list(title=list(text=legend_title)),
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)'
  )
  return(fig)
}

TimeSeriesPlot<-function(df){
  df11 <- df[, c('Year', 'Trade (% of GDP)')]
  names(df11)[2] <- 'Trade_GDP'
  df11 <- df11 %>% drop_na(Trade_GDP)
  df11 <- ddply(df11,"Year",numcolwise(mean))
  
  ################################################
  #Declaring Y as time series data 
  ################################################
  Y <- ts(df11[,2], start=c(1960), end=c(2018), frequency=1)
  
  ################################################
  #Preliminary analysis
  ################################################
  # Time plot 
  autoplot(Y)+ ggtitle("Time Plot :Trade(% of GDP)")+ ylab("Trade")
  
  ################################################
  # Data has trend. Investigate transformation
  ################################################
  DY <- diff(Y)
  
  # Time plot of differenced data
  autoplot(DY)+ ggtitle("Time Plot :Trade(% of GDP)")+ ylab("Trade")
  
  #Fit on ETS model #Residual - 30.9406
  fit_ets <- ets(Y)
  print(summary(fit_ets))
  checkresiduals(fit_ets)
  
  #Fit on Arima model #Residual - 30.05661
  fit_arima <- auto.arima(Y, d=1, stepwise = FALSE,approximation = FALSE, trace = TRUE)
  print(summary(fit_arima))
  ModelPlot<-checkresiduals(fit_arima)
  ModelPlot
  
  #################
  #Forecast with arima model
  ###################
  forst <- forecast(fit_arima, h= 15)
  Modelplot <- autoplot(forst, include = 60) +
    labs(y = "World Trade",
         x = "Years",
         title = "15 Year Forecast of World Trade Using ARIMA Model") +
    theme_bw()
  return(Modelplot)
}
static_path <- 'D:/main/static'
plot_title <- "Life Expectancy"
legend_title <- "Life Expectancy"
attr <- "lifeExp"
server <- function(input, output, session) {
  # DYNAMIC RENDER RULES ----------------------------------------------------
  df<- read_csv(paste(static_path, "/Merged_processed.csv", sep=""))
  
  observeEvent("", {
    show("social_panel")
    output$pop1<-renderImage({
      return(list(
        src = df,paste(static_path, "/stats.png"),
        contentType = "image/png",
        height=180, width = 250
      ))
    }, deleteFile = TRUE)
    
    globe_data <- df %>% filter(Year == 2007)
    globe_data$region_code<- ifelse(globe_data$region=='Asia',1,
                                    ifelse(globe_data$region=='Europe',
                                           5,
                                           
                                           ifelse(globe_data$region=='Americas',3,ifelse(globe_data$region=='Ocenia',4,2))))
    
    output$globe <-renderPlotly({
      plot_globe(globe_data , globe_data$region_code,globe_data$CODE)
    })
    output$stat1 <- renderText("Stat 1")
    
    df1 <- df %>% 
      group_by(region, Year) %>% 
      summarise(mean_pop = mean(`Unemployment, total (% of total labor force) (national estimate)`, na.rm = TRUE)) 
    pop_df <- df%>% 
      filter(!is.na(region))
    
    output$box_pat2 <- renderPlotly({
      plot_bubbleChart(
        pop_df,log(pop_df$`GDP per capita (current US$)`),
        pop_df$lifeExp,
        pop_df$region,
        pop_df$Country,
        pop_df$Year,
        "GDP per Capita vs Life Expectancy",
        "GDP",
        "Life Expectancy",
        "Region")
    })
    
    output$box_pat <- renderImage({
      df$Year <- as.integer(df$Year) 
      
      df2 <- df %>%  
        select(Country, `pop`, Year, region) %>%  
        group_by(Year)  %>% arrange(Year, -`pop`)  %>%  
        dplyr::mutate(rank = 1:n()) %>%  
        filter(rank <= 10) -> ranked_by_year
      
      my_theme <- theme_classic(base_family = "Times") +
        theme(axis.text.y = element_blank()) +
        theme(axis.ticks.y = element_blank()) +
        theme(axis.line.y = element_blank()) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.background =element_blank())
      
      
      ranked_by_year %>%  
        ggplot() +  
        aes(xmin = 0 ,
            xmax = `pop` / 1000000)+  
        aes(ymin = rank - .45,
            ymax = rank + .45,
            y = rank)+  
        facet_wrap(~Year)+geom_rect(alpha = .7) +  
        aes(fill = region) +  
        scale_fill_viridis_d(option = "magma",  
                             direction = -1) +  
        
        geom_text(col = "gray13",
                  hjust = "right",
                  aes(label = Country),
                  x = -50)+ scale_y_reverse() +  
        labs(fill = NULL) +  
        labs(x = 'Population (millions)') +  
        labs(y = "") +  
        my_theme -> my_plot
      
      my_plot<-my_plot + facet_null() +  
        geom_text(x = 1000 , y = -10,  
                  family = "Times",  
                  aes(label = as.character(Year)),  
                  size = 30, col = "grey18") +  
        aes(group = Country) +  
        transition_time(Year)
      
      anim_save("outfileLine.gif", animate(my_plot,duration = 5, width = 400, height = 400,
                                           fps = 25,bg = 'transparent', renderer = gifski_renderer()))
      list(src = "outfileLine.gif", contentType = "image/gif")
      
    },deleteFile = TRUE)
    
    
    output$box_year <- renderPlotly({
      plot_ly(df1)%>%
        add_lines(
          x=df1$Year,
          y=df1$mean_pop,
          color =factor(df1$region),
          line=list(simplify=F)) %>%
        layout(
          title="Percentage Unemployment of Total Labor Force Over Years",
          xaxis=list(title="Year"),
          yaxis=list(title="Unemployment Percentage"),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)',
          legend = list(title=list(text="Region"))
        )
    })
    
    #########
    
    ##########
    output$box1 <- renderPlotly({plot_choropleth(df,
                                                 log(df$`GDP per capita (current US$)`),
                                                 "GDP Per Capita in US$","Log GDP")})
    output$box2 <- renderImage({
      plot_densityChart(df,df$`Mortality rate, infant (per 1,000 live births)`,
                        df$region,df$Year,
                        "Infant Mortality Rate Over Years","Mortality Rate","","")
    },deleteFile = TRUE)
    
    
    output$box3 <- renderPlotly({
      
      imp_df <- df %>% 
        group_by(region) %>% 
        summarise(imp_mean = mean(imports, na.rm = TRUE),
                  exp_mean = mean(exports, na.rm = TRUE))
      plot_ly(imp_df) %>%
        add_trace(x= ~region,y= ~imp_mean,  name = 'Imports',type= 'bar', color = "#a6e2d1", opacity = 0.7) %>%
        add_trace(x= ~region,y = ~exp_mean, name = 'Exports',type= 'bar', color = "#ffc4ac", opacity = 0.7) %>%
        layout(
          title="Imports and Exports of Countries",
          xaxis=list(title="Region"),
          yaxis=list(title="Percentage of GDP"),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)'
        )
    })
    
    #########
    Green_house<-df %>% 
      filter(Year %in% (2018)) %>%
      group_by(Country) %>%
      summarise(Green_house=mean(`Total greenhouse gas emissions (kt of CO2 equivalent)`,na.rm=T))%>%
      arrange(desc(Green_house))%>%
      slice(1:10)
    co2_df <- df %>%
      filter(Year %in% (2000:2020) & Country %in% (Green_house$Country)) %>%
      select(Country,Year,
             `CO2 emissions from gaseous fuel consumption (% of total)`,
             `CO2 emissions from liquid fuel consumption (% of total)`,
             `CO2 emissions from solid fuel consumption (% of total)`,
             `Renewable energy consumption (% of total final energy consumption)`) %>%
      group_by(Country) %>%
      summarise(co2_gas = mean(`CO2 emissions from gaseous fuel consumption (% of total)`,na.rm=T),
                co2_liquid=mean(`CO2 emissions from liquid fuel consumption (% of total)`,na.rm=T),
                co2_solid=mean(`CO2 emissions from solid fuel consumption (% of total)`,na.rm=T),
                Renewable_Energy=mean(`Renewable energy consumption (% of total final energy consumption)`,na.rm=T)) 
    
    tob_alc <- df %>% 
      filter(Year>=2010) %>% 
      select(Country, region, alcohol_consumption, tobacco_consumption) %>% 
      group_by(Country) %>%
      summarise(mean_alc = mean(alcohol_consumption, na.rm = TRUE))
    countries <- c("United Kingdom", "United States", "France", "India", "Germany",
                   "China", "Ireland", "United Arab Emirates","Costa Rica", 
                   "Korea, Dem. People's Rep.", "Kuwait", "Saudi Arabia")
    tob_alc$alc_z <- round((tob_alc$mean_alc - 
                              mean(tob_alc$mean_alc,  na.rm = TRUE))/sd(tob_alc$mean_alc, na.rm = TRUE), 2) # compute normalized mpg 
    tob_alc$alc_type <- ifelse(tob_alc$alc_z < 0, "below", "above") # above / below avg flag
    tob_alc <- tob_alc[order(tob_alc$alc_z), ] # sort
    tob_alc$Country <- factor(tob_alc$Country , levels = tob_alc$Country ) # convert to factor to retain sor
    
    tob_alc <- tob_alc %>% 
      filter(Country %in% countries)
    colors_3 <- c("#ffc4ac", "#cbeda1", "#fcc1e3")
    #plots
    output$box5 <- renderPlotly({ 
      plot_ly(co2_df) %>%
        add_trace(x= ~Country,y= ~co2_gas,  name = 'Gas',type= 'bar', color = "#fcc1e3", opacity = 0.7) %>%
        add_trace(x= ~Country,y = ~co2_liquid, name = 'Liquid',type= 'bar', color = "#ffc4ac", opacity = 0.7) %>%
        add_trace(x= ~Country,y = ~co2_solid, name = 'Solid',type= 'bar', color = "#cbeda1", opacity = 0.7) %>% 
        add_trace(x= ~Country,y=~Renewable_Energy,name='Renewable_Energy',type='scatter',mode="lines") %>%
        layout(
          title="CO2 Emission of Countries",
          xaxis=list(title="Country"),
          yaxis=list(title="Emission Percentage"),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)'
        )
    })
    cols <- c("#e76682","#7b98ee")
    output$box7 <- renderPlotly({
      ggplotly(
        ggplot(tob_alc, aes(x=Country, y=alc_z, label=alc_z)) +
          geom_bar(stat='identity', aes(fill=alc_type), width=.5)  +
          scale_fill_manual(name="Consumption",
                            labels = c("Above Average", "Below Average"),
                            values = cols) +
          labs(y="Normalised Alcohol Consumption",
               x = "Country",
               title = "Alcohol Consumption of Countries") +
          coord_flip()+
          theme_bw()
      )
    })
    output$box6 <- renderPlotly({
      box_df <- df %>% filter(!is.na(region))
      Plot_BoxChart(box_df,
                    box_df$`Poverty headcount ratio at national poverty lines (% of population)`,
                    box_df$ region,"Poverty Ratio",
                    "Continents","Poverty Headcount","Continents")
    })
    
    ##########
    output$box_los1 <- renderPlot({TimeSeriesPlot(df)})
    # output$box_los2 <- renderPlotly({})
    # output$box_los3 <- renderPlotly({})
    # output$box_los4 <- renderPlotly({})
    ##########
    hide("economic_panel")
    hide("environmental_panel")
    hide("modeling_panel")
  }, once = TRUE)
  
  observeEvent(input$social, {
    show("social_panel")
    hide("environmental_panel")
    hide("economic_panel")
    hide("modeling_panel")
  })
  observeEvent(input$economic, {
    show("economic_panel")
    hide("environmental_panel")
    hide("modeling_panel")
    hide("social_panel")
  })
  observeEvent(input$environmental, {
    show("environmental_panel")
    hide("economic_panel")
    hide("modeling_panel")
    hide("social_panel")
  })
  observeEvent(input$modeling, {
    show("modeling_panel")
    hide("environmental_panel")
    hide("economic_panel")
    hide("social_panel")
  })
  
  observeEvent(input$Range,{
    #print(input$Range[1], input$Range[2])
  })
  
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "patients", style = {
      if (x == "Patients") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "antimicrobials", style = {
      if (x == "Antimicrobial consumption") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "diagnostics", style = {
      if (x == "Diagnostics") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "outcome", style = {
      if (x == "Outcome") {
        paste("warning")
      } else {
        paste("success")
      }
    })
  })
  
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Patients", "Antimicrobial consumption", "Diagnostics", "Outcome"),
                      label = "",
                      selected = x
    )
  }
  
  observeEvent(input$patients, {
    update_all("Patients")
  })
  observeEvent(input$antimicrobials, {
    update_all("Antimicrobial consumption")
  })
  observeEvent(input$diagnostics, {
    update_all("Diagnostics")
  })
  observeEvent(input$outcome, {
    update_all("Outcome")
  })
  
}