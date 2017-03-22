library(shinydashboard)
library(shiny)
library(shinyjs)
library(leaflet)
library(ggplot2)
library(plotly)
library(scales)
library(gapminder)
library(dplyr)
library(highcharter)


server <- function(input, output) {

  output$year<-renderText({
    paste('The year of ',input$slider)
  })
  output$aspect<-renderText({
    if(input$select1=='increase_rate'){
      paste('Trend of Rate')
    }else{
      paste('Trend of ',input$select1)
    }
  })

  filteredData<-reactive({
    origin[origin$year==input$slider,]
  })
  sclocation<-reactive({
    schoollocation[schoollocation$year==input$slider,]
  })
  pal<-reactive({
    colorFactor(topo.colors(10),seq(1,10))
  })
  
  proxy<-reactive({
    factpal<-pal()
    leaflet(filteredData())%>%
      addTiles()%>%
      fitBounds(~-150,~-45,~175,~75)%>%
      addCircleMarkers(~long,~lat,
                       stroke=FALSE,
                       radius=~proportion*1.9,
                       fillOpacity = .8,
                       popup=~paste('Origin: ',ori,
                                    '<br/>',
                                    'Rank: ',rank,
                                    '<br/>',
                                    'Amount: ',amount,
                                    '<br/>',
                                    'Proportion: ',proportion,'%'),
                       color=~factpal(rank))%>%
      addMarkers(data=sclocation(),
                       clusterOptions=markerClusterOptions(),
                       popup=paste('School: ',sclocation()$institution,
                                   '<br/>',
                                   'Rank: ',sclocation()$rank,
                                   '<br/>',
                                   'Amount: ',sclocation()$amount,
                                   '<br/>',
                                   'State: ',sclocation()$state,
                                   '<br/>',
                                   'City: ',sclocation()$city),
                       icon=icons(iconUrl=sclocation()$icon,
                                     iconWidth = 70, iconHeight = 70))
    
  })
  
  output$mapcircle<-renderLeaflet({
    factpal<-pal()
    if(input$legend){
      proxy()%>%addLegend(position='bottomright',
                          pal=factpal,value=~rank)
    }else{proxy()}
  })


  p<-reactive({
    ggplot(origin,aes(x=as.factor(year),
                      y=origin[,input$select1],
                      text = paste("Country:",ori,
                                   '<br>',
                                   'y: ',origin[,input$select1])
                      ))+
      scale_y_continuous(labels = comma)+
      theme(legend.title=element_blank(),
            panel.background = element_rect(fill = "#fddbda",
                                            size = 0.5,
                                            linetype = "solid"),
            # legend.position="center",
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.6,0,1,1.5),'cm'),
            legend.background = element_rect(fill='#f2f0cf'),
            plot.background = element_rect(fill= "orange"))+
      guides(fill = guide_legend(keywidth = 2, keyheight = 1
      ))
  })
  output$trends<-renderPlotly({
    # p<-ggplot(origin,aes(as.factor(year),origin[,input$select1]))+
    #   geom_line(aes(group=ori,color=ori))+
    #   scale_colour_hue(c=100)+
    #   geom_point(aes(color=ori))+
    #   scale_y_continuous(
    #                      labels = comma)+
    #   theme(legend.title=element_blank(),
    #         legend.position="center",
    #         axis.title.x=element_blank(),
    #         axis.title.y=element_blank(),
    #         plot.margin=unit(c(.6,0,1,1.5),'cm'),
    #         legend.background = element_rect(fill='#f2f0cf'),
    #         plot.background = element_rect(fill= "orange"))+
    #   guides(fill = guide_legend(keywidth = 2, keyheight = 1
    # ))
    g<-p()+
      geom_line(aes(group=ori,color=ori))+
      scale_colour_hue(c=100)+
      geom_point(aes(color=ori))
    
    ggplotly(g, tooltip = c("text"))
    # gg <- plotly_build(g)
    # gg$x$data[[1]]$text <- paste('Country: ',origin$ori)
    # gg
    })
    
    hc<-reactive({
      # f_d<-data_frame(name=field1$fs,
      #                 y=field1$amo,
      #                 drilldown=field1$drilldown)
      # ds <- list_parse(f_d)
      # names(ds) <- NULL

      highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_xAxis(type = "category") %>% 
        hc_legend(enabled = FALSE) %>% 
        hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE)
          )
        )%>%hc_add_theme(hc_theme_flat()) %>% 
        hc_add_series(
          name= "amount",
          colorByPoint = TRUE,
          data = ds)
    })
    
  hc_sub<-reactive({
    # for (t in ts()) {
    #   assign(paste0("an_", t),data_frame(
    #     name = as.character(dr()[dr()$type==t,]$fs),
    #     value = dr()[dr()$type==t,]$amount))
    # } 
    hc()%>%hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list(
        list(
          id = "agriculture",
          data = list_parse2(an_agriculture),
          name='amount',
          colorByPoint=TRUE
        ),
        list(
          id='busi',
          data=list_parse2(an_busi),
          name='amount',
          colorByPoint=TRUE
        ),
        list(id='communication',
             data=list_parse2(an_communication),
             name='amount',
             colorByPoint=TRUE),
        list(id='education',
             data=list_parse2(an_education),
             name='amount',
             colorByPoint=TRUE),
        list(id='engineer',
             data=list_parse2(an_engineer),
             name='amount',
             colorByPoint=TRUE),
        list(id='art',
             data=list_parse2(an_art),
             name='amount',
             colorByPoint=TRUE),
        list(id='health',
             data=list_parse2(an_health),
             name='amount',
             colorByPoint=TRUE),
        list(id='human',
             data=list_parse2(an_human),
             name='amount',
             colorByPoint=TRUE),
        list(id='intensive',
             data=list_parse2(an_intensive),
             name='amount',
             colorByPoint=TRUE),
        list(id='legal',
             data=list_parse2(an_legal),
             name='amount',
             colorByPoint=TRUE),
        list(id='mc',
             data=list_parse2(an_mc),
             name='amount',
             colorByPoint=TRUE),
        list(id='pl',
             data=list_parse2(an_pl),
             name='amount',
             colorByPoint=TRUE),
        list(id='ss',
             data=list_parse2(an_ss),
             name='amount',
             colorByPoint=TRUE)
      )
    )
  })
    hc_ori<-reactive({
        hc()%>%hc_drilldown(
          allowPointDrilldown = TRUE,
          series = list(
            list(
              id='busi',
              data=list_parse2(subori_busi),
              name='amount',
              colorByPoint=TRUE
            ),
            list(id='education',
                 data=list_parse2(subori_education),
                 name='amount',
                 colorByPoint=TRUE),
            list(id='engineer',
                 data=list_parse2(subori_engineer),
                 name='amount',
                 colorByPoint=TRUE),
            list(id='art',
                 data=list_parse2(subori_art),
                 name='amount',
                 colorByPoint=TRUE),
            list(id='health',
                 data=list_parse2(subori_health),
                 name='amount',
                 colorByPoint=TRUE),
            list(id='human',
                 data=list_parse2(subori_human),
                 name='amount',
                 colorByPoint=TRUE),
            list(id='intensive',
                 data=list_parse2(subori_intensive),
                 name='amount',
                 colorByPoint=TRUE),
            list(id='mc',
                 data=list_parse2(subori_mc),
                 name='amount',
                 colorByPoint=TRUE),
            list(id='pl',
                 data=list_parse2(subori_pl),
                 name='amount',
                 colorByPoint=TRUE),
            list(id='ss',
                 data=list_parse2(subori_ss),
                 name='amount',
                 colorByPoint=TRUE)
          )
        )
    })
    output$plot1<-renderHighchart({
      if(input$select2==1){
        hc_sub()
      }else{
        hc_ori()
      }
    })  
    
    school<-reactive({
      schoollocation[schoollocation$year==input$select3,]
    })
    output$plot2<-renderHighchart({
        highchart() %>% 
         hc_chart(type = "column") %>% 
         hc_xAxis(categories =school()$institution,
                  labels=list(rotation=-90)) %>% 
         hc_add_series(data=school()$amount,
                       colorByPoint=TRUE,
                       name='amount')%>%
         hc_legend(enabled=FALSE)%>%
         hc_add_theme(hc_theme_flat())
    })

    le<-reactive({
      level[level$year==input$select3,]
    })    
    output$plot3<-renderHighchart({
      #   highchart() %>% 
      #     hc_chart(type = "pie") %>% 
      #     hc_title(text = "A highcharter chart") %>% 
      #     hc_xAxis(categories = 2012:2016) %>% 
      #     hc_add_series(data = c(3900,  4200,  5700,  8500, 11900),
      #                   name = "Downloads")
      # 
      highchart() %>% 
        hc_chart(type = "pie",
                 marginTop=110,
                 marginBottom=0) %>% 
        hc_add_series_labels_values(le()$level,
                                    le()$amount,
                                    name='amount')%>% 
        hc_add_theme(hc_theme_flat())
    })
    
    mod<-reactive({
      mo[mo$year==input$model_year,]
    })
    
    glminfo<-reactive({
      glm(mod()$amount~mod()[,input$xcol])
    })
    lminfo<-reactive({
      lm(mod()$amount~mod()[,input$xcol])
    })
    loessinfo<-reactive({
      # loess(mtcars[,input$ycol]~mtcars$mpg)
      loess(mod()$amount~mod()[,input$xcol])
    })
    output$model_info<-renderText({
      if(input$fit=='lm'){
        paste0('Amount=',
               round(lminfo()$coefficients[[2]],3),
               '*',input$xcol,
               '+(',round(lminfo()$coefficients[[1]],3),
               ')')
        
      }else if(input$fit=='glm'){
        paste0('Amount=',
               round(glminfo()$coefficients[[2]],3),
               '*',input$xcol,'+(',
               round(glminfo()$coefficients[[1]],3),
               ')')
        
      }else{
        return('No equation for loess model')
      }             
    })
    
    output$summary<-renderPrint({
      if(input$fit=='lm'){
        summary(lminfo())
      }else if(input$fit=='glm'){
        summary(glminfo())
      }else{
        summary(loessinfo())
      }
    })
    
    output$measurement<-renderText({
      if(input$fit=='lm'){
        paste('AIC: ',round(AIC(lminfo()),3),
               '_Adjusted R-Squared: ',
              round(summary(lminfo())$adj.r.squared,3))
      }else if(input$fit=='glm'){
        paste0('AIC: ',round(AIC(glminfo()),4))
      }else{
        paste0('No AIC for LOESS')
      }
    })
 
    output$model<-renderPlotly({
      
      # p <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec,
      #   color = ~am, colors = c('#BF382A', '#0C4B8E')) %>%
      #   add_markers()
      # p
      p<-ggplot(mod(),aes(x=mod()[,input$xcol],
                          y=amount),
                text1 = paste("Country:",place,
                              '<br>',
                              'y: ',mod()[,input$xcol]))+
        geom_point(aes(size=amount,
                       color=as.character(rank)))+
        geom_smooth(method = input$fit,
                    se=input$band,
                    colour='navy',
                    fill='pink',
                    size=.5)+
        labs(x=input$xcol,y="Amount of International student")+        
        theme(panel.background = element_rect(fill = "white",
                                              # colour = "lightblue",
                                              size = 0.5,
                                              linetype = "solid"),
              panel.grid.major = element_line(colour = "gold"),
              # axis.title.y='Growth rate of the international student(%)',
              plot.margin=unit(c(.6,.5,1,1.5),'cm'),
              legend.position='none')
      ggplotly(p)
    })
    
    uni<-reactive({
      plot_ly(mo, x = ~population, color = I("black")) %>%
        add_markers(y = ~amount, 
                    text = mo$place, showlegend = FALSE) %>%
        add_lines(y = ~fitted(lm(amount~ population)),
                  line = list(color = '#07A4B5'),
                  name = "LM Smoother",
                  showlegend = TRUE)%>%
        layout(xaxis = list(title = 'Population'),
               yaxis = list(title = 'Amount of International student'),
               legend = list(x = 0.80, y = 0.90))
    })
    
    muv<-reactive({
      plot_ly(mo, x = ~population,
                 y = ~amount, z =~gdp,
                 color =~as.character(rank)) %>%
        add_markers()
    })
    output$fm<-renderPlotly({
      if(input$tv==FALSE){
        uni()
      }else{
        muv()
      }
    })

    finalmodel<-reactive({
      lm(mo$rate~mo$gdp_rate)
    })
    tvs<-reactive({
      lm(mo$amount~mo$population+mo$gdp)
    })
    output$final_summary<-renderPrint({
      if(input$tv==FALSE){
        summary(finalmodel())
      }else{
        summary(tvs())
      }
    })
    
    output$myschool<-renderDataTable({
      schoollocation
    })
    output$mylevel<-renderDataTable({
      level
    })
    output$mymd<-renderDataTable({
      mo
    })

}