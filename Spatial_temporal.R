#Sys.sleep(20)

observeEvent(c(#input$population_New_model,
               #input$number_of_cases_New_model,
               #input$alarm_indicators_New_model,
               input$dat_new_Model),
               #input$nlags,
               #input$other_alarm_indicators_New_model),
             {

#eval(parse(text=readLines("New_model_helper_Functions.R")))
               
#eval(parse(text=readLines("New_model_server_Script.R")))    
               
#load INLA
               #if(!dir.exists(paste0(getwd(),"/INLA"))){
                 #inla_tar<-list.files(getwd(),pattern ='.gz|.zip')
                 #if(str_detect(inla_tar,'.zip')){
                   #unzip(inla_tar)
                   
                 #}else{
                 #untar(inla_tar)
                 
               #}
               
               #}
               
       #pkgload::load_all(paste0(getwd(),"/INLA"),export_all =F)
           
               #INLA::inla.dynload.workaround()
               #inla.binary.install()
       #inla.load()
          

dat_fl<-var_names_New_model()$dat
dat_slider<<-dat_fl
boundary_file<-var_names_New_model()$SHP
shp_data<-boundary_file@data
cat(paste('Boundary file variables:\n'),paste(names(shp_data),sep=','),'\n\n')
                
#paste("boundary file")
#var_names_New_model()$SHP
boundary_file$district<-as.numeric(boundary_file$district)
  
boundary_file_mod<-boundary_file[which(boundary_file$district %in% dat_fl$district),]
  

population<-input$population_New_model
pop.var.dat<-input$population_New_model
alarm_indicators<-input$alarm_indicators_New_model
other_alarm_indicators<-input$other_alarm_indicators_New_model
number_of_cases<-input$number_of_cases_New_model
base_vars<-c("district","year","week")

dist<-dat_fl$district
years.dat<-sort(unique(dat_fl$year))

source("New_model_helper_Functions.R")
## update the UI lag plots
alarm.indicators<<-alarm_indicators

bound_Data_Districts<<-unique(boundary_file$district)

all_slice<-foreach(a=1:length(alarm_indicators))%do% get_fluid_slice_Output(a,'alarm.indicators')

#insertUI(
  #selector = "#new_model_Week_plot_spat",
  #where = "beforeBegin",
  #ui = eval(parse(text=create_input_UI_year2("new_model_Year_plot")))
#)

output$var_Slices_Plots<-renderUI(eval(parse(text=paste('tabsetPanel(',paste(unlist(all_slice),collapse =','),')'))))
output$dist_Input1<-renderUI(eval(parse(text=create_input_UI_district("district_new"))))
output$dist_Input2<-renderUI(eval(parse(text=create_input_UI_district("district_seas"))))

#output$dist_Input3<-renderUI(eval(parse(text=create_input_UI_district("output_dist_validation"))))
#output$Year_Input1<-render_html(eval(parse(text=create_input_UI_district("new_model_Year_validation"))
#reactlogShow()
output$Year_Input2<-renderUI(eval(parse(text=create_input_UI_year2("new_model_Year_plot"))))


output$valid_section<-renderUI(eval(parse(text=validation_tab_Func())))

updateSliderInput(session,"new_model_Year_validation",
                  min=min(var_names_New_model()$dat$year)+1,
                  max=max(var_names_New_model()$dat$year),
                  value =max(var_names_New_model()$dat$year))

cat(paste('\nValidation year Beg ..\n',input$new_model_Year_validation),'\n\n')

#showTab("to_show2",target="Model Validation",select=T)
#updateTabsetPanel(inputId="to_show2",selected ="Model Validation")


#refresh()

#Sys.sleep(200)

## update the sliders with data input
#session<<-getDefaultReactiveDomain()

#slide_cmr <- reactiveValues(all_slider_input_Vals=foreach(a=1:length(alarm_vars),.combine =c) %do% update_slider_vals(a,"alarm.indicators")
#)

#all_slider_input_Vals<-foreach(a=1:length(alarm.indicators),.combine =c) %do% update_slider_vals(a,"alarm.indicators")

#observe(
#eval(parse(text=paste(all_slider_input_Vals,collapse =' \n'))) 
#)


alarm_ind.check<<-alarm_indicators
  


## compute lag of environmental variables

dat_A<-var_names_New_model()$dat %>% 
  dplyr::arrange(district,year,week) %>% 
  dplyr::filter(district %in% boundary_file$district &!week==53)

## balance the observations to compute expected cases correctly

beg.year<-min(dat_A$year)
end.year<-max(dat_A$year)


eval(parse(text=readLines("reactive_Model_script.R")))
showTab("to_show2",target="Model Validation",select=T)
barry<<-Out.Mod()
all_basis_vars<-Out.Mod()$all_basis_vars
alarm_vars<-Out.Mod()$alarm_vars
#stop("alright")
#saveRDS(pred_vals_all,'./output/pred_eval_Cesar_colombia_2020.rds')

#pred_vals_all<-readRDS("./output/pred_eval_2013.rds")
#pred_vals_all<-readRDS("./output/pred_eval_Cesar_colombia_2020.rds")

## compute the endemic channel

covar_to_Plot<-Out.Mod()$covar_to_Plot
names_cov_Plot<-Out.Mod()$names_cov_Plot
cat(paste0('\ncovar_to_Plot:\n'),paste(covar_to_Plot,sep=' '),'\n\n')

sel_var_endemic<-c(base_vars,number_of_cases,population)
#cat(names_cov_Plot,sep=' \n')
#stop("helo")

dat.4.endemic<<-Out.Mod()$data_augmented[,sel_var_endemic]
names(dat.4.endemic)<-c(base_vars,"cases","pop")

covar_to_Plot<-c(number_of_cases,pop.var.dat,Out.Mod()$alarm_indicators)

par_text0<-get_UI_update_d(obj.plots=Out.Mod()$covar_to_Plot,
                           panel_Update="Descriptive Plots" ,
                           shinyOutputType="plotOutput",
                           cols=2,
                           out_ref="descr")

output$new_model_data_descriptive_Plots<-renderUI({
  tagList(eval(par_text0))
})

par_text2<-get_UI_update_d(obj.plots=Out.Mod()$covar_to_Plot,
                           panel_Update="Time_series" ,
                           shinyOutputType="dygraphOutput",
                           cols=1,
                           out_ref="time_series")

output$new_model_Time_series_Plots<-renderUI({
  tagList(eval(par_text2))
})

#other_alarm_indicators

## Descriptive Plots
observeEvent(c(input$district_new,
             input$alarm_indicators_New_model,
             input$other_alarm_indicators),{
  
  if(is.null(input$district_new)){
    district_new<-sort(unique(Out.Mod()$data_augmented$district))[1]
  }else{
    district_new<-input$district_new
    
  }
  data_one<-Out.Mod()$data_augmented %>% 
    dplyr::filter(district==district_new) %>% 
    dplyr::mutate(DIR=(get(number_of_cases)/get(pop.var.dat))*1e5)
  
  #vars_get_summary<-c("weekly_hospitalised_cases","population","meantemperature","rainsum","rhdailymean")
  
  vars_get_summary<-c(number_of_cases,pop.var.dat,Out.Mod()$alarm_indicators)
  var.sum<-c("district","year","week",vars_get_summary)
  
  #cat(vars_get_summary,sep=' \n')
  #stop("helo")
  
 
  
  dat_sum<-data_one[,var.sum]
  
  dat_sum_long<-reshape2::melt(dat_sum,c("district","year","week"))
  
  dat_kl<-dat_sum_long %>% 
    dplyr::group_by(variable,year) %>% 
    dplyr::summarise(.groups="drop",min=min(value,na.rm =T),
                     max=max(value,na.rm =T),
                     mean=mean(value,na.rm =T),
                     median=quantile(value,0.5,na.rm =T),
                     p25=quantile(value,0.25,na.rm =T),
                     p75=quantile(value,0.75,na.rm =T),
                     pctn_missing=paste0(round((sum(is.na(value))/n())*100,1),"%"))%>% 
    
    dplyr::mutate_at(.vars=c("min","max","mean","p25","median","p75"),
                     .funs = function(x) ifelse(x %in% c(Inf,-Inf),NA,x)) %>% 
    dplyr::mutate_at(.vars=c("min","max","mean","p25","median","p75"),
                     .funs = function(x) round(x,1))
  
  names(dat_kl)<-c("Variable","Year","Min","Max","Mean","Median","25th Percentile",
                   "75th Percentile","% Missing")
  
  get_packed_st<-function(x){
    paste0("pack_rows('",x,"'",',',min(which(dat_kl$Variable==x)),',',max(which(dat_kl$Variable==x)),')')
  }
  
  all_kl<-foreach(a=as.character(unique(dat_kl$Variable)),.combine =c)%do% get_packed_st(a)
  
  all_kl_cmd<-paste(c("function() { dat_kl[,-1]","kbl(format='html',caption = paste('District ',unique(dat_sum_long$district)))","kable_styling('striped', full_width = F)",
                      "column_spec(8,background='#94a323')",all_kl),collapse ='%>%\n')
  
  
  all_kl_cmd<-paste(all_kl_cmd,'}\n',collapse ='')
  
  output$new_model_data_descriptives<-eval(parse(text=all_kl_cmd))
  
  ## inlcude the plots
  #vars_get_summary
  
  plot_desc<-function(p){
    
   
    data_n<-data_one[,c("year","week",vars_get_summary[p])] 
    names(data_n)[3]<-'var'
    
    beg.year<-min(data_n$year)
    end.year<-max(data_n$year)
    
    plo1<-ggplot(data=data_n)+
      geom_raster(aes(x=week,y=year,fill=var))+
      scale_fill_gradientn(name =vars_get_summary[p], colours = rev(brewer.pal(11, "RdBu"))) + 
      scale_y_continuous(breaks =beg.year:end.year,expand =c(0,0))+
      scale_x_continuous(breaks=seq(0,52,4),expand =c(0,0))+
      ylab("Year")+
      xlab("Week")+
      theme_bw()+
      ggtitle(paste('District:',unique(data_one$district),'\n',vars_get_summary[p]))+
      theme(legend.position ="bottom")+
      guides(fill=guide_colorbar(title =NULL,
                                 barwidth=grid::unit(10,'cm')))
    list(plo1)
    
  }
  #?guide_colorbar 
  plot_List0<-foreach(a=1:length(vars_get_summary),.combine =c)%do% plot_desc(a)
  cat(paste("Summary variables ::\n"),paste(vars_get_summary,collapse =','),'\n\n')
  #prrrrr<<-plot_List
  ## render Plots in a loop
  i<-1
  for(i in 1:length(vars_get_summary)){
    text_rend0<-paste0('output$descr_plot_',i,'<-renderPlot({
                     print(plot_List0[[',i,']])})'
    )
    
    eval(parse(text=text_rend0))
  }
  
})

all_vars<-c(base_vars,number_of_cases,pop.var.dat,Out.Mod()$alarm_indicators)



dat_Sel<-Out.Mod()$data_augmented[,all_vars]


melted_dat<-reshape2::melt(dat_Sel,base_vars) %>% 
  mutate(year_week=paste0(year,'_',str_pad(week,side ="left",pad =0,width =2))) %>% 
  dplyr::select(district,year_week,variable,value)

wide_for_dygraph<-melted_dat %>% 
  dplyr::group_by(variable,year_week) %>% 
  tidyr::spread(district,value)

dates_s<-seq.Date(as.Date(paste0(beg.year,'-01-01')),
                  as.Date(paste0(end.year,'-12-31')),
                  by='day')



data_Weeks<-data.frame(date=dates_s,
                       year_week=format.Date(dates_s,"%Y_%W"),
                       year=year(dates_s),
                       stringsAsFactors =F,
                       week=week(dates_s)) %>% 
  mutate(Week=str_split_fixed(year_week,pattern ='_',n=2)[,2]) %>% 
  dplyr::filter(as.numeric(Week)%in% 1:52)

weeks.in.data<-dat_A %>% 
  dplyr::mutate(year_week=paste0(year,'-',str_pad(week,side ="left",pad =0,width =2))) 

year_week_S<-data_Weeks %>% dplyr::group_by(year,Week) %>% 
  dplyr::summarise(.groups="drop",date_Beg=min(date)) %>% 
  dplyr::mutate(year_week=format.Date(date_Beg,"%Y-%W"))%>% 
  dplyr::filter(year_week %in% weeks.in.data$year_week)

get_xts_dat<-function(p){
  dat_n<-wide_for_dygraph %>% dplyr::filter(variable==covar_to_Plot[p])
  dat_n<-dat_n[,-2]
  dat_n1<-dat_n[,-1]
  dat_n2<-xts(dat_n1,order.by =as.Date(as.character(year_week_S$date_Beg)),
              frequency=52)
  plo<-dygraph(dat_n2,xlab ="Year week",ylab=Out.Mod()$covar_to_Plot[p]) %>%
    #dyMultiColumn()
    dySeries() %>% 
    dyRangeSelector() %>% 
    dyLegend(show = "follow") %>% 
    dyHighlight(highlightCircleSize =2, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = T)
  aa<-list(plo)
  names(aa)<-Out.Mod()$names_cov_Plot[p]
  aa
}

all_xts_Plots<-foreach(a=1:length(Out.Mod()$covar_to_Plot),.combine =c)%do% get_xts_dat(a)

for(i in 1:length(Out.Mod()$covar_to_Plot)){
  
  text_rend1<-paste0('output$time_series_plot_',i,'<-renderDygraph({
                     all_xts_Plots[[',i,']]})'
  )
  eval(parse(text=text_rend1))
}

par_text1<-get_UI_update_d(obj.plots=Out.Mod()$covar_to_Plot,
                           panel_Update="Spat_Covariate_Plots_new_Model" ,
                           shinyOutputType="leafletOutput",
                           cols=2,
                           out_ref="spat_cov")


output$Spat_Covariate_Plots_new_Model<-renderUI({
  tagList(eval(par_text1))
})

melted_dat_wide<-melted_dat %>% 
  dplyr::group_by(district,variable) %>% 
  tidyr::spread(year_week,value)

p<-1
get_Spatial_poly_dat<-function(p){
  dat_n<-melted_dat_wide %>% dplyr::filter(variable==Out.Mod()$covar_to_Plot[p])
  merge_Poly<-merge(boundary_file,dat_n,by="district",sort=F,all.x=T)
  aa<-list(merge_Poly)
  names(aa)<-Out.Mod()$names_cov_Plot[p]
  aa
}

all_Plot_Poly<-foreach(a=1:length(Out.Mod()$covar_to_Plot),.combine =c)%do% get_Spatial_poly_dat(a)
#plotly_All<<-list(all_Plot_Poly=all_Plot_Poly)
var_p<-Out.Mod()$names_cov_Plot
cat(paste0('\nvar_p:\n'),paste(var_p,sep=' '),'\n\n')


observeEvent(c(input$new_model_Year_plot,
               input$new_model_Week_plot_spat,
               input$alarm_indicators_New_model),
             {
               if(is.null(input$new_model_Year_plot)){
                 new_model_Year_plot<-max(Out.Mod()$data_augmented$year)
               }else{
                 new_model_Year_plot<-input$new_model_Year_plot
                 
               }
               var_p<-Out.Mod()$names_cov_Plot
               
               get_xts_dat<-function(p){
                 dat_n<-wide_for_dygraph %>% dplyr::filter(variable==covar_to_Plot[p])
                 dat_n<-dat_n[,-2]
                 dat_n1<-dat_n[,-1]
                 dat_n2<-xts(dat_n1,order.by =as.Date(as.character(year_week_S$date_Beg)),
                             frequency=52)
                 plo<-dygraph(dat_n2,xlab ="Year week",ylab=Out.Mod()$covar_to_Plot[p]) %>%
                   #dyMultiColumn()
                   dySeries() %>% 
                   dyRangeSelector() %>% 
                   dyLegend(show = "follow") %>% 
                   dyHighlight(highlightCircleSize =2, 
                               highlightSeriesBackgroundAlpha = 0.2,
                               hideOnMouseOut = T)
                 aa<-list(plo)
                 names(aa)<-Out.Mod()$names_cov_Plot[p]
                 aa
               }
               
               all_xts_Plots<-foreach(a=1:length(Out.Mod()$covar_to_Plot),.combine =c)%do% get_xts_dat(a)
               
               for(i in 1:length(Out.Mod()$covar_to_Plot)){
                 
                 text_rend1<-paste0('output$time_series_plot_',i,'<-renderDygraph({
                     all_xts_Plots[[',i,']]})'
                 )
                 eval(parse(text=text_rend1))
               }
               
               par_text1<-get_UI_update_d(obj.plots=Out.Mod()$covar_to_Plot,
                                          panel_Update="Spat_Covariate_Plots_new_Model" ,
                                          shinyOutputType="leafletOutput",
                                          cols=2,
                                          out_ref="spat_cov")
               
               
               output$Spat_Covariate_Plots_new_Model<-renderUI({
                 tagList(eval(par_text1))
               })
               
               melted_dat_wide<-melted_dat %>% 
                 dplyr::group_by(district,variable) %>% 
                 tidyr::spread(year_week,value)
               
               p<-1
               get_Spatial_poly_dat<-function(p){
                 dat_n<-melted_dat_wide %>% dplyr::filter(variable==Out.Mod()$covar_to_Plot[p])
                 merge_Poly<-merge(boundary_file,dat_n,by="district",sort=F,all.x=T)
                 aa<-list(merge_Poly)
                 names(aa)<-Out.Mod()$names_cov_Plot[p]
                 aa
               }
               
               all_Plot_Poly<-foreach(a=1:length(Out.Mod()$covar_to_Plot),.combine =c)%do% get_Spatial_poly_dat(a)
               #print(var_p)
               yr_week<-paste0(new_model_Year_plot,'_',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
               yr_week1<-paste0(new_model_Year_plot,':',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
               
               yr_week_input<-paste0(new_model_Year_plot,":",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
               yr_week_input1<-paste0(new_model_Year_plot,"_",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
               
               
               cat(paste("\nfrom input ::",yr_week_input,'\n\n'))
               
               
               #print(names_Plots_s)
              

               plot_Func<-function(p){
                 #browser()
                 plot_Now<-all_Plot_Poly[[var_p[p]]]
                 week.idx<-which(names(plot_Now)==yr_week)
                 week_slice<-plot_Now[,c("district",yr_week)]
                 
                 lng1<-as.numeric(week_slice@bbox[,1][1])
                 lat1<-as.numeric(week_slice@bbox[,1][2])
                 lng2<-as.numeric(week_slice@bbox[,2][1])
                 lat2<-as.numeric(week_slice@bbox[,2][2])
                 
                 labels <- sprintf(
                   "<strong>%s</strong><br/>%g",
                   week_slice$district, eval(parse(text=paste0("week_slice$`",yr_week,"`")))
                 ) %>% lapply(htmltools::HTML)
                 
                 
                 legend_title<-sprintf(
                   "<strong>%s</strong><br/>%s",
                   var_p[p],yr_week1 
                 ) %>% lapply(htmltools::HTML)
                 
                 id.summ<-str_detect(names(week_slice),"[:number:]+_[:number:]+")
                 
                 dom_comp<-unique(as.numeric(unlist((week_slice[,id.summ]@data))))
                 
                 len.dom<-length(dom_comp)
                 if(len.dom==1){
                   if(is.na(dom_comp)){
                     dom_range<-c(eval(parse(text=paste0("week_slice$`",yr_week,"`"))),1)
                     
                   }else{
                     dom_range<-c(eval(parse(text=paste0("week_slice$`",yr_week,"`"))))
                     
                   }
                 }else{
                   dom_range<-eval(parse(text=paste0("week_slice$`",yr_week,"`")))
                 }
                 pal <- colorNumeric("YlOrRd", 
                                     domain =dom_range,
                                     reverse=F) 
                 plo1<-leaflet(week_slice[,yr_week]) %>% 
                   leaflet::addTiles() %>% 
                   leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
                   leaflet::fitBounds(lng1,lat1,lng2,lat2) %>% 
                   #addPolylines() %>% 
                   leaflet::addPolygons(fillColor = eval(parse(text=paste0("~pal(`",yr_week,"`)"))),
                               color = "black",weight =0.8,
                               dashArray = " ",
                               fillOpacity = 0.9,
                               highlight = highlightOptions(
                                 weight = 5,
                                 color = "green",
                                 dashArray = "2",
                                 fillOpacity = 0.7,
                                 bringToFront = TRUE),
                               label = labels,
                               labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px"),
                                 textsize = "15px",
                                 direction = "auto")) %>% 
                   leaflet::addLegend(pal = pal, values = eval(parse(text=paste0("~`",yr_week,"`"))), 
                             opacity = 0.7, title = legend_title,
                             position = "bottomright") 
                 list(plo1)
                 
               }
               
               plot_List<-foreach(a=1:length(var_p),.combine =c)%do% plot_Func(a)
               #print(var_p)
               #prrrrr<<-plot_List
               ## render Plots in a loop
               i<-1
               
               for(i in 1:length(covar_to_Plot)){
                 text_rend<-paste0('output$spat_cov_plot_',i,'<-renderLeaflet({
                     plot_List[[',i,']]})'
                 )
                 eval(parse(text=text_rend))
               }
               
              Out.Mod()$res_promise %...>% 
                 {
                 res<-.
                 #print(res)
                
                 SIR_dat<-Out.Mod()$data_augmented[,base_vars] %>% 
                   mutate(Fitted_cases=res$summary.fitted.values$mean,
                          year_week=paste0(year,'_',str_pad(week,side ="left",pad =0,width =2)))%>% 
                   dplyr::select(district,year_week,Fitted_cases)
                 
                 SIR_wide<-SIR_dat %>% 
                   dplyr::group_by(district) %>% 
                   tidyr::spread(year_week,Fitted_cases)
                 
                 ##merge to polygons for plotting
                 SIR_Poly<-merge(boundary_file,SIR_wide,by="district",sort=F,all.x=T)
                 
                 #print(var_p)
                 yr_week<-paste0(new_model_Year_plot,'_',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
                 yr_week1<-paste0(new_model_Year_plot,':',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
                 
                 yr_week_input<-paste0(new_model_Year_plot,":",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
                 yr_week_input1<-paste0(new_model_Year_plot,"_",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
                 
                 
                 #print(paste("from input ::",yr_week_input))
                 names_Plots_s<-paste(names(SIR_Poly),collapse =" ")
                 
                 first_pos_SIR<-which(stringr::str_detect(names(SIR_Poly),'[:number:]+_[:number:]+'))[1]
                 
                 first_YR_week<-stringr::str_extract(names(SIR_Poly)[first_pos_SIR],'[:number:]+_[:number:]+')
                 first_YR_week1<-str_replace(first_YR_week,'_',":")
                 
                 #print(names_Plots_s)
                 out_for_Test<<-list(SIR_Poly=SIR_Poly,
                                     all_Plot_Poly=all_Plot_Poly)
                 if(stringr::str_detect(names_Plots_s,yr_week_input1)==FALSE){
                   yr_week1<-first_YR_week1
                   yr_week<-first_YR_week
                   
                 }else{
                   yr_week1<-yr_week_input
                   yr_week<-yr_week_input1
                 }
                 #print(paste("from input ::",yr_week1))
                 #print(paste("from input ::",yr_week))
                 

                 #cat("year week is..",yr_week)
                 
                 output$spat_DIR_new_Model<-renderLeaflet({
                  
                   week_slice<-SIR_Poly[,c("district",yr_week)]

                   lng1<-as.numeric(week_slice@bbox[,1][1])
                   lat1<-as.numeric(week_slice@bbox[,1][2])
                   lng2<-as.numeric(week_slice@bbox[,2][1])
                   lat2<-as.numeric(week_slice@bbox[,2][2])
                   
                   labels <- sprintf(
                     "<strong>%s</strong><br/>%g",
                     week_slice$district, eval(parse(text=paste0("week_slice$`",yr_week,"`")))
                   ) %>% lapply(htmltools::HTML)
                   
                   
                   legend_title<-sprintf(
                     "<strong>%s</strong><br/>%s",
                     "DIR",yr_week1 
                   ) %>% lapply(htmltools::HTML)
                   
                   pal <- colorNumeric("YlOrRd", 
                                       domain =eval(parse(text=paste0("week_slice$`",yr_week,"`"))),
                                       reverse=F)
                   
                   leaflet(week_slice[,yr_week]) %>% 
                     leaflet::addTiles() %>% 
                     leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
                     leaflet::fitBounds(lng1,lat1,lng2,lat2) %>% 
                     #addPolylines() %>% 
                     leaflet::addPolygons(fillColor = eval(parse(text=paste0("~pal(`",yr_week,"`)"))),
                                 color = "black",weight =0.8,
                                 dashArray = " ",
                                 fillOpacity = 0.9,
                                 highlight = highlightOptions(
                                   weight = 5,
                                   color = "green",
                                   dashArray = "2",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE),
                                 label = labels,
                                 labelOptions = labelOptions(
                                   style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "15px",
                                   direction = "auto")) %>% 
                     leaflet::addLegend(pal = pal, values = eval(parse(text=paste0("~`",yr_week,"`"))), 
                               opacity = 0.7, title = legend_title,
                               position = "bottomright") 
                   
                 })
               }
             })

#browser()
observeEvent(c(input$district_seas,
              input$alarm_indicators_New_model,
              input$other_alarm_indicators_New_model),{
  
  if(is.null(input$district_seas)){
    district_seas<-sort(unique(Out.Mod()$data_augmented$district))[1]
  }else{
    district_seas<-input$district_seas
    
  }
  
  Out.Mod()$res_promise %...>% {
    
    res<-.
    weekly_effects <- data.table(cbind(rep(unique(Out.Mod()$data_augmented$district), each = 52),
                                       res$summary.random$T1))
    names(weekly_effects)[1:2] <- c("district", "Week")
    
    weekly_effects_check<<-weekly_effects
    weekly_effects_sub<-weekly_effects %>% 
      dplyr::filter(district ==district_seas)
    #dplyr::filter(district ==23)
    plot.seas<-weekly_effects_sub %>% 
      ggplot() + 
      geom_ribbon(aes(x = Week, ymin = `0.025quant`, ymax = `0.975quant`), 
                  fill = "cadetblue4", alpha = 0.5) + 
      geom_line(aes(x = Week, y = `mean`), col = "cadetblue4") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      #facet_wrap(~district,ncol =4)+
      xlab("Week") +
      ggtitle(paste("District:",district_seas))+
      ylab("Contribution to log(DIR)") +
      scale_y_continuous() +
      scale_x_continuous(breaks = seq(0,52,5)) +
      theme_bw()+
      theme(axis.text =element_text(size=14),
            axis.title =element_text(size=16))
    
    output$Seasonality_plot<-renderPlot({ 
      print(plot.seas)
    })
    
  } 
})







#for_obs<-Out.Mod()$for_obs

#paste0('c(',paste0(for_obs,collapse =','),')')

observeEvent(eval(parse(text=paste0('c(',paste0(Out.Mod()$for_obs,collapse =','),')'))),{
  data.basis<<-Out.Mod()$data_augmented
  alarm_vars_lag<-input$alarm_indicators_New_model
  all_basis_vars<-foreach(a=alarm_vars_lag,.combine =c)%do% get_cross_basis(a,"data.basis")
  basis_var_n<-paste0('all_basis_vars$',names(all_basis_vars))
  alarm.indicators<-input$alarm_indicators_New_model
  dat_slider<<-var_names_New_model()$dat
  #all_slice<-foreach(a=1:length(alarm_vars_lag))%do% get_fluid_slice_Output(a,'alarm.indicators')
  
  #output$var_Slices_Plots<-renderUI(eval(parse(text=paste('tabsetPanel(',paste(unlist(all_slice),collapse =','),')'))))
  
  
  par_lag1<-get_UI_update_d(obj.plots=alarm_vars_lag,
                            panel_Update="Lag non linear plots" ,
                            shinyOutputType="plotOutput",
                            cols=1,
                            out_ref="contour")
  
  
  output$lag_contour_Plots<-renderUI({
    tagList(eval(par_lag1))
  })
  basis_vars_lag<-names(all_basis_vars)
  alarm_vars_lag<-input$alarm_indicators_New_model
  #df1<<-df

Out.Mod()$res_promise %...>% {
  res<-.
  get_lag_plots<-function(p){
    
    
    if(str_detect(alarm_vars_lag[p],'rain|prec|precipitation')){
      ylab.text<-paste(alarm_vars_lag[p],'(mm)')
    }else if(str_detect(alarm_vars_lag[p],'temp')){
      ylab.text<-paste0('"',alarm_vars_lag[p],'"','~(','~degree~C)')
    }else if(str_detect(alarm_vars_lag[p],'RH|rh')){
      ylab.text<-paste(alarm_vars_lag[p],'(%)')
    }else{
      ylab.text<-alarm_vars_lag[p]
    }
    
    temp_var<-str_detect(alarm_vars_lag[p],'temp')
    cat(paste(basis_var_n,'\n\n'))
    model<<-res
    coef <- model$summary.fixed$mean
    vcov <- model$misc$lincomb.derived.covariance.matrix
    indt <- grep(basis_vars_lag[p], model$names.fixed)
    computed_basis<-eval(parse(text=(basis_var_n[p])))
    #assign("computed_basis",eval(parse(text=(basis_var_n[p]))))
 
    centering.val=round(mean(data.basis[,alarm_vars_lag[p]],na.rm=T), 0)
    
    min<-round(min(data.basis[,alarm_vars_lag[p]],na.rm=T),0)
    min.1<-min(data.basis[,alarm_vars_lag[p]],na.rm=T)
    max<-round(max(data.basis[,alarm_vars_lag[p]],na.rm=T),0)
    max.1<-max(data.basis[,alarm_vars_lag[p]],na.rm=T)
    
    val.slid<-pretty(min:max,50)
    
    
    predt.h <- crosspred(computed_basis, coef = coef[indt], vcov=vcov[indt,indt],
                         at=val.slid,
                         model.link = "log", bylag = 0.25, cen =centering.val) 
    
    #dat_plot<-data.frame(x=predt.h$predvar,y=seq(0, nlag, 0.25),z=t(predt.h$matRRfit))
    
    plot_d<-reshape2::melt(predt.h$matRRfit) %>% 
      dplyr::mutate(x=as.numeric(str_remove(Var2,"lag")),
                    y=as.numeric(Var1),
                    RR=value) %>% 
      dplyr::select(x,y,RR)
    
    plot_d_lci<-reshape2::melt(predt.h$matRRlow) %>% 
      dplyr::mutate(x=as.numeric(str_remove(Var2,"lag")),
                    y=as.numeric(Var1),
                    RR_low=value)%>% 
      dplyr::select(x,y,RR_low)
    
    plot_d_hci<-reshape2::melt(predt.h$matRRhigh) %>% 
      dplyr::mutate(x=as.numeric(str_remove(Var2,"lag")),
                    y=as.numeric(Var1),
                    RR_high=value)%>% 
      dplyr::select(x,y,RR_high)
    
    pal <- rev(brewer.pal(11, "PRGn"))
    levels <- pretty(predt.h$matRRfit, 20)
    col1 <- colorRampPalette(pal[1:6])
    col2 <- colorRampPalette(pal[6:11])
    cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
    
    
    lag_slices<-plot_d_lci %>% 
      dplyr::left_join(plot_d,by=c("x","y")) %>% 
      dplyr::left_join(plot_d_hci,by=c("x","y"))
    
    cont_plot<-ggplot(aes(x=x,y=y),data=plot_d)+
      geom_contour_filled(aes(z=RR),
                          breaks=levels)+
      scale_fill_manual(values =cols)+
      theme_bw()+
      guides(fill=guide_coloursteps(reverse =F,
                                    title="RR",
                                    ticks=F,
                                    barheight=grid::unit(10,'cm')))+
      theme(panel.grid =element_blank(),
            axis.title =element_text(size=16),
            axis.text =element_text(size=14))+
      scale_x_continuous(breaks=0:input$nlags,
                         expand =c(0,0))+
      scale_y_continuous(breaks=pretty(plot_d$y,15),
                         expand =c(0,0))+
      
      xlab("Lag Weeks")+
      if(temp_var){
        ylab(eval(parse(text=ylab.text)))
      }else{
        ylab(ylab.text)
      }
    
    pout<-list(cont_plot,lag_slices)
    names(pout)<-c(paste0("ContPlot_",alarm_vars_lag[p]),paste0("datLagSlices_",alarm_vars_lag[p]))
    pout
  }
  #browser()
  all_cont_dat_plots<-foreach(a=1:length(Out.Mod()$alarm_vars),.combine =c)%do% get_lag_plots(a)
  all_cont_plots<-all_cont_dat_plots[grep('ContPlot',names(all_cont_dat_plots))]
  all_datLagSlices<-all_cont_dat_plots[grep('datLagSlices',names(all_cont_dat_plots))]
  
  ## send to the output
  #Update  object
  
  
  
  for(i in 1:length(alarm_vars_lag)){
    cont_rend<-paste0('output$contour_plot_',i,'<-renderPlot({
                     all_cont_plots[[',i,']]})'
    )
    eval(parse(text=cont_rend))
  }
  
   
    basis_vars_lag<-names(all_basis_vars)
    alarm_vars_lag<-input$alarm_indicators_New_model
    #cat("it ran oh!!",sep='\n')
    
    
    get_lag_slice_plots<-function(p){
      
      
      
      centering.val<-round(mean(data.basis[,alarm_vars_lag[p]],na.rm=T), 0)
     
      
      if(str_detect(alarm_vars_lag[p],'rain|prec|precipitation')){
        cen.text<-paste0('Reference=',centering.val,'mm')
      }else if(str_detect(alarm_vars_lag[p],'temp')){
        cen.text<-paste0("'Reference='~",centering.val,'~degree~C')
      }else if(str_detect(alarm_vars_lag[p],'RH|rh')){
        cen.text<-paste0('Reference=',centering.val,'%')
      }else{
        cen.text<-paste0('Reference=',centering.val)
      }
      
      temp_var<-str_detect(alarm_vars_lag[p],'temp')
      
      ## get values to choose
      
      ad.v<-paste0('input$',alarm_vars_lag[p],'_',1:3)
      
      cat(paste("selected lag values ..\n\n"),eval(parse(text=paste0('c(',paste0(ad.v,collapse =','),')'))),'\n')
      
      sel_sli_p<-eval(parse(text=paste0('c(',paste0(ad.v,collapse =','),')')))
      
      sel_sli_p_see<<-sel_sli_p
      paste(input$meantemperature_1)
      #dat_plot<-data.frame(x=predt.h$predvar,y=seq(0, nlag, 0.25),z=t(predt.h$matRRfit))
      
      
      lag_slices<-all_datLagSlices[[p]]
      pal <- rev(brewer.pal(11, "PRGn"))
      levels <- pretty(lag_slices$RR, 20)
      col1 <- colorRampPalette(pal[1:6])
      col2 <- colorRampPalette(pal[6:11])
      cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
      
      
      #unique(plot_d$y)
      lag_slices_see<<-lag_slices
      dat_some<-lag_slices %>% 
        dplyr::filter(y%in%  sort(sel_sli_p)) %>% 
        #dplyr::filter(y%in%  sample(lag_slices$y,3)) %>% 
        dplyr::mutate(y_fac=as.factor(y),
                      temp_var=temp_var,
                      ref=cen.text
        )
      
      
      names(dat_some)
      
      min.y_s<-min(c(dat_some$RR_low,dat_some$RR,dat_some$RR_high))
      max.y_s<-max(c(dat_some$RR_low,dat_some$RR,dat_some$RR_high))
      
      
      
      #library(viridis)
      
      slice.p<- ggplot(aes(x=x,y=RR),data=dat_some)+
        geom_line(aes(col=y_fac))+
        geom_ribbon(aes(ymin=RR_low,ymax=RR_high,fill=y_fac),alpha=0.1,
                    show.legend =F)+
        geom_hline(aes(yintercept=1),lty=2,lwd=0.8)+
        facet_wrap(~ref,labeller =if(temp_var){label_parsed}else{label_value})+
        theme_bw()+
        scale_color_manual(values=viridis::plasma(20)[c(2,14,8)] )+
        scale_fill_manual(values=viridis::plasma(20)[c(2,14,8)])+
        scale_x_continuous(breaks=0:input$nlags,
                           expand =c(0,0))+
        scale_y_continuous(limits =c(min.y_s,max.y_s*1.5))+
        xlab("Lag Weeks")+
        ylab("RR")+
        guides(col=guide_legend(title=alarm_vars_lag[p]))+
        theme(panel.grid.minor =element_blank(),
              strip.text =element_text(size=18,face="bold",colour ="black"),
              legend.text =element_text(size=18),
              legend.title =element_text(size=18),
              axis.text =element_text(size=14),
              axis.title =element_text(size=16),
              legend.position ="bottom")
      
      pout<-list(slice.p)
      names(pout)<-alarm_vars_lag[p]
      pout
    }
    
    all_lag_slice_plots<-foreach(a=1:length(basis_vars_lag),.combine =c)%do% get_lag_slice_plots(a)
    #all_lag_slice_plots_check<<-all_lag_slice_plots
    
    
    for(i in 1:length(alarm_vars_lag)){
      out_name<-paste0(alarm_vars_lag[i],'_Slice_plot')
      cont_rend<-paste0('output$',out_name,'<-renderPlot({
                     all_lag_slice_plots[[',i,']]})'
      )
      eval(parse(text=cont_rend))
    }
    
    
    
 # })
}
})


## plot validation data for a district
#browser()
observeEvent(c(input$district_validation,
               input$z_outbreak_new,
               input$new_model_Year_validation),{
                
                 
                 if(is.null(input$new_model_Year_validation)){
                   new_model_Year_validation<-max(Out.Mod()$data_augmented$year)
                 }else{
                   new_model_Year_validation<-input$new_model_Year_validation
                   
                 }
                 if(is.null(input$district_validation)){
                   district_validation<-sort(unique(Out.Mod()$data_augmented$district))[1]
                 }else{
                   district_validation<-input$district_validation
                   
                 }
                 
                 cat(paste('Z_value::',input$z_outbreak_new),'\n\n')
                 #stop("i reached here okay ..") 
                 for_endemic<-Out.Mod()$all_endemic %>% 
                   dplyr::filter(district==district_validation) %>% 
                   dplyr::mutate(threshold_cases=mean_cases+input$z_outbreak_new*(sd_cases),
                                 threshold_rate=mean_rate+input$z_outbreak_new*(sd_rate))
                 
               
                 
                 for_endemic_check<<-for_endemic
                 ## get runin data
                 
                 idx_runin<-which(!Out.Mod()$data_augmented$year>=new_model_Year_validation)
                 
             Out.Mod()$res_promise %...>% 
                   {
                     res<-.
                     
                     runin_dat<-dat.4.endemic %>% 
                       dplyr::filter(!year>=new_model_Year_validation) %>% 
                       dplyr::mutate(observed_cases=cases,
                                     observed_rate=(cases/pop)*1e5,
                                     fitted=res$summary.fitted.values$mean[idx_runin],
                                     fitted=(fitted/pop)*1e5,
                                     fittedp25=res$summary.fitted.values$`0.025quant`[idx_runin],
                                     fittedp25=(fittedp25/pop)*1e5,
                                     fittedp975=res$summary.fitted.values$`0.975quant`[idx_runin],
                                     fittedp975=(fittedp975/pop)*1e5,
                       ) %>% 
                       dplyr::left_join(for_endemic,by=c("district","year","week")) %>% 
                       dplyr::mutate(outbreak=observed_cases>threshold_cases,
                                     observed_alarm=case_when(outbreak==1~observed_rate,
                                                              TRUE~as.numeric(NA))) %>% 
                       dplyr::select(district,year,week,observed_rate,fitted,fittedp25,fittedp975,outbreak,threshold_rate,observed_alarm)
                     
                     
                     
                     cat(paste("district is<<>>",district_validation),'\n')
                     distr_n<-district_validation
                     data_plot_Runin<-runin_dat %>% 
                       dplyr::filter(district==distr_n) 
                     
                     end.runin.year<-new_model_Year_validation-1
                     date_week_runin<-seq.Date(as.Date(paste0(beg.year,'-01-01')),as.Date(paste0(end.runin.year,'-12-31')),by='week')[-1]
                     
                     data_plot_RuninB<-data_plot_Runin %>% 
                       dplyr::select(observed_rate,fitted,fittedp25,
                                     fittedp975,outbreak,
                                     threshold_rate,observed_alarm) %>% 
                       dplyr::rename(observed=observed_rate,
                                     threshold=threshold_rate)
                     
                     data_use_Runin_xts<-xts(data_plot_RuninB,order.by =date_week_runin,
                                             frequency=7)
                     
                     data_plot_RuninB1<-data_plot_Runin %>% 
                       dplyr::mutate(date=date_week_runin) %>% 
                       dplyr::select(date,observed_rate,fitted,fittedp25,
                                     fittedp975,outbreak,
                                     threshold_rate,observed_alarm)%>% 
                       dplyr::rename(observed=observed_rate,
                                     threshold=threshold_rate)
                     
                     data_plot_RuninB1_check<<-data_plot_RuninB1
                     
                     plot.runin<-ggplot(aes(x=date),data=data_plot_RuninB1)+
                       geom_line(aes(x=date,y=observed,col="Observed"),lwd=0.82)+
                       geom_line(aes(x=date,y=fitted,col='Predicted'),lwd=1.2,lty=1)+
                       geom_line(aes(x=date,y=threshold),lwd=1.1,lty=1,col='blue',show.legend =F)+
                       geom_area(aes(x=date,y=threshold,fill='Endemic'),alpha=0.7)+
                       geom_point(aes(x=date,y=observed_alarm,col="Outbreak"),size=3)+
                       geom_ribbon(aes(ymin=fittedp25,ymax=fittedp975,
                                       fill="95 % CI"),alpha=0.3)+
                       scale_color_manual(values=c("Observed"='#00336FFF',
                                                   "Predicted"='#A72197FF',
                                                   #"Endemic"='lightblue2',
                                                   "Outbreak"='orange2'))+
                       scale_fill_manual(values=c("Endemic"='lightblue2',
                                                  "95 % CI"=grey(0.3)))+
                       
                       scale_x_date(date_breaks ="52 weeks")+
                       theme_bw()+
                       guides(col=guide_legend(title =NULL),
                              fill=guide_legend(title =NULL))+
                       ylab("DIR")+
                       theme_bw()
                     
                     output$runin_ggplot_New_model<-renderPlot({
                       print(plot.runin)
                       
                     })
                     output$runin_interactive_New_model<-renderDygraph({
                       dygraph(data_use_Runin_xts,xlab ="Week",ylab="DIR")%>%
                         dySeries("observed",fillGraph=F,color ="grey") %>% 
                         dySeries("fitted",fillGraph=F,color ="blue2",label="Fitted") %>% 
                         dySeries("fittedp25",fillGraph=F,color ="orange2",label="Fitted p2.5") %>% 
                         dySeries("fittedp975",fillGraph=F,color ="orange3",label="Fitted p97.25") %>% 
                         dySeries("threshold",fillGraph=T,color ="purple",label ="Endemic channel") %>% 
                         dySeries("observed_alarm",fillGraph=T,color ="red",label ="Alarm",
                                  drawPoints=T,pointSize =2,pointShape="circle") %>% 
                         dyRangeSelector() %>% 
                         dyLegend(show = "onmouseover") %>% 
                         dyHighlight(highlightCircleSize =2, 
                                     highlightSeriesBackgroundAlpha = 0.2,
                                     hideOnMouseOut = F)
                       
                       
                     })
               }
  #pred_Promise_all<-promises::promise_all(mod_sp=res_promise,pred_eval=pred_vals_all_promise)

Out.Mod()$pred_vals_all_promise %...>% {
    
  pred_vals_all<-.
  #res<-mod_sp %...>%
    
  data_use<-pred_vals_all %>%  
    dplyr::filter(district==district_validation & year>=new_model_Year_validation) 
  
  prob_long<<-reshape2::melt(data_use,c("district","year","week")) %>% 
    dplyr::left_join(for_endemic,by=c("district","year","week")) %>% 
    dplyr::mutate(indicator=as.numeric(value>threshold_cases))
  
  compute_probs<<-prob_long %>% 
    dplyr::group_by(district,year,week) %>% 
    dplyr::summarise(.groups="drop",
                     predicted=mean(value),
                     p25=quantile(value,2.5/100),
                     p50=quantile(value,50/100),
                     p975=quantile(value,97.5/100),
                     Total_obs=n(),
                     prob=sum(indicator)/Total_obs)%>% 
    dplyr::left_join(dat.4.endemic,by=c("district","year","week"))
  
  

  data_use_<<-compute_probs %>% 
    dplyr::left_join(for_endemic,by=c("district","year","week")) %>% 
    dplyr::mutate(observed1=(cases/pop)*1e5,
                  observed=case_when(is.na(observed1)~0,
                                     TRUE~observed1),
                  predicted=(predicted/pop)*1e5,
                  p25=(p25/pop)*1e5,
                  p975=(p975/pop)*1e5) %>% 
    dplyr::mutate(outbreak=cases>threshold_cases,
                  observed_alarm=case_when(outbreak==1~observed,
                                           TRUE~as.numeric(NA)))
  
  
  cat("computed probs \n")
  print(data_use_$prob)
  idx.comp<<-which(!is.na(data_use_$outbreak))
  
  
  roc_try<-try(reportROC(gold=as.numeric(data_use_$outbreak)[idx.comp],
                         predictor=data_use_$prob[idx.comp]),outFile =warning("please.."))
  
  roc_tab_names<-c("Cutoff","AUC","AUC.SE","AUC.low","AUC.up","P","ACC",
                   "ACC.low","ACC.up","SEN","SEN.low","SEN.up",
                   "SPE","SPE.low","SPE.up","PLR","PLR.low",
                   "PLR.up","NLR","NLR.low","NLR.up","PPV",
                   "PPV.low","PPV.up","NPV","NPV.low","NPV.up")
  
  kdd<-data.frame(t(rep(as.character(NA),length(roc_tab_names))))
  names(kdd)<-roc_tab_names
  
  if(class(roc_try) %in% c("NULL","try-error")){
    roc_report<-kdd
  }else{
    roc_report<-reportROC(gold=as.numeric(data_use_$outbreak)[idx.comp],
                          predictor=data_use_$prob[idx.comp])
  }
  
  if(roc_report$Cutoff%in% c(NA,-Inf,NaN,Inf)){
    sens_ppv<-tribble(~var,~val,~CI_Lower,~CI_Upper,
                      "Cutoff probability",roc_report$Cutoff,NA,NA,
                      "Area under the Curve (AUC)",roc_report$AUC,roc_report$AUC.low,roc_report$AUC.up,
                      "Accuracy",roc_report$ACC ,roc_report$ACC.low,roc_report$ACC.up,
                      "Sensitivity",roc_report$SEN,roc_report$SEN.low,roc_report$SEN.up,
                      "Specificity",roc_report$SPE,roc_report$SPE.low,roc_report$SPE.up,
                      "Positive Predictive Value (PPV)",roc_report$PPV,roc_report$PPV.low,roc_report$PPV.up,
                      "Negative Predictive Value (NPV)",roc_report$NPV,roc_report$NPV.low,roc_report$NPV.up)
    
    data_use_a<-data_use_ %>% 
      dplyr::mutate(prob_exceed=probs,
                    cutoff=NA,
                    validation_alarm=as.numeric(NA))
    
  }else{
    sens_ppv<-tribble(~var,~val,~CI_Lower,~CI_Upper,
                      "Cutoff probability",roc_report$Cutoff,NA,NA,
                      "Area under the Curve (AUC)",roc_report$AUC,roc_report$AUC.low,roc_report$AUC.up,
                      "Accuracy",roc_report$ACC ,roc_report$ACC.low,roc_report$ACC.up,
                      "Sensitivity",roc_report$SEN,roc_report$SEN.low,roc_report$SEN.up,
                      "Specificity",roc_report$SPE,roc_report$SPE.low,roc_report$SPE.up,
                      "Positive Predictive Value (PPV)",roc_report$PPV,roc_report$PPV.low,roc_report$PPV.up,
                      "Negative Predictive Value (NPV)",roc_report$NPV,roc_report$NPV.low,roc_report$NPV.up)  
    
    data_use_a<-data_use_ %>% 
      dplyr::mutate(prob_exceed=prob,
                    cutoff=as.numeric(roc_report$Cutoff),
                    validation_alarm=case_when((prob_exceed>=cutoff)~prob_exceed,
                                               TRUE~as.numeric(NA)))
  }
  
  data_test_ggplot<<-data_use_a
  ratio_scale<-max(data_use_a$p975,na.rm =T)/max(data_use_a$prob_exceed,na.rm =T)
  
  data_use_AA<-data_use_a %>% 
    dplyr::mutate(Trend=1:n(),
                  lab_week=paste0(year,'_',str_pad(week,width = 2,side="left",pad='0'))) %>% 
    dplyr::rename(threshold=threshold_rate)
  
  Num_YYears<-length(unique(data_use_AA$year))
  breaks_p<-seq(1,nrow(data_use_AA),4*Num_YYears)
  
  val.plot1<-ggplot(aes(x=Trend),data=data_use_AA)+
    geom_line(aes(x=Trend,y=observed,col="Observed"),lwd=0.82)+
    geom_line(aes(x=Trend,y=predicted,col='Predicted'),lwd=1.2,lty=1)+
    geom_line(aes(x=Trend,y=threshold),lwd=0.5,lty=1,col="blue",show.legend =F)+
    geom_area(aes(x=Trend,y=threshold,fill='Endemic Channel'),alpha=0.6)+
    geom_point(aes(x=Trend,y=observed_alarm,col="Outbreak"),size=3)+
    geom_ribbon(aes(ymin=p25,ymax=p975,
                    fill="95 % CI"),alpha=0.2)+
    geom_point(aes(x=Trend,y=validation_alarm*ratio_scale,col="Alarm"),size=3)+
    geom_line(aes(x=Trend,y=prob_exceed*ratio_scale,col="Excedance Probability"),lwd=1)+
    
    geom_line(aes(x=Trend,y=cutoff*ratio_scale,col="Cutoff Prob"),size=1.2)+
    scale_y_continuous(name = "DIR",sec.axis =sec_axis(~ . /ratio_scale,name="Probability"))+
    scale_x_continuous(breaks=breaks_p,labels =data_use_AA$lab_week[breaks_p])+
                                                         
    scale_color_manual(values=c("Observed"='#00336FFF',
                                "Predicted"='#A72197FF',
                                "Cutoff Prob"="red",
                                "Excedance Probability"="yellowgreen",
                                #"Endemic"='lightblue2',
                                "Outbreak"='orange2',
                                "Alarm"='blue'))+
    scale_fill_manual(values=c("Endemic Channel"='lightblue2',
                               "95 % CI"=grey(0.3)))+
    ggtitle(paste('District:',district_validation))+
    guides(col=guide_legend(title =NULL),
           fill=guide_legend(title =NULL))+
    ylab("DIR")+
    xlab("Year Week")+
    theme_bw()
  
  output$validation_ggplot_New_model<-renderPlot({
    print(val.plot1)
  })
  
  year_VAL<-sort(unique(data_use_AA$year))
  date_week<-seq.Date(as.Date(paste0(min(year_VAL),'-01-01')),as.Date(paste0(max(year_VAL),'-12-31')),by='week')[-1]
  
  data_use_b<-data_use_a %>% 
    dplyr::select(predicted,observed,threshold_rate,
                  cutoff,prob_exceed,validation_alarm)%>% 
    dplyr::rename(threshold=threshold_rate)
  
  data_use_xts<-xts(data_use_b,order.by =date_week,
                    frequency=7)
  
output$validation_interactive_New_model<-renderDygraph({
  dygraph(data_use_xts,xlab ="Week",ylab="DIR")%>%
    dySeries("prob_exceed",col="blue",stepPlot = F,axis="y2",label="Prob exceed") %>% 
    dySeries("cutoff",col="red",stepPlot = F,axis="y2",label="Cutoff") %>% 
    dySeries("observed",fillGraph=F,color ="yellowgreen") %>% 
    dySeries("validation_alarm",fillGraph=F,color ="orange4",drawPoints=T,
             pointSize =2,pointShape ="circle",axis="y2") %>% 
    dySeries("threshold",fillGraph=T,color ="purple",label ="Endemic channel") %>% 
    dyRangeSelector() %>% 
    dyLegend(show = "onmouseover") %>% 
    dyHighlight(highlightCircleSize =2, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = F)
  })
  
##output the sensitivity table


distr_n<-district_validation
sen_spec_cmd<-paste(c("function() { sens_ppv","kbl(format='html',caption = paste('District ',distr_n))","kable_styling('striped', full_width = F)",
                    "column_spec(2,background='#94a323')"),collapse ='%>%\n')


sen_spec_cmd<-paste(sen_spec_cmd,'}\n',collapse ='')

output$sen_spec_table_New_model<-eval(parse(text=sen_spec_cmd))
}
## plot the runin period values

  ##tmap plots
  ##update the plot UI 
  
  
  #print(names_cov_Plot)
  
  ## compute Model outputs
  
  ##plot Lag non linear contours
  
  
  
  ## plot the Lag Slices
  
  
 
  
  
  
  #covar_risk$all_Plot_Poly<-all_Plot_Poly
  #covar_risk$var_p<-names_cov_Plot 
  ##run the INLA model
                
                ##////>end_of_spat
                   
               })

               })