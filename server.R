#pkgs<-c('XLConnect','plyr','dplyr','car','stringr','zoo','foreign','ggplot2','splines',
       # 'mgcv','Hmisc','xtable','foreach','xlsx','lattice','latticeExtra',"gridExtra","grid","shiny","googlesheets")
#for (i in length(pkgs)){
 # if(pkgs[i] %in% rownames(installed.packages()) == FALSE) {
    #install.packages(pkgs[i])
  #}
#}


suppressMessages(library(plyr,warn.conflicts =F))## This package should be loaded before dplyr
suppressMessages(library(dplyr,warn.conflicts =F))
suppressMessages(library(car,warn.conflicts =F))
suppressMessages(library(stringr,warn.conflicts =F))
suppressMessages(library(zoo,warn.conflicts =F))
suppressMessages(library(foreign,warn.conflicts =F))
suppressMessages(library(ggplot2,warn.conflicts =F))
suppressMessages(library(splines,warn.conflicts =F))
suppressMessages(library(mgcv,warn.conflicts =F))
suppressMessages(library(Hmisc,warn.conflicts =F))
suppressMessages(library(xtable,warn.conflicts =F))
suppressMessages(library(foreach,warn.conflicts =F))
#library(xlsx)## Ensure java installed on system same as one for R, e.g 64 bit R with 64 bit Java
#library(XLConnect)
suppressMessages(library(lattice,warn.conflicts =F))
suppressMessages(library(latticeExtra,warn.conflicts =F))
suppressMessages(library(gridExtra,warn.conflicts =F))
suppressMessages(library(grid,warn.conflicts =F))
#library(googlesheets)
suppressMessages(library(reshape2,warn.conflicts =F))
suppressMessages(library(plotly,warn.conflicts =F))
suppressMessages(library(tidyr,warn.conflicts =F))
suppressMessages(library(lubridate,warn.conflicts =F))
#suppressMessages(library(pse,warn.conflicts =F))# for automatic calibration
suppressMessages(library(reportROC,warn.conflicts =F))
#suppressMessages(library(caret,warn.conflicts =F))
#suppressMessages(library(e1071,warn.conflicts =F))
suppressMessages(library(knitr,warn.conflicts =F))
#library(pkgload)
suppressMessages(library(rgeos,warn.conflicts =F))
suppressMessages(library(raster,warn.conflicts =F))
#library(leaflet)
#library(pak)

#options(repos=c(INLA="https://inla.r-inla-download.org/R/stable",
               # CRAN="https://cran.rstudio.com/"))
##load INLA package

#remotes::install_github("hrue/r-inla@stable", subdir = "rinla")
#remotes::install_github("url::https://github.com/hrue/r-inla/archive/refs/heads/stable.zip")
#library(INLA)
#devtools::install_github(repo = "https://github.com/hrue/r-inla", ref = "stable", subdir = "rinla", build = FALSE)
#library(INLA)
suppressMessages(library(doParallel,warn.conflicts =F))
#suppressMessages(library(profvis,warn.conflicts =F))

#library(future)
#library(promises)
#library(ipc)
suppressMessages(library(readxl,warn.conflicts =F))
suppressMessages(library(ROCit,warn.conflicts =F))
suppressMessages(library(RSQLite,warn.conflicts =F))
suppressMessages(library(DT,warn.conflicts =F))
## included after risk mapping integration

suppressMessages(library(tmap,warn.conflicts =F))
suppressMessages(library(tmaptools,warn.conflicts =F))
suppressMessages(library(leaflet,warn.conflicts =F))
suppressMessages(library(rgdal,warn.conflicts =F))
suppressMessages(library(stringr,warn.conflicts =F))
suppressMessages(library(xts,warn.conflicts =F))
suppressMessages(library(dygraphs,warn.conflicts =F))
suppressMessages(library(SpatialEpi,warn.conflicts =F))
suppressMessages(library(spdep,warn.conflicts =F))
suppressMessages(library(cleangeo,warn.conflicts =F))
suppressMessages(library(dlnm,warn.conflicts =F))
suppressMessages(library(ggthemes,warn.conflicts =F))
suppressMessages(library(RColorBrewer,warn.conflicts =F))
suppressMessages(library(tsModel,warn.conflicts =F))
suppressMessages(library(kableExtra,warn.conflicts =F))
suppressMessages(library(viridis,warn.conflicts =F))
#suppressMessages(library(devtools,warn.conflicts =F))
library(INLA)

#inla_tar<-list.files(getwd(),pattern ='tar')
#devtools::install_github(repo = "https://github.com/hrue/r-inla", ref = "stable", subdir = "rinla", build = FALSE)

inla_cmd<-dplyr::tribble(~var,
                         'cnn<-try(find.package("INLA",quiet=TRUE, verbose=F))',
                         paste0('if(length(cnn)==0){devtools::install_github(repo = "https://github.com/hrue/r-inla", ref = "stable", subdir = "rinla", build = FALSE)}'),
                         "library(INLA)",
                         "cnn"
)


#eval(parse(text=inla_cmd$var[1]))
#eval(parse(text=inla_cmd$var[2]))
#eval(parse(text=inla_cmd$var[3]))
#eval(parse(text=inla_cmd$var[4]))

suppressMessages(library(data.table,warn.conflicts =F))
suppressMessages(library(reportROC,warn.conflicts =F))
suppressMessages(library(promises,warn.conflicts =F))
suppressMessages(library(googledrive,warn.conflicts =F))
suppressMessages(library(googleAuthR,warn.conflicts =F))

suppressMessages(library(future,warn.conflicts =F))

#plan(multisession(workers =2))
plan(sequential)
#library(doFuture)

#doFuture::registerDoFuture()


#if (!getDoParRegistered()){
  #cl <<- makeCluster(5)
  #registerDoParallel(cl)
#}

drive_auth(path="ewarsupload-3a42544ce9f8.json")

drive_download("ewars_users_DB",overwrite =T)

server<-function(input,output,session) { 
  
  #ISO2<-"LKA"
  #Country_name<-"Sri Lanka"
  
  output$title_txt<-renderUI(tags$h3("Ewars Dashboard +",style="font:cambria"))
  #con <- dbConnect(SQLite(),"users.sqlite")
  #pb<-dbGetQuery(con, "SELECT user_name,password,role FROM users_db")
  pb<-read_xlsx("ewars_users_DB.xlsx",sheet="users_Db") %>% 
    dplyr::select(user_name,password,role)
  #pb<-data.frame(user_name="demo",password="demo_2019",role="admin")
  #pb_dis<-dbGetQuery(con, "SELECT user_name,district_code FROM users_districts")
  #dbDisconnect(con)
  pb_dis<-read_xlsx("ewars_users_DB.xlsx",sheet="users_districts") %>% 
    dplyr::select(user_name,district_code)
  
  login = F
  USER <- reactiveValues(login = login)
  user_info<-reactiveValues(nm_pwd="demo",user="demo_2019")
  
  observeEvent(input$lg_in,{
    user_info$nm_pwd<-paste(str_trim(tolower(input$user_name)),str_trim(input$pwd))
    user_info$user<-str_trim(tolower(input$user_name))
    output$logged_in_user<-renderUI(input$user_name)
    
  })
  
 
  observe({
    
    role_d1<-pb %>% dplyr::filter(str_trim(tolower(user_name))==user_info$user)
    #role_d<-pb %>% dplyr::filter(str_trim(tolower(user_name))==user_info$user)
    if(nrow(role_d1)==1){
      role_d<-role_d1
    }else{
      role_d<-data.frame(user_name="xxx",role="xxx",stringsAsFactors =F)
    }
    #print(role_d)
    ##change this part for production 2020-11-26
    #-------------------------
    #-------------------------
    if (user_info$nm_pwd %in% paste(str_trim(tolower(pb$user_name)),str_trim(pb$password))){

      USER$login=T
    }  
    #print(user_info$nm_pwd)
    #print(pb)
    output$log_list<-renderUI(
      if(USER$login==T & str_trim(tolower(role_d$role))=="admin"){
        ui_yes
      }
      
      else if(USER$login==T & str_trim(tolower(role_d$role))=="district manager"){
        ui_yes_Restricted
      }
      
      else if(USER$login==F & !user_info$nm_pwd ==""){
        tagList(login_screen,br(),tags$h4("Please enter a valid user name and password"))
      }else if(USER$login==F & user_info$nm_pwd ==""){
        login_screen
      }
    )
  })
  
 
  ## read in the user database
  

  output$logout <- renderUI({
    req(USER$login)
    tags$h5(a( "logout", 
               href="javascript:window.location.reload(true)"))
    #class = "dropdown", 
    #style = "background-color: #eee !important; border: 0;
    #font-weight: bold; margin:5px; padding: 10px;height:40px")
  })
  
  
  #eval(parse(text=readLines("New_model_helper_Functions.R")))
  output$spat_Display_new_Model<-renderUI({output_graphs_New_Model})
  #output$valid_section<-renderUI(eval(parse(text=validation_tab_Func())))
  showTab("to_show2",target="Model Validation",select=T)
  eval(parse(text=readLines("New_model_server_Script.R")))
 

  eval(parse(text=readLines("Spatial_temporal.R")))
  #eval(parse(text=readLines("New_model_Spatial_temporal_Compute_UI.R")))
  
  eval(parse(text=readLines("Reactive_function_DBII_New_Model.R")))
   #output$spat_Display_new_Model<-renderUI({output_graphs_New_Model})
  
  observeEvent(input$Enter_user,{
    
    entry_dat<-data.frame(First_name =input$First_name, 
                          Last_name =input$Last_name,
                          user_name =input$users_name,
                          password =input$pass_word,
                          email =input$email,
                          role =input$role,
                          stringsAsFactors =F)
    
    ##create user district entry 
    
    entry_district<-expand.grid(user_name=input$users_name,
                                district_code=input$district_manage,
                                stringsAsFactors =F)
    
    
    
    con_dist <- dbConnect(SQLite(),"users.sqlite")
    
    ## decide whether to update or insert new record
    ds_cu<-dbGetQuery(con_dist, 'SELECT distinct * FROM users_db')
    ds_cu1<-dbGetQuery(con_dist, 'SELECT distinct * FROM users_districts')
    
    if (!str_trim(entry_dat$user_name)=="" & !str_trim(entry_dat$password)==""){
      if (!entry_dat$user_name %in% ds_cu$user_name){
        dbWriteTable(con_dist,'users_db',entry_dat[1,],append=T)
      }else{
        bbp<-paste(paste(names(entry_dat),'=',paste("'",entry_dat[1,],"'",sep=''),collapse =','))
        
        dbSendQuery(con_dist,paste("update", "users_db","set",bbp,paste("where user_name='",entry_dat$user_name,"'",sep='')))
        # )))
      }
      
      #write users and districts
      if (input$role=="District Manager"){
        ## delete existing records before updating
        if (unique(entry_district$user_name) %in% ds_cu1$user_name){
          dbSendQuery(con_dist,paste("delete  from users_districts",paste("where user_name='",unique(entry_district$user_name),"'",sep='')))
        }
        
        dbWriteTable(con_dist,'users_districts',entry_district,append=T)
        
      }
    }else{
      print("do nothing")
    }
    
    user_di<-dbGetQuery(con_dist, 'SELECT distinct * FROM users_db')
    user_dist<-dbGetQuery(con_dist, 'SELECT distinct * FROM users_districts')
    dbDisconnect(con_dist)
    output$users_dat<-DT::renderDataTable(DT::datatable(user_di,
                                                        options = list(autoWidth = TRUE,
                                                                       searching = T)))
    output$users_districts<-DT::renderDataTable(DT::datatable(user_dist,
                                                              options = list(autoWidth = TRUE,
                                                                             searching = T)))
    
    
    con_dist1 <- dbConnect(SQLite(),"users.sqlite")
    
    dx<-dbGetQuery(con_dist1, 'SELECT distinct * FROM users_db')
    
    dbDisconnect(con_dist1)
    n.a<-paste(dx$user_name,' [',dx$First_name,',',dx$Last_name,']',sep='')
    
    
    
    updateSelectInput(session,"user_de",
                      choices =c(n.a,"John_doe [johh,doe]"),
                      selected ="John_doe [johh,doe]")
    
    
    observeEvent(input$Delete_user,{
      user_to_del<<-input$user_de
      to_de<-str_split(input$user_de,' ',simplify =T)[,1]
      
      
      con_dist1 <- dbConnect(SQLite(),"users.sqlite")
      del_qry<-paste("delete    from users_db where user_name in ",
                     paste('(',paste(paste("'",to_de,"'",sep=''),collapse=','),sep=''),')',sep='')
      
      del_qry1<-paste("delete    from users_districts where user_name in ",
                      paste('(',paste(paste("'",to_de,"'",sep=''),collapse=','),sep=''),')',sep='')
      
      
      dbExecute(con_dist1,del_qry)
      dbExecute(con_dist1,del_qry1)
      
      
      
    })
    
  }
  ) 
  
  
  ### New model
  
  
  
}