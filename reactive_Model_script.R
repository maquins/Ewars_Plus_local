
#Sys.sleep(120)
Out.Mod<-eventReactive(c(
  input$alarm_indicators_New_model,
  input$nlags,
  input$other_alarm_indicators_New_model,
  input$new_model_Year_validation),
  {
    #req(c(input$alarm_indicators_New_model,input$nlags))
    req(c(input$alarm_indicators_New_model,input$nlags,input$new_model_Year_validation))
    #req(input$other_alarm_indicators_New_model)
   
    cat(paste('\nValidation year now ..\n',input$new_model_Year_validation),'\n\n')
    
    #source("New_model_helper_Functions.R")
    dat_A<-var_names_New_model()$dat %>% 
      dplyr::arrange(district,year,week) %>% 
      dplyr::filter(district %in% boundary_file$district &!week==53)
    
    alarm_indicators<-input$alarm_indicators_New_model
    cat(paste('\nAlarm indicators now ..\n'),paste(input$alarm_indicators_New_model,collapse =','),'\n\n')
    alarm.indicators<<-alarm_indicators
    
    all_slice<-foreach(a=1:length(alarm_indicators))%do% get_fluid_slice_Output(a,'alarm.indicators')
    
    #insertUI(
    #selector = "#new_model_Week_plot_spat",
    #where = "beforeBegin",
    #ui = eval(parse(text=create_input_UI_year2("new_model_Year_plot")))
    #)
    
    output$var_Slices_Plots<-renderUI(eval(parse(text=paste('tabsetPanel(',paste(unlist(all_slice),collapse =','),')'))))
    #output$dist_Input1<-renderUI(eval(parse(text=create_input_UI_district("district_new"))))
    #output$dist_Input2<-renderUI(eval(parse(text=create_input_UI_district("district_seas"))))
    
    #output$dist_Input3<-renderUI(eval(parse(text=create_input_UI_district("output_dist_validation"))))
    #output$Year_Input1<-render_html(eval(parse(text=create_input_UI_district("new_model_Year_validation"))
    
    #output$Year_Input2<-renderUI(eval(parse(text=create_input_UI_year2("new_model_Year_plot"))))
    #output$valid_section<-renderUI(eval(parse(text=validation_tab_Func())))
    
    covar_to_Plot<-c(number_of_cases,pop.var.dat,alarm_indicators)
    names_cov_Plot<-c("Cases","Population",alarm_indicators)
    
    sel_var_endemic<-c(base_vars,number_of_cases,population)
    #cat(names_cov_Plot,sep=' \n')
    #stop("helo")
    
    covar_to_Plot<<-c(number_of_cases,pop.var.dat,alarm_indicators)
    
    par_text0<-get_UI_update_d(obj.plots=covar_to_Plot,
                               panel_Update="Descriptive Plots" ,
                               shinyOutputType="plotOutput",
                               cols=2,
                               out_ref="descr")
    
    output$new_model_data_descriptive_Plots<-renderUI({
      tagList(eval(par_text0))
    })
    
    par_text2<-get_UI_update_d(obj.plots=covar_to_Plot,
                               panel_Update="Time_series" ,
                               shinyOutputType="dygraphOutput",
                               cols=1,
                               out_ref="time_series")
    
    output$new_model_Time_series_Plots<-renderUI({
      tagList(eval(par_text2))
    })
    
    par_text1<-get_UI_update_d(obj.plots=covar_to_Plot,
                               panel_Update="Spat_Covariate_Plots_new_Model" ,
                               shinyOutputType="leafletOutput",
                               cols=2,
                               out_ref="spat_cov")
    
    
    output$Spat_Covariate_Plots_new_Model<-renderUI({
      tagList(eval(par_text1))
    })
    
    alarm_vars_lag<-alarm_indicators
    
    par_lag1<-get_UI_update_d(obj.plots=alarm_vars_lag,
                              panel_Update="Lag non linear plots" ,
                              shinyOutputType="plotOutput",
                              cols=1,
                              out_ref="contour")
    
    
    output$lag_contour_Plots<-renderUI({
      tagList(eval(par_lag1))
    })
    
    for_obs<-foreach(a=1:length(alarm_indicators),.combine =c) %do% paste0('input$',alarm_indicators[a],'_',1:3)
    
    
    ## balance the observations to compute expected cases correctly
    
    beg.year<-min(dat_A$year)
    end.year<-max(dat_A$year)
    
    dat_dist<-expand.grid(week=1:52,
                          year=sort(unique(dat_A$year)),
                          district=sort(unique(dat_A$district))) %>% 
      dplyr::select(district,year,week)
    
    data_augmented<<-merge(dat_dist,dat_A,by=c("district","year","week"),all.x=T,sort=T) %>% 
      dplyr::mutate(district_w=paste0(district,'_',week)) %>% 
      dplyr::arrange(district,year,week)
    nrow(data_augmented)  
    
    ##run the INLA model here
    
    
    nlag <<- input$nlags
    alarm_vars<<-input$alarm_indicators_New_model
    
    all_basis_vars<<-foreach(a=alarm_vars,.combine =c)%do% get_cross_basis(a,"data_augmented")
    
    #cat("got here..\n")
    data_augmented<-data_augmented %>% 
      dplyr::arrange(district,year,week) %>% 
      dplyr::group_by(district) %>% 
      dplyr::mutate(time=1:n())
    
    ntime <- length(unique(data_augmented$time))
    
    nyear <- length(unique(data_augmented$year))
    
    n_district <- length(unique(data_augmented$district))
    
    ## create district index 
    
    district_index<-data_augmented %>% 
      dplyr::select(district) %>% 
      unique() %>% 
      dplyr::arrange(district) 
    
    district_index$district_index<-1:nrow(district_index)
    
    data_augmented<-merge(data_augmented,district_index,by="district",sort=F)
    
    
    min.year<-min(data_augmented$year)
    
    data_augmented$year_index <- data_augmented$year - (min.year-1)
    
    fixe_alarm_vars<-input$other_alarm_indicators_New_model
    
    add.var<-fixe_alarm_vars[which(!fixe_alarm_vars%in% alarm_vars)]
    
    data_augmented<<-data_augmented
    
    if(length(add.var)>0){
      sel_mod.vars<-c(number_of_cases,pop.var.dat,"week","year_index","district_index",alarm_vars,add.var)
    }else{
      sel_mod.vars<-c(number_of_cases,pop.var.dat,"week","year_index","district_index",alarm_vars)
      
    }
    #cat(names(data_augmented),sep='..\n')
    #stop("xxx")
    df<-data_augmented[,sel_mod.vars]
    
    names(df)[1:5]<-c("Y","E","T1","T2","S1")
    df$E<-df$E/1e5
    df1<<-df
    
    
    
    #names(df)
    baseformula <- Y ~ 1 + f(T1,replicate = S1, model = "rw1", cyclic = TRUE, constr = TRUE,
                             scale.model = TRUE,  hyper = precision.prior) +
      f(S1,replicate=T2, model = "iid") 
    
    #base_model <- mymodel(baseformula,df)
    
    basis_var_n<<-paste0('all_basis_vars$',names(all_basis_vars))
    
    ## get the variable not among spline and keep as linear
    
    #fixe_alarm_vars<-input$other_alarm_indicators_New_model
    
    
    
    formula0.1 <- eval(parse(text=paste0("update.formula(baseformula, ~. +",paste(alarm_vars,collapse ='+'),')')))
    
    if(length(add.var)>0){
      formula0.2 <- eval(parse(text=paste0("update.formula(baseformula, ~. +",paste(add.var,collapse ='+'),'+',paste(basis_var_n,collapse ='+'),')')))
    }else{
      formula0.2 <- eval(parse(text=paste0("update.formula(baseformula, ~. +",paste(basis_var_n,collapse ='+'),')')))
      
    }
    
    
    #res<-mymodel(formula0.2,df)
    global_Objects<-c("mymodel","formula0.2","df","precision.prior","all_basis_vars")
    
    res_promise<-future_promise({mymodel(formula0.2,df)},
                                packages =c("INLA"),
                                globals =global_Objects,
                                seed=T)
    
    #res_out<<-res
    res_promise %...>% 
      {
        res<-.
        
        summary(res)
      }
    
    formula0.2a<<-formula0.2
    
    
    #base_model <- mymodel(baseformula,df)
    
    #model0.1<-mymodel(formula0.1,df)
    #res<-mymodel(formula0.2,df)
    
    ## save the model objects
    
    #list_objs<-list(base_model=base_model,
    #model0.1=model0.1,
    #res=res)
    
    #saveRDS(list_objs,"/Users/msewe/OneDrive - Umeå universitet/EWARS_new_Model/output/model_for_Test.rds",compress =T)
    
    
    
    #run_Models<-readRDS("~/OneDrive - Umeå universitet/EWARS_new_Model/output/model_for_Test.rds")
    
    #res<-run_Models$res
    
    ## choose cross validation year
    
    #YR.val<-which(beg.year:end.year==input$new_model_Year_validation)
    #YR.val<-which((beg.year:end.year)==input$new_model_Year_validation)
    Max_YR_idx<-which(c(beg.year:end.year)==max(unique(data_augmented$year)))
    
    if(is.null(input$new_model_Year_validation)){
      YR.val<<-Max_YR_idx
      
    }else{
      
      YR.val<<-which(c(beg.year:end.year)==input$new_model_Year_validation)
     
    }
    
    
    total_spaces<-52/4
    Inte.v<-ceiling(52/total_spaces)
    beg_gri<-seq(1,52,Inte.v)
    end_gri<-seq(Inte.v,52,Inte.v)
    #cbind(beg,end)
    #Max_YR_idx<-6
    #YR.val<-5
    all_YR_r<-YR.val:Max_YR_idx
    
    if(is.null(input$new_model_Year_validation)){
      
      run_grid<<-foreach(a=all_YR_r,.combine =rbind)%do% data.frame(beg_week=1,
                                                                    end_week=52,
                                                                    YR=a)
    }else{
      run_grid<<-foreach(a=all_YR_r,.combine =rbind)%do% data.frame(beg_week=beg_gri,
                                                                    end_week=end_gri,
                                                                    YR=a)
    }
    
    
   
    #run_grid<<-expand.grid(week=1:52,YR=YR.val)
    #year_eval<<-input$new_model_Year_validation
    
    if(is.null(input$new_model_Year_validation)){
      year_eval<-max(data_augmented$year)
    }else{
      year_eval<-input$new_model_Year_validation
      
    }
    
    Years_orig<<-year_eval:end.year
    ## run weekly cross validation for the predicted year
    
    #plan(multisession(workers =2))
    #plan(sequential)
  
    p_progress <<- Progress$new()
    
    if(is.null(input$new_model_Year_validation)){
      message_Cross<-"loading .."
    }else{
      message_Cross<-"Cross validation running..."
    }
      
    p_progress$set(value = NULL, message =message_Cross )
    
    pred_vals_all_promise<<-future_promise({
      res_m<-mymodel(formula0.2,df)
      theta_beg<<-res_m$internal.summary.hyperpar$mean 
      foreach(a=1:nrow(run_grid),.combine =rbind)%do% get_weekly_prediction_4(a)
      #foreach(a=1:nrow(run_grid),.combine =rbind)%do% get_weekly_prediction(a)
      #readRDS("pred_eval_2013.rds")
    },
    packages =c("INLA","dplyr"),
    globals=c("df","year_eval","run_grid","YR.val",
              "get_weekly_prediction",
              "get_weekly_prediction_4",
              "df1","mymodel3",
              "mymodel",
              "formula0.2a",
              "formula0.2",
              "precision.prior",
              "all_basis_vars",
              "data_augmented"),
    seed=T) %...>% 
      data.frame()%>%
      finally(~cat("Cross validation finished",'\n'))
    p_progress$close()
    
    
    list(pred_vals_all_promise=pred_vals_all_promise,
         res_promise=res_promise,
         data_augmented=data_augmented,
         all_basis_vars=all_basis_vars,
         alarm_indicators=alarm_indicators,
         alarm_vars=alarm_vars,
         for_obs=for_obs,
         covar_to_Plot=covar_to_Plot,
         names_cov_Plot=names_cov_Plot,
         basis_var_n=basis_var_n)
  },
  ignoreNULL=F)
