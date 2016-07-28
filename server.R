library(shiny)
library(ggplot2)
library(RCurl)
library(jsonlite)
library(stringr)
library(RMySQL)
library(rCharts)

con<-getConnection()
dbSendQuery(con,'SET NAMES gbk')
startDate=as.character(Sys.Date())
qi_days<-as.character(Sys.Date()-7)
#If you want create a timer 
#updatetime=60

shinyServer(function(input,output,session){
  
  observe({
    
    if(input$selecttype=='order'){
      updateSelectInput(session=session,inputId='Statistics_Model',choices=list('各时段订单趋势','日订单趋势图'))
    }
    if(input$selecttype=='customer'){
      updateSelectInput(session=session,inputId='Statistics_Model',choices=list('浏览量统计'))
    }
    if(input$selecttype=='nice'){

      updateSelectInput(session=session,inputId='Statistics_Model',choices=list('热门菜品图','菜品销量趋势','各店铺销售份数统计'))
    
    }
    if(input$selecttype=='comment'){
      updateSelectInput(session=session,inputId = 'Statistics_Model',choice=list('评论云图'))
    }
  })
  
  UpdateMyChart<-eventReactive(input$go3,{
    
      shopname=input$key
      start_time=input$daterange_start
      if(input$Product_Categorys=='美团') plat='meituan_order_valid'
      if(input$Product_Categorys=='饿了么') plat='order_eleme_valid'
      if(input$Product_Categorys=='百度外卖') plat='baidu_order_copy'
    
      if(input$selecttype=='nice'){
        #在这里建立一个分支，用于时间段销量的统计
        #------------------End ---------------------
        sql<-str_c("select order_id,username,userphone,address,shopname,food_name,food_count,DATE_FORMAT(xiadan_time,'%Y-%m-%d') as 'xiadan_time' from ",
                   plat," where xiadan_time >'",start_time[1],"' and xiadan_time<='",start_time[2],
                   "'"," and shopname like N'",shopname,"%'")
        #dbget the data from database 
        result<-data.frame(dbFetch(dbSendQuery(con,sql),n=-1))
      }
      if(input$selecttype=='order'){
        sql<-str_c("SELECT
                	 xiadan_time,1 as jishu,
                   DATE_FORMAT(xiadan_time, '%Y-%m-%d') AS format_xiadan_time,
                   HOUR (xiadan_time) AS shiduan FROM meituan_order_valid ",
                   plat," where xiadan_time >'",start_time[1],"' and xiadan_time<'",start_time[2],
                   "'"," and shopname like N'",shopname,"%'")
        #dbget the data from database 
        result<-data.frame(dbFetch(dbSendQuery(con,sql),n=-1))
      }
      
      edit(sql)
    
      #Progress The Data
      withProgress(message = '正在获取并处理数据',max = nrow(result), {

        if(input$selecttype=='nice'){
          
          rbin_df<-data.frame(stringsAsFactors = FALSE,' ',' ',' ',' ',' ',' ','')
          result_list<-1:nrow(result)

          for(x in result_list){
            rbin_food_name<-unlist(str_split(result[x,]$food_name,','))
            rbin_food_count<-unlist(str_split(result[x,]$food_count,','))
            
            for(m in 1:length(rbin_food_name)){
              
              add_rows<-c(id=result[x,]$order_id,address=result[x,]$address,username=result[x,]$username,foodname=rbin_food_name[m]
                          ,foodcount=rbin_food_count[m],xiadantime=unlist(str_split(result[x,]$xiadan_time,' '))[1],shopname=result[x,]$shopname)
              
              rbin_df<-rbind(rbin_df,add_rows)
            }
            #Progress the message %
            incProgress(0.95, message= paste("进度", (x/nrow(result))*100,'%'))
          }
          
          names(rbin_df)<-c('id','address','username','foodname','foodcount','xiadantime','shopname')
          rbin_df$foodcount<-as.numeric(as.character(rbin_df$foodcount))
          rbin_df$foodname<-as.character(rbin_df$foodname)
          return(rbin_df)
          dbDisconnect(con)
        }
        if(input$Statistics_Model=='日订单趋势图'){
          return(result)
          dbDisconnect(con)
        }
        if(input$Statistics_Model=='各时段订单趋势'){
          return(result)
          dbDisconnect(con)
        }
        if(input$Statistics_Model=='评论云图'){
          return(result)
          dbDisconnect(con)
        }
      }
  )
  })
  
  get_order_timer<-function(){
    
    #order Count
    meituan_sql<-str_c("SELECT count(*) from meituan_order_valid WHERE meituan_order_valid.xiadan_time >='",startDate,"'",sep="")
    meituan_order_count<-dbFetch(dbSendQuery(con,meituan_sql),n=-1)
    baidu_sql<-str_c("SELECT count(*) from mobile_baidu_order  WHERE mobile_baidu_order.create_time >='",startDate,"'",sep="")
    baidu_order_count<-dbFetch(dbSendQuery(con,baidu_sql),n=-1)
    eleme_sql<-str_c("SELECT count(*) from order_eleme_valid_update  WHERE order_eleme_valid_update.order_time >='",startDate,"'",sep="")
    eleme_order_count<-dbFetch(dbSendQuery(con,eleme_sql),n=-1)
    all_order<-meituan_order_count+baidu_order_count+eleme_order_count
    print(str_c('时间流逝',Sys.time()))
    #7 days
    qi_meituan_sql<-str_c("SELECT count(*) from meituan_order_valid WHERE meituan_order_valid.xiadan_time >='",qi_days,"'",sep="")
    qi_meituan_count<-dbFetch(dbSendQuery(con,qi_meituan_sql),n=-1)
    qi_baidu_sql<-str_c("SELECT count(*) from mobile_baidu_order  WHERE mobile_baidu_order.create_time >='",qi_days,"'",sep="")
    qi_baidu_count<-dbFetch(dbSendQuery(con,qi_baidu_sql),n=-1)
    qi_eleme_sql<-str_c("SELECT count(*) from order_eleme_valid_update  WHERE order_eleme_valid_update.order_time >='",qi_days,"'",sep="")
    qi_eleme_count<-dbFetch(dbSendQuery(con,qi_eleme_sql),n=-1)
    qi_mean<-round((qi_meituan_count+qi_baidu_count+qi_eleme_count)/7,2)
    
    #all jin'e
    jin_e_sql<-sprintf("select sum(a) from (
                    SELECT
                     sum(shifu) a 
                     FROM
                     meituan_order_valid
                     WHERE
                     xiadan_time >= '%s'
                     UNION ALL
                     select sum(shifu) a from mobile_baidu_order WHERE mobile_baidu_order.create_time >='%s'
                     union ALL
                     select sum(shifu) a from order_eleme_valid_update WHERE order_eleme_valid_update.order_time >='%s' 
    ) c",startDate,startDate,startDate)
    
    zong_e<-str_c('￥',round(dbFetch(dbSendQuery(con,jin_e_sql),n=-1),2))
    #GZ data
    gz_meituan_order_sql<-sprintf("SELECT
	                                count(*)
                                  FROM
                                  meituan_order_valid
                                  WHERE
                                  meituan_order_valid.xiadan_time >= '%s'
                                  AND meituan_order_valid.city_name = '广州'",startDate)
    gz_meituan_order<-dbFetch(dbSendQuery(con,gz_meituan_order_sql),n=-1) 
    
    sz_meituan_order_sql<-sprintf("SELECT
	                                count(*)
                                  FROM
                                  meituan_order_valid
                                  WHERE
                                  meituan_order_valid.xiadan_time >= '%s'
                                  AND meituan_order_valid.city_name = '深圳'",startDate)
    sz_meituan_order<-dbFetch(dbSendQuery(con,sz_meituan_order_sql),n=-1)
    
    gz_baidu_order_sql<-sprintf("
                                SELECT
                                count(*)
                                FROM
                                mobile_baidu_order
                                WHERE
                                mobile_baidu_order.create_time >= '%s'
                                AND mobile_baidu_order.city_name = '广州'",startDate)
    gz_baidu_order<-dbFetch(dbSendQuery(con,gz_baidu_order_sql),n=-1)
    sz_baidu_order_sql<-sprintf("SELECT
	                              count(*)
                                FROM
                                mobile_baidu_order
                                WHERE
                                mobile_baidu_order.create_time >= '%s'
                                AND mobile_baidu_order.city_name = '深圳'",startDate)
    sz_baidu_order<-dbFetch(dbSendQuery(con,sz_baidu_order_sql),n=-1)
    
    sz_eleme_order_sql<-sprintf("SELECT
	                              count(*)
                                FROM
                                order_eleme_valid_update
                                WHERE
                                order_eleme_valid_update.order_time >= '%s'",startDate)
    sz_eleme_order<-dbFetch(dbSendQuery(con,sz_eleme_order_sql),n=-1)
    #sz meituan qushi
    sz_m_qushi_sql<-"SELECT
	                count(*)
                FROM
          	meituan_order_valid
      WHERE
      	xiadan_time >= DATE_FORMAT(now(), '%Y-%m-%d')
      AND city_name = '深圳'
      UNION ALL
      	SELECT
      		count(*)
      	FROM
      		meituan_order_valid
      	WHERE
      		xiadan_time >= date_sub(curdate(), INTERVAL 1 DAY)
      	AND xiadan_time < date_add(now(), INTERVAL - 24 HOUR)
      	AND city_name = '深圳'"
    
    sz_m_qushi<-dbFetch(dbSendQuery(con,sz_m_qushi_sql),n=-1)
    sz_m_qushi<-sz_m_qushi$`count(*)`[1]-sz_m_qushi$`count(*)`[2]
    
    #sz eleme qushi
    sz_e_qushi_sql<-"SELECT
	count(*)
    FROM
    order_eleme_valid_update
    WHERE
    order_time >= DATE_FORMAT(now(), '%Y-%m-%d')
    
    UNION ALL
    SELECT
    count(*)
    FROM
    order_eleme_valid_update
    WHERE
    order_time >= date_sub(curdate(), INTERVAL 1 DAY)
    AND order_time < CURDATE()
    "
    sz_e_qushi<-dbFetch(dbSendQuery(con,sz_e_qushi_sql),n=-1)
    sz_e_qushi<-sz_e_qushi$`count(*)`[1]-sz_e_qushi$`count(*)`[2]
    
    #sz baidu qushi
    sz_b_qushi_sql<-"SELECT
	count(*)
    FROM
    mobile_baidu_order
    WHERE
    create_time >= DATE_FORMAT(now(), '%Y-%m-%d') and city_name='深圳'
    
    UNION ALL
    SELECT
    count(*)
    FROM
    mobile_baidu_order
    WHERE
    create_time >= date_sub(curdate(), INTERVAL 1 DAY)
    AND create_time < date_add(now(), INTERVAL - 24 HOUR) and city_name='深圳'
    "
    sz_b_qushi<-dbFetch(dbSendQuery(con,sz_b_qushi_sql))
    sz_b_qushi<-sz_b_qushi$`count(*)`[1]-sz_b_qushi$`count(*)`[2]
    
    #--------------------GZ part--------------
    m_qushi_sql<-"SELECT
	                count(*)
    FROM
    meituan_order_valid
    WHERE
    xiadan_time >= DATE_FORMAT(now(), '%Y-%m-%d')
    AND city_name = '广州'
    UNION ALL
    SELECT
    count(*)
    FROM
    meituan_order_valid
    WHERE
    xiadan_time >= date_sub(curdate(), INTERVAL 1 DAY)
    AND xiadan_time < date_add(now(), INTERVAL - 24 HOUR)
    AND city_name = '广州'"
    m_qushi<-dbFetch(dbSendQuery(con,m_qushi_sql))
    m_qushi<-m_qushi$`count(*)`[1]-m_qushi$`count(*)`[2]
    
    b_qushi_sql<-"SELECT
	count(*)
    FROM
    mobile_baidu_order
    WHERE
    create_time >= DATE_FORMAT(now(), '%Y-%m-%d') and city_name='广州'
    
    UNION ALL
    SELECT
    count(*)
    FROM
    mobile_baidu_order
    WHERE
    create_time >= date_sub(curdate(), INTERVAL 1 DAY)
    AND create_time < date_add(now(), INTERVAL - 24 HOUR) and city_name='广州'"
    
    b_qushi<-dbFetch(dbSendQuery(con,b_qushi_sql))
    b_qushi<-b_qushi$`count(*)`[1]-b_qushi$`count(*)`[2]
    
    #rbind the dataframe
    order_count<-data.frame(allorder=all_order,eleme=eleme_order_count,
                            baidu=baidu_order_count,meituan=meituan_order_count,
                            qimean=qi_mean,zonge=zong_e,gz_meituan_order,sz_meituan_order,
                            gz_baidu_order,sz_baidu_order,sz_eleme_order,
                            sz_m_qushi,sz_b_qushi,sz_e_qushi,m_qushi,b_qushi)
    
    updateInput_sql<-str_c("select max(xiadan_time) from meituan_order_valid WHERE meituan_order_valid.xiadan_time >='",startDate,"'",sep="")
    riqi<-dbFetch(dbSendQuery(con,updateInput_sql),n=-1)
    names(order_count)<-c('allorder','eleme','baidu','meituan','qimean',
                          'zonge','gz_meituan','sz_meituan','gz_baidu',
                          'sz_baidu','sz_eleme','sz_m_qushi','sz_b_qushi',
                          'sz_e_qushi','m_qushi','b_qushi')
    #edit(order_count$qimean)
    updateTextInput(session, 'caption', label = '最后一条记录日期', value = paste('New:',as.character(riqi)))
    return(order_count)
  }
    
  UpdateMyHTML <- eventReactive(input$go, {
    
    start_time=input$daterange_start
    if(input$Product_Categorys=='美团') plat='meituan_order_valid'
    if(input$Product_Categorys=='饿了么') plat='order_eleme_valid'
    if(input$Product_Categorys=='百度外卖') plat='baidu_order_copy'
    
    shopname=input$key
    
    sql<-str_c("select order_id,username,userphone,address,shopname,food_name,food_count,xiadan_time from ",
               plat," where xiadan_time >'",start_time[1],"' and xiadan_time<'",start_time[2],
               "'"," and shopname like N'",shopname,"%'")
    
    edit(sql)
    #Gbget the data from database 
    result<-data.frame(dbFetch(dbSendQuery(con,sql),n=-1))
    
    #Progress The Data
    withProgress(message = '正在获取并处理数据',max = nrow(result), {
      
      if(input$selecttype=='nice'){
        
        edit(iris)
        
        rbin_df<-data.frame(stringsAsFactors = FALSE,' ',' ',' ',' ',' ',' ','')
        result_list<-1:nrow(result)
        
        for(x in result_list){
           
          rbin_food_name<-unlist(str_split(result[x,]$food_name,','))
          rbin_food_count<-unlist(str_split(result[x,]$food_count,','))
          
          for(m in 1:length(rbin_food_name)){
            
            add_rows<-c(id=result[x,]$order_id,address=result[x,]$address,username=result[x,]$username,foodname=rbin_food_name[m]
                        ,foodcount=rbin_food_count[m],xiadantime=unlist(str_split(result[x,]$xiadan_time,' '))[1],shopname=result[x,]$shopname)
            
            
            rbin_df<-rbind(rbin_df,add_rows)
          }
          #Progress the message %
          incProgress(0.95, message= paste("进度", (x/nrow(result))*100,'%'))
        }
        
        names(rbin_df)<-c('id','address','username','foodname','foodcount','xiadantime','shopname')
        rbin_df$foodcount<-as.numeric(as.character(rbin_df$foodcount))
        rbin_df$foodname<-as.character(rbin_df$foodname)
        return(rbin_df)
        dbDisconnect(con)
      }
      
    }
    )
  })
  
  observe({
    invalidateLater(1000, session)
    updateTextInput(session,'jishiqi',value=as.character(Sys.time()))
  })
  
  output$kanban<-renderText({
    
    invalidateLater(12000)
    
    order_count<-get_order_timer()
   
    paste0(htmlTemplate('tablePanel.html',
                        orderCount=order_count$allorder,
                        fenshu=order_count$zonge,
                        avgCount=order_count$qimean,
                        meituan_order=order_count$gz_meituan,
                        baidu_order=order_count$gz_baidu,
                        sz_meituan_order=order_count$sz_meituan,
                        sz_eleme_order=order_count$sz_eleme,
                        sz_baidu_order=order_count$sz_baidu,
                        sz_m_qushi=order_count$sz_m_qushi,
                        sz_e_qushi=order_count$sz_e_qushi,
                        sz_b_qushi=order_count$sz_b_qushi,
                        m_qushi=order_count$m_qushi,
                        b_qushi=order_count$b_qushi
    ))
    #close the database 
  })
  
  output$mianmohtmloutput<-renderText({
    
    if (input$go == 0)
      return()
    
    if(input$selecttype=='nice'){
    BIG_DATA<-UpdateMyHTML()
    mydf<-na.omit(BIG_DATA)
    mydf<-aggregate(mydf$foodcount,list(mydf$foodname),sum)
    names(mydf)<-c('foodname','foodcount')
    
    topdf<-head(mydf[order(mydf$foodcount,decreasing = TRUE),],20)
    maxmydf<-mydf[which.max(mydf$foodcount),]
    minmydf<-mydf[which.min(mydf$foodcount),]

    paste0(div(class='h1',span('热门菜品'),tags$i(class='title-fire')),
          div(tags$ul(class='item-name',tags$li(maxmydf$foodname))),
          div(tags$ul(class='item-sales','售出',tags$li(maxmydf$foodcount),'份')),
          div(class='h1',span('冷门菜品')),
          div(tags$ul(class='item-name',tags$li(minmydf$foodname))),
          div(tags$ul(class='item-sales','售出',tags$li(minmydf$foodcount),'份')),
          div(class='h1',tags$span('销量top10菜品')),
          div(tags$ul(class='item-name',tags$li(topdf$foodname[1]),
                      tags$li(topdf$foodcount[1]),
                      tags$li(topdf$foodname[2]),
                      tags$li(topdf$foodcount[2]),
                      tags$li(topdf$foodname[3]),
                      tags$li(topdf$foodcount[3]),
                      tags$li(topdf$foodname[4]),
                      tags$li(topdf$foodcount[4]),
                      tags$li(topdf$foodname[5]),
                      tags$li(topdf$foodcount[5]),
                      tags$li(topdf$foodname[6]),
                      tags$li(topdf$foodcount[6]),
                      tags$li(topdf$foodname[7]),
                      tags$li(topdf$foodcount[7]),
                      tags$li(topdf$foodname[8]),
                      tags$li(topdf$foodcount[8]),
                      tags$li(topdf$foodname[9]),
                      tags$li(topdf$foodcount[9]),
                      tags$li(topdf$foodname[10]),
                      tags$li(topdf$foodcount[10])))
          )
          
    }
  })
      
  output$myChart <- renderChart({
    
    BIG_DATA<-UpdateMyChart()
    mydf<-na.omit(BIG_DATA)
    
    if(input$Statistics_Model=='热门菜品图'){
    
    mydf<-aggregate(mydf$foodcount,list(mydf$foodname),sum)
    names(mydf)<-c('foodname','foodcount')
    edit(mydf)
    mydf<-head(mydf[order(mydf$foodcount,decreasing = TRUE),],100)
    
    p1 <- hPlot(foodcount~foodname,data = mydf,type ="column",title = '热门菜品销售数据',subtitle = '点击查看菜品详细数据')
    p1$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '14px', fontFamily = '华文细黑')), replace = F)
    p1$chart(zoomType = "xy",height=800)
    p1$exporting(enabled = T)
    p1$plotOptions(column = list(colorByPoint =TRUE))
    #p1$colors('rgba(30, 144,255, 1)', 'rgba(30, 144,255, 1)', 'rgba(30, 144, 255, 1)')
    p1$legend(align = 'center', verticalAlign = 'bottom', layout = 'horizontal')
    p1$tooltip(formatter = "#! function() { return '菜品名称：'+this.x + ', 销售总量：' + this.y; } !#")
    p1$addParams(dom = 'myChart')
    
    return(p1)
    }
    
    #改成各区域销售趋势
    if(input$Statistics_Model=='菜品销量趋势'){
    
      mydf<-aggregate(mydf$foodcount,list(mydf$xiadantime,mydf$shopname),sum)
      names(mydf)<-c('xiadantime','shopname','foodcount')
      edit(mydf)
      p1<-hPlot(foodcount~xiadantime,data=mydf,type=c("column","line"),title='菜品销量趋势',group="shopname",subtitle='查看店铺销量趋势')
      p1$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '16px', fontFamily = '华文细黑')), replace = F)
      p1$chart(zoomType = "xy",height=800)
      p1$exporting(enabled = T)
      p1$legend(align = 'center', verticalAlign = 'bottom', layout = 'horizontal')
      p1$tooltip(formatter = "#! function() { return '日期：'+this.x + ', 销售总量：' + this.y; } !#")
      p1$addParams(dom = 'myChart')
      
      return(p1)
    }
  
    if(input$Statistics_Model=='各店铺销售份数统计'){
      
      mydf<-aggregate(mydf$foodcount,list(mydf$shopname),sum)
      names(mydf)<-c('shopname','foodcount')
      edit(mydf)
      mydf<-mydf[order(mydf$foodcount,decreasing = TRUE),]
      p1 <- hPlot(foodcount~shopname,data = mydf,type ="column",title = '店铺销量分布图',subtitle = '查看各个店铺销量分布')
      p1$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '14px', fontFamily = '华文细黑')), replace = F)
      p1$chart(zoomType = "xy",height=800)
      p1$exporting(enabled = T)
      p1$plotOptions(column = list(colorByPoint =TRUE))
      #p1$colors('rgba(30, 144,255, 1)', 'rgba(30, 144,255, 1)', 'rgba(30, 144, 255, 1)')
      p1$legend(align = 'center', verticalAlign = 'bottom', layout = 'horizontal')
      p1$tooltip(formatter = "#! function() { return '店铺名称：'+this.x + ', 销售总量：' + this.y; } !#")
      p1$addParams(dom = 'myChart')
      return(p1)
    }
    
    if(input$Statistics_Model=='日订单趋势图'){
      
      result<-as.data.frame(table(mydf$format_xiadan_time),stringsAsFactors = FALSE)
      names(result)<-c('xiadantime','count')
      edit(result)
      result<-result[order(result$count,decreasing = TRUE),]
      p1 <- hPlot(count~xiadantime,data = result,type ="line",title = '日订单趋势',subtitle = '日订单趋势')
      p1$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '14px', fontFamily = '华文细黑')), replace = F)
      #p1$chart(zoomType = "xy",height=800)
      p1$exporting(enabled = T)
      p1$plotOptions(column = list(colorByPoint =TRUE))
      #p1$colors('rgba(30, 144,255, 1)', 'rgba(30, 144,255, 1)', 'rgba(30, 144, 255, 1)')
      p1$legend(align = 'center', verticalAlign = 'bottom', layout = 'horizontal')
      p1$tooltip(formatter = "#! function() { return '日期：'+this.x + ', 销售总量：' + this.y; } !#")
      p1$addParams(dom = 'myChart')
      return(p1)
    }
    
    if(input$Statistics_Model=='各时段订单趋势'){
      mydf<-aggregate(mydf$jishu,list(mydf$shiduan,mydf$format_xiadan_time),sum)
      names(mydf)<-c('shiduan','format_xiadan_time','jishu')
      edit(mydf)
      
      p1<-hPlot(jishu~shiduan,data=mydf,type="line",title='各时段订单趋势',group="format_xiadan_time",subtitle='各时段订单趋势')
      p1$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '16px', fontFamily = '华文细黑')), replace = F)
      #p1$chart(zoomType = "xy",height=800)
      p1$exporting(enabled = T)
      p1$legend(align = 'center', verticalAlign = 'bottom', layout = 'horizontal')
      p1$tooltip(formatter = "#! function() { return '时段：'+this.x + '点, 销量：' + this.y; } !#")
      p1$addParams(dom = 'myChart')
      return(p1)
    }
  })
})


