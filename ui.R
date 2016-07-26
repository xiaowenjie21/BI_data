library(shiny)
library(DT)
library(rCharts)
library(shinythemes)
library(stringr)

startDate=as.character(Sys.Date())

packageCheck = unlist(packageVersion("shiny"))

if(packageCheck[1] == 0 & packageCheck[2] < 9){
  
  shinyOld = TRUE
} else {
  shinyOld = FALSE
}
shinyUI(fluidPage(
  
  tags$head(
    tags$title('包师傅SHINY'),
    tags$link(rel = "stylesheet", href = "css/style.css",type='text/css')
    #tags$script(type = 'text/javascript', src = 'custom.js'),
   #tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  headerPanel(''),

  sidebarPanel(
    
    conditionalPanel(
      
      condition = "input.theTabs == 'Data_Display'",
      
      h3('数据浏览'),
      h5('观测数据趋势，下载数据表格')
      
    ),
    
    conditionalPanel(
      
      condition = "input.theTabs == 'Data_statistical'",
      
      h3('数据总览表格')
      
    ),
    
    conditionalPanel(
      
      condition = "input.theTabs == 'Charts'",
      
      h3('数据图表'),
      h5('数据统计图')
      
    ),
    
    conditionalPanel(
      
      condition = "input.theTabs == 'data_many'",
      
      helpText('平台现有美团,饿了么,百度外卖'),
      textInput("caption", "上次更新日期:", startDate),
      textInput('jishiqi','计时器',str_c('','秒'))
    ),

    conditionalPanel(
      
      condition = "input.theTabs != 'data_many'",
      
      textInput(inputId = 'key',label = '请输入店铺名称来浏览数据',value=''),
      helpText('为空表示查询所有'),
      
      
      selectInput("Product_Categorys", "请选择平台",list("美团","饿了么", "百度外卖")),
      
      
      
      radioButtons(inputId = "selecttype",
                   label = "查询统计类型",
                   choices = list("评论" = "comment",
                                  "订单记录" = "order",
                                  "访客流量" = "customer",
                                  "菜品统计"   = "nice"))
    ),
    
    conditionalPanel(
      
      condition = "input.theTabs == 'Charts'",
      
      selectInput("Statistics_Model", "请选择内置图表数据模型",list("星级分布图","评价折线图")),
      helpText('关于菜品统计只显示区间销量前100的数据')
      
    ),
    
    conditionalPanel(
      
      condition = "input.theTabs != 'data_many'",
      
      dateRangeInput("daterange_start", "请选择开始日期",
                     start = "2016-01-01",
                     end   = Sys.Date(),startview ='month',language = 'zh-CN',weekstart = 1),
      helpText(tags$code("右侧面板如果出现错误,请缩小日期范"))
    ),
    
    
    
    # conditionalPanel(
    #   
    #   condition = "input.theTabs != 'Data_Display'",
    #   #sliderInput(inputId = 'displaydatasC',label='前几页的数据统计',min = 1,value=1,max=100,step = 1)
    #   dateRangeInput("daterange_end", "请选择结束日期",
    #                  start = "2016-01-01",
    #                  end   = Sys.Date(),startview ='month',language = 'zh-CN',weekstart = 1)
    #   
    #   
    # ),
    # 
    
    
   # helpText('右侧面板如果出现错误,请缩小日期范围'),
   
    #实时更新，不需要按钮点击了
   # conditionalPanel(
   #   
   #   condition = "input.theTabs == 'data_many'",
   #   
   #   actionButton("go0", "更新看板",icon = icon("refresh"))
   #   
   #   
   # ),
   
   
    conditionalPanel(
      
      condition = "input.theTabs == 'Data_Display'",
      
      actionButton("go", "更新",icon = icon("refresh"))
      
      
    ),
    ###submitButton(text = 'Update Now')
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Data_statistical'",
      
      actionButton("go2", "更新数据表",icon = icon("refresh"))
      
      
    ),
    conditionalPanel(
      
      
      
      condition = "input.theTabs == 'Charts'",
      
      actionButton("go3", "更新图表",icon = icon("refresh"))
      
      
    )
    
  ),
  
  fluidPage( 
  mainPanel(
   
   tags$style(type="text/css", "body {padding-top: 50px;font-family:方正标雅宋}"),
   navbarPage("包师傅数据分析APP",position = 'fixed-top',inverse = TRUE,tabPanel('统计与展示',value='tongji',icon=icon('table')),tabPanel('报表与分析',value='baobiao',icon=icon('list-alt')),id='nav',
              theme=shinytheme('cosmo'),
              tabsetPanel(id="theTabs",
                           tabPanel('数据看板',htmlOutput('kanban'),value='data_many'),
                           tabPanel('数据解读',htmlOutput('mianmohtmloutput'),value="Data_Display",icon = icon('file')),
                           tabPanel('数据表',DT::dataTableOutput('DT'),value="Data_statistical",icon=icon('th')),
                           tabPanel('图表',showOutput("myChart","highcharts"),value='Charts',icon=icon('bar-chart'))
                          )
   )
  
)
)
)
)


