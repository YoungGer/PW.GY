library(markdown)
library(shiny)
library(DT)
library(ECharts2Shiny)
library(PW.GY)

ui = shinyUI(navbarPage(
    # We HAVE TO to load the ECharts javascript library in advance
    loadEChartsLibrary(),

    "售电模拟",

    tabPanel(
        "主页",
        fluidRow(
            column(8,
            offset=2,
            h1("应用简介"),
            br(),
            p("本售电模拟应用由华中科技大学的",strong("YoungGy"),"制作，其核心代码参照了",strong("YoungGy"),"的博文",
            tags$a("电力竞价(广东规则)详解与代码",href="http://blog.younggy.com/2016/08/26/%E7%94%B5%E5%8A%9B%E7%AB%9E%E4%BB%B7-%E5%B9%BF%E4%B8%9C%E8%A7%84%E5%88%99-%E8%AF%A6%E8%A7%A3%E4%B8%8E%E4%BB%A3%E7%A0%81/"),"。"),
            p("该代码及其应用的目的是:促进大家对电力竞价规则的了解，共同学习电力交易的相关规则，共同进步。"),
            p("本售电模拟应用及其相关代码仅供学习使用。如需转载请联系",strong("YoungGy"),"。"),
            tags$ul(tags$li("邮箱地址：771657815@qq.com"),tags$li("微信号：guangzhixingno1")),
            p("该代码的算例引用了售电市场研修院的部分内容，并感谢北京清软创新科技股份有限公司研究中心主任罗欣女士的讲解。"),

            h1("竞价规则"),
            br(),
            p("本竞价规则参考广东电力交易中心目前的竞价规则，详情可登陆",tags$a("广东省经济和信息化委员会网站",href="http://www.gdei.gov.cn/"),"查看。"),

            h2("交易规模"),
            p("交易牵涉两大角色，卖电方和买电方："),
            tags$ul(tags$li("卖电方：发电企业"),tags$li("买电方：售电公司、电力大用户")),
            p("交易牵涉一个变量"),
            tags$ul(tags$li("成交比例")),
            p("也就是说，买卖双方的电量不会全部成交。","例如，如果发电企业意向申报交易规模100万千瓦时，售电公司意向申报规模80万千瓦时，电力大用户意向申报20万千瓦时。
若设置成交比例为80%，则竞争交易电力规模为100*0.8=80万千瓦时。"),

            h2("报价规则"),
            p("报价规则牵涉的变量有："),
            tags$ul(tags$li("报价形式（双向报价，下面内容均特指该报价形式）"),tags$li("发电企业申报与上网电价的价差"),
            tags$li("发电企业申报与上网电价的价差范围"),tags$li("售电公司、电力大用户申报与现行目录电价中的价差"),tags$li("售电公司、电力大用户申报与现行目录电价中的价差范围")),
            p("所报价差必须在指定的价差范围内。"),
            p("例如：",
            div("发电企业申报价差范围：-50到-500厘/千瓦时。"),
            div("售电公司及电力用户申报价差范围：0到-100厘/千瓦时。"),
            div("发电企业上网电价为：500厘/千瓦时，申报价差：-100厘/千瓦时，表示期望卖出电价为400厘/千瓦时。"),
            div("售电公司及电力用户目录电价为500厘/千瓦时，申报价差为-50千瓦时，表示期望买入电价为450厘/千瓦时。"),

            h2("撮合规则"),
            p("这里，首先介绍下价差对的概念："),
            p("价差对=发电企业申报价差-售电公司或电力用户申报价差"),
            p("例如：发电企业申报价差为-100厘/千瓦时，电力大用户申报价差为-50厘/千瓦时，价差对为-100-（-50）= -50厘/千瓦时。"),
            p("需要注意的是："),
            tags$ul(tags$li("价差对只有是负的才可以成交，表示卖方对买方让利（试想，如果卖方一直保持高价格不对买方让利，交易怎么可能成交呢）。"),
                    tags$li("卖方（发电企业）肯定希望价差的绝对值越小越好，这样表示他让利让的少啊。"),
                    tags$li("买方（售电企业与电力用户）肯定希望价差的绝对值越大越好，因为这意味着自己买电的价格更低，从卖方获利越大。"),
                    tags$li("价差对绝对值越大的越优先成交（相当于卖家让利最大）。"),
                    tags$li("当价差对相同的时候，按申报价差对应电量比例确定中标电量。")),

            h2("结算规则"),
            p("结算规则主要涉及两点："),
            tags$ul(tags$li("价差电费返还：配对成交的买卖双方存在价差对，因此会产生价差电费，价差电费按照各自申报成交电费成比例返还给发电企业和大用户。"),
                    tags$li("价差返还系数：50%意味着价差电费在价差对双方之间平均分配。"),
                    tags$li("平均结算价格：（成交电量部分申报价差*成交电量+-返还电费）/总成交量")),

            h1("实例讲解"),
            p("下面附上一个例子，并附上相应代码仅供交流学习。"),

            h2("交易双方"),
            p("该交易涉及的发电企业与用电大用户数据如下："),
            img(src="http://7xlwwh.com1.z0.glb.clouddn.com/1.png"),

            h2("交易情况概述"),
            p("每轮的交易情况概述如下："),
            img(src="http://7xlwwh.com1.z0.glb.clouddn.com/2.png"),

            h2("交易结算"),
            p("发电企业的交易结算如下："),
            img(src="http://7xlwwh.com1.z0.glb.clouddn.com/3.png"),
            p("电力用户的交易结算如下："),
            img(src="http://7xlwwh.com1.z0.glb.clouddn.com/4.png")
            ))
        )
    ),

    tabPanel("数据输入",
        sidebarLayout(
            sidebarPanel(
                h3("市场数据输入"),
                selectInput("market", label = h4("市场竞价方式"),
                            choices = list("广东竞价" = 1),selected = 1),
                sliderInput("producer_proc", label = h4("成交比例(按生产者算)"), min = 0,
                            max = 100, value = 80,step=0.1),
                tags$hr(),
                br(),
                br(),
                h3("买卖方数据输入"),
                h4("发电公司数据输入"),


                textInput("pt_name", label = h5("名称"), value = "A,B,C,D,E"),
                textInput("pt_amount", label = h5("申购量"), value = "14975,18099,59015,53707,87809"),
                textInput("pt_price", label = h5("申购价"), value = "-500,-334.3,-145.7,-105.8,-60.6"),
                br(),
                h4("售电公司及电力用户数据输入"),
                textInput("ut_name", label = h5("名称"), value = "1,2,3,4,5"),
                textInput("ut_amount", label = h5("申购量"), value = "67983,39403,64478,38592,19799"),
                textInput("ut_price", label = h5("申购价"), value = "-45,-35.8,-26.7,-14.3,-0.1"),

                submitButton("提交")



            ),
            mainPanel(
                h4('供应侧共申报：'),
                verbatimTextOutput('p_all_amounts'),
                h4('供应侧平均申报价格为：'),
                verbatimTextOutput('p_all_price'),
                h4('需求侧共申报：'),
                verbatimTextOutput('u_all_amounts'),
                h4('需求侧平均申报价格为：'),
                verbatimTextOutput('u_all_price'),
                h4('发电公司的数据为：'),
                DT::dataTableOutput('p_table'),
                h4('售电公司及电力用户的数据为：'),
                DT::dataTableOutput('u_table')
            )
        )
    ),


    tabPanel("交易结果",
         sidebarLayout(
             sidebarPanel(
                 h3("交易结果"),
                 p("交易后的情况如右边的图表所示。")
             ),
             mainPanel(
                 h4('每轮的交易情况为：'),
                 tableOutput("e_df"),
                 h4('发电企业的交易结算如下：'),
                 textOutput("p_deal_max"),
                 textOutput("p_deal_min"),
                 textOutput("p_deal_mean"),

                 tableOutput("e_producers"),
                 h4('电力用户的交易结算如下：'),
                 textOutput("u_deal_max"),
                 textOutput("u_deal_min"),
                 textOutput("u_deal_mean"),

                 tableOutput("e_users")
             )
         )
    ),

    tabPanel("可视化",
             sidebarLayout(
                 sidebarPanel(
                     h3("交易及结算可视化"),
                     p("交易与结算结果的可视化如右边的图表所示。")
                 ),
                 mainPanel(
                     h1('交易可视化'),

                     h4("交易价格趋势", align = "center"),
                     tags$div(id="plot_df", style="width:100%;height:400px;"),
                     deliverChart(div_id = "plot_df"),

                     h4("交易量趋势", align = "center"),
                     tags$div(id="plot_df2", style="width:100%;height:400px;"),
                     deliverChart(div_id = "plot_df2"),

                     hr(),

                     h1('结算可视化'),

                     h2('发电公司结算'),

                     h4('发电公司中标电量及其占比',align = "center"),
                     tags$div(id="plot_prod1", style="width:100%;height:400px;"),
                     deliverChart(div_id = "plot_prod1"),

                     h4('发电公司中标价格与申报价格',align = "center"),
                     tags$div(id="plot_prod2", style="width:100%;height:400px;"),
                     deliverChart(div_id = "plot_prod2"),

                     h2('售电公司与电力用户结算'),

                     h4('售电公司与电力用户中标电量及其占比',align = "center"),
                     tags$div(id="plot_user1", style="width:100%;height:400px;"),
                     deliverChart(div_id = "plot_user1"),

                     h4('售电公司与电力用户中标电量及其占比中标价格与申报价格',align = "center"),
                     tags$div(id="plot_user2", style="width:100%;height:400px;"),
                     deliverChart(div_id = "plot_user2")


                 )
             )
    )

))


server = shinyServer(function(input, output, session) {
    # data init
    producers_init_reac = reactive({
        producers_init = format_data(input$pt_name,input$pt_amount,input$pt_price)
    })
    users_init_reac = reactive({
        users_init = format_data(input$ut_name,input$ut_amount,input$ut_price)
    })
    # data after trading
    return_list_reac = reactive({
        producers_init = format_data(input$pt_name,input$pt_amount,input$pt_price)
        users_init = format_data(input$ut_name,input$ut_amount,input$ut_price)
        return_list = main_trading(producers_init,users_init,input$producer_proc/100)
    })
    e_producers_reac = reactive({
        producers_accounts = return_list_reac()[[1]]
        producers_accounts$amount_deal = producers_accounts$amount - producers_accounts$amount_left
        producers_accounts$accu_money_ratio = producers_accounts$accu_money/sum(producers_accounts$accu_money)
        producers_accounts$return_money = producers_accounts$accu_money_ratio*sum(producers_accounts$accu_money_diff)*0.5
        producers_accounts$real_money = producers_accounts$accu_money -  producers_accounts$return_money
        producers_accounts$real_price = producers_accounts$real_money/producers_accounts$amount_deal
        reorder_columns = c('name','price','amount','amount_left','amount_deal','accu_money','accu_money_ratio','accu_money_diff','return_money',
                            'real_money','real_price')
        producers_accounts = producers_accounts[reorder_columns]
        producers_accounts
    })
    e_users_reac = reactive({
        users_accounts = return_list_reac()[[2]]
        users_accounts$amount_deal = users_accounts$amount - users_accounts$amount_left
        users_accounts$accu_money_ratio = users_accounts$accu_money/sum(users_accounts$accu_money)
        users_accounts$return_money = users_accounts$accu_money_ratio*sum(users_accounts$accu_money_diff)*0.5
        users_accounts$real_money = users_accounts$accu_money +  users_accounts$return_money
        users_accounts$real_price = users_accounts$real_money/users_accounts$amount_deal
        reorder_columns = c('name','price','amount','amount_left','amount_deal','accu_money','accu_money_ratio','accu_money_diff','return_money',
                            'real_money','real_price')
        users_accounts = users_accounts[reorder_columns]
        users_accounts
    })
    e_df_reac = reactive({
        df_accounts = return_list_reac()[[3]]
        df_accounts
    })


    # init table
    output$p_all_amounts = renderText({
        paste(sum(as.numeric((strsplit(input$pt_amount,split=','))[[1]])),'万千瓦时')
    })
    output$u_all_amounts = renderText({
        paste(sum(as.numeric((strsplit(input$ut_amount,split=','))[[1]])),'万千瓦时')
    })

    output$p_all_price = renderText({
        paste(mean(as.numeric((strsplit(input$pt_price,split=','))[[1]])),'厘/千瓦时')
    })
    output$u_all_price = renderText({
        paste(mean(as.numeric((strsplit(input$ut_price,split=','))[[1]])),'厘/千瓦时')
    })

    output$p_deal_max = renderText({
        producers_accounts = e_producers_reac()
        d = max(producers_accounts[,ncol(producers_accounts)])
        paste('成交最高价为',d,'厘/千瓦时')
    })
    output$p_deal_min = renderText({
        producers_accounts = e_producers_reac()
        d = min(producers_accounts[,ncol(producers_accounts)])
        paste('成交最低价为',d,'厘/千瓦时')
    })
    output$p_deal_mean = renderText({
        producers_accounts = e_producers_reac()
        d = mean(producers_accounts[,ncol(producers_accounts)])
        paste('成交平均价为',d,'厘/千瓦时')
    })
    output$u_deal_max = renderText({
        users_accounts = e_users_reac()
        d = max(users_accounts[,ncol(users_accounts)])
        paste('成交最高价为',d,'厘/千瓦时')
    })
    output$u_deal_min = renderText({
        users_accounts = e_users_reac()
        d = min(users_accounts[,ncol(users_accounts)])
        paste('成交最低价为',d,'厘/千瓦时')
    })
    output$u_deal_mean = renderText({
        users_accounts = e_users_reac()
        d = mean(users_accounts[,ncol(users_accounts)])
        paste('成交平均价为',d,'厘/千瓦时')
    })


    pu_table_cols = c('名称','申报量','申报价')
    output$p_table = output_table(producers_init_reac())
    output$u_table = output_table(users_init_reac())


    # evaluate table
    output$e_producers = renderTable({
        producers_accounts = e_producers_reac()
        colnames(producers_accounts) = c('名称', '申报价差', '申报电量', '剩余电量', '中标电量', '累计电费(不含返还)', '累计电费占比',
                                         '累计价差电费', '返还电费', '中标电费(含返还)', '中标价差')
        producers_accounts
    })
    output$e_users = renderTable({
        users_accounts = e_users_reac()
        colnames(users_accounts) = c('名称', '申报价差', '申报电量', '剩余电量', '中标电量', '累计电费(不含返还)', '累计电费占比',
                                     '累计价差电费', '返还电费', '中标电费(含返还)', '中标价差')
        users_accounts
    })
    output$e_df = renderTable({
        df_accounts = e_df_reac()
        colnames(df_accounts) = c('发电用户','发电申报量','发电申报价','用电用户','用电申报量','用电申报价','价差','成交电量')
        df_accounts
    })


    output$plot <- renderPlot({
        plot(cars, type = input$plotType)
    })

    output$summary <- renderPrint({
        summary(cars)
    })

    output$table <- DT::renderDataTable({
        DT::datatable(cars)
    })

    # plot-----------------------------------------------------
    ## plot_df
    observeEvent(e_df_reac(),
        {
            data = e_df_reac()[c('P_price','U_price','price_diff')]
            colnames(data) = c('卖电价格','买电价格','价格差')
            renderLineChart(div_id = "plot_df",data = data,step = "end",show.legend = TRUE)

            data2 = e_df_reac()[c('deal_amount','P_amount','U_amount')]
            colnames(data2) = c('成交数量','卖家申报量','买家申报量')
            renderBarChart(div_id = "plot_df2", data = data2, direction = "vertical")


        }
    )
    observeEvent(e_producers_reac(),
        {
            data = e_producers_reac()[c('name','amount_deal')]
            colnames(data) = c('name','value')
            renderPieChart(div_id = "plot_prod1",data = data,radius = "70%",center_x = "50%", center_y = "50%")

            data2 = e_producers_reac()[c('real_price','price')]
            colnames(data2) = c('实际价格','申报价格')
            renderBarChart(div_id = "plot_prod2", data = data2, direction = "vertical")

        }
    )
    observeEvent(e_users_reac(),
        {
            data = e_users_reac()[c('name','amount_deal')]
            colnames(data) = c('name','value')
            renderPieChart(div_id = "plot_user1",data = data,radius = "70%",center_x = "50%", center_y = "50%")

            data2 = e_users_reac()[c('real_price','price')]
            colnames(data2) = c('实际价格','申报价格')
            renderBarChart(div_id = "plot_user2", data = data2, direction = "vertical")

        }
    )




})


shinyApp(ui = ui,server = server)
