library(markdown)
library(shiny)
library(DT)
library(ECharts2Shiny)


format_data = function(pt_name,pt_amount,pt_price){
    #producer
    pt_name = strsplit(pt_name,split=',')[[1]]
    pt_amount = as.numeric(strsplit(pt_amount,split=',')[[1]])
    pt_price = as.numeric(strsplit(pt_price,split=',')[[1]])
    #df
    return(data.frame(name=pt_name,amount=pt_amount,price=pt_price,amount_left=pt_amount,accu_money=0,accu_money_diff=0))
}


main_trading = function(producers, users, deal_ratio_proc=0.8){
    #确定针对producers_inline和users_inline的指针------------------------------
    producer_pt = 1
    user_pt = 1
    #指定参数------------------------------------------------------------------
    deal_ratio_proc = deal_ratio_proc  #发电企业的成交比例
    deal_amounts = 0      #成交数量累计
    deal_amounts_max = sum(producers$amount)*deal_ratio_proc   #最大的成交数量
    df <- data.frame('P'= character(0),'P_amount'=numeric(0),'P_price'=numeric(0),
                     'U'= character(0),'U_amount'=numeric(0),'U_price'=numeric(0),
                     'price_diff'=numeric(0),'deal_amount'=numeric(0),stringsAsFactors=FALSE)
    #processes = list()     #累计每个交易流程
    flag_stop = FALSE  #判断是否到达成交比例的flag
    #排序producers和users-----------------------------------------------------
    library(plyr)
    producers = arrange(producers,price)
    users = arrange(users,desc(price))
    producers$name = as.character(producers$name)
    users$name = as.character(users$name)
    #算法开始-----------------------------------------------------------------
    while (  user_pt<=nrow(producers) & producer_pt<=nrow(users)  ){
        diff_amount = producers[producer_pt,]$amount_left - users[user_pt,]$amount_left
        diff_price = producers[producer_pt,]$price - users[user_pt,]$price
        # 只有当价差对为负的时候才有可能成交
        if (diff_price > 0)  break
        deal_amount =  min(producers[producer_pt,]$amount_left,users[user_pt,]$amount_left)
        deal_amounts = deal_amounts+deal_amount
        # 只有当交易量在规定范围内才有可能成交，否则将flag_stop设置为True
        if (deal_amounts > deal_amounts_max){
            curr_del_amount = deal_amounts_max - (deal_amounts-deal_amount)
            flag_stop = TRUE
        } else {
            curr_del_amount = deal_amount
        }
        # 针对每次交易过程的数据统计计算
        ##输出信息
        message = paste("P:",producers[producer_pt,]$name,"  U:",users[user_pt,]$name,"  P_left_Amount:",producers[producer_pt,]$amount_left,
                        "  U_left_Amount:",users[user_pt,]$amount_left,"  Deal_Amount:",curr_del_amount,"  Price_Diff:",
                        producers[producer_pt,]$price-users[user_pt,]$price,sep='')
        print(message)
        ##进程信息统计
        ##processes
        ##累计电费与累计价差电费
        users[user_pt,]$accu_money = users[user_pt,]$accu_money + users[user_pt,]$price*curr_del_amount
        users[user_pt,]$accu_money_diff = users[user_pt,]$accu_money_diff + (producers[producer_pt,]$price-users[user_pt,]$price)*curr_del_amount
        producers[producer_pt,]$accu_money = producers[producer_pt,]$accu_money + producers[producer_pt,]$price*curr_del_amount
        producers[producer_pt,]$accu_money_diff = producers[producer_pt,]$accu_money_diff + (producers[producer_pt,]$price-users[user_pt,]$price)*curr_del_amount
        ##数据库信息统计
        df[nrow(df)+1,] = c(producers[producer_pt,]$name,producers[producer_pt,]$amount,producers[producer_pt,]$price,
                            users[user_pt,]$name,users[user_pt,]$amount,users[user_pt,]$price,
                            producers[producer_pt,]$price-users[user_pt,]$price,curr_del_amount)
        ##判断终止条件
        if (flag_stop){
            users[user_pt,]$amount_left = users[user_pt,]$amount_left-curr_del_amount
            producers[producer_pt,]$amount_left = producers[producer_pt,]$amount_left-curr_del_amount
            break
        }
        if ( diff_amount == 0) {
            users[user_pt,]$amount_left = 0
            producers[producer_pt,]$amount_left = 0
            user_pt = user_pt+1
            producer_pt = producer_pt+1
        } else if ( diff_amount < 0) {
            users[user_pt,]$amount_left = users[user_pt,]$amount_left-curr_del_amount
            producers[producer_pt,]$amount_left = 0
            producer_pt = producer_pt+1
        } else {
            users[user_pt,]$amount_left = 0
            producers[producer_pt,]$amount_left = producers[producer_pt,]$amount_left-curr_del_amount
            user_pt = user_pt+1
        }

    }
    #return
    return (list(producers,users,df))
}


output_table <- function(data) {
    # colnames(data) = col_names
    options_table <- list(dom = 't', paging = FALSE, ordering = TRUE)
    table <- DT::renderDataTable(
        data, escape = FALSE, selection = 'none', server = FALSE,
        options = options_table
    )
    return(table)
}




shinyServer(function(input, output, session) {
    # 数据Reactive----------------------------------------------------------------------
    # 数据初始化，读入输入数据
    producers_init_reac = reactive({
        producers_init = format_data(input$pt_name,input$pt_amount,input$pt_price)
    })
    users_init_reac = reactive({
        users_init = format_data(input$ut_name,input$ut_amount,input$ut_price)
    })
    # 交易后的数据
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


    # tab 数据输入
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


    observeEvent(producers_init_reac(),
                 {
                     data = producers_init_reac()[c('name','amount','price')]
                     colnames(data) = c('名称','申报量','申报价')
                     output$p_table = output_table(data)
                 }
    )
    observeEvent(users_init_reac(),
                 {
                     data = users_init_reac()[c('name','amount','price')]
                     colnames(data) = c('名称','申报量','申报价')
                     output$u_table = output_table(data)
                 }
    )






    # plot-----------------------------------------------------
    ## plot_df
    observeEvent(e_df_reac(),
                 {
                     #table
                     data_t = e_df_reac()
                     colnames(data_t) = c('发电用户','发电申报量','发电申报价','用电用户','用电申报量','用电申报价','价差','成交电量')
                     output$e_df= output_table(data_t)

                     #plot
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
                     #table
                     data_t = e_producers_reac()
                     colnames(data_t) = c('名称', '申报价差', '申报电量', '剩余电量', '中标电量', '累计电费(不含返还)', '累计电费占比',
                                          '累计价差电费', '返还电费', '中标电费(含返还)', '中标价差')
                     output$e_producers= output_table(data_t)

                     #plot
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
                     #table
                     data_t = e_users_reac()
                     colnames(data_t) = c('名称', '申报价差', '申报电量', '剩余电量', '中标电量', '累计电费(不含返还)', '累计电费占比',
                                                  '累计价差电费', '返还电费', '中标电费(含返还)', '中标价差')
                     output$e_users= output_table(data_t)

                     #plot
                     data = e_users_reac()[c('name','amount_deal')]
                     colnames(data) = c('name','value')
                     renderPieChart(div_id = "plot_user1",data = data,radius = "70%",center_x = "50%", center_y = "50%")

                     data2 = e_users_reac()[c('real_price','price')]
                     colnames(data2) = c('实际价格','申报价格')
                     renderBarChart(div_id = "plot_user2", data = data2, direction = "vertical")

                 }
    )




})
