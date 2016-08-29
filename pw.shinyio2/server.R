library(markdown)
library(shiny)
library(DT)
library(ECharts2Shiny)
library(PW.GY)


shinyServer(function(input, output, session) {
#     # data init
#     producers_init_reac = reactive({
#         producers_init = format_data(input$pt_name,input$pt_amount,input$pt_price)
#     })
#     users_init_reac = reactive({
#         users_init = format_data(input$ut_name,input$ut_amount,input$ut_price)
#     })
#     # data after trading
#     return_list_reac = reactive({
#         producers_init = format_data(input$pt_name,input$pt_amount,input$pt_price)
#         users_init = format_data(input$ut_name,input$ut_amount,input$ut_price)
#         return_list = main_trading(producers_init,users_init,input$producer_proc/100)
#     })
#     e_producers_reac = reactive({
#         producers_accounts = return_list_reac()[[1]]
#         producers_accounts$amount_deal = producers_accounts$amount - producers_accounts$amount_left
#         producers_accounts$accu_money_ratio = producers_accounts$accu_money/sum(producers_accounts$accu_money)
#         producers_accounts$return_money = producers_accounts$accu_money_ratio*sum(producers_accounts$accu_money_diff)*0.5
#         producers_accounts$real_money = producers_accounts$accu_money -  producers_accounts$return_money
#         producers_accounts$real_price = producers_accounts$real_money/producers_accounts$amount_deal
#         reorder_columns = c('name','price','amount','amount_left','amount_deal','accu_money','accu_money_ratio','accu_money_diff','return_money',
#                             'real_money','real_price')
#         producers_accounts = producers_accounts[reorder_columns]
#         producers_accounts
#     })
#     e_users_reac = reactive({
#         users_accounts = return_list_reac()[[2]]
#         users_accounts$amount_deal = users_accounts$amount - users_accounts$amount_left
#         users_accounts$accu_money_ratio = users_accounts$accu_money/sum(users_accounts$accu_money)
#         users_accounts$return_money = users_accounts$accu_money_ratio*sum(users_accounts$accu_money_diff)*0.5
#         users_accounts$real_money = users_accounts$accu_money +  users_accounts$return_money
#         users_accounts$real_price = users_accounts$real_money/users_accounts$amount_deal
#         reorder_columns = c('name','price','amount','amount_left','amount_deal','accu_money','accu_money_ratio','accu_money_diff','return_money',
#                             'real_money','real_price')
#         users_accounts = users_accounts[reorder_columns]
#         users_accounts
#     })
#     e_df_reac = reactive({
#         df_accounts = return_list_reac()[[3]]
#         df_accounts
#     })
#
#
#     # init table
#     output$p_all_amounts = renderText({
#         paste(sum(as.numeric((strsplit(input$pt_amount,split=','))[[1]])),'万千瓦时')
#     })
#     output$u_all_amounts = renderText({
#         paste(sum(as.numeric((strsplit(input$ut_amount,split=','))[[1]])),'万千瓦时')
#     })
#
#     output$p_all_price = renderText({
#         paste(mean(as.numeric((strsplit(input$pt_price,split=','))[[1]])),'厘/千瓦时')
#     })
#     output$u_all_price = renderText({
#         paste(mean(as.numeric((strsplit(input$ut_price,split=','))[[1]])),'厘/千瓦时')
#     })
#
#
#     pu_table_cols = c('名称','申报量','申报价')
#     output$p_table = output_table(producers_init_reac())
#     output$u_table = output_table(users_init_reac())
#
#
#     # evaluate table
#     output$e_producers = renderTable({
#         producers_accounts = e_producers_reac()
#         colnames(producers_accounts) = c('名称', '申报价差', '申报电量', '剩余电量', '中标电量', '累计电费(不含返还)', '累计电费占比',
#                                          '累计价差电费', '返还电费', '中标电费(含返还)', '中标价差')
#         producers_accounts
#     })
#     output$e_users = renderTable({
#         users_accounts = e_users_reac()
#         colnames(users_accounts) = c('名称', '申报价差', '申报电量', '剩余电量', '中标电量', '累计电费(不含返还)', '累计电费占比',
#                                      '累计价差电费', '返还电费', '中标电费(含返还)', '中标价差')
#         users_accounts
#     })
#     output$e_df = renderTable({
#         df_accounts = e_df_reac()
#         colnames(df_accounts) = c('发电用户','发电申报量','发电申报价','用电用户','用电申报量','用电申报价','价差','成交电量')
#         df_accounts
#     })
#
#
#     output$plot <- renderPlot({
#         plot(cars, type = input$plotType)
#     })
#
#     output$summary <- renderPrint({
#         summary(cars)
#     })
#
#     output$table <- DT::renderDataTable({
#         DT::datatable(cars)
#     })
#
#     # plot-----------------------------------------------------
#     ## plot_df
#     observeEvent(e_df_reac(),
#                  {
#                      data = e_df_reac()[c('P_price','U_price','price_diff')]
#                      colnames(data) = c('卖电价格','买电价格','价格差')
#                      renderLineChart(div_id = "plot_df",data = data,step = "end",show.legend = TRUE)
#
#                      data2 = e_df_reac()[c('deal_amount','P_amount','U_amount')]
#                      colnames(data2) = c('成交数量','卖家申报量','买家申报量')
#                      renderBarChart(div_id = "plot_df2", data = data2, direction = "vertical")
#
#
#                  }
#     )
#     observeEvent(e_producers_reac(),
#                  {
#                      data = e_producers_reac()[c('name','amount_deal')]
#                      colnames(data) = c('name','value')
#                      renderPieChart(div_id = "plot_prod1",data = data,radius = "70%",center_x = "50%", center_y = "50%")
#
#                      data2 = e_producers_reac()[c('real_price','price')]
#                      colnames(data2) = c('实际价格','申报价格')
#                      renderBarChart(div_id = "plot_prod2", data = data2, direction = "vertical")
#
#                  }
#     )
#     observeEvent(e_users_reac(),
#                  {
#                      data = e_users_reac()[c('name','amount_deal')]
#                      colnames(data) = c('name','value')
#                      renderPieChart(div_id = "plot_user1",data = data,radius = "70%",center_x = "50%", center_y = "50%")
#
#                      data2 = e_users_reac()[c('real_price','price')]
#                      colnames(data2) = c('实际价格','申报价格')
#                      renderBarChart(div_id = "plot_user2", data = data2, direction = "vertical")
#
#                  }
#     )
#
#


})
