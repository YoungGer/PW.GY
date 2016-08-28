#' Trading Algorithm.
#' @param producers producers dataframe.
#' @param users users dataframe.
#' @param deal_ratio_proc deal ratio in producers.
#' @return \code{data} which is extracted from extracted.
#' @export
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
