producers_name = c('A','B','C','D','E')
producers_amount = c(14975,18099,59015,53707,87809)
producers_price = c(-500,-334.3,-145.7,-105.8,-60.6)

users_name = c('1','2','3','4','5')
users_amount = c(67983,39403,64478,38592,19799)
users_price = c(-45,-35.8,-26.7,-14.3,-0.1)

producers = list()
for (i in 1:length(producers_name)) {
    name = producers_name[i]
    amount = producers_amount[i]
    price = producers_price[i]
    producers = c(producers,Producer(name,amount,price))
}

users = list()
for (i in 1:length(producers_name)) {
    name = users_name[i]
    amount = users_amount[i]
    price = users_price[i]
    users = c(users,User(name,amount,price))
}


# df
users_df = show_users(users)
producers_df = show_users(producers)


# sort
library(plyr)
producers_inline_df = arrange(producers_df,price)
users_inline_df = arrange(users_df,desc(price))

# algo---------------------------
# 确定针对producers_inline和users_inline的指针
producer_pt = 1
user_pt = 1
# 指定参数
deal_ratio_proc = 0.8  #发电企业的成交比例
deal_amounts = 0      #成交数量累计
deal_amounts_max = sum(producers_amount)*deal_ratio_proc   #最大的成交数量
df = pd.DataFrame()  #数据框初始化
df_colnames = c('发电企业','申报电量', '申报价差','售电公司', '申报电量', '申报价差', '价差', '成交电量')
df_i = 0   #数据框的行指针
processes = list()     #累计每个交易流程
flag_stop = FALSE  #判断是否到达成交比例的flag


while (  user_pt<=nrow(users_inline_df) & producer_pt<=nrow(producers_inline_df) ){
    #
    cur_producer = producers_inline_df[producer_pt]
    cur_user = users_inline_df[user_pt]
    #
    diff_amount = producers_inline_df[producer_pt,]$amount_left - cur_user.amount_left
    diff_price = float(cur_producer.price) - float(cur_user.price)
}
