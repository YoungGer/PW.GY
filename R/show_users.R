#' Show producers or users.
#' @export
show_users = function(data){
    n_len = length(data)
    name = c()
    amount = c()
    price = c()
    amount_left = c()
    accu_money = c()
    accu_money_diff = c()
    for (i in 1:n_len){
        name = c(name,data[[i]]@name)
        amount = c(amount,data[[i]]@amount)
        price = c(price,data[[i]]@price)
        amount_left = c(amount_left,data[[i]]@amount_left)
        accu_money = c(accu_money,data[[i]]@accu_money)
        accu_money_diff = c(accu_money_diff,data[[i]]@accu_money_diff)
    }
    # return
    data.frame(name,amount,price,amount_left,accu_money,accu_money_diff)
}
