setClass(Class = 'Producer', slots = list(name = "character", amount = "numeric", price = "numeric",
                                          amount_left = "numeric", accu_money= "numeric", accu_money_diff= "numeric"),
         prototype = list(name = "0", amount = 0, price = 0,
                          amount_left = 0, accu_money= 0, accu_money_diff= 0))
setClass(Class = 'User', slots = list(name = "character", amount = "numeric", price = "numeric",
                                      amount_left = "numeric", accu_money= "numeric", accu_money_diff= "numeric"),
         prototype = list(name = "0", amount = 0, price = 0,
                          amount_left = 0, accu_money= 0, accu_money_diff= 0))

#' Producer class constructor.
#' @export
Producer = function(name,amount,price){
    return ( new('Producer',name=name,amount=amount,price=price,amount_left=amount) )
}

#' User class constructor.
#' @export
User = function(name,amount,price){
    return ( new('User',name=name,amount=amount,price=price,amount_left=amount) )
}
