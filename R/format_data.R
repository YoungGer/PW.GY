#' Trading Algorithm.
#' @param producers producers dataframe.
#' @param users users dataframe.
#' @param deal_ratio_proc deal ratio in producers.
#' @return \code{data} which is extracted from extracted.
#' @export
format_data = function(pt_name,pt_amount,pt_price){
    #producer
    pt_name = strsplit(pt_name,split=',')[[1]]
    pt_amount = as.numeric(strsplit(pt_amount,split=',')[[1]])
    pt_price = as.numeric(strsplit(pt_price,split=',')[[1]])
    #df
    return(data.frame(name=pt_name,amount=pt_amount,price=pt_price,amount_left=pt_amount,accu_money=0,accu_money_diff=0))
}
