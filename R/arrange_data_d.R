


arrange_data_d <- function(.data){
  .data |> 
  select(participant, trial, order, response, c_pedestal, c_comparison) |> 
  mutate(correct = if_else((order == 1 & response == "right") | (order == 2 & response == "left"), 1, 0)) |> 
  mutate(c_dif= if_else(order == 1, c_comparison - c_pedestal, c_pedestal - c_comparison)) |> 
  group_by(c_pedestal, c_dif) |> 
  mutate(r= if_else(response == "right", 1,0))
}


# mutate(c_dif= if_else(order == 1, c_pedestal - c_comparison, c_comparison - c_pedestal)) |> 
#   group_by(c_pedestal, c_dif) |> 
#   mutate(r= if_else(response == "left", 1,0))