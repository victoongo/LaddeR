morgage <- function (dp, sval, eval, year, ymorgagerate, propertytax, newrenterfee,
                     rent, monthlyadj=0, hoafee=100, managefeerate, empty=1, sellrate=0.0125) {
  loan <- sval-dp
  mmorgagerate <- ymorgagerate/12
  month <- year*12
  morgagemonthlypay <- loan * mmorgagerate*(1+mmorgagerate)^month / ((1+mmorgagerate)^month-1)
#   print(morgagemonthlypay)
  monthlyadj <- morgagemonthlypay + (rent*managefeerate + hoafee)/1000 +
                sval*propertytax/12 + newrenterfee/1000/12 -
                (rent/1000)*((12-empty)/12)
  totalpay <- dp
  print(totalpay)
#   if (monthlyadj >0) totalpay <- totalpay + monthlyadj*month
#   else eval <- eval - monthlyadj*month
  totalpay <- totalpay + monthlyadj*month
  print(totalpay)
#   print((monthlyadj + rent*managefeerate + hoafee)/1000)
#   print(monthlyadj)
  eval <- eval*(1-sellrate)
  gainr <- (eval/totalpay)^(1/year)-1
  print(gainr)
  if (monthlyadj<0) gainr <- gainr - monthlyadj*12/dp
  mtotalpay <- (totalpay-dp)/month
  all <- c(morgagemonthlypay, monthlyadj, totalpay, mtotalpay, eval, gainr)
  return(all)
}


morgage(40, 200, 250, 15, .035, 0.01, 1000, 1200, 0, 100, 0.07, 1)
morgage(80, 200, 250, 15, .035, 0.01, 1000, 1200, 0, 100, 0.07, 1)
morgage(120, 200, 250, 15, .035, 0.01, 1000, 1200, 0, 100, 0.07, 1)
morgage(160, 200, 250, 15, .035, 0.01, 1000, 1200, 0, 100, 0.07, 1)
