library(EpiModel)

cost <- function(dat, at){
    # browser()
    cost_for_s <- 100
    cost_for_i <- 200
    ## Attributes ##
    active <- dat$attr$active
    status <- dat$attr$status
    if (at == 2){
        dat$attr$cost <- rep(0, length(active))
        dat$epi$cumuCost <- 0
    }
    
    
    ## increment cost
    s_and_active <- status == "s" & active
    i_and_active <- status == "i" & active
    dat$attr$cost[s_and_active] <- 
        dat$attr$cost[s_and_active] + cost_for_s
    dat$attr$cost[i_and_active] <-
        dat$attr$cost[i_and_active] + cost_for_i
    
    ## Summary statistics
    totalStepCost <- (sum(s_and_active) * cost_for_s +
                      sum(i_and_active) * cost_for_i)
    dat$epi$totalStepCost[at] <- totalStepCost
    dat$epi$costPerCapita[at] <- totalStepCost/length(active)
    dat$epi$cumuCost[at] <- dat$epi$cumuCost[at - 1] + totalStepCost
    return(dat)
}