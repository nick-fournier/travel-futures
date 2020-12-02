
library(data.table)
library(ggplot2)
library(scales)
# library(extrafont)

#### Functions ####

#Pricing function
fun.price <- function(k, pmin, pmax, a, b)  pmin + (pmax-pmin)/(1 + exp(a-b*k))

#Elasticity function
fun.elasticdemand <- function(k,p1,p2,e) k + k*(exp(-e*(p2-p1)/(p2+p1))-1)
#fun.elasticdemand <- function(k,p1,p2,e) k*((p2-p1)/p1)^-e

#Demand distribution (Manually constructed for this example)
fun.demanddist <- function(scale, t,sd) scale*(dnorm(t,mean=8,sd) + dnorm(t,mean=17,sd) + dnorm(t,mean=12,sd=4) + dnorm(t,mean=14,sd=4))

#Flow-density function, linear-parabolic
fun.flowdensity_linpara <- function(k) {
  k0 = -(k_j^2 - 2*k_j*k_c - sqrt(k_j*(-2*k_c^3 + 5*k_j*k_c^2 - 4*k_c*k_j^2 + k_j^3))) / k_c
  
  sapply(k, function(k)
    if(k <= k0) 
      v_f*k
    else
      (k_c*v_f/2)*(1 - ((k - k_c)^2)/((k_j - k_c)^2))
  )
}

#Flow-density function, double parabolic
fun.flowdensity <- function(k) {
  sapply(k, function(k)
    if(k <= k_c) 
      k*v_f*(1 - (k/(2*k_c)))
    else
      (k_c*v_f/2)*(1 - ((k - k_c)^2)/((k_j - k_c)^2))
  )
}

#Flow-density function, linear by daganzo
fun.flowdensity_dag <- function(k) {
  sapply(k, function(k)
    if(k <= k_c/2)
      v_f*k
    else
      (v_f*k_c/2)*(1 + ( (k_c - 2*k)/(2*k_j - k_c) ) )
  )
}

#Flow-density function, linear by greenshields
fun.flowdensity_green <- function(k) {
  sapply(k, function(k)
    k*v_f*(1 - (k/(2*k_c))) 
  )
}

#Color coding for plotting
fun.coder <- function(vals, step, direction = 1) {
  
  if(is.na(step)) step <- ceiling(diff(ends)/11)
  
  ends <- step*c(floor(100*range(vals)[1]/step),
                 ceiling(100*range(vals)[2]/step))
  cuts <- seq(ends[1], ends[2], by = step)
  labs <- paste0(paste(round(cuts)[-length(cuts)], 
                       round(cuts[-1]), sep = " to "),"%")
  if(direction == 1) pal = c("Reds","Blues") else if(direction == -1) pal = c("Blues","Reds")
  
  colors <- c(brewer_pal(palette = pal[1], direction = -1)(length(cuts[cuts<0])),
              brewer_pal(palette = pal[2])(length(cuts[cuts>=0])))
  
  return(list("cuts" = cuts, "labs" = labs, "colors" = colors))
}

#### Parameters ####
discount = 0.5
surcharge = 2
L = 7
v_f = 100 #Free flow speed
k_j = 120 #Jam density
k_c = 30 #Critical density
E = c("0" = 0.1, "low" = 0.3, "medium" = 0.5, "high" = 0.7)
trips = 100000
inc = 5/60
t = seq(0,24,by=inc)
a = 7
b = 0.3
pfix = 1
nlanes = 6 #lanes



#### Run with constant prices and varying elasticity ####
#Set up combination matrix
dat.elas = as.data.table(expand.grid(tval = t,
                                     elas = E))

#Assign fixed price & scaled price
dat.elas[ , price1 := pfix]
dat.elas[ , pmax := pfix*surcharge]
dat.elas[ , pmin := pfix*discount]
#Calculate demand dist for each combo
dat.elas[ , k1 := fun.demanddist(85, tval, 1.5), by = .(elas)]
#Calculate flow and number of trips for fixed price
dat.elas[ , mu1 := trips*k1/sum(k1)/inc/nlanes, by = .(elas)]
dat.elas[ , q1 := fun.flowdensity(k1), by = .(elas)]
dat.elas[ , n1 := trips*k1/sum(k1), by = .(elas)]
dat.elas[ , v1 := q1/k1, by = .(elas)]
#Calculate new price the previous demand
dat.elas[ , price2 := fun.price(k1, pmin, pmax, a, b), by = .(elas)]
#Calculate new density, flow, and number of trips from price
dat.elas[ , k2 := fun.elasticdemand(k1,price1,price2,elas), by = .(elas)]
dat.elas[ , k2 := sum(k1)*k2/sum(k2), by = .(elas)]
dat.elas[ , mu2 := trips*k2/sum(k2)/inc/nlanes, by = .(elas)]
dat.elas[ , q2 := fun.flowdensity(k2), by = .(elas)]
dat.elas[ , n2 := trips*k2/sum(k2), by = .(elas)]
dat.elas[ , v2 := q2/k2, by = .(elas)]

#Delay
dat.elas[ , d1 := n1*L*((1/v1) - (1/v_f)), by = .(elas)]
dat.elas[ , d2 := n2*L*((1/v2) - (1/v_f)), by = .(elas)]
# dat.elas[ , delaydiff_i := (n2*v1*(v_f - v2))/(n1*v2*(v_f - v1)) - 1, by = .(elas)]

#Revenue
dat.elas[ , rev1 := n1*price1, by = .(elas)]
dat.elas[ , rev2 := n2*price2, by = .(elas)]
#Cumulative sum
dat.elas[ , sumrev1 := cumsum(rev1), by = .(elas)]
dat.elas[ , sumrev2 := cumsum(rev2), by = .(elas)]

#Time
dat.elas[ , time := as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*tval), by = .(elas)]

# #Check sum, make sure the same number of total trips happen
# dat.elas[ , lapply(.SD, sum),.SDcols = c("k1","k2"), by = .(elas)][, all.equal(k1,k2)]
# dat.elas[ , lapply(.SD, sum),.SDcols = c("mu1","mu2"), by = .(elas)][, all.equal(mu1,mu2)]
# dat.elas[ , lapply(.SD, sum),.SDcols = c("n1","n2"), by = .(elas)][, all.equal(n1,n2)]
# dat.elas[ , lapply(.SD, sum),.SDcols = c("q1","q2"), by = .(elas)][, all.equal(q1,q2)]


#### Run with varying pmax and pmin ####
#Set up combination matrix
dat.maxmin = as.data.table(expand.grid(tval = t,
                                   dis = seq(0, 1, by = 0.01),
                                   sur = seq(0, 4, by = 0.01)))
#Assign fixed price & scaled price
dat.maxmin[ , price1 := pfix]
dat.maxmin[ , Pmin := pfix*dis]
dat.maxmin[ , Pmax := pfix*sur]
#Calculate demand dist for each combo
dat.maxmin[ , k1 := fun.demanddist(85, tval, 1.5), by = .(dis,sur)]
#Calculate flow and number of trips for fixed price
dat.maxmin[ , mu1 := trips*k1/sum(k1)/inc/nlanes, by = .(dis,sur)]
dat.maxmin[ , q1 := fun.flowdensity(k1), by = .(dis,sur)]
dat.maxmin[ , n1 := trips*k1/sum(k1), by = .(dis,sur)]
dat.maxmin[ , v1 := q1/k1, by = .(dis,sur)]
#Calculate new price the previous demand
dat.maxmin[ , price2 := fun.price(k1, Pmin, Pmax, a, b)]
#Calculate new density, flow, and number of trips from price
dat.maxmin[ , k2 := fun.elasticdemand(k1,price1,price2,E['low']), by = .(dis,sur)]
dat.maxmin[ , k2 := sum(k1)*k2/sum(k2), by = .(dis,sur)]
dat.maxmin[ , mu2 := trips*k2/sum(k2)/inc/nlanes, by = .(dis,sur)]
dat.maxmin[ , q2 := fun.flowdensity(k2), by = .(dis,sur)]
dat.maxmin[ , n2 := trips*k2/sum(k2), by = .(dis,sur)]
dat.maxmin[ , v2 := q2/k2, by = .(dis,sur)]

#Delay
dat.maxmin[ , d1 := n1*L*((1/v1) - (1/v_f)), by = .(dis,sur)]
dat.maxmin[ , d2 := n2*L*((1/v2) - (1/v_f)), by = .(dis,sur)]
# dat.maxmin[ , delaydiff_i := (n2*v1*(v_f - v2))/(n1*v2*(v_f - v1)) - 1, by = .(dis,sur)]

#Revenue
dat.maxmin[ , rev1 := n1*price1]
dat.maxmin[ , rev2 := n2*price2]
#Cumulative sum
dat.maxmin[ , sumrev1 := cumsum(rev1)]
dat.maxmin[ , sumrev2 := cumsum(rev2)]

#Time
dat.maxmin[ , time := as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*tval)]

# #Check sum, make sure the same number of total trips happen
# dat.maxmin[ , lapply(.SD, sum),.SDcols = c("k1","k2"), by = .(dis,sur)][, all.equal(k1,k2)]
# dat.maxmin[ , lapply(.SD, sum),.SDcols = c("mu1","mu2"), by = .(dis,sur)][, all.equal(mu1,mu2)]
# dat.maxmin[ , lapply(.SD, sum),.SDcols = c("n1","n2"), by = .(dis,sur)][, all.equal(n1,n2)]
# dat.maxmin[ , lapply(.SD, sum),.SDcols = c("q1","q2"), by = .(dis,sur)][, all.equal(q1,q2)]

#Export the percent difference between aggregate total revenue and travel time
mat.maxmin <- dat.maxmin[ , list("revdiff" = (sum(rev2 - rev1))/sum(rev1),
                                 "delaydiff" = sum(d2 - d1)/sum(d1),
                                 "delaydiff2" = (sum((n2/v2)*(v_f - v2)) / sum((n1/v1)*(v_f - v1))) - 1 ), by = .(dis,sur)]

# #Minimum pmax for revenue neutrality
# minsurge_rev <- mat.elas[round(elas,4) == E & revdiff >= 0, ][which.min(revdiff), surge]

#Use this Pmax?
#pmax = pfix*minsurge_rev


#### Run with varying elasticity and pmax ####
#Set up combination matrix
dat.maxelas = as.data.table(expand.grid(tval = t,
                                        elas = seq(0, 2, by = 0.01),
                                        sur = seq(0, 4, by = 0.01)))
#Assign fixed price & scaled price
dat.maxelas[ , price1 := pfix]
dat.maxelas[ , Pmin := pfix*0]
dat.maxelas[ , Pmax := pfix*sur]
#Calculate demand dist for each combo
dat.maxelas[ , k1 := fun.demanddist(85, tval, 1.5), by = .(sur,elas)]
#Calculate flow and number of trips for fixed price
dat.maxelas[ , mu1 := trips*k1/sum(k1)/inc/nlanes, by = .(sur,elas)]
dat.maxelas[ , q1 := fun.flowdensity(k1), by = .(sur,elas)]
dat.maxelas[ , n1 := trips*k1/sum(k1), by = .(sur,elas)]
dat.maxelas[ , v1 := q1/k1, by = .(sur,elas)]
#Calculate new price the previous demand
dat.maxelas[ , price2 := fun.price(k1, Pmin, Pmax, a, b)]
#Calculate new density, flow, and number of trips from price
dat.maxelas[ , k2 := fun.elasticdemand(k1,price1,price2,elas), by = .(sur,elas)]
dat.maxelas[ , k2 := sum(k1)*k2/sum(k2), by = .(sur,elas)]
dat.maxelas[ , mu2 := trips*k2/sum(k2)/inc/nlanes, by = .(sur,elas)]
dat.maxelas[ , q2 := fun.flowdensity(k2), by = .(sur,elas)]
dat.maxelas[ , n2 := trips*k2/sum(k2), by = .(sur,elas)]
dat.maxelas[ , v2 := q2/k2, by = .(sur,elas)]

#Delay
dat.maxelas[ , d1 := n1*L*((1/v1) - (1/v_f)), by = .(sur,elas)]
dat.maxelas[ , d2 := n2*L*((1/v2) - (1/v_f)), by = .(sur,elas)]
# dat.maxelas[ , delaydiff_i := (n2*v1*(v_f - v2))/(n1*v2*(v_f - v1)) - 1, by = .(sur,elas)]

#Revenue
dat.maxelas[ , rev1 := n1*price1, by = .(sur,elas)]
dat.maxelas[ , rev2 := n2*price2, by = .(sur,elas)]
#Cumulative sum
dat.maxelas[ , sumrev1 := cumsum(rev1), by = .(sur,elas)]
dat.maxelas[ , sumrev2 := cumsum(rev2), by = .(sur,elas)]

#Time
dat.maxelas[ , time := as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*tval)]

# #Check sum, make sure the same number of total trips happen
# dat.maxelas[ , lapply(.SD, sum),.SDcols = c("k1","k2"), by = .(sur,elas)][, all.equal(k1,k2)]
# dat.maxelas[ , lapply(.SD, sum),.SDcols = c("mu1","mu2"), by = .(sur,elas)][, all.equal(mu1,mu2)]
# dat.maxelas[ , lapply(.SD, sum),.SDcols = c("n1","n2"), by = .(sur,elas)][, all.equal(n1,n2)]
# dat.maxelas[ , lapply(.SD, sum),.SDcols = c("q1","q2"), by = .(sur,elas)][, all.equal(q1,q2)]

#Export the percent difference between aggregate total revenue and travel time
mat.maxelas <- dat.maxelas[ , list("revdiff" = (sum(rev2 - rev1))/sum(rev1),
                                         "delaydiff" = sum(d2 - d1)/sum(d1),
                                         "delaydiff2" = (sum((n2/v2)*(v_f - v2)) / sum((n1/v1)*(v_f - v1))) - 1 ),
                                 by = .(sur,elas)]



#### Run with varying elasticity and pmax and pmin ####
#Set up combination matrix
dat.maxminelas = as.data.table(expand.grid(tval = t,
                                           #elas = c(0, seq(0.1, 1.5,by = 0.2)),
                                           elas = E,
                                           dis = seq(0, 1, by = 0.05),
                                           sur = seq(0, 4, by = 0.05)))
#Assign fixed price & scaled price
dat.maxminelas[ , price1 := pfix]
dat.maxminelas[ , Pmin := pfix*dis]
dat.maxminelas[ , Pmax := pfix*sur]
#Calculate demand dist for each combo
dat.maxminelas[ , k1 := fun.demanddist(85, tval, 1.5), by = .(dis,sur,elas)]
#Calculate flow and number of trips for fixed price
dat.maxminelas[ , mu1 := trips*k1/sum(k1)/inc/nlanes, by = .(dis,sur,elas)]
dat.maxminelas[ , q1 := fun.flowdensity(k1), by = .(dis,sur,elas)]
dat.maxminelas[ , n1 := trips*k1/sum(k1), by = .(dis,sur,elas)]
dat.maxminelas[ , v1 := q1/k1, by = .(dis,sur,elas)]
#Calculate new price the previous demand
dat.maxminelas[ , price2 := fun.price(k1, Pmin, Pmax, a, b)]
#Calculate new density, flow, and number of trips from price
dat.maxminelas[ , k2 := fun.elasticdemand(k1,price1,price2,elas), by = .(dis,sur,elas)]
dat.maxminelas[ , k2 := sum(k1)*k2/sum(k2), by = .(dis,sur,elas)]
dat.maxminelas[ , mu2 := trips*k2/sum(k2)/inc/nlanes, by = .(dis,sur,elas)]
dat.maxminelas[ , q2 := fun.flowdensity(k2), by = .(dis,sur,elas)]
dat.maxminelas[ , n2 := trips*k2/sum(k2), by = .(dis,sur,elas)]
dat.maxminelas[ , v2 := q2/k2, by = .(dis,sur,elas)]

#Delay
dat.maxminelas[ , d1 := n1*L*((1/v1) - (1/v_f)), by = .(dis,sur,elas)]
dat.maxminelas[ , d2 := n2*L*((1/v2) - (1/v_f)), by = .(dis,sur,elas)]
# dat.maxminelas[ , delaydiff_i := (n2*v1*(v_f - v2))/(n1*v2*(v_f - v1)) - 1, by = .(dis,sur,elas)]

#Revenue
dat.maxminelas[ , rev1 := n1*price1, by = .(dis,sur,elas)]
dat.maxminelas[ , rev2 := n2*price2, by = .(dis,sur,elas)]
#Cumulative sum
dat.maxminelas[ , sumrev1 := cumsum(rev1), by = .(dis,sur,elas)]
dat.maxminelas[ , sumrev2 := cumsum(rev2), by = .(dis,sur,elas)]

#Time
dat.maxminelas[ , time := as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*tval)]

# #Check sum, make sure the same number of total trips happen
# dat.maxminelas[ , lapply(.SD, sum),.SDcols = c("k1","k2"), by = .(dis,sur,elas)][, all.equal(k1,k2)]
# dat.maxminelas[ , lapply(.SD, sum),.SDcols = c("mu1","mu2"), by = .(dis,sur,elas)][, all.equal(mu1,mu2)]
# dat.maxminelas[ , lapply(.SD, sum),.SDcols = c("n1","n2"), by = .(dis,sur,elas)][, all.equal(n1,n2)]
# dat.maxminelas[ , lapply(.SD, sum),.SDcols = c("q1","q2"), by = .(dis,sur,elas)][, all.equal(q1,q2)]

#Export the percent difference between aggregate total revenue and travel time
mat.maxminelas <- dat.maxminelas[ , list("revdiff" = (sum(rev2 - rev1))/sum(rev1),
                                         "delaydiff" = sum(d2 - d1)/sum(d1),
                                         "delaydiff2" = (sum((n2/v2)*(v_f - v2)) / sum((n1/v1)*(v_f - v1))) - 1 ),
                                  by = .(dis,sur,elas)]

# #Constant prices, simple plots
# dat.elas <- dat.maxminelas[dis == discount & sur == surcharge & elas %in% as.character(c(E['0'], E['low'], E['high'])), ]

# #Constant elasticity
# mat.maxmin <- mat.maxminelas[elas == as.character(E['low']), ]

minsur <- as.matrix(mat.maxelas[elas == as.character(E['low']) & delaydiff<0 & revdiff>0, ][which.min(revdiff), ])[1,]


#Simple revenue
# simprev <- dat.elas[ , sum(rev2)/sum(rev1), by=elas][['V1']]

#### Saving data ####
pars = c("a","b","surcharge","discount","E","pfix","inc","k_c","k_j","L","v_f","trips","nlanes")
dats = c("dat.elas", "mat.maxelas","mat.maxmin","mat.maxminelas","minsur")
funs = ls()[grepl("fun",ls())]

save(list = c(pars,dats,funs), file = "toydata.RData", version = 2)




































