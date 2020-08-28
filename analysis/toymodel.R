
library(data.table)
library(ggplot2)
library(scales)
# library(extrafont)

#### Functions ####

#Pricing function
fun.price <- function(k, pmin, pmax, a, b)  pmin + (pmax-pmin)/(1 + exp(a-b*k))

#Elasticity function
fun.elasticdemand <- function(k,p1,p2,E) k + k*(exp(-E*(p2-p1)/(p2+p1))-1)
#fun.elasticdemand <- function(k,p1,p2,E) k*((p2-p1)/p1)^-E

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

#### Parameters ####
L = 7
v_f = 100 #Free flow speed
k_j = 120 #Jam density
k_c = 30 #Critical density
E = 0.3
trips = 100000
inc = 5/60
t = seq(0,24,by=inc)
a = 7
b = 0.3
pfix = 8
pmin = 0*pfix/2
pmax = pfix*4
nlanes = 6 #lanes

#### Run with varying pmax and elasticity ####
#Set up combination matrix
datvar = as.data.table(expand.grid(tval = t,
                                   elas = seq(0,2, by=0.025),
                                   surge = seq(0, 4, by = 0.05)))
#Assign fixed price & scaled price
datvar[ , price1 := pfix]
datvar[ , pmax := pfix*surge]
#Calculate demand dist for each combo
datvar[ , k1 := fun.demanddist(85, tval, 1.5), by = .(elas,surge)]
#Calculate flow and number of trips for fixed price
datvar[ , mu1 := trips*k1/sum(k1)/inc/nlanes, by = .(elas,surge)]
datvar[ , q1 := fun.flowdensity(k1), by = .(elas,surge)]
datvar[ , n1 := trips*k1/sum(k1), by = .(elas,surge)]
datvar[ , v1 := q1/k1, by = .(elas,surge)]
#Calculate new price the previous demand
datvar[ , price2 := fun.price(k1, pmin, pmax, a, b)]
#Calculate new density, flow, and number of trips from price
datvar[ , k2 := fun.elasticdemand(k1,price1,price2,elas), by = .(elas,surge)]
datvar[ , k2 := sum(k1)*k2/sum(k2), by = .(elas,surge)]
datvar[ , mu2 := trips*k2/sum(k2)/inc/nlanes, by = .(elas,surge)]
datvar[ , q2 := fun.flowdensity(k2), by = .(elas,surge)]
datvar[ , n2 := trips*k2/sum(k2), by = .(elas,surge)]
datvar[ , v2 := q2/k2, by = .(elas,surge)]

#Delay
datvar[ , d1 := n1*L*((1/v1) - (1/v_f)), by = .(elas,surge)]
datvar[ , d2 := n2*L*((1/v2) - (1/v_f)), by = .(elas,surge)]
datvar[ , delaydiff := (n2*v1*(v_f - v2))/(n1*v2*(v_f - v1)) - 1, by = .(elas,surge)]

#Revenue
datvar[ , rev1 := n1*price1]
datvar[ , rev2 := n2*price2]
#Cumulative sum
datvar[ , sumrev1 := cumsum(rev1)]
datvar[ , sumrev2 := cumsum(rev2)]

#Time
datvar[ , time := as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*tval)]

#Check sum, make sure the same number of total trips happen
datvar[ , lapply(.SD, sum),.SDcols = c("k1","k2"), by = .(elas,surge)][, all.equal(k1,k2)]
datvar[ , lapply(.SD, sum),.SDcols = c("mu1","mu2"), by = .(elas,surge)][, all.equal(mu1,mu2)]
datvar[ , lapply(.SD, sum),.SDcols = c("n1","n2"), by = .(elas,surge)][, all.equal(n1,n2)]
datvar[ , lapply(.SD, sum),.SDcols = c("q1","q2"), by = .(elas,surge)][, all.equal(q1,q2)]

#Export the percent difference between aggregate total revenue and travel time
varmat <- datvar[ , list("revdiff" = (sum(rev2) - sum(rev1))/sum(rev1),
                         "delaydiff" = (sum(d2*n2) - sum(d1*n1))/sum(d1*n1) ), by = .(elas,surge)]


#Minimum pmax for revenue neutrality
pmax_rev <- varmat[round(elas,4) == E & revdiff >= 0, ][which.min(revdiff), surge]

#Use this Pmax?
pmax = pmax_rev


#### Run for the constant parameters ####
dat = data.table(t=t, k1 = fun.demanddist(85,t,1.5), price1 = pfix)
dat[ , mu1 := trips*k1/sum(k1)/inc/nlanes ]
dat[ , q1 := fun.flowdensity(k1) ]
dat[ , n1 := trips*k1/sum(k1) ]
dat[ , v1 := q1/k1]
dat[ , price2 := fun.price(k1, pmin, pmax, a, b)]

dat[ , k2 := fun.elasticdemand(k1,price1,price2,E)]
dat[ , k2 := sum(k1)*k2/sum(k2)]
dat[ , mu2 := trips*k2/sum(k2)/inc/nlanes ]
dat[ , q2 := fun.flowdensity(k2) ]
dat[ , n2 := trips*k2/sum(k2) ]
dat[ , v2 := q2/k2]

#Delay
dat[ , d1 := n1*L*((1/v1) - (1/v_f))]
dat[ , d2 := n2*L*((1/v2) - (1/v_f))]
dat[ , delaydiff := (n2*v1*(v_f - v2))/(n1*v2*(v_f - v1)) - 1 ]

#Check to make sure they're the same
# all.equal(dat[ , (d2 - d1)/d1 ],
#           dat[ , (n2*v1*(v_f - v2))/(n1*v2*(v_f - v1)) - 1 ])


#Revenue
dat[ , rev1 := n1*price1]
dat[ , rev2 := n2*price2]
#Revenue per hour
dat[ , rev1hr := rev1/inc]
dat[ , rev2hr := rev2/inc]
#Cumulative sum
dat[ , sumrev1 := cumsum(n1*price1)]
dat[ , sumrev2 := cumsum(n2*price2)]

#Time
dat[ , time := as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*t)]

#Check sum, make sure the same number of total trips happen
dat[ , lapply(.SD, sum),.SDcols = c("k1","k2")]
dat[ , lapply(.SD, sum),.SDcols = c("n1","n2")]
dat[ , lapply(.SD, sum),.SDcols = c("mu1","mu2")]
dat[ , lapply(.SD, sum),.SDcols = c("q1","q2")]

#Percent change in delay
deltadelay <- dat[ , (sum(d2*n2) - sum(d1*n1))/sum(d1*n1)]
#Percent change in revenue
deltarevenue <- dat[ , (sum(rev2) - sum(rev1))/sum(rev1)]

#### Plotting

#### Function Plots ####

#### Kernal Density 
#rand = data.table(rand = c(rlnorm(50, sd=0.5), rnorm(50, mean=3, sd=0.5)))
#rand = data.table(rand = rlnorm(5))
rand = data.table(rand = c(-2.1,-1.3,-0.4,1.9,5.1,6.2))
dens = density(rand$rand, kernel = 'gaussian', bw=sqrt(2.25))

plot.kernel <- ggplot(rand, aes(rand)) +
  geom_rug() +
  geom_histogram(aes(y=4*..density..), binwidth=dens$bw, alpha=0.2) +
  geom_density(aes(y=..count..), bw=dens$bw) +
  scale_y_continuous("Density", limits = c(0, 1)) +
  scale_x_continuous("Random value", limits = c(-7.5,12)) +
  theme_classic() 
#theme(text=element_text(family="Times New Roman"))
for(x in rand$rand) plot.kernel <- plot.kernel + stat_function(fun = dnorm, args = list(mean = x, sd = dens$bw), linetype=2)
# plot.kernel


#### Elasticity Plot
elabs <- c(bquote(epsilon ==~.(E/2)), bquote(epsilon ==~.(E)), bquote(epsilon ==~.(2*E)))

plot.elasticity <- ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = function(x) exp(-0.5*E*x)-1, aes(linetype = "E/2", color = "E/2"), alpha = 0.5) +
  stat_function(fun = function(x) exp(-E*x)-1, aes(linetype = "E", color = "E")) +
  stat_function(fun = function(x) exp(-2.0*E*x)-1, aes(linetype = "2E", color = "2E"), alpha = 0.5) +
  geom_vline(xintercept = 0, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  #annotate("text", x=-0.4, y=-.25, label = paste("Constant elasticity =",E)) + #, family = "Times New Roman") +
  scale_x_continuous(expression(Delta~"Price"), labels = scales::percent, limits = c(-1,1)) +
  scale_y_continuous(expression(Delta~"Demand"), labels = scales::percent, limits = c(-0.5,0.5)) +
  scale_color_brewer("Elasticity", palette = "Set1", limits = c("E/2","E","2E"), labels = elabs) +
  scale_linetype_manual("Elasticity", values = c(2,1,5), limits = c("E/2","E","2E"), labels = elabs) +
  theme_classic()
# theme(text=element_text(family="Times New Roman"))
# plot.elasticity

#### Pricing function
plabs <- c(bquote(P[max] ==~"$"*.(pmax/2)), bquote(P[max] ==~"$"*.(pmax)), bquote(P[max] ==~"$"*.(2*pmax)))

plot.price <- ggplot(data.frame(x = c(0, 40)), aes(x)) + 
  stat_function(fun = fun.price, args = list(pmin, 0.5*pmax, a, b), aes(linetype = "pmax/2", color = "pmax/2"), alpha = 0.5) +
  stat_function(fun = fun.price, args = list(pmin, pmax, a, b), aes(linetype = "pmax", color = "pmax")) +
  stat_function(fun = fun.price, args = list(pmin, 2*pmax, a, b), aes(linetype = "2pmax", color = "2pmax"), alpha = 0.5) +
  scale_y_continuous("Price ($/veh)", labels = scales::dollar, limits = c(0,2*pmax)) +
  scale_x_continuous("Traffic density (veh/km/lane)") +
  scale_color_brewer(expression("Upper price limit,"~P[max]), palette = "Set1", limits = c("pmax/2","pmax","2pmax"), labels = plabs) +
  scale_linetype_manual(expression("Upper price limit,"~P[max]), values = c(2,1,5), limits = c("pmax/2","pmax","2pmax"), labels = plabs) +
  theme_classic()
# theme(text=element_text(family="Times New Roman"))
# plot.price


#### Flow-density function
flabs = c("Bi-parabolic","Bi-linear (Daganzo)","Parabolic (Greenshields)")

plot.flowdensity <- ggplot(data.frame(k = c(0, 40)), aes(k)) + 
  stat_function(fun = fun.flowdensity, aes(linetype = "para", color = "para")) +
  stat_function(fun = fun.flowdensity_dag, aes(linetype = "dag", color = "dag"), alpha = 0.5) +
  stat_function(fun = fun.flowdensity_green, aes(linetype = "green", color = "green"), alpha = 0.5) +
  scale_y_continuous("Traffic flow (veh/hr/lane)", labels = scales::comma, breaks = seq(0, 1500, by = 250), expand = c(0,0)) +
  scale_x_continuous("Traffic density (veh/km/lane)", limits = c(0, 150), breaks = seq(0, 150, by = 25), expand = c(0,0)) +
  scale_color_brewer("Traffic Flow Model", palette = "Set1", limits = c("para","dag","green"), labels = flabs) +
  scale_linetype_manual("Traffic Flow Model", values = c(1,2,5), limits = c("para","dag","green"), labels = flabs) +
  coord_cartesian(xlim = c(0,155), ylim = c(0,1550)) +
  theme_classic() +
  theme(legend.position = c(0.85,0.5), 
        legend.background = element_blank())
  #text=element_text(family="Times New Roman"))
# plot.flowdensity


#### Speed-density function
plot.speeddensity <- ggplot(data.frame(k = c(0, 40)), aes(k)) + 
  stat_function(fun = function(k) fun.flowdensity(k)/k, aes(linetype = "para", color = "para")) +
  stat_function(fun = function(k) fun.flowdensity_dag(k)/k, aes(linetype = "dag", color = "dag"), alpha = 0.5) +
  stat_function(fun = function(k) fun.flowdensity_green(k)/k, aes(linetype = "green", color = "green"), alpha = 0.5) +
  scale_y_continuous("Traffic speed (km/hr)", labels = scales::comma, breaks = seq(0, 100, by = 25), expand = c(0,0)) +
  scale_x_continuous("Traffic density (veh/km/lane)", limits = c(0, 150), breaks = seq(0, 150, by = 25), expand = c(0,0)) +
  scale_color_brewer("Traffic Flow Model", palette = "Set1", limits = c("para","dag","green"), labels = flabs) +
  scale_linetype_manual("Traffic Flow Model", values = c(1,2,5), limits = c("para","dag","green"), labels = flabs) +
  coord_cartesian(xlim = c(0,155), ylim = c(0,120)) +
  theme_classic() +
  theme(legend.position = c(0.85,0.5), 
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot.flowdensity


#### Discretized time Density Distributions
captime = data.table(t = seq(0, 24, by = 0.5),
                     time = as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*seq(0, 24, by = 0.5)),
                     k = fun.demanddist(55, seq(0, 24, by = 0.5), 1),
                     cap = 30)
captime[ , diff := cap - k ]
#captime[diff < 0, diff := abs(diff) + cap]
captime[diff < 0, diff := 0]
captime <- melt(captime, id.vars = c("t","time","cap"))
captime[ , variable := factor(variable, levels = c("diff","k"))]

plot.captime <- ggplot(captime) + 
  #geom_col(aes(x=time,y=k), color = alpha("white",0)) +
  #geom_col(aes(x=time,y=cap, color = "Density of pre-purchased time slots"), width = 0, fill = alpha("white",0)) +
  geom_col(aes(x=time, y = value, fill = variable), color = "gray90", width = 1800, position = "stack") +
  scale_fill_manual(NULL, values = c(alpha("white",0),"gray30"), labels = c("Remaining capacity","Pre-pay demand density")) +
  geom_hline(yintercept = captime$cap[1], linetype = "dashed") +
  annotate("text", x = as.POSIXct('2000-01-01 06:00:00 EST', tz='EST'), y = 30, label = "Target Capacity", vjust = -0.5) +
  scale_y_continuous("Trip density", expand = c(0,0), limits = c(0,40)) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour", expand = c(0,0),
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot.captime


#### Demand Distribution
plot.demanddist <- ggplot(dat) + 
  geom_area(aes(x=time,y=n1/inc), fill = "gray80", color = "black") + 
  scale_y_continuous("Travel Demand (trips/hr)", labels = scales::comma) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7), 
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot.demanddist



#### Heatmap matrix comparisons ####

#### Total revenue by Elasticity vs Pmax
revcuts = seq(-125, 125, by = 25)
revlabs <- paste0(paste(round(revcuts)[-length(revcuts)],
                        round(revcuts[-1]), sep = " to "),"%")

plot.revmat <- ggplot(varmat, aes(x = elas, y = surge)) +
  geom_raster(aes(fill = cut(100*revdiff, revcuts))) +
  geom_contour(breaks = revcuts, aes(z = 100*revdiff), color = "black") +
  scale_x_continuous(expression("Price Elasticity of Demand, "~epsilon), expand = c(0,0)) +
  scale_y_continuous(expression("Upper price limit,"~P[max]), expand = c(0,0),
                     labels = scales::percent_format()) +
  scale_fill_brewer(expression("Percent change in revenue\n(Dynamic vs Fixed)"),
                    palette = "RdBu", label = revlabs) +
  coord_cartesian(xlim = c(0, max(varmat$elas)), ylim = c(0,max(varmat$surge))) +
  theme_bw()
# plot.revmat


#### Travel time by Elasticity vs Pmax
delaycuts = seq(-50, 50, by = 10)
delaylabs = paste0(paste(round(delaycuts)[-length(delaycuts)], 
                        round(delaycuts[-1]), sep = " to "),"%")
# delaylabs <- gsub("-Inf to ", "\u2264", delaylabs)
# delaylabs[grepl(" to Inf", delaylabs)] <- paste0(">", gsub(" to Inf", "", delaylabs[grepl(" to Inf", delaylabs)]))

ggplot(varmat, aes(x = elas, y = surge)) +
  geom_contour_filled(aes(z = 100*delaydiff))

plot.delaymat <- ggplot(varmat, aes(x = elas, y = surge)) +
  geom_raster(aes(fill = cut(100*delaydiff, delaycuts))) +
  geom_contour(breaks = delaycuts, aes(z = 100*delaydiff), color = "black") +
  scale_x_continuous(expression("Price Elasticity of Demand, "~epsilon), expand = c(0,0)) +
  scale_y_continuous(expression("Upper price limit,"~P[max]), expand = c(0,0),
                     labels = scales::percent_format()) +
  scale_fill_brewer(expression("Percent change\nin total delay"),
                    palette = "RdBu", label = delaylabs, direction = -1) +
  coord_cartesian(xlim = c(0, max(varmat$elas)), ylim = c(0,max(varmat$surge))) +
  theme_bw()
# plot.delaymat


#### Time series distribution results plots ####

#### Demand Density Distributions (Same as flow, but with density)
plot.demanddensity <- ggplot(dat) + 
  geom_line(aes(x=time,y=k1, linetype="Fixed")) + 
  geom_line(aes(x=time,y=k2, linetype="Dynamic")) + 
  scale_y_continuous("Traffic density (veh/km/ln)") +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7), 
        legend.background = element_blank())
        #text=element_text(family="Times New Roman"))
# plot.demanddensity

#### Demand flow distributions (Same as density, but with flow)
plot.demandflow <- ggplot(dat) + 
  geom_line(aes(x=time,y=q1, linetype="Fixed")) + 
  geom_line(aes(x=time,y=q2, linetype="Dynamic")) +
  # geom_line(aes(x=time,y=mu1, linetype="Fixed", color = "Fixed")) + 
  # geom_line(aes(x=time,y=mu2, linetype="Dynamic", color = "Dynamic")) + 
  scale_y_continuous("Traffic flow (veh/hr/lane)") +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.15,0.6),
        legend.background = element_blank())
        #text=element_text(family="Times New Roman"))
# plot.demandflow

#### Demand speed distributions
plot.demandspeed <- ggplot(dat) + 
  geom_line(aes(x=time,y=v1, linetype="Fixed")) + 
  geom_line(aes(x=time,y=v2, linetype="Dynamic")) + 
  scale_y_continuous("Speed (km/hr)", limits = c(0,100)) +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.15,0.6))
#text=element_text(family="Times New Roman"))
# plot.demandspeed

#### Demand delay
plot.demanddelay <- ggplot(dat) + 
  geom_line(aes(x=time,y=n1*d1, linetype="Fixed")) + 
  geom_line(aes(x=time,y=n2*d2, linetype="Dynamic")) +
  scale_y_continuous("Total delay time (hr)") +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.15,0.6))
#text=element_text(family="Times New Roman"))
# plot.demanddelay


#### Demand delay difference
plot.demanddelaydiff <- ggplot(dat) + 
  geom_line(aes(x=time,y=delaydiff)) +
  scale_y_continuous("Travel time difference", labels = scales::percent_format()) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.15,0.6))
#text=element_text(family="Times New Roman"))
# plot.demanddelaydiff

#### Toll price distribution
plot.toll <- ggplot(dat) +  
  geom_line(aes(x=time,y=price1, linetype="Fixed")) + 
  geom_line(aes(x=time,y=price2, linetype="Dynamic")) + 
  scale_y_continuous("Price ($/veh)", labels = scales::dollar, limits = c(0,pmax)) +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank())
        #text=element_text(family="Times New Roman"))
# plot.toll

#### Revenue distribution
plot.revenue <- ggplot(dat) + 
  geom_line(aes(x=time,y=rev1, linetype=paste0("Fixed toll,\nTotal revenue: ", scales::dollar(sum(dat$rev1))))) +
  geom_line(aes(x=time,y=rev2, linetype=paste0("Dynamic toll,\nTotal revenue: ", scales::dollar(sum(dat$rev2))))) +
  scale_y_continuous(expression("Revenue per hour ($/hr)"), labels = scales::dollar) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),
                              as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  scale_linetype(NULL) +
  # annotate("text", x=as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'), y=0.5*max(dat$rev2),
  #          label = paste0(paste0("Fixed toll: ", scales::dollar(sum(dat$rev1))),"\n",
  #                         paste0("Dynamic toll: ", scales::dollar(sum(dat$rev2)))), hjust = 0, size = 4) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.background = element_blank())
        #text=element_text(family="Times New Roman"))
# plot.revenue


#### Cumulative revenue
plot.revsum <- ggplot(dat) + 
  geom_line(aes(x=time,y=sumrev1, linetype="Fixed")) +
  geom_line(aes(x=time,y=sumrev2, linetype="Dynamic")) +
  scale_y_continuous("Total revenue ($)", labels = scales::dollar) +
  scale_linetype(NULL) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot.revsum



#### Saving ####
# ggsave("../figures/toyprice.pdf", plot.price, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyelasticity.pdf", plot.elasticity, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toydemand.pdf", plot.demanddensity, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toytoll.pdf", plot.toll, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyrevenue.pdf", plot.revenue, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/KDE.pdf", plot.kernel, device = cairo_pdf, width=4.25, height=2, units = "in")








