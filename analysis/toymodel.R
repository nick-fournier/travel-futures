
library(data.table)
library(ggplot2)
library(scales)
# library(extrafont)

#### Functions ####

#Pricing function
fun.price <- function(k, pmin, pmax, a, b)  pmin + (pmax-pmin)/(1 + exp(a-b*k))

#Elasticity function
fun.elasticdemand <- function(k,p1,p2,E) k+k*(exp(-E*(p2-p1)/(p2+p1))-1)
#fun.elasticdemand <- function(k,p1,p2,E) k*((p2-p1)/p1)^-E

#Demand distribution (Manually constructed for this example)
fun.demanddist <- function(scale, t,sd) scale*(dnorm(t,mean=9,sd) + dnorm(t,mean=17,sd) + dnorm(t,mean=12,sd=4) + dnorm(t,mean=14,sd=4))


#### Parameters ####
E = 0.3
trips = 100000
inc = 5/60
t = seq(0,24,by=inc)
a = 7
b = 0.3
pfix = 8
pmin = 0*pfix/2
pmax = pfix*3
nlanes = 6 #lanes

#### Run for the constant parameters ####
dat = data.table(t=t, k1 = fun.demanddist(85,t,1.5), price1 = pfix)
#dat[ , k2 := fun.demanddist(t,2)]
dat[ , "q1" := trips*k1/sum(k1)/inc/nlanes ]
dat[ , "n1" := trips*k1/sum(k1) ]
dat[ , price2 := fun.price(k1, pmin, pmax, a, b)]

dat[ , k2 := fun.elasticdemand(k1,price1,price2,E)]
dat[ , k2 := sum(k1)*k2/sum(k2)]
dat[ , "q2" := trips*k2/sum(k2)/inc/nlanes ]
dat[ , "n2" := trips*k2/sum(k2) ]

#Revenue
dat[ , rev1 := n1*price1]
dat[ , rev2 := n2*price2]
#Revenue per house
dat[ , rev1hr := rev1/inc]
dat[ , rev2hr := rev2/inc]
#Cumulative sum
dat[ , sumrev1 := cumsum(n1*price1)]
dat[ , sumrev2 := cumsum(n2*price2)]

#Time
dat[ , time := as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*t)]

#Check sum, make sure the same number of total trips happen
dat[ , lapply(.SD, sum),.SDcols = c("k1","k2")]
dat[ , lapply(.SD, sum),.SDcols = c("q1","q2")]
dat[ , lapply(.SD, sum),.SDcols = c("n1","n2")]


#### Run with varying pmax and elasticity ####
#Set up combination matrix
datvar = as.data.table(expand.grid(tval = t,
                                   E = seq(0,2, by=0.025),
                                   surge = seq(0, 4, by = 0.05)))
#Assign fixed price & scaled price
datvar[ , price1 := pfix]
datvar[ , pmax := pfix*surge]
#Calculate demand dist for each combo
datvar[ , k1 := fun.demanddist(85, tval, 1.5), by = .(E,surge)]
#Calculate flow and number of trips for fixed price
datvar[ , "q1" := trips*k1/sum(k1)/inc/nlanes, by = .(E,surge)]
datvar[ , "n1" := trips*k1/sum(k1), by = .(E,surge)]
#Calculate new price the previous demand
datvar[ , price2 := fun.price(k1, pmin, pmax, a, b)]
#Calculate new density, flow, and number of trips from price
datvar[ , k2 := fun.elasticdemand(k1,price1,price2,E), by = .(E,surge)]
datvar[ , k2 := sum(k1)*k2/sum(k2), by = .(E,surge)]
datvar[ , "q2" := trips*k2/sum(k2)/inc/nlanes, by = .(E,surge)]
datvar[ , "n2" := trips*k2/sum(k2), by = .(E,surge)]
#Revenue
datvar[ , rev1 := cumsum(n1*price1)]
datvar[ , rev2 := cumsum(n2*price2)]

#Revenue
datvar[ , rev1 := n1*price1]
datvar[ , rev2 := n2*price2]
#Cumulative sum
datvar[ , sumrev1 := cumsum(rev1)]
datvar[ , sumrev2 := cumsum(rev2)]

#Time
datvar[ , time := as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*tval)]

#Check sum, make sure the same number of total trips happen
datvar[ , lapply(.SD, sum),.SDcols = c("k1","k2"), by = .(E,surge)][, all.equal(k1,k2)]
datvar[ , lapply(.SD, sum),.SDcols = c("q1","q2"), by = .(E,surge)][, all.equal(q1,q2)]
datvar[ , lapply(.SD, sum),.SDcols = c("n1","n2"), by = .(E,surge)][, all.equal(n1,n2)]

#Percent difference between aggregate total revenue
revmat <- datvar[ , list("revratio" = (sum(rev2)-sum(rev1))/sum(rev1)), by = .(E,surge)]



#### Ploting ####


#### Kernal Density 
#rand = data.table(rand = c(rlnorm(50, sd=0.5), rnorm(50, mean=3, sd=0.5)))
#rand = data.table(rand = rlnorm(5))
rand = data.table(rand = c(-2.1,-1.3,-0.4,1.9,5.1,6.2))
d = density(rand$rand, kernel = 'gaussian', bw=sqrt(2.25))

plot.kernel <- ggplot(rand, aes(rand)) +
  geom_rug() +
  geom_histogram(aes(y=4*..density..), binwidth=d$bw, alpha=0.2) +
  geom_density(aes(y=..count..), bw=d$bw) +
  scale_y_continuous("Density", limits = c(0, 1)) +
  scale_x_continuous("Random value", limits = c(-7.5,12)) +
  theme_classic() 
#theme(text=element_text(family="Times New Roman"))
for(x in rand$rand) plot.kernel <- plot.kernel + stat_function(fun = dnorm, args = list(mean = x, sd = d$bw), linetype=2)
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


#### Discretized time Density Distributions
captime = data.table(t = seq(0, 24, by = 0.5),
                     time = as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*seq(0, 24, by = 0.5)),
                     k = fun.demanddist(55, seq(0, 24, by = 0.5), 1),
                     cap = 30)
captime[ , diff := cap - k ]
captime[diff < 0, diff := abs(diff) + cap]
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
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
plot.captime


#### Demand Distributions
plot.demanddist <- ggplot(dat) + 
  geom_area(aes(x=time,y=n1/inc), fill = "gray80", color = "black") + 
  scale_y_continuous("Travel Demand (trips/hr)", labels = scales::comma) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7), 
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot.demanddist



#### Demand Density Distributions (Same as flow, but with density)
plot.demanddensity <- ggplot(dat) + 
  geom_line(aes(x=time,y=k1, linetype="Fixed")) + 
  geom_line(aes(x=time,y=k2, linetype="Dynamic")) + 
  scale_y_continuous("Traffic density (veh/km/ln)") +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7), 
        legend.background = element_blank())
        #text=element_text(family="Times New Roman"))
# plot.demanddensity

#### Demand flow distributions (Same as density, but with flow)
plot.demandflow <- ggplot(dat) + 
  geom_line(aes(x=time,y=q1, linetype="Fixed")) + 
  geom_line(aes(x=time,y=q2, linetype="Dynamic")) + 
  scale_y_continuous("Traffic flow (veh/hr/lane)") +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.15,0.6))
        #text=element_text(family="Times New Roman"))
# plot.demandflow


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
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot.revsum


#### Total revenue by Elasticity vs Pmax
# cuts = with(revmat, 
#             c(seq(10*floor(10*min(revratio)), 100, length.out = 6)[-6],
#               seq(100, 10*ceiling(10*max(revratio)), length.out = 6)))

cuts = seq(-150, 150, by = 25)
cutlabs = c(paste0(paste(round(cuts)[-length(cuts)], round(cuts[-1]), sep = " to "),"%"),paste0(">",round(cuts)[length(cuts)],"%"))

plot.revmat <- ggplot(revmat, aes(x = E, y = surge)) +
  #geom_contour_filled(breaks = cuts, aes(z = 100*revratio)) +
  geom_raster(aes(fill = cut(100*revratio, c(cuts,Inf), include.lowest = T))) +
  geom_contour(breaks = cuts, aes(z = 100*revratio), color = "black") +
  scale_x_continuous(expression("Price Elasticity of Demand, "~epsilon), expand = c(0,0)) +
  scale_y_continuous(expression("Maximum surge pricing,"~P[max]), expand = c(0,0),
                     labels = scales::percent_format()) +
  scale_fill_brewer(expression("Revenue change\n(Dynamic vs Fixed)"),
                    palette = "RdBu", label = cutlabs) +
  coord_cartesian(xlim = c(0, max(revmat$E)), ylim = c(0,max(revmat$surge))) +
  theme_bw()
# plot.revmat





# ggsave("../figures/toyprice.pdf", plot.price, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyelasticity.pdf", plot.elasticity, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toydemand.pdf", plot.demanddensity, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toytoll.pdf", plot.toll, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyrevenue.pdf", plot.revenue, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/KDE.pdf", plot.kernel, device = cairo_pdf, width=4.25, height=2, units = "in")








