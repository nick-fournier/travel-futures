
library(data.table)
library(ggplot2)
library(scales)
# library(extrafont)

#### Functions ####

#Pricing function
fun.price <- function(k, pmin, pmax, a, b)  pmin + (pmax-pmin)/(1 + exp(a-b*k))

#Elasticity function
fun.elasticdemand <- function(k,p1,p2,E) k+k*(exp(-E*(p2-p1)/(p2+p1))-1)

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
pmax = pfix*2

#### Run for the constant parameters ####
dat = data.table(t=t, k1 = fun.demanddist(85,t,1.5), price1 = pfix)
#dat[ , k2 := fun.demanddist(t,2)]
dat[ , "q1" := trips*k1/sum(k1)/inc/6 ]
dat[ , "n1" := trips*k1/sum(k1) ]
dat[ , price2 := fun.price(k1, pmin, pmax, a, b)]

dat[ , k2 := fun.elasticdemand(k1,price1,price2,E)]
dat[ , k2 := sum(k1)*k2/sum(k2)]
dat[ , "q2" := trips*k2/sum(k2)/inc/6 ]
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

#Check sum
dat[ , lapply(.SD, sum),.SDcols = c("k1","k2")]
dat[ , lapply(.SD, sum),.SDcols = c("q1","q2")]
dat[ , lapply(.SD, sum),.SDcols = c("n1","n2")]


#### Run with varying pmax and elasticity ####
#Set up combination matrix
datvar = as.data.table(expand.grid(tval = t, E = seq(0,1, by=0.05), surge = seq(1, 5, by = 0.1)))
#Assign fixed price & scaled price
datvar[ , price1 := pfix]
datvar[ , pmax := pfix*surge]
#Calculate demand dist for each combo
datvar[ , k1 := fun.demanddist(85, tval, 1.5)]
#Calculate flow and number of trips for fixed price
datvar[ , "q1" := trips*k1/sum(k1)/inc/6 ]
datvar[ , "n1" := trips*k1/sum(k1) ]
#Calculate new price the previous demand
datvar[ , price2 := fun.price(k1, pmin, pmax, a, b)]
#Calculate new density, flow, and number of trips from price
datvar[ , k2 := fun.elasticdemand(k1,price1,price2,E)]
datvar[ , k2 := sum(k1)*k2/sum(k2)]
datvar[ , "q2" := trips*k2/sum(k2)/inc/6 ]
datvar[ , "n2" := trips*k2/sum(k2) ]
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

#Check sum
datvar[ , lapply(.SD, sum),.SDcols = c("k1","k2")]
datvar[ , lapply(.SD, sum),.SDcols = c("q1","q2")]
datvar[ , lapply(.SD, sum),.SDcols = c("n1","n2")]

#Aggregate total revenue
revmat <- datvar[ , list("revratio" = sum(rev2)/sum(rev1)), by = .(E,surge)]



#### Ploting ####

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

plot.price <- ggplot(data.frame(x = c(0, 40)), aes(x)) + 
  stat_function(fun = fun.price, args = list(pmin, pmax, a, b)) +
  scale_y_continuous("Price [$/veh]", labels = scales::dollar, breaks = seq(0,15,by=2), limits = c(0,pmax)) +
  scale_x_continuous("Traffic density [veh/km/lane]") +
  theme_classic()
# theme(text=element_text(family="Times New Roman"))
# plot.price

plot.elasticity <- ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = function(x) exp(-E*x)-1) +
  geom_vline(xintercept = 0, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  annotate("text", x=-0.4, y=-.25, label = paste("Constant elasticity =",E)) + #, family = "Times New Roman") +
  scale_x_continuous(expression(Delta~"Price"), labels = scales::percent, limits = c(-1,1)) +
  scale_y_continuous(expression(Delta~"Demand"), labels = scales::percent, limits = c(-0.5,0.5)) +
  theme_classic()
# theme(text=element_text(family="Times New Roman"))
# plot.elasticity


plot.demanddensity <- ggplot(dat) + 
  geom_line(aes(x=time,y=k1, linetype="Flat")) + 
  geom_line(aes(x=time,y=k2, linetype="Dynamic")) + 
  scale_y_continuous("Traffic density [veh/km/ln]") +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7), 
        legend.background = element_blank())
        #text=element_text(family="Times New Roman"))
# plot.demanddensity


plot.demandflow <- ggplot(dat) + 
  geom_line(aes(x=time,y=q1, linetype="Flat")) + 
  geom_line(aes(x=time,y=q2, linetype="Dynamic")) + 
  scale_y_continuous("Traffic flow [veh/hr/lane]") +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.15,0.6))
        #text=element_text(family="Times New Roman"))
# plot.demandflow


plot.toll <- ggplot(dat) +  
  geom_line(aes(x=time,y=price1, linetype="Flat")) + 
  geom_line(aes(x=time,y=price2, linetype="Dynamic")) + 
  scale_y_continuous("Price [$/veh]", labels = scales::dollar, breaks = seq(0,pmax,by=2), limits = c(0,pmax)) +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank())
        #text=element_text(family="Times New Roman"))
# plot.toll


plot.revenue <- ggplot(dat) + 
  geom_line(aes(x=time,y=rev1, linetype=paste0("Fixed toll, total revenue: ", scales::dollar(sum(dat$rev1))))) +
  geom_line(aes(x=time,y=rev2, linetype=paste0("Dynamic toll, total revenue: ", scales::dollar(sum(dat$rev2))))) +
  scale_y_continuous(expression("Revenue per hour, $/hr"), labels = scales::dollar) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),
                              as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'))) +
  scale_linetype("Tolling scheme") +
  # annotate("text", x=as.POSIXct('2000-01-01 24:00:00 EST', tz='EST'), y=0.5*max(dat$rev2),
  #          label = paste0(paste0("Fixed toll: ", scales::dollar(sum(dat$rev1))),"\n",
  #                         paste0("Dynamic toll: ", scales::dollar(sum(dat$rev2)))), hjust = 0, size = 4) +
  theme_classic() +
  theme(legend.position = "right",
        legend.background = element_blank())
        #text=element_text(family="Times New Roman"))
# plot.revenue



plot.revsum <- ggplot(dat) + 
  geom_line(aes(x=time,y=sumrev1, linetype="Flat")) +
  geom_line(aes(x=time,y=sumrev2, linetype="Dynamic")) +
  scale_y_continuous("Total revenue [$]", labels = scales::dollar) +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot.revsum


cuts = c(with(revmat, seq(100*min(revratio), 100, length.out = 6))[-6], 
  with(revmat, seq(100, 100*max(revratio), length.out = 6)))

plot.revmat <- ggplot(revmat, aes(x = E, y = surge, z = 100*revratio)) +
  geom_contour_filled(breaks = cuts) +
  scale_x_continuous(expression("Price Elasticity of Demand,"~E), limits = c(0,1), expand = c(0,0)) +
  scale_y_continuous(expression("Scale of fixed price,"~P[max]), limits = c(1,5),
                     labels = scales::percent_format(), expand = c(0,0)) +
  scale_fill_brewer(expression("Revenue Ratio\n(Dynamic vs Fixed)"),
                    palette = "RdBu", label = paste0(paste(round(cuts), round(cuts[-1]), sep = "-"),"%")) +
  theme_classic()
# plot.revenue






# ggsave("../figures/toyprice.pdf", plot.price, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyelasticity.pdf", plot.elasticity, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toydemand.pdf", plot.demanddensity, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toytoll.pdf", plot.toll, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyrevenue.pdf", plot.revenue, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/KDE.pdf", plot.kernel, device = cairo_pdf, width=4.25, height=2, units = "in")








