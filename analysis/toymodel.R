
library(data.table)
library(ggplot2)
library(scales)
library(extrafont)

#Functions 
fun.price <- function(k, pmin, pmax, a, b)  pmin + (pmax-pmin)/(1 + exp(a-b*k))
fun.demanddist <- function(scale, t,sd) scale*(dnorm(t,mean=9,sd) + dnorm(t,mean=17,sd) + dnorm(t,mean=12,sd=4) + dnorm(t,mean=14,sd=4))
fun.elasticdemand <- function(k,p1,p2,E) k+k*(exp(-E*(p2-p1)/(p2+p1))-1)

E = 0.4
trips = 100000
inc = 5/60
t = seq(0,24,by=inc)
a = 7
b = 0.3
pmin = 3
pmax = 12

#Paramters
# dat = data.table(t=t, k1 = fun.demanddist(t,1.25), price1 = 7.75)
# 
# dat[ , k2 := fun.demanddist(t,2)]
# dat[ , c("q1","q2") := lapply(.SD, function(x) trips*x/sum(x)/inc/6), .SDcols = c("k1","k2")]
# dat[ , c("n1","n2") := lapply(.SD, function(x) trips*x/sum(x)), .SDcols = c("k1","k2")]
# dat[ , c("k1","k2") := lapply(.SD, function(x) 75*x), .SDcols = c("k1","k2")]
# dat[ , price2 := fun.price(k1, pmin, pmax, a, b)]
# 

dat = data.table(t=t, k1 = fun.demanddist(85,t,1.5), price1 = 7.75)
#dat[ , k2 := fun.demanddist(t,2)]
dat[ , "q1" := trips*k1/sum(k1)/inc/6 ]
dat[ , "n1" := trips*k1/sum(k1) ]
dat[ , price2 := fun.price(k1, pmin, pmax, a, b)]

dat[ , k2 := fun.elasticdemand(k1,price1,price2,E)]
dat[ , k2 := sum(k1)*k2/sum(k2)]
dat[ , "q2" := trips*k2/sum(k2)/inc/6 ]
dat[ , "n2" := trips*k2/sum(k2) ]


#Revenue
dat[ , rev1 := cumsum(n1*price1)]
dat[ , rev2 := cumsum(n2*price2)]

#Time
dat[ , time := as.POSIXct('2000-01-01 00:00:00 EST', tz='EST') + (3600*t)]

#Check sum
dat[ , lapply(.SD, sum),.SDcols = c("k1","k2")]
dat[ , lapply(.SD, sum),.SDcols = c("q1","q2")]
dat[ , lapply(.SD, sum),.SDcols = c("n1","n2")]


#Ploting
plot.price <- ggplot(data.frame(x = c(0, 40)), aes(x)) + 
  stat_function(fun = fun.price, args = list(pmin, pmax, a, b)) +
  scale_y_continuous("Price [$/veh]", labels = scales::dollar, breaks = seq(0,15,by=2), limits = c(0,pmax)) +
  scale_x_continuous("Traffic density [veh/km/lane]") +
  theme_classic() +
  theme(text=element_text(family="Times New Roman"))
plot.price

plot.elasticity <- ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = function(x) exp(-E*x)-1) +
  geom_vline(xintercept = 0, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  annotate("text", x=-0.4, y=-.25, label = paste("Constant elasticity =",E), family = "Times New Roman") +
  scale_x_continuous(expression(Delta~"Price"), labels = scales::percent, limits = c(-1,1)) +
  scale_y_continuous(expression(Delta~"Demand"), labels = scales::percent, limits = c(-0.5,0.5)) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman"))
plot.elasticity


plot.demanddensity <- ggplot(dat) + 
  geom_line(aes(x=time,y=k1, linetype="Flat")) + 
  geom_line(aes(x=time,y=k2, linetype="Dynamic")) + 
  scale_y_continuous("Traffic density [veh/km/ln]") +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7), 
        legend.background = element_blank(),
        text=element_text(family="Times New Roman"))
plot.demanddensity


plot.demandflow <- ggplot(dat) + 
  geom_line(aes(x=time,y=q1, linetype="Flat")) + 
  geom_line(aes(x=time,y=q2, linetype="Dynamic")) + 
  scale_y_continuous("Traffic flow [veh/hr/lane]") +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.15,0.6), 
        text=element_text(family="Times New Roman"))
plot.demandflow


plot.toll <- ggplot(dat) +  
  geom_line(aes(x=time,y=price1, linetype="Flat")) + 
  geom_line(aes(x=time,y=price2, linetype="Dynamic")) + 
  scale_y_continuous("Price [$/veh]", labels = scales::dollar, breaks = seq(0,15,by=2), limits = c(0,pmax)) +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank(),
        text=element_text(family="Times New Roman"))
plot.toll


plot.revenue <- ggplot(dat) + 
  geom_line(aes(x=time,y=rev1, linetype="Flat")) +
  geom_line(aes(x=time,y=rev2, linetype="Dynamic")) +
  scale_y_continuous("Total revenue [$]", labels = scales::dollar) +
  scale_linetype("Tolling scheme") +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:00:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank(),
        text=element_text(family="Times New Roman"))
plot.revenue


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
  theme_classic() + 
  theme(text=element_text(family="Times New Roman"))
for(x in rand$rand) plot.kernel <- plot.kernel + stat_function(fun = dnorm, args = list(mean = x, sd = d$bw), linetype=2)
plot.kernel

# ggsave("../figures/toyprice.pdf", plot.price, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyelasticity.pdf", plot.elasticity, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toydemand.pdf", plot.demanddensity, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toytoll.pdf", plot.toll, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyrevenue.pdf", plot.revenue, device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/KDE.pdf", plot.kernel, device = cairo_pdf, width=4.25, height=2, units = "in")








