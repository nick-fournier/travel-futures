# This script Contains all the plotting functions

#
library(data.table)
library(ggplot2)
library(scales)
# library(extrafont)

if(file.exists("../analysis/toydata.RData")) load("../analysis/toydata.RData") else load("toydata.RData")

#Plot list
plot <- list()

#### Plots: Function Plots ####
#### Kernal Density 
#rand = data.table(rand = c(rlnorm(50, sd=0.5), rnorm(50, mean=3, sd=0.5)))
#rand = data.table(rand = rlnorm(5))
rand = data.table(rand = c(-2.1,-1.3,-0.4,1.9,5.1,6.2))
dens = density(rand$rand, kernel = 'gaussian', bw=sqrt(2.25))

plot[['kernel']] <- ggplot(rand, aes(rand)) +
  geom_rug() +
  geom_histogram(aes(y=4*..density..), binwidth=dens$bw, alpha=0.2) +
  geom_density(aes(y=..count..), bw=dens$bw) +
  scale_y_continuous("Density", limits = c(0, 1)) +
  scale_x_continuous("Random value", limits = c(-7.5,12)) +
  theme_classic() 
#theme(text=element_text(family="Times New Roman"))
for(x in rand$rand) plot[['kernel']] <- plot[['kernel']] + stat_function(fun = dnorm, args = list(mean = x, sd = dens$bw), linetype=2)
# plot[['kernel']]


#### Elasticity Plot
elabs <- c(bquote(epsilon ==~.(E/2)), bquote(epsilon ==~.(E)), bquote(epsilon ==~.(2*E)))

plot[['elasticity']] <- ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = function(x) exp(-0.5*E*x)-1, aes(linetype = "E/2", color = "E/2"), alpha = 0.5) +
  stat_function(fun = function(x) exp(-E*x)-1, aes(linetype = "E", color = "E")) +
  stat_function(fun = function(x) exp(-2.0*E*x)-1, aes(linetype = "2E", color = "2E"), alpha = 0.5) +
  geom_vline(xintercept = 0, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_x_continuous(expression(Delta~"Price"), labels = scales::percent, limits = c(-1,1)) +
  scale_y_continuous(expression(Delta~"Demand"), labels = scales::percent, limits = c(-0.5,0.5)) +
  scale_color_brewer("Elasticity", palette = "Set1", limits = c("E/2","E","2E"), labels = elabs) +
  scale_linetype_manual("Elasticity", values = c(2,1,5), limits = c("E/2","E","2E"), labels = elabs) +
  theme_classic()
# plot[['elasticity']]

#### Pricing function
plabs <- c(bquote("\u00B1 75%"*P[fix]), bquote("\u00B1 50%"*P[fix]), bquote("\u00B1 25%"*P[fix]))

plot[['price']] <- ggplot(data.frame(x = c(0, 40)), aes(x)) + 
  stat_function(fun = fun.price, args = list(0.25*pfix, 1.75*pfix, a, b), aes(linetype = "75", color = "75"), alpha = 0.5) +
  stat_function(fun = fun.price, args = list(0.50*pfix, 1.50*pfix, a, b), aes(linetype = "50", color = "50")) +
  stat_function(fun = fun.price, args = list(0.75*pfix, 1.25*pfix, a, b), aes(linetype = "25", color = "25"), alpha = 0.5) +
  scale_y_continuous(expression("Price"), labels = scales::percent_format(), limits = c(0,surcharge)) +
  scale_x_continuous("Traffic density (veh/km/lane)") +
  scale_color_brewer(expression("Price limits,"~P[max]~"&"~P[min]), palette = "Set1", limits = c("75","50","25"), labels = plabs) +
  scale_linetype_manual(expression("Price limits,"~P[max]~"&"~P[min]), values = c(2,1,5), limits = c("75","50","25"), labels = plabs) +
  theme_classic()
# plot[['price']]


#### Flow-density function
flabs = c("Bi-parabolic","Bi-linear (Daganzo)","Parabolic (Greenshields)")

plot[['flowdensity']] <- ggplot(data.frame(k = c(0, 40)), aes(k)) + 
  stat_function(fun = fun.flowdensity, aes(linetype = "para", color = "para")) +
  stat_function(fun = fun.flowdensity_dag, aes(linetype = "dag", color = "dag"), alpha = 0.5) +
  stat_function(fun = fun.flowdensity_green, aes(linetype = "green", color = "green"), alpha = 0.5) +
  scale_y_continuous("Traffic flow (veh/hr/lane)", labels = scales::comma, breaks = seq(0, 1500, by = 250), expand = c(0,0)) +
  scale_x_continuous("Traffic density (veh/km/lane)", limits = c(0, 150), breaks = seq(0, 150, by = 25), expand = c(0,0)) +
  scale_color_brewer("Traffic Flow Model", palette = "Set1", limits = c("para","dag","green"), labels = flabs) +
  scale_linetype_manual("Traffic Flow Model", values = c(1,2,5), limits = c("para","dag","green"), labels = flabs) +
  coord_cartesian(xlim = c(0,155), ylim = c(0,1550)) +
  theme_classic() +
  theme(legend.position = "none", 
        legend.background = element_blank())
# plot[['flowdensity']]


#### Speed-density function
plot[['speeddensity']] <- ggplot(data.frame(k = c(0, 40)), aes(k)) + 
  stat_function(fun = function(k) fun.flowdensity(k)/k, aes(linetype = "para", color = "para")) +
  stat_function(fun = function(k) fun.flowdensity_dag(k)/k, aes(linetype = "dag", color = "dag"), alpha = 0.5) +
  stat_function(fun = function(k) fun.flowdensity_green(k)/k, aes(linetype = "green", color = "green"), alpha = 0.5) +
  scale_y_continuous("Traffic speed (km/hr)", labels = scales::comma, breaks = seq(0, 100, by = 25), expand = c(0,0)) +
  scale_x_continuous("Traffic density (veh/km/lane)", limits = c(0, 150), breaks = seq(0, 150, by = 25), expand = c(0,0)) +
  scale_color_brewer("Traffic Flow Model", palette = "Set1", limits = c("para","dag","green"), labels = flabs) +
  scale_linetype_manual("Traffic Flow Model", values = c(1,2,5), limits = c("para","dag","green"), labels = flabs) +
  coord_cartesian(xlim = c(0,155), ylim = c(0,120)) +
  theme_classic() +
  theme(legend.position = c(0.75,0.5), 
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot[['flowdensity']]


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

plot[['captime']] <- ggplot(captime) + 
  #geom_col(aes(x=time,y=k), color = alpha("white",0)) +
  #geom_col(aes(x=time,y=cap, color = "Density of pre-purchased time slots"), width = 0, fill = alpha("white",0)) +
  geom_col(aes(x=time, y = value, fill = variable), color = "gray90", width = 1800, position = "stack") +
  scale_fill_manual(NULL, values = c(alpha("white",0),"gray30"), labels = c("Remaining capacity","Pre-pay demand density")) +
  geom_hline(yintercept = captime$cap[1], linetype = "dashed") +
  annotate("text", x = as.POSIXct('2000-01-01 06:00:00 EST', tz='EST'), y = 30, label = "Target Capacity", vjust = -0.5) +
  scale_y_continuous("Trip density", expand = c(0,0), limits = c(0,40)) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour", expand = c(0,0),
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:30:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot[['captime']]


#### Demand Distribution
plot[['demanddist']] <- ggplot(dat.elas[elas == as.character(E), ]) + 
  geom_area(aes(x=time, y=n1/inc), fill = "gray80", color = "black") + 
  scale_y_continuous("Travel Demand (trips/hr)", labels = scales::comma) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:30:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7), 
        legend.background = element_blank())
# plot[['demanddist']]



#### Plots: Time series distribution results plots ####
#### Demand Density Distributions (Same as flow, but with density)
dlabs = c("Fixed toll", sapply(unique(dat.elas$elas), function(x) bquote("Dynamic toll,"~epsilon == .(x))))

plot[['density']] <- ggplot(dat.elas) + 
  geom_line(aes(x=time,y=k1, color = "Fixed toll")) + 
  geom_line(aes(x=time,y=k2, color = factor(elas)), linetype = "dashed", alpha = 0.5) + 
  scale_y_continuous("Traffic density (veh/km/ln)") +
  scale_color_brewer("Tolling scheme", palette = "Set1", labels = dlabs, limits = c("Fixed toll", unique(dat.elas$elas))) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:30:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7), 
        legend.background = element_blank())
# plot[['density']]

#### Demand flow distributions (Same as density, but with flow)
plot[['flow']] <- ggplot(dat.elas) + 
  geom_line(aes(x=time,y=q1, color="Fixed toll")) + 
  geom_line(aes(x=time,y=q2, color = factor(elas)), linetype = "dashed", alpha = 0.5) + 
  # geom_line(aes(x=time,y=mu1, linetype="Fixed", color = "Fixed")) + 
  # geom_line(aes(x=time,y=mu2, linetype="Dynamic", color = "Dynamic")) + 
  scale_y_continuous("Traffic flow (veh/hr/lane)") +
  scale_color_brewer("Tolling scheme", palette = "Set1", labels = dlabs, limits = c("Fixed toll", unique(dat.elas$elas))) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour",
                   limits = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:30:00 EST', tz='EST'))) +
  theme_classic() +
  theme(legend.position = c(0.15,0.6),
        legend.background = element_blank())
#text=element_text(family="Times New Roman"))
# plot[['flow']]

#### Demand speed distributions
plot[['speed']] <- ggplot(dat.elas) + 
  geom_line(aes(x=time,y=v1, color="Fixed toll")) + 
  geom_line(aes(x=time,y=v2, color = factor(elas)), linetype = "dashed", alpha = 0.8) + 
  scale_y_continuous("Speed (km/hr)") +
  scale_color_brewer("Tolling scheme", palette = "Set1", labels = dlabs, limits = c("Fixed toll", unique(dat.elas$elas))) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour") +
  coord_cartesian(xlim = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:30:00 EST', tz='EST')),
                  ylim = c(40, 100)) +
  theme_classic() +
  theme(legend.position = "right",#c(0.8,0.5),
        legend.direction = "vertical",
        legend.background = element_blank())
# plot[['speed']]

#### Demand delay
plot[['delay']] <- ggplot(dat.elas) + 
  geom_line(aes(x=time,y=d1, color="Fixed toll")) + 
  geom_line(aes(x=time,y=d2, color = factor(elas)), linetype = "dashed", alpha = 0.5) + 
  scale_y_continuous("Total delay time (hr)") +
  scale_color_brewer("Tolling scheme", palette = "Set1", labels = dlabs, limits = c("Fixed toll", unique(dat.elas$elas))) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour") +
  theme_classic() +
  coord_cartesian(xlim = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:30:00 EST', tz='EST'))) +
  theme(legend.position = c(0.15,0.6))
# plot[['delay']]

#### Toll price distribution
plot[['toll']] <- ggplot(dat.elas) +  
  geom_line(aes(x=time,y=price1, linetype="Fixed")) + 
  geom_line(aes(x=time,y=price2, linetype="Dynamic")) + 
  scale_linetype("Tolling scheme") +
  scale_y_continuous("Price ratio (Dynamic / Fixed)", labels = scales::percent_format(), breaks = seq(0,2,by = 0.25)) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour") +
  coord_cartesian(xlim = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:30:00 EST', tz='EST')),
                  ylim = c(discount, surcharge)) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank())
# plot[['toll']]

#### Revenue distribution
drlabs = c("Fixed toll, Revenue: 100%", apply(dat.elas[ , sum(rev2)/sum(rev1), by = elas], 1, function(x) {
  bquote("Dynamic toll,"~epsilon == .(x[1])*", Revenue: "~.(round(100*x[2]))*"%")
}))


plot[['revenue']] <- ggplot(dat.elas) + 
  geom_line(aes(x=time,y=rev1/rev1, color="Fixed toll")) + 
  geom_line(aes(x=time,y=rev2/rev1, color = factor(elas)), linetype = "dashed") + 
  scale_y_continuous(expression("Revenue"~frac(Dynamic, Fixed)), labels = scales::percent_format()) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour") +
  scale_color_brewer("Tolling scheme", palette = "Set1", labels = drlabs, limits = c("Fixed toll", unique(dat.elas$elas))) +
  coord_cartesian(xlim = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'),as.POSIXct('2000-01-01 23:30:00 EST', tz='EST')),
                  ylim = c(discount, surcharge)) +
  theme_classic() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank())
# plot[['revenue']]

#### Cumulative revenue
plot[['revsum']] <- ggplot(dat.elas) + 
  geom_line(aes(x=time,y=sumrev1/max(sumrev1), color="Fixed toll")) + 
  geom_line(aes(x=time,y=sumrev2/max(sumrev1), color = factor(elas)), linetype = "dashed") + 
  scale_y_continuous(expression("Cumulative Revenue"~frac(Dynamic, Fixed)), labels = scales::percent_format()) +
  scale_x_datetime("Time of day", labels = date_format("%l%p", tz='EST'), date_breaks = "3 hour") +
  coord_cartesian(xlim = c(as.POSIXct('2000-01-01 00:00:00 EST', tz='EST'), as.POSIXct('2000-01-01 23:30:00 EST', tz='EST')),
                  ylim = c(0, 1.25)) +
  scale_color_brewer("Tolling scheme", palette = "Set1", labels = dlabs, limits = c("Fixed toll", unique(dat.elas$elas))) +
  theme_classic() +
  theme(legend.position = "right",#c(0.75,0.2),
        legend.direction = "vertical",
        legend.background = element_blank())
# plot[['revsum']]

#### Plots: Heatmap matrix comparisons ####

# # # # # # # Pmax vs Pmin # # # # # # # # # # 
#### Total revenue by Pmax and Pmin
mmrevcodes <- fun.coder(mat.maxmin$revdiff, 25)

plot[['maxmin.rev']] <- ggplot(mat.maxmin, aes(x = dis, y = sur)) +
  geom_raster(aes(fill = cut(100*revdiff, mmrevcodes$cuts, include.lowest = T))) +
  geom_contour(breaks = mmrevcodes$cuts, aes(z = 100*revdiff), color = "black") +
  geom_smooth(data = mat.maxmin[ revdiff > -0.01 & revdiff < 0.01, ], 
              aes(x = dis, y = sur, linetype = "Break even point"), 
              size = 1.5, color = "black", formula = 'y~x', method = "loess", se=F) +
  scale_x_continuous(expression("Lower price limit,"~frac(P[min],P[fix])), expand = c(0,0), labels = scales::percent_format()) +
  scale_y_continuous(expression("Upper price limit,"~frac(P[max],P[fix])), expand = c(0,0), labels = scales::percent_format()) +
  scale_fill_manual(expression("Percent change\nin revenue"), label = mmrevcodes$labs, values = mmrevcodes$colors) +
  scale_linetype(NULL) +
  coord_fixed(ratio = max(mat.maxmin$dis)/max(mat.maxmin$sur),
              xlim = c(0, max(mat.maxmin$dis)),
              ylim = c(0, max(mat.maxmin$sur))) +
  theme_bw()
# plot[['maxmin.rev']]


#### Travel time by Pmax and Pmin
mmdelaycodes <- fun.coder(mat.maxmin$delaydiff, 5, direction = -1)

plot[['maxmin.delay']] <- ggplot(mat.maxmin, aes(x = dis, y = sur)) +
  geom_raster(aes(fill = cut(100*delaydiff, mmdelaycodes$cuts, include.lowest = T))) +
  geom_contour(breaks = mmdelaycodes$cuts, aes(z = 100*delaydiff), color = "black") +
  geom_smooth(data = mat.maxmin[ delaydiff > -0.01 & delaydiff < 0.01, ], 
              aes(x = dis, y = sur, linetype = "Break even point"), 
              size = 1.5, color = "black", formula = 'y~x', method = "loess", se=F) +
  scale_x_continuous(expression("Lower price limit,"~frac(P[min],P[fix])), expand = c(0,0), labels = scales::percent_format()) +
  scale_y_continuous(expression("Upper price limit,"~frac(P[max],P[fix])), expand = c(0,0), labels = scales::percent_format()) +
  scale_fill_manual(expression("Percent change\nin total delay"), label = mmdelaycodes$labs, values = mmdelaycodes$colors) +
  scale_linetype(NULL) +
  coord_fixed(ratio = max(mat.maxmin$dis)/max(mat.maxmin$sur),
              xlim = c(0, max(mat.maxmin$dis)),
              ylim = c(0,max(mat.maxmin$sur))) +
  theme_bw()
# plot[['maxmin.delay']]


# # # # # # # # # Break even points # # # # # #
#### Total revenue by Elasticity vs break even point
plot[['breakeven']] <- ggplot(data = mat.maxminelas[elas == as.character(E), .SD[which.min(abs(revdiff))], by = .(dis,elas)]) +
  #Revenue lines
  geom_smooth(aes(x = dis, y = sur), color = "black", formula = 'y~x', method = "loess", se=F, fullrange = T, span = 1) +
  geom_ribbon(aes(x = dis, y = sur, ymin = 0, ymax = predict(loess(sur ~ dis)), fill = "Revenue negative and\ndelay reducing"), alpha = 0.5) +
  geom_ribbon(aes(x = dis, y = sur, ymin = predict(loess(sur ~ dis)), ymax = 5, fill = "Revenue positive and\ndelay reducing"), alpha = 0.5) +
  #Delay lines
  geom_smooth(data = mat.maxminelas[ , .SD[which.min(abs(delaydiff))], by = .(dis,elas)],
              aes(x = dis, y = sur, color = "Delay breakeven point"), color = "black",
              formula = 'y~x', method = "loess", se=F, fullrange = T, span = 1, fill = "green") +
  geom_ribbon(data = mat.maxminelas[ , .SD[which.min(abs(delaydiff))], by = .(dis,elas)],
              aes(x = dis, y = sur, ymin = 0, ymax = predict(loess(sur ~ dis)), fill = "Revenue negative and\ndelay increasing"), alpha = 0.5) +
  scale_x_continuous(expression("Lower price limit,"~frac(P[min],P[fix])), expand = c(0,0), labels = scales::percent_format()) +
  scale_y_continuous(expression("Upper price limit,"~frac(P[max],P[fix])), expand = c(0,0), labels = scales::percent_format()) +
  scale_fill_brewer("Outcome region", palette = "RdYlBu") +
  #scale_linetype(expression("Elasticity,"~epsilon)) +
  coord_fixed(ratio = max(mat.maxminelas$dis)/max(mat.maxminelas$sur),
              xlim = c(0, max(mat.maxminelas$dis)),
              ylim = c(0, 4)) +
  theme_bw()
# plot[['breakeven']]



# # # # # # # # # Elasticity vs Pmax while Pmin = 0 # # # # # # # #
#### Revenue by Elasticity vs Pmax
Erevcodes <- fun.coder(mat.maxelas$revdiff, 25, direction = 1)

plot[['emax.rev']] <- ggplot(mat.maxelas, aes(x = elas, y = sur)) +
  geom_raster(aes(fill = cut(100*revdiff, Erevcodes$cuts, include.lowest = T))) +
  geom_contour(breaks = Erevcodes$cuts, aes(z = 100*revdiff), color = "black") +
  scale_x_continuous(expression("Price Elasticity of Demand, "~epsilon), expand = c(0,0)) +
  scale_y_continuous(expression("Upper price limit,"~P[max]), expand = c(0,0), labels = scales::percent_format()) +
  scale_fill_manual(expression("Percent change\nin total delay"), label = Erevcodes$labs, values = Erevcodes$colors) +
  coord_fixed(ratio = max(mat.maxelas$elas) / max(mat.maxelas$sur),
              xlim = c(0, max(mat.maxelas$elas)),
              ylim = c(0, max(mat.maxelas$sur))) +
  theme_bw()
# plot[['emax.rev']]

#### Travel time by Elasticity vs Pmax
Edelaycodes <- fun.coder(mat.maxelas$delaydiff, 5, direction = -1)

# ggplot(mat.elas, aes(x = elas, y = surge, z = 100*delaydiff)) + geom_contour_filled()
plot[['emax.delay']] <- ggplot(mat.maxelas, aes(x = elas, y = sur)) +
  geom_raster(aes(fill = cut(100*delaydiff, Edelaycodes$cuts, include.lowest = T))) +
  geom_contour(breaks = Edelaycodes$cuts, aes(z = 100*delaydiff), color = "black") +
  scale_x_continuous(expression("Price Elasticity of Demand, "~epsilon), expand = c(0,0)) +
  scale_y_continuous(expression("Upper price limit,"~P[max]), expand = c(0,0), labels = scales::percent_format()) +
  scale_fill_manual(expression("Percent change\nin total delay"), label = Edelaycodes$labs, values = Edelaycodes$colors) +
  coord_fixed(ratio = max(mat.maxelas$elas) / max(mat.maxelas$sur),
              xlim = c(0, max(mat.maxelas$elas)),
              ylim = c(0, max(mat.maxelas$sur))) +
  theme_bw()
# plot[['emax.delay']]


#### Saving ####
# ggsave("../figures/toyprice.pdf", plot[['price']], device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyelasticity.pdf", plot[['elasticity']], device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toydemand.pdf", plot[['demanddensity']], device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toytoll.pdf", plot[['toll']], device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/toyrevenue.pdf", plot[['revenue']], device = cairo_pdf, width=4.25, height=2, units = "in")
# ggsave("../figures/KDE.pdf", plot[['kernel']], device = cairo_pdf, width=4.25, height=2, units = "in")



