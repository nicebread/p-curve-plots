library(pwr)
source("p-curve-comp.R")

# ---------------------------------------------------------------------
# H0 plot
dev.new(width=6, height=4)
plot(NA, xlim=c(0, 1), ylim=c(0, 5), xlab="p value", ylab="Density", bty="n", par(bg="white"))
lines(c(0, 0), c(0, 1), col="black", lwd=1, lty="dotted")
lines(c(1, 1), c(0, 1), col="black", lwd=1, lty="dotted")
lines(c(0, 1), c(0, 0), col="black", lwd=1, lty="dotted")
rect(0, 0, 1, 1, col="#FCD9D9", lwd=0)
lines(c(0, 1), c(1, 1), col="red", lwd=2)

rect(0, 0, 0.05, 1, col="firebrick")
# font=2: bold
text(0.025, 0.5, "5%", col="white", offset=2, font=2, srt=90)


# ---------------------------------------------------------------------
# H1 plots
pcurve.plot <- function(power=.10, p.max=.9999, ymax=5, sig.region=FALSE) {
	ps <- seq(.001, p.max, length.out=1000)
	dev.new(width=6, height=4)
	plot(NA, xlim=c(0, p.max), ylim=c(0, ymax), xlab="p value", ylab="Density", bty="n", par(bg="white"))

	pcurve <- dpvalue(ps, power=power)
	polygon(c(ps, rev(ps)), c(pcurve, rep(0, length(ps))), col="#DFFABE", lty="dotted")
	lines(ps, pcurve, col="green", lwd=2)
	lines(c(0, 1), c(1, 1), col="red", lwd=1, lty="dashed")
	
	if (sig.region==TRUE) {
		polygon(c(ps[ps<=.05], rev(ps[ps<=.05])), c(pcurve[ps<=.05], rep(0, length(ps[ps<=.05]))), col="#93F759", lty="dotted")
		# font=2: bold
		text(0.025, 0, paste0(round(power*100), "%"), col="black", adj=c(-0.1,0.5), font=2, srt=90)
	}
}

pcurve.plot(.10, ymax=8)
pcurve.plot(.10, ymax=8, sig.region=TRUE)

pcurve.plot(.35, ymax=12, sig.region=TRUE)

pcurve.plot(.80, ymax=30, sig.region=TRUE)

pcurve.plot(.95, ymax=30, sig.region=TRUE)


# ---------------------------------------------------------------------
# Special plot: p-curve with region .04 - .05
p.max=.9999
ymax=22
power=.65
ps <- seq(.001, p.max, length.out=1000)
dev.new(width=6, height=4)
plot(NA, xlim=c(0, p.max), ylim=c(0, ymax), xlab="p value", ylab="Density", bty="n", par(bg="white"))

pcurve <- dpvalue(ps, power=power)
polygon(c(ps, rev(ps)), c(pcurve, rep(0, length(ps))), col="#DFFABE", border=NA)
lines(ps, pcurve, col="green", lwd=2)

polygon(c(ps[ps<=.05 & ps >= .03], rev(ps[ps<=.05 & ps >= .03])), c(pcurve[ps<=.05 & ps >= .03], rep(0, length(ps[ps<=.05 & ps >= .03]))), col="#93F759", lty="dotted")



# ---------------------------------------------------------------------
# Special plot 2: p-curve with region 0% - 2.5% and 2.5% - 5%
p.max=0.2
ymax=50
power=.60
ps <- seq(.001, p.max, length.out=1000)
dev.new(width=6, height=4)
plot(NA, xlim=c(0, p.max), ylim=c(0, ymax), xlab="p value", ylab="Density", bty="n", par(bg="white"))

pcurve <- dpvalue(ps, power)
polygon(c(ps, rev(ps)), c(pcurve, rep(0, length(ps))), col="#FC8C6C", border=NA)

polygon(c(ps[ps<=.05 & ps >= .025], rev(ps[ps<=.05 & ps >= .025])), c(pcurve[ps<=.05 & ps >= .025], rep(0, 
length(ps[ps<=.05 & ps >= .025]))), col="#FAC12A", border=NA)

polygon(c(ps[ps<=.025], rev(ps[ps<=.025])), c(pcurve[ps<=.025], rep(0, length(ps[ps<=.025]))), col="#83F41F", border=NA)

lines(ps, pcurve, col="grey20", lwd=1)

