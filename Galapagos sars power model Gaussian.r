# Loading sars package
require(sars)
# Reading galap data from paquete sars
# galap contains: a, s (Area & species richness, respectively)
data(galap)
attach(galap)
ntotal <- length(s)
# sar-power fits a power model by means of non-linear least squares
galap.power.sars <- sar_power(data=galap)
summary(galap.power.sars)
dev.new()
windows()
plot(galap.power.sars, ModTitle = "")
