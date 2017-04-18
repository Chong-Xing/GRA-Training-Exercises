## 20150918


library(openxlsx)

launch <- "../workingdata/"
outdir <- "../output/"
dts <- function(name) gsub("/$", "", name)
if(!file.exists(dts(outdir))) dir.create(outdir, recursive = TRUE)

library(openxlsx)
dat <- read.xlsx(paste0(launch, "district.xlsx"), startRow = 3)

plot(phys ~ Year, data = dat, type = "n", main = "phys")
text(phys ~ Year, data = dat, labels = USDNo)

plot(soc ~ Year, data = dat, type = "n", main = "soc")
text(soc ~ Year, data = dat, labels = USDNo)

plot(sym ~ Year, data = dat, type = "n", main = "sym")
text(sym ~ Year, data = dat, labels = USDNo)

plot(gk ~ Year, data = dat, type = "n", main = "gk")
text(gk ~ Year, data = dat, labels = USDNo)

plot(oc ~ Year, data = dat, type = "n", main = "oc")
text(oc ~ Year, data = dat, labels = USDNo)

plot(wl ~ Year, data = dat, type = "n", main = "wl")
text(wl ~ Year, data = dat, labels = USDNo)

plot(mc ~ Year, data = dat, type = "n", main = "mc")
text(mc ~ Year, data = dat, labels = USDNo)

plot(wh ~ Year, data = dat, type = "n", main = "wh")
text(wh ~ Year, data = dat, labels = USDNo)

plot(att ~ Year, data = dat, type = "n", main = "att")
text(att ~ Year, data = dat, labels = USDNo)


