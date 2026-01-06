wd <- "Q:/20-forskning/20-dfad/users/jostou/home/yearly/WGBIFS/Allocation/"

library(icesDatras)
library(data.table)

hh <- rbind(getDATRAS(record = "HH", survey = "BITS", c(2024:2025), 1),
            getDATRAS(record = "HH", survey = "BITS", c(2023:2024), 4))


hh <- rbind(getDATRAS(record = "HH", survey = "BITS", c(2024), 4))


dat <- data.frame(table(hh$Country, hh$StatRec))
names(dat)[1:2] <- c("Country", "RectangleAlpha")
dat <- dat[dat$Freq> 0, ]

shared <- dat[dat$RectangleAlpha %in% dat$RectangleAlpha[duplicated(dat$RectangleAlpha)], ]
national <- dat[! dat$RectangleAlpha %in% dat$RectangleAlpha[duplicated(dat$RectangleAlpha)], ]

#set % allocation pr country
setDT(dat)
dat <- dat[, 'Pct' := (Freq/sum(Freq))*100, by=.(RectangleAlpha)]

dat$Freq <- NULL
saveRDS(dat, paste0(wd, "data/nation_rect_distr.rds"))
