wd <- "Allocation/"

library(icesDatras)
library(data.table)

hh <- rbind(getDATRAS(record = "HH", survey = "BITS", c(2020:2025), 1),
            getDATRAS(record = "HH", survey = "BITS", c(2020:2024), 4))


dat <- data.frame(table(hh$Country, hh$StatRec))
names(dat)[1:2] <- c("Country", "Rect")
dat <- dat[dat$Freq> 0, ]

shared <- dat[dat$Rect %in% dat$Rect[duplicated(dat$Rect)], ]
national <- dat[! dat$Rect %in% dat$Rect[duplicated(dat$Rect)], ]

#set % allocation pr country
setDT(dat)
dat <- dat[, 'Pct' := (Freq/sum(Freq))*100, by=.(Rect)]

dat$Freq <- NULL
saveRDS(dat, paste0(wd, "data/nation_rect_distr.rds"))
