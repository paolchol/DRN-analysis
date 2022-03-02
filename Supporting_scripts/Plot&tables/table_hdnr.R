#Table construction

setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")


HDNR <- readOGR("./Data/HDNR/HDNR.shp")

View(HDNR@data)

res_class<-c(
  5000, #m3
  25000,
  50000,
  100000,
  max(HDNR$capacity)
)

table <- data.frame(class = seq(1,5,1))
table$Upper_bound <- res_class
table$n_res <- 0
table$perc_n <- 0
table$volume <- 0
for(i in 1:nrow(table)){
  table$n_res[i] <- sum(HDNR$class == table$class[i])
  table$volume[i] <- sum(HDNR$capacity[HDNR$class == table$class[i]])
}
table$perc_n <- table$n_res/sum(table$n_res) * 100
table$perc_vol <- table$volume/sum(table$volume) * 100

tot <- c("Total", "-", sum(table$n_res), "100", sum(table$volume), "100")
table <- rbind(table, tot)

View(table)

options(scipen = 0)
write.table(table, file = "./Data/Plot_table/small_res_rip.txt", sep = "\t", quote = FALSE)


