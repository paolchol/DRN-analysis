#Update of reservoir.dat and cav.dat

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")

source("Libraries.R")
source("Functions.R")

setwd("C:/Thesis_fortran/WASA")

#Input list
input<-list.files(path="./Input",full.names = T,recursive = TRUE)
input_index<-as.data.frame(matrix(nrow = length(input),ncol = 2))
names(input_index)<-c("path","description")
input_index$path<-input; remove(input)
for(i in 1:length(input_index$path)){
  input_index$description[i]<-readLines(input_index$path[i], n=5)[1]
}
View(input_index)

#Import the values to correctly read each file
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis"
read_value<-read.table(paste0(path,"/sk_vn_sep.txt"),header = TRUE,sep = "\t")

#cav.dat to update
# cav_u<-read.table(input_index$path[12], skip = read_value$sk[12], sep = "\t")
# vn<-as.numeric(read_value$vn[12])
# if(read_value$sep[12] == 1){
#   col<-unlist(strsplit(readLines(input_index$path[12], n = 5)[vn], "[,]"))
# }else col<-unlist(strsplit(readLines(input_index$path[12],n = 5)[vn], " "))
# names(cav_u)<-col; remove(col)

#reservoir.dat to update
reservoir_u<-read.table(input_index$path[17], skip = read_value$sk[17], sep = "\t")
vn<-as.numeric(read_value$vn[17])
if(read_value$sep[17] == 1){
  col<-unlist(strsplit(readLines(input_index$path[17], n = 5)[vn], "[,]"))
}else col<-unlist(strsplit(readLines(input_index$path[17],n = 5)[vn], " "))
names(reservoir_u)<-col; remove(col)

#New resevoirs information
#Morada Nova
#sources:
#http://atlas.cogerh.com.br/
#https://portal.cogerh.com.br/wp-content/uploads/pdf/inventarios/2012/Inventario%20Ambiental%20do%20acude%20Curral%20Velho.pdf
#Minimum elevation: https://arquivos.ana.gov.br/imprensa/eventosprojetos/XX_SeminarioAlocacaoValesJaguaribeBanabuiu_2013.pdf
#Min and max elevation: hidro.ce.gov.br


Morada_Nova<-c(
  138, #Subasin-ID
  75, #from Google Earth #minlevel[m]  initial minimum level
  84.6, #cota sangradouro #maxlevel[m]  maximum...
  -999, #vol0([1000m**3]; unknown=-999) initial volume 
  12165.745, #storcap[1000m**3]  initial storage capacity
  0.401, #damflow[m**3/s]  target release through the barrage's intake devices
  0.9, #damq_frac[-] maximum fraction of damflow which is released from the subbasin reservoir
  0.183, #withdrawal[m**3/s] water withdrawal to supply the water use sector
  2007, #damyear[YYYY]  year of construction
  413, #maxdamarea[ha] initial maximum area
  1216.57, #put equal to alert #damdead[1000m**3] initial dead volume
  1216.57, #storcap/10 #damalert[1000m**3] initial alert volume, should be set to volume at the height of the barrage intake devices
  57.5, #dama[-]  a parameter in the area volume relationship
  0.68, #damb[-]  b parameter in the area volume relationship
  0, #qoutlet[m**3/s]  maximum outflow discharge released through the bottom outlets
  1, #fvol_bottom[-] fraction of storage capacity, indicates the minimum storage volume
  1, #fvol_over[-] flag to simulate the retention of reservoir overflow
  80, #damc[-] parameter of the spillway rating curve
  1.5, #damd[-] parameter of the spillway rating curve
  75 #elevbottom[m]  bottom outlet elevation (not used, dummy values)
)


#Umari
#sources:
#storcap:  hidro.ce.gov.br
#http://atlas.cogerh.com.br/
#Min and max elevation: hidro.ce.gov.br
#and https://documents1.worldbank.org/curated/en/791031468015574409/pdf/E19320v40PORTU1RSAO0FINAL0110NOV008.pdf

Umari<-c(
  142, #Subasin-ID
  294, #from hidro.ce.gov.br  initial minimum level
  310, #cota sangradouro #maxlevel[m]  maximum...
  -999, #vol0([1000m**3]; unknown=-999) initial volume 
  30000, #storcap[1000m**3]  initial storage capacity
  0.39, #Vazão Regularizada #damflow[m**3/s]  target release through the barrage's intake devices
  0.9, #damq_frac[-] maximum fraction of damflow which is released from the subbasin reservoir
  0.229, #withdrawal[m**3/s] water withdrawal to supply the water use sector
  2010, #damyear[YYYY]  year of construction
  738.28, #maxdamarea[ha] initial initial maximum area
  3000, #put equal to alert #damdead[1000m**3] initial dead volume
  3000, #storcap/10 #damalert[1000m**3] initial alert volume, should be set to volume at the height of the barrage intake devices
  11.883, #dama[-]  a parameter in the area volume relationship
  0.787, #damb[-]  b parameter in the area volume relationship
  0, #qoutlet[m**3/s]  maximum outflow discharge released through the bottom outlets
  1, #fvol_bottom[-] fraction of storage capacity, indicates the minimum storage volume
  1, #fvol_over[-] flag to simulate the retention of reservoir overflow
  96, #damc[-] parameter of the spillway rating curve
  1.5, #damd[-] parameter of the spillway rating curve
  294 #elevbottom[m]  bottom outlet elevation (not used, dummy values)
)

#Estimation of the Molle equation's parameters for the two reservoirs
#source: Molle, 1994 (pag. 20)

path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/new_reservoirs"
H_vol<-read.table(paste0(path,"/H_volume_points_Curral.txt"),skip = 1, sep = "\t")
H_vol_C<-data.frame(t(H_vol)); names(H_vol_C)<-c("elevation","volume")
H_vol<-read.table(paste0(path,"/H_volume_points_Umari.txt"),skip = 1, sep = "\t")
H_vol_U<-data.frame(t(H_vol)); names(H_vol_U)<-c("elevation","volume")
H_vol<-read.table(paste0(path,"/H_volume_points_Quixeramobim.txt"),skip = 1, sep = "\t")
H_vol_Q<-data.frame(t(H_vol)); names(H_vol_Q)<-c("elevation","volume")


#Curral Velho
H_vol_C$depth<-c(0,H_vol_C$elevation[2:10]-H_vol_C$elevation[1:9])
H_vol_C$depth<-cumsum(H_vol_C$depth)
H_vol_C$volume<-H_vol_C$volume*1000 #to have m3
lm_C = lm(log(H_vol_C$volume[2:10])~log(H_vol_C$depth[2:10]))
#summary(lm_C)
k = as.numeric(exp(lm_C$coefficients[1])) #k
alpha = as.numeric(lm_C$coefficients[2])  #alpha

##Parameters for WASA
dama = (1/k * (alpha * k)^(alpha/(alpha-1)))^((alpha-1)/alpha)
#57.500
damb = (alpha-1)/alpha #(damb should be > 0 and <= 1)
#0.6801479

#Umari
H_vol_U$depth<-c(0,H_vol_U$elevation[2:14]-H_vol_U$elevation[1:13])
H_vol_U$depth<-cumsum(H_vol_U$depth)
H_vol_U$volume<-H_vol_U$volume*1000 #to have m3
lm_U = lm(log(H_vol_U$volume[2:14])~log(H_vol_U$depth[2:14]))
#summary(lm_U)
k = as.numeric(exp(lm_U$coefficients[1])) #k
alpha = as.numeric(lm_U$coefficients[2])  #alpha

##Parameters for WASA
dama = (1/k * (alpha * k)^(alpha/(alpha-1)))^((alpha-1)/alpha)
#11.883
damb = (alpha-1)/alpha #(damb should be > 0 and <= 1)
#0.787

#Quixeramobim
H_vol_Q$depth<-c(0,H_vol_Q$elevation[2:11]-H_vol_Q$elevation[1:10])
H_vol_Q$depth<-cumsum(H_vol_Q$depth)
H_vol_Q$volume<-H_vol_Q$volume*1000 #to have m3
lm_Q = lm(log(H_vol_Q$volume[2:14])~log(H_vol_Q$depth[2:14]))
#summary(lm_Q)
k = as.numeric(exp(lm_Q$coefficients[1])) #k
alpha = as.numeric(lm_Q$coefficients[2])  #alpha

##Parameters for WASA
dama = (1/k * (alpha * k)^(alpha/(alpha-1)))^((alpha-1)/alpha)
#115.57
damb = (alpha-1)/alpha #(damb should be > 0 and <= 1)
#0.748


#Computation of the parameter damc of the spillway rating curve
#Spillway rating curve equation: Q = damc * H^(damd)
#damc = damd*C*L
#damd = 1.5
#C = weir coefficient
#L = width of weir (m)

#Computation of the withdrawal as the Q90 of the outflow from the reservoir
#Linear regression model between capacity and withdrawal
# The Q90 value indicates that 90% of the time, stream flow has been greater
# than that value. In other words, the stream flow has only been that level or
# below 10% of the time. Q90 is considered protected low flow level in Minnesota
# and is used for suspending water appropriation permits.

dataset<-data.frame(reservoir_u$` storcap[1000m**3]`,reservoir_u$` withdrawal[m**3/s]`)
names(dataset)<-c("capacity","Q90")

regressor = lm(formula = Q90 ~ capacity, data = dataset)
summary(regressor)

new_df<-data.frame(capacity=c(Morada_Nova[5], Umari[5]))
pred = predict(regressor, newdata = new_df)


dataset<-data.frame(reservoir_u$` storcap[1000m**3]`,reservoir_u$` damc[-]`)
names(dataset)<-c("capacity","damc")

regressor2 = lm(formula = damc ~ capacity, data = dataset)
summary(regressor2)

new_df<-data.frame(capacity=c(Morada_Nova[5], Umari[5], 7880))
pred2 = predict(regressor2, newdata = new_df)

plot(dataset)
