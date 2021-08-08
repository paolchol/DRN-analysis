hum <- read.table("./Inputs/Model_input/Model_enhancement/Time_series_update/humidity.dat", skip = 3)
columns <- c("date", "doy", "ID123", "ID125", "ID126", "ID127", "ID134", "ID137", "ID138", "ID139", "ID142", "ID143", "ID144", "ID145", "ID146", "ID147", "ID148", "ID149", "ID150", "ID151", "ID152", "ID153", "ID154", "ID155", "ID156", "ID157", "ID158", "ID159", "ID160")
names(hum) <- columns
hum$date <- change_date_WASA_input(hum$date)

hum<- hum[hum$date >= as.Date("1981-01-01"), ]

path_export <- "./Inputs/Model_input/Model_enhancement/Time_series_reanalysis"

h = "Daily average humidity [in %] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_hum <- WASA_input_format(hum, hum, header = h, path = path_export, name = "humidity")
