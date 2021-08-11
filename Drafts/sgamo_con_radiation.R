radold <- read.table("./Inputs/Model_input/Base/Time_series/radiation.dat", skip = 3)
radnew <- read.table("./Inputs/Model_input/MEs/2_Time_series_reanalysis/radiation.dat", skip = 3)

radold[,1] <- change_date_WASA_input(radold[,1])
radnew[,1] <- change_date_WASA_input(radnew[,1])

x <- radold[radold[,1] <= as.Date("1980-12-31"), ]
rad_f <- rbind(x, radnew)
names(rad_f)[1] <- "date"


path_export <- "./Inputs/Model_input/MEs/1_Time_series_update/"
date_df <- data.frame(date = create_date_vector(1980, 2020))
h = "Daily average shortwave radiation [in Wm2] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
rad_f <- WASA_input_format(date_df, rad_f, header = h, path = path_export, name = "radiation")
