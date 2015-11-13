babies = read.table("/opt/data/babies23.txt", header = T)

babies = babies[c(7, 5, 8, 10, 12, 13, 21, 11)]
names(babies) = c("bwt", "gestation", "parity", "age", "height", "weight", "smoke", "education")

babies[babies$bwt == 999, 1] = NA
babies[babies$gestation == 999, 2] = NA
babies[babies$age == 99, 4] = NA
babies[babies$height == 99, 5] = NA
babies[babies$weight == 999, 6] = NA
babies[babies$smoke == 9, 7] = NA
babies[babies$education == 9, 8] = NA

babies$smoke = factor(c("NonSmoking", "Smoking", "NonSmoking", "NonSmoking")[babies$smoke+1])
babies$education = factor(babies$education, ordered = T)

babies
