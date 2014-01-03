e <- l[[1]]
df <- data.frame(l[[1]], stringsAsFactors=FALSE)
df2 <- df

for(i in 2:length(l)) {
	df_temp <- data.frame(l[[i]], stringsAsFactors=FALSE)
	df2 <- rbind(df2, df_temp)
}