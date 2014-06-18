library(RMySQL)

username = 'crowdpacdata'
passwd = 'crowdpacdata2014'
dbname = 'cfdb'
port = 5841
host = 'cp-data-analysis3.cbtto5axwp1k.us-east-1.rds.amazonaws.com'

con <- dbConnect(MySQL(), user=username, password=passwd, dbname=dbname, port=port, host=host)


prioritycands <- read.csv('~/workspace/crowdpac/prioritycands.csv',stringsAsFactors = FALSE)

numrows <- nrow(prioritycands)

# m <- match(prioritycands[,'bonica_rid'],'cand9996649') # kashkari
# CAND <- which(!is.na(m))

for (i in 1:numrows) {
# for (i in CAND) {
	temp <- prioritycands[i,]

	# temp[which(temp=='')] <- NA
	# query <- paste("INSERT INTO cfdb.newconts VALUES('", paste(temp, collapse = "', '"), "')",sep='')

	query1 <- "INSERT INTO cfdb.newconts VALUES("
	query2 <- paste(temp[1],", ",sep='')
	query3 <- paste("'",paste(temp[2:4],collapse="', '"),"', ",sep='')
	query4 <- paste(paste(temp[5:6],collapse=", "),", ",sep='')
	query5 <- paste("'",paste(temp[7:23],collapse="', '"),"', ",sep='')
	query6 <- paste(temp[24],", ",sep='')
	query7 <- paste("'",paste(temp[25:44],collapse="', '"),"', ",sep='')
	query8 <- paste(paste(temp[45:52],collapse=", "),", ",sep='')
	query9 <- paste("'",paste(temp[53:55],collapse="', '"),"', ",sep='')
	query10 <- paste(paste(temp[56:61],collapse=", "),", ",sep='')
	query11 <- paste("'",paste(temp[62:64],collapse="', '"),"', ",sep='')
	query12 <- paste(paste(temp[65:68],collapse=", "),", ",sep='')
	query13 <- paste("'",temp[69],"', ",sep='')
	query14 <- paste(temp[70],", ",sep='')
	query15 <- paste("'",paste(temp[71:82],collapse="', '"),"', ",sep='')
	query16 <- paste(paste(temp[83:91],collapse=", "),", ",sep='')
	query17 <- paste("'",paste(temp[92:97],collapse="', '"),"'",sep='')
	queryLast <- ");"

	fullQuery <- paste(query1, query2, query3, query4, query5, 
		               query6, query7, query8, query9, query10,
		               query11, query12, query13, query14, query15,
		               query16, query17, queryLast, sep='')

	try(dbGetQuery(con,fullQuery),silent=TRUE)

	print(paste(i,"of",numrows,sep=' '))

}
