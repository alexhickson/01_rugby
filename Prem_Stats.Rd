# the XML library is used to data scrape match stats from ESPN HTML pages 
require(XML)

#  Example of a page used is:
# http://www.espn.co.uk/rugby/matchstats?gameId=99170&league=267979

# each number in vector Y corresponds to the first game of each season 2008 to 2017
# Years 2009,  2010,  2011,   2012,   2013,   2014,   2015,   2016,   2017
# Y = c(28141, 99164, 118616, 142486, 166464, 188681, 231903, 267707, 289993)

Y = c(28141, 99170, 118616, 142486, 166464, 188681, 231903, 267707, 289993)

# generate the loop vector used to generate all the page ids in each html page on ESPN
loop <- NULL;
for (i in c(1:length(Y))){
 loop = c(loop,Y[i]:(Y[i]+131))
}

# below can be uncommented if we need to hardcode the loop vector
# loop = c(118613:118615, 289920:289922)
loop = c(289993)

# create the Overall_Stats data frame using a loop for each HTML page
rm(Summary,KickPass,Attack)
Overall_Stats <- NULL;
for (i in c(1:length(loop))){	

	message('Processing match ', i, '/',length(loop))		
	u = paste("http://www.espn.co.uk/rugby/matchstats?gameId=",loop[i],"&league=267979",sep="");
	doc = htmlParse(u);
	titleNodes = getNodeSet(doc, "//title")
	tableNodes = getNodeSet(doc, "//table")
	tackleNodes = getNodeSet(doc, "//span[@class='home-team']")
	
	Match = capture.output(titleNodes[[1]]);
	HomeTeam = substr(Match,8,regexpr("vs",Match)-2)
	AwayTeam = substr(Match,regexpr("vs",Match)+3,regexpr(" - ",Match)-1);
	Year = substr(Match,regexpr("20",Match),regexpr("20",Match)+3);
	
	Summary = readHTMLTable(tableNodes[[1]]);
	KickPass = readHTMLTable(tableNodes[[2]]);
	Attack = readHTMLTable(tableNodes[[3]]);
	Tackles = capture.output(tackleNodes[1:2])
	Tackles = substr(Tackles[c(2,5)],regexpr(">",Tackles[c(2,5)])+1,regexpr("</",Tackles[c(2,5)])-1);
	
	if(Match == "<title/> "){
		message("no data found, processing as NULL");
		Stats = matrix(rep("null",51),3,17);
		}	else{
		Stats = t(rbind(matrix(c(Year,"Year",Year),1,3),Summary, KickPass, Attack,matrix(c(Tackles[1],"Tackles",Tackles[2]),1,3)))	;
	}	
	colnames(Stats) <- Stats[2,];
	Stats = Stats[-c(2),]
	rownames(Stats) = c(HomeTeam, AwayTeam)
	Overall_Stats = rbind(Overall_Stats,Stats);
}

# Write the Overall_Stats data frame to a text file.
# Comment the below line out to not generate text file output
# write.table(Overall_Stats, "Overall_Stats.txt")

# -------- NOTES IN BELOW COMMENTS
# 184, 262:264, 425, 436, 528 empty
# game ids 231999 (BATH QUINS 28/11/14)  empty