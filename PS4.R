## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\PS4")

library(rvest)   # loading in required library 
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'
# creating an object with specified web link

wiki_text <- read_html(wikiURL) #reading the html code from the link
wiki_tables <- html_nodes(wiki_text, "table") # using the html code, locating all instances
                                              # of table on the page
wiki_data <- html_table(wiki_tables[2])[[1]] # wikitables is a list of 4; after manually
                                             # checking each table, we see that we want
                                             # table 2; the output of html_table is a list
                                             # so we select the first element of the list,
                                             # which is a data frame
# cleaning data
wiki_data <- wiki_data[3:length(wiki_data$Election),] # first two rows of data frame
                                                      # are subheaders, so we discard them
wiki_data <- apply(wiki_data, MARGIN=2, FUN=function(x) gsub("\\%","",x))
# removing all percent signs from data; \\ is required as escape
wiki_data <- apply(wiki_data, MARGIN=2, FUN=function(x) gsub("\\[a\\]","",x))
# removing all footnote indicators from data; \\ is required as escape
wiki_data[,9:10] <- apply(wiki_data[,9:10], MARGIN=2, FUN=function(x) gsub(",","",x))
# removing commas from vote totals
wiki_data[,8:10] <- apply(wiki_data[,8:10], MARGIN=2, FUN=function(x) 
  ifelse(substr(x,1,1) %in% c(0:9)==FALSE, paste0("-",substr(x,2,nchar(x))), x))
# replacing long subtraction characters '???' with short subtraction characters '-'
# note: Ryden and JB could replace the long subtraction sign with a gsub, but for some
# reason I was unable to even when running their same code; thus, I had to construct
# this workaround which checks to see if the first character is not a number from 0 to 9,
# and if it isn't, replace the value with a minus sign pasted to the number in the cell
wiki_data <- as.data.frame(wiki_data, stringsAsFactors=FALSE)
# coercing wiki_data back to a data.frame object
wiki_data[,-c(3,4,5,11,12)] <- apply(wiki_data[,-c(3,4,5,11,12)], MARGIN=2, 
                   FUN=function(x) as.numeric(x))
# for all numeric columns, change from character to numeric
colnames(wiki_data) <- c("Election_Number", "Election_Year", "Winner", "Winner_Party",
                         "Electoral_College_Proportion", "Electoral_College_Percentage",
                         "Popular_Vote_Percentage", "Popular_Vote_Margin", 
                         "Total_Popular_Vote", "Total_Popular Vote_Margin", "Runner-up",
                         "Runner-up_Party", "Turnout_Percentage")
# rename all columns to more intuitive names
wiki_data <- wiki_data[order(wiki_data$Election_Year),]
# sorting data by election year for plotting purposes

opar <- par()
# saving default plotting parameters so we can restore them after adding the legend
# for each plot

pdf("PresElectPlots.pdf")
# naming the file to which we will save the plots
layout(matrix(c(1,2), ncol=1, byrow=FALSE), heights = c(0.65,0.35))
# setting the layout to accommodate the plot and the legend
plot(NULL,
     main="Comparison of Electoral and Popular Vote\n Percentages for Winning Candidate",
     ylab="% of Vote Received", xlab="Election Year", ylim=c(35,100),
     xlim=c(1820,2020))
# creating a blank plot with the requisite labels and xlim and ylim
points(wiki_data$Election_Year[which(wiki_data$Winner_Party=="Rep.")],
       wiki_data$Electoral_College_Percentage[which(wiki_data$Winner_Party=="Rep.")],
       pch=19, col="firebrick1")
# adding solid red points for winning Republicans' EC votes; which function selects on years
# for which the Republican candidate won
points(wiki_data$Election_Year[which(wiki_data$Winner_Party=="Dem.")],
       wiki_data$Electoral_College_Percentage[which(wiki_data$Winner_Party=="Dem.")],
       pch=19, col="dodgerblue")
# adding solid red points for winning Democrats' EC votes which function selects on years
# for which the Democratic candidate won
points(wiki_data$Election_Year[which(wiki_data$Winner_Party=="Rep.")],
       wiki_data$Popular_Vote_Percentage[which(wiki_data$Winner_Party=="Rep.")],
       pch=1, col="firebrick1")
# adding solid red points for winning Republicans' popular votes; which function selects on years
# for which the Republican candidate won
points(wiki_data$Election_Year[which(wiki_data$Winner_Party=="Dem.")],
       wiki_data$Popular_Vote_Percentage[which(wiki_data$Winner_Party=="Dem.")],
       pch=1, col="dodgerblue")
# adding solid red points for winning Democrats' popular votes; which function selects on years
# for which the Democratic candidate won
segments(x0=wiki_data$Election_Year[which(wiki_data$Winner_Party=="Rep.")],
         y0=wiki_data$Electoral_College_Percentage[which(wiki_data$Winner_Party=="Rep.")],
         y1=wiki_data$Popular_Vote_Percentage[which(wiki_data$Winner_Party=="Rep.")],
         lty=4, col="firebrick1")
# adding dotted line segments connecting the points for winning Republicans;
# which function selects on years for which the Republican candidate won
segments(x0=wiki_data$Election_Year[which(wiki_data$Winner_Party=="Dem.")],
         y0=wiki_data$Electoral_College_Percentage[which(wiki_data$Winner_Party=="Dem.")],
         y1=wiki_data$Popular_Vote_Percentage[which(wiki_data$Winner_Party=="Dem.")],
         lty=4, col="dodgerblue")
# adding dotted line segments connecting the points for winning Democrats
# which function selects on years for which the Democratic candidate won

# adjusting plotting margins to create legend
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="") # creating null plot
# creating a legend for the null plot, which is effectively the legend for the above
# plot
legend("center",legend=c("% of Popular Vote Received", 
                         "% of Electoral College Vote Received",
                         "Difference in % Received", "Democratic Winner", 
                         "Republican Winner"), # providing items in legend
       col=c("black","black","black","dodgerblue","firebrick1"), 
       # providing color for each item in legend
       lty = c(NA,NA,4,1,1), # providing line type for each item in legend
       pch=c(19,1,NA,NA,NA)) # providing point type for each item in legend

par(opar) # resetting par for second plot

layout(matrix(c(1,2), ncol=1, byrow=FALSE), heights = c(0.75,0.25))
# setting layout for plot and legend
plot(wiki_data$Election_Year, wiki_data$Popular_Vote_Percentage, type="l", col="forestgreen",
     lty=1, main="Comparison of Turnout Rate with Support for Winning Candidate",
     ylab="Percentage", xlab="Election Year", ylim=c(25,100),
     xlim=c(1820,2020))
# creating a plot of popular vote % of winning candidate with requisite labels and limits
lines(wiki_data$Election_Year, wiki_data$Electoral_College_Percentage, 
      col="firebrick3", lty=1)
# plotting line for EC vote % of winning candidate 
lines(wiki_data$Election_Year, wiki_data$Turnout_Percentage, 
      col="navy", lty=1)
# plotting line for turnout percentage

par(mar=c(0,0,0,0)) #adjusting plotting margins to create legend
plot(0,0, type="n", axes=FALSE, xlab="", ylab="") # creating null plot
# creating a legend for the null plot, which is effectively the legend for the above
# plot
legend("center",legend=c("% of Popular Vote Received", 
                         "% of Electoral College Vote Received",
                         "Turnout %"), # providing items for legend
       col=c("forestgreen","firebrick3","navy"), # providing colors for legend
       lty = c(1,1,1)) # providing line types for legend
dev.off() # closing the pdf file

######################################

library(htmltab) # loading in a different library which can more easily handle
                 # the new table

wikiURL2 <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'
# creating an object with the specified link
wiki_table2 <- htmltab(wikiURL2, which=3) # creating an object of the desired table

wiki_table2[,7] <- sapply(strsplit(wiki_table2[,7], "/"), function(x) x[1])
# for all elements of the electoral college vote column, split the string at the /,
# and then replace the element with the first part of the string, which is the vote
# total for the candidate

electoral_vote_table <- data.frame() # creating an empty data frame
for(i in unique(wiki_table2$Year)){ # initializing a loop to iterate over each
  # election year
  year_df <- wiki_table2[wiki_table2$Year==i,] #isolating the rows for year i
  candidate_vote_totals <- NULL # creating an empty object for the vote totals for each
  # candidate
  for(j in unique(year_df$`Presidential candidate`)){ # initializing a loop to iterate 
    # over each presidential candidate in year i
    candidate_votes <- sum(as.numeric(year_df$`Electoral votes`[which(year_df$`Presidential candidate`==j)]))
    # summing the votes received by candidate j, if multiple rows exist for the candidate
    candidate_vote_totals <- as.data.frame(rbind(candidate_vote_totals, cbind(j, candidate_votes)), stringsAsFactors=FALSE)
    # binding the extant data frame for all years i, and this year i, and then converting
    # object back to a data frame
  }
  candidate_vote_totals[,2] <- as.numeric(candidate_vote_totals[,2])
  # converting vote totals column from a factor to numeric
  candidate_vote_totals <- candidate_vote_totals[order(candidate_vote_totals$candidate_votes, decreasing=TRUE),]
  # sorting the data frame by the vote totals column in descending order
  year_row <- cbind(i, candidate_vote_totals[1,1], candidate_vote_totals[1,2],
                    candidate_vote_totals[2,1], candidate_vote_totals[2,2])
  # creating a row for the election year which contains the year, the name and
  # vote total for the candidate with the highest EC total, and the name and the
  # vote total for the candidate with the second-highest vote total
  electoral_vote_table <- rbind(electoral_vote_table, year_row)
  # binding the year row to a data frame which will have the vote totals for the
  # candidates with the highest and second-highest margins of victory for each year
}

colnames(electoral_vote_table) <- c("Election_Year", "Winner", "Winner_Electoral_Votes",
                                    "Runner-up", "Runner-up_Electoral_Votes")
# renaming the columns of the data frame with the EC vote totals
rownames(electoral_vote_table) <- 1:length(electoral_vote_table$Election_Year)
# in the course of the loops, the row names became NULL; we replace the NULL row names
# with generic indicies
electoral_vote_table[,c(1,3,5)] <- apply(electoral_vote_table[,c(1,3,5)], MARGIN=2, function(x) as.numeric(x))
# converting the vote total columns from factor to numeric
electoral_vote_table[,c(2,4)] <- apply(electoral_vote_table[,c(2,4)], MARGIN=2, function(x) as.character(x))
# converting the name columns from factor to character


electoral_vote_table[which(electoral_vote_table$Election_Year==1872), 
                     "Runner-up_Electoral_Votes"] <- 42
# in the 1872 election, Wikipedia reports that the Democratic candidate won 42
# electoral votes, but these votes were distributed among different VP tickets
#depending on the historical source consulted, and the columns
# yielded from the table via scraping yield both minimum and maximum possibilities for
# each ticker; because there is no easy way to account for this idiosynchracy, we
# manually replace the cell
electoral_vote_table[which(electoral_vote_table$Election_Year==1824),2:5] <- 
  c("John Quincy Adams", 84, "Andrew Jackson", 99)
# in the 1824 election, Jackson won a plurality of electoral college votes, but ultimately
# lost the presidency in the House; this is only instance in which this occurs, so we
# manually edit the elements of this row

complete_table <- merge(wiki_data, electoral_vote_table, by="Election_Year")
# we merge the table from part 1, and the table we have created in part 2, to yield
# a table containing information from both tables by election year
