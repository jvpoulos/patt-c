# analyze survey data for free (http://asdfree.com) with the r language
# national health interview survey
# 2011 personsx plus samadult with multiple imputation

# # # # # # # # # # # # # # # # #
# # block of code to run this # #
# # # # # # # # # # # # # # # # #
# library(downloader)
# setwd( "C:/My Directory/NHIS/" )
# source_url( "https://raw.github.com/ajdamico/usgsd/master/National%20Health%20Interview%20Survey/2011%20personsx%20plus%20samadult%20with%20multiple%20imputation%20-%20analyze.R" , prompt = FALSE , echo = TRUE )
# # # # # # # # # # # # # # #
# # end of auto-run block # #
# # # # # # # # # # # # # # #

# if you have never used the r language before,
# watch this two minute video i made outlining
# how to run this script from start to finish
# http://www.screenr.com/Zpd8

# anthony joseph damico
# ajdamico@gmail.com

# if you use this script for a project, please send me a note
# it's always nice to hear about how people are using this stuff

# for further reading on cross-package comparisons, see:
# http://journal.r-project.org/archive/2009-2/RJournal_2009-2_Damico.pdf



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#####################################################################################################################################
# prior to running, the nhis 2011 personsx, samadult, incmimp# files must be loaded as an R data file (.rda) on the local machine.  #
# running the "1963-2011 - download all microdata.R" script will create this R data file (note: only 2011 files need to be loaded)  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://github.com/ajdamico/usgsd/blob/master/National%20Health%20Interview%20Survey/1963-2011%20-%20download%20all%20microdata.R #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# that script will create a files "/2011/_filename_.rda" in C:/My Directory/NHIS (or wherever the working directory was chosen)     #
#####################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



#########################################################################################################
# Analyze the 2011 National Health Interview Survey household, personsx, samadult, and imputed income files with R #
#########################################################################################################


# set your working directory.
# the NHIS 2011 personsx, samadult, and incmimp# data files should have been
# stored in a year-specific directory within this folder.
# so if the file "personsx.rda" exists in the directory "C:/My Directory/NHIS/2011/" 
# then the working directory should be set to "C:/My Directory/NHIS/"
# use forward slashes instead of back slashes

# uncomment this line by removing the `#` at the front..
setwd("~/Dropbox/github/stat215b-final-project/data/NHIS")
# ..in order to set your current working directory



# remove the # in order to run this install.packages line only once
# install.packages( c( "survey" , "mitools" ) )

#library(survey) 	# load survey package (analyzes complex design surveys)
#library(mitools)	# allows analysis of multiply-imputed survey data

# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
#options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN


# choose what year of data to analyze
# note: this can be changed to any year that has already been downloaded locally
# by the "1963-2011 - download all microdata.R" program above
# year <- 2008 #JP: set year outside script


# construct the filepath (within the current working directory) to the three rda files
path.to.personsx.file <- paste( getwd() , year , "personsx.rda" , sep = "/" )
path.to.samadult.file <- paste( getwd() , year , "samadult.rda" , sep = "/" )
path.to.incmimp.file <- paste( getwd() , year , "incmimp.rda" , sep = "/" )
path.to.househld.file <- paste( getwd() , year , "househld.rda" , sep = "/" )

# print those filepaths to the screen
print( path.to.personsx.file )
print( path.to.samadult.file )
print( path.to.incmimp.file )
print( path.to.househld.file )

# now the "NHIS.11.personsx.df" data frame can be loaded directly
# from your local hard drive.  this is much faster.
load( path.to.personsx.file )		# this loads a data frame called NHIS.11.personsx.df
load( path.to.samadult.file )		# this loads a data frame called NHIS.11.samadult.df
load( path.to.househld.file )  	


# the five imputed income files will be loaded later

# all objects currently in memory can be viewed with the list function
ls()


# construct a string containing the data frame name of the personsx data table
# stored within the R data file (.rda)
# note: for 2011, this data frame will be named "NHIS.11.personsx.df"
# but constructing it dynamically will allow analyses of other years
# by simply changing the 'year' variable above
df.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "personsx" , "df" , sep = "." )

# repeat this for household and sample adult data frame, 
# but not for the five imputed income data frames (which will be dealt with later)
samadult.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "samadult" , "df" , sep = "." )
househld.name <- paste( "NHIS" , substr( year , 3 , 4 ) , "househld" , "df" , sep = "." )

# copy the personsx data frame to the variable x for easier analyses
# (because NHIS.11.personsx.df is unwieldy to keep typing)
x <- get( df.name )

# copy the samadult data frame to the variable sa for easier typing
# (because NHIS.11.samadult.df is unwieldy to keep typing)
sa <- get( samadult.name )

# HH df
hh <- get(househld.name)

# remove the original copy of the two data frames from memory
rm( list = c( df.name , samadult.name, househld.name ) )

# clear up RAM
gc()


#####################################
# merge personsx and samadult files #
#####################################

# note: the logical steps taken here could also be applied to 
# merging the personsx and samchild files

# the personsx and samadult files are both at the individual or person-level
# (as opposed to family-level or household-level)
# so merging them together will require three variables:
# hhx (household unique identifier)
# fmx (family unique identifier)
# fpx (person unique identifier)

# store the names of these three columns in a character vector
merge.vars <- c( "hhx" , "fmx" , "fpx" )

# these two files have multiple overlapping (redundant) columns,
# so determine which columns are included in both data frames
# at the same time, enclose this statement in () thereby printing the vector to the screen
( columns.in.both.dfs <- intersect( names( sa ) , names( x ) ) )


# since the merge.vars will be used to merge the two data frames,
# those three variables should be excluded from the list of redundant columns
# keep all column names that don't match the merge variables
# at the same time, enclose this statement in () thereby printing the vector to the screen
( redundant.columns <- columns.in.both.dfs[ !( columns.in.both.dfs %in% merge.vars ) ] )

# notice that the three merge.vars have disappeared


# most analyses start with the personsx file,
# so shave the redundant columns off of the samadult file
# keep all columns in the samadult file that are not in the redundant.columns vector
sa <- sa[ , !( names( sa ) %in% redundant.columns ) ]


# at this point, the only overlap between the personsx and samadult files
# should be the three merge.vars
# throw an error if that's not true
stopifnot( merge.vars == intersect( names( sa ) , names( x ) ) )


# remember that the samadult file contains a subset of the individuals in personsx
# therefore, an inner join of the two should have the same number of records as samadult

# perform the actual merge
x.sa <- merge( x , sa )
# note that the merge() function merges using all intersecting columns -

# uncomment this line to see intersecting columns
intersect( names( sa ) , names( x ) )

# - by default, the 'by' parameter does not need to be specified
# for more detail about the merge function, type ?merge in the console

# throw an error if the number of records in the merged file
# does not match the number of records in the samadult file
stopifnot( nrow( x.sa ) == nrow( sa ) )


# now the x.sa data frame contains all of the rows in the samadult file and 
# all columns from both the samadult and personsx files
# therefore, there's no more need for the samadult file on its own
# so delete the samadult file
rm( sa ) 

#####################################
# merge merged and househld files #
#####################################

# store the names of these three columns in a character vector
merge.vars <- c( "hhx")

# these two files have multiple overlapping (redundant) columns,
# so determine which columns are included in both data frames
# at the same time, enclose this statement in () thereby printing the vector to the screen
( columns.in.both.dfs <- intersect( names( x.sa ) , names( hh ) ) )


# since the merge.vars will be used to merge the two data frames,
# those three variables should be excluded from the list of redundant columns
# keep all column names that don't match the merge variables
# at the same time, enclose this statement in () thereby printing the vector to the screen
( redundant.columns <- columns.in.both.dfs[ !( columns.in.both.dfs %in% merge.vars ) ] )

# notice that the three merge.vars have disappeared


# most analyses start with the personsx file,
# so shave the redundant columns off of the samadult file
# keep all columns in the samadult file that are not in the redundant.columns vector
hh <- hh[ , !( names( hh ) %in% redundant.columns ) ]


# at this point, the only overlap between the personsx and samadult files
# should be the three merge.vars
# throw an error if that's not true
stopifnot( merge.vars == intersect( names( x.sa ) , names( hh ) ) )


# remember that the samadult file contains a subset of the individuals in personsx
# therefore, an inner join of the two should have the same number of records as samadult

# perform the actual merge
x.sa <- merge( x.sa , hh )
# note that the merge() function merges using all intersecting columns -

# - by default, the 'by' parameter does not need to be specified
# for more detail about the merge function, type ?merge in the console

# now the x.sa data frame contains all of the rows in the samadult file and 
# all columns from both the samadult and personsx files
# therefore, there's no more need for the samadult file on its own
# so delete the samadult file
rm( hh ) 

# # # # # # # # # # # #

# now load the imputed income data frames
load( path.to.incmimp.file )		# this loads five data frames called ii1, ii2, ii3, ii4, and ii5


# loop through all five imputed income files
for ( i in 1:5 ){
  
  # create a temporary current.i data frame
  # containing the current iteration's (1 through 5) imputed income file
  current.i <- get( paste0( "ii" , i ) )
  
  # the 2011 imputed income merge fields are currently stored as character variables
  # and should immediately be converted over to numeric types
  merge.vars <- intersect( names( x.sa ) , names( current.i ) )
  
  # loop through all variables used in the merge
  # overwrite each column with itself, only converted to a numeric field
  for ( j in merge.vars ) x.sa[ , j ] <- as.numeric( x.sa[ , j ] )
  for ( j in merge.vars ) current.i[ , j ] <- as.numeric( current.i[ , j ] ) # JP
  
  # a handy trick to view the class of all columns within a data frame at once:
  # sapply( x.sa , class )
  
  # merge the merged file with each of the five imputed income files
  y <- merge( 
      x.sa , # the 2011 samadult-personsx merged data frame
      current.i # ii1 - ii5, depending on the current iteration of this loop
    )
  
  # and confirm the new data frame (merged + the current iteration of the multiply-imputed data)
  # contains the same number of records as the original merged file
  stopifnot( nrow( x.sa ) == nrow( y ) )
  
  # save the data frames as objects x1 - x5, depending on the iteration in the loop
  assign( paste0( 'x' , i ) , y )
  
  # delete the y and ii# data frames
  y <- NULL
  assign( paste0( "ii" , i ) , NULL )
  
  # garbage collection - free up RAM from recently-deleted data tables
  gc()
}

# Take means of family income
if(year %in% c(2009:2013)){
faminci2 <- cbind(x1$faminci2,x2$faminci2,x3$faminci2,x4$faminci2,x5$faminci2)
x.sa$faminci2 <- rowMeans(faminci2)
}

if(year %in% c(2008)){
  x.sa$incgrpi2 <-round(rowMeans(cbind(x1$incgrpi2,x2$incgrpi2,x3$incgrpi2,x4$incgrpi2,x5$incgrpi2)),0) 
}

# Take means of poverty ratio
if(year %in% c(2010:2013)){
  povrati3 <- cbind(x1$povrati3,x2$povrati3,x3$povrati3,x4$povrati3,x5$povrati3)
  x.sa$povrati3 <- rowMeans(povrati3)
} 
if(year %in% c(2009)){
  povrati2 <- cbind(x1$povrati2,x2$povrati2,x3$povrati2,x4$povrati2,x5$povrati2)
  x.sa$povrati2 <- rowMeans(povrati2)
}

if(year %in% c(2008)){
x.sa$rat_cati <-round(rowMeans(cbind(x1$rat_cati,x2$rat_cati,x3$rat_cati,x4$rat_cati,x5$rat_cati)),0) 
}
