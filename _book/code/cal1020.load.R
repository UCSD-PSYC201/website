# rjags examples.
setwd('~/TEACHING/2014.201ab/code/')
cal1020 = read.csv('http://vulstats.ucsd.edu/data/cal10202.txt', sep='\t', header = T)

# What's in these data?
head(cal1020)

# we have some unnecessary columns (which I happen to know have somewhat error-prone data).  
# Let's get rid of all the columns after Zip
# First check that we picked out the right columns
head(cal1020[,c(1:12)])
# now cut them out
cal1020 = cal1020[,c(1:12)]

# How are these data formatted?
str(cal1020)

# a few things we should do to get the most use out of these data.
# 1) we have two times (gun and chip).  We care about the actual time, rather than the gun time (because of start delays)
# 2) time is a string (factor), we want to convert it to a number, so we need to parse out the hh:mm:ss format and convert into seconds
# 3) age is a factor, not a number, because it has some undefined levels (marked with "?")
# 4) we don't care about class and overall position
# 5) we do care about "corral", but right now it's encoded in the bib number.
# 6) we want to separate out the wheelchair racers from the runners
# 7) we want to get the net pace (time/mile) and speed (mile/hour)

# 1,2) first, let's get time in seconds from the time.chip factor.
# convert time.chip factor to character strings
cal1020$time.chip = as.character(cal1020$time.chip)
# split "hh:mm:ss" strings into separate "hh" "mm" "ss" elements
z <- strsplit(cal1020$time.chip, ':')
head(z)
# this gives us a list of character vectors
# for each one we want to do the following:
z[[105]]  # extract a single list element
as.numeric(z[[105]])   # convert character vector into numeric vector
as.numeric(z[[105]])*c(60*60, 60, 1)  # multiply by number of seconds in each unit (h, m, s)
sum(as.numeric(z[[105]])*c(60*60, 60, 1)) # sum up elements to get total number of seconds.
# we are going to need to do this to every list element.  the lapply function is handy for this sort of thing.
lapply(z, function(x)(sum(as.numeric(x)*c(60*60, 60, 1))))
# this takes each list element in z, and passes it to the function we defined. 
# in the function, the element is called x, and then we do the procedure described above to get seconds
# what we get out is a list of numbers.  we want to convert them to a vector and store them in our data frame
cal1020$time.sec = unlist(lapply(z, function(x)(sum(as.numeric(x)*c(60*60, 60, 1)))))
str(cal1020)
# now get rid of the time.gun and time.chip string columns.
str(cal1020[, c(3:13)]) 
cal1020 = cal1020[, c(3:13)]
str(cal1020)

# 3) age is a factor, not a number, because it has some undefined levels (marked with "?")
cal1020$Age = as.numeric(as.character(cal1020$Age))

# 4) we don't care about class and overall position
str(cal1020)
cal1020 = cal1020[, c(1:6, 9:11)]
str(cal1020)

# 5) we do care about "corral", but right now it's encoded in the bib number.
cal1020$corral = as.factor(floor(cal1020$bib/1000))
str(cal1020)

# 6) we want to separate out the wheelchair racers from the runners
cal1020$wheelchair = FALSE
levels(cal1020$Division) # check to see if we can identify wheelchairs from division name...
grep("Wheelchair", cal1020$Division)  # string matching (grep can do more complicated things too) -- shows which elements matched
cal1020$wheelchair[grep("Wheelchair", cal1020$Division)] = TRUE  # set those elements where Division matches Wheelchair to true.

# 7) we want to get the net pace (time/mile) and speed (mile/hour) [ race was 10 miles ]
cal1020$pace.sec = cal1020$time.sec/10
cal1020$speed.mph = 10/(cal1020$time.sec/3600)

# now let's see what we got
str(cal1020)
levels(cal1020$State)  # weird redundant coding... what is AP? BCN? BC? 
states <- levels(cal1020$State)
# let's fix redundancies first
states[states=="ARIZONA"] = "AZ"
states[states=="CALIFORNIA"] = "CA"
states[states=="FLORIDA"] = "FL"
states[states=="NEBRASKA"] = "NE"
states[states=="NEVADA"] = "NV"
states[states=="NEW JERSEY"] = "NJ"
states[states=="VIRGINIA"] = "VA"
states[states=="WASHINGTON"] = "WA"
levels(cal1020$State) <- states
states <- levels(cal1020$State)
states  # ok.  BCN is likely Baja -- let's call it MEXICO.  BC is likely british columbia -- call it CANADA.... forgot to fix UTAH
states[states=="B.C.N"] = "MEXICO"
states[states=="BC"] = "CANADA"
states[states=="UTAH"] = "UT"
states[states=="EUR"] = "EUROPE"
levels(cal1020$State) <- states
states <- levels(cal1020$State)
states 

str(cal1020)

# this looks good to me.  let's save it in a handy R format, as well as a csv file
write.csv(cal1020, file="cal1020.cleaned.csv")

save(list = "cal1020", file="cal1020.cleaned.Rdata")
