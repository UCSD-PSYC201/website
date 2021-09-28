# This data was collected with an on-line version of the Nature Relatedness Scale (NR-6), see https://www.frontiersin.org/articles/10.3389/fpsyg.2013.00813/full

# retrieved from openpsychometrics

NR6 <- read_tsv('http://vulstats.ucsd.edu/data/OP-NR6/data-final.csv')

NR6 <- NR6 %>% 
  mutate(nature_relatedness = Q1A + Q2A + Q3A + Q4A + Q5A + Q6A) %>% 
  select(-(Q1A:Q6E))


# The following items were included in the survey:
# Q1    My ideal vacation spot would be a remote, wilderness area.
# Q2    I always think about how my actions affect the environment.
# Q3    My connection to nature and the environment is a part of my spirituality.
# Q4    I take notice of wildlife wherever I am.
# Q5    My relationship to nature is an important part of who I am.
# Q6    I feel very connected to all living things and the earth.

# Each item was presented one at a time in a random order for each 
# new participant along with a 5 point rating scale from 
# 1=disagree to 5=agree. 
# This response is stored in variable A (e.g. Q1A). 
# Also recorded was the time taken in milliseconds to answer that question (E) 
# and that question's position in the survey (I).



# These other durations were also recorded (measured on the server side):
#   
#   introelapse		The time spent on the introduction/landing page (in seconds)
# testelapse		The time spent on all the DASS questions (should be equivalent to the time elapsed on all the indiviudal questions combined)
# surveyelapse	The time spent answering the rest of the demographic and survey questions
# 
# On the next page was a generic demographics survey with many different questions.
# 
# The Ten Item Personality Inventory was administered (see Gosling, S. D., Rentfrow, P. J., & Swann, W. B., Jr. (2003). A Very Brief Measure of the Big Five Personality Domains. Journal of Research in Personality, 37, 504-528.):
#   
#   TIPI1       Extraverted, enthusiastic.
# TIPI2	      Critical, quarrelsome.
# TIPI3   	Dependable, self-disciplined.
# TIPI4	      Anxious, easily upset.
# TIPI5	      Open to new experiences, complex.
# TIPI6	      Reserved, quiet.
# TIPI7	      Sympathetic, warm.
# TIPI8	      Disorganized, careless.
# TIPI9	      Calm, emotionally stable.
# TIPI10	Conventional, uncreative.
# 
# The TIPI items were rated "I see myself as: _____" such that
# 
# 1 = Disagree strongly
# 2 = Disagree moderately
# 3 = Disagree a little
# 4 = Neither agree nor disagree
# 5 = Agree a little
# 6 = Agree moderately
# 7 = Agree strongly

NR6 <- NR6 %>% 
  mutate_at(vars(TIPI1:TIPI10), function(x){x-4}) %>% 
  mutate(extroversion = TIPI1 - TIPI6,
         agreeableness = TIPI7-TIPI2,
         consientiousness = TIPI3 - TIPI8,
         emotional_stability = TIPI9 - TIPI4,
         openness = TIPI5-TIPI10) %>% 
  select(-(TIPI1:TIPI10))

# The following items were presented as a check-list and subjects were instructed "In the grid below, check all the words whose definitions you are sure you know":
#   
#   VCL1	boat
# VCL2	incoherent
# VCL3	pallid
# VCL4	robot
# VCL5	audible
# VCL6	cuivocal
# VCL7	paucity
# VCL8	epistemology
# VCL9	florted
# VCL10	decide
# VCL11	pastiche
# VCL12	verdid
# VCL13	abysmal
# VCL14	lucid
# VCL15	betray
# VCL16	funny
# 
# A value of 1 is checked, 0 means unchecked. The words at VCL6, VCL9, and VCL12 are not real words and can be used as a validity check.

NR6 <- NR6 %>% 
  rowwise() %>% 
  mutate(lure_words = VCL6+VCL9+VCL12) %>% 
  select(-VCL6, -VCL9, -VCL12) %>% 
  mutate(words_known = sum(c_across(VCL1:VCL16))) %>% 
  select(-(VCL1:VCL16)) %>% 
  ungroup()

# 
# A bunch more questions were then asked:
#   
#   
# education			"How much education have you completed?", 1=Less than high school, 2=High school, 3=University degree, 4=Graduate degree
# urban				"What type of area did you live when you were a child?", 1=Rural (country side), 2=Suburban, 3=Urban (town, city)
# gender				"What is your gender?", 1=Male, 2=Female, 3=Other
# engnat				"Is English your native language?", 1=Yes, 2=No
# age					"How many years old are you?"
# orientation			"What is your sexual orientation?", 1=Heterosexual, 2=Bisexual, 3=Homosexual, 4=Asexual, 5=Other
# race				"What is your race?", 11=Asian, 12=Arab, 13=Black, 14=Indigenous Australian, 15=Native American, 16=White, 17=Other, 10=""
# voted				"Have you voted in a national election in the past year?", 1=Yes, 2=No
# married				"What is your marital status?", 1=Never married, 2=Currently married, 3=Previously married
# familysize			"Including you, how many children did your mother have?"		
# major				"If you attended a university, what was your major (e.g. "psychology", "English", "civil engineering")?"

NR6 <- NR6 %>% 
  mutate(hand = recode(as.character(hand), '1'='right', '2'='left', '3'='both')) %>% 
  # hand				"What hand do you use to write with?", 1=Right, 2=Left, 3=Both
  # religion			"What is your religion?", 1=Agnostic, 2=Atheist, 3=Buddhist, 4=Christian (Catholic), 5=Christian (Mormon), 6=Christian (Protestant), 7=Christian (Other), 8=Hindu, 9=Jewish, 10=Muslim, 11=Sikh, 12=Other
  mutate(religion = recode(as.character(religion), 
                           '1'='Agnostic', 
                           '2'='Atheist', 
                           '2'='Buddhist', 
                           '4'='Christian-Catholic', 
                           '5'='Christian-Mormon', 
                           '6'='Christian-Protestant', 
                           '7'='Christian-Other', 
                           '8'='Hindu', 
                           '9'='Jewish', 
                           '10'='Muslim', 
                           '11'='Sikh', 
                           '12'='Other')) %>% 
  # gender				"What is your gender?", 1=Male, 2=Female, 3=Other
  mutate(race = recode(as.character(race),
                       "11"="Asian", 
                       "12"="Arab", 
                       "13"="Black", 
                       "14"="Indigenous Australian", 
                       "15"="Native American", 
                       "16"="White", 
                       "17"="Other", 
                       "10"="")) %>% 
  mutate(gender = recode(as.character(gender),
                         "1"="Male", "2"="Female", "3"="Other"))
# education			"How much education have you completed?", 1=Less than high school, 2=High school, 3=University degree, 4=Graduate degree

dat <- NR6 %>% 
  select(testelapse, surveyelapse, education, gender, race, age, screenw, screenh, hand, familysize, major, nature_relatedness:words_known)

dat$age = as.character(dat$age)
i = 66
dat[i,'age'] = 'fifty eight'

i = 187
dat[i,'familysize'] = -4


i = 220
dat[i,'age'] = '222'

dat %>% mutate_if(is.numeric, as.character)

i = 880
dat[i,'familysize'] = "two"
  
  

save(dat, file = 'midterm.Rdata')

# 
# 
# This value was determined with technical information:
#   country     the country the respondent connected to the survey from