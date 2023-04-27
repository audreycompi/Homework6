# First, I will load tidyverse
library(tidyverse)

# Next, I will read my csv file
tree_data = read_csv("Data/raw_tree_data.csv")

view(tree_data)
# Question 1

# I need to break up the city attribute into the city and state separately. Before
# I write a regular expression, I want to know the different entries 
unique(tree_data$City)
# There are only a handful unique entries but they are different cities within 
# different states so I must identify the pattern they share. I believe the pattern
# is city starts with an alphabetic character, followed by some number more, and 
# ends with a comma. State starts with a space and then is only two alphabetic 
# characters. Now I have to write that into a regular expression.

# I'm first going to check that my regular expression will match all the unique entries
# and check that it will return desired results.
all(str_detect(tree_data$City, "[:alpha:]+, [:alpha:]{2}"))
str_match(tree_data$City, "[:alpha:]+, [:alpha:]{2}")
str_match(tree_data$City, "([:alpha:]+), ([:alpha:]{2})")

# My last function separated the state and city how I wanted so now I can select 
# my two new columns of data and assign them to two columns
tree_data[,c("city", "state")] = str_match(tree_data$City, "([:alpha:]+), ([:alpha:]{2})")[,2:3]
view(tree_data)

# Awesome, my two new columns were added. Now I just want to double check that all
# of my unique city and state entries were properly included and compare them to
# the original column
unique(tree_data$City)
unique(tree_data$city)
unique(tree_data$state)
# Yup, they all checked out.

# Now I have to create my bar plots. First, I have to groupby and summarize to get
# total entries for each city and state
city_records = group_by(tree_data, city) %>%
  summarize(length(TreeID))

state_records = group_by(tree_data, state) %>%
  summarize(length(TreeID))
            
# Not fully sure why but it renamed my columns weirdly so I changed that
city_records = rename(city_records, total_entries = 'length(TreeID)')
state_records = rename(state_records, total_entries = 'length(TreeID)')

# My final tables:
view(city_records)
view(state_records)

# Question 2

# I have to filter the data to just North and South Carolina. I was having trouble 
# figuring out the code to make one table so I made one for each state and then
# used bindrows to combine the data. I used unique to identify the cities from 
# the total dataset 
nc_trees = filter(tree_data, state == "NC")
sc_trees = filter(tree_data, state == "SC")
nc_sc_trees = bind_rows(nc_trees, sc_trees)
unique(nc_sc_trees$city)

# Question 3

# I will start with the same process as question 1 to create a new columns from
# data currently in a different column

# First, I have to identify the pattern. There isn't a clear pattern except it
# starts with an alphabetic character and ends with a space. I will write a regular
# expression just for the genus since I don't need the species and then test it
# whether it extracts properly.
all(str_detect(nc_sc_trees$ScientificName, "[:alpha:]+[:space:]"))
str_match(nc_sc_trees$ScientificName, "[:alpha:]+[:space:]")
str_match(nc_sc_trees$ScientificName, "([:alpha:]+)[:space:]")

# My last function pulled the genus and removed the space from the first match 
# so now I can select my new column of data and assign it to a new column
nc_sc_trees[,c("genus")] = str_match(nc_sc_trees$ScientificName, "([:alpha:]+)[:space:]")[,2]
view(nc_sc_trees)
# Now I have a table with the data a genus column and need to calculate the average
# canopy diameter per genus.

# The diameter field was giving me some trouble so I renamed it to a simpler name
genus_avg_canopy = rename(nc_sc_trees, canopy_diam = 'AvgCdia (m)')

# Now I can create a table of the average canopy lengths
genus_avg_canopy = group_by(genus_avg_canopy, genus) %>%
  summarize(canopy_diam=mean(canopy_diam))

max(genus_avg_canopy$canopy_diam)
# The largest crown diameter is 13.62316 which belongs to the genus Quercus

# Extra Credit
# Tree Age
# I'm going to make a new dataset with genus, age, and crown diameter
extra1 = rename(nc_sc_trees, canopy_diam = 'AvgCdia (m)')
#remaking my genus_avg_canopy dataset before I changed it with groupby

canopy_size = group_by(extra1, genus) %>%
  summarize(canopy_diam=mean(canopy_diam), Age=mean(Age))

# For the second question, I made a new dataset of the genus and its average growth
# per year
recommend = group_by(canopy_size, genus) %>%
  summarize(canopy_diam/Age)
max(recommend$`canopy_diam/Age`)
# Maximum annual growth is 0.5806922 m which is the genus Ulmus.

# Species
# Similar to before, I have to make a regular expression for species. Except there
# is more variation in species so it must accommodate all cases.
all(str_detect(extra1$ScientificName, "^[:alpha:]+ x?[:space:]?[:alpha:]+[:space:]?"))
str_match(extra1$ScientificName, "^[:alpha:]+ x?[:space:]?[:alpha:]+[:space:]?")
str_match(extra1$ScientificName, "^[:alpha:]+ x?[:space:]?([:alpha:]+)[:space:]?")

# My last function pulled the species under all conditions so now I can assign it
# to a new column
extra1[,c("species")] = str_match(extra1$ScientificName, "^[:alpha:]+ x?[:space:]?([:alpha:]+)[:space:]?")[,2]
view(extra1)

# Now I'm aggregating all of the unique species for each genus
species_per_genus = group_by(extra1, genus, species) %>%
  summarize()
# Then I just wanted the count of unique species per genus. I probably could have
# combined these steps but I just did them separately because that's how my brain
# best rationalized it
num_species = group_by(species_per_genus, genus) %>%
  summarize(length(species))
