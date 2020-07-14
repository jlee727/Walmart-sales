
### preliminary data cleaning

## merge-data

data = bind_rows(train, test)

data = data %>% 
  left_join(key, by = c("store_nbr")) %>% 
  left_join(weather, by = c("station_nbr", "date"))


# subset and remove NAs
data = data %>% 
  as_tibble %>% 
  select(- depart) %>% 
  mutate_if(is.character, as.numeric)

# change item_nbr and store_nbr into categorical variables
data$item_nbr = factor(data$item_nbr)
data$store_nbr = factor(data$store_nbr)
data$station_nbr = factor(data$station_nbr)

# split data into test-train 
train_index = 1:nrow(train)
test_index = (nrow(train) + 1):nrow(data)

mytrain = data[train_index, ] %>% select(-codesum)
mytest = data[test_index, ] %>% select(-codesum)

# replace NA with median value
mytrain[6:21] = mytrain[6:12]%>% 
  map(~ replace_na(median(.x, na.rm=TRUE))) 


### feature engineering

# codesum-partition-function
# function for extracting weather phenomena info from codesum
part = function(data, x) {
  str_detect(data, x)
}

# create dummy variables to replace codesum
var_codesum = data$codesum

data2 = data %>% 
  mutate(
    TS = part(var_codesum, 'TS'),
    GR = part(var_codesum, 'GR'),
    RA = part(var_codesum, 'RA'),
    DZ = part(var_codesum, 'DZ'),
    SN = part(var_codesum, 'SN'),
    SG = part(var_codesum, 'SG'),
    GS = part(var_codesum, 'GS'),
    PL = part(var_codesum, 'PL'),
    FG_plus = part(var_codesum, "FG\\+"),
    FG = part(var_codesum, 'FG'),
    BR = part(var_codesum, 'BR'),
    UP = part(var_codesum, 'UP'),
    HZ = part(var_codesum, 'HZ'),
    FU = part(var_codesum, 'FU'),
    DU = part(var_codesum, 'DU'),
    SS = part(var_codesum, 'SS'),
    SQ = part(var_codesum, 'SQ'),
    FZ = part(var_codesum, 'FZ'),
    MI = part(var_codesum, 'MI'),
    PR = part(var_codesum, 'PR'),
    BC = part(var_codesum, 'BC'),
    BL = part(var_codesum, 'BL'),
    VC = part(var_codesum, 'VC')
  ) %>% 
  select(- codesum) 

# replace NA with median value
data2[6:21] = data2[6:21] %>% 
  map(~ replace_na(median(.x, na.rm=TRUE))) 

# replace NA in (codesome) dummy variables with FALSE
data2[22:44] = data2[22:44] %>% 
  map(~ replace_na(FALSE)) 

#second-test-train-split
# split data into test-train 
train_index = 1:nrow(train)
test_index = (nrow(train) + 1):nrow(data)

train2 = data2[train_index, ] 
test2 = data2[test_index, ]

