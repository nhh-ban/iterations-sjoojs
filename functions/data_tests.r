# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

# FIRST SECTION - Checking column names
test_stations_metadata_colnames <-
  function(df) { # Creating a function which takes the df as an argument
    
    # Inserting the expected column names of the df
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    # If all the column names are equal to the expected column names
    # then print PASS, if not the function prints FAIL
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }
test_stations_metadata_colnames(stations_metadata_df) # Pass

# NEXT SECTION -
test_stations_metadata_nrows <-
  function(df) { # Creating a function which takes df as argument
    
    min_expected_rows <- 5000 #minimum expected rows of 5000
    max_expected_rows <- 10000 #maximum expected rows of 10000
    
    #logical test: if the number of rows are between the minimum expected and 
    # maximum expected rows, the dataset passes. If not, it fail. 
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }
test_stations_metadata_nrows(stations_metadata_df) # Testing; pass

# THIRD SECTION. Testing to see if the column types are chr, chr, dbl, dbl and dbl
# We do this by the function below
# if all the column types are equal to the expected, it passes. if not, fail
test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }

test_stations_metadata_coltypes(stations_metadata_df) #test, pass
  
# FOURTH SECTION; testing to see if number of missing values is reasonable or not
test_stations_metadata_nmissing <-
  function(df) { # the function with argument df
    max_miss_vals <- 200 #max missing values acceptable
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  } #if the dataframe has a sum of missing values of less than 200, it passes

test_stations_metadata_nmissing(stations_metadata_df)

# FIFTH SECTION - timezone function
test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") { # if df is UTC
      print("PASS: latestData has UTC-time zone") #pass 
    } else { #if not
      print("FAIL: latestData does not have expected UTC-time zone") #fail
    }
  }

test_stations_metadata_latestdata_timezone(stations_metadata_df) # fails

#combing all the functions in a function
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  } # nested functions. So when you type the main function, all these functions
# start

test_stations_metadata(stations_metadata_df) # 4 pass, 1 fail



