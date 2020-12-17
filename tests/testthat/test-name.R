test_that("Throws error when date doesn't match expected dates", {
  #Expect an error
  expect_error(geocheck("Cornwall", "Region", "31-Dec-13"), "31-Dec-13 is not a recognised date for area type: Region") 
  expect_error(geocheck("Cornwall", "Region", "31-Dec-18"), NA) #Expect no error
})


test_that("Throws error when date is in incorrect format", {
  #Expect an error
  expect_error(geocheck("Cornwall", "Region", "31-12-18"), "Date is not in the expected format DD-MMM-YY") 
  expect_error(geocheck("Cornwall", "Region", "31-Dec-18"), NA) #Expect no error
})