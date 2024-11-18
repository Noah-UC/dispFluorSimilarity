test_that("Panel list needs at least two character entries", {

  #Case when nothing is passed

  expect_error(dispFluorSimilarity(panel=NULL),
    "Panel should have at least two Fluors.")

  #Case when only one Fluor is passed

  expect_error(dispFluorSimilarity(panel=c("LIVE DEAD Blue")),
               "Panel should have at least two Fluors.")

  #Case when Fluors not in database are passed

  expect_error(dispFluorSimilarity(panel=c("Live Dead Blue", "PE")), #incorrect spelling
               "Fluor not in database. Check spellings on spectrum.cytekbio.com.")

  #Case when at panel has the right number of character entries

  expect_silent(dispFluorSimilarity(panel = c("BUV395", "PE")))




})
