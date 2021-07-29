dat1 <- tibble::tibble(
  metadataType = c(),
  assay = c(),
  data = list()
)

test_that("join_full_study_metadata returns NA if no expected metadataType", {
  dat1 <- tibble::tibble(
    metadataType = c(NA, "foo", "bar"),
    assay = c(NA, NA, NA),
    data = list(NA, NA, NA)
  )

  expect_true(is.na(join_full_study_metadata(dat1)))
})

test_that("join_full_study_metadata returns NA if no data", {
  dat1 <- tibble::tibble(
    metadataType = c("assay", "biospecimen", "individual"),
    assay = c("rnaSeq", NA, NA),
    data = list(NA, NA, NA)
  )

  expect_true(is.na(join_full_study_metadata(dat1)))
})

test_that("join_full_study_metadata returns expected data", {
  ## A "nice", simple dataset
  dat1 <- tibble::tibble(
    metadataType = c("individual", "biospecimen", "assay"),
    assay = c(NA, NA, "rnaSeq"),
    data = list(
      tibble::tibble(
        individualID = c("i1", "i2", "i3")
      ),
      tibble::tibble(
        individualID = c("i1", "i2", "i3"),
        specimenID = c("s1", "s2", "s3"),
        assay = c("rnaSeq", "rnaSeq", "rnaSeq")
      ),
      tibble::tibble(
        specimenID = c("s1", "s2", "s3"),
        assay = c("rnaSeq", "rnaSeq", "rnaSeq")
      )
    )
  )

  ## Full dataset should end up with the biospecimen data
  res1 <- join_full_study_metadata(dat1)
  expect_equal(res1, dat1$data[[2]])

  ## Only individual should have individualID and NA for other cols
  res2 <- join_full_study_metadata(dat1[1, ])
  expected2 <- tibble::tibble(
    individualID = c("i1", "i2", "i3"),
    specimenID = as.character(NA),
    assay = as.character(NA)
  )
  expect_equal(res2, expected2)

  ## Only biospecimen and assay should end up with biospecimen data
  res3 <- join_full_study_metadata(dat1[c(2, 3), ])
  expect_equal(res3, dat1$data[[2]])

  ## Assay and individual should be odd since there's no individualID in assay
  res4 <- join_full_study_metadata(dat1[c(1, 3), ])
  expected4 <- tibble::tibble(
    individualID = c(NA, NA, NA, "i1", "i2", "i3"),
    specimenID = c("s1", "s2", "s3", NA, NA, NA),
    assay = c("rnaSeq", "rnaSeq", "rnaSeq", NA, NA, NA)
  )
  expect_equal(res4, expected4)

  ## Only assay or biospecimen will error because it's weird; tested below

  ## A less "nice", multiple assay dataset
  dat2 <- tibble::tibble(
    metadataType = c("individual", "biospecimen", "assay", "assay", "assay"),
    assay = c(NA, NA, "r", "m", "c"),
    data = list(
      tibble::tibble(
        individualID = c("i1", "i2", "i3")
      ),
      tibble::tibble(
        individualID = c("i1", "i2", "i3", "i1", "i2", "i3"),
        specimenID = c("s1", "s2", "s3", "s4", "s5", "s6"),
        assay = c("r", "r", "r", NA, NA, NA)
      ),
      tibble::tibble(
        specimenID = c("s1", "s2", "s3"),
        assay = c("r", "r", "r")
      ),
      tibble::tibble(
        individualID = c("i1", "i2", "i3")
      ),
      tibble::tibble(
        specimenID = c("s4", "s5", "s6")
      )
    )
  )

  ## Full dataset
  res5 <- join_full_study_metadata(dat2)
  expected5 <- tibble::tibble(
    individualID = c("i1", "i2", "i3", "i1", "i2", "i3", "i1", "i2", "i3"),
    specimenID = c("s1", "s2", "s3", NA, NA, NA, "s4", "s5", "s6"),
    assay = c("r", "r", "r", "m", "m", "m", "c", "c", "c")
  )
  expect_equal(res5, expected5)

  ## Assay and individual is going to look odd because no biospecimen to match
  res6 <- join_full_study_metadata(dat2[c(1, 3), ])
  expected6 <- tibble::tibble(
    individualID = c(NA, NA, NA, "i1", "i2", "i3"),
    specimenID = c("s1", "s2", "s3", NA, NA, NA),
    assay = c("r", "r", "r", NA, NA, NA)
  )
  expect_equal(res6, expected6)

  ## Assay and biospecimen
  res7 <- join_full_study_metadata(dat2[c(2, 3, 4, 5), ])
  expected7 <- tibble::tibble(
    individualID = c("i1", "i2", "i3", "i1", "i2", "i3", "i1", "i2", "i3"),
    specimenID = c("s1", "s2", "s3", NA, NA, NA, "s4", "s5", "s6"),
    assay = c("r", "r", "r", "m", "m", "m", "c", "c", "c")
  )
  expect_equal(res7, expected7)

  ## Gross set with missing data and duplicate specimenIDs
  dat3 <- tibble::tibble(
    metadataType = c("individual", "biospecimen", "assay", "assay"),
    assay = c(NA, NA, "r", "m"),
    data = list(
      tibble::tibble(
        individualID = c("i1", "i2", "i3", "i4", "i5")
      ),
      tibble::tibble(
        individualID = c("i1", "i2", "i4"),
        specimenID = c("s1", "s2", "s4")
      ),
      tibble::tibble(
        specimenID = c("s1", "s2", "s3")
      ),
      tibble::tibble(
        individualID = c("i4", "i5", "i6")
      )
    )
  )

  ## Full dataset
  res8 <- join_full_study_metadata(dat3)
  expected8 <- tibble::tibble(
    individualID = c("i1", "i2", NA, "i4", "i5", "i6", "i4", "i3"),
    specimenID = c("s1", "s2", "s3", NA, NA, NA, "s4", NA),
    assay = c("r", "r", "r", "m", "m", "m", NA, NA)
  )
  expect_equal(res8, expected8)

  ## Assay and individual, again, looks gross
  res9 <- join_full_study_metadata(dat3[c(1, 3, 4), ])
  expected9 <- tibble::tibble(
    individualID = c(NA, NA, NA, "i4", "i5", "i6", "i1", "i2", "i3"),
    specimenID = c("s1", "s2", "s3", NA, NA, NA, NA, NA, NA),
    assay = c("r", "r", "r", "m", "m", "m", NA, NA, NA)
  )
  expect_equal(res9, expected9)

  ## Assay and biospecimen
  res10 <- join_full_study_metadata(dat3[c(2, 3, 4), ])
  expected10 <- tibble::tibble(
    individualID = c("i1", "i2", NA, "i4", "i5", "i6", "i4"),
    specimenID = c("s1", "s2", "s3", NA, NA, NA, "s4"),
    assay = c("r", "r", "r", "m", "m", "m", NA)
  )
  expect_equal(res10, expected10)
})

test_that("join_full_study_metadata handles assay only studies", {
  dat <- tibble::tibble(
    metadataType = c("assay", "assay", "assay"),
    assay = c("a1", "a2", "a3"),
    data = list(
      tibble::tibble(
        specimenID = c("s1", "s2"),
        assay = "a1"
      ),
      tibble::tibble(
        individualID = c("i1", "i2", "i3"),
        assay = "a2"
      ),
      tibble::tibble(
        specimenID = c("s3", "s4", "s5"),
        individualID = c("i1", "i2", "i3"),
        assay = "a3"
      )
    )
  )
  ## One assay
  expected1 <- tibble::tibble(
    individualID = as.character(NA),
    specimenID = c("s1", "s2"),
    assay = "a1"
  )
  res1 <- join_full_study_metadata(dat[1, ])
  expect_equal(res1, expected1)

  ## Three assays should just be row binded
  expected2 <- tibble::tibble(
    individualID = c(NA, NA, "i1", "i2", "i3", "i1", "i2", "i3"),
    specimenID = c("s1", "s2", NA, NA, NA, "s3", "s4", "s5"),
    assay = c("a1", "a1", "a2", "a2", "a2", "a3", "a3", "a3")
  )
  res2 <- join_full_study_metadata(dat)
  expect_equal(res2, expected2)
})

test_that("join_full_study_metadata errors if only biospecimen", {
  dat <- tibble::tibble(
    metadataType = c("biospecimen"),
    assay = as.character(NA),
    data = list(
      tibble::tibble(
        individualID = c("i1", "i2", "i3"),
        specimenID = c("s1", "s2", "s3"),
        assay = c("rnaSeq", "rnaSeq", "rnaSeq")
      )
    )
  )
  expect_error(join_full_study_metadata(dat))
})

## -----------------------------------------------------------------------------

test_that("has_col returns col_names if have columns", {
  dat <- tibble::tibble(
    a = c(1, 2),
    b = c("foo", "bar"),
    c = c(TRUE, FALSE)
  )

  expect_equal(has_col(dat, "a"), "a")
  expect_equal(has_col(dat, c("a", "c")), c("a", "c"))
  expect_equal(has_col(dat, c("a", "b", "c", "d")), c("a", "b", "c"))
})

test_that("has_col returns NA if doesn't have column", {
  dat <- tibble::tibble(
    a = c(1, 2),
    b = c("foo", "bar"),
    c = c(TRUE, FALSE)
  )
  expect_equal(has_col(dat, "d"), NA)
})

## -----------------------------------------------------------------------------
test_that("join_ids_assay handles only individualID, no assay or bio_df", {
  assay1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3")
  )
  expected1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    assay = "a"
  )
  # Should use the given assay
  res1 <- join_ids_assay(
    assay_df = assay1,
    bio_df = "",
    assay = "a"
  )
  expect_equal(res1, expected1)
})

test_that("join_ids_assay handles individualID and assay, no bio_df", {
  # Should use assay col in assay
  assay1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    assay = c("a1", "a2", "a3")
  )
  expected1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    assay = c("a1", "a2", "a3")
  )
  res1 <- join_ids_assay(assay_df = assay1, bio_df = "", assay = "a")
  expect_equal(res1, expected1)

  # Should replace missing assay in col with given assay
  assay2 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    assay = c("a1", "a2", NA)
  )
  expected2 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    assay = c("a1", "a2", "a")
  )
  res2 <- join_ids_assay(assay_df = assay2, bio_df = "", assay = "a")
  expect_equal(res2, expected2)

  assay3 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    assay = as.character(NA)
  )
  expected3 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    assay = "a"
  )
  res3 <- join_ids_assay(assay_df = assay3, bio_df = "", assay = "a")
  expect_equal(res3, expected3)
})

test_that("join_ids_assay with individualID, specimenID, no assay or bio_df", {
  assay1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3")
  )
  expected1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = "a"
  )
  # Should use the given assay
  res1 <- join_ids_assay(
    assay_df = assay1,
    bio_df = "",
    assay = "a"
  )
  expect_equal(res1, expected1)
})

test_that("join_ids_assay handles individualID, specimenID, and assay, no bio_df", { # nolint
  # Should use assay col in assay
  assay1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", "a3")
  )
  expected1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", "a3")
  )
  res1 <- join_ids_assay(assay_df = assay1, bio_df = "", assay = "a")
  expect_equal(res1, expected1)

  # Should replace missing assay in col with given assay
  assay2 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", NA)
  )
  expected2 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", "a")
  )
  res2 <- join_ids_assay(assay_df = assay2, bio_df = "", assay = "a")
  expect_equal(res2, expected2)
})

test_that("join_ids_assay handles individualID, specimenID, bio_df, assay", {
  # Should join on assay col in assay
  assay1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", "a3")
  )
  bio1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", "a3")
  )
  expected1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", "a3")
  )
  res1 <- join_ids_assay(assay_df = assay1, bio_df = bio1, assay = "a")
  expect_equal(res1, expected1)

  # Should replace missing assay in col with given assay
  # no bio_df assay means it should replace assay with given assay
  assay2 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", NA)
  )
  bio2 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3")
  )
  expected2 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", "a")
  )
  res2 <- join_ids_assay(assay_df = assay2, bio_df = bio2, assay = "a")
  expect_equal(res2, expected2)

  assay3 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = as.character(NA)
  )
  bio3 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = as.character(NA)
  )
  expected3 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = "a"
  )
  res3 <- join_ids_assay(assay_df = assay3, bio_df = bio3, assay = "a")
  expect_equal(res3, expected3)

  # Should join with bio_df for the ones that match and add those that don't
  assay4 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a1", NA)
  )
  bio4 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a1", "a")
  )
  expected4 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a1", "a")
  )
  res4 <- join_ids_assay(assay_df = assay4, bio_df = bio4, assay = "a")
  expect_equal(res4, expected4)

  # Similar to above, but note that ind4,spec4 is dropped because it's assumed
  # it's for a different assay
  assay5 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a1", NA)
  )
  bio5 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", "ind4"),
    specimenID = c("spec1", "spec2", "spec3", "spec4"),
    assay = c("a1", "a1", NA, NA)
  )
  expected5 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a1", "a")
  )
  res5 <- join_ids_assay(assay_df = assay5, bio_df = bio5, assay = "a")
  expect_equal(res5, expected5)

  # Assays don't match in assay_df, bio_df --> technically the bio_df assays
  # are "dropped" even though they have the same ids
  # This isn't great, but it's the expected behavior since we don't know if
  # those ids are the same or are simply re-used
  assay5 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a1", NA)
  )
  bio5 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", "ind4"),
    specimenID = c("spec1", "spec2", "spec3", "spec4"),
    assay = c("b", "b", "c", "c")
  )
  expected5 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3"),
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a1", "a")
  )
  res5 <- join_ids_assay(assay_df = assay5, bio_df = bio5, assay = "a")
  expect_equal(res5, expected5)
})

## -----------------------------------------------------------------------------

## join_ids_assay_specimen was technically tested above
test_that("join_ids_assay_specimen handles assay_df with specimenID and bio_df with no assay", { # nolint
  assay1 <- tibble::tibble(
    specimenID = c("spec1", "spec2", "spec3"),
    assay = "a"
  )
  assay2 <- tibble::tibble(
    specimenID = c("spec1", "spec2"),
    assay = c("a1", "a2")
  )
  bio1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", "ind4"),
    specimenID = c("spec1", "spec2", "spec3", "spec4")
  )
  expected1 <- tibble::tibble(
    specimenID = c("spec1", "spec2", "spec3"),
    assay = "a",
    individualID = c("ind1", "ind2", "ind3")
  )
  expected2 <- tibble::tibble(
    specimenID = c("spec1", "spec2"),
    assay = c("a1", "a2"),
    individualID = c("ind1", "ind2"),
  )
  res1 <- join_ids_assay_specimen(assay_df = assay1, bio_df = bio1, assay = "a")
  res2 <- join_ids_assay_specimen(assay_df = assay2, bio_df = bio1, assay = "a")
  expect_equal(res1, expected1)
  expect_equal(res2, expected2)
})

test_that("join_ids_assay_specimen for assay_df with specimenIDs duplicated in bio_df grabs full subset", { # nolint
  # Not really wanted, but is what should happen based on implementation
  assay1 <- tibble::tibble(
    specimenID = c("spec1", "spec2"),
    assay = "a"
  )
  bio1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", "ind4"),
    specimenID = c("spec1", "spec2", "spec1", "spec2")
  )
  expected1 <- tibble::tibble(
    specimenID = c("spec1", "spec1", "spec2", "spec2"),
    assay = "a",
    individualID = c("ind1", "ind3", "ind2", "ind4"),
  )
  res1 <- join_ids_assay_specimen(assay_df = assay1, bio_df = bio1, assay = "a")
  expect_equal(res1, expected1)
})

test_that("join_ids_assay_specimen handles assay_df with specimenID and bio_df with all matching assay", { # nolint
  assay1 <- tibble::tibble(
    specimenID = c("spec1", "spec2", "spec3"),
    assay = "a"
  )
  assay2 <- tibble::tibble(
    specimenID = c("spec1", "spec2"),
    assay = c("a1", "a2")
  )
  bio1 <- tibble::tibble(
    specimenID = c("spec1", "spec2", "spec3"),
    assay = "a",
    individualID = c("ind1", "ind2", "ind3")
  )
  bio2 <- tibble::tibble(
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", "a2", NA),
    individualID = c("ind1", "ind2", "ind3")
  )
  bio3 <- tibble::tibble(
    specimenID = c("spec1", "spec2", "spec3"),
    assay = c("a1", NA, NA),
    individualID = c("ind1", "ind2", "ind3")
  )
  # assay1, bio1
  res1 <- join_ids_assay_specimen(assay_df = assay1, bio_df = bio1, assay = "a")
  expect_equal(res1, bio1)
  # assay2, bio2
  res2 <- join_ids_assay_specimen(assay_df = assay2, bio_df = bio2, assay = "a")
  expect_equal(res2, bio2[c(1:2), ])
  # assay2, bio3 -- assume NA matches are for the assay type
  res3 <- join_ids_assay_specimen(assay_df = assay2, bio_df = bio2, assay = "a")
  expected3 <- tibble::tibble(
    specimenID = c("spec1", "spec2"),
    assay = c("a1", "a2"),
    individualID = c("ind1", "ind2")
  )
  expect_equal(res3, expected3)
  # assay1, bio3 -- doesn't have matching indID with same assay
  # Don't drop the specimenID, but don't assume it's a specimen indicated for
  # different assay, either; just set as NA for indID
  res4 <- join_ids_assay_specimen(assay_df = assay1, bio_df = bio3, assay = "a")
  expected4 <- tibble::tibble(
    specimenID = c("spec1", "spec2", "spec3"),
    assay = "a",
    individualID = c(NA, "ind2", "ind3")
  )
  expect_equal(res4, expected4)
})

## -----------------------------------------------------------------------------

dat <- tibble::tibble(
  metadataType = c("assay", "assay", "assay", "biospecimen", "individual"),
  assay = c("a1", "a2", "a3", NA, NA),
  data = list(
    tibble::tibble(
      specimenID = c("spec1", "spec2"),
      assay = "a1"
    ),
    tibble::tibble(
      individualID = c("ind1", "ind2", "ind3"),
      assay = "a2"
    ),
    tibble::tibble(
      specimenID = c("spec3", "spec4", "spec5"),
      assay = "a3"
    ),
    tibble::tibble(
      specimenID = c(NA, NA, NA, "spec1", "spec2", "spec3", "spec4", "spec5"),
      individualID = c("ind1", "ind2", "ind3", "ind4", "ind5", "ind6", "ind7", "ind8"), # nolint
      assay = c("a2", "a2", "a2", "a1", "a1", "a3", "a3", "a3")
    ),
    tibble::tibble(
      individualID = c("ind1", "ind2", "ind3", "ind4", "ind5", "ind6", "ind7", "ind8") # nolint
    )
  )
)

## -----------------------------------------------------------------------------

test_that("add_individual_metadata adds missing individuals", {
  ind <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", NA, "ind4")
  )

  dat1 <- tibble::tibble(
    individualID = c("ind1", "ind2"),
    specimenID = c("spec1", "spec2"),
    assay = "a"
  )
  expected1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", "ind4"),
    specimenID = c("spec1", "spec2", NA, NA),
    assay = c("a", "a", NA, NA)
  )
  res1 <- add_individual_metadata(ind, dat1)
  expect_equal(res1, expected1)

  dat2 <- tibble::tibble(
    individualID = as.character(NA),
    specimenID = c("spec1", "spec2"),
    assay = "a"
  )
  expected2 <- tibble::tibble(
    individualID = c(NA, NA, "ind1", "ind2", "ind3", "ind4"),
    specimenID = c("spec1", "spec2", NA, NA, NA, NA),
    assay = c("a", "a", NA, NA, NA, NA)
  )
  res2 <- add_individual_metadata(ind, dat2)
  expect_equal(res2, expected2)

  dat3 <- tibble::tibble(
    individualID = c("ind1", "ind2"),
    specimenID = as.character(NA),
    assay = "a"
  )
  expected3 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", "ind4"),
    specimenID = as.character(NA),
    assay = c("a", "a", NA, NA)
  )
  res3 <- add_individual_metadata(ind, dat3)
  expect_equal(res3, expected3)

  dat4 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", "ind4"),
    specimenID = c("spec1", "spec2", NA, NA),
    assay = c("a", "a", NA, NA)
  )
  res4 <- add_individual_metadata(ind, dat4)
  expect_equal(res4, dat4)
})

## -----------------------------------------------------------------------------

test_that("add_biospecimen_metadata adds missing biospecimens", {
  bio1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", NA, "ind4"),
    specimenID = c("spec1", "spec2", "spec3", NA, "spec4"),
    assay = c("a", "a", NA, NA, NA)
  )
  bio2 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", NA),
    specimenID = c("spec1", "spec2", "spec3", "spec4")
  )
  dat1 <- tibble::tibble(
    individualID = c("ind1", "ind2"),
    specimenID = c("spec1", "spec2"),
    assay = "a"
  )
  expected1 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", NA, "ind4"),
    specimenID = c("spec1", "spec2", "spec3", NA, "spec4"),
    assay = c("a", "a", NA, NA, NA)
  )
  expected2 <- tibble::tibble(
    individualID = c("ind1", "ind2", "ind3", NA),
    specimenID = c("spec1", "spec2", "spec3", "spec4"),
    assay = c("a", "a", NA, NA)
  )
  res1 <- add_biospecimen_metadata(bio1, dat1)
  res2 <- add_biospecimen_metadata(bio2, dat1)
  expect_equal(res1, expected1)
  expect_equal(res2, expected2)

  ## Different dat
  dat2 <- tibble::tibble(
    individualID = as.character(NA),
    specimenID = c("spec1", "spec2"),
    assay = "a"
  )
  expected3 <- tibble::tibble(
    individualID = c(NA, NA, "ind3", NA, "ind4"),
    specimenID = c("spec1", "spec2", "spec3", NA, "spec4"),
    assay = c("a", "a", NA, NA, NA)
  )
  expected4 <- tibble::tibble(
    individualID = c(NA, NA, "ind3", NA),
    specimenID = c("spec1", "spec2", "spec3", "spec4"),
    assay = c("a", "a", NA, NA)
  )
  res3 <- add_biospecimen_metadata(bio1, dat2)
  res4 <- add_biospecimen_metadata(bio2, dat2)
  expect_equal(res3, expected3)
  expect_equal(res4, expected4)
})

## -----------------------------------------------------------------------------

test_that("add_missing_specimens adds expected missing specimens", {
  dat1 <- tibble::tibble(
    individualID = c("i1", "i2"),
    specimenID = c("s1", "s2"),
    assay = c("a", "a"),
    study = c("s1", "s1")
  )
  dat2 <- tibble::tibble(
    individualID = c("i1", "i2", "i3", "i4"),
    specimenID = c("s1", "s2", "s3", "s4"),
    assay = c("a", "a", "b", "b"),
    study = c("s1", "s1", "s2", "s2")
  )
  expected1 <- tibble::tibble(
    individualID = c("i1", "i2", "i3", "i4"),
    specimenID = c("s1", "s2", "s3", "s4"),
    assay = c("a", "a", "b", "b"),
    study = c("s1", "s1", "s2", "s2")
  )
  res1 <- add_missing_specimens(meta_df = dat1, annot_df = dat2)
  expect_equal(res1, expected1)

  dat3 <- tibble::tibble(
    individualID = c("i1", NA, "i3", "i4", "i5"),
    specimenID = c("s1", "s2", "s3", NA, NA),
    assay = c("a", "a", "b", "b", "b"),
    study = c("s1", "s1", "s2", "s2", NA)
  )
  expected2 <-  tibble::tibble(
    individualID = c("i1", "i2", "i3", "i4", "i5"),
    specimenID = c("s1", "s2", "s3", NA, NA),
    assay = c("a", "a", "b", "b", "b"),
    study = c("s1", "s1", "s2", "s2", NA)
  )
  res2 <- add_missing_specimens(meta_df = dat1, annot_df = dat3)
  expect_equal(res2, expected2)

  dat4 <- tibble::tibble(
    individualID = c("i1", "i2"),
    specimenID = c("s1", NA),
    assay = c("a", "a"),
    study = c("s1", "s1")
  )
  expected3 <- tibble::tibble(
    individualID = c("i1", "i2", "i2"),
    specimenID = c("s1", "s2", NA),
    assay = c("a", "a", "a"),
    study = c("s1", "s1", "s1")
  )
  res3 <- add_missing_specimens(meta_df = dat1, annot_df = dat4)
  expect_equal(res3, expected3)
})

## -----------------------------------------------------------------------------
test_that("join_ids_assay_all handles one or more assays", {
  dat <- tibble::tibble(
    metadataType = c("assay", "assay", "assay"),
    assay = c("a1", "a2", "a3"),
    data = list(
      tibble::tibble(
        specimenID = c("s1", "s2"),
        assay = "a1"
      ),
      tibble::tibble(
        individualID = c("i1", "i2", "i3"),
        assay = "a2"
      ),
      tibble::tibble(
        specimenID = c("s3", "s4", "s5"),
        individualID = c("i1", "i2", "i3"),
        assay = "a3"
      )
    )
  )
  ## One assay
  expected1 <- tibble::tibble(
    specimenID = c("s1", "s2"),
    assay = "a1",
    individualID = as.character(NA)
  )
  res1 <- join_ids_assay_all(dat[1, ])
  expect_equal(res1, expected1)

  ## Three assays should just be row binded
  expected2 <- tibble::tibble(
    specimenID = c("s1", "s2", NA, NA, NA, "s3", "s4", "s5"),
    assay = c("a1", "a1", "a2", "a2", "a2", "a3", "a3", "a3"),
    individualID = c(NA, NA, "i1", "i2", "i3", "i1", "i2", "i3")
  )
  res2 <- join_ids_assay_all(dat)
  expect_equal(res2, expected2)
})
