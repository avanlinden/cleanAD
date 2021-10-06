test_that("gather_study_metadata_synIDs handles single level", {
  dir1 <- list(
    list(
      name = "foo",
      id = "syn1"
    ),
    list(
      name = "bar",
      id = "syn2"
    ),
    list(
      name = "metadata",
      id = "syn3"
    )
  )
  files <- list(
    list(
      name = "correct.csv",
      id = "syn9"
    ),
    list(
      name = "also_correct.csv",
      id = "syn8"
    )
  )
  m <- mockery::mock(dir1, files, cycle = TRUE)
  expected1 <- tibble::tibble(
    name = c("correct.csv", "also_correct.csv"),
    id = c("syn9", "syn8"),
    study = "testing"
  )
  res1 <- with_mock(
    wrapper_synGetChildren = m,
    gather_study_metadata_synIDs(dir_id = "fakedir", study = "testing")
  )
  expect_equal(
    res1,
    expected1
  )
  dir2 <- list(
    list(
      name = "foo",
      id = "syn1"
    ),
    list(
      name = "Metadata",
      id = "syn3"
    ),
    list(
      name = "bar",
      id = "syn2"
    )
  )
  m <- mockery::mock(dir2, files, cycle = TRUE)
  res2 <- with_mock(
    wrapper_synGetChildren = m,
    gather_study_metadata_synIDs(dir_id = "fakedir", study = "testing")
  )
  expect_equal(
    res2,
    expected1
  )
})

test_that("gather_study_metadata_synIDs handles double level", {
  dir1 <- list(
    list(
      name = "foo",
      id = "syn1"
    ),
    list(
      name = "bar",
      id = "syn2"
    ),
    list(
      name = "data",
      id = "syn3"
    )
  )
  dir2 <- list(
    list(
      name = "foo",
      id = "syn1"
    ),
    list(
      name = "Metadata",
      id = "syn3"
    ),
    list(
      name = "bar",
      id = "syn2"
    )
  )
  files <- list(
    list(
      name = "correct.csv",
      id = "syn9"
    ),
    list(
      name = "also_correct.csv",
      id = "syn8"
    )
  )
  m <- mockery::mock(dir1, dir2, files, cycle = TRUE)
  expected1 <- tibble::tibble(
    name = c("correct.csv", "also_correct.csv"),
    id = c("syn9", "syn8"),
    study = "testing"
  )
  res1 <- with_mock(
    wrapper_synGetChildren = m,
    gather_study_metadata_synIDs(dir_id = "fakedir", study = "testing")
  )
  expect_equal(
    res1,
    expected1
  )
})

test_that("gather_study_metadata_synIDs handles triple level", {
  dir1 <- list(
    list(
      name = "foo",
      id = "syn1"
    ),
    list(
      name = "bar",
      id = "syn2"
    ),
    list(
      name = "data",
      id = "syn3"
    )
  )
  dir2 <- list(
    list(
      name = "foo",
      id = "syn1"
    ),
    list(
      name = "Data",
      id = "syn3"
    ),
    list(
      name = "bar",
      id = "syn2"
    )
  )
  dir3 <- list(
    list(
      name = "foo",
      id = "syn1"
    ),
    list(
      name = "Metadata",
      id = "syn3"
    ),
    list(
      name = "bar",
      id = "syn2"
    )
  )
  files <- list(
    list(
      name = "correct.csv",
      id = "syn9"
    ),
    list(
      name = "also_correct.csv",
      id = "syn8"
    )
  )
  m <- mockery::mock(dir1, dir2, dir3, files, cycle = TRUE)
  expected1 <- tibble::tibble(
    name = c("correct.csv", "also_correct.csv"),
    id = c("syn9", "syn8"),
    study = "testing"
  )
  res1 <- with_mock(
    wrapper_synGetChildren = m,
    gather_study_metadata_synIDs(dir_id = "fakedir", study = "testing")
  )
  expect_equal(
    res1,
    expected1
  )
})

test_that("gather_study_metadata_synIDs returns NA if no metadata files", {
  dir1 <- list(
    list(
      name = "foo",
      id = "syn1"
    ),
    list(
      name = "bar",
      id = "syn2"
    ),
    list(
      name = "data",
      id = "syn3"
    )
  )
  dir2 <- list(
    list(
      name = "foo",
      id = "syn1"
    ),
    list(
      name = "Metadata",
      id = "syn3"
    ),
    list(
      name = "bar",
      id = "syn2"
    )
  )
  files <- NA
  m <- mockery::mock(dir1, dir2, files, cycle = TRUE)
  expected1 <- tibble::tibble(
    name = c("correct.csv", "also_correct.csv"),
    id = c("syn9", "syn8"),
    study = "testing"
  )
  res1 <- with_mock(
    wrapper_synGetChildren = m,
    gather_study_metadata_synIDs(dir_id = "fakedir", study = "testing")
  )
  expect_equal(res1, NA)
})

## -----------------------------------------------------------------------------

test_that("Does_child_exist returns true iff child exists", {
  dat1 <- list(
    list(
      name = "wrong",
      stuff = "foo"
    ),
    list(
      name = "also_wrong",
      other = "bar"
    ),
    list(
      name = "right",
      more = "baz"
    )
  )
  dat2 <- list(
    list(
      name = "right",
      stuff = "foo"
    ),
    list(
      name = "wrong",
      other = "right"
    )
  )
  res1 <- does_child_exist(dat1, "right")
  res2 <- does_child_exist(dat1[3], "right")
  res3 <- does_child_exist(dat1[c(3, 2)], "right")
  res4 <- does_child_exist(dat2, "right")
  expect_equal(res1, c(FALSE, FALSE, TRUE))
  expect_equal(res2, TRUE)
  expect_equal(res3, c(TRUE, FALSE))
  expect_equal(res4, c(TRUE, FALSE))
})
