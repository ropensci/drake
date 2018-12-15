drake_context("utils")

test_with_dir("file system", {
  expect_equal(file_extn("a.b/c.d/e/f/g_h.i.j.k"), "k")
  expect_equal(file_extn("123"), "123")
})

test_with_dir("merge_lists()", {
  x <- list(a = 1, b = 1:2, c = 1:3)
  y <- list(b = 3:4, c = 4:5, d = 1:5)
  z <- merge_lists(x, y)
  z <- lapply(z, sort)
  w <- list(a = 1, b = 1:4, c = 1:5, d = 1:5)
  expect_equal(z, w)
})

test_with_dir("drake_pmap", {
  # Basic functionality: example from purrr::pmap
  x <- list(1, 10, 100)
  y <- list(1, 2, 3)
  z <- list(5, 50, 500)
  ans <- list(x[[1]] + y[[1]] + z[[1]],
              x[[2]] + y[[2]] + z[[2]],
              x[[3]] + y[[3]] + z[[3]])
  expect_identical(ans, drake_pmap(list(x, y, z), sum))

  # Catches inputs of wrong type
  expect_error(drake_pmap("not a list", sum))
  expect_error(drake_pmap(list(), "not a function"))

  # Handles empty list
  expect_identical(list(), drake_pmap(list(), sum))

  # Passes dots to function
  x[2] <- NA
  ans[[2]] <- sum(x[[2]], y[[2]], z[[2]], na.rm = TRUE)
  expect_identical(ans, drake_pmap(list(x, y, z), sum, na.rm = TRUE))

  # Catches unequally-lengthed sublists
  x[[2]] <- NULL
  expect_error(drake_pmap(list(x, y, z), sum))
})

test_with_dir("operators", {
  expect_equal("a" %||% "b", "a")
  expect_equal(NULL %||% "b", "b")
  expect_true(is.numeric(Inf %||% "b"))
  expect_true(is.na(NA %||% "b"))
  expect_equal("a" %||NA% "b", "a")
  expect_equal(NULL %||NA% "b", "b")
  expect_true(is.numeric(Inf %||NA% "b"))
  expect_false(is.na(NA %||NA% "b"))
})

test_with_dir("weak_tibble", {
  # We test forcing to data frame and not, but results will differ
  # depending on whether tibble is installed or not
  if("tibble" %in% installed.packages()[,1]) {
    classes <- list("FALSE" = c("tbl_df", "tbl", "data.frame"), 
                    "TRUE" = "data.frame")
  } else {
    classes <- list("FALSE" = "data.frame", 
                    "TRUE" = "data.frame")
  }
  
  for(fdf in c(FALSE, TRUE)) { # force to data frame?
    # Empty object
    df <- weak_tibble(.force_df = fdf)
    expect_equivalent(dim(df), c(0, 0), info = fdf)
    expect_identical(class(df), classes[[as.character(fdf)]], info = fdf)
    
    # No factors!
    df <- weak_tibble(a = "a", .force_df = fdf)
    expect_identical(class(df$a), "character", info = fdf)
    
    # Test weak_as_tibble
    m <- matrix(letters[1:4], nrow = 2, ncol = 2)
    from_m <- weak_as_tibble(m, .force_df = fdf)
    expect_identical(class(from_m), classes[[as.character(fdf)]], info = fdf)
    expect_identical(names(from_m), c("V1", "V2"))
    expect_identical(class(from_m$V1), "character")
    
    df <- as.data.frame(m, stringsAsFactors = FALSE)
    expect_identical(weak_as_tibble(df, .force_df = fdf), from_m, info = fdf)
    
    # Complex function calls like in cache_log
    lst <- list(data.frame(x = 1), data.frame(x = 2))
    out <- weak_as_tibble(do.call(rbind, lst))
    expect_s3_class(out, "data.frame")
    out <- weak_as_tibble(do.call(rbind, lst), .force_df = fdf)
    expect_identical(class(out), classes[[as.character(fdf)]], info = fdf)
  }
})
