drake_context("rstudio")

test_with_dir("loading targets at cursor works under a real conditions", {
  skip_on_cran()
  skip_if_not_installed("rstudioapi")
  ## Template for context content. We're testing the addin on target names in
  ## every position occupied by the "%s" placeholder.
  content_template <- c(
    "foo <- function(a, %s, b) {",
    "  b$%s <- a",
    "  b$%s$a2b3 <- \"foo\"",
    "  %s <- \"foo\"",
    "  if(%s>1) \"foo\"",
    "    if(!%s|TRUE) \"bar\"",
    "  a <- 1; %s <- \"foo\"",
    "    b[%s] <- \"foo\"",
    "    print(%s[\"foo\"])",
    "xyz <- %s"
  )

  placeholder_column_pos <- unlist(
    gregexpr(
      text = content_template,
      pattern = "%s"
    )
  )

  target_list <- list(
    target1 = "value",
    zarget_2_ = "value",
    .target.3_ = "value",
    TARGET = "value"
  )
  target_names <- names(target_list)
  target_test_grid <- data.frame(
    test_text = array(
      outer(
        content_template,
        target_names,
        sprintf
      )
    ),
    target_start_col = rep(
      placeholder_column_pos,
      length(target_names)
    ),
    target_length = rep(
      nchar(target_names),
      rep(
        length(content_template),
        length(target_names)
      )
    )
  )

  test_loadd_start_end_middle <- function(
    test_text,
    column_pos,
    target_length
  ) {
    ## helper to mock up rstudio contexts
    make_context <- function(test_text, column_pos) {
      structure(list(
        id = "966E9371",
        path = "",
        contents = test_text,
        selection = structure(
          list(list(
            range = structure(
              list(
                start = structure(
                  c(row = 1, column = column_pos),
                  class = "document_position"
                ),
                end = structure(
                  c(row = 1, column = column_pos),
                  class = "document_position"
                )
              ),
              class = "document_range"
            ),
            text = ""
          )),
          .Names = "",
          class = "document_selection"
        )),
        class = "document_context"
      )
    }

    mapply(
      function(test_text, column_pos, target_length) {
        loadd_start <- rs_addin_loadd(make_context(test_text, column_pos))
        loadd_middle <- rs_addin_loadd(
          make_context(
            test_text,
            column_pos + floor(target_length / 2)
          )
        )
        loadd_end <- rs_addin_loadd(
          make_context(
            test_text,
            column_pos + target_length - 1
          )
        )
        all(c(loadd_start, loadd_middle, loadd_end) == "value")
      },
      test_text,
      column_pos,
      target_length,
      USE.NAMES = FALSE
    )
  }

  make(do.call(drake_plan, target_list), session_info = FALSE)
  suppressMessages(
    outcome <- test_loadd_start_end_middle(
      target_test_grid$test_text,
      target_test_grid$target_start_col,
      target_test_grid$target_length)
  )
  expect_identical(outcome, rep(TRUE, nrow(target_test_grid)))
})

test_with_dir("do nothing if the cursor is in the console", {
  skip_on_cran()
  skip_if_not_installed("rstudioapi")
  console_context <- structure(list(
    id = "#console",
    path = "",
    contents = "NOTARGET",
    selection = structure(
      list(list(
        range = structure(
          list(
            start = structure(
              c(row = 1, column = 1),
              class = "document_position"
            ),
            end = structure(
              c(row = 1, column = 1),
              class = "document_position"
            )
          ),
          class = "document_range"
        ),
        text = ""
      )),
      .Names = "",
      class = "document_selection"
    )
  ),
  class = "document_context")
  make(drake_plan(TARGET = "value"))
  result <- rs_addin_loadd(console_context)
  expect_null(result)
  expect_error(
    get(
      "NOTARGET",
      envir = globalenv()
    ),
    regexp = "object 'NOTARGET' not found"
  )
})

test_with_dir("graceful handling of no symbol at cursor.", {
  skip_on_cran()
  skip_if_not_installed("rstudioapi")
  whitespace_context <- structure(
    list(
      id = "966E9371",
      path = "",
      contents = "  ",
      selection = structure(
        list(list(
          range = structure(list(
            start = structure(
              c(row = 1, column = 1),
              class = "document_position"
            ),
            end = structure(
              c(row = 1, column = 1),
              class = "document_position"
            )
          ), class = "document_range"),
          text = ""
        )),
        .Names = "",
        class = "document_selection"
      )
    ),
    class = "document_context"
  )

  expect_message(
    result <- rs_addin_loadd(whitespace_context),
    "Could not find object name at cursor position"
  )
  expect_null(result)

  after_symbol_context <- structure(
    list(
      id = "966E9371",
      path = "",
      contents = "NOTARGET",
      selection = structure(
        list(list(
          range = structure(
            list(
              start = structure(
                c(row = 1, column = 9),
                class = "document_position"
              ),
              end = structure(
                c(row = 1, column = 9),
                class = "document_position"
              )
            ),
            class = "document_range"
          ),
          text = ""
        )),
        .Names = "",
        class = "document_selection"
      )
    ),
    class = "document_context"
  )

  expect_message(
    result <- rs_addin_loadd(after_symbol_context),
    "Could not find object name at cursor position"
  )
  expect_null(result)
})
