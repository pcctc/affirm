test_that("prepend_df_name()  works", {
  expect_equal(
    DM |> prepend_df_name() |> colnames(),
    c("DM.SUBJECT", "DM.AGE", "DM.RACE")
  )

  # ✖ Could not determine the name of the passed data frame. Specify the `prepend_df_name(df_name)` argument.
  expect_error(
    DM %>% select(SUBJECT, AGE) %>% prepend_df_name(),
    "Could not determine the name of the passed data frame"
  )
})
