# Shared parameters for the notebooks related to the toxic comments classification challenge

# default types for the columns
col_types = list(
  id = col_character(),
  comment_text = col_character(),
  toxic = col_integer(),
  severe_toxic = col_integer(),
  obscene = col_integer(),
  threat = col_integer(),
  insult = col_integer(),
  identity_hate = col_integer()
)

col_types_df = list(
  df_split = col_integer(),
  df_id = col_character(),
  df_comment_text = col_character(),
  df_toxic = col_integer(),
  df_severe_toxic = col_integer(),
  df_obscene = col_integer(),
  df_threat = col_integer(),
  df_insult = col_integer(),
  df_identity_hate = col_integer()
)

# labels of one hot encoded columns for classification
labels = c("toxic", "severe_toxic", "obscene", "threat", "insult", "identity_hate")

#' Report 3 levels of error
report_traceback_3 = function() {
  sink(stderr())
  on.exit(sink(NULL))
  traceback(3, max.lines = 1L)
  if (!interactive()) {
    q(status = 1)
  }
}