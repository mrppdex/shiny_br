
test_data <- data.frame(list(
    "AxisID"   = 1,
    "BoxID"    = 1,
    "Axis Name" = "Difference (95%CI)",
    "Endpoint" = c("Endpoint A"),
    "value" = 2,
    "lower"  = 1, 
    "upper"  = 3
))

column_specs <- colnames(test_data)[4:ncol(test_data)]
names(column_specs) <- column_specs

breaks_widths <- c(0.2, -0.1, -0.15, 0.15)

plot_br(
  test_data,
  column_specs,
  breaks_widths,
  split_axis_by_col='AxisID',
  axis_labels_col='Axis Name',
  split_box_by_col='BoxID',
  neutral_pos = 3,
  num_ticks = 6,
#   top_margin = NULL,
#   userect = FALSE,
  arrow_labels = c("Favors\nTreatment", "Favors\nPlacebo"),
  value_collapse = rep(FALSE, length(column_specs)),
#   box_group = NULL,
  options_br = page_options$new()
)

######

