library(shiny)
library(shinyjs)
library(DT)
library(bslib)
library(dplyr)
library(tidyr)

options(shiny.minified=TRUE)
options(shiny.fullstacktrace = FALSE)

# load brrr from cloned repo
current_wd <- getwd()
setwd("/Users/mrppdex/projects/R/brrr")
devtools::load_all()
setwd(current_wd)

get_next_index <- function(vec) {
  next_index <- NULL
  vec <- vec[!is.na(vec)]
  if(all(is.na(vec))) {
      next_index <- 1
  } else {
      tstdiff <- setdiff(1:max(vec, na.rm=TRUE), vec)
      next_index <- ifelse(length(tstdiff)==0, max(vec)+1, tstdiff[1])
  }
  return(next_index)
}

rotate_vector <- function(x, k) {
  return(c(x[(k+1):length(x)], x[1:k]))
}

ui <- page_sidebar(
  title = "Benefit-Risk Visualization Tool",
  header = tags$head(useShinyjs()),
  sidebar = sidebar(
    h4("Table Controls"),
    actionButton("add_col", "Add Column", class = "btn-primary"),
    textInput('column_name', '', placeholder = 'Enter Columns Name'),
    br(),
    actionButton("remove_col", "Remove Selected Columns", class = "btn-danger"),
    textInput('remove_column_name', '', placeholder = 'Remove Column Name'),
    br(), br(),
    actionButton("add_row", "Add Row", class = "btn-primary"),
    actionButton("remove_row", "Remove Selected Rows", class = "btn-danger"),
    br(), br(),
    p("Rename column:"),
    selectInput('current_col_name', "", choices = c("Column_1")),
    p(' = '),
    textInput("new_col_name", "", "New Name"),
    actionButton("rename_col", "Rename column", class = "btn-primary")
  ),
  navset_card_tab(
    nav_panel("Input Data", 
      fluidRow(
        column(4, p('Assign selected rows to axis')),
        column(4, selectInput('add_to_axis_number', 
          label=NULL, choices=c(1), selected=1)), 
        column(4, actionButton('add_to_axis_btn', label='Add'))
      ), hr(),
      DTOutput("data_table")
    ),
    nav_panel("Axes",
        actionButton("add_axis", "Add Axis"),
        br(),
        fluidRow(
          column(1, p('')),
          column(2, p("#")),
          column(5, p("label")),
          column(1, p("reversed?")),
          column(1, p("log scale?")),
          column(2, p("log base"))
        ),
        fluidRow(
          column(1, p('')),
          column(2, p("New axis: ")),
          column(5, textInput("axis_label", label=NULL)),
          column(1, selectInput("axis_isreversed", label=NULL, choices=c('Yes', 'No'), selected='No')),
          column(1, selectInput("axis_islog", label=NULL, choices=c('Yes', 'No'), selected='No')),
          column(2, textInput("axis_logbase", label=NULL))
        ),
        hr(),
        uiOutput("dynamic_axis_table"),
        hr(),
        actionButton("delete_axes", "Remove selected", class="btn-danger")
    ),
    nav_panel("Columns", 
        fluidRow(
          column(4, p("Column Name")),
          column(2, p("Separate")),
          column(3, p("Width")),
          column(1, p("Include")),
          column(2, p("Order"))
        ),
        uiOutput("dynamic_col_table")
    ),
    nav_panel("Plot Options",
      actionButton("br_refresh", "Refresh"),
      accordion(
        accordion_panel(
          "Labels and grouping",
          fluidRow(
            column(2, selectInput("est_col", "Estimate", choices=NULL)),
            column(2, selectInput("lci_col", "Lower CI", choices=NULL)),
            column(2, selectInput("uci_col", "Upper CI", choices=NULL)),
            column(2, selectInput("box_col", "Boxes by", choices=NULL)),
            column(2, selectInput("color_col", "Color by", choices=NULL))
          )
        ),
        accordion_panel(
          "The point of no difference",
          fluidRow(
            column(2, selectInput("neutral_pos_N", label=NULL, choices=3:20, selected=6)),
            column(4, sliderInput("neutral_pos_n", label=NULL, min=1, max=6, value=3, step=1, ticks=FALSE))
          )
        ),
        accordion_panel(
          "Arrows",
          fluidRow(
            column(3, textAreaInput("left_arrow", NULL, value="Favors Placebo")),
            column(3, textAreaInput("right_arrow", NULL, value="Favors Treatment")),
            column(3, checkboxInput("reverse_arrows", "", value=FALSE))
          )
        )
      )
    ),
    nav_panel("Plot",
      downloadButton("download_png", "Download Plot as PNG"),
      accordion(
        accordion_panel("Output options", 
          fluidRow(
            column(4, textInput("output_width", "Width", value=1000)),
            column(4, textInput("output_height", "Height", value=400)),
            column(4, sliderInput("dpi", "DPI", min = 72, max = 300, value = 150))
        ))
      ),
      plotOutput("br_plot")
    )
  )
)

server <- function(input, output, session) {
  # colum order index tracker
  col_order_idxs <- reactiveVal(c())

  # Initialize reactive data
  data <- reactiveVal(
    data.frame(list(
                AxisID = 1,
                Endpoint = 'Endpoint A', 
                Estimate = 2, LowerCI = 1, UpperCI = 3),
               stringsAsFactors = FALSE)
    )

  colspecs_data <- reactiveVal(
      data.frame(
        column_names=character(0))
  )

  br_plot_data <- reactiveVal(
    data.frame(
      col1=character(0)
    )
  )

  observe({
      req(data())
      colspecs_data(
        data.frame(list(
          column_names = colnames(data()),
          has_boundary = TRUE,
          column_width = 0.1,
          col_idx = NA)
      ))
  })

  axes_data <- reactiveVal(
    data.frame(
      list(
        id = 1,
        label = 'Difference (95% CI)',
        islog = TRUE,
        logbase = 2.0,
        isreversed = FALSE
      )
    )
  )
  
  # Add Column
  observeEvent(input$add_col, {
    new_col <- input$column_name
    if(is.na(new_col) | new_col=='' | new_col %in% colnames(data())) {
        new_col <- paste0('Column_', ncol(data())+1)
    }
    
    current_data <- data()
    current_data[[new_col]] <- ""
    data(current_data)

    # update select input for renaming
    updateSelectInput(session, "current_col_name", choices = colnames(data()))

    # update other select inputs
    current_est <- input$estimate_column
    current_low <- input$lowerci_column
    current_high <- input$upperci_column

    all_column_names <- colnames(data())

    updateSelectInput(session, 'estimate_column',
                    choices = all_column_names[all_column_names!=current_low & all_column_names!=current_high],
                    selected = current_est)

    updateSelectInput(session, 'lowerci_column',
                    choices = all_column_names[all_column_names!=current_est & all_column_names!=current_high],
                    selected = current_low)

    updateSelectInput(session, 'upperci_column',
                    choices = all_column_names[all_column_names!=current_low & all_column_names!=current_est],
                    selected = current_high)
  })

  
  # Remove Selected Columns
  observeEvent(input$remove_col, {
    selected_cols <- input$remove_column_name
    if (selected_cols %in% colnames(data()) & selected_cols!='AxisID') {
      current_data <- data()
      selected_col_idx <- which(colnames(data()) == selected_cols)
      current_data <- current_data[, -selected_col_idx, drop = FALSE]
      data(current_data)
    } else {
        showModal(modalDialog(
            title = "Warning!",
            sprintf("whaaat? Column [%s] is not present???", selected_cols),
            easyClose = TRUE,
            footer = tagList(
                modalButton("OK")
            )
        ))
    }
  })
  
  # Add Row
  observeEvent(input$add_row, {
    new_row <- setNames(as.list(rep("", ncol(data()))), names(data()))
    current_data <- data()
    data(rbind(current_data, new_row))
  })
  
  # Remove Selected Rows
  observeEvent(input$remove_row, {
    selected_rows <- input$data_table_rows_selected
    if (length(selected_rows) > 0) {
      current_data <- data()
      current_data <- current_data[-selected_rows, , drop = FALSE]
      data(current_data)
    }
  })

  # assign row to axis
  observeEvent(input$add_to_axis_btn, {
    selected_rows <- input$data_table_rows_selected
    if (length(selected_rows) > 0) {
      current_data <- data()
      current_data[selected_rows, 'AxisID'] <- as.numeric(input$add_to_axis_number)
      data(current_data)
    }
  })

  # Rename column
  observeEvent(input$rename_col, {
    selected_col_name <- input$current_col_name[1]
    new_col_name <- trimws(input$new_col_name)

    if(!is.na(selected_col_name) & selected_col_name!="" & selected_col_name %in% colnames(data()) & !new_col_name %in% colnames(data())) {
        cat(sprintf('CURRENT COL [%s], new name [%s]\n', selected_col_name, new_col_name))
        df <- data()
        names(df)[names(df) == selected_col_name] <- new_col_name
        data(df)
        updateSelectInput(session, 'current_col_name', choices=colnames(data()))
    }
  })

  # Dynamic Axes Settings
  axes_num <- reactiveVal(0)

  observeEvent(input$add_axis, {
    req(axes_data())
    df <- axes_data()
    id_to   <- max(df$id)
    id_diff <- setdiff(1:id_to, df$id)

    new_id <- NULL
    if (length(id_diff)==0) {
      new_id <- id_to + 1
    } else {
      new_id <- id_diff[1]
    }

    new_axis_df <- 
    data.frame(
      list(
        id = new_id, 
        label = input$axis_label,
        isreversed = input$axis_isreversed=='Yes',
        islog = input$axis_islog=='Yes',
        logbase= ifelse(input$axis_islog=='Yes' & is.na(as.numeric(input$axis_logbase)), 
                        2, as.numeric(input$axis_logbase))
      )
    )

    df$logbase <- as.numeric(df$logbase)
    df <- bind_rows(
      df,
      new_axis_df,
    )

    axes_data(df)

  })

  observeEvent(axes_data(), {
    updateSelectInput(session, 'add_to_axis_number', 
    choices=unique(axes_data()$id))
  })

  output$dynamic_axis_table <- renderUI({
    req(axes_data())
    axes_num(nrow(axes_data()))
    n <- axes_num()
    table_rows <- lapply(1:n, function(i) {
      fluidRow(
        column(1, checkboxInput(sprintf("axis_%d_selected", i), label='', value=FALSE)),
        column(2, textOutput(sprintf("axis_%d_id", i))),
        column(5, textInput(sprintf("axis_%d_label", i), label=NULL)),
        column(1, checkboxInput(sprintf("axis_%d_isreversed", i), label=NULL)),
        column(1, checkboxInput(sprintf("axis_%d_islog", i), label=NULL)),
        column(2, textInput(sprintf("axis_%d_logbase", i), label=NULL))
      )
    })
    do.call(tagList, table_rows)
  })

  # AXES: dynamically update labels (requires data frame axes_data())
  observeEvent(input$delete_axes, {
    req(axes_data())
    df <- axes_data()
    n <- axes_num()
    del_idx <- c()

    lapply(1:n, function(i) {
      cat('selection ', i, ': ', input[[sprintf("axis_%d_selected", i)]], '\n')
      del_idx <<- c(del_idx, input[[sprintf("axis_%d_selected", i)]])
    })

    if(all(del_idx)) del_idx <- c(FALSE)

    df <- df[!as.logical(del_idx), ]
    axes_num(nrow(df))
    axes_data(df)
  })

  observe({
    req(axes_data())
    df <- axes_data()
    n <- axes_num()

    if(n>=1) {
      lapply(1:n, function(i) {
        output[[sprintf("axis_%d_id", i)]] <- renderText({df$id[i]})

        updateTextInput(session,
          sprintf("axis_%d_label", i),
          value = df$label[i]
        )

        updateCheckboxInput(session, 
          sprintf("axis_%d_isreversed", i),
          value = df$isreversed[i])

        updateCheckboxInput(session,
          sprintf("axis_%d_islog", i),
          value = as.logical(df$islog[i]))

        updateTextInput(session,
          sprintf("axis_%d_logbase", i),
          value = as.character(df$logbase[i])
        )
        
      })
    }
  })
  
  observe({
    req(axes_data())
    df <- axes_data()
    n <- axes_num()

    if(n>=1) {
      for(i in 1:n) {
        if(any(grepl(paste0('^axis_', i), names(input)))) {
          df[i, 'label'] <- input[[sprintf("axis_%d_label", i)]]
          df[i, 'isreversed'] <- input[[sprintf("axis_%d_isreversed", i)]]
          df[i, 'islog'] <- input[[sprintf("axis_%d_islog", i)]]
          df[i, 'logbase'] <- input[[sprintf("axis_%d_logbase", i)]]
        }
      }
    }

    axes_data(df)
    print(df)
  })

  # Dynamic column specs

  # Reactive value to keep track of the number of rows
  num_rows <- reactiveVal(1)
  observe({
    req(colspecs_data())
    num_rows(nrow(colspecs_data()))
  })
  
  # # Add a new row when the "Add Row" button is clicked
  # observeEvent(input$add_col2, {
  #   num_rows(num_rows() + 1)
  # })
  
  # Render the dynamic table UI
  output$dynamic_col_table <- renderUI({
    req(colspecs_data())

    df <- colspecs_data()
    n <- nrow(df)
    
    table_rows <- NULL
    if(n>=1) {
      table_rows <- lapply(2:n, function(i) {
        has_boundary <- ifelse(is.na(df$has_boundary[i]), FALSE, df$has_boundary[i])
        col_width <- ifelse(is.na(df$column_width[i]), 0.1, df$column_width[i])
        is_included <- ifelse(is.na(df$col_idx[i]), FALSE, TRUE)
        fluidRow(
          # components
          column(4, textOutput(paste0("col_label_", i))),
          column(2, checkboxInput(paste0("col_line_", i), label = NULL, value = has_boundary)),
          column(3, sliderInput(paste0("col_width_", i), label = NULL, min=0.05, max=0.5, value=col_width)),
          column(1, checkboxInput(paste0("col_include_", i), label = NULL, value = is_included)),
          column(2, textOutput(paste0("col_include_no_", i)))
        )
      })
    }

    do.call(tagList, table_rows)
  })
  
  # Dynamically set the text output for each label
  observe({
    req(colspecs_data())

    df <- colspecs_data()
    col_idxs <- df$col_idx

    lapply(2:num_rows(), function(i) {
      output[[paste0("col_label_", i)]] <- renderText({
        df$column_name[i]
      })
      output[[paste0("col_include_no_", i)]] <- renderText({
        ifelse(is.na(df$col_idx[i]), '', as.character(df$col_idx[i]))
      })
      has_boundary <- as.logical(input[[paste0("col_line_", i)]])
      column_width <- as.numeric(input[[paste0("col_width_", i)]])
      df[i, 'has_boundary'] <<- ifelse(!is.null(has_boundary), has_boundary, TRUE)
      df[i, 'column_width'] <<- ifelse(!is.null(column_width), column_width, 0.1)
    })

    if(any(grepl('col_include_', names(input)))) {
      lapply(2:num_rows(), function(i) {
        print(input[[paste0("col_include_", i)]])
        if(paste0("col_include_", i) %in% names(input)) {
          if(input[[paste0("col_include_", i)]] & is.na(df$col_idx[i])) {
            df[i, 'col_idx'] <<- get_next_index(col_idxs)
          } else if(!input[[paste0("col_include_", i)]]) {
            df[i, 'col_idx'] <<- NA
          }
        }
      })
    }

    colspecs_data(df)
  })

  # observe({
  #   print(colspecs_data())
  # })
  
  # Render the editable data table
  output$data_table <- renderDT({
    datatable(
      data(), 
      editable = TRUE, 
      selection = list(mode = "multiple", target = "row"),
      options = list(
        dom = 't',  # Hides the table controls (search, length, etc.)
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(list(width = '150px', targets = "_all"))
      ),
      class = 'table table-striped table-bordered'
    )
  }, server = TRUE)
  
  # Update the data when edited
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    current_data <- data()
    current_data[info$row, info$col] <- info$value
    data(current_data)
  })

  observe({
    req(colspecs_data())
    req(data())

    col_data <- colspecs_data()
    col_data <- col_data[!is.na(col_data$col_idx),]

    col_data <- col_data[order(col_data$col_idx),]

    df <- data()
    df <- df[, c(col_data$column_names)]

    if(!is.null(df) & class(df)=='data.frame') {
      if(ncol(df)>0) {
        #numeric_df <- df[sapply(df, is.numeric)]
        #numeric_cols <- colnames(numeric_df)

        numeric_cols <- colnames(df)
        updateSelectInput(session, "est_col", choices=rotate_vector(numeric_cols, 1))
        updateSelectInput(session, "lci_col", choices=rotate_vector(numeric_cols, 2))
        updateSelectInput(session, "uci_col", choices=rotate_vector(numeric_cols, 3))
        updateSelectInput(session, "box_col", choices=c("<NA>", numeric_cols))
        updateSelectInput(session, "color_col", choices=c("<NA>", numeric_cols))
      }
    }
  })

  observeEvent(input$neutral_pos_N, {
    updateSliderInput(session, "neutral_pos_n", max=as.integer(input$neutral_pos_N)-1)
  })

  # plot
  observeEvent(input$br_refresh, {
    req(colspecs_data())
    req(data())

    col_data <- colspecs_data()
    col_data <- col_data[!is.na(col_data$col_idx),]

    col_data <- col_data[order(col_data$col_idx),]

    col_widths <- col_data$column_width
    if(sum(col_widths)>0.8) {
      col_widths <- 0.8*col_widths/sum(col_widths)
    } 
    col_sep <-  col_data$has_boundary
    col_widths <- col_widths * ifelse(col_sep, 1, -1)

    df <- data()
    
    df <- df[, unique(c('AxisID', col_data$column_names))]

    axes_df <- axes_data()

    if(class(df)=="data.frame") {
      column_specs <- colnames(df)
      column_specs <- column_specs[2:length(column_specs)]
      names(column_specs) <- column_specs
      value_col_name <- input[['est_col']]
      lci_col_name   <- input[['lci_col']]
      uci_col_name   <- input[['uci_col']]

      box_col_name   <- input[['box_col']]
      color_col_name <- input[['color_col']]

      cat(sprintf('color_col_name = %s\n', color_col_name))

      if(length(unique(c(value_col_name, lci_col_name, uci_col_name)))==3) {
        df_colnames <- colnames(df)
        df_colnames_ <- gsub(paste0('^', value_col_name, '$'), 'value', df_colnames)
        df_colnames_ <- gsub(paste0('^', lci_col_name, '$'), 'lower', df_colnames_)
        df_colnames_ <- gsub(paste0('^', uci_col_name, '$'), 'upper', df_colnames_)
        colnames(df) <- df_colnames_

        df$value <- as.numeric(df$value)
        df$lower <- as.numeric(df$lower)
        df$upper <- as.numeric(df$upper)

        df_colnames_ <- df_colnames_[df_colnames_!='AxisID']
        names(df_colnames_) <- df_colnames[df_colnames!='AxisID']

        df_colnames <- df_colnames_

        adf_colnames <- colnames(axes_df)
        adf_colnames <- gsub('^islog$', 'logscale', adf_colnames)
        adf_colnames <- gsub('^isreversed$', 'reversed', adf_colnames)
        colnames(axes_df) <- adf_colnames

        axes_df$id <- as.character(axes_df$id)
        df$AxisID  <- as.character(df$AxisID)

        plot_df <- df %>% left_join(axes_df, by=c('AxisID'='id')) %>% 
          mutate(tmp=1, logbase=as.numeric(logbase))
   
        arrow_labels <- NULL
        if(input$reverse_arrows) {
          arrow_labels <- rev(c(input$left_arrow, input$right_arrow))
        } else {
          arrow_labels <- rev(c(input$right_arrow, input$left_arrow))
        }

        br_fun <- function() {
          plot_br(
            plot_df,
            df_colnames,
            col_widths,
            split_axis_by_col = 'AxisID',
            split_box_by_col = ifelse(box_col_name=='<NA>', 'tmp', box_col_name),
            axis_labels_col = 'label',
            colors_by = ifelse(color_col_name=='<NA>', NA, color_col_name),
            arrow_labels = arrow_labels,
            neutral_pos = as.integer(input$neutral_pos_n),
            num_ticks = as.integer(input$neutral_pos_N),
            value_collapse = rep(FALSE, length(df_colnames)),
            options_br = page_options$new()
          )
        }

        output$br_plot <- renderPlot({
          br_fun()
        })

        output$download_png <- downloadHandler(
          filename = function() {
            paste("plot", Sys.Date(), ".png", sep = "")
          },
          content = function(file) {
            png(file, width=input$output_width, 
                      height=input$output_height, 
                      pointsize=10, 
                      res=input$dpi)  # Open the PNG device
            br_fun()
            dev.off()  # Close the device
          })


      }
    }
  })

}

shinyApp(ui = ui, server = server)