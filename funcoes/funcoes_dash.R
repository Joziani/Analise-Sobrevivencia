info_tooltip <- function(mensagem = NULL, margin_top = 0, float = "right", lado_box = "left") {
  margin_top <- paste0("margin-top: ", margin_top, "px;")
  float <- paste0("float:", float, ";")
  style <- paste(float, margin_top)
  
  a(`data-toggle`="tooltip",
    `data-placement`=lado_box,
    `data-html`="true",
    style=style,
    class="tool-message",
    title=mensagem,
    tags$strong(icon("info-circle")))
}

# ----

box <- function (..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE, 
                 background = NULL, width = 6, height = NULL, collapsible = FALSE, 
                 collapsed = FALSE, message = NULL, message_position = "right")  {
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    if (!is.null(message)) {
      messageTag <- a(`data-toggle`="tooltip",
                      `data-placement`= message_position,
                      `data-html`="true",
                      style= "float: left; margin-top: -10px; color: #2F4C9C;",
                      class="tool-message",
                      title=message,
                      tags$strong(icon("info-circle")))
      
      titleTag <- h3(class = "box-title", messageTag, title)
    } else {
      titleTag <- h3(class = "box-title", title)
    }
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) 
      "plus"
    else "minus"
    collapseTag <- div(class = "box-tools pull-right", tags$button(class = paste0("btn btn-box-tool"), 
                                                                   `data-widget` = "collapse", shiny::icon(collapseIcon)))
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header", titleTag, collapseTag)
  }
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style)) 
      style, headerTag, div(class = "box-body", ...), if (!is.null(footer)) 
        div(class = "box-footer", footer)))
}


# ----

loader <- function(element) {
  element %>% 
    withSpinner(type = getOption("spinner.type", default = 6), color = "#050f29")
  
}


