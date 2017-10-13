library(jsonlite)
library(shiny)
library(httr)

fromJSONVerbatim <- function(what) {
  fromJSON(what, simplifyVector = FALSE)
}
toJSONVerbatim <- function(what) {
  toJSON(what, auto_unbox = TRUE)
}

dbUrl <- "http://127.0.0.1:5984/shiny"
dbInfo <- try({
  url(dbUrl)
}, silent = TRUE)
if ("try-error" == class(dbInfo)) {
  PUT(url = dbUrl)
}

todos <- list()
fetchTodos <- function() {
  todos <<- lapply(fromJSONVerbatim(url(
    paste0(dbUrl, "/_all_docs?include_docs=true")
  ))$rows, function(r) {
    r$doc
  })
}
postTodo <- function(title) {
  tmplte <- fromJSONVerbatim('{"title": "", "done": false}')
  tmplte$title <- title
  POST(url = dbUrl,
       body = toJSONVerbatim(tmplte),
       content_type_json())
}
completeTodo <- function(doc) {
  doc$done <- TRUE
  PUT(url = paste0(dbUrl, "/", doc$`_id`),
      body = toJSONVerbatim(doc),
      content_type_json())
}

observeCount <- 0
showTodos <- function(input, output, session) {
  removeUI("#todos *", immediate = TRUE, multiple = TRUE)
  lapply(c(1:length(todos)), function(i) {
    insertUI(
      "#todos",
      ui = checkboxInput(
        paste0("done", i),
        label = as.character(todos[[i]]$title),
        value = as.logical(todos[[i]]$done)
      ),
      where = "beforeEnd"
    )
    if (i >= observeCount) {
      observeEvent(input[[paste0("done", i)]], {
        if (input[[paste0("done", i)]] & (!todos[[i]]$done)) {
          completeTodo(todos[[i]])
          fetchAndShowTodos(input, output, session)
        } else {
          updateCheckboxInput(session, paste0("done", i), value = todos[[i]]$done)
        }
      }, ignoreInit = TRUE)
      observeCount <<- observeCount + 1
    }
  })
  df <-
    data.frame(status = unlist(lapply(todos, function(r) {
      if (r$done)
        'done'
      else
        'open'
    })), cnt = c(1))
  dfAgg <- aggregate(cnt ~ status, df, FUN = sum)
  output$pie <- renderPlot(pie(dfAgg$cnt, labels = dfAgg$status, radius=1))
}
fetchAndShowTodos <- function(input, output, session) {
  fetchTodos()
  showTodos(input, output, session)
}

shinyApp(
  ui = fluidPage(HTML('<style type="text/css">#pie {border: 2px dashed lightgrey; overflow: hidden; } #pie img {margin: -10px; }</style>'), flowLayout(
    verticalLayout(
      tags$div(id = "todos"),
      verticalLayout(
        textInput("newTitle", label = "New Todo: "),
        actionButton("addNew", label = "Add")
      )
    ),
    plotOutput("pie", width = 300, height = 300)
  )),
  server = function(input, output, session) {
    fetchAndShowTodos(input, output, session)
    observeEvent(input$addNew, {
      if (nchar(input$newTitle) > 0) {
        postTodo(input$newTitle)
        updateTextInput(session, "newTitle", value = "")
        fetchAndShowTodos(input, output, session)
      }
    })
    session$onSessionEnded(function() {
      stopApp()
    })
  }
)
