library(shiny)
library(dplyr)
library(tidyr)

PEOPLE <- sort(c("BC", "KY", "GA", "AJ", "PJ", "BS"))
MOVIE_LIST <- sort(c("Batman Begins", "Tropic Thunder", "Mean Girls",
                     "The man who knew too little"))
MOVIE_LIST <- NULL

shinyServer(function(input, output) {

  values <- reactiveValues(movie_list = MOVIE_LIST)

  observeEvent(input$input_add, {
    new_movie <- isolate(input$input_movie)

    if(new_movie %in% values$movie_list) {
      values$movie_list <- setdiff(values$movie_list, new_movie)
    } else if(new_movie != "") {
      values$movie_list <- sort(unique(c(values$movie_list, new_movie)))
    }
  })

  output$output_movie_list <- renderText({
    movie_list <- values$movie_list
    paste0(movie_list, collapse = "<br>")
  })

  output$all_tabs <- renderUI({
    movie_list <- values$movie_list
    if(length(input$input_people) == 0) {
      people <- PEOPLE
    } else {
      people <- input$input_people
    }

    setup_panel <- list(
      tabPanel("Setup",
               selectizeInput("input_people", "Who's voting?", PEOPLE,
                              multiple = TRUE, selected = people),
               textInput("input_movie", "Movies/Shows"),
               actionButton("input_add", "Add/Remove"),
               htmlOutput("output_movie_list")))
    results_panel <- list(
      tabPanel("Results", htmlOutput("results"))
    )
    summary_panel <- list(
      tabPanel("Vote summary", tableOutput("summary_tables"))
    )

    out <- lapply(seq_along(people), function(p) {
      person <- people[p]
      movie_count <- length(movie_list)
      tabPanel(person, lapply(seq_along(movie_list), function(m) {
        id <- paste("vote", tolower(person), m, sep="_")
        movie <- movie_list[m]
        selectInput(id, movie, 1:movie_count, selected = m)
      }))
    })

    out <- c(setup_panel, out, summary_panel, results_panel)
    do.call(tabsetPanel, out)
  })

  votes <- reactive({
    people     <- input$input_people
    movie_list <- values$movie_list

    votes <- list(NULL)
    for(person in people) {
      for(m in seq_along(movie_list)) {
        id <- paste("vote", tolower(person), m, sep="_")
        votes <- c(votes, list(data.frame(
          person = person,
          movie  = movie_list[m],
          rank   = input[[id]],
          value  = 1,
          stringsAsFactors = FALSE)))
      }
    }
    votes <- bind_rows(votes) %>%
      spread(rank, value, fill = 0) %>%
      arrange(person, movie)

    # Check that ranks are unique by person
    chk <- votes %>%
      select(-movie) %>%
      group_by(person) %>%
      summarise_all(funs(sum)) %>%
      mutate(chk = rowSums(.[-1] != 1)) %>%
      filter(chk != 0)
    if(nrow(chk) > 0) {
      err <- paste("Damnit", chk$person, "you ranked two or movies as the same.")
      validate(need(FALSE, err))
    }
    votes <- votes %>%
      gather(rank, value, matches("\\d")) %>%
      filter(value == 1)
  })

  output$summary_tables <- renderTable({
    votes() %>%
      select(-value) %>%
      spread(person, rank) %>%
      arrange(movie) %>%
      rename(Movie = movie)

  })

  output$results <- renderText({
    votes <- votes()
    votes <<- votes
    out <- irv_results(votes)
    paste0(out, collapse = "<br>")
  })

})
