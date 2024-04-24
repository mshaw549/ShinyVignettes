#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shinyWidgets)
library(Rcpp)
library(shiny)

(ui <- fluidPage(
  titlePanel("Factorial Vignettes App and Items - Lofgreen et al. 2021"),
  sidebarLayout(
    sidebarPanel(
      selectInput("name", "Choose a name", choices = c("Madison",
                                                       "Emma",
                                                       "Olivia",
                                                       "Hannah",
                                                       "Abigail",
                                                       "Isabella",
                                                       "Samantha",
                                                       "Elizabeth",
                                                       "Ashley",
                                                       "Alexis",
                                                       "Sarah",
                                                       "Sophia",
                                                       "Alyssa",
                                                       "Grace",
                                                       "Ava",
                                                       "Taylor",
                                                       "Brianna",
                                                       "Lauren",
                                                       "Chloe")),
      
      checkboxGroupButtons("variables", "Variables for vignette:", c("Attire" = "attire_choice",
                                                                     "Date Sexual History" = "sexhist_choice",
                                                                     "Dyad Sexual History" = "pair_choice",
                                                                     "Alcohol Use" = "alcohol_choice"
                                                                     #"Level of Sexual Intimacy" = "intimacy_choice"
      )),
      selectInput("responsetype", "Choose a response type", choices = c(  "In response, she says 'Let's not do this right now'",
                                                                          "In response, she pushes away from you",
                                                                          "In response, she pulls you closer to her.",
                                                                          "In response, she says 'I want you.'",
                                                                          "In response she pulls you closer to her and says 'I want you.'",
                                                                          "In response, she tenses up - but doesn't say anything",
                                                                          "She stops responding to you, but doesn't resist you in any way.")),
      textOutput("result")
    ),
    mainPanel(
      HTML("<B>You can create your own vignettes for studying sexual scenarios using this app.</B>"),
      p("Select which factors you would like to add to the factorial vignettes as well as the name of the character in the vignette and their response type."),
      p("You can download all vignettes in either a CSV or in a format directly prepared for qualtrics."),
      p("Note that if using this for qualtrics, you must add [[AdvancedFormat]] and [[Block:'NAME']], \n
        the name being what you would like to call the block, to the first two rows of the text file like so:\n"),
        p("[[AdvancedFormat]]\n"),
        p("[[Block:Vignette]]\n"),
        HTML("<B>The legend for the vignettes is as follows:</B>"),
      p("Attire Factor: 10000 = pretty attire, 20000 = sexy attire"),
      p("Female Sexual History Factor: 1000 = no prior female sexual history, 2000 = female sexual history with one male, 3000 = female casual sexual history"),
      p("Dyad Sexual History Factor: 100 = no dyad sexual history, 200 = dyad sexual history"),
      p("Alcohol Factor: 10 = no alcohol use, 20 = conjoint alcohol use."),
      p("Intimacy Factor: 1 = attempt to kiss, 2 = kissing, attempt to reach under shirt, 3 = oral sex, attempt for intercourse"),
      p(),
      downloadButton("downloadData", "Download CSV Vignettes"),
      downloadButton("downloadQualtricsFormat", "Download Formatted Qualtrics Vignettes (Be sure to add [[AdvancedFormat]] and [[Block:'NAME']] in rows 1 and 2 of .txt file for import)
"),
      p(),
      HTML("<B>Example Vignette:</B>"),
      textOutput("result2"),
      p(),
      downloadButton("downloadResponseitems", "Download Vignette Response Items"),
      p(),
      HTML("<B>Citation for published article: Lofgreen, A. M., Mattson, R. E., Wagner, S. A., Ortiz, E. G., & Johnson, M. D. (2021). Situational and dispositional determinants of college men’s perception of women’s sexual desire and consent to sex: A factorial vignette analysis. Journal of Interpersonal violence, 36(1-2), NP1064-NP1097.</B>")
      
    )
  )
))

server <- function(input, output){
  
  vign_name <- reactive({input$name})
  vign_type <- reactive({input$responsetype})
  
  #make strings appear for the paragraph
  
  str1 <- "You just spent an evening out with "
  str3 <- ", a girl who you think is very attractive. You've been dating her for several weeks. "
  attire_pret_str1 <- "You think "
  attire_pret_str3 <- " looks really pretty. She's wearing a nice blouse and skirt. "
  attire_sexy_str1 <- "You think "
  attire_sexy_str3 <- " looks really sexy. She's wearing a short skirt and a blouse that shows her cleavage. "
  Sexualhist_nosex_str1 <-  "You know that "
  Sexualhist_nosex_str3 <- " has never had sex with anyone else before. "
  Sexualhist_1part_str1 <-  "You know that "
  Sexualhist_1part_str3 <- " has only had sex before with an ex-boyfriend that she dated seriously. "
  Sexualhist_casualsex_str1 <-  "You know that "
  Sexualhist_casualsex_str3 <- " has had casual sex with several guys since she's been in college. "
  pairnosex_str1 <- "Although you haven't had sex with "
  pairnosex_str3 <- ", you're really hoping you'll get the chance to tonight. "
  pairsex_str1 <- "You've had sex with "
  pairsex_str3 <- ", and you're really hoping you'll get the chance to again tonight. "
  Alcohol_str <- c("After your outing the two of you go back to your place where you have some privacy. ",
                   "During your date both of you had several drinks before going back to your place where you have some privacy. ")
  intimacy_kiss_str1 <- "After chatting for a while, you're feeling really turned on, so you move towards "
  intimacy_kiss_str3 <- " to give her a kiss. "
  intimacy_shirt_str1 <- "After chatting for a while, you and "
  intimacy_shirt_str3 <- " start kissing. After a few minutes of making-out you're feeling really turned on, so you start to reach under "
  intimacy_shirt_str5 <- "'s shirt. "
  intimacy_oral_str1 <- "After chatting for a while, you and "
  intimacy_oral_str3 <- " start making out. Within a few minutes, the two of you are completely undressed and having oral sex. You're feeling really turned on, so you begin to move your penis towards "
  intimacy_oral_str5 <- "'s vagina. "
  
  #create  compiled strings for vignette vars
  
  intro <- reactive({paste0(str1, vign_name(), str3)})
  attire_pret <- reactive({paste0(attire_pret_str1, vign_name(), attire_pret_str3)})
  attire_sexy <- reactive({paste0(attire_sexy_str1, vign_name(), attire_sexy_str3)})
  
  Sexualhist_nosex <- reactive({paste0(Sexualhist_nosex_str1,vign_name(),Sexualhist_nosex_str3)})
  Sexualhist_1part <- reactive({paste0(Sexualhist_1part_str1,vign_name(),Sexualhist_1part_str3)})
  Sexualhist_casualsex <- reactive({paste0(Sexualhist_casualsex_str1, vign_name(), Sexualhist_casualsex_str3)})
  pairnosex <- reactive({paste0(pairnosex_str1, vign_name(), pairnosex_str3)})
  pairsex <- reactive({paste0(pairsex_str1, vign_name(), pairsex_str3)})
  intimacy_kiss <- reactive({paste0(intimacy_kiss_str1, vign_name(), intimacy_kiss_str3, sep = " ")})
  intimacy_shirt <- reactive({paste0(intimacy_shirt_str1, vign_name(), intimacy_shirt_str3, vign_name(), intimacy_shirt_str5, sep = " ")})
  intimacy_oral <- reactive({paste0(intimacy_oral_str1, vign_name(), intimacy_oral_str3, vign_name(), intimacy_oral_str5, sep = " ")})
  
  Attire <- reactive({c(attire_pret(), attire_sexy())})
  Sexualhist <- reactive({c(Sexualhist_nosex(), Sexualhist_1part(), Sexualhist_casualsex())})
  Pairhist <- reactive({c(pairnosex(), pairsex())})
  Intimacy <- reactive({c(intimacy_kiss(), intimacy_shirt(), intimacy_oral())})
  Alcohol <- reactive({Alcohol_str})
  
  perms <- reactive({
    # Initialize vectors for each factor
    Attire_factors <- c(attire_pret(), attire_sexy())
    Sexualhist_factors <- c(Sexualhist_nosex(), Sexualhist_1part(), Sexualhist_casualsex())
    Pairhist_factors <- c(pairnosex(), pairsex())
    Alcohol_factors <- Alcohol_str
    Intimacy_factors <- c(intimacy_kiss(), intimacy_shirt(), intimacy_oral())
    
    # Get all possible combinations of factors based on user selection
    selected_factors <- input$variables
    selected_factors <- selected_factors[selected_factors != "Attire"]
    
    # Create a dataframe with all combinations of selected factors
    df_combinations <- expand.grid(Attire = if ("attire_choice" %in% selected_factors) Attire_factors else "",
                                   Sexualhist = if ("sexhist_choice" %in% selected_factors) Sexualhist_factors else "",
                                   Pairhist = if ("pair_choice" %in% selected_factors) Pairhist_factors else "",
                                   Alcohol = if ("alcohol_choice" %in% selected_factors) Alcohol_factors else "",
                                   Intimacy = Intimacy_factors)
    
    # Filter out rows where all factors are empty
    df_combinations <- df_combinations[rowSums(df_combinations != "") > 0, ]
    
    # Add the introductory text
    df_combinations$intro <- intro()
    
    # Create the vignette by pasting all factors
    df_combinations$vignette <- paste0(df_combinations$intro,
                                       df_combinations$Attire,
                                       df_combinations$Sexualhist,
                                       df_combinations$Pairhist,
                                       df_combinations$Alcohol,
                                       df_combinations$Intimacy,
                                       vign_type())
    
    # Assign IDs based on the levels of each vignette
    df_combinations$code <- as.integer(factor(df_combinations$intro, levels = unique(df_combinations$intro))) * 100000 +
      as.integer(factor(df_combinations$Attire, levels = unique(df_combinations$Attire))) * 10000 +
      as.integer(factor(df_combinations$Sexualhist, levels = unique(df_combinations$Sexualhist))) * 1000 +
      as.integer(factor(df_combinations$Pairhist, levels = unique(df_combinations$Pairhist))) * 100 +
      as.integer(factor(df_combinations$Alcohol, levels = unique(df_combinations$Alcohol))) * 10 +
      as.integer(factor(df_combinations$Intimacy, levels = unique(df_combinations$Intimacy)))
    
    # Create an empty data frame to store the result
    result_df <- data.frame(Result = character(), stringsAsFactors = FALSE)
    
    # Loop through each row of the combinations dataframe
    for (i in 1:nrow(df_combinations)) {
      # Create the structured text for the current row
      row_text <- paste(
        "\n[[Question:MC]]\n[[ID:", df_combinations$code[i], "]]\n",
        df_combinations$vignette[i],
        "\n[[Choices]]\nCheck this box to confirm that you read the vignette",
        sep = ""
      )
      
      # Append the structured text to the result data frame
      result_df <- rbind(result_df, data.frame(Result = row_text, stringsAsFactors = FALSE))
    }
    
    # Reset row names of result_df
    rownames(result_df) <- NULL
    
    list(df = df_combinations, result_df = result_df)
  })
  
  
  
  output$result2 <- renderText({
    sample(perms()$df$vignette, 1)
  })
  
  output$result <- renderText({
    paste0("Date Name chosen: ", vign_name())
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("vignettes", ".csv")
    },
    content = function(file) {
      write.csv(perms()$df, file, row.names = FALSE)
    }
  )
  
  output$downloadQualtricsFormat <- downloadHandler(
    filename = function() {
      paste0("vignettes", ".txt")
    },
    content = function(file) {
      writeLines(perms()$result_df$Result, file)
    }
  )
  
  output$downloadResponseitems <- downloadHandler(
    filename <- function() {
      paste("VignetteItemResponses", "zip", sep=".")
    },
    
    content <- function(file) {
      file.copy("ItemResponse_Files.zip", file)
    },
    contentType = "application/zip"
  )
  
  

  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste0("vignette", ".csv")
  #   },
  #   content = function(file) {
  #     write.csv(perms()$df, file)
  #   }
  # )
  # 
  # output$downloadQualtricsFormat <- downloadHandler(
  #   filename = function() {
  #     paste0("vignette", ".txt")
  #   },
  #   content = function(file) {
  #     writeLines(perms()$result_df$Result, file)
  #   }
  # )
  
  
}

shinyApp(ui, server)