# app.R
library(shiny)
library(googlesheets4)
library(mirtCAT)



 gs4_auth(path = "quiz-487314-684225b92f49.json",
          scopes = "https://www.googleapis.com/auth/spreadsheets")
 sheet_url <- "https://docs.google.com/spreadsheets/d/1n2xYMXIvxaOoyZgdPtlMaOumWOmUyP_aipFXxD7PDQw"


#Items (minus SRG-15 which was misfitting)
srg.items=c("SRG1", "SRG2",  "SRG3",  "SRG4",  "SRG5",  "SRG6",  "SRG7",  "SRG8",  "SRG9", "SRG10", "SRG11", "SRG12", "SRG13", "SRG14") # minus "SRG15"

#CAT parameter
start = "random"
next_item = "MI"
score_function = "WLE"
stop = list(min_SEM = 0.5)


# Obtaining a calibrated item bank
#mirt.srg = mirt(srg.data[,srg.items], 1, itemtype = "Rasch")
#saveRDS(mirt.srg, paste0(getwd(), "/mirtCAT_test/mirtsrg.rds"))

#mirt.srg = readRDS(paste0(getwd(),"/mirtCAT_test/mirtsrg.rds"))
mirt.srg = readRDS("mirtsrg.rds")




####preparing an assessment interface:

#create the data frame of items and responses
options = matrix(c("not at all", "somewhat", "a great deal"), 
                 ncol = 3, nrow = 14, byrow = TRUE)

questions = c("SRG1: I learned to be nicer to others.",
              "SRG2: I feel freer to make my own decisions.",
              "SRG3: I learned that I have something of value to teach others about life.",
              "SRG4: I learned to be myself and not try to be what others want me to be.",
              "SRG5: I learned to work through models and not just give up.",
              "SRG6: I learned to find more meaning in life.",
              "SRG7: I learned to how to reach out and help others.",
              "SRG8: I learned to be a more confident person.",
              "SRG9: I learned to listen more carefully when others talk to me.",
              "SRG10: I learned to be open to new information and ideas.",
              "SRG11: I learned to communicate more honestly with others.",
              "SRG12: I learned that I want to have some impact on the world.",
              "SRG13: I learned that it's okay to ask others for help.",
              "SRG14: I learned to stand up for my personal rights.")

df <- data.frame(Question = questions, Option = options, Type = "radio")


# change some aesthetics of the interface
title <- "Stress Related Growth"
authors <- "Park - 1996"
firstpage <- list(h2("Computer Adaptive Testing Example"), h4("Please answer each item to the best of your ability. ",        
                                    "The data will not be shared ", 
                                    "and is only for today's colloquium."))

lastpage <- function(person){
  
  resp_vec <- rep(NA, 14)
  
  valid <- !is.na(person$items_answered)
  
  resp_vec[person$items_answered[valid]] <- person$responses[valid]
  
  resp_df <- as.data.frame(t(resp_vec))
  colnames(resp_df) <- paste0("SRG", 1:14)
  
  result <- cbind(
    data.frame(
      timestamp = Sys.time(),
      id = person$demographics$participant_id,
      gender = person$demographics$gender,
      age = person$demographics$age,
      theta = round(person$thetas[1],4),
      score = round((person$thetas[1] - (-4.39986)) / (4.880836 - (-4.39986)) * 100,2)
    ),
    resp_df
  )
  
  print(result)
  
  sheet_append(
    ss = sheet_url,
    data = result
  )
  

  list(h2("Thank you for completing the test."), 
       h4(sprintf('Your final SRG logit score is:  %.3f', person$thetas[1])),
       h4(sprintf("Percentage of max SRG score: %.3f", (person$thetas[1] - -4.39986)/(4.880836-(-4.39986)) *100  )))  #,
  #person$thetas_SE_history[nrow(person$thetas_SE_history), 1])))
}

demographics <- list(
  textInput(inputId = "participant_id", label = "Please enter your participant ID or name:"),
  selectInput(inputId = "gender", label = "Please select your gender.",
              choices = c("Male", "Female", "Other"), selected = ""),
  selectInput(inputId = "age", label = "Please select your age group.", 
              choices = c("16-25", "26-35", "36-45", "46-55", "56-65", "> 65 years"), selected = ""))

shinyGUI_list <- list(title = title, authors = authors, demographics = demographics,
                      demographics_inputIDs = c("participant_id", "gender", "age"),
                      firstpage = firstpage, lastpage = lastpage)


## running the assessment

mirtCAT_preamble(df = df, mo = mirt.srg, 
                          method = score_function, start_item = start, 
                          criteria = next_item,  
                          design = stop, shinyGUI = shinyGUI_list)

createShinyGUI()


