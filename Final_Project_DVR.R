library(dplyr)
library(ggplot2)
library(caret)
library(shiny)

# Load the dataset
data <- read.csv("C:/Users/Himangi/Downloads/heart.csv")

# Clean data: replace '0' in 'thal' with NA
data$thal[data$thal == 0] <- NA

# Impute missing values
data_cleaned <- data %>% 
  mutate(thal = ifelse(is.na(thal), median(thal, na.rm = TRUE), thal))

# Convert necessary columns to factors
data_cleaned$sex <- as.factor(data_cleaned$sex)
data_cleaned$cp <- as.factor(data_cleaned$cp)
data_cleaned$fbs <- as.factor(data_cleaned$fbs)
data_cleaned$restecg <- as.factor(data_cleaned$restecg)
data_cleaned$exang <- as.factor(data_cleaned$exang)
data_cleaned$slope <- as.factor(data_cleaned$slope)
data_cleaned$ca <- as.factor(data_cleaned$ca)
data_cleaned$thal <- as.factor(data_cleaned$thal)
data_cleaned$target <- as.factor(data_cleaned$target)

# Split into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data_cleaned$target, p = .8, list = FALSE)
trainData <- data_cleaned[trainIndex, ]
testData <- data_cleaned[-trainIndex, ]

# Train a logistic regression model
model_lr <- glm(target ~ ., data = trainData, family = binomial)

# Dummy user database (in-memory)

user_db <- data.frame(
  username = c("user1"),
  password = c("password1"),
  stringsAsFactors = FALSE
)


# Define server logic
server <- function(input, output, session) {
  
  # Flag to track if the user is logged in
  is_logged_in <- reactiveVal(FALSE)
  
  
  # Login logic
  observeEvent(input$login_button, {
    if (input$login_username %in% user_db$username) {
      user_row <- which(user_db$username == input$login_username)
      if (user_db$password[user_row] == input$login_password) {
        is_logged_in(TRUE)
        output$login_message <- renderText("Login successful!")
        updateTabsetPanel(session, "tabs", selected = "Home")  # Navigate to Home
      } else {
        output$login_message <- renderText("Incorrect password.")
      }
    } else {
      output$login_message <- renderText("Username not found.")
    }
  })
  
  # Create a vector of numeric column names for plotting
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  
  # Update the x and y variable selections when the app initializes
  observe({
    updateSelectInput(session, "xVar",
                      choices = numeric_vars,
                      selected = numeric_vars[1]
    )
    updateSelectInput(session, "yVar",
                      choices = numeric_vars,
                      selected = numeric_vars[2]
    )
  })
  
  # Redirect to the Register tab when link is clicked
  observeEvent(input$go_register, {
    updateTabsetPanel(session, "tabs", selected = "Register")
  })
  
  # Registration logic with redirection to login
  observeEvent(input$register_user, {
    # Check if passwords match
    if (input$reg_password == input$confirm_password) {
      if (input$reg_username %in% user_db$username) {
        output$reg_message <- renderText("Username already exists!")
      } else {
        # Append new user details to the user_db data frame
        new_user <- data.frame(
          username = input$reg_username,
          password = input$reg_password,
          stringsAsFactors = FALSE
        )
        
        # Add the new user to the database
        user_db <<- rbind(user_db, new_user)
        
        # Display success message
        output$reg_message <- renderText("Registration successful! Please log in.")
        
        # Redirect to the login page
        updateTabsetPanel(session, "tabs", selected = "Login")
      }
    } else {
      # Display error message if passwords do not match
      output$reg_message <- renderText("Passwords do not match!")
    }
  })
  
  
  # Expose the login state to the UI
  output$is_logged_in <- reactive({
    is_logged_in()
  })
  
  # Ensure the reactive state is updated even when hidden
  outputOptions(output, "is_logged_in", suspendWhenHidden = FALSE)
  
  
  # Logout logic
  observeEvent(input$logout, {
    is_logged_in(FALSE)
    updateTabsetPanel(session, "tabs", selected = "Login")
    # Stop the app when logout button is clicked
    #stopApp()
  })
  
  
  
  
  # Navigate from Home to Prediction tab
  observeEvent(input$goToDashboard, {
    updateTabsetPanel(session, "tabs", selected = "Prediction Results")
  })
  
  # Prediction logic
  observeEvent(input$predict, {
    # Prepare new data based on user input
    new_data <- data.frame(
      age = input$age,
      sex = as.factor(input$sex),
      cp = as.factor(input$cp),
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = as.factor(input$fbs),
      restecg = as.factor(input$restecg),
      thalach = input$thalach,
      exang = as.factor(input$exang),
      oldpeak = input$oldpeak,
      slope = as.factor(input$slope),
      ca = as.factor(input$ca),
      thal = as.factor(input$thal)
    )
    
    # Ensure the levels of factors in new_data match the training data
    new_data$sex <- factor(new_data$sex, levels = levels(trainData$sex))
    new_data$cp <- factor(new_data$cp, levels = levels(trainData$cp))
    new_data$fbs <- factor(new_data$fbs, levels = levels(trainData$fbs))
    new_data$restecg <- factor(new_data$restecg, levels = levels(trainData$restecg))
    new_data$exang <- factor(new_data$exang, levels = levels(trainData$exang))
    new_data$slope <- factor(new_data$slope, levels = levels(trainData$slope))
    new_data$ca <- factor(new_data$ca, levels = levels(trainData$ca))
    new_data$thal <- factor(new_data$thal, levels = levels(trainData$thal))
    
    # Predict using the trained logistic regression model
    prediction_prob <- predict(model_lr, newdata = new_data, type = "response")
    prediction <- ifelse(prediction_prob > 0.5, 1, 0)
    
    # Render prediction output
    output$prediction <- renderUI({
      div(
        style = "border: 2px solid black; 
                 background-color: #f0f8ff; 
                 padding: 15px; 
                 border-radius: 10px; 
                 color: #007bff; 
                 font-weight: bold; 
                 text-align: center; 
                 font-size: 18px;",
        paste("Prediction:", ifelse(prediction == 1, "High risk of Heart Disease Detected", "Low Risk of Heart Disease Detected"))
      )
    })
  })
  
  # About page plotting
  output$aboutPlot <- renderPlot({
    ggplot(data, aes(x = chol, fill = factor(sex))) +  # Fill color based on 'sex' factor
      geom_histogram(bins = 15, color = "brown", alpha = 0.6) +
      labs(title = "Histogram of Cholesterol Levels",
           x = "Cholesterol",
           y = "Frequency") +
      scale_fill_manual(values = c("pink", "blue"), 
                        labels = c("Female", "Male")) +  # Manually set colors and labels for sex
      theme_minimal() +
      theme(legend.position = "right",  # Position the legend to the right
            legend.title = element_blank(),  # Remove the legend title
            legend.text = element_text(size = 12))  # Optional: Customize legend text size
  })
  
  # Render the plot dynamically based on user selections
  output$predictionPlot <- renderPlot({
    req(input$xVar, input$yVar) # Ensure variables are selected before plotting
    
    if (input$plotType == "Scatter Plot") {
      ggplot(data, aes_string(x = input$xVar, y = input$yVar)) +
        geom_point(color = "blue", size = 1.5) +
        labs(title = paste("Scatter Plot of", input$xVar, "vs", input$yVar),
             x = input$xVar, 
             y = input$yVar) +
        theme_minimal()
      
    } else if (input$plotType == "Box Plot") {
      ggplot(data, aes_string(x = input$xVar, y = input$yVar, fill = input$xVar)) +
        geom_boxplot() +
        labs(title = paste("Box Plot of", input$yVar, "by", input$xVar),
             x = input$xVar,
             y = input$yVar) +
        theme_minimal()
      
    } else if (input$plotType == "Bar Chart") {
      ggplot(data, aes_string(x = input$xVar, fill = input$yVar)) +
        geom_bar(position = "dodge") +
        labs(title = paste("Bar Chart of", input$yVar, "by", input$xVar),
             x = input$xVar,
             y = "Count") +
        theme_minimal()
      
    } else if (input$plotType == "Histogram") {
      ggplot(data, aes_string(x = input$xVar, fill = input$yVar)) +
        geom_histogram(bins = 15, color = "black", alpha = 0.6) +
        labs(title = paste("Histogram of", input$xVar),
             x = input$xVar,
             y = "Frequency") +
        theme_minimal()
    }
  })
}

# Define UI
ui <- fluidPage(
  titlePanel("Heart Disease Prediction and Analysis"),
  # Add custom CSS for styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f0f8ff;
        font-family: 'Arial', sans-serif;
      }
      .container {
        background-color: white;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
      }
      .sidebarPanel {
        background-color: #e0f7fa;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
      }
      .mainPanel {
        background-color: #ffffff;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
      }
      .action-btn {
        background-color: #007bff;
        color: white;
        border: none;
        border-radius: 5px;
        padding: 10px 20px;
        font-size: 16px;
      }
      .action-btn:hover {
        background-color: #0056b3;
      }
      h3 {
        color: #007bff;
      }
      .input-label {
        font-weight: bold;
        color: #007bff;
      }
      .prediction-box {
        font-size: 20px;
        font-weight: bold;
        color: #333;
        background-color: #e9f7fc;
        padding: 20px;
        border-radius: 5px;
      }
      /* Custom CSS for positioning */
      .footer {
        position: absolute;
        bottom: 10px;
        left: 8px;
        font-size: 16px;
        color: green;
      }
      
      .go-to-dashboard {
     position: absolute;
    bottom: 5px;  /* Adjust this value to move the button closer to the bottom */
    right: 10px;  /* Keeps the button on the right side */
    font-size: 18px;
    padding: 8px 12px;
    background-color: green;
    color: white;
    border: none;
    border-radius: 5px;
      }
      .go-to-dashboard:hover {
        background-color: #0056b3;
      }
     .logout{
    position: absolute;
    background-color: red;
    color: white;
    border: none;
    border-radius: 5px;
     } 
     .logout:hover {
        background-color: #0056b3;
     }
     /* New Login styling */
      .login-container, .register-container {
        display: flex;
        justify-content: center;
        align-items: center;
        min-height: 80vh;
      }
      .login-box, .register-box {
        background-color: white;
        padding: 30px;
        border-radius: 10px;
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
        width: 100%;
        max-width: 400px;
        text-align: center;
      }
      .login-box input , .register-box input {
        margin-bottom: 15px;
        width: 100%;
      }
      .login-box .btn , .register-box .btn {
        margin-top: 10px;
        width: 100%;
        background-color: #007bff;
        color: white;
      }
      .login-box .register-link {
        margin-top: 15px;
        display: block;
        color: #007bff;
        text-decoration: none;
      }

      

      
      
    "))
  ),
  
  tabsetPanel(
    id = "tabs",
    
    
    tabPanel("Login",
             div(class = "login-container",
                 div(class = "login-box",
                     h2("Login", style = "margin-bottom: 20px;"),
                     textInput("login_username", "Username"),
                     passwordInput("login_password", "Password"),
                     actionButton("login_button", "Log in", class = "btn"),
                     actionLink("go_register", "Don't have an account? Register", class = "register-link"),
                     textOutput("login_message")
                 )
             )
    ),
    
    tabPanel("Register",
             div(class = "register-container",
                 div(class = "register-box",
                     h2("Register", style = "margin-bottom: 20px;"),
                     textInput("reg_username", "Username"),
                     passwordInput("reg_password", "Password"),
                     passwordInput("confirm_password", "Confirm Password"),
                     actionButton("register_user", "Register", class = "btn"),
                     textOutput("reg_message")
                 )
             )
    ),
    
    
    # Home Page
    tabPanel("Home",
             conditionalPanel(
               condition = "output.is_logged_in == true", # Check if user is logged in
               # Add CSS for positioning SVGs side by side
               tags$head(
                 tags$style(HTML("
      .top-right-container {
        position: absolute;
        top: 20px;
        right: 20px;
        display: flex;
        gap: 20px; /* Space between SVGs */
      }
      .svg-icon {
        width: 80px;  /* Adjust size as needed */
        height: 80px;
        fill: #C71585; /* Color for SVGs */
      }
    "))
               ),
               
               # Container for top-right SVG images
               div(class = "top-right-container",
                   HTML('
      <svg class="svg-icon" viewBox="0 0 24 24">
        <path d="M12 21.35l-1.45-1.32C5.4 15.36 2 12.28 2 8.5 2 5.42 4.42 3 7.5 3c1.74 0 3.41.81 4.5 2.09C13.09 3.81 14.76 3 16.5 3 19.58 3 22 5.42 22 8.5c0 3.78-3.4 6.86-8.55 11.54L12 21.35z"/>
      </svg>
      <svg class="svg-icon" viewBox="0 0 24 24">
        <path d="M19 3H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm-2 10h-4v4h-2v-4H7v-2h4V7h2v4h4v2z"/>
      </svg>
    ')
               ),
               h2(style = "font-size: 36px; color: black; text-align: center;","Welcome to the Heart Disease Prediction App"),
               p(style ="font-size: 20px;  color:#B22222;text-align:left;","This app helps predict the likelihood of heart disease based on user input.
               his web application allows users to predict the likelihood of 
               having heart disease based on various health parameters. By providing a set of personal health indicators, such as age, sex, cholesterol levels, blood pressure, and others, the app calculates the probability of heart disease. This prediction can help raise awareness and guide individuals 
               toward taking necessary medical actions, such as visiting a doctor or getting further tests."),
               h4(style = "font-size: 26px; color:  black; text-align: left;","Features of the App"),
               p(style ="font-size: 20px; text-align: left; color: #C71585;", 
                 "Prediction Results: Based on the information you enter (such as age, cholesterol, etc.), the app predicts whether you may be at risk for heart disease."),
               
               p(style ="font-size: 20px; text-align: left; color: #C71585;", 
                 "Visualizations: The app also provides graphical representations to better understand how certain health factors relate to heart disease."),
               
               p(style ="font-size: 20px; text-align: left; color: #C71585;", 
                 "Easy-to-Use Interface: The app is designed to be user-friendly, with simple forms to input your health data and see the results instantly."),
               h5(style = "font-size: 22px; color:  black; text-align: left;","About the Dataset"),
               p(style ="font-size: 20px;  color:#B22222;text-align:left;","The dataset used to train this model is 
             based on the Cleveland Heart Disease dataset, which contains information on 303 patients. Each record includes various medical
             details and the corresponding diagnosis of whether the patient had heart disease (1) or not (0)."),
               h5(style = "font-size: 22px; color:  black; text-align: left;","Data Privacy"),
               p(style ="font-size: 20px;  color:#B22222;text-align:left;","We take your privacy seriously. All data you input into the app is used solely for 
               the purpose of prediction and analysis. The app does not store any personal information after the session ends."),
               h5(style = "font-size: 22px; color:  black; text-align: left;","Take Action Today"),
               p(style ="font-size: 20px;  color:#B22222;text-align:left;","If the app predicts that you are at risk for heart disease, 
               we encourage you to consult with a healthcare professional for a more thorough evaluation and preventive measures."),
               
               # Add the logout button and position it at the bottom center
               div(
                 actionButton("logout", "Logout"),class="logout",
                 style = "position: absolute; bottom: 20px; left: 50%; transform: translateX(-50%);"
               ),
               
               # Footer with name at bottom-left
               div(class = "footer", "Designed by Shambhavi Jain and Himangi Acharaya",style ="font-size: 16px;"),
               actionButton("goToDashboard", "Go to Dashboard", class = "go-to-dashboard",style = "display: block; margin: 20px auto; font-size: 18px;")
             ) ),
    
    # About Page with a histogram
    tabPanel("About", 
             conditionalPanel(
               condition = "output.is_logged_in == true", # Check if user is logged in
               # Add CSS for positioning SVGs side by side
               tags$head(
                 tags$style(HTML("
      .top-right-container {
        position: absolute;
        top: 20px;
        right: 20px;
        display: flex;
        gap: 20px; /* Space between SVGs */
      }
      .svg-icon {
        width: 80px;  /* Adjust size as needed */
        height: 80px;
        fill: #C71585; /* Color for SVGs */
      }
    "))
               ),
               
               # Container for top-right SVG images
               div(class = "top-right-container",
                   HTML('
      <svg class="svg-icon" viewBox="0 0 24 24">
        <path d="M12 21.35l-1.45-1.32C5.4 15.36 2 12.28 2 8.5 2 5.42 4.42 3 7.5 3c1.74 0 3.41.81 4.5 2.09C13.09 3.81 14.76 3 16.5 3 19.58 3 22 5.42 22 8.5c0 3.78-3.4 6.86-8.55 11.54L12 21.35z"/>
      </svg>
      <svg class="svg-icon" viewBox="0 0 24 24">
        <path d="M19 3H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm-2 10h-4v4h-2v-4H7v-2h4V7h2v4h4v2z"/>
      </svg>
    ')
               ),
               
               
               h3(style = "font-size: 30px; color:black; text-align: center;","About The Application"),
               p(style ="font-size: 16px;  color:#007bff;text-align:left;","This app predicts the likelihood of heart disease based on various health metrics. 
                It also provides visualizations of cholesterol distribution and the relationship between age and 
               cholesterol.In this  heart disease prediction model was built using logistic regression, a statistical method for modeling the relationship
               between a binary outcome (heart disease: yes or no) and several predictor variables. The model
               was trained on a publicly available dataset of heart disease cases and evaluated for its predictive accuracy.
                 The purpose of this app is not to replace professional medical advice but to provide individuals with useful insights
               into their heart health. By entering your health data, you can get a better understanding of your risk factors and have 
               more informed discussions with your healthcare provider.
               This app is based on data from a real-world medical dataset, and the machine learning model is designed to predict
               heart disease with reasonable accuracy. However, always consult with a healthcare professional for diagnosis and treatment options.
               The heart disease prediction project significantly enhances healthcare by enabling early diagnosis and improving clinical 
             decision-making.It supports personalized healthcare through risk stratification and tailored treatment plans, ensuring that high-risk patients receive focused care. 
             Additionally, it can drive preventive healthcare initiatives,  empowering individuals to monitor their heart health proactively."),
               plotOutput("aboutPlot"),
               
               # Footer with name at bottom-left
               div(class = "footer", "Designed by Shambhavi Jain and Himangi Acharaya",style ="font-size: 16px;")
             )  ),
    
    # Prediction Results Page with prediction logic
    tabPanel("Prediction Results",
             conditionalPanel(
               condition = "output.is_logged_in == true", # Check if user is logged in
               # Add CSS for positioning SVGs side by side
               tags$head(
                 tags$style(HTML("
      .top-right-container {
        position: absolute;
        top: 20px;
        right: 20px;
        display: flex;
        gap: 20px; /* Space between SVGs */
      }
      .svg-icon {
        width: 80px;  /* Adjust size as needed */
        height: 80px;
        fill: #C71585; /* Color for SVGs */
      }
    "))
               ),
               
               # Container for top-right SVG images
               div(class = "top-right-container",
                   HTML('
      <svg class="svg-icon" viewBox="0 0 24 24">
        <path d="M12 21.35l-1.45-1.32C5.4 15.36 2 12.28 2 8.5 2 5.42 4.42 3 7.5 3c1.74 0 3.41.81 4.5 2.09C13.09 3.81 14.76 3 16.5 3 19.58 3 22 5.42 22 8.5c0 3.78-3.4 6.86-8.55 11.54L12 21.35z"/>
      </svg>
      <svg class="svg-icon" viewBox="0 0 24 24">
        <path d="M19 3H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm-2 10h-4v4h-2v-4H7v-2h4V7h2v4h4v2z"/>
      </svg>
    ')
               ),
               
               
               sidebarLayout(
                 sidebarPanel(
                   h3("Enter Your Information"),
                   numericInput("age", "Age", value = 32, min = 1, max = 100),
                   selectInput("sex", "Sex", choices = c("Male" = 1, "Female" = 0)),
                   selectInput("cp", "Chest Pain Type", choices = c("Typical Angina" = 0, 
                                                                    "Atypical Angina" = 1, 
                                                                    "Non-anginal Pain" = 2, 
                                                                    "Asymptomatic" = 3)),
                   numericInput("trestbps", "Resting Blood Pressure (mm Hg)", value = 120),
                   numericInput("chol", "Serum Cholesterol (mg/dl)", value = 200),
                   selectInput("fbs", "Fasting Blood Sugar > 120 mg/dl", choices = c("No" = 0, "Yes" = 1)),
                   selectInput("restecg", "Resting ECG", choices = c("Normal" = 0, 
                                                                     "ST-T Abnormality" = 1, 
                                                                     "Left Ventricular Hypertrophy" = 2)),
                   numericInput("thalach", "Maximum Heart Rate Achieved", value = 150),
                   selectInput("exang", "Exercise Induced Angina", choices = c("No" = 0, "Yes" = 1)),
                   numericInput("oldpeak", "ST Depression", value = 0.0),
                   selectInput("slope", "Slope of Peak Exercise ST Segment", choices = c("Upsloping" = 0, 
                                                                                         "Flat" = 1, 
                                                                                         "Downsloping" = 2)),
                   selectInput("ca", "Number of Major Vessels", choices = c(0, 1, 2, 3)),
                   selectInput("thal", "Thalassemia", choices = c("Normal" = 2, "Fixed Defect" = 1, "Reversible Defect" = 3)),
                  
                   
                 selectInput("plotType", "Select Plot Type:",
                             choices = c("Scatter Plot", "Box Plot", "Bar Chart", "Histogram"),
                             selected = "Scatter Plot"),
                 selectInput("xVar", "Select X-axis Variable:", choices = NULL),
                 selectInput("yVar", "Select Y-axis Variable:", choices = NULL),
                 actionButton("predict", "Predict", class = "btn btn-primary")
               ),
                 
                 mainPanel(
                   h3("Prediction Result :"),
                   uiOutput("prediction"),
                   plotOutput("predictionPlot")
                   
                 )
               )
             )
    ) )
)

# Run the app
shinyApp(ui = ui, server = server)