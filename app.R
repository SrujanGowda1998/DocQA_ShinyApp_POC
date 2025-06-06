# app.R
library(shiny)
library(pdftools)    # For PDF reading
library(officer)     # For Word document generation



ui <- fluidPage(
  titlePanel("Document Q&A - Upload Mode"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Document", multiple = FALSE, accept = c(".pdf")),
      textInput("question", "Ask a question"),
      actionButton("submit", "Submit"),
      downloadButton("download", "Download Response")
    ),
    
    mainPanel(
      h4("AI Answer:"),
      verbatimTextOutput("answer"),
      br(),
      h4("Extracted Text (From Document):"),
      verbatimTextOutput("extracted_text")
    )
  )
)


##################################################################################################################

server <- function(input, output, session) {
  BASE_URL <- "https://iliad.abbvienet.com" # Replace with actual base URL
  ILIAD_API_KEY <- Sys.getenv("ILIAD_API_KEY") # Or set it manually
  
  extract_text_prompt <- "
  - Extract all readable text content from this document, including text from tables, captions, labels, and any text embedded in images, diagrams, or flowcharts.
  - Your task:
  1. Extract all readable text content from the document, including captions, labels, and any text within images or diagrams.
  2. For tables, represent the data in formatted text so that it should be parsable by an LLM.
  3. For images, screenshots, and diagrams without text:
      - Provide a brief description of the image content.
      - Describe any visual elements, such as icons, symbols, or graphical representations.
      - Explain the overall purpose or meaning of the image in the context of the document.
  4. Maintain the logical order and structure the output clearly and systematically.
  5. Clearly indicate when you are describing an image or screenshot versus extracting text.
  6. Exclude any commentary or extra text — provide only the extracted and organized content.
  "
  
  observeEvent(input$file, {
    req(input$file)
    pdf_path <- input$file$datapath
    output_dir <- "temp_pages"
    
    # Create directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
    # Convert PDF to images
    image_files <- pdftools::pdf_convert(pdf_path, format = "jpeg", dpi = 300, filenames = file.path(output_dir, "page_%d.jpg"))
    
    full_text <- ""
    for (image_file in image_files) {
      img_data <- base64enc::base64encode(image_file)
      
      messages <- list(list(
        role = "user",
        content = list(
          list(type = "text", text = extract_text_prompt),
          list(type = "image", encoding = "base64", media_type = "image/jpeg", data = img_data)
        )
      ))
      
      res <- tryCatch({
        httr::POST(
          url = paste0(BASE_URL, "/api/v1/chat/gpt-4o"),
          body = list(messages = messages),
          encode = "json",
          httr::add_headers("x-api-key" = ILIAD_API_KEY)
        )
      }, error = function(e) {
        warning(paste("API call failed:", e$message))
        NULL
      })
      
      if (!is.null(res) && !httr::http_error(res)) {
        parsed <- httr::content(res, as = "parsed", simplifyVector = TRUE)
        text_part <- parsed$completion$content
        full_text <- paste0(full_text, "\n\n", text_part)
        cat(sprintf("Processed: %s\n", image_file))
      } else {
        warning(sprintf("Failed to process image: %s", image_file))
      }
    }
    
    # Clean up images
    unlink(output_dir, recursive = TRUE)
    
    # Store extracted text in reactiveVal
    extracted_text <- gsub("[[:cntrl:]]", "", full_text)  # sanitize control characters
    rv$text <- extracted_text
  })
  
  rv <- reactiveValues(text = "")
  
  observeEvent(input$submit, {
    req(input$question, rv$text)
    
    messages <- list(
      list(role = "user", content = paste(rv$text, "\n\nQuestion:", input$question))
    )
    
    res <- httr::POST(
      url = paste0(BASE_URL, "/api/v1/chat/gpt-4o"),
      body = list(messages = messages),
      encode = "json",
      httr::add_headers("x-api-key" = ILIAD_API_KEY)
    )
    
    if (httr::http_error(res)) {
      output$answer <- renderText("Error querying AI API.")
      return()
    }
    
    result <- httr::content(res, as = "parsed", simplifyVector = TRUE)
    output$answer <- renderText(result$completion$content)
  })
  
  output$extracted_text <- renderText({
    rv$text
  })
}






##################################################################################################################

shinyApp(ui, server)
