# Here, I'll build a Shiny App that lets me subset data quickly and efficiently.

# The base dataset is called POS.Data. We'll subset that as we go forward to create more detailed data sets.
rm(list=ls())
POS.Data <- read.csv(file = "Aggregated POS data.csv", fill = NA)

Sys.setlocale('LC_ALL','C') # This makes my life a bit easier, because we get to ignore certain characters that make some annoying errors.


#    To do list:
# Write up a list of a few questions I've been asked about the POS system.
# Add duration control



# Questions:
#   How many employee parking passes exist (parttime+full time) during an active semester (Say mid-November). AKA, how many cars are on campus?
#   The same question for student commuters who have a car...how many commuter cars are on campus on a typical day?
#   How many on-campus Frosh and Sophomores purchased R Permits during move out.


# Issues:
#   How to deal with Expiration dates that are before the Issue date [I decided to delete them.]
#   How to deal with Purchases vs Returns. [I've made a note of that]
#   How to deal with edge cases in general....


POS.Culling <- function(inputData = POS.Data, dropNA = TRUE, dropVoid = TRUE, dropPermits = TRUE, dropBadDates = TRUE){
  if (dropNA) {# This removes any blank entries for Permit Cost, Quantity, and any zero count Quantities. Usually, those are 'void', so it's good to ignore.
    inputData <- subset(inputData, !is.na(ExtCost))
    inputData <- subset(inputData, !is.na(Qty))
    inputData <- subset(inputData, Qty > 0)
  }
  if (dropVoid) { # This removes any entries that mention any form of "void", because that's just annoying.
    inputData$Void <- 0
    inputData$Void[grep("void", inputData$PaymentType, ignore.case = TRUE)] <- 1
    inputData$Void[grep("void", inputData$Days, ignore.case = TRUE)] <- 1
    inputData <- subset(subset(inputData, Void == 0), select = -Void)
  }
  if (dropPermits) { # This drops all entries that don't have a permit code.
    inputData <- subset(inputData, !is.na(PermitCode))
  }
  if (dropBadDates) {# This removes any entry with an Expiration Date that is less than the Date Issued. 
    inputData <- subset(inputData, as.Date(ExpirationDate) >= as.Date(Date))
  }
  
  return(subset(inputData, select = -X))
}

WorkingData <- POS.Culling()

POS.Subset <- function(
  input = WorkingData,
  startDate = as.Date("2018-06-01"),
  endDate = as.Date("2019-07-01"),
  permitList = c(),
  duration = c(),
  students = 1,
  staff = 1,
  nonaffiliates = 1,
  medicalPermits = 1,
  returnedPermits = 1,
  onCampus = 1,
  limited = 0,
  scratch = 0){
    output <- input
  
    output <- subset(output, as.Date(Date) >= startDate & as.Date(Date) <= endDate)
    
    if (length(permitList) > 0) {output <- subset(output, Permit.Type %in% permitList)}
    
    if (length(duration) > 0) {output <- subset(output, Permit.Duration %in% duration)}
    
    ifelse(students == 0,
           output <- subset(output, Student == 0),
           ifelse(students == 2,
                  output <- subset(output, Student == 1),
                  output <- output))
    
    ifelse(staff == 0,
           output <- subset(output, Staff == 0),
           ifelse(staff == 2,
                  output <- subset(output, Staff == 1),
                  output <- output))
  
    ifelse(nonaffiliates == 0,
           output <- subset(output, NonAffiliate == 0),
           ifelse(nonaffiliates == 2,
                  output <- subset(output, NonAffiliate == 1),
                  output <- output))
    
    ifelse(medicalPermits == 0,
           output <- subset(output, Medical == 0),
           ifelse(medicalPermits == 2,
                  output <- subset(output, Medical == 1),
                  output <- output))
    
    ifelse(returnedPermits == 0,
           output <- subset(output, Return == 0),
           ifelse(returnedPermits == 2,
                  output <- subset(output, Return == 1),
                  output <- output))
    
    ifelse(onCampus == 0,
           output <- subset(output, Resident == ""),
           ifelse(onCampus == 2,
                  output <- subset(output, Resident != ""),
                  output <- output))
    
    ifelse(limited == 0,
           output <- subset(output, Limited == 0),
           ifelse(limited == 2,
                  output <- subset(output, Limited == 1),
                  output <- output))
    
    ifelse(scratch == 0,
           output <- subset(output, Scratcher == 0),
           ifelse(scratch == 2,
                  output <- subset(output, Scratcher == 1),
                  output <- output))
    
  return(output)
}



# We'll tag some input variables as 0, 1, or 2.
# 0: Exclude this subset
# 1: Include this subset
# 2: Only include this subset

#--------

ui <- fluidPage(
  
  # App title 
  titlePanel("UCSC Parking Sales Data: Summaries"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(width = 3,
                 print("Leave check boxes blank for full data set, except for Limited and Scratch-off Permits."),
                 
                 radioButtons("Returns",
                             "Exclude returns?",
                             c("Yes" = TRUE,
                               "No" = FALSE),
                             selected = TRUE),
                 
                 # Input: Selector for data set
                 checkboxGroupInput(inputId = "PType",
                                    label = "Permit Type",
                                    choices = c("A Permits" = "A",
                                                "B Permits" = "B",
                                                "C Permits" = "C",
                                                "R Permits" = "R",
                                                "N Permits" = "N",
                                                "Motorcycle Permits" = "MC",
                                                "Summer Session Permits" = "SS",
                                                "Van Pool Permits" = "VP",
                                                "Bus Passes" = "BP"),
                                    selected = c("A","B","C","R")),
                 
                 checkboxGroupInput(inputId = "LOS",
                                    label=" Include Limited And/Or Scratch-off Permits?",
                                    choices = c("Limited Days" = "Limited",
                                                "Scratch-offs" = "Scratch"),
                                    selected = "Limited"),
                 
                 checkboxGroupInput(inputId = "PDuration",
                                    label = "Permit Duration",
                                    choices = c("Daily Permits" = "Day",
                                                "Weekly Permits" = "Week",
                                                "Monthly Permits" = "Month",
                                                "Quarterly Permits" = "Quarter",
                                                "Annual Permits" = "Annual",
                                                "Academic Permits" = "Academic",
                                                "Multiyear Permits" = "Multiyear"),
                                    selected = FALSE),
                 
                 checkboxGroupInput(inputId = "PBuyer",
                                    label = "Permit Designation",
                                    choices = c("Student" = "Student",
                                                "Staff & Faculty" = "Staff/Fac",
                                                "NonAffiliate" = "NonAff"),
                                    selected = c("Student","Staff/Fac","NonAff")),
                 
                 checkboxGroupInput(inputId = "Resident",
                                    label = "Residency On Campus",
                                    choices = c("Resident" = "Res",
                                                "Not a Resident" = "Not"),
                                    selected = FALSE),
                 
                 # Input: Date Range
                 dateRangeInput(inputId = "dateRange",
                                label= "Time Frame to consider",
                                min = "2018-06-01",
                                max = "2019-07-1",
                                start = "2018-06-01",
                                end = "2019-07-01",
                                format = "mm/dd/yy"
                 ),
                 
                 # Input: Checkboxes for specific data
                 checkboxGroupInput(inputId = "Outputs",
                                    label = "Outputs",
                                    choices = c("Total Quantity" = "Qty",
                                                "Permit Code Counts" = "Pcc",
                                                "Faculty/Staff or Student Percentages" = "Per",
                                                "Quantity Sold Summary" = "Qss",
                                                "Purchase Method Counts" = "Pmc",
                                                "Class Level" = "Clv"),
                                    selected = c("Qty","Pcc","Per","Qss","Pmc","Clv")),
                 
                 print("If you receive an error, there is no data for the specific subset you entered. Try another combination of inputs.")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Outputs the plot
      column(5, dataTableOutput("summary"))
    )
  )
)

#--------

server <- function(input, output) {
  
  
  output$summary <- renderDataTable({
    
    POS <- POS.Subset(
      startDate = as.Date(input$dateRange[1]), 
      endDate = as.Date(input$dateRange[2]), 
      permitList = input$PType, 
      duration = input$PDuration,
      students = ifelse("Student" %in% input$PBuyer | length(input$PBuyer) == 0, 1, 0), 
      staff = ifelse("Staff/Fac" %in% input$PBuyer | length(input$PBuyer) == 0, 1, 0), 
      nonaffiliates = ifelse("NonAff" %in% input$PBuyer | length(input$PBuyer) == 0, 1, 0),
      onCampus = ifelse(
        ("Res" %in% input$Resident & "Not" %in% input$Resident) | (length(input$Resident) == 0), 
        1, 
        ifelse(
          "Res" %in% input$Resident, 
          2,
          0)
        ), 
      limited = ifelse("Limited" %in% input$LOS, 1, 0),
      scratch = ifelse("Scratch" %in% input$LOS, 1, 0),
      returnedPermits = ifelse(input$Returns == TRUE,1,0)
      )
    
    #c("Qty","Pcc","Qss","Pmc","Clv")
    
    outputDF <- data.frame()[1:7,]
    
    if("Qty" %in% input$Outputs) {
      outputDF$Quantity <- c(sum(POS$RealQty),"","","","","","")
    }
    
    if("Pcc" %in% input$Outputs) {
      outputDF$Permit.Codes <- summary(subset(POS, select = PermitCode))
    }
    
    if("Per" %in% input$Outputs) {
      outputDF$Fac.Staff.Percent <- 
        c(
          paste(as.numeric(strsplit(strsplit(summary(subset(POS, select = Staff))[4],":")[[1]][2]," ")[[1]][1])*100,"%",sep=""), #HAHA HOLY FUCK THIS IS SOME MESSY ASS CODE
          "","","","","","") # What it does: 1. Only look at the Staff vector of the data. 2. Take a summary of that column. 3. Select the Mean from that summary. 4. Since the Mean comes as a string, split that string into two, divided by the semicolon. 5. Trim off the spaces at the end. 6. Convert the remaining number to numeric, and multiply it by 100. 7. Add a percentage sign at the end, and call it a day.
      outputDF$Student.Percent <- 
        c(
          paste(as.numeric(strsplit(strsplit(summary(subset(POS, select = Student))[4],":")[[1]][2]," ")[[1]][1])*100,"%",sep=""),
          "","","","","","")
    }
    
    if("Qss" %in% input$Outputs) {
      outputDF$Quantity.Summary <- c(summary(subset(POS, select = RealQty)),"")
    }
    
    if("Pmc" %in% input$Outputs) {
      outputDF$Purchase.Method <- summary(subset(POS, select = PaymentType))
    }
    
    if ("Clv" %in% input$Outputs) {
      outputDF$Class.Level <- c(summary(subset(POS, select = Class)),"")
    }
    
    outputDF
  })}

shinyApp(ui = ui, server = server)

