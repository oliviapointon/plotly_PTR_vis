# --- Call relevant packages ---
library(plotly)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(withr)
library(htmlwidgets)

# --- Data reading and preparation ---

# Read in the data
Pos_Test_Data <- read.csv("Positive Test Rate by State.csv", header=TRUE, fileEncoding = "UTF-8-BOM")

# Replace blank values of moving average with NA
Pos_Test_Data$X5.Day.Moving.Average <- ifelse(Pos_Test_Data$X5.Day.Moving.Average=="", NA, Pos_Test_Data$X5.Day.Moving.Average)

# Convert date to date variable using lubridate package
Pos_Test_Data$Date <- Pos_Test_Data$Date %>%
  dmy()

# Convert the data from wide to narrow. This format ensures the 'variable' column can be used to define the different plot series on the legend
Pos_Test_Data_Narrow <- melt(Pos_Test_Data, id.vars=c("State", "Date"))

# --- Calculating axis ranges for each state ---

# Calculating transformation coefficients for ACT

# Apply a linear transformation to convert both y axes
# Using the formula y = a + bx, where y and x are vectors and a and b are scalars
# i.e. b = (y2-y1/x2-x1) and a = y1 - (b*x1)
# But we set x1 and y1 to 0 since that's where we want both our vertical axes to start
y2_ACT <- Pos_Test_Data_Narrow %>%
  subset(variable=="Positive.Test.Rate" & State=="ACT") %>%
  select(value) %>%
  max()
x2_ACT <- Pos_Test_Data_Narrow %>%
  subset(variable=="Tests" & State=="ACT") %>%
  select(value) %>%
  max()
b_ACT <- y2_ACT / x2_ACT

# Calculating transformation coefficients for NSW
y2_NSW <- Pos_Test_Data_Narrow %>%
  subset(variable=="Positive.Test.Rate" & State=="NSW") %>%
  select(value) %>%
  max()
x2_NSW <- Pos_Test_Data_Narrow %>%
  subset(variable=="Tests" & State=="NSW") %>%
  select(value) %>%
  max()
b_NSW <- y2_NSW / x2_NSW

# Also calculate min and max of date axis
# Write a function that takes the State and function (min or max) as inputs
date.axis.bounds.calc <- function(StateName, fn){
  if (fn=="max") {
    (Pos_Test_Data_Narrow %>%
       subset(State==StateName))$Date %>%
      max()   
  }
  else if (fn=="min") {
    (Pos_Test_Data_Narrow %>%
       subset(State==StateName))$Date %>%
      min()   
  }
  # If any function other than min or max has been entered, throw an error
  else {
    stop('Invalid function name entered.')
  }
}

mindate_ACT <- date.axis.bounds.calc("ACT", "min")
maxdate_ACT <- date.axis.bounds.calc("ACT", "max")
mindate_NSW <- date.axis.bounds.calc("NSW", "min")
maxdate_NSW <- date.axis.bounds.calc("NSW", "max")

# Apply the linear transformations to the "Tests" column, and append this data to the dataframe as "Tests.Scaled"
Pos_Test_Data_Narrow <- rbind(Pos_Test_Data_Narrow,
                              Pos_Test_Data_Narrow %>%
    subset(variable=="Tests" & State=="ACT") %>%
    mutate(variable="Tests.Scaled") %>%
    mutate(value=value* b_ACT))

Pos_Test_Data_Narrow <- rbind(Pos_Test_Data_Narrow,
                              Pos_Test_Data_Narrow %>%
                                subset(variable=="Tests" & State=="NSW") %>%
                                mutate(variable="Tests.Scaled") %>%
                                mutate(value=value* b_NSW))

# --- Creating initial ggplot ---

# Note that all 6 data series are added to the same plot
scatter_plot <- ggplot(Pos_Test_Data_Narrow, aes(x=Date, y=value)) +
  ylab("Positive Test Rate") +
  scale_y_continuous(labels=scales::percent, sec.axis = sec_axis(~./ b_ACT, name = "Number of Tests Administered")) +
  ggtitle("Positive Test Rate") +
  # Number of Tests ACT (Scaled)
  geom_col(data=Pos_Test_Data_Narrow[Pos_Test_Data_Narrow$variable=="Tests.Scaled"&Pos_Test_Data_Narrow$State=="ACT",], aes(fill=variable, text=paste0("<b> Date: </b>", Date, "<br>", "<b> Tests: </b>", format(value/b_ACT, big.mark=",", scientific=FALSE)))) +
  # Number of Tests NSW (Scaled)
  geom_col(data=Pos_Test_Data_Narrow[Pos_Test_Data_Narrow$variable=="Tests.Scaled"&Pos_Test_Data_Narrow$State=="NSW",], aes(fill=variable, text=paste0("<b> Date: </b>", Date, "<br>", "<b> Tests: </b>", format(value/b_NSW, big.mark=",", scientific=FALSE)))) +
  # Positive Test Rate ACT
  geom_point(data=Pos_Test_Data_Narrow[Pos_Test_Data_Narrow$variable=="Positive.Test.Rate"&Pos_Test_Data_Narrow$State=="ACT",], aes(colour=variable, alpha=0.6, shape=variable, text=paste0("<b> Date: </b>", Date, "<br>", "<b> Positive Test Rate: </b>", scales::percent(value, accuracy=0.01))), shape=18, size=3) +
  # Positive Test Rate NSW
  geom_point(data=Pos_Test_Data_Narrow[Pos_Test_Data_Narrow$variable=="Positive.Test.Rate"&Pos_Test_Data_Narrow$State=="NSW",], aes(colour=variable, alpha=0.6, shape=variable, text=paste0("<b> Date: </b>", Date, "<br>", "<b> Positive Test Rate: </b>", scales::percent(value, accuracy=0.01))), shape=18, size=3) +
  # 5 Day Moving Average ACT
  geom_line(data=Pos_Test_Data_Narrow[Pos_Test_Data_Narrow$variable=="X5.Day.Moving.Average"&Pos_Test_Data_Narrow$State=="ACT",], aes(colour=variable, text=paste0("<b> Date: </b>", Date, "<br>", "<b> 5 Day Moving Average Positive Test Rate: </b>", scales::percent(value, accuracy=0.01)), group=variable), size=1.5) +
  # 5 Day Moving Average NSW
  geom_line(data=Pos_Test_Data_Narrow[Pos_Test_Data_Narrow$variable=="X5.Day.Moving.Average"&Pos_Test_Data_Narrow$State=="NSW",], aes(colour=variable, text=paste0("<b> Date: </b>", Date, "<br>", "<b> 5 Day Moving Average Positive Test Rate: </b>", scales::percent(value, accuracy=0.01)), group=variable), size=1.5) +
  theme_minimal() +
  scale_color_manual(labels=c("Positive.Test.Rate" = "Daily Positive Test Rate (LHS)",
                              "X5.Day.Moving.Average" = "5 Day Moving Average Positive Test Rate (LHS)"),
                     values=c("Positive.Test.Rate" = "dodgerblue", "X5.Day.Moving.Average" = "dodgerblue")) +
  guides(shape="none",
        colour=guide_legend(override.aes = list(shape = c(18, NA), linetype=c("blank", "solid")))) +
  scale_fill_manual(values = c("#D9D9D9", "#D9D9D9"), labels = c("Daily Tests Administered (RHS)", "Daily Tests Administered (RHS)")) +
  labs(fill='', colour='') +
  theme(legend.position = "bottom")
plot(scatter_plot)

# --- Parsing ggplot output to ggplotly to create an interactive version of the scatterplot ---
# Add a buffer when passing values to the range() function. This ensures the edges of the series aren't cut off.
scatter_plot_interactive <- ggplotly(scatter_plot, tooltip="text") %>%
  add_lines(name="", opacity=0, x =~Date, y=~(value/b_ACT), yaxis="y2", visible=FALSE, showlegend=FALSE, inherit=FALSE, color=NULL) %>%
  layout (
    yaxis2 = list(side = "right", title = list(text="Number of Tests Administered",font=list(size=14)) , linewidth = 0, overlaying = "y", showgrid=FALSE, range=c(0,x2_ACT+100),
                  tickfont=list(size=14), showline=FALSE, tickformat=",.1r"),
    legend=list(x=1.15),
    yaxis=list(range=c(0,y2_ACT+0.001),title=list(text="Positive Test Rate", font=list(size=14)),tickformat=".2%", tick0=0, dtick=0.002, tickmode="linear",
               tickfont=list(size=14)),
    xaxis=list(tickmode="array", tickvals=list(18840, 18854, 18868, 18882), ticktext=list("Aug 01", "Aug 15", "Aug 29", "Sep 12"), tickfont=list(size=14),
               showline=TRUE, linecolor="#A9a9a9", tickcolor="#A9a9a9", showspikes=TRUE, ticks="outside", ticklen=5, tickwidth=1, title=list(text="Date", font=list(size=14)),
               showgrid=FALSE, range=c(as.numeric(mindate_ACT)-2,as.numeric(maxdate_ACT)+2)),
    title=list(font=list(size=20), text="Positive Test Rate - ACT", x=0, xref='paper'),
    margin=list(l=80, r=80, t=50, b=50)
)

# Update legend text
# List element 1: ACT tests
scatter_plot_interactive$x$data[[1]]$name <- "Number of Tests (RHS)"
# List element 2: NSW tests
scatter_plot_interactive$x$data[[2]]$name <- "Number of Tests (RHS)"
# List element 3: ACT positive test rate
scatter_plot_interactive$x$data[[3]]$name <- "Daily Positive Test Rate (LHS)"
# List element 4: NSW positive test rate
scatter_plot_interactive$x$data[[4]]$name <- "Daily Positive Test Rate (LHS)"
# List element 5: ACT 5 day moving average
scatter_plot_interactive$x$data[[5]]$name <- "5 Day Moving Average <br> Positive Test Rate (LHS)"
# List element 6: NSW 5 day moving average
scatter_plot_interactive$x$data[[6]]$name <- "5 Day Moving Average <br> Positive Test Rate (LHS)"

# We only want one state's data to show at a time. Initiate the plot to the ACT. 
# That is, the series associated with ACT will be initially visible, and the series associated with other states will be initially invisible.
scatter_plot_interactive$x$data[[1]]$visible <- TRUE
scatter_plot_interactive$x$data[[2]]$visible <- FALSE
scatter_plot_interactive$x$data[[3]]$visible <- TRUE
scatter_plot_interactive$x$data[[4]]$visible <- FALSE
scatter_plot_interactive$x$data[[5]]$visible <- TRUE
scatter_plot_interactive$x$data[[6]]$visible <- FALSE
scatter_plot_interactive$x$data[[1]]$showlegend <- TRUE
scatter_plot_interactive$x$data[[2]]$showlegend <- FALSE
scatter_plot_interactive$x$data[[3]]$showlegend <- TRUE
scatter_plot_interactive$x$data[[4]]$showlegend <- FALSE
scatter_plot_interactive$x$data[[5]]$showlegend <- TRUE
scatter_plot_interactive$x$data[[6]]$showlegend <- FALSE

# Plotly gets confused by the 'alpha' aesthetic from ggplot2. The opacity displays correctly, but there is also an additional annotation added to the chart. Let's remove this
# by setting to null.
scatter_plot_interactive$x$layout$annotations[[1]] <- NULL

# --- Adding a drop down menu to toggle the different state's data ---
scatter_plot_interactive_menu <- scatter_plot_interactive %>% layout(
  updatemenus = list(
    list(
      # This determines the x- and y-coordinates of where to place the dropdown
      x = 1.35,
      y = 0.5,
      buttons = list(
        # List the actions the ACT button should perform
        # This includes toggling the visible/invisible status of each series
        # And rescaling the axes to fit the data snugly
        list(method = "update",
             args = list(list("visible"=list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                              "showlegend"=list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)),
                         list("yaxis2"=list(side = "right", title = list(text="Number of Tests Administered",font=list(size=14)) , linewidth = 0, overlaying = "y",
                                            showgrid=FALSE,range=c(0,x2_ACT+100), tickfont=list(size=14), tickformat=",.1r"),
                              "yaxis"=list(range=c(0,y2_ACT+0.001),title=list(text="Positive Test Rate", font=list(size=14)),tickformat=".2%", tickfont=list(size=14)),
                              "xaxis"=list(range=c(mindate_ACT,maxdate_ACT),tickmode="array", tickvals=list(18840, 18854, 18868, 18882),
                                           ticktext=list("Aug 01", "Aug 15", "Aug 29", "Sep 12"), linecolor="#A9a9a9", tickcolor="#A9a9a9", showspikes=TRUE, ticklen=5,
                                           title=list(text="Date", font=list(size=14)),tickfont=list(size=14)),
                              "title"=list(font=list(size=20), text="Positive Test Rate - ACT", x=0, xref='paper'),
                              "margin"=list(l=80, r=80, t=50, b=50))),
             label = "ACT"),
        # Similarly, list the actions the NSW button should perform
        list(method = "update",
             args = list(list("visible"=list(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
                              "showlegend"=list(FALSE, TRUE,FALSE,TRUE,FALSE,TRUE)),
                         list("yaxis2"=list(side = "right", title = list(text="Number of Tests Administered",font=list(size=14)), linewidth = 0, overlaying = "y",
                                            showgrid=FALSE, range=c(0,x2_NSW+100), tickfont=list(size=14), tickformat=",.2s"),
                              "yaxis"=list(range=c(0,y2_NSW+0.001),title=list(text="Positive Test Rate", font=list(size=14)),tickformat=".2%", tickfont=list(size=14)),
                              "xaxis"=list(range=c(mindate_NSW,maxdate_NSW),tickmode="array", tickvals=list(18840, 18854, 18868, 18882),
                                           ticktext=list("Aug 01", "Aug 15", "Aug 29", "Sep 12"), linecolor="#A9a9a9", tickcolor="#A9a9a9", showspikes=TRUE, ticklen=5,
                                           title=list(text="Date", font=list(size=14)),tickfont=list(size=14)),
                              "title"=list(font=list(size=20), text="Positive Test Rate - NSW", x=0, xref='paper'),
                              "margin"=list(l=80, r=80, t=50, b=50))),
             label = "NSW")))
     ),
  # Add a label above the dropdown
  annotations=list(text="<b> Choose State: </b>", x=1.4, y=0.55, xref='paper', yref='paper', showarrow=FALSE)
)

# Ensure the scatterplot is the correct dimensions to render nicely
scatter_plot_interactive_menu$width = 870
scatter_plot_interactive_menu$height = 545

# Print the output to the plotting window
print(scatter_plot_interactive_menu)

# Output a HTML file in the working directory (see plot.html in main directory of repo to see the output)
with_dir(getwd(), saveWidget(scatter_plot_interactive_menu, "plot.html"))

# --- Optional: Upload the output to plotly chart studio for HTML embedding ---
# Note: you must set up your API key for this to work
plotly_POST(
  x = last_plot(),
  file = "COVID Positive Test Rate Plot",
  fileopt = "overwrite",
  sharing = c("public"),
  world_readable=TRUE
)
