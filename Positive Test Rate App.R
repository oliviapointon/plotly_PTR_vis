# Set the working directory
setwd("~/~ COVID Analysis ~")

# Call relevant packages
library(plotly)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(htmlwidgets)

# Read in the data
Pos_Test_Data <- read.csv("Positive Test Rate by State.csv", header=TRUE, fileEncoding = "UTF-8-BOM")

# Replace blank values of moving average with NA
Pos_Test_Data$X5.Day.Moving.Average <- ifelse(Pos_Test_Data$X5.Day.Moving.Average=="", NA, Pos_Test_Data$X5.Day.Moving.Average)

# Replace Date with a date variable
Pos_Test_Data$Date <- dmy(Pos_Test_Data$Date)

# Convert the data from wide to narrow, to be able to populate the legend
Pos_Test_Data_Narrow <- melt(Pos_Test_Data, id.vars=c("State", "Date"))

# --- Calculating axis ranges for each state ---

ACT_Data <- Pos_Test_Data_Narrow %>%
    subset(State=="ACT")

NSW_Data <- Pos_Test_Data_Narrow %>%
  subset(State=="NSW")


#Calculating transformation coefficients for ACT
#Apply a linear transformation to convert both y axes
#Using the formula b = (y2-y1/x2-x1) and a = y1 - (b*x1)
#But we set x1 and y1 to 0 since that's where we want both our vertical axes to start
y2_ACT <- ACT_Data %>%
  subset(variable=="Positive.Test.Rate") %>%
  select(value) %>%
  max()
x2_ACT <- ACT_Data %>%
  subset(variable=="Tests") %>%
  select(value) %>%
  max()
b_ACT <- y2_ACT / x2_ACT

#Also calculate min and max of date axis
mindate_ACT <- ACT_Data$Date %>%
  min()
maxdate_ACT <- ACT_Data$Date %>%
  max()
mindate_NSW <- NSW_Data$Date %>%
  min()
maxdate_NSW <- NSW_Data$Date %>%
  max()

#Calculating transformation coefficients for NSW
#y1_NSW <- NSW_Data %>%
#  subset(variable=="Positive.Test.Rate") %>%
#  select(value) %>%
#  min()
y2_NSW <- NSW_Data %>%
  subset(variable=="Positive.Test.Rate") %>%
  select(value) %>%
  max()
x2_NSW <- NSW_Data %>%
  subset(variable=="Tests") %>%
  select(value) %>%
  max()
b_NSW <- y2_NSW / x2_NSW
#b_NSW <- (y2_NSW - y1_NSW) / (x2_NSW - x1_NSW)
#a_NSW <- y1_NSW - b_NSW * x1_NSW

#Adding scaled test data back to dataframe
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
                                #mutate(value=value* b_NSW + a_NSW))

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

#Interactive version
scatter_plot_interactive <- ggplotly(scatter_plot, tooltip="text") %>%
  add_lines(name="", opacity=0, x =~Date, y=~(value/b_ACT), yaxis="y2", visible=FALSE, showlegend=FALSE, inherit=FALSE, color=NULL) %>%
  layout (
    yaxis2 = list(side = "right", title = list(text="Number of Tests Administered",font=list(size=14)) , linewidth = 0, overlaying = "y", showgrid=FALSE, range=c(0,x2_ACT+100), tickfont=list(size=14), showline=FALSE, tickformat=",.1r"),
    legend=list(x=1.15),
    yaxis=list(range=c(0,y2_ACT+0.001),title=list(text="Positive Test Rate", font=list(size=14)),tickformat=".2%", tick0=0, dtick=0.002, tickmode="linear", tickfont=list(size=14)),
    xaxis=list(tickmode="array", tickvals=list(18840, 18854, 18868, 18882), ticktext=list("Aug 01", "Aug 15", "Aug 29", "Sep 12"), tickfont=list(size=14), showline=TRUE, linecolor="#A9a9a9", tickcolor="#A9a9a9", showspikes=TRUE, ticks="outside", ticklen=5, tickwidth=1, title=list(text="Date", font=list(size=14)), showgrid=FALSE),
    title=list(font=list(size=20), text="Positive Test Rate - ACT", x=0, xref='paper'),
    margin=list(l=80, r=80, t=50, b=50)
)
#range=c(mindate_ACT,maxdate_ACT),

#Update legend values
#List element 1: ACT tests
scatter_plot_interactive$x$data[[1]]$name <- "Number of Tests (RHS)"
#List element 2: NSW tests
scatter_plot_interactive$x$data[[2]]$name <- "Number of Tests (RHS)"
#List element 3: ACT positive test rate
scatter_plot_interactive$x$data[[3]]$name <- "Daily Positive Test Rate (LHS)"
#List element 4: NSW positive test rate
scatter_plot_interactive$x$data[[4]]$name <- "Daily Positive Test Rate (LHS)"
#List element 5: ACT 5 day moving average
scatter_plot_interactive$x$data[[5]]$name <- "5 Day Moving Average <br> Positive Test Rate (LHS)"
#List element 6: NSW 5 day moving average
scatter_plot_interactive$x$data[[6]]$name <- "5 Day Moving Average <br> Positive Test Rate (LHS)"

#Initiate plot to ACT
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

#Add a buffer either side of x axis
scatter_plot_interactive$x$layout$xaxis$range <- c(as.numeric(mindate_ACT)-2,as.numeric(maxdate_ACT)+2)

#Remove 'alpha' annotation
scatter_plot_interactive$x$layout$annotations[[1]] <- NULL

#Drop down menu for different state's data
scatter_plot_interactive_menu <- scatter_plot_interactive %>% layout(
  updatemenus = list(
    list(
      x = 1.35,
      y = 0.5,
      buttons = list(
        list(method = "update",
             args = list(list("visible"=list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                              "showlegend"=list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)),
                         list("yaxis2"=list(side = "right", title = list(text="Number of Tests Administered",font=list(size=14)) , linewidth = 0, overlaying = "y", showgrid=FALSE,range=c(0,x2_ACT+100), tickfont=list(size=14), tickformat=",.1r"),
                              "yaxis"=list(range=c(0,y2_ACT+0.001),title=list(text="Positive Test Rate", font=list(size=14)),tickformat=".2%", tickfont=list(size=14)),
                              "xaxis"=list(range=c(mindate_ACT,maxdate_ACT),tickmode="array", tickvals=list(18840, 18854, 18868, 18882), ticktext=list("Aug 01", "Aug 15", "Aug 29", "Sep 12"), linecolor="#A9a9a9", tickcolor="#A9a9a9", showspikes=TRUE, ticklen=5, title=list(text="Date", font=list(size=14)),tickfont=list(size=14)),
                              "title"=list(font=list(size=20), text="Positive Test Rate - ACT", x=0, xref='paper'),
                              "margin"=list(l=80, r=80, t=50, b=50))),
             label = "ACT"),
        list(method = "update",
             args = list(list("visible"=list(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
                              "showlegend"=list(FALSE, TRUE,FALSE,TRUE,FALSE,TrRUE)),
                         list("yaxis2"=list(side = "right", title = list(text="Number of Tests Administered",font=list(size=14)) , linewidth = 0, overlaying = "y", showgrid=FALSE,range=c(0,x2_NSW+100), tickfont=list(size=14), tickformat=",.2s"),
                              "yaxis"=list(range=c(0,y2_NSW+0.001),title=list(text="Positive Test Rate", font=list(size=14)),tickformat=".2%", tickfont=list(size=14)),
                              "xaxis"=list(range=c(mindate_NSW,maxdate_NSW),tickmode="array", tickvals=list(18840, 18854, 18868, 18882), ticktext=list("Aug 01", "Aug 15", "Aug 29", "Sep 12"), linecolor="#A9a9a9", tickcolor="#A9a9a9", showspikes=TRUE, ticklen=5, title=list(text="Date", font=list(size=14)),tickfont=list(size=14)),
                              "title"=list(font=list(size=20), text="Positive Test Rate - NSW", x=0, xref='paper'),
                              "margin"=list(l=80, r=80, t=50, b=50))),
             label = "NSW")))
     ),
  annotations=list(text="<b> Choose State: </b>", x=1.4, y=0.55, xref='paper', yref='paper', showarrow=FALSE)
)

print(scatter_plot_interactive_menu)

#Output a HTML file in the working directory
withr::with_dir(getwd(), htmlwidgets::saveWidget(scatter_plot_interactive_menu, "plot.html"))
