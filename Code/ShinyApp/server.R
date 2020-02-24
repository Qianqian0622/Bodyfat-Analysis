library(shiny)
library(ggplot2)

CatchError = function(expr){
  err = NULL
  tryCatch(expr, error = function(e){
    err <<- e
    NULL})
  return (err)
}

dat<-read.csv("./data/BodyFat_new.csv",header=T)
lm1 = lm(dat$BODYFAT ~ WEIGHT + ABDOMEN, dat)
lm2 = lm(dat$BODYFAT ~ log(WEIGHT / 2.204623) + log((HEIGHT / 0.3937008) ^ 2), dat)

server = shinyServer(function(input, output){
  pred <- reactive({
    validate(
      need(input$abdomen1 > 0, "Please input a valid abdomen."),
      need(input$weight1 > 0, "Please input a valid weight."),
      need(input$abdomen2 > 0, "Please input a valid abdomen."),
      need(input$weight2 > 0, "Please input a valid weight."),
      need(input$height1 > 0, "Please input a valid height."),
      need(input$height2 > 0, "Please input a valid height.")
    )
    if (input$metric == "The Metric System"){
      we = input$weight1 * 2.204623
      ab = input$abdomen1
      h = input$height1 * 0.3937008
    }else{
      ab = input$abdomen2 * 2.54
      we = input$weight2
      h = input$height2
    }
    if (input$model == "Weight + Height"){
      pre.bodyfat = round(predict(lm2, data.frame(WEIGHT = we, HEIGHT = h), interval="predict"), 1)
    }else{
      pre.bodyfat = round(predict(lm1, data.frame(WEIGHT = we, ABDOMEN = ab), interval="predict"), 1)
    }
    validate(
      need(pre.bodyfat[1] >= 2, "The number you input maybe invalid, since your body fat is smaller than 0.")
    )
    pre.bodyfat
  })
  #observeEvent(input$submit,{
    output$ref = renderTable({
      Desc = c("Essential Fat", "Athletes", "Fitness", "Average", "Obese")
      Bodyfat = c("2-5%", "6-13%", "14-17%", "18-24%", "25+%")
      Desc = rev(Desc)
      Bodyfat = rev(Bodyfat)
      Status = rep(" ", 5)
      if (is.null(CatchError(pred())) == TRUE){
        if (pred()[1] < 6){
          Status[5] = "You are here."
        }else if(pred()[1] < 14){
          Status[4] = "You are here."
        }else if(pred()[1] < 18){
          Status[3] = "You are here."
        }else if(pred()[1] < 25){
          Status[2] = "You are here."
        }else if(is.na(pred()[1]) != TRUE){
          Status[1] = "You are here."
        }
        data.frame(Desc, Bodyfat, Status)
      }else{
        data.frame(Desc, Bodyfat)
      }
    })
    
    output$conclusion = renderText({
      if (pred()[1] < 6){
        paste("You are in Essential Fat Category, and the body fat proportion is: ", 
              pred()[1], '%.', sep = "")
      }
      else if(pred()[1] < 14){
        paste("You are in Athletes Category, and the body fat proportion is: ", 
              pred()[1], '%.', sep = "")
      }
      else if(pred()[1] < 18){
        paste("You are in Fitness Category, and the body fat proportion is: ", 
              pred()[1], '%.', sep = "")
      }
      else if(pred()[1] < 25){
        paste("You are in Average Category, and the body fat proportion is: ", 
              pred()[1], '%.', sep = "")
      }
      else{paste("You are in Obese Category, and the body fat proportion is: ", 
                 pred()[1], '%.', sep = "")}
    })
    
    output$plot = renderPlot({
      if (is.null(CatchError(pred())) == TRUE){
        ggplot(dat, aes(x = 1, y = BODYFAT)) + geom_violin() + geom_boxplot(x = 0, width = 0.2) +
          annotate("point", x = 1, y = pred()[1], colour = "red", size = 5) + 
          geom_errorbar(aes(ymin = pred()[2], ymax = pred()[3]), width = 0.2) +
          geom_rect(data=data.frame(ymin = c(-Inf, 6, 14, 18, 25),
                                    ymax = c(6, 14, 18, 25, Inf),
                                    xmin = -Inf,
                                    xmax = Inf,
                                    col =letters[1:5]),
                    inherit.aes=F, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill= col),
                    show.legend = FALSE) +
          scale_fill_manual('Region',
                            values = adjustcolor(c("#024b30","#2ECC71",
                                                   "#AFB42B","#F4D03F","#F44336"),
                                                 alpha.f = 0.35)) +
          annotate("text", y = c(3,10,16,21.5,35), x = 1.2, size = 5, label = c("Essential", "Athletes", "Fitness", "Average","Obese"))
      }
      else{
        ggplot(dat, aes(x = 1, y = BODYFAT)) + geom_violin() + geom_boxplot(x = 0, width = 0.1) +
          geom_rect(data=data.frame(ymin = c(-Inf, 6, 14, 18, 25),
                                    ymax = c(6, 14, 18, 25, Inf),
                                    xmin = -Inf,
                                    xmax = Inf,
                                    col =letters[1:5]),
                    inherit.aes=F, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill= col),
                    show.legend = FALSE) +
          scale_fill_manual('Region',
                            values = adjustcolor(c("#024b30","#2ECC71",
                                                   "#AFB42B","#F4D03F","#F44336"),
                                                 alpha.f = 0.35)) +
          annotate("text", y = c(3,10,16,21.5,35), x = 1.2, size = 5, label = c("Essential", "Athletes", "Fitness", "Average","Obese"))
      }})
  #})
})




