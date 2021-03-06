library(shiny)

ui <- fluidPage(
    titlePanel("Probability Distribution Functions"),
    sidebarPanel(
        selectInput(
            inputId = "selection",
            label = "Choose a distribution: ",
            choices = c("Beta", "Cauchy", "Chi Square", "Exponential", "Gamma",
                        "Gosset (T)", "Logistic", "Normal", "Snedecor (F)",
                        "Weibull", "Lindley", "Uniform", "Binomial", "Geometric",
                        "Hypergeometric", "Poisson", "Negative Binomial", "Discrete Lindley", "ZIP")),
        checkboxInput(inputId = "MEAN", label = "Show first mmoment", value = TRUE),
        conditionalPanel("input.selection == 'Weibull'",
                         sliderInput(inputId = "weibull.shape", label = "Shape",
                                     min = 0.5, max = 8, value = 1,
                                     step = 0.5),
                         sliderInput(inputId = "weibull.scale", label = "Scale",
                                     min = 0.5, max = 8, value = 1)),
        conditionalPanel("input.selection == 'Chi Square'",
                         sliderInput(inputId = "chisq.df", label = "DF",
                                     min = 1, max = 5, value = 1,
                                     step = 0.5)),
        conditionalPanel("input.selection == 'Normal'",
                         sliderInput(inputId = "norm.mean", label = "Mean",
                                     min = -4, max = 4, value = 0,
                                     step = 0.5),
                         sliderInput(inputId = "norm.sd", label = "SD",
                                     min = 0.1, max = 3, value = 1,
                                     step = 0.1)),
        conditionalPanel("input.selection == 'Uniform'",
                         sliderInput(inputId = "unif.min", label = "Min.",
                                     min = -3, max = 0, value = -1,
                                     step = 0.1),
                         sliderInput(inputId = "unif.max", label = "Máx.",
                                     min = 0.1, max = 3, value = 1,
                                     step = 0.1)),
        conditionalPanel("input.selection == 'Exponential'",
                         sliderInput(inputId = "exp.rate", label = "Rate",
                                     min = 0.1, max = 10, value = 1,
                                     step = 0.5)),
        conditionalPanel("input.selection == 'Cauchy'",
                         sliderInput(inputId = "cauchy.location",
                                     label = "Location", min = -5, max = 5,
                                     value = 0, step = 1),
                         sliderInput(inputId = "cauchy.scale",
                                     label = "Scale", min = 0.1, max = 5,
                                     value = 1, step = 0.5)),
        conditionalPanel("input.selection == 'Snedecor (F)'",
                         sliderInput(inputId = "f.df1", label = "DF1",
                                     min = 3, max = 10, value = 3,
                                     step = 1),
                         sliderInput(inputId = "f.df2", label = "DF2",
                                     min = 2, max = 10, value = 2,
                                     step = 1),
                         sliderInput(inputId = "f.ncp", label = "NCP",
                                     min = 1, max = 10, value = 1,
                                     step = 1,)),
        conditionalPanel("input.selection == 'Gamma'",
                         sliderInput(inputId = "gamma.shape",
                                     label = "Shape", min = 0.05, max = 5,
                                     value = 2, step = 0.05),
                         sliderInput(inputId = "gamma.rate", label = "Rate",
                                     min = 0.05, max = 5, value = 3,
                                     step = 0.05)),
        conditionalPanel("input.selection == 'Beta'",
                         sliderInput(inputId = "beta.shape1",
                                     label = "Shape 1",
                                     min = 1, max = 5, value = 2,
                                     step = 0.05),
                         sliderInput(inputId = "beta.shape2",
                                     label = "Shape 2",
                                     min = 1, max = 5, value = 3,
                                     step = 0.05),
                         sliderInput(inputId = "beta.ncp", label = "NCP",
                                     min = 0, max = 10, value = 0,
                                     step = 1)),
        conditionalPanel("input.selection == 'Logistic'",
                         sliderInput(inputId = "logis.location",
                                     label = "Location", min = -2, max = 2,
                                     value = 0, step = 0.05),
                         sliderInput(inputId = "logis.scale",
                                     label = "Scale", min = 1, max = 5,
                                     value = 1, step = 0.5)),
        conditionalPanel("input.selection == 'Gosset (T)'",
                         sliderInput(inputId = "t.df", label = "DF", min = 1,
                                     max = 20, value = 1, step = 1),
                         sliderInput(inputId = "t.ncp", label = "NCP",
                                     min = -5,  max = 5, value = 0,
                                     step = 1)),
        conditionalPanel("input.selection == 'Binomial'",
                         sliderInput(inputId = "binom.size", label = "Size",
                                     min = 1, max = 20, value = 10,
                                     step = 1),
                         sliderInput(inputId = "binom.prob",
                                     label = "Probability", min = 0, max = 1,
                                     value = 0.5, step = 0.05)),
        conditionalPanel("input.selection == 'Poisson'",
                         sliderInput(inputId = "pois.lambda",
                                     label = "Lambda", min = 0, max = 10,
                                     value = 1, step = 1)),
        conditionalPanel("input.selection == 'Geometric'",
                         sliderInput(inputId = "geom.prob",
                                     label = "Probability", min = 0.05,
                                     max = 1, value = 0.5, step = 0.05)),
        conditionalPanel("input.selection == 'Hypergeometric'",
                         sliderInput(inputId = "hyper.m", label = "m",
                                     min = 0, max = 30, value = 5,
                                     step = 1)),
        conditionalPanel("input.selection == 'Negative Binomial'",
                         sliderInput(inputId = "nbinom.size",
                                     label = "Size",
                                     min = 1, max = 10, value = 5,
                                     step = 1),
                         sliderInput(inputId = "nbinom.prob",
                                     label = "Probability", min = 0,
                                     max = 1, value = 0.5, step = 0.05)),
        conditionalPanel("input.selection == 'Lindley'",
                         sliderInput(inputId = "theta.lindley",
                                     label = "theta",
                                     min = 0.01, max = 10, value = 1,
                                     step = 0.05)),
        conditionalPanel("input.selection == 'Discrete Lindley'",
                         sliderInput(inputId = "discrete.lindley.lambda",
                                     label = "Lambda",
                                     min = 0, max = 1, value = 0.5,
                                     step = 0.05)),
        conditionalPanel("input.selection == 'ZIP'",
                         sliderInput(inputId = "zip.lambda",
                                     label = "Lambda",
                                     min = 0, max = 20, value = 0.5,
                                     step = 0.5),
                         sliderInput(inputId = "zip.pi",
                                     label = "Pi",
                                     min = 0, max = 1, value = 0.1,
                                     step = 0.05))),
    mainPanel(
        plotOutput(outputId = "distPlot")
    ),
    hr(),
    h5(actionButton("exit", "Exit"), br(), br(), hr(),
       "Created by:"),
    tags$a("Vinicius Riffel", href = "https://github.com/vriffel")
)

server <- function(input, output) {
    funcInput <- reactive({
        par(mfrow = c(1, 2))
        switch(input$selection,
               "Weibull" = {
                   curve(dweibull(x, scale = input$weibull.scale,
                                  shape = input$weibull.shape), from = 0,
                         to = 15, xlab = "x", ylab = "y", main = "Density")
                   if (input$MEAN) {
                       exp.weibull <- function(x) {
                           return(x * dweibull(x, shape = input$weibull.shape,
                                               scale = input$weibull.scale))
                       }
                       xinter <- integrate(exp.weibull, 0, Inf)$value
                       abline(v = xinter, col = "red")
                   }
                   curve(pweibull(x, scale = input$weibull.scale,
                                  shape = input$weibull.shape), xlab = "x",
                         ylab = "y", from = 0, to = 15, main = "Distribution")
               },
               "Chi Square" = {
                   curve(dchisq(x, df = input$chisq.df), from = 0,
                         to = 15, xlab = "x", ylab = "y", main = "Density")
                   if (input$MEAN) {
                       exp.chisq <- function(x) {
                           return(x * dchisq(x, df = input$chisq.df))
                       }
                       xinter <- integrate(exp.chisq, 0, Inf)$value
                       abline(v = xinter, col = "red")
                   }
                   curve(pchisq(x, df = input$chisq.df), from = 0,
                         to = 15, xlab = "x", ylab = "y",
                         main = "Distribution")},
               "Normal" = {
                   curve(dnorm(x, mean = input$norm.mean,
                               sd = input$norm.sd), from = -7, to = 7,
                         xlab = "x", ylab = "y", main = "Density")
                   if (input$MEAN) {
                       exp.norm <- function(x) {
                           return(x * dnorm(x, mean = input$norm.mean,
                                            sd = input$norm.sd))
                       }
                       xinter <- integrate(exp.norm, -Inf, Inf)$value
                       abline(v = xinter, col = "red")
                   }
                   curve(pnorm(x, mean = input$norm.mean,
                               sd = input$norm.sd), from = -7, to = 7,
                         xlab = "x", ylab = "y", main = "Distribution")},
               "Uniform" = {
                   curve(dunif(x, min = input$unif.min,
                               max = input$unif.max), from = -4, to = 4,
                         xlab = "x", ylab = "y", main = "Density")
                   if (input$MEAN) {
                       exp.unif <- function(x) {
                           return(x * dunif(x, min = input$unif.min,
                                            max = input$unif.max))
                       }
                       xinter <- integrate(exp.unif, -Inf, Inf)$value
                       abline(v = xinter, col = "red")
                   }
                   curve(punif(x, min = input$unif.min,
                               max = input$unif.max), from = -4, to = 4,
                         xlab = "x", ylab = "y", main = "Distribution")},
               "Exponential" ={
                   curve(dexp(x, rate = input$exp.rate), from = 0,
                         to = 3, xlab = "x", ylab = "y", main = "Density")
                   if (input$MEAN) {
                       exp.exp <- function(x) {
                           return(x * dexp(x, rate = input$exp.rate))
                       }
                       xinter <- integrate(exp.exp, -Inf, Inf)$value
                       abline(v = xinter, col = "red")
                   }
                   curve(pexp(x, rate = input$exp.rate), from = 0,
                         to = 3, xlab = "x", ylab = "y", main = "Distribution")},
               "Cauchy" = {
                   curve(dcauchy(x, location = input$cauchy.location,
                                 scale = input$cauchy.scale), from = -4,
                         to = 4, xlab = "x", ylab = "y", main = "Density")
                   curve(pcauchy(x, location = input$cauchy.location,
                                 scale = input$cauchy.scale), from = -4,
                         to = 4, xlab = "x", ylab = "y", main = "Distribution")},
               "Snedecor (F)" = {
                   curve(df(x, df1 = input$f.df1, df2 = input$f.df2,
                            ncp = input$f.ncp), from = 0, to = 10,
                         xlab = "x", ylab = "y", main = "Density")
                   curve(pf(x, df1 = input$f.df1, df2 = input$f.df2,
                            ncp = input$f.ncp), from = 0, to = 10,
                         xlab = "x", ylab = "y", main = "Distribution")},
               "Gamma" = {
                   curve(dgamma(x, shape = input$gamma.shape,
                                rate = input$gamma.rate), from = 0,
                         to = 15, xlab = "x", ylab = "y", main = "Density")
                   if (input$MEAN) {
                       exp.gamma <- function(x) {
                           return(x * dgamma(x, shape = input$gamma.shape,
                                             rate = input$gamma.rate))
                       }
                       xinter <- integrate(exp.gamma, -Inf, Inf)$value
                       abline(v = xinter, col = "red")
                   }
                   curve(pgamma(x, shape = input$gamma.shape,
                                rate = input$gamma.rate), from = 0,
                         to = 15, xlab = "x", ylab = "y", main = "Distribution")},
               "Beta" = {
                   curve(dbeta(x, shape1 = input$beta.shape1,
                               shape2 = input$beta.shape2,
                               ncp = input$beta.ncp), from = 0, to = 1,
                         xlab = "x", ylab = "y", main = "Density")
                   if (input$MEAN) {
                       exp.beta <- function(x) {
                           return(x * dbeta(x, shape1 = input$beta.shape1,
                                            shape2 = input$beta.shape2,
                                            ncp = input$beta.ncp))
                       }
                       xinter <- integrate(exp.beta, -Inf, Inf)$value
                       abline(v = xinter, col = "red")
                   }
                   curve(pbeta(x, shape1 = input$beta.shape1,
                               shape2 = input$beta.shape2,
                               ncp = input$beta.ncp), from = 0, to = 1,
                         xlab = "x", ylab = "y", main = "Distribution")},
               "Logistic" = {
                   curve(dlogis(x, location = input$logis.location,
                                scale = input$logis.scale), from = 5,
                         to = -5, xlab = "x", ylab = "y", main = "Density")
                   if (input$MEAN) {
                       exp.logis <- function(x) {
                           return(x * dlogis(x, location = input$logis.location,
                                             scale = input$logis.scale))
                       }
                       xinter <- integrate(exp.logis, -Inf, Inf)$value
                       abline(v = xinter, col = "red")
                   }
                   curve(plogis(x, location = input$logis.location,
                                scale = input$logis.scale), from = 5,
                         to = -5, xlab = "x", ylab = "y",
                         main = "Distribution")},
               "Gosset (T)" = {
                   curve(dt(x, df = input$t.df, ncp = input$t.ncp),
                         from = -5, to = 5, xlab = "x", ylab = "y",
                         main = "Density")
                   if (input$MEAN) {
                       exp.t <- function(x) {
                           return(x * dt(x, df = input$t.df,
                                         ncp = input$t.ncp))
                       }
                       xinter <- integrate(exp.t, -Inf, Inf)$value
                       abline(v = xinter, col = "red")
                   }
                   curve(pt(x, df = input$t.df, ncp = input$t.ncp),
                         from = -5, to = 5, xlab = "x", ylab = "y",
                         main = "Distribution")},
               "Binomial" = {
                   curve(dbinom(x, size = input$binom.size,
                                prob = input$binom.prob),  n = 21,
                         type = "h", from = 0, to = 15, xlab = "x",
                         ylab = "y", main = "Probability Function")
                   if (input$MEAN) {
                       abline(v = input$binom.size * input$binom.prob, col = "red")
                   }
                   curve(pbinom(x, size = input$binom.size,
                                prob = input$binom.prob), n = 21,
                         type = "h", from = 0, to = 15, xlab = "x",
                         ylab = "y", main = "Distribution")},
               "Poisson" = {
                   curve(dpois(x, lambda = input$pois.lambda), from = 0,
                         to = 15, type = "h", xlab = "x", ylab = "y",
                         main = "Probability Function")
                   if (input$MEAN) {
                       abline(v = input$pois.lambda, col = "red")
                   }
                   curve(ppois(as.integer(x), lambda = input$pois.lambda),
                         from = 0, to = 15, xlab = "x", ylab = "y",
                         main = "Distribution")},
               "Geometric" = {
                   curve(dgeom(x, prob = input$geom.prob), from = 0,
                         to = 20, type = "h", xlab = "x", ylab = "y",
                         main = "Probability Function")
                   if (input$MEAN) {
                       abline(v = (1 - input$geom.prob)/input$geom.prob, col = "red")
                   }
                   curve(pgeom(x, prob = input$geom.prob), from = 0,
                         to = 20, xlab = "x", ylab = "y",
                         main = "Distribution")},
               "Hypergeometric" = {
                   curve(dhyper(x, m = input$hyper.m, n = 10, k = 10),
                         type = "h", from = 0, to = 10, xlab = "x",
                         ylab = "y", main = "Probability Function")
                   curve(phyper(x, m = input$hyper.m, n = 10, k = 10), from = 0,
                         to = 10, xlab = "x", ylab = "y",
                         main = "Distribution")},
               "Negative Binomial" = {
                   curve(dnbinom(x, size = input$nbinom.size,
                                 prob = input$nbinom.prob), from = 0, to = 20,
                         type = "h", xlab = "x", ylab = "y",
                         main = "Probability Function")
                   curve(pnbinom(x, size = input$nbinom.size,
                                 prob = input$nbinom.prob), from = 0, to = 20,
                         xlab = "x", ylab = "y", main = "Distribution")},
               "Lindley" =  {
                   lindley <- function(x, theta) {
                       fx <- (((theta)^2) / (theta + 1)) * (1 + x) *
                           exp(-theta * x)}
                   lindley_cum <- function(x, theta) {
                       Fx <- sapply(x, function(k) integrate(lindley, 0, k,
                                                             theta = theta)$value)
                       }
                   curve(lindley(x, theta = input$theta.lindley), from = 0,
                         to = 10, xlab = "x", ylab = "y", main = "Density")
                   curve(lindley_cum(x, theta = input$theta.lindley), from = 0,
                         to = 5, xlab = "x", ylab = "y", main = "Distribution")
               },
               "Discrete Lindley" = {
                   plindley <- function(x, lambda) {
                       px <- ((lambda^x) / (1 - log(lambda))) *
                           (lambda * log(lambda) + (1 - lambda) *
                            (1 - log(lambda^(x + 1))))
                       return(px)
                   }
                   dlindley <- function(x, lambda) {
                       Fx <- ((1 - lambda^(x + 1)) + ((2 + x) * lambda^(x + 1) - 1) * log(lambda)) /
                           (1 - log(lambda))
                       return(Fx)
                   }
                   hlindley <- function(lambda, x) {
                       Hx <- (lambda * log(lambda) + (lambda - 1) * (log(lambda^(x + 1)) - 1)) /
                           (1 - (1 + x) * log(lambda))
                       return(Hx)
                   }
                   par(mfrow = c(1, 3))
                   plot(plindley(x = 1:30, lambda = input$discrete.lindley.lambda), xlab = "x", ylab = "y", main = "Probability")
                   plot(dlindley(x = 1:30, lambda = input$discrete.lindley.lambda), xlab = "x", ylab = "y", main = "Distribution")
                   plot(hlindley(x = 1:60, lambda = input$discrete.lindley.lambda), xlab = "x", ylab = "y", main = "Hazard")
               },
               "ZIP" = {
                   x <- 0:30
                   plot(gamlss.dist::dZIP(x, mu = input$zip.lambda,
                                          sigma = input$zip.pi), type = "h",
                        xlab = "x", ylab = "y", main = "Probability Function")
                   if (input$MEAN) {
                       abline(v = input$zip.lambda * (1 - input$zip.pi),
                              col = "red")
                   }
                   curve(gamlss.dist::pZIP(x, mu = input$zip.lambda,
                                 sigma = input$zip.pi), from = 0, to = 30,
                         xlab = "x", ylab = "y", main = "Distribution")
               })
    })
    output$distPlot <- renderPlot({
        funcInput()
    })
    observeEvent(input$exit, {stopApp()})
}

shinyApp(ui, server)
