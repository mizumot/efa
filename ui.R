library(shiny)
library(shinyAce)


shinyUI(bootstrapPage(


    headerPanel("Exploratory Factor Analysis"),

    mainPanel(
        tabsetPanel(

        tabPanel("Main",

            p('Estimation may take a few seconds to minutes depending on the dataset.'),

            h3("Data"),
            p('Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),
            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Please make sure that your data includes the header (variable names) in the first row.</div></b>")),

            aceEditor("text", value="item1\titem2\titem3\titem4\titem5\titem6\titem7\titem8\n7\t6\t6\t6\t7\t6\t6\t7\n5\t5\t6\t7\t6\t5\t5\t4\n7\t6\t5\t7\t6\t6\t6\t6\n3\t3\t3\t5\t4\t4\t3\t3\n6\t4\t5\t5\t5\t4\t5\t4\n5\t4\t4\t7\t6\t4\t6\t4\n6\t7\t6\t6\t7\t5\t5\t6\n7\t7\t7\t7\t4\t4\t4\t4\n5\t6\t4\t5\t4\t2\t2\t4\n7\t5\t6\t7\t6\t4\t5\t6\n6\t4\t4\t6\t3\t2\t2\t4\n6\t5\t4\t5\t5\t4\t4\t2\n7\t6\t5\t6\t7\t6\t6\t6\n6\t5\t7\t7\t5\t4\t4\t4\n6\t2\t1\t5\t2\t2\t2\t2\n4\t4\t5\t5\t5\t3\t3\t2\n5\t6\t4\t7\t5\t3\t3\t5\n5\t6\t5\t6\t6\t6\t5\t5\n5\t7\t6\t6\t6\t3\t4\t4\n4\t5\t4\t5\t5\t2\t3\t3\n7\t7\t2\t4\t7\t1\t1\t1\n5\t6\t5\t6\t5\t3\t4\t5\n5\t5\t5\t5\t7\t4\t4\t4\n4\t5\t2\t5\t6\t5\t5\t5\n6\t6\t6\t7\t6\t7\t7\t4\n6\t5\t3\t6\t4\t3\t4\t4\n5\t5\t5\t6\t5\t4\t5\t5\n6\t7\t6\t7\t7\t5\t6\t7\n5\t5\t4\t6\t5\t4\t4\t5\n6\t4\t7\t7\t7\t6\t5\t5\n6\t6\t4\t6\t6\t6\t6\t5\n5\t5\t3\t4\t4\t4\t4\t5\n6\t6\t6\t6\t6\t5\t5\t5\n5\t6\t4\t5\t4\t3\t4\t6\n4\t4\t4\t5\t5\t5\t5\t5\n6\t7\t6\t6\t5\t5\t4\t6\n4\t7\t5\t5\t5\t4\t4\t5\n7\t7\t3\t7\t7\t7\t7\t7\n6\t7\t5\t7\t7\t6\t5\t6\n5\t5\t5\t5\t7\t5\t7\t7\n7\t2\t2\t2\t5\t1\t2\t3\n5\t5\t4\t6\t6\t4\t5\t5\n5\t5\t5\t5\t6\t5\t5\t6\n5\t4\t4\t4\t1\t1\t1\t1\n5\t7\t6\t7\t6\t4\t4\t5\n5\t6\t4\t6\t5\t3\t4\t4\n5\t5\t4\t5\t6\t4\t4\t4\n6\t4\t4\t7\t4\t4\t4\t4\n6\t6\t4\t7\t6\t4\t4\t5\n6\t7\t3\t5\t4\t3\t4\t5\n5\t6\t6\t6\t5\t4\t4\t5\n7\t7\t7\t7\t7\t7\t6\t6\n7\t6\t5\t7\t6\t5\t5\t4\n5\t5\t4\t5\t5\t3\t3\t4\n7\t6\t5\t7\t7\t5\t7\t7\n6\t6\t4\t5\t3\t2\t3\t2\n4\t4\t3\t4\t2\t1\t1\t1\n7\t7\t7\t7\t7\t6\t6\t7\n3\t5\t5\t3\t5\t4\t4\t2\n3\t2\t2\t5\t2\t2\t2\t2\n7\t6\t4\t5\t7\t6\t7\t7\n4\t7\t4\t7\t7\t7\t7\t7\n5\t5\t4\t6\t5\t4\t4\t5\n5\t5\t5\t5\t5\t3\t3\t3\n5\t4\t3\t5\t3\t2\t3\t3\n5\t4\t5\t6\t5\t3\t5\t4\n6\t6\t4\t6\t5\t4\t3\t4\n4\t6\t5\t6\t6\t6\t4\t4\n6\t7\t3\t4\t4\t2\t3\t3\n5\t4\t4\t5\t6\t5\t5\t4\n7\t7\t7\t7\t7\t7\t7\t7\n4\t3\t3\t5\t6\t5\t5\t4\n5\t7\t4\t6\t5\t4\t4\t4\n6\t6\t6\t6\t6\t5\t5\t5\n5\t5\t4\t5\t5\t4\t4\t5\n6\t5\t4\t6\t5\t4\t4\t4\n6\t3\t3\t4\t5\t4\t4\t4\n6\t7\t6\t5\t3\t3\t2\t1\n5\t5\t4\t6\t6\t5\t5\t4\n4\t6\t5\t7\t7\t6\t7\t7\n5\t5\t3\t5\t4\t3\t4\t4\n6\t4\t5\t5\t5\t4\t3\t3\n6\t7\t6\t7\t5\t2\t6\t6\n4\t7\t5\t7\t5\t3\t4\t5\n5\t4\t4\t5\t3\t3\t3\t3\n6\t5\t5\t6\t5\t4\t5\t6\n5\t4\t5\t6\t4\t3\t3\t4\n4\t3\t3\t5\t5\t3\t4\t4\n4\t6\t4\t5\t4\t2\t2\t4\n6\t5\t4\t6\t6\t4\t4\t5\n6\t6\t4\t7\t3\t2\t2\t4\n5\t4\t6\t6\t4\t1\t4\t3\n4\t5\t5\t5\t3\t3\t3\t3\n4\t6\t4\t6\t4\t4\t4\t5\n4\t4\t4\t4\t4\t3\t3\t3\n5\t5\t5\t5\t4\t3\t3\t4\n7\t5\t5\t6\t6\t5\t4\t6\n6\t6\t5\t6\t7\t6\t6\t6\n6\t6\t6\t6\t5\t6\t6\t5\n7\t5\t6\t7\t6\t3\t5\t5\n6\t6\t4\t6\t5\t4\t5\t5\n6\t6\t4\t5\t5\t4\t4\t6\n5\t5\t4\t6\t2\t2\t2\t5\n6\t7\t4\t6\t4\t4\t3\t4\n5\t5\t4\t5\t4\t4\t4\t4\n6\t5\t3\t5\t4\t3\t6\t6\n3\t7\t2\t5\t6\t5\t4\t4\n2\t5\t5\t5\t4\t3\t3\t3\n7\t6\t5\t7\t7\t6\t7\t7\n6\t6\t6\t7\t6\t6\t7\t6\n5\t3\t4\t5\t6\t5\t5\t5\n5\t3\t3\t3\t7\t2\t2\t5\n5\t5\t4\t6\t6\t2\t5\t4\n7\t6\t5\t6\t7\t7\t7\t7\n7\t5\t7\t6\t3\t4\t2\t3\n5\t4\t5\t5\t6\t6\t6\t6\n4\t6\t3\t5\t5\t3\t3\t4\n5\t4\t4\t5\t6\t5\t5\t5\n6\t6\t6\t7\t6\t4\t4\t4\n5\t7\t5\t6\t7\t6\t6\t6\n5\t6\t4\t6\t5\t4\t4\t4\n5\t4\t4\t6\t4\t4\t4\t4\n6\t7\t5\t5\t4\t3\t3\t4\n4\t6\t6\t7\t6\t3\t3\t3\n5\t5\t5\t5\t5\t5\t5\t5\n6\t4\t5\t5\t6\t4\t4\t5\n6\t6\t6\t6\t6\t4\t5\t6\n7\t7\t7\t7\t7\t7\t7\t7\n5\t3\t4\t5\t5\t3\t4\t5",
                mode="r", theme="cobalt"),

            br(),

            h3("Basic statistics"),
            verbatimTextOutput("textarea.out"),

            br(),

            h3("Correlation"),
            verbatimTextOutput("correl.out"),

            br(),

            strong("Scatter plot matrices"),

            br(),

            plotOutput("corPlot"),

            br(),

            h3("Scree plot"),

            plotOutput(outputId ="makesPlot", width="80%"),

            strong("Suggested number of factors"),
            verbatimTextOutput("nf.out"),

            br(),
            br(),

            h3("Specifying the number of factors"),

            radioButtons("numfactor", strong("Number of factors:"),
            list("Use the number of factors suggested by parallel analysis" = "parallel",
                 "Specify the number of factors" = "spec"), 'Use the number of factors suggested by parallel analysis'),
                conditionalPanel(
                    condition = "input.numfactor == 'spec'",
                    numericInput("numspec", "Number of factors:", 3)
                    ),
            br(),

            h3("Results of exploratory factor analysis"),
            p("Maximum likelihood with promax rotation"),
            verbatimTextOutput("efaresult.out"),

            br(),

            h3("Factor loadings plot (Factors 1 and 2)"),
            downloadButton('downloadPlot1', 'Download the plot as pdf'),
            plotOutput("facPlot1", height = "600px"),

            br(),

            downloadButton('downloadPlot2', 'Download the plot as pdf'),
            plotOutput("facPlot2", height = "500px"),

            br(),
            br(),

            strong('R session info'),
            verbatimTextOutput("info.out")

            ),


        tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

            br(),

            strong('List of Packages Used'), br(),
            code('library(shiny)'),br(),
            code('library(shinyAce)'),br(),
            code('library(psych)'),br(),

            br(),

            strong('Code'),
            p('Source code for this application is based on',
            a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href='http://mizumot.com/handbook/', target="_blank")),

            p('The code for this web application is available at',
            a('GitHub.', href='https://github.com/mizumot/efa', target="_blank")),

            p('If you want to run this code on your computer (in a local R session), run the code below:',
            br(),
            code('library(shiny)'),br(),
            code('runGitHub("efa","mizumot")')
            ),

            br(),

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="http://www.urano-ken.com/blog/2013/02/25/installing-and-using-macr/", target="_blank"),
            'is defenitely the way to go!'),

            br(),

            strong('Author'),
            p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
            'Associate Professor of Applied Linguistics',br(),
            'Faculty of Foreign Language Studies /',br(),
            'Graduate School of Foreign Language Education and Research,',br(),
            'Kansai University, Osaka, Japan'),

            br(),

            a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/"),

            p(br())

            )

))
))