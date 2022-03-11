library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(gapminder)
library(tidyverse)
library(scales)

df <- gapminder

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(
        list(
            htmlH1('Our Changin World'),
            htmlBr(),
                dbcRow(
                    list(
                        dbcCol(
                            list(
                                htmlBr(),
                                htmlBr(),
                                htmlH5('What do you want to know?'),
                                dccDropdown(
                                    id='topic_dropdown',
                                    options = list(
                                        list(label = "Population", value = "pop"),
                                        list(label = "Life Expectancy", value = "lifeExp"),
                                        list(label = "GDP per Capita", value = "gdpPercap")
                                        ),
                                    value = 'pop'
                                    ),
                                htmlBr(),
                                htmlH5('Choose year'),
                                dccSlider(
                                    id='year_slider',
                                    min = 1952,
                                    max = 2007,
                                    step = 5,
                                    value = 1957,
                                    marks = list(
                                      "1952" = "1952",
                                      "1957" = "1957",
                                      "1962" = "1962",
                                      "1967" = "1967",
                                      "1972" = "1972",
                                      "1977" = "1977",
                                      "1982" = "1982",
                                      "1987" = "1987",
                                      "1992" = "1992",
                                      "1997" = "1997",
                                      "2002" = "2002",
                                      "2007" = "2007"
                                    )
                                )
                            )
                        ),
                        dbcCol(
                            list(
                                htmlH3('World Ranking'),
                                dccGraph(id='ranking_chart')
                            )
                        )
                    )
                )
        )
    )
)


app$callback(
    output('ranking_chart', 'figure'),
    list(input('year_slider', 'value'),
         input('topic_dropdown', 'value')),
    function(x_year, y_axis) {
      df <- gapminder %>% 
        filter(year == x_year)
      bar <- df %>% 
        arrange(desc(!!sym(y_axis))) %>% 
        mutate(ranking = paste0('#', as.numeric(rownames(df)))) %>% 
        ggplot() +
        aes(x = !!sym(y_axis), 
            y = reorder(country, !!sym(y_axis)), 
            fill = continent, 
            label = ranking) +
        geom_bar(stat = 'identity') +
        geom_text() +
        theme(text = element_text(size = 20)) +
        theme_bw() +
        scale_x_continuous(labels = comma)
      
      if (y_axis == "pop") {
        chart <- bar + labs(x = "Population", y = "Country")
      }
      if (y_axis == "lifeExp") {
        chart <- bar + labs(x = "Life Expectancy [years]", y = "Country")
      }
      if (y_axis == "gdpPercap") {
        chart <- bar + labs(x = "GDP per Capita [USD]", y = "Country")
      }
      
      return(ggplotly(chart, height = 3000, width=800, tooltip = c(y_axis)))
    }
)


app$run_server(host = '0.0.0.0')
