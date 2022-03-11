library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(gapminder)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- gapminder

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
                                    value = 1957
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
    list(input('topic_dropdown', 'value'),
         input('year_slider', 'value')),
    function(x_year, y_axis) {
        df <- df %>%
            filter(year == x_year)
        bar <- df %>%
            arrange(desc(!!sym(y_axis))) %>%
            mutate(ranking = paste0('#', as.numeric(rownames(df)))) %>%
            ggplot(aes(x = !!sym(y_axis),
                       y = reorder(country, !!sym(y_axis)),
                       fill = continent,
                       label = ranking)) +
            geom_bar(stat = "identity") +
            geom_text()

        chart <- bar +
            theme(text = element_text(size = 20)) +
            theme_bw() +
            scale_x_continuous(labels = comma)

        ggplotly(chart, height = 3000, width=800, tooltip = c(y_axis))

    }
 )

app$run_server(debug = T)
