# Analysis of Transit Infrastructure Cost in the US as Compared to Other Countries


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = F)
library(tidyverse)
library(countrycode)
library(scales)
library(viridis)
library(RColorBrewer)
library("DT")
library(plotly)

```

**Loading, Cleaning and Preparing the Data:**

-   The 'country' variable had 2-letter country codes, which made it difficult to identify a country So we added the complete country names for each country.

-   UK had to be renamed to "GB" because that is how the countrycode() function identifies UK.

-   A region variable was also added for regional analysis. The countrycode() function does not identify the region of Taiwan. So we manually had to change the NA values created in Region for Taiwan to "Eastern Asia".

-   Many of the variables that should have been numeric, were character in this dataset. So we converted their class type to 'dbl' from 'chr'.

```{r eval = TRUE, message = FALSE, warning = FALSE}
tt <- tidytuesdayR::tt_load("2021-01-05")
transit_cost <- tt$transit_cost %>%
  filter(!is.na(e)) %>%
  mutate(
    country = ifelse(country == "UK", "GB", country),
    country_name = countrycode(
      sourcevar = country,
      origin = "iso2c",
      destination = "country.name"
    ),
    start_year = as.numeric(start_year),
    end_year = as.numeric(end_year),
    year = as.numeric(year),
    real_cost = as.numeric(real_cost),
    tunnel_per = as.numeric(gsub("%", "", tunnel_per))
  ) %>%
  rename(
    country_code = country,
    country = country_name
  ) %>% 
   mutate(
     region= countrycode(
    sourcevar = country,
    origin = "country.name",
    destination ="un.region.name")) %>% 
  mutate(
     subregion= countrycode(
    sourcevar = country,
    origin = "country.name",
    destination ="un.regionsub.name")) 

transit_cost$region <- transit_cost$region %>% 
  replace_na("Asia") 
transit_cost$subregion <- transit_cost$subregion %>% 
  replace_na("Eastern Asia")#This was done to convert Taiwan to Eastern Asia

```

```{r eval = TRUE, message = FALSE, warning = FALSE}
transit_cost %>%
  group_by(country,city) %>% 
  summarize(n=n())
```

**Data Description**

```{r descriptive-stats, eval = TRUE, echo = FALSE}
description <- transit_cost %>% 
  filter(!is.na(start_year),
         !is.na(end_year),
         !is.na(year)) %>% 
  group_by(country) %>% 
  summarise(
    n = n(),
    avg_cost_km = mean(cost_km_millions),
    avg_duration_km = mean((end_year - start_year) / length)) %>% 
  mutate(
    across(where(is.numeric), round, 2)) %>% 
  slice_max(n, n = 56) %>% 
  datatable(
    colnames = c(
      "Country"="country",
      "Average Cost per km (Millions of USD)"="avg_cost_km",
      "Average Duration per km (Years)"= "avg_duration_km",
      "Number of Lines"= "n"))
  
description


```

**Average Cost per KM (in Millions of USD) by Country**

```{r}
avg_tunnel_df <- transit_cost %>%
  filter(!is.na(tunnel_per)) %>%
  group_by(country) %>%
  summarize(avg_tunnel_per = mean(tunnel_per))
```

```{r figure-one, fig.cap= "Average Cost per KM (in Millions of USD) by Country", eval=TRUE}

Fig1 <- transit_cost %>%
  left_join(avg_tunnel_df, by = "country") %>%
  filter(!is.na(cost_km_millions)) %>%
  mutate(country = fct_reorder(country, cost_km_millions, .fun = "mean")) %>%
  ggplot(aes(cost_km_millions, country, fill = avg_tunnel_per)) +
  stat_summary(geom = "col", fun = mean) +
  scale_fill_viridis(
    option = "F",
    name = "Avg % of Tunnel \nCompleted") +
  scale_x_continuous(labels = dollar) +
  labs(
    x = "Cost per KM (Millions of USD)",
    y = "",
    title = "Average Cost per KM (in Millions of USD) by Country",
    subtitle = "Transit lines in the US have the highest average cost in the world") +
  theme_minimal()+
  theme(
    legend.title=element_text(size=8),
    legend.text= element_text(size =6),
    legend.position = c(.95, .15),
    legend.justification = c("right", "bottom"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 6)) 


Fig1
```

Figure \@ref(fig:figure-one) displays the global average cost per km of transit projects. It also distinguishes the average level of project completeness by country. United States has the highest average cost per km and majority of its tunnels are completed because the United States bar matches the lighter color shade in the legend.

**Cost per km (in Millions of USD) Against Length of Line**

```{r figure-two, fig.cap= "Cost per km Against Length of Line", eval=TRUE}
Fig2 <- transit_cost %>%
  ggplot(aes(length, cost_km_millions, color = tunnel_per)) +
  geom_point(shape = 1,
             size = 2,
             stroke = .75) +
  scale_color_continuous(
    name = "% of Tunnel \nCompleted")+
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(
    x = "Length of Project Lines (in KM)",
    y = "Cost per KM (in Millions of USD)",
    title = "Cost per km Against Length of Line",
    subtitle = "Percentage Completed Represented by Color Gradient"
  )+
  theme(
    panel.background = element_rect(fill='white', colour='gray'),
    panel.grid.major = element_line(color = 'grey85'),
    panel.grid.minor = element_line(colour = 'grey90'))+
  theme_minimal()
ggplotly(Fig2)


```

Figure \@ref(fig:figure-two) shows the cost per km in millions of USD of project lines all over the world, in comparison to the length of the lines. Each circle represents a transit line in this data. The color gradient represents the percentage of completion for each transit line. As expected, lines with a higher proposed length tend to have higher costs.

**Cost per KM (in Millions of USD) Against duration of Project**

```{r figure-three, fig.cap= "Cost per km Against Duration of Project",  eval=TRUE}
Fig3 <- transit_cost %>% 
  mutate(n_years = end_year - start_year) %>% 
ggplot(aes(x = n_years, y = cost_km_millions, color =region)) +
  geom_point(position = position_jitter(width = 0.3, height = 0.1),
             alpha =.9)+
   scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(trans = "log1p")+
  scale_y_continuous(trans = "log1p")+
  labs(
    x = "Duration of Project Lines (in years)",
    y = "Cost per KM (in Millions of USD)",
    title = "Cost per km Against Duration of Project"
  )+
  theme(
    panel.background = element_rect(fill='white', colour='gray'),
    panel.grid.major = element_line(color = 'grey95'),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())
ggplotly(Fig3)

```

Figure \@ref(fig:figure-three) shows that projects with a higher duration of construction generally cost higher. Generally, tunnels that cost higher and take longer are in the Americas and tunnels that have lower costs and take shorter periods of time are in Asia.

**Comparison of Cost per KM (in Millions of USD) of Completed Tunnel lines in the New York and Shanghai**

```{r figure-four, fig.cap="Cost per KM: New York vs Shanghai", eval= TRUE}
transit_cost %>%
  filter(
    country == c("United States","China"),
    city == c("New York", "Shanghai"),
    !is.na(cost_km_millions),
    tunnel_per == 100
  ) %>%
  mutate(line = fct_reorder(line, cost_km_millions)) %>%
  ggplot(aes(cost_km_millions,line, fill = city)) +
  geom_col()+
  labs(
    y= "",
    x = "Cost per KM (Millions USD)",
    title = "Cost per KM: New York vs Shanghai")+
  theme_minimal()+
theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y= element_blank())

```

Figure \@ref(fig:figure-four) shows a data from New York and Shanghai, cities with the highest number of tunnels in their respective countries. It juxtaposes the cost of the completed tunnels in both cities to show the stark difference in the cost between the two. It clearly highlights that New York has a higher cost/km compared to Shanghai

**Cost per km by cities in US Lines**

```{r figure-five, fig.cap="Cost/km of Complete and On-going Lines in the US", eval=TRUE}

transit_cost %>%
  filter(
    country == "United States",
    !is.na(cost_km_millions),
    tunnel_per == 100,
    year <= 2025) %>% 
    ggplot(aes(year, cost_km_millions, group=year)) +
  geom_boxplot()+
  geom_jitter(aes(color=line))+
  theme_minimal()+
  labs(
    x="",
    y = "Cost per KM (Millions USD)",
    title="Cost/km of Complete and On-going Lines in the US",
    subtitle= "2010-2025"
  )
```

Figure \@ref(fig:figure-five) shows the cost per km, in millions of USD, of transit lines that are ongoing construction or complete from 2010 and 2025, with 'year' being the mid-point year of construction. Hence, values in future are also included. Lines have also been identified to their relative midpoint years and costs. It shows that the cost has fluctuated significantly over the past couple of years. Most importantly, in 2015 the East Side Access line pulled the cost per km to an all time high of approximately \$4bn.

**Average Cost/km per Country and Duration of Construction per km**

```{r figure-six, fig.cap="Average Cost/km per Country and Duration of Construction per km, by Country", eval=TRUE}

avg_cost_df <- transit_cost %>%
  filter(
    !is.na(cost_km_millions),
    tunnel_per == 100,
    !is.na(end_year),
    !is.na(start_year)
  ) %>%
  group_by(region, country) %>%
  summarise(
    avg_cost_km = mean(cost_km_millions),
    avg_duration_km = mean((end_year - start_year) / length),
    n = n(),
    avg_tunnel_per =mean(tunnel_per)
  )
avg_cost_df %>%
  ggplot(aes(avg_duration_km, avg_cost_km, label = country)) +
  geom_label(aes(label = country, size = n, fill = n), colour = "white", fontface = "bold")+
  scale_size_continuous(name= "Number \nof Lines")+
   guides(size=FALSE)+
  scale_fill_viridis_c(
    trans = "log", option = "C",
    breaks = c(0, 1, 100, 10000, 1000000),
    name = "Number \nof Lines"
  ) +
  theme(
    panel.background = element_rect(
      fill = "grey95",
      colour = "grey95",
      size = 0.9,
      linetype = "solid"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(trans = 'log10')+
  scale_x_continuous(trans='log1p')+
  labs(
    x = "Average Duration per KM (in years)",
    y = "Cost per KM (Millions USD)",
    title = "Average Cost/km per Country and Duration of Construction per km, by Country",
    subtitle = "Size of font represents the number of observations"
  )

```

Figure \@ref(fig:figure-six) shows the average cost/km and the average number of years per km by country. The font size and the color of the labels indicates the number of observations in each country. The figure clearly indicates that China has the highest number of observations, one of the lowest cost per km and one of the lowest average duration per km.

```{r figure-seven, fig.cap="Average Cost/km per Country and Duration of Construction per km, by Country", eval=TRUE}
fig7 <- avg_cost_df %>%
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~avg_duration_km,
    y = ~avg_cost_km,
    marker = list(size = ~n, sizeref = 0.08, sizemode = 'area'),
    color = ~region,
    text = ~country,
    hovertemplate = paste(
      "<b>%{text}</b><br><br>",
      "%{yaxis.title.text}: %{y:$,.0f}<br>",
      "%{xaxis.title.text}: %{x:.0}<br>",
      "Number: %{marker.size:,}",
      "<extra></extra>"
      )
    ) %>% 
  layout(
    yaxis = list(title = "Average Cost per km (Millions USD)"), 
    xaxis = list(title = "Average Duration per KM (in years)"),
    legend = list(orientation = 'h', y = -0.3))

fig7



```

Figure \@ref(fig:figure-seven) and Figure \@ref(fig:figure-six), both present the same information, except Figure \@ref(fig:figure-seven) provides additional detail using the interactive hover box.

Note:

-   This data has been retrieved from the Tidy Tuesday website and has been originally sourced from [\<https://transitcosts.com\>](https://transitcosts.com/). A few of the data visualization plots have been inspired by the original website. The coding for the reproduction of these plots and everything in this .rmd file is our own, authentic work.

-   We have also learned and used some of the data preparation and cleaning tips from the 'Tidy Tuesday live screencast: Analyzing transit costs in R' <https://www.youtube.com/watch?v=8jNQzce13SE>\
