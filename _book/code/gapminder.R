library(gapminder)
library(tidyverse)
library(gganimate)

gapminder %>% filter(year==1952) %>% arrange(desc(pop))

comp.countries = c('China', 'India', 'United States')

pop.lim = range(gapminder$pop)
range(gapminder$lifeExp)
gdp.breaks = c(100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000)
p <- gapminder %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent, color=continent, frame=year)) + 
  geom_point(shape=21, alpha=0.6, color="black") +
  scale_size(limits = pop.lim, range=c(1, 50), labels = scales::comma)+
  scale_color_discrete() +
  geom_label(data = filter(gapminder, country %in% comp.countries), aes(label = country), fill = 'white', size=6, alpha=0.3, hjust=-0.1) + 
  scale_y_continuous("Life Expectancy", breaks=seq(20,90,10)) +
  scale_x_log10("GDP/capita", breaks = gdp.breaks, labels=scales::dollar) +
  coord_cartesian(xlim = c(100, 125000), ylim=c(20,90))+
  ylab('Life expectancy')+
  guides(fill = guide_legend(override.aes = list(size=10, alpha=1)),
         size = 'none',
         color = 'none') +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        text = element_text(size=20),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)
        )
print(p)
gganimate(p, filename="gapminder.gif", ani.width=1024, ani.height=768)

comp.years = c(1952, 2007)

gapminder %>% filter(year %in% comp.years) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) + 
  facet_grid(.~year)+
  geom_point() +
  scale_size(limits = pop.lim)+
  scale_color_discrete() +
  scale_y_continuous("Life Expectancy", breaks=seq(20,90,10)) +
  scale_x_log10("GDP/capita", breaks = gdp.breaks, labels=scales::dollar) +
  coord_cartesian(xlim = c(100, 125000), ylim=c(20,90))+
  ylab('Life expectancy')+
  theme_minimal()+
  geom_label(data = filter(gapminder, year %in% comp.years, country %in% comp.countries), aes(label = country), size=3, alpha=0.3, hjust=-0.1) + 
  theme(panel.grid = element_blank(),
        strip.text = element_text(size=14),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)
  )
