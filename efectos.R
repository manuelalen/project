require("ggplot2")

library(plotly)
#efecto <- dlgInput(message="Ingrese su productividad de plata: ")$res
# Standardized Mean Difference (Cohen's d)
ES <- 65.52
# get mean2 depending on value of ES from d = (u1 - u2)/sd
mean1 <- ES*1 + 1
# create x sequence
x <- seq(1 - 3*1, mean1 + 3*1, .01)
# generate normal dist #1
y1 <- dnorm(x, 1, 1)
# put in data frame
df1 <- data.frame("x" = x, "y" = y1)
# generate normal dist #2
y2 <- dnorm(x, mean1, 1)
# put in data frame
df2 <- data.frame("x" = x, "y" = y2)
# get y values under overlap
y.poly <- pmin(y1,y2)
# put in data frame
poly <- data.frame("x" = x, "y" = y.poly)

# Cohen's U3, proportion of control > 50th perc. treatment
u3 <- 1 - pnorm(1, mean1,1)
u3 <- round(u3,3)

# plot with ggplot2
cohen <- ggplot(df1, aes(x,y, color="#062535")) +
  # add line for treatment group
  geom_area(size=1,fill="#062535",color="#062535") + 
  # add line for control group
  geom_area(data=df2, aes(color="#088bce"),size=1,fill="#088bce",color="#088bce") +
  # shade overlap
  geom_polygon(aes(color=NULL), data=poly, fill="red", alpha=I(4/10),
               show.legend=F) +
  # add vlines for group means
  geom_vline(xintercept = 1, linetype="dotted") + 
  geom_vline(xintercept = mean1, linetype="dotted") + 
  # add plot title
  
  # change colors and legend annotation
  scale_color_manual("Group", 
                     values= c("experimental" = "#062535","control" = "#088bce")) +
  # remove axis labels
  ylab(NULL) + xlab(NULL)+
  theme_minimal()+
  theme_light()+
  theme(text = element_text(family = "Tahoma"),
        panel.background = element_rect(color = "black", # Border color
                                        size = 1, fill = "#FFFFFF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold",hjust = 0.5,size = 25),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = "plot" ,
        panel.border = element_blank(),
        axis.text = element_text(size=7),
        axis.line = element_line(size = 1, colour = "black", linetype=1))+
  labs(title = paste0("Tamaño de efecto: ",ES),
       caption = "Author: Manuel Alén Sánchez",
       x = "Outcome")


ggplotly(cohen, tooltip = c("x","y")) %>%
  layout(plot_bgcolor='#373737')
