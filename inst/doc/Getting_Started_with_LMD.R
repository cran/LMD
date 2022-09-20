## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "#>")

## ----setup, warning = FALSE, include = FALSE----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "672px",
  out.height = "480px",
  fig.width = 7,
  fig.height = 5,
  fig.align = "center",
  fig.retina = 1,
  dpi = 150
)

library("ggplot2")
library("rmarkdown")
library("knitr")
library("ggformula")
library("patchwork")
library( "LMD")

# # installing all required packages
# list.of.packages <- c("ggplot2",
#                       "rmarkdown",
#                       "knitr",
#                       "ggformula",
#                       "patchwork",
#                       "LMD")
# 
# new.packages <-
#   list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
# if (length(new.packages))
#   install.packages(new.packages, dependencies = TRUE)
# 
# # Loading the required libraries
# lapply(list.of.packages, library, character.only = T)



options(expressions = 10000)


## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------

fs <- 2000 #Sampling Frequency
time_component <- seq(from = 0, to = 9, by = 1 / fs)
# Simulated Signal
signal <- (0.25 * time_component + sin(pi * time_component) + sin(2 * pi * time_component) +
      sin(6 * pi * time_component))

dummy_df <- data.frame(Time = time_component, Value = signal)

p1 <- ggplot(dummy_df) +
  aes(x = Time, y = Value) +
  geom_spline(size = 2, colour = "#1D2B71") +
  scale_color_hue() +
  labs(title = "Fig 1. Sample Signal") +
  theme_minimal() +
  theme(legend.position = "none")

plot(p1)


## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------

extreme_values <- LMD::find_extrema(signal)

p2 <- ggplot() +
  geom_spline(
    data = dummy_df,
    aes(x = Time, y = Value),
    size = 2,
    colour = "#1D2B71"
  ) +
  geom_point(aes(x = dummy_df$Time[extreme_values], y = dummy_df$Value[extreme_values]),
             size = 5,
             colour = "red") +
  scale_color_hue() +
  labs(title = "Fig 2. Sifting Process: (1)") +
  theme_minimal() +
  theme(legend.position = "none")

plot(p2)

## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------

extreme_values <- LMD::find_extrema(signal)
ma_enve = local_mean_and_envelope(signal, extreme_values)
mean = ma_enve$mean

p3 <- ggplot() +
  geom_spline(
    data = dummy_df,
    aes(x = Time, y = Value),
    size = 2,
    colour = "#1D2B71",
    alpha = 0.5
  ) +
  geom_point(
    aes(x = dummy_df$Time[extreme_values], y = dummy_df$Value[extreme_values]),
    size = 5,
    colour = "red",
    alpha = 0.5
  ) +
  geom_line(aes(x = dummy_df$Time, y = mean),
            size = 1,
            colour = "red") +
  scale_color_hue() +
  labs(title = "Fig 3. Sifting Process: (2.1)") +
  theme_minimal() +
  theme(legend.position = "none")

plot(p3)

## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------

ma = ma_enve$ma

p4 <- ggplot() +
  geom_spline(
    data = dummy_df,
    aes(x = Time, y = Value),
    size = 2,
    colour = "#1D2B71",
    alpha = 0.5
  ) +
  geom_point(
    aes(x = dummy_df$Time[extreme_values], y = dummy_df$Value[extreme_values]),
    size = 5,
    colour = "red",
    alpha = 0.5
  ) +
  geom_line(aes(x = dummy_df$Time, y = ma),
            size = 1,
            colour = "red") +
  scale_color_hue() +
  labs(title = "Fig 4. Sifting Process: (2.2) ") +
  theme_minimal() +
  theme(legend.position = "none")

plot(p4)

## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------

envelope = ma_enve$enve
Time=dummy_df$Time

p5 <- ggplot() +
  geom_line(aes(x = Time, y = envelope),
            size = 1,
            colour = "red") +
  scale_color_hue() +
  labs(title = "Fig 5. Sifting Process: (2.3)") +
  theme_minimal() +
  theme(legend.position = "none")

plot(p5)

## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------

enve_sm = ma_enve$enve_sm
Time=dummy_df$Time
p6 <- ggplot() +  
  geom_line(aes(x = Time, y = envelope),
            size = 0.5,
            colour = "red",alpha=0.5) +
  geom_line(aes(x = Time, y = enve_sm),
            size = 1,
            colour = "red") +
  scale_color_hue() +
  labs(title = "Fig 6. Sifting Process: (2.4) ") +
  theme_minimal() +
  theme(legend.position = "none")

plot(p6)

## -----------------------------------------------------------------------------
t=0:600
x1=(1+0.5*cos(pi*t/100))*(cos(pi*t/2+2*cos(pi*t/50)))
x2=4*sin(pi*t/2500)*sin(6*pi*t/50)
y=x1+x2

Time=t
combined_signal=y

p7 <- ggplot() +
  ggplot2::geom_line(aes(x = Time, y = combined_signal),
                     size = 1,
                     colour = "midnightblue") +
  ggplot2::ylab("Combined Signal x(t)") + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(size = 0.1),
    legend.position = c(0.8, 0.8),
    panel.grid.major.x = ggplot2::element_blank()
  )

p8 <- ggplot() +
  ggplot2::geom_line(aes(x = Time, y = x1),
                     size = 1,
                     colour = "midnightblue") +
  ggplot2::ylab("x1(t)") + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(size = 0.1),
    legend.position = c(0.8, 0.8),
    panel.grid.major.x = ggplot2::element_blank()
  )

p9 <- ggplot() +
  ggplot2::geom_line(aes(x = Time, y = x2),
                     size = 1,
                     colour = "midnightblue") +
  ggplot2::ylab("x2(t)") + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(size = 0.1),
    legend.position = c(0.8, 0.8),
    panel.grid.major.x = ggplot2::element_blank()
  )

print((p7/p8/p9)+ 
    plot_annotation(title = "Fig 8. Simulated Signal"))


## -----------------------------------------------------------------------------
lmd_object=LMD::lmd(combined_signal,max_num_pf=2)
PF1=lmd_object[["pf"]][[1]]
PF2=lmd_object[["pf"]][[2]]
residual_signal=lmd_object[["residue"]]



## -----------------------------------------------------------------------------
plot_lmd(lmd_object)+ 
    plot_annotation(title = "Fig 9. LMD Decompsition of Simulated Signal")

