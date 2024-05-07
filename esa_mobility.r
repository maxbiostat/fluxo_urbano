library(ggplot2)
library(scales)

midpoints <- function(x, dp = 3){
  # taken from https://mcfromnz.wordpress.com/2014/05/23/finding-the-midpoint-when-creating-intervals/  
  lower <- as.numeric(gsub(",.*", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  upper <- as.numeric(gsub(".*,", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  return(round(lower + (upper-lower)/2, dp))
}
Ndisc <- 20 ## number of bins
Discretize <- function(x, n = Ndisc){
  midpoints(ggplot2::cut_interval(x, n = n))
}
#
number_ticks <- function(n) {function(limits) pretty(limits, n)}

#### Carregando os dados
DistancesAndMovement <- read.csv("data/FlowDataSet.csv") 

#######
cor(na.omit(DistancesAndMovement[, -c(1:2)])) ## correlation for all data
cor(na.omit(DistancesAndMovement[which(DistancesAndMovement$dist.euc > 0 ), -c(1:2)])) ## correlation for non-zero euclidean distances

NonZero <- subset(DistancesAndMovement,
                  dist.euc > 0 & dist.dr > 0 & flow.dr > 0)


#
NonZero$discretised.dist.euc <- Discretize(NonZero$dist.euc)
NonZero$discretised.dist.dr <- Discretize(NonZero$dist.dr)
NonZero$discretised.dist.wk <- Discretize(NonZero$dist.wk)


## Figuras (plots) 

### Walking

ggplot(NonZero, aes(x = discretised.dist.euc/1000, y = flow.wk) )+
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("Euclidean distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "walking (log) flow", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("Euclidean distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Walking flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()

ggplot(NonZero, aes(x = discretised.dist.wk, y = flow.wk ) ) +
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("walking distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "walking (log) flow", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("Walking distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Walking flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()

MaximumFlowA <- aggregate(flow.wk ~ discretised.dist.euc, NonZero, max)
MaximumFlowA$flow.wk[MaximumFlowA$flow.wk == 0] <- NA

ggplot(MaximumFlowA, aes(x = discretised.dist.euc/1000, y = flow.wk) )+
  geom_point() +
  stat_smooth(method = "loess") + 
  scale_x_continuous("Euclidean distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "Maximum walking (log) flow",
  #                    expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_x_log10("Euclidean distance (Km)",
  #               breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Maximum walking flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

MedianFlowA <- aggregate(flow.wk ~ discretised.dist.euc, NonZero, median)
MedianFlowA$flow.wk[MedianFlowA$flow.wk == 0] <- NA


ggplot(NonZero, aes(x = discretised.dist.wk, y = flow.wk ) ) +
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("walking distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "(log) walking flow", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("walking distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("walking flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

MaximumFlowB <- aggregate(flow.wk ~ discretised.dist.wk, NonZero, max)
MaximumFlowB$flow.wk[MaximumFlowB$flow.wk == 0] <- NA


ggplot(MaximumFlowB, aes(x = discretised.dist.wk, y = flow.wk ) )+
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("walking distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "Maximum walking (log) flow",
  #                    expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("Walking distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Maximum walking flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

## Driving

ggplot(NonZero, aes(x = discretised.dist.euc/1000, y = flow.dr) )+
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("Euclidean distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "Driving (log) flow", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("Euclidean distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Driving flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()

ggplot(NonZero, aes(x = discretised.dist.dr, y = flow.dr ) ) +
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("Driving distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "Driving (log) flow", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("Driving distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Driving flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()


MaximumDRFlowA <- aggregate(flow.dr ~ discretised.dist.euc, NonZero, max)

ggplot(MaximumDRFlowA, aes(x = discretised.dist.euc/1000, y = flow.dr) )+
  geom_point() +
  stat_smooth(method = "loess") + 
  scale_x_continuous("Euclidean distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "Maximum Driving (log) flow",
  #                    expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_x_log10("Euclidean distance (Km)",
  #               breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Driving flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()


MedianDRFlowA <- aggregate(flow.dr ~ discretised.dist.euc, NonZero, median)
ggplot(MedianDRFlowA, aes(x = discretised.dist.euc/1000, y = flow.dr ) )+
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("Euclidean distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "Median Driving (log) flow",
  #                    expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("Euclidean distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Median driving flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()


ggplot(NonZero, aes(x = discretised.dist.dr, y = flow.dr ) ) +
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("Driving distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "(log) Driving flow", expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("Driving distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Driving flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()


MaximumDRFlowB <- aggregate(flow.dr ~ discretised.dist.dr, NonZero, max)

ggplot(MaximumDRFlowB, aes(x = discretised.dist.dr, y = flow.dr ) )+
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("Driving distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "Maximum Driving (log) flow",
  #                    expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("Driving distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Maximum driving flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()


MedianDRFlowB <- aggregate(flow.dr ~ discretised.dist.dr, NonZero, median)

ex_fig <- ggplot(MedianDRFlowB, aes(x = discretised.dist.dr, y = flow.dr ) )+
  geom_point() +
  stat_smooth(method = "loess") + 
  # scale_x_continuous("Driving distance (Km)", expand = c(0, 0), breaks = number_ticks(10)) +
  # scale_y_continuous(trans = log_trans(), "Median Driving (log) flow",
  #                    expand = c(0, 0), breaks = number_ticks(10)) +
  scale_x_log10("Driving distance (Km)",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10("Median driving flow",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

ex_fig

ggsave(
  plot = ex_fig,
  filename = "results/fluxo_dr_mediana_drDist.pdf",
  scale = 1,
  width = 297,
  height = 210,
  units = "mm",
  dpi = 300
)

