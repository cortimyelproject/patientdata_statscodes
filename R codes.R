library(MASS)
data$Group <- as.ordered(data$Group)
data$Group <- ordered(data$Group, levels = c("GOSE12", "GOSE36", "GOSE78", "Control"))
modelcnp <- polr(Group ~ CNPase, data = data, Hess = TRUE)
summary(modelcnp)
(ctablecnp <- coef(summary(modelcnp)))
pcnp <- pnorm(abs(ctablecnp[, "t value"]), lower.tail = FALSE) * 2
(ctablecnp <- cbind(ctablecnp, "p value" = pcnp))
modelmbp <- polr(Group ~ MBP, data = data, Hess = TRUE)
summary(modelmbp)
(ctablembp <- coef(summary(modelmbp)))
pmbp <- pnorm(abs(ctablembp[, "t value"]), lower.tail = FALSE) * 2
(ctablembp <- cbind(ctablembp, "p value" = pmbp))
library(ggeffects)
predcnpase  <- ggpredict(modelcnp, terms = "CNPase")
plot(predcnpase, facet = TRUE) + geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) + geom_line(aes(color = group), size = 1) + scale_fill_manual(values = "#56B4E9") + scale_color_manual(values = "black") + theme_bw(base_size = 10) + labs(x = "CNPase",y = "Predicted probability", title = "CNPase effect plot")
predmbp <- ggpredict(modelmbp, terms = "MBP")
plot(predmbp, facet = TRUE) + geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) + geom_line(aes(color = group), size = 1) + scale_fill_manual(values = "#56B4E9") + scale_color_manual(values = "black") + theme_bw(base_size = 10) + labs(x = "MBP",y = "Predicted probability", title = "MBP effect plot")
library(ggplot2)
data$Group_num <- as.numeric(data$Group)
ggplot(data, aes(x = Group_num, y = MBP)) +
  geom_point(size = 3, colour = "black") +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "#56B4E9",
    fill = "grey70",
    alpha = 0.3
  ) +
  scale_x_continuous(
    breaks = 1:4,
    labels = levels(data$Group)
  ) +
  theme_classic(base_size = 16) +
  labs(x = "Group", y = "MBP")
ggplot(data, aes(x = Group_num, y = CNPase)) +
  geom_point(size = 3, colour = "black") +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "#56B4E9",
    fill = "grey70",
    alpha = 0.3
  ) +
  scale_x_continuous(
    breaks = 1:4,
    labels = levels(data$Group)
  ) +
  theme_classic(base_size = 16) +
  labs(x = "Group", y = "CNPase")
