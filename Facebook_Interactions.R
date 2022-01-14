# ALAN PEREZ
# FINAL

# ---- Data Loading ----
rm(list=ls())
load("facebook.RData")
names(facebook)
str(facebook)
attach(facebook)

# ---- PROBLEM 1 ----
# part a
slr <- lm(interactions ~ impressions)



# i 
summary(slr)
sd(impressions)
coef(slr)
B <- coef(slr)[2]
B * 30000
"For every additional 30,000 impressions on a post, the number of interactions 
with the post is higher by 91.04 on average. "


# ii 
A <- coef(slr)[1]
A
"The average number of interactions a post with 0 impressions receives is 128."


# iii
summary(impressions)
"This interpretation is not meaningful because it is not possible for a facebook
post to be interacted with (as in being liked, commented on, or shared) without
having been viewed. The number of interactions when the number of impressions
is zero should also be 0.
"

# iv
summary(slr)
"Since our p-value is p = 2.2 x 10^-16, we can confidently say the relationship
between interactions and impressions is statistically significant."


# part b
# i 
plot(impressions, interactions, pch=20)
abline(slr, col='Blue')


# ii
lines(lowess(impressions, interactions), col="RED") # lowess creates x and y atributes 
"We see from our scatterplot that for posts with more than 150,000 impressions, 
points are systematically below the line of best fit.
To aid our visualization, we add a LOWESS which is a scatter plot smoother. The 
curve suggests that we have nonlinearity. 
To fix this, we could 'expand' our interactions term into a quadratic."


E <- residuals(slr)
plot(impressions, E, pch=20, main="Residuals versus X  plot for SLR")
abline(h=0)  # a flat line

# add a LOWESS curve for visuals reference: 
lines(lowess(impressions, E), col="RED", lw=2)


quad <- lm(interactions ~ poly(impressions, 2, raw=TRUE))
E_quad <- residuals(quad)
plot(impressions, E_quad, pch=20)
lines(lowess(impressions, E_quad), pch=20, col="red")
abline(h=0)

"Since our LOWESS curve is now flat along the horizontal line, this is a sign
of improvement in our model."


# iii 
"Constant variance does seem violated because the spread of the points about 
the line of best fit differe for different regions of X values."

# iv
E <- residuals(slr)
hist(E)
"The histogram appears to have a relatively long right tail, which leads me to 
believe the normality of errors assumption is violated. The errors are skewed
right. "


# v
"I suspect that 
it is interesting that we are tracking the total number of interactions that 
Facebook users had with the post, when in reality we are actually just tracking
the total number of interactions that their specific Facebook followers had 
with the post. 
This would also likely attribute to why posts with more impressions have almost
no interactions since these most likely are a result of advertising.
"

# part c
# i 
slr <- lm(interactions ~impressions)
plot(impressions, interactions, pch = 20)
abline(slr, col="blue")

# ii 
h <- hatvalues(slr)
hbar <- mean(h)
hbar
high_lev <- which(h > 3*hbar)
length(high_lev)

points(impressions[high_lev], interactions[high_lev], col="orange", cex=2)


# iii. 
E_star <- rstudent(slr)
y_outlier <- which(abs(E_star) > 3)
length(y_outlier)
points(impressions[y_outlier], interactions[y_outlier], col="BLUE", pch="X")


# iv. 
D <- cooks.distance(slr)
high_cook <- which(D > 0.28)
length(high_cook)
points(impressions[high_cook], interactions[high_cook], col="RED", pch="C", cex = 2)


# v. 
"There were 5 points flagged as being influential by COok's Distance. Yes, each 
of the points were both high leverage and high residual points indicated by both the 
orange circle and the blue X. No, every pointy that was high leverage and high 
residual was flagged as influential by Cook's Distance. This is indicated by the
graph below."

both <- which(abs(E_star) > 3 & h > 3*hbar)
both

points(impressions[both], interactions[both], col = "pink", pch = "O", cex = 4)


# ---- Problem 2 ----
# part a
mlr <- lm(interactions ~ pagelikes + paid + factor(type))
summary(mlr)

# i 
sd(pagelikes)
0.001008 * 15000
"On posts with fixed advertisement status (whether it was an advertised post or 
regular post) and type, every additional 15,000 pagelikes is associated with 15
more interactions, on average."

# ii paid
"Advertised posts of fixed pagelikes and type, receive approximately 49 
(49.53) more interactions than regular non-paid posts, on average. "

# iii
"On average, photo posts of fixed page likes/follows and advertisement status have 
about 104 (104.25) more interactions than link posts."
# among posts of fixed page likes/follows and advertisement status, photos have 
# on average 104 more interactions that links.

# iv
"Assuming the the number of page likes/follows and advertisement status are fixed, 
it appears that video posts get the most interactions on average. Under the 
same conditions, it seems that link posts get the least amount of interaction 
on average. "


# part b: an overall F-test B-)
# i 
"
H0: B1 = B2 = B3 = B4 = B5 = 0; None of the variables pagelikes, adverisement status
(paid), and post type are related to post interactions.
HA: Bj != 0 for some j = 1, 2,3,4,or 5; At least one of the variables pagelikes, 
adverisement status (paid), or post type are related to post interactions. 
"

# ii. 
summary(mlr)
"F-statistics = 2.22 and the p-value = 0.0512."

# iii. =
"Since our p-value is little larger than our significance level of 0.05, we do
 not have enough evidence to reject H0. That is, there is not enough evidence 
 to conclude that at least one of the variables pagelikes, advertisement status
 (paid), or post type are related to post interactions. "


# iv. 
"Had our significance level been 0.06, then we would have had enough evidence to 
reject H0 and conclude that at least one of our variables are related to the count
of interactions a post receives. 
In other words, we almost have enough evidence to conclude that at least one 
of the X's are related to interactions. 
"