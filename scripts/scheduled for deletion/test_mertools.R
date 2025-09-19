library(merTools)
library(lme4)

# Test the predictInterval function with a simple example
data(sleepstudy)
model <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)

# This should work - predict for existing data
pred1 <- predictInterval(model, newdata = sleepstudy, level = 0.95, n.sims = 500)
print(head(pred1))

# Create a smaller dataset with identical columns but fewer rows
# This might reproduce the error if it's caused by data mismatch
sleepstudy_subset <- sleepstudy[1:10, ]
pred2 <- predictInterval(model, newdata = sleepstudy_subset, level = 0.95, n.sims = 500)
print(head(pred2))

# Try with different parameters to test the "which" argument
pred3 <- predictInterval(model, newdata = sleepstudy, level = 0.95, n.sims = 500, which = "fixed")
print(head(pred3))
