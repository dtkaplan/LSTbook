# make a renamed copy of palmerpenguins::penguins
# var names shouldn't include the units.

Penguins <- palmerpenguins::penguins
Penguins <- Penguins[complete.cases(Penguins),]
names(Penguins)[3:6] <- c("bill_length", "bill_depth", "flipper", "mass")
save(Penguins, file="data/Penguins.rda")
