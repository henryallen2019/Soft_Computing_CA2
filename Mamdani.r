library(sets)
library(splines)
sets_options("universe", seq(0,255))
variables <- tuple(
  SD = fuzzy_variable(NL = fuzzy_trapezoid(corners = c(-10,0,31,63)), NM = fuzzy_triangular(corners = c(31,63,95)), NS = fuzzy_triangular(corners = c(63,95,127)), ZE = fuzzy_triangular(corners = c(95,127,159)), PS = fuzzy_triangular(corners = c(127,159,191)), PM = fuzzy_triangular(corners = c(159,191,227)), PL = fuzzy_trapezoid(corners = c(191,227,255,370))),
  AC = fuzzy_variable(NL = fuzzy_trapezoid(corners = c(-10,0,31,63)), NM = fuzzy_triangular(corners = c(31,63,95)), NS = fuzzy_triangular(corners = c(63,95,127)), ZE = fuzzy_triangular(corners = c(95,127,159)), PS = fuzzy_triangular(corners = c(127,159,191)), PM = fuzzy_triangular(corners = c(159,191,227)), PL = fuzzy_trapezoid(corners = c(191,227,255,370))),
  TC = fuzzy_variable(NL = fuzzy_trapezoid(corners = c(-10,0,31,63)), NM = fuzzy_triangular(corners = c(31,63,95)), NS = fuzzy_triangular(corners = c(63,95,127)), ZE = fuzzy_triangular(corners = c(95,127,159)), PS = fuzzy_triangular(corners = c(127,159,191)), PM = fuzzy_triangular(corners = c(159,191,227)), PL = fuzzy_trapezoid(corners = c(191,227,255,370)))
)

# Fuzzy rules
rules <- set(
  fuzzy_rule(SD %is% NL && AC %is% ZE, TC %is% PL),
  fuzzy_rule(SD %is% ZE && AC %is% NL, TC %is% PL),
  fuzzy_rule(SD %is% NM && AC %is% ZE, TC %is% PM),
  fuzzy_rule(SD %is% NS && AC %is% PS, TC %is% PS),
  fuzzy_rule(SD %is% PS && AC %is% NS, TC %is% NS),
  fuzzy_rule(SD %is% PL && AC %is% ZE, TC %is% NL),
  fuzzy_rule(SD %is% ZE && AC %is% NS, TC %is% PS),
  fuzzy_rule(SD %is% ZE && AC %is% NM, TC %is% PM)
)

model <- fuzzy_system(variables, rules)
plot(model)

example.1 <- fuzzy_inference(model, list(SD = 100, AC = 70))
#membership functions
print(example.1)

#rule strength computation
ans.1 <- gset_defuzzify(example.1, "centroid")
ans.2 <- gset_defuzzify(example.1, "meanofmax")
ans.3 <- gset_defuzzify(example.1, "smallestofmax")
ans.4 <- gset_defuzzify(example.1, "largestofmax")

plot(example.1)
print("Centroid method:")
print(ans.1)
