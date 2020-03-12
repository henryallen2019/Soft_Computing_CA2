varinp.mf <- matrix(c(2, 0, 20, 40, NA, 4, 20, 40, 60, 80, 3, 60, 80, 100, NA,
                      2, 0, 35, 75, NA, 3, 35, 75, 100, NA,
                      2, 0, 20, 40, NA, 1, 20, 50, 80, NA, 3, 60, 80, 100, NA,
                      2, 0, 20, 40, NA, 4, 20, 40, 60, 80, 3, 60, 80, 100, NA),
                    nrow = 5, byrow = FALSE)
                    
num.fvalinput <- matrix(c(3, 2, 3, 3), nrow=1)
varinput.1 <- c("a1", "a2", "a3")
varinput.2 <- c("b1", "b2")
varinput.3 <- c("c1", "c2", "c3")
varinput.4 <- c("d1", "d2", "d3")
names.varinput <- c(varinput.1, varinput.2, varinput.3, varinput.4)
range.data <- matrix(c(0,100, 0, 100, 0, 100, 0, 100, 0, 100), nrow=2)

type.defuz <- "WAM"
type.tnorm <- "MIN"
type.snorm <- "MAX"
type.implication.func <- "ZADEH"
name <- "Sim-0"
newdata<- matrix(c(25, 40, 35, 15, 45, 75, 78, 70, 15, 45, 38, 30), nrow= 3, byrow = TRUE)
colnames.var <- c("input1", "input2", "input3", "input4", "output1")

type.model <- "TSK"

func.tsk<-matrix(c(1, 1, 5, 2, 1, 3, 1, 0.5, 0.1, 2, 1, 3, 2, 2, 2), nrow=3, byrow=TRUE)
rule <- matrix(c("very a1","and","b1","and","slghtly c1","and","d1","->",
                 "a2","and","extremely b2","and","c2","and","d2", "->",  
                 "a3","and","b2","and","c2","and","dont_care", "->"),
               nrow=3, byrow=TRUE)
object <- frbs.gen(range.data, num.fvalinput, names.varinput, num.fvaloutput = NULL,
                   varout.mf = NULL, names.varoutput = NULL, rule,
                   varinp.mf, type.model, type.defuz = NULL, type.tnorm, type.snorm, func.tsk, colnames.var,
                   type.implication.func, name)
plotMF(object)
res <- predict(object, newdata)$predicted.val
print(res)
