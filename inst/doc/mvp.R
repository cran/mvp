## ----set-options, echo = FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", dev = "png", fig.width = 7, fig.height = 3.5, message = FALSE, warning = FALSE)
options(width = 80, tibble.width = Inf)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/mvp.png", package = "mvp"))

## ----simplemult---------------------------------------------------------------
library("mvp",quietly=TRUE)
library("magrittr")
(p <- as.mvp("3 x y + z^3 + x y^6 z"))

## ----stoatgoat----------------------------------------------------------------
(M <- as.mvp("3 stoat goat^6 -4 + 7 stoatboat^3 bloat -9 float boat goat gloat^6"))
dput(M)

## ----printlex-----------------------------------------------------------------
print(M,order="lex", varorder=c("stoat","goat","boat","bloat","gloat","float","stoatboat"))

## ----arithasexpected----------------------------------------------------------
(S1 <- rmvp(5,2,2,4))
(S2 <- rmvp(5,2,2,4))
S1 + S2
S1 * S2
S1^2

## ----s3subs-------------------------------------------------------------------
(S3 <- as.mvp("x + 5 x^4 y + 8 y^2 x z^3"))

## ----subsxequal1--------------------------------------------------------------
subs(S3, x = 1)

## ----naturalidiom-------------------------------------------------------------
subs(S3, x = 1, y = 2, z = 3)

## ----suppresscoercion---------------------------------------------------------
subs(S3, x = 1, y = 2, z = 3, drop=FALSE)

## ----subavar------------------------------------------------------------------
subs(as.mvp("a+b+c"), a="x^6")

## ----subsorder----------------------------------------------------------------
subs(as.mvp("a+b+c"), a="x^6",x="1+a")
subs(as.mvp("a+b+c"), x="1+a",a="x^6")

## ----workswellwithpipes-------------------------------------------------------
as.mvp("a+b") %>% subs(a="a^2+b^2") %>% subs(b="x^6")

## ----subvec_example-----------------------------------------------------------
p <- rmvp(6,2,2,letters[1:3])
p
subvec(p,a=1,b=2,c=1:5)   # supply a named list of vectors

## ----diffisimp----------------------------------------------------------------
(S <- as.mvp("a + 5 a^5*b^2*c^8 -3 x^2 a^3 b c^3"))
deriv(S, letters[1:3])
deriv(S, rev(letters[1:3]))  # should be the same.

## ----aderivisalso-------------------------------------------------------------
aderiv(S, a = 3, b = 1, c = 2)

## ----pipenice-----------------------------------------------------------------
S %<>% aderiv(a=1,b=2) %>% subs(c="x^4") %>% `+`(as.mvp("o^99"))
S

## ----taylorX------------------------------------------------------------------
(X <- as.mvp("1+x+x^2 y")^3)
trunc(X,3)         # truncate, retain only terms with total power <= 3
trunc1(X,x=3)    # truncate, retain only terms with  power of x <= 3
onevarpow(X,x=3) # retain only terms with power of x == 3

## ----secondordertaylor--------------------------------------------------------
## second order taylor expansion of f(x)=sin(x+y) for x=1.1, about x=1:
sinxpy <- horner("x+y",c(0,1,0,-1/6,0,+1/120,0,-1/5040))  # sin(x+y)
dx <- as.mvp("dx")
t2 <- sinxpy  + aderiv(sinxpy,x=1)*dx + aderiv(sinxpy,x=2)*dx^2/2
(t2 %<>% subs(x=1,dx=0.1))  # (Taylor expansion of sin(y+1.1), left in symbolic form)
(t2 %>% subs(y=0.3))  - sin(1.4)  # numeric; should be small

## ----mvpseries----------------------------------------------------------------
p <- as.mvp("a^2 x b + x^2 a b + b c x^2 + a b c + c^6 x")
p
series(p,'x')

## ----subsxv-------------------------------------------------------------------
p %>% subs(x="xmv+a+b") %>% series("xmv") 

## ----seriespower--------------------------------------------------------------
p == p %>% subs(x="xmv+a+b") %>% subs(xmv="x-a-b")

## ----anyvar-------------------------------------------------------------------
as.mvp('x^3 + x*a') %>% subs(x="x_m_a + a") %>% series("x_m_a")

## ----betternonce--------------------------------------------------------------
as.mvp('x^2 + x*a+b^3') %>% subs(x="x_m_v + a^2+b") %>% series("x_m_v")

## ----wrapperonestep-----------------------------------------------------------
p <- as.mvp("1+x-x*y+a")^2
taylor(p,'x','a')

## ----implementdiff------------------------------------------------------------
P <- as.mvp("1 + z + y^2 + x*z^2 +  x*y")^4
onevarpow(P,x=1,y=2)

## ----negpowers----------------------------------------------------------------
(p <- as.mvp("1+x+x^2 y"))
invert(p)

## ----samerules----------------------------------------------------------------
p + as.mvp("z^6")

## ----introtodisord------------------------------------------------------------
a <- as.mvp("5 + 8*x^2*y - 13*y*x^2 + 11*z - 3*x*yz")
a
coeffs(a)

## ----disordpluscoeffs,error=TRUE----------------------------------------------
try({
b <- a*2
b
coeffs(a) + coeffs(b)
})

## ----disordRsingleobjectstuff-------------------------------------------------
coeffs(a) > 0
coeffs(a) + coeffs(a)^4

## ----disordextract------------------------------------------------------------
coeffs(a)[coeffs(a) > 0]

## ----disordnotallowed,error=TRUE----------------------------------------------
try({
coeffs(a)[coeffs(b) > 0]
})

## ----disordreplace------------------------------------------------------------
coeffs(a)[coeffs(a)<0] <- coeffs(a)[coeffs(a)<0] + 1000 # add 1000 to every negative coefficient
a

## ----usemagrittr--------------------------------------------------------------
coeffs(b)[coeffs(b)%%3==1] %<>% `+`(100)  # add 100 to every element equal to 1 modulo 3
b

## ----mvpzapsmall--------------------------------------------------------------
x <- as.mvp("1 - 0.11*x + 0.005*x*y")^2
x

## ----zapmvp-------------------------------------------------------------------
cx <- coeffs(x)
cx[abs(cx) < 0.01] <- 0
coeffs(x) <- cx
x

## ----knightgener--------------------------------------------------------------
knight(2)

## ----constknight--------------------------------------------------------------
constant(knight(4)^4)

