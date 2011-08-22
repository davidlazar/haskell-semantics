--Hi there all, this is a quine for hugs by Andrew Mitchell
--just run with the command main
a1 z zz = [zz] ++ e ++ init (init (concat [x:c|x <- z]))
a2 = unlines ([((f!!z):[]) ++ e ++ t ++ (m!!z) ++ t | z <- [0..15]] ++ [[yf!!(2*z)]++[yf!!(2*z+1)] ++ e ++ t ++ (ym!!z) ++ t | z <- [0..11]])
b = "a1 z zz = [zz] ++ e ++ init (init (concat [x:c|x <- z]))"
c = "++"
d = "quine"
e = " = "
f = "bcdefghijklnopwx"
g = "a2 = unlines ([((f!!z):[]) ++ e ++ t ++ (m!!z) ++ t | z <- [0..15]] ++ [[yf!!(2*z)]++[yf!!(2*z+1)] ++ e ++ t ++ (ym!!z) ++ t | z <- [0..11]])"
h = "t"
i = "u"
j = "v"
k = "n"
l = "h++e++t++v++t++t"
n = "i++e++t++v++k++t"
o = "j++e++t++v++v++t"
p = "main = putStr (unlines ([yj] ++ [yk] ++ [b] ++ [g] ++ [a2] ++ [q] ++ [r] ++ [s] ++ [a1 ya (head yd)] ++ [a1 yb (head ye)] ++ [a1 yc (head yg)] ++ [yh] ++ [yi] ++ [p]))"
w = "["
x = "]"
ya = "hetvtt"
yb = "ietvkt"
yc = "jetvvt"
yd = "q"
ye = "r"
yf = "yaybycydyeyfygyhyiyjykyl"
yg = "s"
yh = "ym = [ya,yb,yc,yd,ye,yf,yg,yh,yi,yj,yk,yl]"
yi = "m = [b,c,d,e,f,g,h,i,j,k,l,n,o,p,w,x]"
yj = "--Hi there all, this is a quine for hugs by Andrew Mitchell"
yk = "--just run with the command main"
yl = ""

t = "\""
u = "\n"
v = "\\"
q = h++e++t++v++t++t
r = i++e++t++v++k++t
s = j++e++t++v++v++t
ym = [ya,yb,yc,yd,ye,yf,yg,yh,yi,yj,yk,yl]
m = [b,c,d,e,f,g,h,i,j,k,l,n,o,p,w,x]
main = putStr (unlines ([yj] ++ [yk] ++ [b] ++ [g] ++ [a2] ++ [q] ++ [r] ++ [s] ++ [a1 ya (head yd)] ++ [a1 yb (head ye)] ++ [a1 yc (head yg)] ++ [yh] ++ [yi] ++ [p]))
