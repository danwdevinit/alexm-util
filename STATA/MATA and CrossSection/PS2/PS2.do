***Originally Created by Alex Miller on 02/18/2014
***filename: PS2.do
***Purpose: Complete problem set 2

**Problem 2

clear

webuse set "http://rlhick.people.wm.edu/econ407/data"
webuse fertility

*a
foreach v of var agefbrth educ knowmeth usemeth agefm idlnchld electric radio tv urban age frsthalf heduc { 
	drop if missing(`v') 
}
drop if educ0==1
drop if evermarr==0
reg agefbrth educ knowmeth usemeth agefm idlnchld electric radio tv urban age , robust
est store ols

*d
reg educ frsthalf knowmeth usemeth agefm idlnchld electric radio tv urban age
test frsthalf=0

ivreg agefbrth knowmeth usemeth agefm idlnchld electric radio tv urban age (educ=frsthalf), robust
est store iv

esttab ols iv, se

*e
mata
mata clear

function inv(X)
{
	return(issymmetric(X)?invsym(X):luinv(X))
}

y=st_data(.,("agefbrth"))
x=st_data(.,("educ", "knowmeth", "usemeth", "agefm", "idlnchld", "electric", "radio", "tv", "urban", "age"))
x=(J(rows(x),1,1),x);
n = rows(x)
k = cols(x)

/*OLS*/
b = inv(x'x)*(x'y);
b
yhat = x*b;
ehat = y-yhat;

e2 = ehat*ehat'
vhat=diag(e2)
v_robust = inv(x'x)*x'*vhat*x*inv(x'x)*(n/(n-k))
se_robust = sqrt(diagonal(v_robust))
se_robust

/*IVREG*/
xsansk=st_data(.,("knowmeth", "usemeth", "agefm", "idlnchld", "electric", "radio", "tv", "urban", "age"))
z=(J(rows(xsansk),1,1),st_data(.,("frsthalf")),xsansk);

biv = inv(z'x)*(z'y);
biv
yhativ = x*biv;
ehativ = y-yhativ;

e2iv = ehativ*ehativ'
vhativ=diag(e2iv)

xk = x[.,2]
theta = inv(z'z)*(z'xk)
xkhat = z*theta
xhat = (J(rows(x),1,1),xkhat,xsansk);

v_robustiv = inv(xhat'x)*xhat'*vhativ*xhat*inv(xhat'x)*(n/(n-k))
se_robustiv = sqrt(diagonal(v_robustiv))
se_robustiv

end

*f
reg educ frsthalf knowmeth usemeth agefm idlnchld electric radio tv urban age
predict r, resid
reg agefbrth educ knowmeth usemeth agefm idlnchld electric radio tv urban age r, robust
test r=0
drop r

ivreg2 agefbrth knowmeth usemeth agefm idlnchld electric radio tv urban age (educ=frsthalf)

*g
reg educ frsthalf ceb knowmeth usemeth agefm idlnchld electric radio tv urban age
test frsthalf=0
test ceb=0
test frsthalf ceb

ivreg agefbrth knowmeth usemeth agefm idlnchld electric radio tv urban age (educ=frsthalf ceb), robust
est store iv2

esttab ols iv iv2, se

*h
reg educ frsthalf ceb knowmeth usemeth agefm idlnchld electric radio tv urban age
predict r, resid
reg agefbrth educ knowmeth usemeth agefm idlnchld electric radio tv urban age r, robust
test r=0
drop r

*i
ivreg2 agefbrth knowmeth usemeth agefm idlnchld electric radio tv urban age (educ=frsthalf ceb)





