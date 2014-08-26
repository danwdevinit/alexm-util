***Originally Created by Alex Miller on 02/04/2012
***filename: PS1.do
***Purpose: Complete problem set 1

**Problem 1
mata

mata clear

educ=(2\3\2\1\1);

x=J(5,4,.);

for (i=1; i<=rows(x); i++) {
		x[i,1]=1;
        x[i,2]=(educ[i]==1?1:0);
        x[i,3]=(educ[i]==2?1:0);
        x[i,4]=(educ[i]==3?1:0);
};

x;
rank(x);
rank(x)==cols(x);

end

**Problem 2
mata

mata clear

z=(2,6\9,2);
m=(4\8);

/*a*/
luinv(z);
z*luinv(z);

/*b*/
z'z;
rows(z'z)==cols(z'z);

/*c*/
(z*m)'==m'z';

end

**Problem 3
mata

mata clear

y=(0.5377\2.8339\-0.2588);
x=(1\2\3);
x=(J(rows(x),1,1),x);

/*a*/
b = invsym(x'x)*(x'y);
b
yhat = x*b;
ehat = y-yhat;

/*b*/
round(x'ehat,0.01)==J(rows(x'ehat),cols(x'ehat),0);


end

**Problem 6

clear

webuse set "http://rlhick.people.wm.edu/econ407/data"
webuse mroz

*a
sum *
sum * if lfp==1

hist lfp
hist whrs
hist kl6
hist k618
hist wa
hist we
hist ww
hist rpwg
hist hhrs
hist ha
hist he
hist hw
hist faminc
hist mtr
hist wmed
hist wfed
hist un
hist cit
hist ax

*b
drop if  lfp==0
reg whrs ww faminc kl6 k618 un cit wmed wfed
estat hettest

reg whrs ww faminc kl6 k618 un cit wmed wfed, robust

*c
*1
mata

mata clear

y=st_data(.,("whrs"))
x=st_data(.,("ww", "faminc", "kl6", "k618", "un", "cit", "wmed", "wfed"))

x=(J(rows(x),1,1),x);

b = invsym(x'x)*(x'y);
b
yhat = x*b;
ehat = y-yhat;

s2 = (ehat'ehat)/(rows(x)-rows(b))

rmse=sqrt(s2)

varcov = invsym(x'x)*s2
se=sqrt(diagonal(varcov))

tstat = diagonal(diag(b) * invsym(diag(se)))

/*2*/

ybar = mean(y)
r2 = 1-((ehat'ehat)/((y:-ybar)'(y:-ybar)))

/*3*/

e2 = ehat*ehat'
vhat=diag(e2)
rvarcov = invsym(x'x)*x'*vhat*x*invsym(x'x)
robustse = sqrt(diagonal(rvarcov))

end

*d
reg whrs ww faminc kl6 k618 un cit wmed wfed
estat hettest

predict e, resid
predict yhat, xb
gen r = (e^2)/(e(rss)/e(N))
reg r yhat
display e(mss)/2




