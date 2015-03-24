gen correct=.
replace correct=1 if pget>=.5 & netget==1
replace correct=1 if pget<.5 & netget==0
replace correct=0 if pget<.5 & netget==1
replace correct=0 if pget>=.5 & netget==0

predict double pget, p
gen correct=.
gen correcttrue=.
gen correctfalse=.
replace correct=1 if pget>=.5 & getwb==1
replace correct=1 if pget<.5 & getwb==0
replace correct=0 if pget<.5 & getwb==1
replace correct=0 if pget>=.5 & getwb==0
replace correcttrue=1 if pget>=.5 & getwb==1
replace correcttrue=0 if pget<.5 & getwb==1
replace correctfalse=1 if pget<.5 & getwb==0
replace correctfalse=0 if pget>=.5 & getwb==0
sum correct correcttrue correctfalse


predict double pfail2, p
gen correct2=.
gen correcttrue2=.
gen correctfalse2=.
replace correct2=1 if pfail2>=.5 & wfail2==1
replace correct2=1 if pfail2<.5 & wfail2==0
replace correct2=0 if pfail2<.5 & wfail2==1
replace correct2=0 if pfail2>=.5 & wfail2==0
replace correcttrue2=1 if pfail2>=.5 & wfail2==1
replace correcttrue2=0 if pfail2<.5 & wfail2==1
replace correctfalse2=1 if pfail2<.5 & wfail2==0
replace correctfalse2=0 if pfail2>=.5 & wfail2==0
sum correct2 correcttrue2 correctfalse2
