str = 'keith, english 67, maths 56, science 34'

def isNumber(s):
    try:
        int(s)
        return True
    except:
        return False


splitstr = [substr.strip().split(' ') for substr in str.split(',')]
for pair in splitstr:
    if len(pair)>1:
        if isNumber(pair[1]):
            number = int(pair[1])
        else:
            number = "Is not a number"
    else:
        number = "There is no number"
    print(pair[0],number)