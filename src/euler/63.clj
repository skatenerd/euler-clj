;we are trying to find pairs, (k,n) such that k^n has n digits
;
;observation: for k=10, no n will work
;for k >= 10, no n will work for that matter.  there's too many digits!
;so, we're only interested in the numbers 1-9.  We raise the powers on them until they "fall behind"
;(they have too few digits)
;
;note:
;log_10(k^n) = log_k(k^n) / log_k(10)
;            = n / log_k(10)
;so:
;number-of-digits(k,n) =  log_10(k^n) + 1
;number-of-digits(k,n) =  n/log_k(10) + 1
;
;we are interested in finding out when the number of digits "falls behind" n, for a given k
;so we solve:
;n = number-of-digits(k,n)
;n = n/log_k(10) + 1
;-1 = (n/log_k(10)) - n
;-1 = n * (1/log_k(10) - 1)
;-1 / ((1/log_k(10)) - 1) = n
;
;For a given K, any n's higher than this quantity (-1 / ((1/log_k(10)) - 1)) won't have enough digits.
;
;so, for the problem, we want to find:
;
;sum_{k=1}^{k=9} floor(-1 / ((1/log_k(10)) - 1))
;
;use this in wolfram alpha:
;"sum floor(-1 / ((1/log_k(10)) - 1)) k from 1 to 9"

