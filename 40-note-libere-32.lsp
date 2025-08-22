================

 NOTE LIBERE 32

================

    "Cuius rei demonstrationem mirabilem sane detexi
     hanc marginis exiguitas non caperet." Pierre de Fermat

----------------------------------------
Somma dei numeri che non dividono N
----------------------------------------

Sequenza OEIS A024816:
Antisigma(n): Sum of the numbers less than n that do not divide n.
  0, 0, 2, 3, 9, 9, 20, 21, 32, 37, 54, 50, 77, 81, 96, 105, 135, 132, 170,
  168, 199, 217, 252, 240, 294, 309, 338, 350, 405, 393, 464, 465, 513, 541,
  582, 575, 665, 681, 724, 730, 819, 807, 902, 906, 957, 1009, 1080, 1052,
  1168, 1182, 1254, 1280, 1377, 1365, ...

(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))

(define (divisors num)
"Generate all the divisors of an integer number"
  (local (f out)
    (cond ((= num 1) '(1))
          (true
           (setq f (factor-group num))
           (setq out '())
           (divisors-aux 0 1)
           (sort out)))))
; auxiliary function
(define (divisors-aux cur-index cur-divisor)
  (cond ((= cur-index (length f))
         (push cur-divisor out -1)
        )
        (true
         (for (i 0 (f cur-index 1))
           (divisors-aux (+ cur-index 1) cur-divisor)
           (setq cur-divisor (* cur-divisor (f cur-index 0)))))))

Funzione 1
----------
(define (seq1 num)
  (- (apply + (sequence 1 num)) (apply + (divisors num))))

(map seq1 (sequence 1 54))
;-> (0 0 2 3 9 9 20 21 32 37 54 50 77 81 96 105 135 132 170
;->  168 199 217 252 240 294 309 338 350 405 393 464 465 513 541
;->  582 575 665 681 724 730 819 807 902 906 957 1009 1080 1052
;->  1168 1182 1254 1280 1377 1365)

Funzione 2
----------
(define (seq2 num)
  (apply + (filter (fn(x) (!= (% num x) 0)) (sequence 1 num))))

(map seq2 (sequence 1 54))
;-> (0 0 2 3 9 9 20 21 32 37 54 50 77 81 96 105 135 132 170
;->  168 199 217 252 240 294 309 338 350 405 393 464 465 513 541
;->  582 575 665 681 724 730 819 807 902 906 957 1009 1080 1052
;->  1168 1182 1254 1280 1377 1365)

Test di correttezza:

(= (map seq1 (sequence 1 100)) (map seq2 (sequence 1 100)))
;-> true

Test di velocità:

(time (map seq1 (sequence 1 100)) 1e3)
;-> 469.732

(time (map seq2 (sequence 1 100)) 1e3)
;-> 989.143


============================================================================

