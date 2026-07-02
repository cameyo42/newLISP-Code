================

 NOTE LIBERE 37

================

  "A clueless amateur"

---------------------------------------------------
Sottolista più lunga con elementi di un'altra lista
---------------------------------------------------

Abbiamo due liste L1 e L2 che contengono elementi di diverso tipo.
Determinare la sottolista più lunga di L1 che contiene solo elementi di L2.

Esempio:
L1 = (11 a 21 3 b "a" c d 3 4 5)
L2 = (a b c d 1 2 3)
Sottolista di L1 = (c d 3)

(define (sub-max L1 L2)
  (local (max-len max-lst cur-len cur-seq)
    (setq max-len 0)
    (setq max-lst '())
    (setq cur-len 0)
    (setq cur-seq '())
    (dolist (el L1)
      (cond
        ; se l'elemento corrente si trova in L2,
        ; allora lo aggiunge alla sottolista corrente
        ((find el L2) (push el cur-seq -1))
        (true
          (setq cur-len (length cur-seq))
          ; aggiornare la sotttolista massima?
          (when (> cur-len max-len)
            (setq max-len cur-len)
            (setq max-lst cur-seq))
            ; azzera la sottosequenza
            (setq cur-len 0)
            (setq cur-seq '()))))
    ; se ultimo elemento di L1 si trova in L2,
    ; allora dobbiamo analizzare l'ultima sottolista generata
    (setq cur-len (length cur-seq))
    (when (> cur-len max-len)
      (setq max-len cur-len)
      (setq max-lst cur-seq))
    (list max-len max-lst)))

Proviamo:

(setq L1 '(11 a 21 3 b "a" c d 3 4 5))
(setq L2 '(a b c d 1 2 3))
(sub-max L1 L2)
;-> (3 (c d 3))

(setq L1 '(11 a 21 3 b "a" c d 3 4 5 a b c d))
(setq L2 '(a b c d 1 2 3))
(sub-max L1 L2)
;-> (4 (a b c d))

Complessità temporale: O(length(L1) * length(L2))


-----------------------------
Modulo/resto tra due frazioni
-----------------------------

Metodo del denominatore comune
Per calcolare mod(f1, f2) con le frazioni:
1) Trasformare entrambe le frazioni in modo che abbiano lo stesso denominatore.
2) Calcolare il resto (modulo) tra i due nuovi numeratori: mod(a/c,b/c) = (a mod b)/c

Quindi prima bisogna rendere le due frazioni con lo stesso denominatore e poi applicare il modulo sui numeratori.
Dato: f1 = (a c) e f2 = (b d)
1. Denominatore comune (LCM dei denominatori): l = lcm(c d)
2. Scala i numeratori: a' = a * (l / c) e b' = b * (l / d)
3. Modulo sui numeratori: r = a' mod b'
4. Risultato: (r l)

(define (lcm a b) (/ (* a b) (gcd a b)))

(define (frac-mod f1 f2)
  (letn ((a (f1 0))  (c (f1 1))
         (b (f2 0))  (d (f2 1))
         (l (lcm c d))
         (na (mul a (div l c)))
         (nb (mul b (div l d)))
         (r (mod na nb)))
    (list r l)))

(frac-mod '(7 12) '(5 18))
;-> (1 36)


--------------------
Teorema di Dirichlet
--------------------

Dato un intero positivo dispari N trovare due numeri primi p e q tali che N = p & q.

Dato n dispari, per il teorema di Dirichlet esistono numeri primi:

  p = n + k*2^n
  q = n + l*2^n

tali che: p & q = n

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (pow-i num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (pot out)
        (setq pot (pow-i num (/ power 2)))
        (if (odd? power)
            (setq out (* num pot pot))
            (setq out (* pot pot)))
        out)))

; Funzione che dato N (dispari) genera i primi p e q ()
; (solo per N < 20, dopo occorrono i big-integer)
(define (diri n)
  (local (out p q stop limite)
    (setq limite 100)
    (setq out '())
    (setq stop nil)
    (for (k 0 limite 1 stop)
      (for (l 0 limite 1 stop)
        (setq p (+ n (* (pow-i 2L n) k)))
        (setq q (+ n (* (pow-i 2L n) l)))
        (when (and (= n (& p q)) (> p n) (> q n) (prime? q) (prime? p))
              (setq stop true)
              (setq out (list n p q (& p q))))))
    out))

Proviamo:

(map diri (sequence 1 19 2))
;-> ((1 3 5 1) (3 11 19 3) (5 37 197 5) (7 263 647 7) (9 521 1033 9)
;->  (11 14347 32779 11) (13 40973 655373 13) (15 32783 65551 15)
;->  (17 786449 2097169 17) (19 2621459 9437203 19))


------------------
Funzioni alternate
------------------

Scrivere una funzione che trasforma un numero intero positivo N in base alle seguenti regole:
1) se N diviso per 6 da resto 0, calcolare (sin 1/N)
2) se N diviso per 6 da resto 1, calcolare (cos 1/N)
3) se N diviso per 6 da resto 2, calcolare (tan 1/N)
4) se N diviso per 6 da resto 3, calcolare (asin 1/N)
5) se N diviso per 6 da resto 4, calcolare (acos 1/N)
6) se N diviso per 6 da resto 5, calcolare (atan 1/N)
7) in tutti gli altri casi restituire N
La funzione deve essere la più corta possibile.

Notiamo che la regola 7) non è mai applicabile perchè qualunque intero diviso per 6 produce uno dei resti 0,1,2,3,4 e 5.

(define (calc N)
  (let ((op (list sin cos tan asin acos atan)) ; lista delle operazioni
        (idx (% N 6))) ; indice dell'operazione
    ;(println (op idx) { } N)
    ((op idx) (div N)))) ; esecuzione dell'operazione

(map calc (sequence 1 10))
;-> (0.5403023058681398 0.5463024898437905 0.3398369094541219
;->  1.318116071652818 0.1973955598498808 0.165896132693415
;->  0.9898132604466151 0.125655136575131 0.1113410143409639
;->  1.470628905633337)

Versione code-golf (68 caratteri):
(define(f n(o(list sin cos tan asin acos atan)))((o(% n 6))(div n)))

(= (map calc (sequence 1 50)) (map f (sequence 1 50)))
;-> true


-----------------------------------------
Bitwise and, or, xor, not per big-integer
-----------------------------------------

Scriviamo le funzioni bitwise AND, OR, XOR e NOT per big-integer.

Algoritmo (solo per numeri non negativi)
1) Convertire i due big-integer in stringhe binarie.
2) Allineare le stringhe aggiungendo zeri a sinistra.
3) Eseguire l'operazione bit a bit sui caratteri.
4) Convertire il risultato in decimale big-integer.

Conversione da decimale a binario (big-integer):

(define (dec-binL num)
  (let (MAXINT 4611686018427387904L) ; (2^62)
    (define (prep s) (string (dup "0" (- 62 (length s))) s))
    (if (<= num MAXINT) (bits (int num))
      (string (dec-binL (/ num MAXINT))
              (prep (bits (int (% num MAXINT))))))))

(bits 9223372036854775807)
;-> "111111111111111111111111111111111111111111111111111111111111111"
(dec-binL 9223372036854775807)
;-> "111111111111111111111111111111111111111111111111111111111111111"

Conversione da binario a decimale (big-integer):

(define (bin-decL binary)
  (let (num 0L)
    (dolist (b (explode binary)) (setq num (+ (* num 2L) (int b))))
    num))

(dec-binL 102394746378492383765)
;-> "1011000110100000011001101110101001000010100010000010001001000010101"
(bin-decL "1011000110100000011001101110101001000010100010000010001001000010101")
;-> 102394746378492383765L

(= 102394746378492383765 (bin-decL (dec-binL 102394746378492383765)))
;-> true

; aggiunge zeri a sinistra fino a lunghezza len
(define (pad-left str len)
  (string (dup "0" (- len (length str))) str))

; allinea due stringhe binarie
(define (align-bin b1 b2)
  (let (len (max (length b1) (length b2)))
    (list (pad-left b1 len)
          (pad-left b2 len))))

; bitwise AND
(define (andL a b)
  (letn ((ab (align-bin (dec-binL a) (dec-binL b)))
         (s1 (ab 0))
         (s2 (ab 1))
         (out ""))
    (for (i 0 (- (length s1) 1))
      (push (if (and (= (s1 i) "1") (= (s2 i) "1")) "1" "0")
            out -1))
    (bin-decL out)))

; bitwise OR
(define (orL a b)
  (letn ((ab (align-bin (dec-binL a) (dec-binL b)))
         (s1 (ab 0))
         (s2 (ab 1))
         (out ""))
    (for (i 0 (- (length s1) 1))
      (push (if (or (= (s1 i) "1") (= (s2 i) "1")) "1" "0")
            out -1))
    (bin-decL out)))

; bitwise XOR
(define (xorL a b)
  (letn ((ab (align-bin (dec-binL a) (dec-binL b)))
         (s1 (ab 0))
         (s2 (ab 1))
         (out ""))
    (for (i 0 (- (length s1) 1))
      (push (if (!= (s1 i) (s2 i)) "1" "0")
            out -1))
    (bin-decL out)))

; bitwise NOT su n bit
; Osservazione: NOT non è ben definito senza specificare una larghezza in bit.
; La funzione 'notL' inverte solo la rappresentazione binaria minima di 'a'.
(define (notL a)
  (letn ((s (dec-binL a))
         (out ""))
    (for (i 0 (- (length s) 1))
      (push (if (= (s i) "1") "0" "1")
            out -1))
    (bin-decL out)))

; bitwise NOT (trick)
(define (not-L a) (- (++ a)))

Proviamo:

Verifica della correttezza delle funzioni:
(confronto con le primitive AND '&', OR '|' e XOR '^')

(define (test n)
  (for (a 1 n)
    (for (b 1 n)
      ; ~ e notL producono risultati diversi
      ; (if (!= (~ a) (notL a)) (println a { } "notL"))
      (if (!= (& a b) (andL a b)) (println a { } b { } "andL"))
      (if (!= (| a b) (orL a b)) (println a { } b { } "orL"))
      (if (!= (^ a b) (xorL a b)) (println a { } b { } "xorL")))))

(test 1000)
;-> nil

Con numeri big integer:

(setq c 302712037371203753750731235135L)
(setq d 2712037371203753750731235135L)

(andL c d)
;-> 1210111330292086977666879L
Wolfram-alpha: 1210111330292086977666879
(orL c d)
;-> 305422864631077215414484803391L
Wolfram-alpha: 305422864631077215414484803391
(xorL c d)
;-> 305421654519746923327507136512L
Wolfram-alpha: 305421654519746923327507136512
(notL c)
;-> 14200612685853596623444566208L
(not-L c)
;-> -302712037371203753750731235136L
Wolfram-alpha: -302712037371203753750731235136
(notL d)
;-> 2239722785937767348865261760L
(not-L d)
;-> -2712037371203753750731235136
Wolfram-Alpha: -2712037371203753750731235136


------------------------------------------
Matematica con le frazioni (rationale.lsp)
------------------------------------------

;; rationale.lsp
;; Rational library for newLISP 10.7.5
;; by cameyo 2026 (MIT-0 Licence)
;; version 0.2
;; Load library: (load "rationale.lsp")
;;
;; Specifications:
;; - Big integer handling
;; - The fraction sign always goes in the numerator and
;;   the denominator always remains positive.
;; - The output fraction is always reduced to lowest terms (normalized).
;; - The zero fraction is: (0 1) ((0 x) is normalized to (0 1)).
;; - The gcd of fractions is always positive
;; - Checks denominator for 0 --> throwing error
;; - Checks division by 0 --> throwing error
;;
;; List of functions:
;; (rat-big r):       "Convert fraction numbers to big integers"
;; (rat n d):         "Reduce a fraction to its lowest terms"
;; (+rat r1 r2):      "Add two fractions"
;; (-rat r1 r2):      "Subtract two fractions"
;; (*rat r1 r2):      "Multiply two fractions"
;; (/rat r1 r2):      "Divide two fractions"
;; (**rat r power):   "integer power of a fraction"
;; (rat-mod r1 r2):   "modulus/remainder of two fractions"
;; (rat-gcd):         "Greatest Common Divisor (gcd) of two or more fractions"
;; (rat-lcm):         "Least Common Multiple (lcm) of two or more fractions"
;; (rat< r1 r2):      "Greater than (>)"
;; (rat> r1 r2):      "Less than (<)"
;; (rat= r1 r2):      "Equal (=)"
;; (+r):              "Add two of more fractions"
;; (-r):              "Subtract two or more fractions"
;; (*r):              "Multiplicate two or more fractions"
;; (/r):              "Divide two or more fractions"
;----------------------------------------------------------------------------
; Calcola il Minimo Comune Multiplo (lcm) tra due interi
(define (lcm_ a b) (/ (* a b) (gcd a b)))
;----------------------------------------------------------------------------
; Calcola il Minimo Comune Multiplo (lcm) tra due o più interi
(define (lcm)
"Calculate the lcm of two or more number"
  (apply lcm_ (args) 2))
;----------------------------------------------------------------------------
; Calcola la potenza intera di un intero
(define (pow-i num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (pot out)
        (setq pot (pow-i num (/ power 2)))
        (if (odd? power)
            (setq out (* num pot pot))
            (setq out (* pot pot)))
        out)))
;----------------------------------------------------------------------------
; Riduce una frazione ai minimi termini
(define (rat n d)
"Reduce a fraction to its lowest terms"
  (cond ((zero? d) (throw-error "rat: denominator is zero"))
        ((zero? n) '(0 1))
    (true
      (letn ((g (gcd n d)) (nn (/ n g)) (dd (/ d g)))
        (if (< dd 0)
            (list (- nn) (- dd))
            (list nn dd))))))
;----------------------------------------------------------------------------
; Converte i numeri di una frazione in big-integer
(define (rat-big r)
"Convert fraction numbers to big integers"
  (list (bigint (r 0)) (bigint (r 1))))
;----------------------------------------------------------------------------
; Addiziona due frazioni
(define (+rat r1 r2)
"Add two fractions"
  (setq r1 (rat-big r1))
  (setq r2 (rat-big r2))
  (rat (+ (* (r1 0) (r2 1)) (* (r2 0) (r1 1)))
       (* (r1 1) (r2 1))))
;----------------------------------------------------------------------------
; Sottrae due frazioni
(define (-rat r1 r2)
"Subtract two fractions"
  (setq r1 (rat-big r1))
  (setq r2 (rat-big r2))
  (rat (- (* (r1 0) (r2 1)) (* (r2 0) (r1 1)))
       (* (r1 1) (r2 1))))
;----------------------------------------------------------------------------
; Moltiplica due frazioni
(define (*rat r1 r2)
"Multiply two fractions"
  (setq r1 (rat-big r1))
  (setq r2 (rat-big r2))
  (rat (* (r1 0) (r2 0))
       (* (r1 1) (r2 1))))
;----------------------------------------------------------------------------
; Divide due frazioni
(define (/rat r1 r2)
"Divide two fractions"
  (setq r1 (rat-big r1))
  (setq r2 (rat-big r2))
  (if (zero? (r2 0))
      (throw-error "/rat: division by zero")
      (rat (* (r1 0) (r2 1))
           (* (r1 1) (r2 0)))))
;----------------------------------------------------------------------------
; Calcola la potenza intera di una frazione
(define (**rat r power)
"Calculate the integer power of a fraction"
  (setq r (rat (bigint (r 0)) (bigint (r 1))))
  (cond ((zero? power) '(1 1))
        ((< power 0)
          (if (zero? (r 0))
              (throw-error "**rat: division by zero")
              (rat (pow-i (r 1) (- power)) (pow-i (r 0) (- power)))))
        (true
          (rat (pow-i (r 0) power) (pow-i (r 1) power)))))
;----------------------------------------------------------------------------
; Calcola il modulo/resto di due frazioni
(define (rat-mod r1 r2)
"Calculate the modulus/remainder of two fractions"
  (letn ( (r1 (rat-big r1))
          (r2 (rat-big r2)) )
    (if (zero? (r2 0))
        (throw-error "rat-mod: division by zero")
        (letn ( (a (r1 0)) (c (r1 1))
                (b (r2 0)) (d (r2 1))
                (l (lcm c d))
                (na (* a (/ l c)))
                (nb (* b (/ l d)))
                (r (% na nb)) )
          (rat r l)))))
;----------------------------------------------------------------------------
; Calcola il Massimo Comun Divisore (gcd) di due o più frazioni
(define (rat-gcd)
"Calculate the Greatest Common Divisor (gcd) of two or more fractions"
  (let (lst '())
    (doargs (f) (push (apply rat (rat-big f)) lst))
    (rat (apply gcd (map first lst))
         (apply lcm (map last lst)))))
;----------------------------------------------------------------------------
; Calcola il Minimo Comune Multiplo (lcm) di due o più frazioni
(define (rat-lcm)
"Calculate the Least Common Multiple (lcm) of two or more fractions"
  (let (lst '())
    (doargs (f) (push (apply rat (rat-big f)) lst))
    (rat (apply lcm (map (fn (x) (abs (first x))) lst))
         (apply gcd (map last lst)))))
;----------------------------------------------------------------------------
; Maggiore (>)
(define (rat> r1 r2)
"Relation operator between two fractions: Greater than (>)"
  (setq r1 (rat-big r1))
  (setq r2 (rat-big r2))
  (> (* (r1 0) (r2 1))
     (* (r2 0) (r1 1))))
;----------------------------------------------------------------------------
; Minore (<)
(define (rat< r1 r2)
"Relation operator between two fractions: Less than (<)"
  (setq r1 (rat-big r1))
  (setq r2 (rat-big r2))
  (< (* (r1 0) (r2 1))
     (* (r2 0) (r1 1))))
;----------------------------------------------------------------------------
; Uguale (=)
(define (rat= r1 r2)
"Comparison operator between two fractions: Equal (=)"
  (setq r1 (rat-big r1))
  (setq r2 (rat-big r2))
  (= (* (r1 0) (r2 1)) (* (r2 0) (r1 1))))
;----------------------------------------------------------------------------
; Addiziona due o più frazioni
(define (+r)
"Add two of more fractions"
  (apply +rat (args) 2))
;----------------------------------------------------------------------------
; Sottrae due o più frazioni
(define (-r)
"Subtract two or more fractions"
  (apply -rat (args) 2))
;----------------------------------------------------------------------------
; Moltiplica due o più frazioni
(define (*r)
"Multiplicate two or more fractions"
  (apply *rat (args) 2))
;----------------------------------------------------------------------------
; Divide due o più frazioni
(define (/r)
"Divide two or more fractions"
  (apply /rat (args) 2))
;----------------------------------------------------------------------------
; Test delle funzioni (basic)
;----------------------------------------------------------------------------
;(define (test desc expr expected)
;  (let (res (eval expr))
;    (if (= res expected)
;        (println "[OK]   " desc)
;        (println "[FAIL] " desc
;                 " -> " res
;                 " expected: " expected))))
;; rat
;(test "rat 2/4" '(rat 2 4) '(1 2))
;(test "rat -2/4" '(rat -2 4) '(-1 2))
;(test "rat 2/-4" '(rat 2 -4) '(-1 2))
;(test "rat -2/-4" '(rat -2 -4) '(1 2))
;(test "rat 0/5" '(rat 0 5) '(0 1))
;; +rat
;(test "1/2 + 1/3" '(+rat '(1 2) '(1 3)) '(5 6))
;(test "-1/2 + 1/2" '(+rat '(-1 2) '(1 2)) '(0 1))
;; -rat
;(test "1/2 - 1/3" '(-rat '(1 2) '(1 3)) '(1 6))
;(test "1/3 - 1/2" '(-rat '(1 3) '(1 2)) '(-1 6))
;; *rat
;(test "2/3 * 3/5" '(*rat '(2 3) '(3 5)) '(2 5))
;(test "0 * 3/5" '(*rat '(0 1) '(3 5)) '(0 1))
;; /rat
;(test "(2/3)/(3/5)" '(/rat '(2 3) '(3 5))'(10 9))
;(test "(0/1)/(3/5)" '(/rat '(0 1) '(3 5))'(0 1))
;; **rat
;(test "(2/3)^4" '(**rat '(2 3) 4) '(16 81))
;(test "(2/3)^(-2)" '(**rat '(2 3) -2) '(9 4))
;(test "(2/3)^0" '(**rat '(2 3) 0) '(1 1))
;(test "(0/1)^5" '(**rat '(0 1) 5) '(0 1))
;; rat-mod
;(test "7/6 mod 1/4" '(rat-mod '(7 6) '(1 4)) '(1 6))
;(test "5/6 mod 1/3" '(rat-mod '(5 6) '(1 3)) '(1 6))
;(test "1/2 mod 3/2" '(rat-mod '(1 2) '(3 2)) '(1 2))
;; rat-gcd
;(test "gcd(2/3,4/5)" '(rat-gcd '(2 3) '(4 5)) '(2 15))
;(test "gcd(6/7,9/14)" '(rat-gcd '(6 7) '(9 14)) '(3 14))
;; rat-lcm
;(test "lcm(2/3,4/5)" '(rat-lcm '(2 3) '(4 5)) '(4 1))
;(test "lcm(6/7,9/14)" '(rat-lcm '(6 7) '(9 14)) '(18 7))
;; confronti
;(test "1/2 > 1/3" '(rat> '(1 2) '(1 3)) true)
;(test "1/3 > 1/2" '(rat> '(1 3) '(1 2)) nil)
;(test "1/3 < 1/2" '(rat< '(1 3) '(1 2)) true)
;(test "1/2 < 1/3" '(rat< '(1 2) '(1 3)) nil)
;(test "2/4 = 1/2" '(rat= '(2 4) '(1 2)) true)
;(test "1/2 = 3/4" '(rat= '(1 2) '(3 4)) nil)
;; operatori variadici
;(test "+r" '(+r '(1 2) '(1 3) '(1 6)) '(1 1))
;(test "-r" '(-r '(3 2) '(1 2) '(1 4)) '(3 4))
;(test "*r" '(*r '(2 3) '(3 5) '(5 7)) '(2 7))
;(test "/r" '(/r '(2 3) '(3 5) '(5 7)) '(14 9))


----------------------------------
Prodotto dei divisori di un numero
----------------------------------

Scrivere una funzione che calcola il prodotto di tutti i divisori di un numero intero.

Sequenza OEIS A007955:
Product of divisors of n.
  1, 2, 3, 8, 5, 36, 7, 64, 27, 100, 11, 1728, 13, 196, 225, 1024, 17, 5832,
  19, 8000, 441, 484, 23, 331776, 125, 676, 729, 21952, 29, 810000, 31,
  32768, 1089, 1156, 1225, 10077696, 37, 1444, 1521, 2560000, 41, 3111696,
  43, 85184, 91125, 2116, 47, 254803968, 343, ...

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

; Funzione che calcola il prodotto dei divisori di un numero intero
(define (prod-div n)
  (apply * (divisors n)))

(map prod-div (sequence 1 40))
;-> (1 2 3 8 5 36 7 64 27 100 11 1728 13 196 225 1024 17 5832
;->  19 8000 441 484 23 331776 125 676 729 21952 29 810000 31
;->  32768 1089 1156 1225 10077696 37 1444 1521 2560000)

Possiamo usare un altro metodo (più veloce):
il prodotto di tutti i divisori di n è sempre n^((numero di divisori di n)/2).

(define (divisors-count num)
"Count the divisors of an integer number"
  (if (= num 1)
      1
      (let (lst (factor-group num))
        (apply * (map (fn(x) (+ 1 (last x))) lst)))))

(define (prod-div2 n)
  (pow n (div (divisors-count n) 2)))

(= (map prod-div (sequence 1 40)) (map prod-div2 (sequence 1 40)))
;-> true

(time (prod-div 200) 1e5)
;-> 651.793
(time (prod-div2 200) 1e5)
;-> 210.373


-----------------------------
Numeri primi con Miller-Rabin
-----------------------------

Il test di Miller–Rabin è un algoritmo per stabilire se un numero intero n è primo oppure composto.
Non prova la primalità in modo assoluto per tutti i numeri, ma identifica i composti con altissima affidabilità.
Per alcuni insiemi di basi e per intervalli limitati (ad esempio interi a 64 bit), diventa di fatto deterministico.

1) Idea di base
Si parte da un numero dispari n > 2.
Si scrive:
  n - 1 = d * 2^s
d è dispari e s >= 1.
Questo serve a separare la parte 'dispari' da quella con fattori di 2.

2) Scelta della base
Si sceglie un numero a tale che:
  2 <= a <= n - 2
Questo numero viene usato per testare la struttura modulare di n.

3) Primo test
Si calcola:
  x = a^d mod n
Se:
  x = 1 oppure x = n - 1
allora il numero supera questo test e potrebbe essere primo.

4) Test delle potenze successive
Se il primo test fallisce, si continua a elevare al quadrato:
  x = x^2 mod n
ripetendo fino a s - 1 volte.
Se in uno di questi passi si ottiene:
  x = n - 1
allora n passa il test per questa base.

5) Fallimento del test
Se nessuno dei valori raggiunge n - 1, allora:
  n è sicuramente composto.

6) Ripetizione su più basi
Il test viene ripetuto per più valori di a.
Se n passa tutti i test, allora è:
- probabilmente primo
- oppure un caso raro (pseudoprimo forte per quelle basi)

7) Caso pratico (numeri grandi)
Per numeri fino a circa 2^64, esiste un insieme fisso di basi che rende il test deterministico in pratica.
Questo significa che:
- nessun numero composto passa tutti i test
- il risultato è equivalente a un test deterministico

8) Perchè funziona
L'idea matematica è che in un campo modulo primo, le radici dell'unità hanno struttura molto rigida.
Se n è composto, questa struttura si rompe quasi sempre, tranne in casi molto rari (pseudoprimi forti).

9) Vantaggi
- molto veloce (logaritmico)
- funziona bene su numeri grandi (anche 100+ cifre)
- molto più efficiente della divisione per tentativi

10) Limite
Non è un test puramente deterministico per numeri arbitrari (a meno di basi speciali su intervalli limitati), ma in pratica è uno dei metodi più affidabili e usati in crittografia e teoria dei numeri applicata.

Caratteristiche:
powmod: esponenziazione modulare veloce per bigint.
mr-test: esegue un singolo round di Miller-Rabin per una base a.
prime-i?: prova più basi fisse e restituisce true solo se il numero supera tutti i test.

; ============================================================
; Modular exponentiation: computes (a^d mod n)
; Works with big integers safely
; Complexity: O(log d)
; ============================================================
(define (powmod a d n)
  (let (res 1L)
    ; reduce base modulo n at start
    (setq a (% a n))
    ; exponentiation by squaring
    (while (> d 0)
      ; if current exponent bit is 1, multiply result
      (if (!= (% d 2) 0)
        (setq res (% (* res a) n)))
      ; shift exponent right (divide by 2)
      (setq d (/ d 2))
      ; square base modulo n
      (setq a (% (* a a) n)))
    res))

; ============================================================
; Single Miller-Rabin round
; Returns:
;   true  -> "probably prime for this base"
;   nil   -> composite detected
; ============================================================
(define (mr-test n a)
  (letn (
          ; write n-1 = d * 2^s
          (d (- n 1))
          (s 0)
          ; working variable
          (x 0)
          ; flag for detecting success inside loop
          (found nil)
          (i 0)
        )
    ; factor out powers of 2 from n-1
    (while (zero? (% d 2))
      (setq d (/ d 2))
      (++ s))
    ; compute a^d mod n
    (setq x (powmod a d n))
    ; first Miller-Rabin condition
    (if (or (= x 1) (= x (- n 1)))
        true
        (begin
          ; square x up to s-1 times
          (setq i 1)
          (setq found nil)
          (while (and (<= i (- s 1)) (not found))
            ; x = x^2 mod n
            (setq x (% (* x x) n))
            ; if we hit n-1, this base passes
            (if (= x (- n 1)) (setq found true))
            (++ i))
          found))))

; ============================================================
; Full primality test using multiple bases
; For 64-bit integers, this is deterministic in practice
; ============================================================
(define (prime-i? n)
  (letn (
          ; fixed set of bases (good for large integers)
          (bases '(2L 3L 5L 7L 11L 13L 17L 19L 23L 29L 31L 37L))
          ; index and current base
          (i 0)
          (a 0)
          ; global primality flag
          (is-prime true)
        )
    ; small cases
    (cond
      ((< n 2) nil)
      ((or (= n 2) (= n 3)) true)
      ((zero? (% n 2)) nil)
      (true
        ; test all bases until failure
        (while (and is-prime (< i (length bases)))
          (setq a (bases i))
          ; skip invalid base
          (if (< a n)
            (if (not (mr-test n a)) (setq is-prime nil)))
          (++ i))
        is-prime))))

; Funzione che verifica se un numero è primo (no big-integer)
(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

Proviamo:

Test di correttezza (fino a 1e6):

(= (filter prime? (sequence 2 1e6)) (filter prime-i? (sequence 2 1e6)))
;-> true

Test di velocità:

Numeri fino a 6 cifre:
(time (filter prime? (sequence 2 1e6)))
;-> 929.907
(time (filter prime-i? (sequence 2 1e6)))
;-> 16770.194

Numeri con 15 cifre:
  100000000000031 --> primo
  100000000000067 --> primo
  100000000000097 --> primo
  100000000000073 --> 7478413 * 13371821
  100000000000039 --> 821 * 3163 * 38508593
  100000000000045 --> 5 * 41 * 487 * 10243 * 97789
  100000000000055 --> 5 * 827087 * 24181253
  100000000000065 --> 3 * 5 * 11 * 31 * 15809 * 1236659
  100000000000075 --> 5 * 5 * 7 * 571428571429

(setq big1 '(100000000000031 100000000000067 100000000000097
             100000000000073 100000000000039 100000000000045
             100000000000055 100000000000065 100000000000075))

(map prime? big1)
;-> (true true true nil nil nil nil nil nil)
(map prime-i? big1)
;-> (true true true nil nil nil nil nil nil)
(map prime-i? big1)

(time (filter prime? big1) 10)
;-> 1169.968
(time (filter prime-i? big1) 10)
;-> 15.112

Con numeri di 15 cifre Miller-Rabin è molto più veloce.

Il più grande numero primo minore di 9223372036854775807 = 2^63−1 è:

  9223372036854775783

È uno dei più grandi primi rappresentabili in signed 64-bit ed è comunemente usato come 'largest 64-bit prime'.

(setq max-int 9223372036854775807)
(setq primo64 9223372036854775783)

(time (println (prime-i? primo64)))
;-> true
;-> 0
(time (println (prime? primo64))) --> tanto tanto tempo...

Numeri con 25 cifre:
  1000000000000000000000007 --> primo
  1000000000000000000000049 --> primo
  1000000000000000000000091 --> 34429 * 29045281594005053879
  1000000000000000000000121 --> primo
  1000000000000000000000159 --> 328043 * 3048380852510189213
  1000000000000000000000177 --> primo
  1000000000000000000000183 --> primo
  1000000000000000000000189 --> 89 * 11235955056179775280901
  1000000000000000000000663 --> primo

(setq big2 '(1000000000000000000000007 1000000000000000000000049
             1000000000000000000000091 1000000000000000000000121
             1000000000000000000000159 1000000000000000000000177
             1000000000000000000000183 1000000000000000000000189
             1000000000000000000000663))

(map prime-i? big2)
;-> (true true nil true nil true true nil true)

Considerazioni sulle basi
-------------------------
L'insieme di basi usate:

  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37

è molto forte in pratica, ma NON è una garanzia deterministica per tutti i numeri a 64 bit.
Per numeri fino a 64 bit esistono insiemi di basi che rendono Miller–Rabin deterministico al 100%.
Per esempio (risultato noto in teoria dei numeri computazionale):
Set deterministico per 64 bit (classico):

  2, 325, 9375, 28178, 450775, 9780504, 1795265022

Con queste basi:
- ogni intero < 2^64 viene classificato correttamente
- nessun composto passa tutti i test

Le basi piccole (tipo 2–37):
- eliminano quasi tutti i composti
- funzionano perfettamente in pratica su numeri casuali
- ma esistono 'strong pseudoprimes' a quelle basi
Quindi non sono sufficienti per la determinazione teorica su tutto l'intervallo 64-bit.

Praticamente:
  + ----------------------- + -------------------------------------- +
  | Set di basi             | Sicurezza                              |
  + ----------------------- + -------------------------------------- |
  | 2–37                    | molto alta (uso pratico, 100 cifre ok) |
  | basi 64-bit certificate | deterministico assoluto                |
  | basi casuali (random)   | probabilistico                         |
  + ----------------------- + -------------------------------------- +

Cambiamo la funzione 'prime-i?' per utilizzare un set di basi come parametro.
In questo modo possiamo stabilire la 'sicurezza' del risultato.
La funzione diventa '(prime-i? n bases)'.

; ============================================================
; Full primality test using multiple bases
; Examples:
; (2L 3L 5L 7L 11L 13L 17L 19L 23L 29L 31L 37L) (probabilistic to 100+ digits)
; (2 325 9375 28178 450775 9780504 1795265022) (64-bit deterministic)
; For 64-bit integers, this is deterministic in practice
; ============================================================
(define (prime-i? n bases)
  (letn (
          ; index and current base
          (i 0)
          (a 0)
          ; global primality flag
          (is-prime true)
        )
    ; small cases
    (cond
      ((< n 2) nil)
      ((or (= n 2) (= n 3)) true)
      ((zero? (% n 2)) nil)
      (true
        ; test all bases until failure
        (while (and is-prime (< i (length bases)))
          (setq a (bases i))
          ; skip invalid base
          (if (< a n)
            (if (not (mr-test n a)) (setq is-prime nil)))
          (++ i))
        is-prime))))

Proviamo:

; (probabilistic to 100+ digits)
(setq b1 '(2L 3L 5L 7L 11L 13L 17L 19L 23L 29L 31L 37L))
; (64-bit deterministic)
(setq b2 '(2L 325L 9375L 28178L 450775L 9780504L 1795265022L))

(map (fn(x) (prime-i? x b1)) big1)
;-> (true true true nil nil nil nil nil nil)
(map (fn(x) (prime-i? x b2)) big1)
;-> (true true true nil nil nil nil nil nil)

(map (fn(x) (prime-i? x b1)) big2)
;-> (true true nil true nil true true nil true)
(map (fn(x) (prime-i? x b2)) big2)
;-> (true true nil true nil true true nil true)

Possiamo scegliere:
- poche basi -> velocissimo (probabilistico)
- molte basi -> quasi certezza
- basi speciali -> determinismo su intervalli noti
Possiamo anche combinare strategie:
- prime basi piccole per filtraggio rapido
- poi basi 'forti' per conferma

Il parametro 'bases' diventa concettualmente un insieme di test indipendenti.
Ogni base è un "controllo" della struttura modulare di n.
Più basi aggiungiamo più riduciamo la probabilità di errore, ma aumentiamo anche il costo computazionale.


--------------------------------------------
Scomposizione di interi con prodotto massimo
--------------------------------------------

Dato un numero intero n, scomporlo nella somma di k numeri interi positivi, dove k >= 2, e massimizzare il prodotto di tali numeri.

Metodo 1 (Brute-force)
----------------------

Generiamo tutte le partizioni del numero e poi prendiamo quella con prodotto massimo.

(define (partition-num num)
"Generate a list of all the partitions of a positive integer"
(catch
  (local (part k temp-value out)
    (setq out '())
    (setq part (array num '(0)))
    (setq k 0)
    (setf (part k) num)
    ; Questo ciclo prima aggiunge la partizione corrente alla lista
    ; poi genera la partizione successiva.
    ; Il ciclo termina quando la partizione corrente è costituita da tutti 1.
    (while true
      ; Aggiunge la partizione corrente alla lista delle soluzioni
      ;(push (slice part 0 (+ k 1)) out -1)             ; array
      (push (array-list (slice part 0 (+ k 1))) out -1) ; list
      ; Genera la partizione successiva
      ; Trova il valore non-uno più a destra di part[]
      ; Aggiorna anche il valore di temp-value
      ; (cioè quanti valori possono essere inseriti)
      (setq temp-value 0)
      (while (and (>= k 0) (= (part k) 1))
        (setq temp-value (+ temp-value (part k)))
        (-- k)
      )
      ; se k < 0, tutti i valori valgono 1
      ; quindi non ci sono altre partizioni da generare
      (if (< k 0) (throw out))
      ; Decrementa part[k] trovato sopra e calcola il valore di temp-value
      (setf (part k) (- (part k) 1))
      (++ temp-value)
      ; Se rem_val è maggiore, allora l'ordine è violato.
      ; Divide temp-value in diversi valori di dimensione part[k] e
      ; copia questi valori in posizioni diverse dopo part[k]
      (while (> temp-value (part k))
        (setf (part (+ k 1)) (part k))
        (setq temp-value (- temp-value (part k)))
        (++ k)
      )
      ; Copia rem_val nella posizione successiva e incrementa la posizione
      (setf (part (+ k 1)) temp-value)
      (++ k)))))

(length (partition-num 10))
;-> 42
(length (partition-num 50))
;-> 204226

(setq par (partition-num 10))
(list? (par 0))
(setq par (array-list par))

(define (max-prod1 n)
  (let ((max-val 0) (max-lst '())
        (part (partition-num n)))
    (dolist (el part)
      (when (> (apply * el) max-val)
        (setq max-val (apply * el))
        (setq max-lst el)))
    (setq max-lst (flat (replace 4 max-lst '(2 2))))
    (list (apply + max-lst) max-lst (apply * max-lst))))

Proviamo:

(max-prod1 4)
;-> (4 (2 2) 4)
(max-prod1 10)
;-> (10 (2 2 3 3) 36)
(time (println (max-prod1 50)))
;-> (50 (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2) 86093442)
;-> 695.915
(time (println (max-prod1 65)))
;-> (65 (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2) 20920706406)
;-> 7729.944

Metodo 2 (dynamic programming)
------------------------------
Creiamo un vettore V di dimensione (n + 1) dove V[i] memorizza il prodotto massimo ottenibile suddividendo l'intero i.
Inizializziamo tutti i valori a 1 poichè il prodotto minimo per qualsiasi intero positivo è 1.

Algoritmo:
  Caso base: V[1] = 1 (già impostato durante l'inizializzazione)
  Iterazione: Per ogni intero i da 2 a n:
  Prova tutti i modi possibili per suddividere i in due parti
  Per ogni possibile posizione di suddivisione j da 1 a i-1:
    Calcola il prodotto quando dividiamo i in j e i-j
    Abbiamo due opzioni per il resto i-j:
      Non suddividere ulteriormente: prodotto = (i - j) * j
      Suddividere in modo ottimale: prodotto = V[i - j] * j
    Aggiorna V[i] con il massimo tra il valore corrente e entrambe le opzioni

Equazione di transizione di stato:

  V[i] = max(V[i], V[i - j] * j, (i - j) * j), dove j in [1, i)

(define (max-prod2 n)
    ; (dp i) rappresenta il massimo prodotto che
    ; possiamo avere spezzando l'intero 'i'
    (let (dp (array (+ n 1) '(0)))
      ; Caso base: per n=1, il massimo prodotto vale 1
      (setf (dp 1) 1L)
      ; Riempie il vettore 'dp' per ogni numero da 2 a n
      (for (i 2 n)
      ; Prova tutti i modi possibili di spezzare l'intero 'i' in
      ; due parti: i e (i-j)
        (for (j 1 (- i 1))
          ; Per ogni suddivisione, abbiamo due possibilità:
          ; 1. Non suddividere ulteriormente (i-j): prodotto = (* j (- i j))
          ; 2. Suddividere ulteriormente (i-j): prodotto = (* j (dp (- i j)))
          ; Prendere il valore massimo tra il valore corrente e
          ; la somma dei due risultati.
          (setf (dp i) (max (dp i) (max (* j (- i j)) (* j (dp (- i j))))))))
      ; il prodotto massimo di n si trova in (dp n)
      (dp n)))

Proviamo:

(max-prod2 4)
;-> 4
(max-prod2 10)
;-> 36
(time (println (max-prod2 50)))
;-> 86093442
;-> 0
(time (println (max-prod2 65)))
;-> (65 (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2) 20920706406)
;-> 0

Metodo 3 (matematica)
---------------------

Per massimizzare il prodotto:
- 3 è il 'miglior mattone' (3*3 > 2*2*2) perchè massimizza il prodotto locale
- 4 è l’unico caso in cui 3+1 è peggio di 2+2
- evitare 1 nei fattori finali

Regola
1. usare il più possibile 3
2. quando il resto è:
   - 2 -> si tiene
   - 3 -> si tiene
   - 4 -> si sostituisce con 2 + 2
3. se resta 1, si trasforma (3 + 1) in (2 + 2)
   se resta 2, si lascia 2
4. nessun 1 deve comparire

Algoritmo
1) Affinchè (n > 4): prendere 3)
2) alla fine:
  - se n == 4  -> (2 2)
  - altrimenti -> (n)

(define (max-prod3 n)
  (setq n (bigint n))
  (cond
    ((< n 2) '())
    ((= n 2) '(2L (2L) 2L))
    ((= n 3) '(3L (3L) 3L))
    (true
      (let (res '())
        (while (> n 4)
          (push 3L res -1)
          (setf n (- n 3)))
        (if (= n 4)
            (extend res '(2L 2L))
            (push n res -1))
        (list (apply + res) res (apply * res))))))

Proviamo:

(max-prod3 4)
;-> (4L (2L 2L) 4L)
(max-prod3 10)
;-> (10L (3L 3L 2L 2L) 36L)
(max-prod3 50)
;-> (50L (3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 2L) 86093442L)
(max-prod3 65)
;-> (65L (3L 3L 3L 3L 3L 3L 3L ... 2L) 20920706406L)
(time (println (max-prod3 100)))
;-> (100L (3L 3L 3L 3L 3L 3L 3L 3L ... 2L 2L) 7412080755407364L)
;-> 0

Versione ottimizzata

(define (max-prod4 n)
  (cond
    ((< n 2) '())
    ((= n 2) '(2L (2L) 2L))
    ((= n 3) '(3L (3L) 3L))
    (true
      (let ((out '()) (num3 (/ n 3)) (resto (% n 3)))
        (cond ((= resto 0)
                (setq out (dup 3L num3)))
              ((= resto 1)
                (setq out (append (dup 3L (- num3 1)) '(2L 2L))))
              ((= resto 2)
                (setq out (append (dup 3L num3) '(2L)))))
        (list (apply + out) out (apply * out))))))

Proviamo:

(max-prod4 4)
;-> (4L (2L 2L) 4L)
(max-prod4 10)
;-> (10L (3L 3L 2L 2L) 36L)
(max-prod4 50)
;-> (50L (3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 3L 2L) 86093442L)
(max-prod4 65)
;-> (65L (3L 3L 3L 3L 3L 3L 3L ... 2L) 20920706406L)
(time (println (max-prod3 100)))
;-> (100L (3L 3L 3L 3L 3L 3L 3L 3L ... 2L 2L) 7412080755407364L)
;-> 0

Test di velocità:

(time (max-prod2 1000) 100)
;-> 11144.695
(time (max-prod3 1000) 100)
;-> 29.94
(time (max-prod4 1000) 100)
;-> 19.594


-------------------------------------------------
Indicizzazione mista di liste, vettori e stringhe
-------------------------------------------------

Le strutture dati come liste, vettori e stringhe possono essere utilizzate in combinazione tra loro.
Per esempio possiamo avere una lista di vettori o un vettore di liste, ecc.
In questi casi bisogna fare attenzione al metodo per indicizzare gli elementi.

Regola: quando si usano insieme strutture di tipo diverso bisogna utilizzare una 'doppia' indicizzazione.

Vediamo alcuni esempi.

Lista di liste
--------------
(setq L1 '((1 2 3) (4 5 6) (7 8 9)))
(L1 0 2)  ; indicizzazione 'normale' (due indici lineari)
;-> 3

Lista di vettori
----------------
(setq v1 (array 3 '(1 2 3)))
(setq v2 (array 3 '(4 5 6)))
(setq v3 (array 3 '(7 8 9)))
Errato:
(setq L2 (list v1 v2 v3)) ; v1,v2 e v3 vengono trasformati in liste
(array? (L2 0))
;-> nil
Corretto:
(setq L2 '())
(push v1 L2 -1)
(push v2 L2 -1)
(push v3 L2 -1)
(array? (L2 0))
;-> true
(L2 0 2)   ; l'indicizzazione 'normale' restituisce il vettore con indice 0
;-> (1 2 3)
((L2 0) 2) ; quindi occorre una doppia indicizzazione
;-> 3

Vettore di liste
----------------
(setq l1 (list 1 2 3))
(setq l2 (list 4 5 6))
(setq l3 (list 7 8 9))
(setq V1 (array 3 (list l1 l2 l3)))
(list? (V1 0))
;-> true
(V1 0 2)   ; l'indicizzazione 'normale' restituisce la lista con indice 0
;-> (1 2 3)
((V1 0) 2) ; quindi occorre una doppia indicizzazione
;-> 3

Vettore di vettori
------------------
(setq V2 (array 3 3 '(1 2 3 4 5 6 7 8 9)))
(array? (V2 0))
;-> true
(V2 0 2)  ; indicizzazione 'normale' (due indici lineari)
;-> 3

Lista di stringhe
-----------------
(setq s1 "abc" s2 "xyz" s3 "ijk")
(setq L3 (list s1 s2 s3))
(string? (L3 0))
;-> true
(L3 1 0)  ; l'indicizzazione 'normale' restituisce la stringa con indice 0
;-> "xyz"
((L3 1) 0); quindi occorre una doppia indicizzazione
;-> "x"

Vettore di stringhe
-------------------
(setq V3 (array 3 (list s1 s2 s3)))
(string? (V3 0))
;-> true
(V3 1 0)  ; l'indicizzazione 'normale' restituisce la stringa con indice 0
;-> "xyz"
((V3 1) 0); quindi occorre una doppia indicizzazione
;-> "x"


-----------------
Treni e biglietti
-----------------

Un treno che compie un percorso effettuando K fermate.
Abbiamo i seguenti dati per ogni biglietto che è stato venduto:

  (stazione-partenza stazione-arrivo)

Non possono essere venduti ulteriori biglietti.
Sapendo che il treno può contenere al massimo N persone, determinare se tutti i possessori di biglietto possono effettuare il proprio viaggio.

Per risolvere il problema simuliamo il percorso del treno.

Algoritmo
Alla partenza il numero di passeggeri vale P.
Ad ogni stazione:
  1) salgono alcuni passeggeri  (+ x)
  2) scendono alcuni passeggeri (- y)
  3) P = P + x - y
  4) controllo sul numero massimo dei passeggeri
     (cioè se qualche passeggero rimane a terra)
     Eventuale aggiornamento del numero di passeggeri
  5) stampa della situazione

Per applicare l'algoritmo dobbiamo trasformare i dati iniziali in una lista con elementi del tipo:

  (numero-stazione persone-salite persone-scese)

Inoltre il treno parte dalla stazione 0 ed effettua N fermate.

Prima scriviamo una funzione che genera i biglietti dei passeggeri:

(define (crea-biglietti num-biglietti num-fermate)
  (let ( (out '()) (start 0) (end 0) )
    (for (i 1 num-biglietti)
      ; stazione di partenza
      (setq start (rand (+ num-fermate 1)))
      ; stazione di arrivo
      (setq end (rand (+ num-fermate 1)))
      ; controllo sui valori di arrivo e partenza
      ; deve essere: end > start e start != end e end > 0
      (cond ((> start end) (swap start end))
            ((= start end)
              (if (zero? start)
                  (++ end)
                  (-- start))))
      (push (list start end) out -1))
    out))

(setq pass (crea-biglietti 10 5))
;-> ((0 1) (0 1) (4 5) (0 1) (2 5) (1 5) (2 4) (2 3) (3 5) (3 4))

(define (trasforma-dati data num-fermate)
  (let ((stazioni '())
        (in (array (+ num-fermate 1) '(0)))
        (out (array (+ num-fermate 1) '(0))))
    (dolist (el data)
      (++ (in (el 0)))
      (++ (out (el 1))))
    (map list in out)))

(trasforma-dati pass 5)
;-> ((3 0) (1 3) (3 0) (2 1) (1 2) (0 4))

Cosa significano i dati:
L'indice dell'elemento si riferisce al numero della stazione
(3 0) --> alla partenza 3 passeggeri (non scende mai nessuno)
(1 3) --> alla stazione 1 salgono in 1 e scendono in 3
(3 0) --> alla stazione 2 salgono in 3 e scendono in 0
...

Scriviamo la funzione che simula il viaggio del treno

La simulazione si interrompe alla stazione in cui rimane a terra qualche passeggero.
Infatti quando qualcuno NON sale poi NON scende e nelle fermate successive i dati sono falsati.

(define (viaggio capienza num-fermate num-biglietti)
(catch
  (local (ok ground pas percorso in out totale)
    (setq ok true)
    ; numero di passeggeri rimasti a terra
    (setq ground 0)
    ; creazione dei num-biglietti random
    (setq pass (crea-biglietti num-biglietti num-fermate))
    (println pass)
    ; trasformazione dei dati
    (setq percorso (trasforma-dati pass num-fermate))
    (println percorso)
    (setq totale 0)
    ; ciclo per ogni fermata
    (dolist (f percorso)
      (setq in (f 0))  ; saliti
      (setq out (f 1)) ; scesi
      (setq totale (+ totale in (- out))) ; totale passeggeri
      ; controllo superamento capienza del treno
      ; e calcolo dei passeggeri rimasti a terra
      (when (> totale capienza)
        (setq ground (- totale capienza))
        (setq in (- in ground))
        (setq totale capienza)
        (setq ok nil))
      ; stampa della situazione della stazione corrente
      (println "Stazione: " $idx)
      (println "scese: " out)
      (println "salite: " in)
      (println "passeggeri: " totale)
      (when (> ground 0)
        (println "Errore --> " ground " persone a terra")
        (setq ground 0)
        (throw ok))
      (println))
    ok)))

Proviamo:

(viaggio 10 5 20)
;-> ((1 3) (0 3) (0 4) (2 5) (3 5) (0 1) (1 2) (2 4) (2 5) (2 4) (0 4) (0 4)
;->  (3 5) (1 2) (0 2) (1 2) (1 5) (0 1) (1 2) (0 1))
;->  ((8 0) (6 3) (4 5) (2 2) (0 5) (0 5))
;-> Stazione: 0
;-> scese: 0
;-> salite: 8
;-> passeggeri: 8
;->
;-> Stazione: 1
;-> scese: 3
;-> salite: 5
;-> passeggeri: 10
;-> Errore --> 1 persone a terra
;-> nil

(viaggio 10 5 20)
;-> ((0 1) (4 5) (1 5) (0 4) (0 1) (0 3) (2 3) (0 1) (0 1) (0 2) (1 4) (1 3)
;->  (4 5) (0 1) (2 4) (1 2) (3 4) (0 3) (3 4) (0 3))
;-> ((10 0) (4 5) (2 2) (2 5) (2 5) (0 3))
;-> Stazione: 0
;-> scese: 0
;-> salite: 10
;-> passeggeri: 10
;->
;-> Stazione: 1
;-> scese: 5
;-> salite: 4
;-> passeggeri: 9
;->
;-> Stazione: 2
;-> scese: 2
;-> salite: 2
;-> passeggeri: 9
;->
;-> Stazione: 3
;-> scese: 5
;-> salite: 2
;-> passeggeri: 6
;->
;-> Stazione: 4
;-> scese: 5
;-> salite: 2
;-> passeggeri: 3
;->
;-> Stazione: 5
;-> scese: 3
;-> salite: 0
;-> passeggeri: 0
;-> true


--------------------------------
yass - yet another sudoku solver
--------------------------------

Progettiamo un solver per sudoku con un metodo brute-force.

Un solver Sudoku brute-force effettua una ricerca sistematica nello spazio di tutte le possibili configurazioni della griglia.

Metodo di soluzione
-------------------
Dato un Sudoku 9×9 con o senza celle già riempite:
1) Si cerca una cella vuota.
2) Si prova a inserire un numero da 1 a 9.
3) Dopo ogni inserimento si verifica che:
   - il numero non sia già presente nella riga;
   - il numero non sia già presente nella colonna;
   - il numero non sia già presente nel blocco 3×3.
4) Se il numero è valido, si passa alla cella vuota successiva.
5) Se in una cella non esiste alcun numero valido:
   - si torna indietro (backtracking);
   - si modifica l'ultima scelta effettuata.
6) Se tutte le celle sono riempite correttamente, il Sudoku è risolto.

Esempio:
Supponiamo di avere una cella vuota.
Prova 1: se valido -> continua
Prova 2: se valido -> continua
 ...
Ogni scelta genera un nuovo livello dell'albero.
Se una strada porta a una contraddizione, l'intero ramo viene scartato.

Il cuore del metodo è il backtracking.
Esempio:
- Cella A -> scelgo 4
- Cella B -> scelgo 7
- Cella C -> nessun numero possibile

Allora:
- annullo la scelta di B
- provo un altro valore per B
- se non esistono alternative:
  - annullo anche A
  - provo un altro valore per A

In pratica il solver esplora l'albero in profondità (Depth First Search).

Complessità teorica
-------------------
Se ci sono n celle vuote e per ciascuna si provano fino a 9 valori: O(9^n)
Nel caso peggiore 9^81 che è astronomico.
Fortunatamente i vincoli del Sudoku eliminano quasi tutti i rami molto presto.

Ottimizzazione fondamentale
---------------------------
Anche restando nel brute-force conviene non scegliere la prima cella vuota.
Si sceglie invece la cella con il minor numero di candidati.
Esempio:
  +-------+------------------+
  | Cella | Possibili valori |
  +-------+------------------+
  | A     | (1 2 3 4 5)      |
  | B     | (7)              |
  | C     | (2 8)            |
  +-------+------------------+
Conviene scegliere B.
Se B fallisce, il ramo viene eliminato immediatamente.
Questa semplice euristica riduce enormemente il numero di tentativi.

Rappresentazione dei candidati
------------------------------
Per ogni cella vuota si può mantenere:
- insieme dei valori ammessi;
- numero di candidati.

Ad ogni inserimento:
- si aggiornano i candidati della stessa riga;
- della stessa colonna;
- dello stesso blocco.

Se una cella rimane senza candidati:
- il ramo è impossibile;
- si effettua immediatamente il backtracking.

Algoritmo per un solver brute-force efficiente:
1) Calcola i candidati di tutte le celle vuote.
2) Se esiste una cella senza candidati -> fallimento.
3) Se non esistono celle vuote -> soluzione trovata.
4) Seleziona la cella con meno candidati.
5) Prova ciascun candidato:
   - inserisci il valore;
   - aggiorna i candidati;
   - richiama ricorsivamente il solver.
6) Se nessun candidato funziona -> backtracking.

Questo è ancora un metodo brute-force, ma con una forte potatura dell'albero di ricerca, sufficiente per risolvere praticamente tutti i Sudoku classici in tempi molto brevi.

Struttura dei dati
------------------
Usiamo una matrice 9×9 per i valori e matrice 9×9 parallela per i candidati.

Ad esempio:
(grid 0 0) --> 5
(grid 0 1) --> 3
(grid 0 2) --> 0
(cand 0 2) --> (1 2 4)

Quindi ogni posizione (r,c) ha:
  valore[r][c]
  candidati[r][c]

Quando assegniamo un numero:
- aggiorniamo il valore nella griglia;
- eliminiamo quel numero dai candidati della stessa riga;
- della stessa colonna;
- dello stesso blocco.

Inoltre possiamo scegliere sempre la cella con il minor numero di candidati (euristica 'minimum remaining values'), che riduce drasticamente il numero di tentativi rispetto al brute-force che sceglie semplicemente la prima cella vuota.

La prima funzione da scrivere è quella che calcola la matrice dei candidati:

(define (build-candidates grid)
  (local (cand used r0 c0)
    (setq cand (array-list (array 9 9 '(()))))
    (for (row 0 8)
      (for (col 0 8)
        (if (!= (grid row col) 0)
            (setf (cand row col) '())
            (begin
              (setq used '())
              ; riga
              (for (c 0 8)
                (if (!= (grid row c) 0)
                    (push (grid row c) used -1)))
              ; colonna
              (for (r 0 8)
                (if (!= (grid r col) 0)
                    (push (grid r col) used -1)))
              ; blocco 3x3
              (setq r0 (* (/ row 3) 3))
              (setq c0 (* (/ col 3) 3))
              (for (r r0 (+ r0 2))
                (for (c c0 (+ c0 2))
                  (if (!= (grid r c) 0)
                      (push (grid r c) used -1))))
              (setf (cand row col)
                    (difference '(1 2 3 4 5 6 7 8 9)
                                (unique used)))))))
    cand))

(setq grid '(
  (5 3 0 0 7 0 0 0 0)
  (6 0 0 1 9 5 0 0 0)
  (0 9 8 0 0 0 0 6 0)
  (8 0 0 0 6 0 0 0 3)
  (4 0 0 8 0 3 0 0 1)
  (7 0 0 0 2 0 0 0 6)
  (0 6 0 0 0 0 2 8 0)
  (0 0 0 4 1 9 0 0 5)
  (0 0 0 0 8 0 0 7 9)))

(setq cand (build-candidates grid))
(cand 0 2)
;-> (1 2 4)
(cand 4 4)
;-> (5)

A questo punto il solver può:
1) costruire cand;
2) cercare la cella vuota con meno candidati;
3) provare ciascun candidato;
4) aggiornare grid;
5) ricalcolare cand;
6) continuare ricorsivamente;
7) fare backtracking in caso di fallimento.

Non si cerchiamo di mantenere coerente la matrice dei candidati dopo ogni assegnazione.
La ricostruiamo da zero.
Supponiamo di avere:
grid  = valori del sudoku
cand  = candidati
All'inizio:
1) Leggiamo il Sudoku.
2) Calcoliamo tutti i candidati.
3) Ottieniamo la matrice cand.
Per esempio:
  grid(0,2) = 0
  cand(0,2) = (1 2 4)
  grid(4,5) = 0
  cand(4,5) = (3 9)
Ora scegliamo la cella con meno candidati.
Supponiamo:
  (4,5) --> (3 9)
Proviamo il valore 3.
La griglia diventa:
  grid(4,5) = 3
A questo punto non aggiorniamo nessun candidato.
Invece eseguiamo:
  cand = build-candidates(grid)
cioè ricalcoliamo completamente tutti i candidati della nuova configurazione.
Questo funziona perchè i candidati dipendono esclusivamente dallo stato corrente della griglia.
Se la griglia è corretta:
  grid --> candidati univocamente determinati
Non serve ricordare cosa è cambiato.

Il passo successivo è una funzione che, data la matrice 'cand', trova la cella vuota con il 'minor numero di candidati'.
La funzione deve restituire qualcosa del tipo: (row col lista-candidati)
Ad esempio:
(4 4 (5))
oppure:
(2 3 (1 7))

Algoritmo
---------
1) Scorrere tutte le 81 celle.
2) Ignorare le celle con lista vuota:
   - possono essere celle già assegnate;
   - oppure celle impossibili (0 candidati).
3) Mantenere la lunghezza minima trovata.
4) Salvare coordinate e candidati della cella corrispondente.
5) Restituire la migliore cella trovata.

Inoltre conviene rilevare subito una contraddizione:
  cella vuota + 0 candidati
In questo caso il ramo corrente è impossibile e il solver deve fare immediatamente backtracking.
Quindi, durante la scansione, per ogni posizione (r,c):
- se grid(r,c) != 0 -> ignorare;
- se grid(r,c) = 0 e cand(r,c) è vuota -> fallimento;
- altrimenti confrontare il numero di candidati con il minimo corrente.

La funzione potrebbe quindi restituire nil se trova una contraddizione, oppure (row col candidati) se esiste almeno una cella vuota, oppure un valore speciale (ad esempio 'solved') se non esistono più celle vuote e il Sudoku è completato.

(define (best-cell grid cand)
  (local (best-r best-c best-list min-len found)
    (setq best-r nil)
    (setq best-c nil)
    (setq best-list nil)
    (setq min-len 10)
    (setq found nil)
    (for (r 0 8)
      (for (c 0 8)
        (if (= (grid r c) 0)
            (let (cur (cand r c))
              ; cella impossibile
              (if (= (length cur) 0)
                  (setq found 'fail)
                  ; nuovo minimo
                  (if (< (length cur) min-len)
                      (begin
                        (setq min-len (length cur))
                        (setq best-r r)
                        (setq best-c c)
                        (setq best-list cur)
                        (setq found true))))))))
    (cond
      ((= found 'fail) nil)
      ((= found nil) 'solved)
      (true (list best-r best-c best-list)))))

(best-cell grid cand)
-> (4 4 (5))

A questo punto abbiamo:

a) grid = matrice dei valori.
b) build-candidates = costruisce 'cand'.
c) best-cell = sceglie la prossima cella da esplorare.

Dobbiamo scrivere il solver vero e proprio.

Algoritmo
---------
solve(grid)
1. cand = build-candidates(grid)
2. scelta = best-cell(grid, cand)
3. se scelta = solved
      soluzione trovata
4. se scelta = nil
      fallimento
5. estrarre:
      row
      col
      candidati
6. per ogni candidato:
      copiare la griglia
      assegnare il candidato
      richiamare solve(copia)
      se trova una soluzione
          restituiscila
7. nessun candidato funziona
      fallimento

In pratica il backtracking agisce solo sulla griglia.
La matrice dei candidati non viene mai salvata né ripristinata:

  grid -> build-candidates -> cand

viene sempre ricostruita da zero.

Per evitare problemi durante il backtracking conviene lavorare su una copia della griglia:
  grid
   ├─ prova 1
   │    └─ solve(...)
   ├─ prova 2
   │    └─ solve(...)
   └─ prova 3
        └─ solve(...)
Ogni ramo modifica la propria copia.
Così non occorre annullare le mosse quando si torna indietro.

Schema finale
-------------
solve(grid)
cand = build-candidates(grid)
scelta = best-cell(grid cand)
se solved -> return grid
se nil -> return nil
per ogni candidato
    new-grid = copia(grid)
    assegna candidato
    soluzione = solve(new-grid)
    se soluzione
       return soluzione
return nil

(define (solve grid)
  (local (cand choice row col vals new-grid sol)
    (setq cand (build-candidates grid))
    (setq choice (best-cell grid cand))
    (cond
      ; soluzione trovata
      ((= choice 'solved)
       grid)
      ; ramo impossibile
      ((nil? choice)
       nil)
      ; continua la ricerca
      (true
        (setq row (choice 0))
        (setq col (choice 1))
        (setq vals (choice 2))
        (setq sol nil)
        (dolist (v vals)
          (if (nil? sol)
              (begin
                ;(setq new-grid (copy-grid grid))
                (setq new-grid grid)
                (setf (new-grid row col) v)
                (setq sol (solve new-grid)))))
        sol))))

(define (sudoku grid) (solve grid))

(setq sol (sudoku grid))
;-> ((5 3 4 6 7 8 9 1 2)
;->  (6 7 2 1 9 5 3 4 8)
;->  (1 9 8 3 4 2 5 6 7)
;->  (8 5 9 7 6 1 4 2 3)
;->  (4 2 6 8 5 3 7 9 1)
;->  (7 1 3 9 2 4 8 5 6)
;->  (9 6 1 5 3 7 2 8 4)
;->  (2 8 7 4 1 9 6 3 5)
;->  (3 4 5 2 8 6 1 7 9))

Per finire scriviamo una funzione di verifica di una soluzione.
(verifica righe, colonne e blocchi)

(define (valid-group lst)
  (= (sort lst) '(1 2 3 4 5 6 7 8 9)))

(define (sudoku? grid)
  (local (ok group)
    (setq ok true)
    ; righe
    (dolist (el grid (not ok))
      (setq ok (valid-group el)))
    ; colonne
    (dolist (el (transpose grid) (not ok))
      (setq ok (valid-group el)))
    ; blocchi 3x3
    (for (r0 0 6 3 (not ok))
      (for (c0 0 6 3 (not ok))
        (setq group '())
        (for (r r0 (+ r0 2))
          (for (c c0 (+ c0 2))
            (push (grid r c) group -1)))
        (setq ok (valid-group group))))
    ok))

(sudoku? sol)
;-> true

Test:

(setq x
'((0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)))

(time (println (sudoku x)))
;-> ((1 2 3 4 5 6 7 8 9)
;->  (4 5 6 7 8 9 1 2 3)
;->  (7 8 9 1 2 3 4 5 6)
;->  (2 1 4 3 6 5 8 9 7)
;->  (3 6 5 8 9 7 2 1 4)
;->  (8 9 7 2 1 4 3 6 5)
;->  (5 3 1 6 4 2 9 7 8)
;->  (6 4 2 9 7 8 5 3 1)
;->  (9 7 8 5 3 1 6 4 2))
;-> 31.235

I dieci sudoku più difficili:

(setq escargot
'((1 0 0 0 0 7 0 9 0)
  (0 3 0 0 2 0 0 0 8)
  (0 0 9 6 0 0 5 0 0)
  (0 0 5 3 0 0 9 0 0)
  (0 1 0 0 8 0 0 0 2)
  (6 0 0 0 0 4 0 0 0)
  (3 0 0 0 0 0 0 1 0)
  (0 4 0 0 0 0 0 0 7)
  (0 0 7 0 0 0 3 0 0)))
(time (println (sudoku escargot)))
;-> 46.788

(setq killer
'((0 0 0 0 0 0 0 7 0)
  (0 6 0 0 1 0 0 0 4)
  (0 0 3 4 0 0 2 0 0)
  (8 0 0 0 0 3 0 5 0)
  (0 0 2 9 0 0 7 0 0)
  (0 4 0 0 8 0 0 0 9)
  (0 2 0 0 6 0 0 0 7)
  (0 0 0 1 0 0 9 0 0)
  (7 0 0 0 0 8 0 6 0)))
(time (println (sudoku killer)))
;-> 1329.315

(setq diamond
'((1 0 0 5 0 0 4 0 0)
  (0 0 9 0 3 0 0 0 0)
  (0 7 0 0 0 8 0 0 5)
  (0 0 1 0 0 0 0 3 0)
  (8 0 0 6 0 0 5 0 0)
  (0 9 0 0 0 7 0 0 8)
  (0 0 4 0 2 0 0 1 0)
  (2 0 0 8 0 0 6 0 0)
  (0 0 0 0 0 1 0 0 2)))
(time (println (sudoku diamond)))
;-> 1004.084

(setq wormhole
'((0 8 0 0 0 0 0 0 1)
  (0 0 7 0 0 4 0 2 0)
  (6 0 0 3 0 0 7 0 0)
  (0 0 2 0 0 9 0 0 0)
  (1 0 0 0 6 0 0 0 8)
  (0 3 0 4 0 0 0 0 0)
  (0 0 1 7 0 0 6 0 0)
  (0 9 0 0 0 8 0 0 5)
  (0 0 0 0 0 0 0 4 0)))
(time (println (sudoku wormhole)))
;-> 160.744

(setq labyrinth
'((1 0 0 4 0 0 8 0 0)
  (0 4 0 0 3 0 0 0 9)
  (0 0 9 0 0 6 0 5 0)
  (0 5 0 3 0 0 0 0 0)
  (0 0 0 0 0 1 6 0 0)
  (0 0 0 0 7 0 0 0 2)
  (0 0 4 0 1 0 9 0 0)
  (7 0 0 8 0 0 0 0 4)
  (0 2 0 0 0 4 0 8 0)))
(time (println (sudoku labyrinth)))
;-> 266.684

(setq circles
'((0 0 5 0 0 9 7 0 0)
  (0 6 0 0 0 0 0 2 0)
  (1 0 0 8 0 0 0 0 6)
  (0 1 0 7 0 0 0 0 4)
  (0 0 7 0 6 0 0 3 0)
  (6 0 0 0 0 3 2 0 0)
  (0 0 0 0 0 6 0 4 0)
  (0 9 0 0 5 0 1 0 0)
  (8 0 0 1 0 0 0 0 2)))
(time (println (sudoku circles)))
;-> 299.263

(setq squadron
'((6 0 0 0 0 0 2 0 0)
  (0 9 0 0 0 1 0 0 5)
  (0 0 8 0 3 0 0 4 0)
  (0 0 0 0 0 2 0 0 1)
  (5 0 0 6 0 0 9 0 0)
  (0 0 7 0 9 0 0 0 0)
  (0 7 0 0 0 3 0 0 2)
  (0 0 0 4 0 0 5 0 0)
  (0 0 6 0 7 0 0 8 0)))
(time (println (sudoku squadron)))
;-> 252.293

(setq honeypot
'((1 0 0 0 0 0 0 6 0)
  (0 0 0 1 0 0 0 0 3)
  (0 0 5 0 0 2 9 0 0)
  (0 0 9 0 0 1 0 0 0)
  (7 0 0 0 4 0 0 8 0)
  (0 3 0 5 0 0 0 0 2)
  (5 0 0 4 0 0 0 0 6)
  (0 0 8 0 6 0 0 7 0)
  (0 7 0 0 0 5 0 0 0)))
(time (println (sudoku honeypot)))
;-> 1266.399

(setq tweezers
'((1 0 0 0 0 0 0 6 0)
  (0 0 0 1 0 0 0 0 3)
  (0 0 5 0 0 2 9 0 0)
  (0 0 9 0 0 1 0 0 0)
  (7 0 0 0 4 0 0 8 0)
  (0 3 0 5 0 0 0 0 2)
  (5 0 0 4 0 0 0 0 6)
  (0 0 8 0 6 0 0 7 0)
  (0 7 0 0 0 5 0 0 0)))
(time (println (sudoku tweezers)))
;-> 1292.902

(setq brokenbrick
'((4 0 0 0 6 0 0 7 0)
  (0 0 0 0 0 0 6 0 0)
  (0 3 0 0 0 2 0 0 1)
  (7 0 0 0 0 8 5 0 0)
  (0 1 0 4 0 0 0 0 0)
  (0 2 0 9 5 0 0 0 0)
  (0 0 0 0 0 0 7 0 5)
  (0 0 9 1 0 0 0 3 0)
  (0 0 3 0 4 0 0 8 0)))
(time (println (sudoku brokenbrick)))
;-> 687.381

The World's Hardest Sudoku:

(setq world
'((8 0 0 0 0 0 0 0 0)
  (0 0 3 6 0 0 0 0 0)
  (0 7 0 0 9 0 2 0 0)
  (0 5 0 0 0 7 0 0 0)
  (0 0 0 0 4 5 7 0 0)
  (0 0 0 1 0 0 0 3 0)
  (0 0 1 0 0 0 0 6 8)
  (0 0 8 5 0 0 0 1 0)
  (0 9 0 0 0 0 4 0 0)))
(time (println (sudoku world)))
;-> 2687.746

In genere i puzzle più difficili per gli umani vengono risolti in breve tempo (pochi secondi) con la nostra funzione brute-force. Adesso vediamo un puzzle considerato difficile per il metodo brute-force:

(setq beast
'((0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 3 0 8 5)
  (0 0 1 0 2 0 0 0 0)
  (0 0 0 5 0 7 0 0 0)
  (0 0 4 0 0 0 1 0 0)
  (0 9 0 0 0 0 0 0 0)
  (5 0 0 0 0 0 0 7 3)
  (0 0 2 0 1 0 0 0 0)
  (0 0 0 0 4 0 0 0 9)))
(time (println (sudoku beast)))
;-> 11672.643

Un altro sudoku difficile con la tecnica brute-force:

(setq hell
'((9 0 0 8 0 0 0 0 0)
  (0 0 0 0 0 0 5 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 2 0 0 1 0 0 0 3)
  (0 1 0 0 0 0 0 6 0)
  (0 0 0 4 0 0 0 7 0)
  (7 0 8 6 0 0 0 0 0)
  (0 0 0 0 3 0 1 0 0)
  (4 0 0 0 0 0 2 0 0)))
(time (println (sudoku hell)))
;-> 1594.429

Un sudoku con solo 4 numeri di partenza:

(setq only4
'((0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (3 8 4 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 0)
  (0 0 0 0 0 0 0 0 2)))
(time (println (sudoku only4)))
;-> 84.744

Vedi anche "Sudoku" su "Rosetta Code".
Vedi anche "Sudoku test" su "Note libere 2".
Vedi anche "Sudoku mania" su "Note libere 3".


---------------
Funzioni locali
---------------

In alcuni casi lo sviluppo di una funzione richiede di eseguire una funzione ausiliaria.
Quando la funzione ausiliaria è utile solo per funzione principale, allora possiamo definirla come variabile locale.
In questo modo non può essere usata da un'altra funzione.

Esempio
  func -> unione(a b) - unione(b a)
  func-aux -> unione(x y)

  a = 32
  b = 258
  unione(a b) = 32258
  unione(b a) = 25832
  unione(a b) - unione(b a) = 32258 - 25832

Caso 1: func-aux -> funzione globale

(define (func1 a b)
  (define (func1-aux x y)
    (int (string x y) 0 10))
  (- (func1-aux a b) (func1-aux b a)))

(func1 32 258)
;-> 6426

In questo caso la funzione 'func1-aux' può essere chiamata senza problemi.
(func1-aux 36 23)
;-> 3623

Caso 2: func-aux -> funzione locale

(define (func2 a b)
  (local (func2-aux) ; definiamo 'func2-aux' come variabile locale
    (define (func2-aux x y)
      (int (string x y) 0 10))
    (- (func2-aux a b) (func2-aux b a))))

(func2 32 258)
;-> 6426

In questo caso la funzione 'func2-aux' non può essere chiamata esternamente perchè quando termina 'func2' la funzione 'func2-aux' viene eliminata:

(func2-aux 36 23)
;-> ERR: invalid function: (func2-aux 36 23)


---------------
Numeri exponial
---------------

I numeri exponial sono definiti nel modo seguente:

  exponial(n) = n^(n-1)^(n-2)^...^2

                                  2
                              ...^
                        (n-2)^
                  (n-1)^
  exponial(n) = n^

Regola associativa delle potenze: a^b^c = a^(b^c)

Sequenza OEIS A049384:
a(0) = 1, a(n+1) = (n+1)^a(n).
  1, 1, 2, 9, 262144

a(5) ha 183231 cifre.

(define (pow-i num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (pot out)
        (setq pot (pow-i num (/ power 2)))
        (if (odd? power)
            (setq out (* num pot pot))
            (setq out (* pot pot)))
        out)))

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (expo n)
  (let (a (array (+ n 1) '(0)))
    (setf (a 0) 1L)
    (for (i 0 (- n 1))
      (setf (a (+ i 1)) (pow-i (bigint (+ i 1)) (a i))))))
      ;(setf (a (+ i 1)) (** (+ i 1) (a i))))))

(map expo (sequence 1 4))
;-> (1L 2L 9L 262144L)

(**)
(time (println (length (expo 5))))
;-> 183231
;-> 5527.543

(pow-i)
(time (println (length (expo 5))))
;-> 183231
;-> 239.801


-----------------
La sequenza tower
-----------------

La sequenza tower è definita nel modo seguente:

  a(n) = n^((n-1)*(n-2)*...*3*2*1)

Sequenza OEIS A067039:
The tower function n^((n-1)!).
  1, 2, 9, 4096, 59604644775390625

(define (fact-i num)
"Calculate the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

tower(n) = n^((n-1)*(n-2)...*2) = n^(fact (n-1))

(define (tower n)
  (cond ((= n 0) 1L)
        (true (** n (fact-i (- n 1))))))

(map tower (sequence 0 5))
;-> (1L 1L 2L 9L 4096L 59604644775390625L)


---------------------------------------------------
Numeri elevati alla somma e al prodotto delle cifre
---------------------------------------------------

Sequenza OEIS A067040
a(n) = n^(sum of digits of n).
  1, 1, 4, 27, 256, 3125, 46656, 823543, 16777216, 387420489, 10, 121, 1728,
  28561, 537824, 11390625, 268435456, 6975757441, 198359290368,
  6131066257801, 400, 9261, 234256, 6436343, 191102976, 6103515625,
  208827064576, 7625597484987, ...

Sequenza OEIS A067041:
a(n) = n^(product of digits of n).
  1, 1, 4, 27, 256, 3125, 46656, 823543, 16777216, 387420489, 1, 11, 144,
  2197, 38416, 759375, 16777216, 410338673, 11019960576, 322687697779, 1,
  441, 234256, 148035889, 110075314176, 95367431640625, 95428956661682176, ...

(define (digit-sum num)
"Calculate the sum of the digits of an integer"
  (let (out 0)
    (while (!= num 0)
      (setq out (+ out (% num 10)))
      (setq num (/ num 10)))
    out))

(define (digit-prod num)
"Calculate the product of the digits of an integer"
  (if (zero? num)
      0
      (let (out 1)
        (while (!= num 0)
          (setq out (* out (% num 10)))
          (setq num (/ num 10)))
    out)))

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (pow-sum n)
  (** (bigint n) (bigint (digit-sum n))))

(map pow-sum (sequence 0 20))
;-> (1L 1L 4L 27L 256L 3125L 46656L 823543L 16777216L 387420489L 10L 121L 1728L
;->  28561L 537824L 11390625L 268435456L 6975757441L 198359290368L
;->  6131066257801L 400L)

(define (pow-prod n)
  (** (bigint n) (bigint (digit-prod n))))

(map pow-prod (sequence 0 20))
;-> (1L 1L 4L 27L 256L 3125L 46656L 823543L 16777216L 387420489L 1L 11L 144L
;->  2197L 38416L 759375L 16777216L 410338673L 11019960576L 322687697779L 1L)


-------------------------------------------------------------------
Prodotto uguali delle cifre pari e delle cifre dispari di un numero
-------------------------------------------------------------------

Determinare la sequenza dei numeri per cui risulta:

  prodotto delle cifre pari = prodotto delle cifre dispari

Regola: quando esiste una sola cifra pari (o dispari), allora il prodotto vale quella cifra.

Esempio:
  numero = 482
  indici = 123
  cifre dispari --> 4 e 2 --> 4*2 = 8
  cifre pari --> 8 --> 8

Sequenza OEIS A067042:
Numbers in which the product of digits in even positions = product of digits in odd positions.
  11, 22, 33, 44, 55, 66, 77, 88, 99, 100, 111, 122, 133, 144, 155, 166,
  177, 188, 199, 200, 221, 242, 263, 284, 300, 331, 362, 393, 400, 441,
  482, 500, 551, 600, 661, 700, 771, 800, 881, 900, 991, 1000, 1001, 1002,
  1003, 1004, 1005, 1006, 1007, 1008, 1009, ...

(define (seq? num)
  (let ((s (string num)) (l (length num)) (pari 1) (dispari 1))
    (for (i 0 (- l 1))
      ; quando l'indice è pari aggiorna 'dispari'
      ; perchè l'indice del numero inizia da 1 (che è dispari)
      (if (even? i)
        (setq dispari (* dispari (int (s i))))
        (setq pari (* pari (int (s i))))))
    (= pari dispari)))

(filter seq? (sequence 10 1009))
;-> (11 22 33 44 55 66 77 88 99 100 111 122 133 144 155 166
;->  177 188 199 200 221 242 263 284 300 331 362 393 400 441
;->  482 500 551 600 661 700 771 800 881 900 991 1000 1001 1002
;->  1003 1004 1005 1006 1007 1008 1009)


-------------------
Somma di fattoriali
-------------------

Sequenza OEIS A007489:
a(n) = Sum[k=1..n]k!
  0, 1, 3, 9, 33, 153, 873, 5913, 46233, 409113, 4037913, 43954713,
  522956313, 6749977113, 93928268313, 1401602636313, 22324392524313,
  378011820620313, 6780385526348313, 128425485935180313,
  2561327494111820313, 53652269665821260313, 1177652997443428940313, ...

(define (a n)
  (if (zero? n) 0
      ;else
      (let ( (sum 0L) (val 1L))
        (for (x 1 n)
          (setq val (* val x))
          (++ sum val))
        sum)))

(a 3)
;-> 9L

(map a (sequence 0 20))
;-> (0 1L 3L 9L 33L 153L 873L 5913L 46233L 409113L 4037913L 43954713L
;->  522956313L 6749977113L 93928268313L 1401602636313L 22324392524313L
;->  378011820620313L 6780385526348313L 128425485935180313L
;->  2561327494111820313L)


-------------------
Calcolo di punteggi
-------------------

In un gioco tutti i giocatori hanno un grado che determina la loro forza di gioco e viene aggiornato dopo ogni partita giocata.
Ci sono 25 gradi regolari e un grado extra, "Leggenda", sopra a questo.
I gradi sono numerati in ordine decrescente, 25 è il grado più basso, 1 il secondo grado più alto e Leggenda il grado più alto.
Ogni grado ha un certo numero di "stelle" che è necessario guadagnare prima di avanzare al grado successivo.
Se un giocatore vince una partita, guadagna una stella.
Se prima della partita il giocatore si trovava nella posizione 6-25 e questa era la terza o più vittorie consecutive, guadagna una stella bonus aggiuntiva per quella
vincere.
Quando avrà tutte le stelle per il suo grado (vedi elenco sotto) e otterrà un'altra stella, guadagnerà invece un grado e avrà una stella sul nuovo grado.
Ad esempio, se prima di una partita vincente il giocatore aveva tutte le stelle del suo grado attuale, dopo la partita avrà guadagnato un grado e avrà 1 o 2 stelle (a seconda se ha ottenuto una stella bonus) sul nuovo grado.
Se d'altra parte avesse tutte le stelle tranne una in un grado e vincesse una partita che le dava anche una stella bonus, guadagnerebbe un grado e avrebbe 1 stella nel nuovo grado.
Se un giocatore di rango 1-20 perde una partita, perde una stella.
Se un giocatore ha zero stelle in un grado e perde una stella, perderà un grado e avrà tutte le stelle meno una nel grado inferiore.
Tuttavia, non si può mai scendere al di sotto del grado 20 (perdere una partita al grado 20 senza stelle non avrà alcun effetto).
Se un giocatore raggiunge il grado Leggenda, rimarrà leggenda, indipendentemente da quante perdite subirà in seguito.
Il numero di stelle su ciascun grado è il seguente:
Grado 25-21: 2 stelle
Grado 20-16: 3 stelle
Grado 15-11: 4 stelle
Grado 10-1:  5 stelle
Grado 0: (Leggenda)
Un giocatore inizia dal grado 25 senza stelle.
Data una lista con lo storico delle partite di un giocatore (es. (W L W W L)), qual è il suo grado alla fine della sequenza delle partite?

Logica generale
---------------

Caso WIN:
controllo Leggenda
Aggiornamento vittorie consecutive
Aggiornamento numero di star
Controllo numero di vittorie consecutive (solo se rank 6-25)
Aggiornamento numero di star
Controllo se passaggio di rank (con eventuale aggiornamento numero di star)

Caso LOSE:
controllo Leggenda
Azzeramento vittorie consecutive
Aggiornamento numero di star (solo se rank 1-20)
Controllo se passaggio di rank (con eventuale aggiornamento numero di star)

(define (win)
  ; se Leggenda non succede nulla
  (if (= rank 0)
      nil
      (begin
        ; aggiornamento vittorie consecutive
        (++ striscia)
        ; aggiornamento star
        (++ star)
        ; controllo bonus per vittorie consecutive
        (if (and (>= rank 6) (<= rank 25) (>= striscia 3)) (++ star))
        ; controllo passaggio di rank superiore
        (cond
          ((and (>= rank 21) (<= rank 25) (> star 2)) (-- rank) (-- star 2))
          ((and (>= rank 16) (<= rank 20) (> star 3)) (-- rank) (-- star 3))
          ((and (>= rank 11) (<= rank 15) (> star 4)) (-- rank) (-- star 4))
          ((and (>= rank 2) (<= rank 10) (> star 5))  (-- rank) (-- star 5))
          ; passaggio a Leggenda
          ((and (= rank 1) (> star 5)) (setq rank 0) (setq star 0))))))

(define (lose)
  ; se Leggenda non succede nulla
  (if (= rank 0) nil
      ;else
      (begin
        ; azzeramento vittorie consecutive
        (setq striscia 0)
        ; aggiornamento star
        ; al grado 20 con 0 stelle non succede nulla
        (if (not (and (= rank 20) (= star 0)))
            (if (and (>= rank 1) (<= rank 20))
                (-- star)))
        ; controllo passaggio di rank inferiore
        (cond
          ((and (>= rank 17) (<= rank 20) (= star -1)) (++ rank) (setq star 2))
          ((and (>= rank 11) (<= rank 15) (= star -1)) (++ rank) (setq star 3))
          ((and (>= rank 1) (<= rank 10) (= star -1))  (++ rank) (setq star 4))))))

(define (check lst)
  (let ( (rank 25) (star 0) (striscia 0) )
    (dolist (el lst)
      (cond ((= el 'W) (win))
            ((= el 'L) (lose))
            (true (println "Error: " el))))
      ;(println rank { } star { } striscia)
    (list rank star striscia)))

Proviamo:

(check '(W W))
;-> 25
(check '(W W W))
;-> 24
(check '(W W W W))
;-> 23
(check '(W L W L W L W L))
;-> 24
(check '(W W W W W W W W W L L W W))
;-> 19
(check '(W W W W W W W W W L W W L))
;-> 18


--------------------------
Lunghezze di sottostringhe
--------------------------

Abbiamo una lista L di interi che rappresentano lunghezze di stringhe e una stringa S costituita solo dai caratteri "x" e "_".
Il carattere "_" non compare mai all'inizio o alla fine della stringa.
Scrivere una funzione che determina se le sottostringhe di S delimitate da "_" hanno esattamente le stesse lunghezze degli interi della lista.
La funzione deve essere la più corta possibile.

Esempi:
  L = (2 3 2)
  S = xx_xxx_xx
  Sottostringhe di S = (xx xxx xx)
  Lunghezze delle sottostringhe di S = (2 3 2)
  Quindi L = lunghezze.

  L = (2 1)
  S = xxxx
  Sottostringhe di S = (xxxx)
  Lunghezze delle sottostringhe di S = (4)
  Quindi L != lunghezze.

  L = (2 3)
  S = xxx_xx
  Sottostringhe di S = (xxx xx)
  Lunghezze delle sottostringhe di S = (3 2)
  Quindi L != lunghezze.

(define (check lst str)
  (= lst (map length (parse str "_"))))

(check '(2 3 2) "xx_xxx_xx")
;-> true

(check '(2 3) "xxx_xx")
;-> nil

Versione code-golf (44 caratteri):
(define(f l s)(= l(map length(parse s"_"))))
(f '(2 3 2) "xx_xxx_xx")
;-> true
(f '(2 3) "xxx_xx")
;-> nil


------------------------
Primi con cifre ripetute
------------------------

Consideriamo i numeri primi di 4 cifre che contengono cifre ripetute, è chiaro che non possono essere tutti uguali:
1111 è divisibile per 11, 2222 è divisibile per 22 e così via.
Esistono però nove numeri primi di 4 cifre che contengono tre uno:
  1117, 1151, 1171, 1181, 1511, 1811, 2111, 4111, 8111
Indichiamo con:

- M(n, d) il numero massimo di cifre ripetute per un numero primo di n cifre
  (dove d è la cifra ripetuta)

- N(n, d) il numero di tali numeri primi

- S(n, d) la somma di questi numeri primi

Quindi M(4, 1) = 3 è il numero massimo di cifre ripetute per un numero primo di 4 cifre in cui la cifra ripetuta è 1.
Ci sono N(4, 1) = 9 di questi numeri primi e la somma di questi numeri primi è S(4, 1) = 22275.
Si scopre che per d = 0, è possibile avere solo M(4, 0) = 2 cifre ripetute, ma ci sono N(4, 0) = 13 casi del genere.
Allo stesso modo otteniamo i seguenti risultati per i numeri primi di 4 cifre.

  +---------+----------+---------+---------+
  | Digit,d |  M(4, d) | N(4, d) | S(4, d) |
  +---------+----------+---------+---------+
  |   0     |   2      |  13     |  67061  |
  |   1     |   3      |   9     |  22275  |
  |   2     |   3      |   1     |   2221  |
  |   3     |   3      |  12     |  46214  |
  |   4     |   3      |   2     |   8888  |
  |   5     |   3      |   1     |   5557  |
  |   6     |   3      |   1     |   6661  |
  |   7     |   3      |   9     |  57863  |
  |   8     |   3      |   1     |   8887  |
  |   9     |   3      |   7     |  48073  |
  +---------+----------+---------+---------+

Trovare i valori di M, N e S per tutti i primi con lunghezza da 1 a 8.

(define (primes-range n1 n2)
"Generate all prime numbers in the interval [n1..n2]"
  (if (> n1 n2) (swap n1 n2))
  (cond ((= n2 1) '())
        ((= n2 2) '(2))
        (true
          (let ((lst '(2)) (arr (array (+ n2 1))))
            ; initialize lst
            (if (> n1 2) (setq lst '()))
            (for (x 3 n2 2)
              (when (not (arr x))
                ; push current primes (x) only if > n1
                (if (>= x n1) (push x lst -1))
                (for (y (* x x) n2 (* 2 x) (> y n2))
                  (setf (arr y) true)))) lst))))

(time (println (length (primes-range 1e6 1e7))))
;-> 586081
;-> 1654.352

(define (calc num-digit)
  (local (M N S primi cifre minimo massimo conta)
    ; array dei valori per M, N e S
    (setq M (array 10 '(0)))
    (setq N (array 10 '(0)))
    (setq S (array 10 '(0)))
    (setq cifre '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
    ; calcolo dei numeri primi con num-digit
    (setq minimo (pow 10 (- num-digit 1)))
    (setq massimo (- (pow 10 num-digit) 1))
    (println "Primes: " (time (setq primi (primes-range minimo massimo))))
    ; ciclo per ogni primo di lunghezza num-digit...
    (dolist (p primi)
      ; conteggio delle cifre del primo corrente
      (setq conta (count cifre (explode (string p))))
      ;(println conta)
      ; ciclo per ogni conteggio...
      (dolist (c conta)
        (cond
          ; se il conteggio della cifra corrente è uguale al massimo
          ; allora aggiorna i valori di N e S (per la cifra corrente)
          ((= c (M $idx))
            (++ (N $idx))
            (++ (S $idx) p))
          ; se il conteggio della cifra corrente è maggiore del massimo
          ; allora resetta i valori di M, N e S (per la cifra corrente)
          ((> c (M $idx))
            (setq (M $idx) c)
            (setq (N $idx) 1)
            (setq (S $idx) p)))))
    (println "M: " M)
    (println "N: " N)
    (println "S: " S) '>))

Proviamo:

(calc 1)
;-> Primes: 0
;-> M: (0 0 1 1 0 1 0 1 0 0)
;-> N: (4 4 1 1 4 1 4 1 4 4)
;-> S: (17 17 2 3 17 5 17 7 17 17)

(calc 2)
;-> Primes: 0
;-> M: (0 2 1 1 1 1 1 1 1 1)
;-> N: (21 1 2 8 3 2 2 8 2 6)
;-> S: (1043 11 52 356 131 112 128 488 172 372)

(calc 3)
;-> Primes: 0
;-> M: (1 2 2 2 2 2 2 2 2 2)
;-> N: (15 10 3 9 2 1 1 10 3 7)
;-> S: (6883 3112 679 3489 892 557 661 7226 2651 5133)

(calc 4)
;-> Primes: 0
;-> M: (2 3 3 3 3 3 3 3 3 3)
;-> N: (13 9 1 12 2 1 1 9 1 7)
;-> S: (67061 22275 2221 46214 8888 5557 6661 57863 8887 48073)

(calc 5)
;-> Primes: 10.112
;-> M: (3 4 4 4 4 3 3 4 4 4)
;-> N: (8 10 1 7 1 28 31 12 1 8)
;-> S: (450046 115756 22229 226559 44449 1506848 1999373 907810 88883 683904)

(calc 6)
;-> Primes: 144.686
;-> M: (4 5 4 5 5 5 5 5 5 5)
;-> N: (9 13 33 14 2 1 1 11 1 10)
;-> S: (4200049 3358141 10838231 5065904 888892 555557 666667 8510217 888887 8580230)

(time (println (calc 7)))
;-> Primes: 1639.361
;-> M: (5 6 5 6 5 5 5 6 5 6)
;-> N: (5 7 48 13 43 35 41 8 38 7)
;-> S: (25000027 7847587 117535528 43755197 187966743 186146245
;->     267152609 61765166 311771476 57829215)
;-> 3730.04

(time (println (calc 8)))
;-> M: (6 7 7 7 6 7 7 7 7 7)
;-> N: (3 16 1 11 34 2 1 4 1 13)
;-> S: (90000007 288629940 22222223 415038501 1567796906 111111112
;->     66666667 311100078 88888883 1099396937)
;-> 37474.613

Non provare "(time (println (calc 9)))" con meno di 64 Gb di RAM.


----------------------------------
Funzione booleane con due ingressi
----------------------------------

La seguente tabella mostra tutte le 16 possibili funzioni booleane con due ingressi (X e Y).
Nella tabella il valore 1 rappresenta 'true' e il valore 0 rappresenta 'nil'.

           Z        N     N     N     X
           E  N     O     O  X  A  A  N                 O
           R  O     T     T  O  N  N  O              O  N
           O  R     X     Y  R  D  D  R  Y     X     R  E
  +------+------------------------------------------------+
  | X  Y | 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F |
  |------|------------------------------------------------|
  | 0  0 | 0  1  0  1  0  1  0  1  0  1  0  1  0  1  0  1 |
  | 0  1 | 0  0  1  1  0  0  1  1  0  0  1  1  0  0  1  1 |
  | 1  0 | 0  0  0  0  1  1  1  1  0  0  0  0  1  1  1  1 |
  | 1  1 | 0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1 |
  +------+------------------------------------------------+

Scrivere le 16 funzioni utilizzando solo gli operatori 'and', 'or' e 'not'.

(define (f0 x y) nil)                                     ;zero
(define (f1 x y) (not (or x y)))                          ;nor
(define (f2 x y) (not (or x (not y))))
(define (f3 x y) (not x))                                 ;not x
(define (f4 x y) (and x (not y)))
(define (f5 x y) (not y))                                 ;not y
(define (f6 x y) (if (not (and x y)) (or x y) nil))       ;xor
(define (f7 x y) (not (and x y)))                         ;nand
(define (f8 x y) (and x y))                               ;and
(define (f9 x y) (not (if (not (and x y)) (or x y) nil))) ;xnor
(define (fA x y) y)                                       ;y
(define (fB x y) (not (and x (not y))))
(define (fC x y) x)                                       ;x
(define (fD x y) (or x (not y)))
(define (fE x y) (or x y))                                ;or
(define (fF x y) true)                                    ;one

Per verificare le funzioni scriviamo una funzione che stampa la tabella (con 0 e 1).

(define (bool16)
  (local (func xval yval xbool ybool x y xb yb out)
    (setq func (list f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 fA fB fC fD fE fF))
    (setq xval '(nil nil true true))
    (setq yval '(nil true nil true))
    (setq xbool'(0 0 1 1))
    (setq ybool'(0 1 0 1))
    (println " X  Y  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F")
    (for (i 0 3)
      (setq x (xval i))
      (setq y (yval i))
      (setq xb (xbool i))
      (setq yb (ybool i))
      (print (format "%2d %2d" xb yb))
      (dolist (el func)
        (if ((eval el) x y) (setq out 1) (setq out 0))
        (print (format "%3d" out)))
      (println)) '>))

(bool16)
;-> X  Y  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
;-> 0  0  0  1  0  1  0  1  0  1  0  1  0  1  0  1  0  1
;-> 0  1  0  0  1  1  0  0  1  1  0  0  1  1  0  0  1  1
;-> 1  0  0  0  0  0  1  1  1  1  0  0  0  0  1  1  1  1
;-> 1  1  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1


---------------------------
Funzioni booleane con 0 e 1
---------------------------

Le funzioni booleane di newLISP accettano solo i parametri 'true' e 'nil'.
Altri linguaggi utilizzano 1 per il valore 'true' e 0 per il valore 'nil.

              N     N     X
           N  O  X  A  A  N
           O  T  O  N  N  O  O
           R  X  R  D  D  R  R
  +------+---------------------+
  | X  Y | Z  Z  Z  Z  Z  Z  Z |
  |------|---------------------|
  | 0  0 | 1  1  0  1  0  1  0 |
  | 0  1 | 0  1  1  1  0  0  1 |
  | 1  0 | 0  0  1  1  0  0  1 |
  | 1  1 | 0  0  0  0  1  1  1 |
  +------+---------------------+

Scriviamo le funzioni 'and' 'or' e 'not' che accettano come parametri 0 e 1.

(define (and01 x y) (if (and (= x 1) (= y 1)) 1 0))
(define (or01 x y) (if (or (= x 1) (= y 1)) 1 0))
(define (not01 x) (if (zero? x) 1 0))

Possiamo definire gli operatori 'nor', 'xor', 'nand' e 'xnor'.

Con le funzioni 'and01', 'or01' e 'not01':

(define (nand01 x y) (not01 (and01 x y)))
(define (nor01 x y) (not01 (or01 x y)))
(define (xor01 x y) (if (not01 (and01 x y)) (or01 x y) nil))
(define (xnor01 x y) (not01 (if (not01 (and01 x y)) (or01 x y) nil)))

Oppure in modo indipendente:

(define (nand01 x y) (if (and (= x 1) (= y 1)) 0 1))
(define (nor01 x y) (if (or (= x 1) (= y 1)) 0 1))
(define (xor01 x y) (if (or (= x y 1) (= x y 0)) 0 1))
(define (xnor01 x y) (if (or (= x y 1) (= x y 0)) 1 0))

Funzione che verifica la correttezza delle funzioni booleane:

(define (bool-check)
  (local (func xbool ybool x y out)
    (setq func (list nor01 not01 xor01 nand01 and01 xnor01 or01))
    (setq xbool'(0 0 1 1))
    (setq ybool'(0 1 0 1))
    (for (i 0 3)
      (setq x (xbool i))
      (setq y (ybool i))
      (print (format "%2d %2d" x y))
      (dolist (el func)
        (print (format "%3d" ((eval el) x y))))
      (println)) '>))

(bool-check)
;->  0  0  1  1  0  1  0  1  0
;->  0  1  0  1  1  1  0  0  1
;->  1  0  0  0  1  1  0  0  1
;->  1  1  0  0  0  0  1  1  1

Per essere utilizzabili con newLISp occorre restituire 'true' e 'nil' invece di 0 e 1.

(define (and01 x y)  (if (and (= x 1) (= y 1)) true nil))
(define (or01 x y)   (if (or (= x 1) (= y 1)) true nil))
(define (not01 x)    (if (zero? x) true nil))
(define (nand01 x y) (if (and (= x 1) (= y 1)) nil true))
(define (nor01 x y)  (if (or (= x 1) (= y 1)) nil true))
(define (xor01 x y)  (if (or (= x y 1) (= x y 0)) nil true))
(define (xnor01 x y) (if (or (= x y 1) (= x y 0)) true nil))


-----------------------------------
Mappe di Karnaugh (Quine-McCluskey)
-----------------------------------

La mappa di Karnaugh è un metodo di rappresentazione esatta di sintesi di reti combinatorie a uno o più livelli.
Una tale mappa costituisce una rappresentazione visiva di una funzione booleana in grado di mettere in evidenza le coppie di mintermini o di maxtermini a distanza di Hamming unitaria (ovvero di termini che differiscono per una sola variabile binaria (o booleana)).
Poiché derivano da una meno intuitiva visione delle funzioni booleane in spazi (0,1)^n con n numero delle variabili della funzione, le mappe di Karnaugh risultano applicabili efficacemente solo a funzioni con al più 5 - 6 variabili.
La mappa di Karnaugh è stata inventata nel 1953 da Maurice Karnaugh, ingegnere delle telecomunicazioni impiegato presso i Bell Laboratories.
Una mappa di Karnaugh è un metodo grafico che ha come obiettivo quello di ridurre la complessità delle funzioni booleane espresse in forme canoniche.
Essa si costruisce a partire dalla tabella della verità di una funzione booleana, nel processo di sintesi di una rete combinatoria.
Le mappe di Karnaugh permettono di costruire semplicemente la forma minima di una funzione come somma di prodotti logici (forma disgiuntiva) o come prodotto di somme logiche (forma congiuntiva) e quindi semplificazioni della funzione booleana spesso più immediate di quelle ottenibili con modifiche algebriche.

Un risolutore di mappe di Karnaugh può essere visto come un problema di copertura ottimale dei mintermini.
L'idea generale è:

1. Costruire la mappa
Data una funzione booleana di (n) variabili:
- ogni cella rappresenta una combinazione delle variabili;
- righe e colonne sono ordinate in codice Gray;
- si inseriscono:
  - 1 per i mintermini veri,
  - 0 per quelli falsi,
  - eventualmente X per i "don't care".

2. Cercare tutti i gruppi validi
Un gruppo valido deve:
- contenere solo 1 e/o X;
- avere dimensione potenza di due: 1, 2, 4, 8, 16, ...
- essere rettangolare;
- poter attraversare i bordi della mappa (top-bottom, left-right).
Per esempio, in una mappa a 4 variabili:
  gruppi 1×1
  gruppi 1×2
  gruppi 2×2
  gruppi 1×4
  gruppi 2×4
  gruppi 4×4
  ecc.
Conviene generare tutti i rettangoli possibili con lati di lunghezza potenza di due.

3. Eliminare i gruppi ridondanti
Se un gruppo è completamente contenuto in un gruppo più grande, normalmente non serve.
Ad esempio:
- gruppo di 2 celle
- gruppo di 4 celle che contiene le stesse celle
si conserva solo quello di 4.
In questo modo si ottengono i primi implicanti.

4. Trovare gli implicanti essenziali
Per ogni mintermine uguale a 1 si determina quali implicanti lo coprono.
Se un mintermine è coperto da un solo implicante, quell'implicante è essenziale.
Tutti gli implicanti essenziali entrano automaticamente nella soluzione.

5. Coprire i mintermini rimanenti
Dopo aver scelto gli implicanti essenziali alcuni mintermini potrebbero restare scoperti.
Occorre scegliere il più piccolo insieme di implicanti che li copra tutti.
Questa è una variante del problema del set cover.
Per mappe fino a 6 variabili è spesso sufficiente:
- provare tutte le combinazioni degli implicanti residui;
- scegliere quella con:
  1. minor numero di termini;
  2. a parità, minor numero totale di letterali.
Questo equivale sostanzialmente al metodo di **Petrick**.

6. Convertire i gruppi in termini booleani
Per ogni gruppo:
- si osservano le variabili che non cambiano;
- le variabili che cambiano vengono eliminate.
Esempio:
| A | B | C |
| - | - | - |
| 0 | 1 | 0 |
| 0 | 1 | 1 |
C cambia, quindi scompare.  _
Il termine risultante è:    AB

7. Rappresentazione interna
Un modo molto comodo è rappresentare ogni implicante come:
- mask = bit delle variabili significative;
- value = valore delle variabili significative.
Per esempio:
  A = 1
  B = qualsiasi
  C = 0
  D = qualsiasi
può essere rappresentato come:
  mask  = 1010
  value = 1000
Questa rappresentazione è la stessa usata nell'algoritmo di Quine-McCluskey e rende molto facile:
- verificare se un mintermine appartiene a un gruppo;
- confrontare implicanti;
- generare l'espressione finale.

8. Approccio alternativo
Invece di lavorare direttamente sulla mappa, si può usare:
1. Quine-McCluskey per generare tutti i primi implicanti;
2. Metodo di Petrick per scegliere quelli ottimali.

Il seguente programma implementa una versione semplificata dell'algoritmo di Quine-McCluskey, che ha lo stesso obiettivo delle mappe di Karnaugh: trovare una rappresentazione booleana equivalente ma più compatta.

1) Rappresentazione dei mintermini
----------------------------------
Ogni mintermine viene rappresentato come una stringa binaria.
Esempio:
  10110 rappresenta: A=1 B=0 C=1 D=1 E=0
  cioè:
  A and (not B) and C and D and (not E)

2) Fusione di due mintermini
----------------------------
Due mintermini possono essere fusi se differiscono in una sola posizione.
Esempio:
  10110
  10111
  differiscono solo nell'ultima posizione.
  La fusione produce: 1011-
  Il simbolo '-' significa: questa variabile può valere sia 0 che 1
  Perciò 1011- copre contemporaneamente:
  10110
  10111
  e corrisponde a: A and (not B) and C and D
  La variabile E scompare.

3) Fusioni successive
---------------------
Anche i termini contenenti '-' possono essere fusi.
Ad esempio:
  1011-
  1111-
  diventano: 1-11-
  e così via.

Ogni fusione elimina una variabile dalla formula.

4) Primi implicanti
-------------------
Quando un termine non può più essere fuso con nessun altro viene chiamato 'primo implicante'.
Ad esempio:
  1--01
  può rappresentare il gruppo:
  10001
  10101
  11001
  11101

5) Tabella di copertura
-----------------------
Dopo aver trovato tutti i primi implicanti, il programma costruisce una tabella:
implicante -> mintermini coperti
Esempio:
  1--01 -> (10001 10101 11001 11101)

6) Implicanti essenziali
------------------------
Un implicante è essenziale se copre almeno un mintermine che nessun altro implicante copre.
Se un mintermine compare una sola volta nella tabella, l'implicante corrispondente deve necessariamente appartenere alla soluzione finale.

7) Costruzione della formula
----------------------------
Ogni implicante viene convertito in una espressione booleana.
Esempio:
  1--01 significa: A = 1, D = 0, E = 1
  quindi: A and (not D) and E
Infine tutti gli implicanti vengono uniti tramite OR.

; bitcount
; Conta il numero di bit uguali a '1' presenti
; in una stringa binaria.
; Esempio:
; "101101" -> 4
; Questa funzione non viene utilizzata nella versione
; finale del programma, ma è spesso impiegata nelle
; implementazioni classiche di Quine-McCluskey per
; raggruppare i mintermini in base al numero di 1.
(define (bitcount s)
  (length (find-all "1" s)))

; diff?
; Verifica se due termini differiscono in una sola posizione.
; Restituisce:
;   indice della posizione diversa
; oppure:
;   nil se le differenze sono 0 oppure > 1
; Esempi:
; "1010" "1011" -> 3
; "1010" "1111" -> nil
; La funzione è il cuore della fase di fusione.
(define (diff1? a b)
  (letn ((pos -1)
         (cnt 0))
    (for (i 0 (- (length a) 1))
      (if (!= (a i) (b i))
          (begin
            (++ cnt)
            (setq pos i))))
    (if (= cnt 1) pos nil)))

; merge-term
; Tenta di fondere due termini.
; Se differiscono in una sola posizione,
; il bit differente viene sostituito con '-'.
; Esempio: 1010 1011 -> 101-
; Se la fusione non è possibile restituisce nil.
(define (merge-term a b)
  (let (p (diff1? a b))
    (if p
        (string (slice a 0 p) "-" (slice a (+ p 1)))
        nil)))

; qm-step
; Esegue un singolo livello dell'algoritmo
; di Quine-McCluskey.
; Confronta tutti i termini due a due.
; Output:
; merged
;   true se è stata effettuata almeno una fusione
; used
;   indici dei termini che hanno partecipato
;   ad almeno una fusione
; next
;   nuovi termini ottenuti dalle fusioni
; I termini che non compaiono in 'used'
; diventeranno primi implicanti.
(define (qm-step terms)
  (letn ((used '())
         (next '())
         (merged nil)
         (n (length terms)))
    (if (< n 2)
        (list nil '() '())
        (begin
          (for (i 0 (- n 2))
            (for (j (+ i 1) (- n 1))
              (let (m (merge-term (terms i) (terms j)))
                (if m
                    (begin
                      (setq merged true)
                      (push i used -1)
                      (push j used -1)
                      (if (not (ref m next))
                          (push m next -1)))))))
          (list merged used next)))))

; covers?
; Verifica se un implicante copre un mintermine.
; Il simbolo '-' viene considerato un wildcard.
; Esempio:
; implicante = 1--01
; mintermine = 10101
; risultato = true
(define (covers? implicant minterm)
  (let (ok true)
    (for (i 0 (- (length implicant) 1))
      (if (and (!= (slice implicant i 1) "-")
               (!= (slice implicant i 1) (slice minterm i 1)))
          (setq ok nil)))
    ok))

; covered
; Restituisce l'insieme degli implicanti che
; coprono almeno uno dei mintermini indicati.
; La funzione elimina automaticamente i duplicati.
(define (covered implicants minterms)
  (let (out '())
    (dolist (m minterms)
      (dolist (p implicants)
        (if (covers? p m)
            (push p out -1))))
    (unique out)))

; term->expr
; Converte un implicante nella corrispondente
; espressione booleana.
; Esempio:
; 1--01 -> (A and (not D) and E)
(define (term->expr termine)
  (let (vars "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        out '())
    (for (i 0 (- (length termine) 1))
      (let (b (slice termine i 1))
        (cond
          ((= b "1")
            (push (slice vars i 1) out -1))
          ((= b "0")
            (push (string "(not " (slice vars i 1) ")") out -1)))))
    (cond
      ((= (length out) 0) "true")
      ((= (length out) 1) (out 0))
      (true (string "(" (join out " and ") ")")))))

; solution->expr
; Converte una lista di implicanti nella
; formula booleana completa.
; Esempio:
; ("1--" "--0") -> ((A) or ((not C)))
(define (solution->expr solution)
  (string "(" (join (map term->expr solution) " or ") ")"))

; quine
; Funzione principale.
; Fasi:
; 1) genera tutti i primi implicanti
; 2) costruisce la tabella di copertura
; 3) individua gli implicanti essenziali
; 4) costruisce la soluzione
; 5) converte la soluzione in formula booleana
; Input:
;   lista di mintermini binari
; Output:
;   formula logica semplificata
; Durante l'esecuzione stampa tutti i passaggi
; intermedi per scopi didattici e di debug.
(define (quine minterms)
  (local (current primes step merged used next table essential remain best)
    (setq current minterms)
    (setq primes '())
    (setq go true)
    (while go
      (println "Livello:")
      (println current)
      (setq step (qm-step current))
      (setq merged (step 0))
      (setq used (step 1))
      (setq next (step 2))
      (for (i 0 (- (length current) 1))
        (if (not (ref i used))
            (if (not (ref (current i) primes))
                (push (current i) primes -1))))
      (if merged
          (setq current next)
          (setq go nil)))
    (println "Primi implicanti:")
    (println primes)
    (setq table '())
    (dolist (p primes)
      (let (lst '())
        (dolist (m minterms)
          (if (covers? p m)
              (push m lst -1)))
        (push (list p lst) table -1)))
    (println "Copertura:")
    (dolist (x table)
      (println (x 0) " -> " (x 1)))
    (setq essential '())
    (dolist (m minterms)
      (let (hit '())
        (dolist (x table)
          (if (ref m (x 1))
              (push (x 0) hit -1)))
        (if (= (length hit) 1)
            (push (hit 0) essential))))
    (setq essential (unique essential))
    (println "Essenziali:")
    (println essential)
    (setq remain '())
    (dolist (m minterms)
      (let (found nil)
        (dolist (e essential)
          (if (covers? e m)
              (setq found true)))
        (if (not found)
            (push m remain -1))))
    (if (= (length remain) 0)
        (setq best '())
        (setq best primes))
    (println "Soluzione:")
    (println (setq solution (unique (append essential best))))
    (println "Formula:")
    (solution->expr solution)))

L'algoritmo implementato è una versione didattica di Quine-McCluskey: genera i primi implicanti tramite fusioni successive, costruisce la tabella di copertura e produce una formula booleana leggibile usando 'and', 'or' e 'not'.

Proviamo:

(quine '("000" "010" "100" "101" "110" "111"))
;-> ...
;-> (A or (not C))

(quine '("0000" "0001" "0010" "0011" "1000" "1001" "1010" "1011" "1110" "1111"))
;-> ...
;-> "((A and C) or (not B))"
La variabile D è completamente eliminata perché in tutti i gruppi scelti può cambiare senza modificare l'uscita.
Questo è proprio uno degli obiettivi della minimizzazione di Karnaugh/Quine-McCluskey: eliminare le variabili non necessarie.
In generale, se una variabile non compare nella formula finale significa che la funzione è indipendente da quella variabile.

(quine '("011" "010" "110" "111" "000" "001"))
;-> "((not A) or B)"

(quine '("01101" "11001" "11111" "11101" "10100" "00001" "10101" "10001"))
;-> ...
;-> (((not B) and (not C) and (not D) and E) or (A and (not B) and C and
;->  (not D)) or (A and B and C and E) or (A and (not D) and E) or
;->  (B and C and (not D) and E))
La minimizzazione non sempre riduce drasticamente la formula.
Se i mintermini sono scelti quasi casualmente, spesso la forma minimizzata resta abbastanza complessa.

(quine '("00000" "00001" "00010" "00011"
         "00100" "00101" "00110" "00111"
         "10000" "10001" "10010" "10011"
         "10100" "10101" "10110" "10111"))
;-> ...
;-> "((not B))"


----------------
Catene non vuote
----------------

Abbiamo una lista di interi non negativi, per esempio, (1 2 4 7 14 28).
Le 'catene non vuote' sono i seguenti sottoinsiemi dei numeri della lista:

  (1) (2) (4) (7) (14) (28)
  (1 2) (1 4) (1 7) (1 14) (1 28) (2 4)
  (2 14) (2 28) (4 28) (7 14) (7 28) (14 28)
  (1 2 4) (1 2 14) (1 2 28) (1 4 28) (1 7 14)
  (1 7 28) (1 14 28) (2 4 28) (2 14 28) (7 14 28)
  (1 2 4 28) (1 2 14 28) (1 7 14 28)

Queste catene sono quei sottoinsiemi non vuoti di (1 2 4 7 14 28) tali che tutte le coppie di elementi (a, b) soddisfano o a/b o b/a ovvero, uno è divisore dell'altro.
Poiché 2 non divide 7 e 7 non divide 2, nessuna catena ha un sottoinsieme di (2 7).
Analogamente, nessuna catena ha un sottoinsieme di (4 7) o (4 14).

Un modo semplice è generare tutti i sottoinsiemi non vuoti della lista e verificare se formano una catena.
Un sottoinsieme è una catena se, per ogni coppia di elementi (a b), 'a' divide 'b' oppure 'b' divide 'a'.

Algoritmo
Generazione di tutti i sottoinsiemi della lista.
Per ogni sottoinsieme non vuoto S:
1) Poniamo ok = true.
2) Per ogni coppia (i,j) con (i < j):
   - a = S[i]
   - b = S[j]
   - se (a % b != 0) e (b % a != 0) allora
     - 'ok = nil'
     - interrompiamo i cicli.
3) Se (ok == true), aggiungiamo S all'output.

Con una lista di 'n' elementi i sottoinsiemi sono (2^n - 1) e considereando che la verifica di una catena costa O(n^2), il costo totale è circa O(2^n*n^2) che va bene per liste di piccole dimensioni.

Una versione più efficiente consiste nel costruire prima la matrice di compatibilità:
  compat[i][j] = true, se a divide b oppure b divide a
Poi ogni sottoinsieme è una catena se tutte le coppie di elementi sono compatibili.
Nell' esempio (1 2 4 7 14 28) le sole coppie incompatibili sono:
  (2 7)
  (4 7)
  (4 14)
Perciò una catena è semplicemente un sottoinsieme che non contiene nessuna di queste coppie.

Algoritmo
1) Costruire il grafo delle incompatibilità.
2) Generare ricorsivamente le catene aggiungendo un elemento solo se è compatibile con tutti quelli già scelti.
In pratica si costruiscono direttamente le catene valide senza enumerare tutti i 2^n sottoinsiemi.

(define (catene lst show)
  ; Genera tutte le catene non vuote rispetto alla relazione di divisibilità.
  ; Una catena è un insieme di numeri dove ogni coppia di elementi è confrontabile:
  ; a divide b oppure b divide a.
  ; L'algoritmo costruisce le catene progressivamente:
  ; - parte dalla catena vuota
  ; - prova ad aggiungere ogni elemento successivo
  ; - aggiunge l'elemento solo se è compatibile con tutti gli elementi
  ;   già presenti nella catena corrente.
  (local (out)
    ; Verifica se due numeri sono confrontabili secondo la divisibilità.
    ; Restituisce true se:
    ;   a divide b   oppure   b divide a
    ; Esempi:
    ;   2 e 14  -> true  (2 divide 14)
    ;   7 e 4   -> nil   (nessuno divide l'altro)
    (define (compatibile? a b)
      (or (= 0 (% a b))
          (= 0 (% b a))))
    ; Procedura ricorsiva che estende una catena esistente.
    ; chain : catena costruita fino ad ora
    ; start : indice dal quale iniziare a provare nuovi elementi
    ; Per esempio:
    ; chain = (1 2)
    ; start = 2
    ; proverà ad aggiungere gli elementi dopo il 2: 4, 7, 14, 28
    ; Quando trova un elemento compatibile:
    ; 1) crea una nuova catena
    ; 2) la salva in output
    ; 3) continua ricorsivamente cercando estensioni più lunghe
    (define (espandi chain start)
      (if show (println "chain = " chain " start = " start))
      ; Evita di accedere fuori dalla lista quando start
      ; è uguale alla lunghezza della lista
      (if (< start (length lst))
        (for (i start (- (length lst) 1))
          (let (x (lst i))
            (if show (println "  provo: " x))
            ; Supponiamo inizialmente che x possa essere aggiunto
            (setq ok true)
            ; Controlliamo x contro tutti gli elementi
            ; della catena già costruita
            (dolist (y chain)
              ; Se troviamo una coppia incompatibile,
              ; la nuova catena non è valida
              (if (not (compatibile? x y))
                (begin
                  (if show (println "    incompatibile con " y))
                  (setq ok nil))))
            ; Se x è compatibile con tutta la catena:
            ; possiamo creare una nuova catena valida
            (if ok
              (begin
                ; Aggiunge x alla fine della catena corrente
                (setq nuova (append chain (list x)))
                (if show (println "    nuova catena: " nuova))
                ; Salva la nuova catena trovata.
                ; push con -1 mantiene l'ordine di generazione
                (push nuova out -1)
                ; Cerca ulteriori estensioni usando solo
                ; gli elementi dopo x
                (espandi nuova (+ i 1))))))))
    ; Lista risultato
    (setq out '())
    ; Avvia la costruzione partendo dalla catena vuota.
    ; La prima aggiunta creerà le catene di un solo elemento.
    (espandi '() 0)
    out))

La caratteristica importante è questa parte:
  (espandi nuova (+ i 1))
che impone che gli elementi vengano aggiunti solo andando avanti nella lista.
Quindi non genera duplicati tipo (1 2 4), (1 4 2), (2 1 4), ma considera ogni sottoinsieme una sola volta.

Proviamo:

(catene '(1 2 4 7 14 28))
;-> ((1) (1 2) (1 2 4) (1 2 4 28) (1 2 14) (1 2 14 28) (1 2 28) (1 4) (1 4 28)
;->  (1 7) (1 7 14) (1 7 14 28) (1 7 28) (1 14) (1 14 28) (1 28) (2) (2 4)
;->  (2 4 28) (2 14) (2 14 28) (2 28) (4) (4 28) (7) (7 14) (7 14 28) (7 28)
;->  (14) (14 28) (28))

(catene '(1 2 3 4 5))
;-> ((1) (1 2) (1 2 4) (1 3) (1 4) (1 5) (2) (2 4) (3) (4) (5))


Sequenza OEIS A253249:
Number of nonempty chains in the divides relation on the divisors of n.
  1, 3, 3, 7, 3, 11, 3, 15, 7, 11, 3, 31, 3, 11, 11, 31, 3, 31, 3, 31, 11,
  11, 3, 79, 7, 11, 15, 31, 3, 51, 3, 63, 11, 11, 11, 103, 3, 11, 11, 79,
  3, 51, 3, 31, 31, 11, 3, 191, 7, 31, 11, 31, 3, 79, 11, 79, 11, 11, 3,
  175, 3, 11, 31, 127, 11, 51, 3, 31, 11, 51, ...

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
         (push cur-divisor out -1))
        (true
         (for (i 0 (f cur-index 1))
           (divisors-aux (+ cur-index 1) cur-divisor)
           (setq cur-divisor (* cur-divisor (f cur-index 0)))))))

(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))

(map (fn(x) (length (catene (divisors x)))) (sequence 1 50))
;-> (1 3 3 7 3 11 3 15 7 11 3 31 3 11 11 31 3 31 3 31 11
;->  11 3 79 7 11 15 31 3 51 3 63 11 11 11 103 3 11 11 79
;->  3 51 3 31 31 11 3 191 7 31)


-------------
Barcode EAN-8
-------------

Un codice a barre EAN-8 include 7 cifre di informazioni e un'ottava cifra di controllo.
Il checksum viene calcolato moltiplicando le cifre alternativamente per 3 e 1, sommando i risultati e sottraendo il risultato dal multiplo di 10 successivo.
Ad esempio, date le cifre 2103498:

Cifre:            2   1   0   3   4   9   8
Multiplicatore:   3   1   3   1   3   1   3
Risultato:        6   1   0   3  12   9  24
La somma di queste cifre risultanti è 55, quindi il checksum vale 60 - 55 = 5.

Scrivere una funzione che prende le 8 cifre come lista e verifica il checksum.

Per calcolare il multiplo di 10 successivo a un numero intero positivo usiamo la seguente formula:

  x(succ) = ((x + 9)/10) * 10) (eseguendo una divisione intera)

(define (check ean)
  (let (res (apply + (map * (chop ean) '(3 1 3 1 3 1 3))))
    (setq res (- (* (/ (+ res 9) 10) 10) res))
    (= (ean -1) res)))

Proviamo:

(check '(2 1 0 3 4 9 8 5))
;-> true
(check '(2 1 0 3 4 9 8 4))
;-> nil

Versione code-golf (100 caratteri):

(define(f e)(let(r(apply +(map *(chop e)'(3 1 3 1 3 1 3))))(setq r(-(*(/(+ r 9)10)10)r))(=(e -1)r)))

(f '(2 1 0 3 4 9 8 5))
;-> true
(f '(2 1 0 3 4 9 8 4))
;-> nil


------------------
Norma di Frobenius
------------------

La norma di Frobenius è una norma matriciale definita come la radice quadrata della somma dei quadrati assoluti dei suoi elementi.
Viene definita come la radice quadrata della somma dei quadrati di tutti gli elementi di una matrice, misurandone la 'dimensione'.
Per una matrice A di dimensioni m × n con elementi a(i, j), la formula è:

  AF = sqrt(Somma[i=1,m]Somma[j=1,n] |(a(i, j)|^2))

In sostanza, tratta una matrice come un vettore lungo e ne calcola la lunghezza euclidea standard.
Viene spesso utilizzata per misurare le distanze tra matrici.

(define (frobenius matrix)
  (sqrt (apply add (map (fn(x) (mul x x)) (flat matrix)))))

(frobenius '((1 2 3) (4 5 6) (7 8 9)))
;-> 16.88194301613413

(frobenius '((1 3) (-2 4)))
;-> 5.477225575051661

(frobenius '((2 0 -1) (3 5 -2)))
;-> 6.557438524302

La norma di Frobenius può essere calcolata anche con la formula seguente:

  AF = sqrt(Traccia(AH*A)), dove AH è la trasposta coniugata di A.

La matrice trasposta coniugata di una matrice M a valori complessi è la matrice ottenuta effettuando la trasposta di M e scambiando ogni valore con il suo complesso coniugato.
Nel caso di matrici con valori reali, la matrice trasposta coniugata di M è uguale alla trasposta di M (perchè il coniugato di un numero reale è il numero reale stesso).

(define (matrix-trace M)
  (let ((n (length M)) (s 0))
    (dotimes (i n)
      (setf s (add s (M i i))))
    s))

(define (froben matrix)
  (sqrt (matrix-trace (multiply (transpose matrix) matrix))))

(froben '((1 2 3) (4 5 6) (7 8 9)))
;-> 16.88194301613413

(froben '((1 3) (-2 4)))
;-> 5.477225575051661

(froben '((2 0 -1) (3 5 -2)))
;-> 6.557438524302


---------------------------------------
Numeri di Wostenholme (Numeri armonici)
---------------------------------------

I numeri di Wostenholme sono i numeratori dei numeri armonici di ordine 2:

  H2(k) = Sum[i=1,k]1/i = 1 + 1/4 + 1/9 + ... + 1/(k^2)

Sequenza OEIS A007406:
Wolstenholme numbers: numerator of Sum[k=1,n]1/k^2.
  1, 5, 49, 205, 5269, 5369, 266681, 1077749, 9778141, 1968329, 239437889,
  240505109, 40799043101, 40931552621, 205234915681, 822968714749,
  238357395880861, 238820721143261, 86364397717734821, 17299975731542641,
  353562301485889, 354019312583809, 187497409728228241, ...

; Minimal rational
(define (rat n d)
  (cond
    ((zero? d) (throw-error "rat: denominator is zero"))
    ((zero? n) '(0L 1L))
    (true (letn ((g (gcd n d)) (nn (/ n g)) (dd (/ d g)))
            (if (< dd 0) (list (- nn) (- dd)) (list nn dd))))))
(define (+rat r1 r2) (rat (+ (* (r1 0) (r2 1)) (* (r2 0) (r1 1))) (* (r1 1) (r2 1))))
(define (-rat r1 r2) (rat (- (* (r1 0) (r2 1)) (* (r2 0) (r1 1))) (* (r1 1) (r2 1))))
(define (*rat r1 r2) (rat (* (r1 0) (r2 0)) (* (r1 1) (r2 1))))
(define (/rat r1 r2)
  (if (zero? (r2 0))
      (throw-error "/rat: division by zero")
      (rat (* (r1 0) (r2 1)) (* (r1 1) (r2 0)))))
(define (+r) (apply +rat (args) 2))
(define (-r) (apply -rat (args) 2))
(define (*r) (apply *rat (args) 2))
(define (/r) (apply /rat (args) 2))

(define (wostenholme limite)
  (let ((h '(0L 1L)) (out '()))
    (for (k 1 limite)
      (setq h (+rat h (list 1L (* 1L k k))))
      (push (h 0) out -1))
    out))

(wostenholme 20)
;-> (1L 5L 49L 205L 5269L 5369L 266681L 1077749L 9778141L 1968329L 239437889L
;->  240505109L 40799043101L 40931552621L 205234915681L 822968714749L
;->  238357395880861L 238820721143261L 86364397717734821L 17299975731542641L)

Vediamo i denominatori:

Sequenza OEIS A007407:
a(n) = denominator of Sum_{k=1..n} 1/k^2.
  1, 4, 36, 144, 3600, 3600, 176400, 705600, 6350400, 1270080, 153679680,
  153679680, 25971865920, 25971865920, 129859329600, 519437318400,
  150117385017600, 150117385017600, 54192375991353600, 10838475198270720,
  221193371393280,...

(define (wostenholme-denom limite)
  (let ((h '(0L 1L)) (out '()))
    (for (k 1 limite)
      (setq h (+rat h (list 1L (* 1L k k))))
      (push (h 1) out -1))
    out))

(wostenholme-denom 20)
;-> (1L 4L 36L 144L 3600L 3600L 176400L 705600L 6350400L 1270080L 153679680L
;->  153679680L 25971865920L 25971865920L 129859329600L 519437318400L
;->  150117385017600L 150117385017600L 54192375991353600L 10838475198270720L)

Vediamo i numeratori e i denominatori dei numeri armonici di ordine 1:

  H1(k) = Sum[i=1,k]1/i = 1 + 1/2 + 1/3 + ... + 1/k

Sequenza OEIS A001008:
a(n) = numerator of harmonic number H(n) = Sum[i=1,n]1/i.
  1, 3, 11, 25, 137, 49, 363, 761, 7129, 7381, 83711, 86021, 1145993,
  1171733, 1195757, 2436559, 42142223, 14274301, 275295799, 55835135,
  18858053, 19093197, 444316699, 1347822955, 34052522467, 34395742267,
  312536252003, 315404588903, 9227046511387, ...

(define (harmo-numer limite)
  (let ((h '(0L 1L)) (out '()))
    (for (k 1 limite)
      (setq h (+rat h (list 1L (bigint k))))
      (push (h 0) out -1))
    out))

(harmo-numer 20)
;-> (1L 3L 11L 25L 137L 49L 363L 761L 7129L 7381L 83711L 86021L 1145993L 1171733L
;->  1195757L 2436559L 42142223L 14274301L 275295799L 55835135L)

Sequenza OEIS A002805:
a(n) = denominator of harmonic number H(n) = Sum_{i=1..n} 1/i.
  1, 2, 6, 12, 60, 20, 140, 280, 2520, 2520, 27720, 27720, 360360, 360360,
  360360, 720720, 12252240, 4084080, 77597520, 15519504, 5173168, 5173168,
  118982864, 356948592, 8923714800, 8923714800, 80313433200, 80313433200,
  2329089562800, 2329089562800, 72201776446800, ...

(define (harmo-denom limite)
  (let ((h '(0L 1L)) (out '()))
    (for (k 1 limite)
      (setq h (+rat h (list 1L (bigint k))))
      (push (h 1) out -1))
    out))

(harmo-denom 20)
;-> (1L 2L 6L 12L 60L 20L 140L 280L 2520L 2520L 27720L 27720L 360360L 360360L
;->  360360L 720720L 12252240L 4084080L 77597520L 15519504L)

Adesso vediamo i numeri armonici alternati:

  HA(k) = Sum[i=1,k](-1)^(i+1)*(1/i) = 1 - 1/2 + 1/3 - ... +- 1/k

La serie dei numeri armonici è divergente.
La serie dei numeri armonici alternati è convergente:

  HA(k->inf) ~= ln(2) = 0.6931471805599453...

(define (harmo-alt limite)
  (let ((num 0) (h 0))
    (for (k 1 limite)
      (if (odd? k)
        (inc h (div k))
        (dec h (div k)))
        (if show (println k { } h)))
    h))

(harmo-alt 1e5)
;-> 0.6931421805849816
(harmo-alt 1e7)
;-> 0.6931471305601064
(time (println (harmo-alt 1e9)))
;-> 0.6931471800606472
;-> 84751.41

Sequenza OEIS A058313:
Numerator of the n-th alternating harmonic number, Sum_{k=1..n} (-1)^(k+1)/k.
  1, 1, 5, 7, 47, 37, 319, 533, 1879, 1627, 20417, 18107, 263111, 237371,
  52279, 95549, 1768477, 1632341, 33464927, 155685007, 166770367, 156188887,
  3825136961, 3602044091, 19081066231, 18051406831, 57128792093, 7751493599,
  236266661971, ...

(define (harmoalt-numer limite)
  (let ((h '(0L 1L)) (out '()))
    (for (k 1 limite)
      (if (odd? k)
        (setq h (+rat h (list 1L (bigint k))))
        (setq h (-rat h (list 1L (bigint k)))))
      (push (h 0) out -1))
    out))

(harmoalt-numer 20)
;-> (1L 1L 5L 7L 47L 37L 319L 533L 1879L 1627L 20417L 18107L 263111L 237371L
;->  52279L 95549L 1768477L 1632341L 33464927L 155685007L)

Sequenza OEIS A058312:
Denominator of the n-th alternating harmonic number, Sum_{k=1..n} (-1)^(k+1)/k.
  1, 2, 6, 12, 60, 60, 420, 840, 2520, 2520, 27720, 27720, 360360, 360360,
  72072, 144144, 2450448, 2450448, 46558512, 232792560, 232792560,
  232792560, 5354228880, 5354228880, 26771144400, 26771144400, 80313433200,
  11473347600, ...

(define (harmoalt-denom limite)
  (let ((h '(0L 1L)) (out '()))
    (for (k 1 limite)
      (if (odd? k)
        (setq h (+rat h (list 1L (bigint k))))
        (setq h (-rat h (list 1L (bigint k)))))
      (push (h 1) out -1))
    out))

(harmoalt-denom 20)
;-> (1L 2L 6L 12L 60L 60L 420L 840L 2520L 2520L 27720L 27720L 360360L 360360L
;->  72072L 144144L 2450448L 2450448L 46558512L 232792560L)


---------------------------
Numero armonico minore di N
---------------------------

La successione dei numeri armonici è costituita dalla somma dei reciproci dei primi k numeri naturali (escluso lo zero):

  H(k) = Sum[i=1,k]1/i = 1 + 1/2 + 1/3 + ... + 1/k

La successione è divergente.
Generare la successione di numeri a(n) = il più piccolo k tale che il k-esimo numero armonico sia maggiore di n.

Sequenza OEIS A002387:
Least k such that H(k) > n, where H(k) is the harmonic number Sum[i=1,k]1/i.
  1, 2, 4, 11, 31, 83, 227, 616, 1674, 4550, 12367, 33617, 91380, 248397,
  675214, 1835421, 4989191, 13562027, 36865412, 100210581, 272400600,
  740461601, 2012783315, 5471312310, 14872568831, 40427833596,
  109894245429, 298723530401, 812014744422, ...

(define (harmo limite show)
  (let ((num 0) (h 0) (out '()))
    (for (k 1 limite)
      (inc h (div k))
      (when (> h num)
        (push k out -1)
        (if show (println "num: " num { } "k: " k { } "h: " h))
        (++ num)))
    out))

Proviamo:

(harmo 1e7 true)
;-> num: 0 k: 1 h: 1
;-> num: 1 k: 2 h: 1.5
;-> num: 2 k: 4 h: 2.083333333333333
;-> num: 3 k: 11 h: 3.019877344877345
;-> num: 4 k: 31 h: 4.02724519543652
;-> num: 5 k: 83 h: 5.002068272680166
;-> num: 6 k: 227 h: 6.004366708345567
;-> num: 7 k: 616 h: 7.001274097134162
;-> num: 8 k: 1674 h: 8.000485571995782
;-> num: 9 k: 4550 h: 9.000208062931115
;-> num: 10 k: 12367 h: 10.00004300827578
;-> num: 11 k: 33617 h: 11.00001770863642
;-> num: 12 k: 91380 h: 12.00000305166562
;-> num: 13 k: 248397 h: 13.00000122948093
;-> num: 14 k: 675214 h: 14.00000136205325
;-> num: 15 k: 1835421 h: 15.00000037826723
;-> num: 16 k: 4989191 h: 16.00000009545252
;-> (1 2 4 11 31 83 227 616 1674 4550 12367 33617
;->  91380 248397 675214 1835421 4989191)

(time (println (harmo 1e9)))
;-> (1 2 4 11 31 83 227 616 1674 4550 12367 33617 91380 248397 675214
;->  1835421 4989191 13562027 36865412 100210581 272400600 740461601)
;-> 78863.064


------------------------------------------------------------
Equazione diofantea del tipo a1*x1 + a2*x2 + ... + an*xn = b
------------------------------------------------------------

Risoluzione di una equazione diofantea lineare

Consideriamo l'equazione:
  a1*x1 + a2*x2 + ... + an*xn = b
dove:
- a1, a2, ..., an sono interi
- b è un intero
- x1, x2, ..., xn devono essere interi

L'obiettivo è trovare tutte le soluzioni intere.
1) Condizione di esistenza
Calcoliamo: g = gcd(a1,a2,...,an)
L'equazione ha soluzione se e solo se: g divide b
cioè: b % g = 0
Se questa condizione non è verificata non esiste nessuna soluzione intera.

2) Identita' di Bezout
L'algoritmo di Euclide esteso permette di trovare:
  g = a*x + b*y
cioè una combinazione lineare dei due numeri.
Applicando Euclide esteso piu' volte otteniamo:
  g = a1*u1 + a2*u2 + ... + an*un
dove (u1,u2,...,un) sono i coefficienti di Bezout.

3) Costruzione di una soluzione particolare

Se:
  g divide b
poniamo:
  k = b / g

Moltiplicando tutta l'identita' di Bezout per k:
  b = a1*(u1*k) + a2*(u2*k) + ... + an*(un*k)
quindi:
  x1 = u1*k
  x2 = u2*k
  ...
  xn = un*k
è una soluzione particolare.

4) Tutte le altre soluzioni
Se abbiamo una soluzione particolare:
  X0 = (x1,x2,...,xn)
tutte le soluzioni sono:
  X = X0 + H
dove H soddisfa l'equazione omogenea:
  a1*x1 + a2*x2 + ... + an*xn = 0
Quindi bisogna aggiungere tutte le combinazioni intere dei vettori della soluzione omogenea.

5) Segno dei coefficienti
I coefficienti possono essere: positivi, negativi, zero.
Non cambia il metodo.
Si usa:
  g = gcd(abs(a1),abs(a2),...,abs(an))
ma i segni vengono mantenuti nei coefficienti di Bezout.

6) Coefficienti zero
Un coefficiente nullo significa che la relativa variabile non influenza l'equazione.
Esempio:
  3*x + 0*y + 5*z = 7
la variabile y è libera.

(define (egcd a b)
  (letn ((old-r a) (r b) (old-s 1) (s 0) (old-t 0) (t 1) (q 0) (tmp 0))
    ; Euclide esteso iterativo
    ; Manteniamo due identita':
    ; old-r = a*old-s + b*old-t
    ; r     = a*s     + b*t
    ; Ad ogni passo riduciamo il resto fino ad arrivare al gcd
    (println "egcd: a=" a " b=" b)
    (while (!= r 0)
      ; Quoziente della divisione euclidea
      (setq q (/ old-r r))
      (println "  q=" q " old-r=" old-r " r=" r)
      ; Aggiorna i resti:
      ; nuovo resto = vecchio resto - quoziente * resto corrente
      (setq tmp r)
      (setq r (- old-r (* q r)))
      (setq old-r tmp)
      ; Aggiorna i coefficienti del primo numero
      (setq tmp s)
      (setq s (- old-s (* q s)))
      (setq old-s tmp)
      ; Aggiorna i coefficienti del secondo numero
      (setq tmp t)
      (setq t (- old-t (* q t)))
      (setq old-t tmp))
    ; Alla fine old-r contiene il gcd
    ; e old-s, old-t sono i coefficienti di Bezout
    ; Se il gcd è negativo cambiamo tutti i segni
    (if (< old-r 0)
        (begin
          (setq old-r (- old-r))
          (setq old-s (- old-s))
          (setq old-t (- old-t))))
    (println "  risultato: "
             (list old-r old-s old-t))
    (list old-r old-s old-t)))

(define (diofantea coeff b)
  (letn ((n (length coeff))
         (g 0)
         (bez '())
         (oldg 0)
         (res '())
         (scale 0)
         (cleans '()))
    ; Stampa i dati iniziali del problema
    (println "Equazione diofantea:")
    (println coeff " = " b)
    ; Rimuove i coefficienti nulli perchè non contribuiscono alla somma
    ; e aggiunge un coefficiente Bezout iniziale nullo associato
    (dolist (a coeff)
      (if (!= a 0)
          (begin
            (push a cleans -1)
            (push 0 bez -1))))
    ; Sostituisce la lista originale con quella senza zeri
    (setq coeff cleans)
    ; Se tutti i coefficienti erano zero bisogna trattare il caso separato
    (if (= (length coeff) 0)
        (if (= b 0)
            (begin
              (println "Tutti i coefficienti sono zero")
              (println "Infinite soluzioni")
              '())
            (begin
              (println "Tutti i coefficienti sono zero ma b non è zero")
              nil))
        (begin
          ; Il primo coefficiente inizializza la combinazione di Bezout
          ; iniziale: g = coeff[0]*1
          (setq g (coeff 0))
          (setq bez '(1))
          (println "GCD iniziale = " g)
          ; Combina progressivamente il gcd corrente con il coefficiente successivo
          ; Se abbiamo:
          ; g = a*u1 + b*u2
          ; e calcoliamo:
          ; g2 = gcd(g,c)
          ; otteniamo i nuovi coefficienti di Bezout
          (for (i 1 (- (length coeff) 1))
            (setq oldg g)
            (setq res (egcd g (coeff i)))
            (setq g (res 0))
            (println "Combino " oldg " con " (coeff i))
            (println "Nuovo gcd = " g)
            ; Moltiplica tutti i vecchi coefficienti Bezout
            ; per il coefficiente restituito da Euclide esteso
            (setq bez (map (fn (x) (* x (res 1))) bez))
            ; Aggiunge il nuovo coefficiente Bezout
            (push (res 2) bez -1)
            (println "Coefficienti Bezout = " bez))
          ; Se b non è divisibile per gcd non esistono soluzioni intere
          (if (!= (% b g) 0)
              (begin
                (println "Nessuna soluzione")
                (println b " non è divisibile per " g)
                nil)
              (begin
                ; Moltiplicando i coefficienti Bezout per b/g
                ; otteniamo una soluzione particolare
                (setq scale (/ b g))
                (println "Fattore di scala = " scale)
                (setq bez (map (fn (x) (* x scale)) bez))
                (println "Soluzione particolare:" bez)
                bez))))))

Esempio:
(diofantea '(-6 15 9) 12)
;-> (8 4 0)
che soddisfa -6*x + 15*y + 9*z = 12.

Adesso scriviamo la funzione che genera tutte le soluzioni in forma parametrica.
Per parametrizzare tutte le soluzioni serve anche la lista dei coefficienti 'a1..an', perché la sola soluzione particolare non contiene l'informazione sull'equazione omogenea.

Partiamo da A * X0 = b, dove X0 è la soluzione particolare trovata dalla funzione 'diofantea'.
Le altre soluzioni sono:

  X = X0 + t1V1 + t2V2 + ... + t(n-1)V(n-1)

dove i vettori 'Vi' generano: a1x1 + ... + anxn=0
Per una sola equazione possiamo costruire una base scegliendo le prime 'n-1' variabili come parametri.

(define (param-diofantea coeff sol)
  (letn ((n (length coeff))
         (base '())
         (v '())
         (g 0))
    ; costruisce n-1 vettori della soluzione omogenea
    ; ogni vettore lascia libera una variabile e calcola l'ultima
    (setq g (coeff (- n 1)))
    (for (i 0 (- n 2))
      (setq v (dup 0 n))
      ; parametro i-esimo = 1
      (setf (v i) 1)
      ; calcolo dell'ultima componente:
      ; a1*x1+...+an*xn=0
      ; quindi xn=-(somma precedente)/an
      (set 's 0)
      (for (j 0 (- n 2))
        (setq s (+ s (* (coeff j) (v j)))))
      (setf (v (- n 1)) (/ (- s) g))
      (push v base -1))
    (println "Soluzione particolare:")
    (println sol)
    (println "Base soluzione omogenea:")
    (dolist (b base)
      (println b))
    (list sol base)))

Esempio:
(setq s (diofantea '(1 1 1 1 1) 5))
;-> (0 0 0 0 5)

(param-diofantea '(1 1 1 1 1) s)
;-> ...
;-> Base soluzione omogenea:
;-> (1 0 0 0 -1)
;-> (0 1 0 0 -1)
;-> (0 0 1 0 -1)
;-> (0 0 0 1 -1)
;-> ((0 0 0 0 5) ((1 0 0 0 -1) (0 1 0 0 -1) (0 0 1 0 -1) (0 0 0 1 -1)))

Quindi la soluzione generale è:
  (0 0 0 0 5)
  +t1*(1 0 0 0 -1)
  +t2*(0 1 0 0 -1)
  +t3*(0 0 1 0 -1)
  +t4*(0 0 0 1 -1)
  x1=t1,  x2=t2,  x3=t3,  x4=t4,  x5=5-t1-t2-t3-t4
che è esattamente: x1 + x2 + x3 + x4 + x5 = 5 con tutti i parametri interi.

Vedi anche "Equazione diofantea lineare" su "Note libere 5".


-----------------------
Contare senza una cifra
-----------------------

Data una cifra 'x' (0..9) scrivere una funzione che genera la sequenza dei numeri naturali che non sono multipli di 'x' e non contengono 'x' come cifra.
Esempio
  x = 3
  Numeri: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ...
  Dobbiamo eliminare i multipli di 3 e i numeri che contengono 3.
  Sequenza: 1 2 4 5 7 8 10 11 14 16 17 19 20 22 25 ...

(define (conta x limite)
  (let ((out '()) (xs (string x)))
    (for (num 1 limite)
      (if-not (or (zero? (% num 3)) (find xs (string num)))
        (push num out -1)))
    out))

Proviamo:
(conta 3 25)
;-> (1 2 4 5 7 8 10 11 14 16 17 19 20 22 25)

(map conta (sequence 0 9) (dup 25 10))
;-> ((1 2 4 5 7 8 11 13 14 16 17 19 22 23 25)
;->  (2 4 5 7 8 20 22 23 25)
;->  (1 4 5 7 8 10 11 13 14 16 17 19)
;->  (1 2 4 5 7 8 10 11 14 16 17 19 20 22 25)
;->  (1 2 5 7 8 10 11 13 16 17 19 20 22 23 25)
;->  (1 2 4 7 8 10 11 13 14 16 17 19 20 22 23)
;->  (1 2 4 5 7 8 10 11 13 14 17 19 20 22 23 25)
;->  (1 2 4 5 8 10 11 13 14 16 19 20 22 23 25)
;->  (1 2 4 5 7 10 11 13 14 16 17 19 20 22 23 25)
;->  (1 2 4 5 7 8 10 11 13 14 16 17 20 22 23 25))


-------------------------------------------
Problema della moneta (Numeri di Frobenius)
-------------------------------------------

Siano n>=2 interi 0 < a1 < ... < an con MCD(a1,a2,...,an)=1.
I valori a(i) rappresentano i tagli di n monete diverse, dove questi tagli hanno il massimo comune divisore pari a 1.
Le somme di denaro che possono essere rappresentate utilizzando le monete date sono quindi definite da:

  N = Sum[i=1,N]a(i)*x(i),

dove x(i() sono interi non negativi che indicano il numero di ciascuna moneta utilizzata.
Se a1 = 1, è ovviamente possibile rappresentare qualsiasi quantità di denaro N.
Tuttavia, nel caso generale, solo alcune quantità N possono essere rappresentate.
Ad esempio, se le monete consentite sono (2, 5, 10), è impossibile rappresentare N=1 e 3, sebbene tutte le altre quantità possano essere rappresentate.
Determinare la funzione g(a1,a2,...,an) che fornisce il più grande N=g(a1,a2,...,an) per cui non esiste soluzione è detto problema della moneta, o talvolta problema del resto. Il più grande N di questo tipo per un dato problema è detto numero di Frobenius g(a1,a2,...).

Solo il caso N = 2 ha una formula chiusa per determinare il numero di Frobenius:

 g(a1,a2) = (a1 - 1)*(a2 - 1) - 1 = a1*a2 - (a1 + a2)

Il numero totale di tali importi non rappresentabili è dato da:

  (1/2)*(N+1) = (1/2)*(a1 - 1)*(a2 - 1)

Vediamo tutti i casi con (1 <= a1,a2 <= limite).

(define (print-matrix-int matrix)
"Print a matrix of integer"
  (let ( (row (length matrix))
         (col (length (first matrix)))
         (len-cols '()) (fmt "") )
    ; calcola la larghezza di ogni colonna
    ; (in base all'intero più grande/lungo di ogni colonna)
    (setq len-cols (map (fn(col) (apply max (map length (flat col))))
                           (transpose matrix)))
    ; ciclo per ogni numero intero della matrice...
    (for (i 0 (- row 1))
      (for (j 0 (- col 1))
        ; formattazione dell'intero corrente --> (larghezza-colonna + 2)
        ; (uno spazio ed un eventuale segno -)
        (setq fmt (string "%" (+ (len-cols j) 2) "d"))
        ; stampa dell'intero corrente
        (print (format fmt (matrix i j))))
      (println))))

(define (frob2 limite)
  (setq g (array (+ limite 1) (+ limite 1) '(0)))
  (for (a1 1 limite)
    (for (a2 1 limite)
      (cond ((or (= a1 1) (= a2 1)) ; caso a1==1 o a2==1
              (setf (g a1 a2) 999)
              (setf (g a2 a1) 999))
            ((!= (gcd a1 a2) 1)     ; caso gcd != 1
              (setf (g a1 a2) 0)) 
            (true                   ; caso generale
              (setq (g a1 a2) (- (* a1 a2) (+ a1 a2)))))))
   (print-matrix-int (array-list g)))

(frob2 10)
;->  0    0    0    0    0    0    0    0    0    0    0
;->  0  999  999  999  999  999  999  999  999  999  999
;->  0  999    0    1    0    3    0    5    0    7    0
;->  0  999    1    0    5    7    0   11   13    0   17
;->  0  999    0    5    0   11    0   17    0   23    0
;->  0  999    3    7   11    0   19   23   27   31    0
;->  0  999    0    0    0   19    0   29    0    0    0
;->  0  999    5   11   17   23   29    0   41   47   53
;->  0  999    0   13    0   27    0   41    0   55    0
;->  0  999    7    0   23   31    0   47   55    0   71
;->  0  999    0   17    0    0    0   53    0   71    0

Per N > 2 vedi " Computing the Continuous Discretely" di Beck e Robins, 2006:
https://matthbeck.github.io/ccd.html


-------------------------------------
Moltiplicazione inversa nelle matrici
-------------------------------------

Sia A * B = C, dove:
  A è una matrice (m*n)
  B è una matrice (n*p)
  C è una matrice (m*p)

Caso 1: dati A e C, trovare B
-----------------------------
Se A è quadrata (n==m) e invertibile:

  A(-1)*A*B = A(-1)*C  -->  B = A(-1)*C

Se (A) non è invertibile oppure non è quadrata, la soluzione:
- può non esistere;
- può non essere unica;
- può essere ottenuta con la pseudoinversa di Moore-Penrose:

Caso 2: dati B e C, trovare A
-----------------------------
Se B è quadrata (n==p) e invertibile:

  A*B*B(-1) = C*B(-1)  -->  A = C*B(-1)

Caso generale
-------------
Se nessuna delle due matrici è invertibile, il problema diventa un sistema lineare.
Per esempio, se A è nota, allora gli elementi di B sono le incognite e le equazioni derivano da: A*B=C.
Si ottiene un sistema lineare con (n * p) incognite.

Condizione di unicità (invertibilità di A e B)
----------------------------------------------
Per ricavare B occorre che A abbia rango massimo sulle colonne.
Nel caso di una matrice quadrata: det(A) != 0
Per ricavare A occorre che B abbia rango massimo sulle righe.
Nel caso di una matrice quadrata: det(B) != 0
Negli altri casi si deve risolvere un sistema lineare (eventualmente usando una pseudoinversa), e la soluzione può essere nulla, unica o infinita.

Proviamo:

(setq A '((1 2 3) (4 5 6) (7 8 8)))
(det A)
;-> 3
(setq B '((1 4 5) (4 2 3) (8 2 3)))
(det B)
;-> 8
(setq C (multiply A B))
;-> ((33 14 20) (72 38 53) (103 60 83))
(multiply (invert A) C)
;-> ((0.9999999999999858 3.999999999999986 4.999999999999986)
;->  (4.000000000000028  2.000000000000028 3.000000000000028)
;->  (7.999999999999986  1.999999999999986 2.999999999999986))

(multiply C (invert B))
;-> ((1.000000000000004 1.999999999999986 3.000000000000007)
;->  (4 5 6)
;->  (7 8 8))


--------------------------------------------
Determinante esatto di una matrice di interi
--------------------------------------------

Algoritmo di Bareiss (Fraction-Free Gaussian Elimination)
---------------------------------------------------------
L'algoritmo di Bareiss permette di calcolare il determinante di una matrice di interi usando soltanto operazioni intere.
L'idea è simile all'eliminazione di Gauss, ma evita la comparsa di frazioni durante i calcoli.
Data una matrice quadrata:
  A = [a(i,j)]
al passo k (k = 0, 1, ..., n-2) si aggiorna ogni elemento del sottomatrice in basso a destra mediante:
  a(i,j) = (a(k,k)*a(i,j) - a(i,k)*a(k,j))/d
dove:
  d = 1 al primo passo
  d = pivot del passo precedente negli altri casi
ovvero:
  d = a(k-1,k-1)

La proprietà fondamentale è che la divisione è sempre esatta quando la matrice iniziale contiene interi.

Matrice:
  | 2  3  1 |
  | 4  1  5 |
  | 6  2  3 |
Passo k = 0
  pivot = 2
  d = 1
Calcolo:
  a(1,1) = (2*1 - 4*3)/1 = -10
  a(1,2) = (2*5 - 4*1)/1 = 6
  a(2,1) = (2*2 - 6*3)/1 = -14
  a(2,2) = (2*3 - 6*1)/1 = 0
Matrice:
  | 2   3    1 |
  | 4 -10    6 |
  | 6 -14    0 |
Passo k = 1
  pivot = -10
  d = 2
a(2,2) = (-10*0 - (-14)*6)/2 = 42
Matrice finale:
  | 2   3    1 |
  | 4 -10    6 |
  | 6 -14   42 |
Determinante = 42

La complessità è come l'eliminazione di Gauss: O(n^3)

Questo algoritmo ha i seguenti vantaggi:
- nessuna perdita di precisione
- molto efficiente con bigint
- evita la crescita di numeratori e denominatori tipica dell'aritmetica razionale

Pivoting
--------
L'algoritmo richiede che tutti i pivot siano diversi da zero.
Quindi occorre aggiungere il pivoting: se il pivot è zero si scambia la riga corrente con una riga sottostante avente pivot non nullo e si cambia il segno del determinante.
Questo permette di trattare qualsiasi matrice non singolare.

La seguente funzione:
- usa l'algoritmo di Bareiss;
- scambia righe quando il pivot vale 0;
- tiene conto dei cambi di segno del determinante;
- lavora con interi/bigint;
- restituisce il determinante come big-integer
- restituisce 0 se la matrice è singolare.

; Funzione che calcola il determinante di una matrice di interi
; con il metodo di Bareiss (usa solo operazioni intere)
(define (det-bareiss matrix)
  (local (n m d pivot segno riga i j k tmp trovato)
    (setq n (length matrix))
    ; converte i valori della matrice in big integer
    (setq m (map (fn (row) (map bigint row)) matrix))
    (cond
      ((= n 0) 1L)
      ((= n 1) (bigint ((m 0) 0)))
      (true
        (setq d 1L)
        (setq segno 1L)
        (for (k 0 (- n 2))
          ; cerca un pivot non nullo
          (setq trovato nil)
          (setq riga k)
          (while (and (< riga n) (not trovato))
            (if (!= ((m riga) k) 0)
                (setq trovato true)
                (++ riga)))
          ; matrice singolare
          (if (not trovato)
              (begin
                (setq segno 0L)
                (setq k n)))
          ; eventuale scambio di righe
          (if (and (!= segno 0L) (!= riga k))
              (begin
                (setq tmp (m k))
                (setf (m k) (m riga))
                (setf (m riga) tmp)
                (setq segno (- segno))))
          (if (!= segno 0L)
              (begin
                (setq pivot ((m k) k))
                (for (i (+ k 1) (- n 1))
                  (for (j (+ k 1) (- n 1))
                    (setf ((m i) j)
                          (/ (- (* pivot ((m i) j))
                                (* ((m i) k) ((m k) j)))
                             d))))
                (setq d pivot))))
        (if (= segno 0L)
            0L
            (* segno ((m (- n 1)) (- n 1))))))))

Proviamo:

(setq K '((0 0 1) (0 2 3) (1 4 5)))
(det K)
;-> -2
(det-bareiss K)
;-> -2L

(setq Q '((1 2 3) (4 5 6) (7 8 8)))
(det Q)
(det-bareiss Q)
;-> 3L

(setq T (explode (map bigint (rand 4 100)) 10))
(det T)
;-> 6692.999999999996
(det-bareiss T)
;-> 6693L


---------------------------------------------------
Primi della sommatoria dei numeri naturali composti
---------------------------------------------------

Determinare la sequenza dei numeri primi che si ottengono sommando ripetutamente i numeri naturali composti.

Esempi:
1 + 4 = 5 --> numero primo
1 + 4 + 6 = 11 --> numero primo
1 + 4 + 6 + 8 = 19 --> numero primo
1 + 4 + 6 + 8 + 9 = 28 --> numero composto
Sequenza di primi (fino a k=9): 5 11 19

Sequenza OEIS A128927:
Primes which are the sum of the first k nonprimes for some k >= 2.
  5, 11, 19, 79, 113, 251, 401, 709, 947, 1579, 1847, 2063, 5077, 7603,
  9049, 10457, 11621, 17509, 18353, 19433, 21911, 22853, 24551, 27073,
  30809, 33923, 34213, 37781, 39313, 39623, 45757, 46091, 47779, 51241,
  53381, 56299, 58537, 67927, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (seq k)
  (let ((out '()) (somma 0))
    (for (i 1 k)
      (unless (prime? i)
        (++ somma i)
        (if (prime? somma) (push somma out -1))))
    out))

(seq 500)
;-> (5 11 19 79 113 251 401 709 947 1579 1847 2063 5077 7603
;->  9049 10457 11621 17509 18353 19433 21911 22853 24551 27073
;->  30809 33923 34213 37781 39313 39623 45757 46091 47779 51241
;->  53381 56299 58537 67927 70381 80621 96827 99259 101723)


---------------------------------------
Il gioco dell'inversione (Reverse game)
---------------------------------------

Abbiamo una matrice NxN riempita con 0 e 1.
L'obiettivo è rendere ogni cella uguale (tutti 0 o tutti 1).
Possiamo eseguire le seguenti operazioni sulla matrice:
1) Selezionare una cella.
2) Tutte le celle nella stessa riga e nella stessa colonna (tranne la cella stessa) vengono trasformate nel loro opposto: da 0 a 1 e da 1 a 0.
Il gioco termina quando i valori di tutte le celle della matrice sono uguali (tutti 0 o tutti 1).

(define (print-grid grid ch0 ch1 coord)
"Print a matrix with only digits (0..9)"
  (local (row col)
    (setq row (length grid))
    (setq col (length (first grid)))
    ; indici di colonna della griglia
    (if coord
        (println "  " (join (map (fn(x) (format "%2d" x)) (sequence 0 (- col 1))))))
    (for (i 0 (- row 1))
      ; indice di riga della griglia
      (if coord (print (format "%2d" i)))
      ; stampa della griglia
      (for (j 0 (- col 1))
        (if (and (!= ch0 "") (!= ch1 ""))
            (begin
              (cond ((= (grid i j) 0) (print ch0))
                    ((= (grid i j) 1) (print ch1))
                    (true
                      (print (format "%2d" (grid i j))))))
            ;else
            (print (format "%2d" (grid i j)))))
      (println))))

; Funzione che inverte 0 in 1 e 1 in 0
(define (flip x) (- 1 x))

; Funzione che inverte i valori di una riga e di una colonna
(define (flip-row-col row col)
  ; flip dei valori della colonna
  (for (r 0 (- N 1))
    (setf (matrix r col) (flip (matrix r col))))
  ; flip dei valori della riga
  (for (c 0 (- N 1))
    (setf (matrix row c) (flip (matrix row c)))))

; Funzione di input per numeri in un intervallo
(define (input-number str min-num max-num)
  (print str)
  (let (input (int (read-line)))
    (while (or (< input min-num) (> input max-num))
      (print str)
      (setq input (int (read-line))))
    input))

; Funzione che verifica se una matrice è formata da tutti 0 o da tutti 1
(define (end-game? matrix)
  (or (for-all (fn(x) (= x 0)) (flat matrix))
      (for-all (fn(x) (= x 1)) (flat matrix))))

; Funzione che gestisce il gioco (interattivo)
(define (start-game N level show-sol)
  (local (sol mosse matrix r c)
    ; Lista delle mosse di soluzione del gioco
    (setq sol '())
    ; Mosse effettuate
    (setq mosse 0)
    ; creazione matrice casuale
    (setq matrix (explode (dup 0 (* N N)) N))
    ; Partiamo da una matrice con tutti i valori a 0 e
    ; applichiamo 'level' volte la funzione 'flip-row-col' con celle casuali.
    ; In questo modo la matrice risultante è sicuramente risolvibile e
    ; creiamo anche una lista di mosse che risolvono la matrice.
    (for (i 1 level)
      (setq r (rand N))
      (setq c (rand N))
      (push (list r c) sol)
      (flip-row-col r c))
    (println "--- REVERSE GAME ---")
    (println "Scacchiera: Matrice casuale con 0 e 1")
    (println "Obbiettivo: ottenere una matrice con tutti 0 o tutti 1")
    (println "Azione: Selezionando una cella (riga e colonna) tutte le celle ")
    (println "        nella stessa riga e nella stessa colonna (tranne la cella stessa)")
    (println "        vengono trasformate nel loro opposto: da 0 a 1 e da 1 a 0.")
    ; Mostra le mosse che risolvono il gioco
    (if show-sol (println "Soluzione: " sol))
    ; Ciclo del gioco
    ; Finchè la matrice corrente non contiene tutti 0 o tutti 1...
    (until (end-game? matrix)
      ; Stampa situazione corrente
      (println "\nMosse: " mosse)
      (print-grid matrix "" "" true)
      ; Input utente (riga e colonna)
      (setq r (input-number "Riga: " 0 (- N 1)))
      (setq c (input-number "Colonna: " 0 (- N 1)))
      (++ mosse)
      ; Applica la mossa corrente
      (flip-row-col r c))
    ; Matrice risolta
    (println "\nMosse: " mosse)
    (print-grid matrix "" "" true)
    (println "\nBRAVO!!!") '>))

Proviamo:

(start-game 3 3 true)
;-> --- REVERSE GAME ---
;-> Scacchiera: Matrice casuale con 0 e 1
;-> Obbiettivo: ottenere una matrice con tutti 0 o tutti 1
;-> Azione: Selezionando una cella (riga e colonna) tutte le celle
;->         nella stessa riga e nella stessa colonna (tranne la cella stessa)
;->         vengono trasformate nel loro opposto: da 0 a 1 e da 1 a 0.
;-> Soluzione: ((2 0) (1 2) (2 1))
;-> 
;-> Mosse: 0          Mosse: 1          Mosse: 2          Mosse: 3
;->    0 1 2             0 1 2             0 1 2             0 1 2
;->  0 1 1 1           0 0 1 1           0 0 1 0           0 0 0 0
;->  1 0 0 0           1 1 0 0           1 0 1 0           1 0 0 0
;->  2 1 1 1           2 1 0 0           2 1 0 1           2 0 0 0
;-> Riga: 2           Riga: 1           Riga: 2
;-> Colonna: 0        Colonna: 2        Colonna: 1
;-> 
;-> BRAVO!!!

Se si sceglie la stessa cella due volte di seguito, la matrice non cambia.

Adesso scriviamo un programma che risolve il problema utilizzando un algoritmo BFS (Best-First-Search).
Il problema consiste nel trovare il numero minimo di mosse per trasformare una matrice binaria in una matrice con tutti gli elementi uguali.
Una mossa consiste nello scegliere una cella (r,c) e invertire:
- tutte le celle della riga r tranne (r,c)
- tutte le celle della colonna c tranne (r,c)
Ogni cella della matrice puo' essere vista come uno stato del problema.
Uno stato è semplicemente una configurazione della matrice.
L'algoritmo BFS (Breadth First Search) esplora gli stati a livelli:
livello 0:
- contiene solo la matrice iniziale
livello 1:
- contiene tutte le matrici ottenute con una sola mossa
livello 2:
- contiene tutte le matrici ottenute con due mosse
e cosi' via.
Quando BFS trova per la prima volta una matrice composta solo da 0 oppure solo da 1, il numero di mosse usate è sicuramente minimo.

Per evitare di visitare piu' volte lo stesso stato, ogni matrice viene trasformata in una stringa.

Esempio:
matrice:
  1 0 1
  1 0 1
  1 0 1
diventa: "101101101"
Questa stringa viene memorizzata negli stati gia' visitati.

Per ogni stato:
1. estraiamo la matrice dalla coda BFS
2. controlliamo se è una soluzione
3. proviamo tutte le n*n mosse possibili
4. per ogni nuovo stato non visitato lo inseriamo in coda

La BFS termina quando:
- trova una soluzione
- oppure esaurisce tutti gli stati possibili
Il numero massimo di stati possibili per una matrice N*N vale: 2^(N*N)
quindi BFS è adatta per matrici piccole.
Per esempio:
N=3 -> 512 stati
N=4 -> 65536 stati
N=5 -> 33554432 stati

; Controlla se una matrice è composta solo da 0
; oppure solo da 1.
; In questo caso abbiamo raggiunto uno stato finale.
(define (goal? matrix)
  (apply = (flat matrix)))

; Applica una mossa alla matrice.
; La cella (r,c) scelta non viene modificata.
; Vengono invertite tutte le altre celle della stessa
; riga e della stessa colonna.
(define (move matrix r c)
  (letn ((n (length matrix))
         (out matrix))
    (for (i 0 (- n 1))
      (if (!= i r)
          (setf (out i c) (^ (out i c) 1)))
      (if (!= i c)
          (setf (out r i) (^ (out r i) 1))))
    out))

; Trasforma una matrice in una stringa.
; Serve per memorizzare gli stati gia' visitati.
; Esempio:
; ((1 0)(0 1)) -> "1001"
(define (encode matrix)
  (join (map string (flat matrix)) ""))

; Ricerca BFS della soluzione minima.
; Restituisce la lista delle coordinate cliccate.
; Se non esiste soluzione restituisce nil.
(define (bfs matrix)
  (letn ((n (length matrix))
         (queue (list (list matrix '())))
         (visited '())
         (state nil)
         (cur nil)
         (path nil)
         (next nil)
         (key nil))
    ; Inserisco lo stato iniziale tra quelli gia' visti.
    (push (encode matrix) visited)
    ; Finche' esistono stati da analizzare.
    (while queue
      ; Prendo il primo stato della coda.
      (setq state (pop queue))
      (setq cur (state 0))
      (setq path (state 1))
      ; Debug: mostra livello e stato corrente.
      (println "livello=" (length path)
               " coda=" (length queue)
               " stato=" (encode cur))
      ; Se lo stato corrente è finale,
      ; abbiamo trovato il minimo.
      (if (goal? cur)
          (begin
            (println "SOLUZIONE")
            (println "mosse=" (length path))
            (throw path)))
      ; Provo tutte le possibili celle da cliccare.
      (for (r 0 (- n 1))
        (for (c 0 (- n 1))
          ; Creo il nuovo stato.
          (setq next (move cur r c))
          ; Se è gia' soluzione termino subito.
          (if (goal? next)
              (begin
                (println "SOLUZIONE")
                (println "mosse=" (+ (length path) 1))
                (throw (append path (list (list r c))))))
          ; Codifico il nuovo stato.
          (setq key (encode next))
          ; Se non è stato visto, lo accodo.
          (if (not (ref key visited))
              (begin
                (push key visited)
                (push (list next
                            (append path (list (list r c))))
                      queue -1))))))
    ; Nessuna soluzione trovata.
    nil))

Proviamo:

(setq m '((1 0 1)
          (1 0 1)
          (1 0 1)))
(catch (bfs m))
;-> livello=0 coda=0 stato=101101101
;-> livello=1 coda=8 stato=110001001
;-> SOLUZIONE
;-> mosse=2
;-> ((0 0) (0 2))

(setq m '((1 0) (0 1)))
(catch (bfs m))
;-> livello=0 coda=0 stato=1001
;-> SOLUZIONE
;-> mosse=1
;-> ((0 0))

(setq m '((1 0) (1 0)))
(catch (bfs m))
;-> livello=0 coda=0 stato=1010
;-> livello=1 coda=1 stato=1100
;-> livello=1 coda=1 stato=0011
;-> livello=2 coda=0 stato=0101
;-> nil

Le soluzioni ottenute dal gioco interattivo non necessariamente saranno uguali a quelle trovate dalla BFS, perché possono esistere più soluzioni con lo stesso numero minimo di mosse.


--------------------------------
Numeri con parentesi ben formate
--------------------------------

Dato un numero intero positivo, trasformarlo in binario, sostituire '1' con '(' e '0' con ')'.
L'espressione di parentesi che otteniamo è ben formata (cioè le parentesi sono accoppiate correttamente)?

Esempi:
  N = 9
  Binario = 1001
  Espressione = ())( --> non è ben formata

  N = 44
  Binario = 101100
  Espressione = ()(())  --> ben formata

Sequenza OEIS A014486:
List of totally balanced sequences of 2n binary digits written in base 10. Binary expansion of each term contains n 0's and n 1's and reading from left to right (the most significant to the least significant bit), the number of 0's never exceeds the number of 1's.
  0, 2, 10, 12, 42, 44, 50, 52, 56, 170, 172, 178, 180, 184, 202, 204, 210,
  212, 216, 226, 228, 232, 240, 682, 684, 690, 692, 696, 714, 716, 722, 724,
  728, 738, 740, 744, 752, 810, 812, 818, 820, 824, 842, 844, 850, 852, 856,
  866, 868, 872, 880, 906, 908, 914, ...

Sequenza OEIS A063171:
Dyck language interpreted as binary numbers in ascending order.
  0, 10, 1010, 1100, 101010, 101100, 110010, 110100, 111000, 10101010,
  10101100, 10110010, 10110100, 10111000, 11001010, 11001100, 11010010,
  11010100, 11011000, 11100010, 11100100, 11101000, 11110000, 1010101010,
  1010101100, 1010110010, 1010110100, 1010111000, ...

(define (well-formed? num)
  (if (zero? num) 0
  ;else
    (let ((bin (bits  num)) (opened 0) (stop nil))
      (dolist (el (explode bin) stop)
        (if (= el "0")
            (-- opened)
            (++ opened))
        (if (< opened 0) (setq stop true)))
      (and (not stop) (zero? opened)))))

(filter well-formed? (sequence 0 914))
;-> (0 2 10 12 42 44 50 52 56 170 172 178 180 184 202 204 210
;->  212 216 226 228 232 240 682 684 690 692 696 714 716 722 724
;->  728 738 740 744 752 810 812 818 820 824 842 844 850 852 856
;->  866 868 872 880 906 908 914)

(map sym (map bits (filter well-formed? (sequence 0 696))))
;-> (0 10 1010 1100 101010 101100 110010 110100 111000 10101010
;->  10101100 10110010 10110100 10111000 11001010 11001100 11010010
;->  11010100 11011000 11100010 11100100 11101000 11110000 1010101010
;->  1010101100 1010110010 1010110100 1010111000)


-----------------------
Biquadratico più vicino
-----------------------

Dato un numero intero positivo determinare il numero biquadratico più vicino.
Un numero biquadratico è un numero che è la quarta potenza di un altro numero intero, ad esempio: 3^4 = 3*3*3*3 = 81
I primi numeri biquadratici:  1, 16, 81, 256, 625, 1296, 2401, 4096, ...
Esempio:
  N = 10 --> il numero biquadratico più vicino è 16
  N = 63 --> il numero biquadratico più vicino è 81

Sequenza OEIS A000583:
Fourth powers: a(n) = n^4.
  0, 1, 16, 81, 256, 625, 1296, 2401, 4096, 6561, 10000, 14641, 20736, 28561,
  38416, 50625, 65536, 83521, 104976, 130321, 160000, 194481, 234256, 279841,
  331776, 390625, 456976, 531441, 614656, 707281, 810000, 923521, 1048576,
  1185921, 1336336, 1500625, 1679616, 1874161, ...

È interessante notare che non si verificherà mai un pareggio, poiché la sequenza alterna numeri pari e dispari.

Metodo 1 (brute-force)
----------------------

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (biquad1 N)
  (let ((sol nil) (k 1) (k 1) (found nil) (cur-pow 1) (prev-pow 1))
    (until found
      ; potenza corrente
      (setq cur-pow (** k 4))
      (cond ((= cur-pow N) ; N == potenza corrente
              (setq found true)
              (setq sol cur-pow))
            ((> cur-pow N) ; N > potenza corrente
              ; adesso la soluzione è la potenza corrente oppure
              ; la potenza precedente
              (setq found true)
              (if (> (- cur-pow N) (- N prev-pow))
                  (setq sol prev-pow)
                  (setq sol cur-pow)))
            (true (setq prev-pow cur-pow))) ; N < potenza corrente
      ;(print k { } prev-pow { } cur-pow) (read-line)
      (++ k))
    sol))

Proviamo:

(biquad1 10)
;-> 16L
(biquad1 63)
;-> 81L
(biquad1 120)
;-> 81L
(biquad1 625)
;-> 625L
(biquad1 123456789)
;-> 121550625L

Metodo 2 (matematica)
---------------------

Il valore N per cui l'uscita passa da (k − 1)^4 a k^4 soddisfa sqrt(sqrt(N) − 3/4) + 1/2 = k, ovvero N = ((k − 1/2)^2 + 3/4)^2 = (k^2 − k + 1)^2 = ((k − 1)^4 + k^4 + 1)/2, che è esattamente il primo intero più vicino a k^4.
Quindi dato N il numero quadratico più vicino vale:

  (int (sqrt(sqrt(N) − 3/4) + 1/2))^4

(define (f x) (/ (+ (** (- x 1) 4) (** x 4) 1) 2))

(define (biquad2 N) (** (int (add (sqrt (sub (sqrt N) 0.75)) 0.5)) 4))

Proviamo:

(biquad2 10)
;-> 16L
(biquad2 63)
;-> 81L
(biquad2 120)
;-> 81L
(biquad2 625)
;-> 625L
(biquad2 123456789)
;-> 121550625L

Test di correttezza:
(= (map biquad1 (sequence 1 1e4)) (map biquad2 (sequence 1 1e4)))
;-> true

Test di velocità:
(time (map biquad1 (sequence 1 1e5)))
;-> 1740.115
(time (map biquad2 (sequence 1 1e5)))
;-> 115.075


-----------------
Numero più vicino
-----------------

Data un intero N e una lista di interi trovare l'intero più vicino (o uguale) a N.
Esempio:
N = 11
Lista = (8 3 15 22)
Il numero della lista più vicino a 11 vale 8.

Prima di scrivere una funzione dobbiamo analizzare quale 'lavoro' deve svolgere.
Dobbiamo fare una sola ricerca su una lista?
Dobbiamo fare molte ricerche sulla stessa lista?
Dobbiamo fare poche ricerche su liste sempre diverse?
ecc.
Le risposte a queste domande ci aiutano a definire una funzione ottimizzata per quello che deve realmente fare.

Metodo 1
--------
Attraversiamo tutta la lista una volta mantenendo il valore più vicino trovato.
Complessità: O(n)

(define (find-close1 N lst)
  (let ((out (lst 0)) (diff (abs (- N (lst 0)))))
    (dolist (el lst)
      (if (< (abs (- N el)) diff)
          (set 'out el 'diff (abs (- N el)))))
    out))

(find-close1 3 '(3 5 25 34 56 678))
;-> 3
(find-close1 -2 '(3 5 25 34 56 678))
;-> 3
(find-close1 678 '(3 5 25 34 56 678))
;-> 678
(find-close1 1000 '(3 5 25 34 56 678))
;-> 678
(find-close1 4 '(3 5 25 34 56 678))
;-> 3
(find-close1 0 '(-25 -20 -10 -3 5 25 34 56 678))
;-> -3
(find-close1 -15 '(-25 -20 -10 -3 5 25 34 56 678))
;-> -20
(find-close1 1 '(-25 -20 -10 -3 5 25 34 56 678))
;-> -3

Metodo 2
--------
Ordiniamo la lista e poi eseguiamo una ricerca lineare.
Complessità: ordinamento = O(n*log(n)), ricerca = O(n) --> O(n*log(n))

(define (find-close2 num lst sorted)
  (let ((out nil) (found nil) (cur-num 1) (prev-num 1))
    (if-not sorted (sort lst))
    (cond
      ((>= num (lst -1)) (setq out (lst -1)))
      ((<= num (lst 0)) (setq out (lst 0)))
      (true
        (dolist (el lst found)
          (setq cur-num el) ; numero corrente della lista
          (cond ((= cur-num num)
                  (setq found true)
                  (setq out cur-num))
                ((> cur-num num) ; num > numero corrente
                  ; adesso la soluzione è il numero corrente oppure
                  ; il numero precedente
                  (setq found true)
                  (if (>= (- cur-num num) (- num prev-num))
                      (setq out prev-num)
                      (setq out cur-num)))
                (true (setq prev-num cur-num)))))) ; num < numero corrente
    out))

(find-close2 3 '(3 5 25 34 56 678))
;-> 3
(find-close2 -2 '(3 5 25 34 56 678))
;-> 3
(find-close2 678 '(3 5 25 34 56 678))
;-> 678
(find-close2 1000 '(3 5 25 34 56 678))
;-> 678
(find-close2 4 '(3 5 25 34 56 678))
;-> 3
(find-close2 0 '(-25 -20 -10 -3 5 25 34 56 678))
;-> -3
(find-close2 -15 '(-25 -20 -10 -3 5 25 34 56 678))
;-> -20
(find-close2 1 '(-25 -20 -10 -3 5 25 34 56 678))
;-> -3

Metodo 3
--------
Ordiniamo la lista e poi eseguiamo una ricerca binaria:
1) trovare con ricerca binaria la posizione in cui num dovrebbe essere inserito;
2) i soli candidati sono gli elementi immediatamente a sinistra e a destra;
3) scegliere quello con distanza minima.
Complessità: ordinamento = O(n*log(n)), ricerca = O(log(n)) --> O(n*log(n))

(define (find-close3 num lst sorted vector)
  (letn ((n (length lst))
         (out nil)
         (lo 0)
         (hi (- (length lst) 1))
         (mid 0)
         (found nil))
    (if-not vector (setq lst (array n lst)))
    (if-not sorted (sort lst))
    (cond
      ((>= num (lst -1)) (setq out (lst -1)))
      ((<= num (lst 0)) (setq out (lst 0)))
      (true
        (while (and (<= lo hi) (not found))
          (setq mid (/ (+ lo hi) 2))
          (cond
            ((= (lst mid) num)
              (setq out num)
              (setq found true))
            ((< (lst mid) num)
              (setq lo (+ mid 1)))
            (true
              (setq hi (- mid 1)))))
        (if (not found)
          (letn ((left (lst hi))
                 (right (lst lo)))
            (if (>= (- right num) (- num left))
                (setq out left)
                (setq out right))))))
    out))

(find-close3 3 '(3 5 25 34 56 678))
;-> 3
(find-close3 -2 '(3 5 25 34 56 678))
;-> 3
(find-close3 678 '(3 5 25 34 56 678))
;-> 678
(find-close3 1000 '(3 5 25 34 56 678))
;-> 678
(find-close3 4 '(3 5 25 34 56 678))
;-> 3
(find-close3 0 '(-25 -20 -10 -3 5 25 34 56 678))
;-> -3
(find-close3 -15 '(-25 -20 -10 -3 5 25 34 56 678))
;-> -20
(find-close3 1 '(-25 -20 -10 -3 5 25 34 56 678))
;-> -3

Vediamo la velocità delle funzioni:

(setq lst (rand 100 100))
(time (find-close1 50 lst) 1e5)
;-> 929.98
(time (find-close2 50 lst) 1e5)
;-> 1189.433
(time (find-close3 50 lst) 1e5)
;-> 900.162

(silent (setq lst (rand 100000 100000)))
(time (find-close1 50000 lst) 100)
;-> 1158.9
(time (find-close2 50000 lst) 100)
;-> 3443.445
(time (find-close3 50000 lst) 100)
;-> 3242.479

(time (find-close1 90000 lst) 100)
;-> 1158.9
(time (find-close2 90000 lst) 100)
;-> 4339.817
(time (find-close3 90000 lst) 100)
;-> 3241.947

Le funzioni 'find-close2' e 'find-close3' ordinano la lista ad ogni chiamata.
Vediamo la loro velocità su una lista già ordinata:

(silent (setq lst (sort (rand 100000 100000))))
(time (find-close2 90000 lst true) 100)
;-> 1379.925
(time (find-close3 90000 lst true) 100)
;-> 835.704
L'ordinamento della lista è l'oprazione più gravosa in termini di tempo.

Inoltre la funzione 'find-close3' trasforma la lista in vettore ad ogni chiamata (perchè il vettore è molto più veloce quandi si recupera un valore con un indice).
Vediamo la sua velocità su un vettore già ordinata:

(time (find-close1 90000 lst true true) 100)
(time (find-close3 90000 lst true true) 100)
;-> 678.183

Conclusioni
-----------
Il calcolo della complessità teorica di una funzione è molto importante, comunque per ottenere una fuzione veramente efficiente bisogna considerare il tipo di operazioni che vogliamo effettuare e scegliere i tipi di dati e gli algoritmi di conseguenza.


-----------------------
Alfabeto semi-diagonale
-----------------------

Dato un carattere maiuscolo dell'alfabeto inglese, stampare un alfabeto semidiagonale da "A" fino al carattere dato.

Esempio:
Carattere = F
Output =
A 
 B B 
  C C C 
   D D D D 
    E E E E E 
     F F F F F F 

(char "A")
;-> 65

(define (prn ch)
  (let (ord (- (char ch) 65))
    (for (i 0 ord)
      (println (dup " " i) (dup (string (char (+ 65 i)) " ") (+ i 1))))))

(prn "F")
;-> A
;->  B B
;->   C C C
;->    D D D D
;->     E E E E E
;->      F F F F F F

Versione code-golf (101 caratteri):
(define(f c)(let(d(-(char c)65))(for(i 0 d)(println(dup" "i)(dup(string(char(+ 65 i))" ")(+ i 1))))))
(f "F")


--------------------
Seqindignot sequence
--------------------

https://codegolf.stackexchange.com/questions/147409/seqindignot-sequence

Generare la seguente sequenza (0-indexed):

  1 0 3 2 5 4 7 6 9 8 22 20 30 24 23 26 25 28 27 32 11 33 10 14 13
  16 15 18 17 31 12 29 19 21 50 40 41 42 44 45 35 36 37 51 38 39 52
  53 55 56 34 ...

Come funziona questa sequenza?
Il numero all'indice 'i' deve essere il primo in ordine che non ha cifre in comune con 'i' e non è ancora comparso negli indici precedenti.

Esempi:
0: Il primo numero (0) contiene la stessa cifra, quindi cerchiamo il successivo (1), che non contiene la stessa cifra. Quindi n=0 restituisce 1.
1: Il primo numero (0) non contiene la stessa cifra, quindi n=1 restituisce 0.
2: Abbiamo già incontrato 0 e 1, e la cifra successiva (2) contiene la stessa cifra, quindi cerchiamo il successivo (3), che non contiene la stessa cifra. Quindi n=2 restituisce 3.
...
10: Abbiamo già incontrato i numeri da 0 a 9, quindi il successivo è 10. I numeri da 10 a 19 contengono la cifra corrispondente 1, il 20 contiene la cifra corrispondente 0, il 21 contiene di nuovo la cifra corrispondente 1, il 22 è valido, quindi n=10 restituisce 22.

; Funzione che verifica se due numeri hanno almeno una cifra in comune
(define (common-digit? num1 num2)
  (intersect (explode (string num1)) (explode (string num2))))

(define (seq limit)
  (local (out curr trovato)
    (setq out '())
    ; ciclo per ogni indice...
    (for (i 0 (- limit 1))
      ; numero candidato corrente
      ; (parte sempre da 0 per ogni numero della sequenza)
      (setq curr 0)
      (setq trovato nil)
      (until trovato
        ; se il candidato corrente non esiste nella sequenza e
        ; non ha cifre in comune con l'indice corrente,
        ; allora abbiamo trovato il numero relativo all'indice corrente
        (when (and (not (ref curr out)) (null? (common-digit? i curr)))
              (push curr out -1)
              (setq trovato true))
        ; prossimo candidato
        (++ curr)))
    out))

(seq 51)
;-> (1 0 3 2 5 4 7 6 9 8 22 20 30 24 23 26 25 28 27 32 11 33 10 14 13
;->  16 15 18 17 31 12 29 19 21 50 40 41 42 44 45 35 36 37 51 38 39 52
;->  53 55 56 34)


----------------------------------------
Oggetti con almeno un elemento in comune
----------------------------------------

Scriviamo una funzione che verifica se due oggetti hanno almeno un elemento in comune.
I due oggetti evono essere dello stesso tipo: liste o stringhe o numeri interi o numeri floating.
Per due liste: hanno almeno un elemento in comune?
Per due interi: hanno almeno una cifra in comune?
Per due float: hanno almeno una cifra in comune?

Per gli interi prendiamo il valore assoluto per eliminare il segno "-" (che non deve essere confrontato).
Per gli interi prendiamo il valore assoluto per eliminare il segno "-" e togliamo il carattere di separazione ".".

(define (common? obj1 obj2)
  (cond ((list? obj1)
          (true? (intersect obj1 obj2)))
        ((integer? obj1)
          (true? (intersect (explode (string (abs obj1)))
                            (explode (string (abs obj2))))))
        ((float? obj1)
          (true? (intersect (explode (replace "." (string (abs obj1)) ""))
                            (explode (replace "." (string (abs obj2)) "")))))))

Proviamo:

(common? '(1 2 3) '(4 5 6))
(common? '(1 2 3) '(4 5 1))

(common? 123 456)
;-> nil
(common? -123 -456)
;-> nil

(common? 123 451)
(common? 123 451)
;-> true

(common? -123.456 -789.789)
;-> nil
(common? -123.456 -789.781)
;-> true


--------------------------------------------
Ricostruzione di una matrice dalle diagonali
--------------------------------------------

Date le diagonali di una matrice quadrata (da quella in alto a destra fino a quella in basso a sinistra), ricostruire la matrice.
Esempio:
Diagonali = ((5) (4 10) (3 9 15) (2 8 14 20) (1 7 13 19 25) (6 12 18 24) (11 17 23) (16 22) (21))
Matrice =    1  2  3  4  5
             6  7  8  9 10
            11 12 13 14 15
            16 17 18 19 20
            21 22 23 24 25

Il problema è invertire la trasformazione che estrae le diagonali secondarie (antidiagonali), ordinate dalla diagonale più in alto a destra fino a quella più in basso a sinistra.
Per una matrice N*N, ogni elemento (r,c) appartiene all'antidiagonale di indice

 (setq r (+ d c (- (- n 1))))

  d = r - c + (N - 1)

Infatti:

  (0,N-1) --> d = 0
  (N-1,0) --> d = 2N-2

Le diagonali sono fornite proprio in quest'ordine.
L'unica attenzione riguarda l'ordine degli elementi all'interno di ciascuna diagonale.

Nel nostro caso:

  ((5)
   (4 10)
   (3 9 15)
   (2 8 14 20)
   (1 7 13 19 25)
   (6 12 18 24)
   (11 17 23)
   (16 22)
   (21))

gli elementi di ogni diagonale sono elencati dall'alto verso il basso (cioè con riga crescente).
Quindi basta percorrere ogni diagonale assegnando:

  c = max(0, N-1-d) + k
  r = d + c - (N-1)
  dove k è la posizione dell'elemento nella diagonale.

Per N = 5 si ottiene:

  +---+----------------+-------------------------------+
  | d | diagonale      | coordinate                    |
  +---+----------------+-------------------------------+
  | 0 | (5)            | (0,4)                         |
  | 1 | (4 10)         | (0,3) (1,4)                   |
  | 2 | (3 9 15)       | (0,2) (1,3) (2,4)             |
  | 3 | (2 8 14 20)    | (0,1) (1,2) (2,3) (3,4)       |
  | 4 | (1 7 13 19 25) | (0,0) (1,1) (2,2) (3,3) (4,4) |
  | 5 | (6 12 18 24)   | (1,0) (2,1) (3,2) (4,3)       |
  | 6 | (11 17 23)     | (2,0) (3,1) (4,2)             |
  | 7 | (16 22)        | (3,0) (4,1)                   |
  | 8 | (21)           | (4,0)                         |
  +---+----------------+-------------------------------+

che ricostruisce:  1  2  3  4  5
                   6  7  8  9 10
                  11 12 13 14 15
                  16 17 18 19 20
                  21 22 23 24 25

In pratica, ogni elemento delle diagonali viene copiato direttamente nella sua posizione (r,c) della matrice.

(define (diag2matrix diags)
  (local (n out d r c start)
    (setq n (/ (+ (length diags) 1) 2))
    (setq out (array-list (array n n '(0))))
    (for (d 0 (- (length diags) 1))
      (setq start (max 0 (- (- n 1) d)))
      (for (k 0 (- (length (diags d)) 1))
        (setq c (+ start k))
        (setq r (+ d c (- (- n 1))))
        (setf ((out r) c) ((diags d) k))))
    out))

(setq dia '((5) (4 10) (3 9 15) (2 8 14 20) (1 7 13 19 25) (6 12 18 24) (11 17 23) (16 22) (21)))

(diag2matrix dia)
;-> ((1 2 3 4 5)
;->  (6 7 8 9 10)
;->  (11 12 13 14 15)
;->  (16 17 18 19 20)
;->  (21 22 23 24 25))


---------------
Sequenza minima
---------------

https://codegolf.stackexchange.com/questions/170787/compute-the-minimum-anan-1-such-that-a1a2-dotsan-is-prime

Calcolare la sequenza dove i numeri sono i minimi per cui risulta:
  a(1) = 2
  a(n) > a(n−1) tale che a(1) + a(2) + ... + a(n) sia primo

Sequenza OEIS A051935:
a(n) = smallest number > a(n-1) such that a(1) + a(2) + ... + a(n) is a prime.
  2, 3, 6, 8, 10, 12, 18, 20, 22, 26, 30, 34, 36, 42, 44, 46, 50, 52, 60,
  66, 72, 74, 76, 78, 80, 82, 102, 108, 114, 116, 118, 126, 128, 132, 136,
  138, 144, 146, 150, 154, 158, 162, 166, 170, 174, 186, 196, 198, 210,
  222, 228, 236, 240, 244, 246, 254, 270, 280, 282, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

Prima versione
--------------

(define (seq1 limite)
  (local (out somma cur-val trovato)
    (setq out '(2))
    (setq somma 2)
    (setq cur-val 3)
    (for (i 2 limite)
      (setq trovato nil)
      (until trovato
        (when (prime? (+ somma cur-val))
            (setq trovato true)
            (push cur-val out -1)
            (setq somma (apply + out)))
      (++ cur-val)))
    out))

(seq 60)
;-> (2 3 6 8 10 12 18 20 22 26 30 34 36 42 44 46 50 52 60
;->  66 72 74 76 78 80 82 102 108 114 116 118 126 128 132 136
;->  138 144 146 150 154 158 162 166 170 174 186 196 198 210
;->  222 228 236 240 244 246 254 270 280 282 300)

Seconda versione
----------------

1) Ricalcolo della 'somma'
Quando troviamo un nuovo elemento calcoliamo:
  (setq somma (apply + out))
Questo ricalcola tutta la somma della lista ogni volta, con costo O(n).
Però conosciamo già la somma precedente, quindi basta fare:
(++ somma cur-val)
Il costo diventa O(1).

2) Variabile 'trovato'
Può essere eliminata usando direttamente until sulla condizione:
  (until (prime? (+ somma cur-val))
    (++ cur-val))
poi, una volta usciti dal ciclo:
  (push cur-val out -1)
  (++ somma cur-val)
  (++ cur-val)

(define (seq2 limite)
  (local (out somma cur-val)
    (setq out '(2))
    (setq somma 2)
    (setq cur-val 3)
    (for (i 2 limite)
      (until (prime? (+ somma cur-val))
        (++ cur-val))
      (push cur-val out -1)
      (++ somma cur-val)
      (++ cur-val))
    out))

(seq2 60)
;-> (2 3 6 8 10 12 18 20 22 26 30 34 36 42 44 46 50 52 60
;->  66 72 74 76 78 80 82 102 108 114 116 118 126 128 132 136
;->  138 144 146 150 154 158 162 166 170 174 186 196 198 210
;->  222 228 236 240 244 246 254 270 280 282 300)

Test di velocità
(time (seq1 10000))
;-> 2030.871
(time (seq2 10000))
;-> 1340.855

Terza versione
--------------

Sul sito di code-golf è presente la seguente soluzione in python:

n=f=a=b=1
while 1:
 f*=n;n+=1;a+=1
 if~f%n<a-b:print a;b=a;a=0

Riscriviamola in forma leggibile:

n = 1
f = 1
a = 1
b = 1
while True:
    f *= n
    n += 1
    a += 1
    if ~f % n < a - b:
        print(a)
        b = a
        a = 0

1) Cosa rappresentano le variabili
  n = intero corrente.
  f = (n-1)!.
  a = contatore dei passi dall'ultima stampa.
  b = ultimo valore stampato.
Infatti, all'inizio di ogni iterazione vale
  f = (n-1)!
perché viene eseguito
  f *= n
  n += 1
quindi, dopo l'incremento di n, f contiene proprio (n-1)!.

2) La condizione if ~f % n < a - b:
L'espressione
  ~f % n
usa il fatto che
  ~x = -x - 1
quindi equivale a
  (-f - 1) % n
Poiché f = (n-1)!, otteniamo
  (-(n-1)!-1) mod n
Per il Teorema di Wilson (n-1)! ≡ -1 (mod n) se e solo se n è primo.
Di conseguenza
  (-(n-1)!-1) mod n = 0
quando n è primo.
Se n è composto, il resto è positivo (salvo il caso particolare n=4, che qui non crea problemi).
La condizione completa 'if ~f % n < a - b' diventa quindi:
  resto < distanza
dove:
  distanza = a - b
è la differenza fra il contatore corrente e l'ultimo valore stampato.
Quando il resto è sufficientemente piccolo (in particolare 0 quando n è primo), viene stampato il valore di a.
Il programma sfrutta contemporaneamente il teorema di Wilson (~f % n), il fatto che ~x = -x-1 e un aggiornamento molto compatto delle variabili a e b.

Per convertirlo in newLISP dobbiamo fare le seguenti considerazioni:

a) inizializzare f come 1L (bigint) per gestire il fattoriale

b) l'operatore '~' in newLISP non supporta i bigint.
Nel codice Python ~f serve solo perché ~f = -f - 1
Quindi basta sostituire (% (~ f) n) con (% (- (- f) 1L) n)

c) Python fa il modulo con resto sempre non negativo.
In newLISP invece % segue la regola del resto con il segno del dividendo:
(% -5 3)
;-> -2
Non 1 come Python.
Bisogna quindi normalizzare il modulo:
(mod (- (- f) 1L) n)
con la funzione 'pmod' che simula la funzione 'mod' di python:

(define (pmod a b)
  (% (+ (% a b) b) b))

(define (seq3 limite)
  (local (out n f a b)
    (setq out '())
    (setq n 1L)
    (setq f 1L)
    (setq a 1)
    (setq b 1)
    (while (< (length out) limite)
      (setq f (* f n))
      (++ n)
      (++ a)
      (if (< (pmod (- (- f) 1L) n) (- a b))
        (begin
          (push a out -1)
          (setq b a)
          (setq a 0)))
      )
    out))

(seq3 60)
;-> (2 3 6 8 10 12 18 20 22 26 30 34 36 42 44 46 50 52 60
;->  66 72 74 76 78 80 82 102 108 114 116 118 126 128 132 136
;->  138 144 146 150 154 158 162 166 170 174 186 196 198 210
;->  222 228 236 240 244 246 254 270 280 282 300)

L'algoritmo è molto elegante, anche se è più lento di 'seq2' perché manipola fattoriali che crescono molto rapidamente.


---------------------------------------------------
Media della somma delle lunghezze dei numeri binari
---------------------------------------------------

https://codegolf.stackexchange.com/questions/80586/mean-bits-an-average-challenge

Dato un intero N >= 1, calcolare il numero medio di bit della somma delle lunghezze di ciascun intero binario compreso tra 0 e N - 1.
Il risultato può essere calcolato come la somma del numero di bit nella rappresentazione binaria di ciascun intero da 0 a N - 1, divisa per N.
La rappresentazione binaria di un intero non ha zeri iniziali (ad eccezione dello zero).

Esempio
  N = 6
  num  binario  lunghezza
  0      0       1 bit
  1      1       1 bit
  2      10      2 bit
  3      11      2 bit
  4      100     3 bit
  5      101     3 bit
Numero medio di bit = (1 + 1 + 2 + 2 + 3 + 3) / 6 = 2

Media dei primi numeri naturali:
  1 --> 1
  2 --> 1
  3 --> 1.3333333
  4 --> 1.5
  5 --> 1.8
  6 --> 2
  7 --> 2.1428571
  8 --> ...

(define (bmean N)
  (let (sum 0)
    (for (num 0 (- N 1))
      (++ sum (length (bits num))))
    (div sum N)))

(map bmean (sequence 1 8))
;-> (1 1 1.333333333333333 1.5 1.8 2 2.142857142857143 2.25)

Versione code-golf (64 caratteri):
(define(f N s)(for(k 0(- N 1))(inc s(length(bits k))))(div s N))

(map f (sequence 1 8))
;-> (1 1 1.333333333333333 1.5 1.8 2 2.142857142857143 2.25)


----------------------------------------------
Numeri generati dal lancio ripetuto di un dado
----------------------------------------------

Determinare la sequenza dei numeri che possono essere generati lanciando successivamente un dado regolare con i numeri da 1 a 6 e concatenando i risultati.
Esempio:
Il numero 61 si può ottenere lanciando un dado due volte e combinando i risultati, mentre il 17 no.

Sequenza OEIS A057436:
Contains digits 1 through 6 only.
  1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25, 26, 31, 32,
  33, 34, 35, 36, 41, 42, 43, 44, 45, 46, 51, 52, 53, 54, 55, 56, 61, 62,
  63, 64, 65, 66, 111, 112, 113, 114, 115, 116, 121, 122, 123, 124, 125,
  126, 131, 132, 133, 134, 135, 136, 141, 142, 143, ...

Formula:
  a(0) = 1
  a(n+1) = 1 + (if a(n) mod 10 < 6 then a(n) else a(a(n)\10)*10)

La sequenza è costituita da tutti gli interi positivi la cui rappresentazione decimale contiene solo le cifre da 1 a 6.
Infatti ogni lancio del dado produce una cifra tra 1 e 6, e la concatenazione dei risultati genera un numero formato esclusivamente da tali cifre.
Per esempio:
  1, 2, 3, 4, 5, 6,
  11, 12, 13, 14, 15, 16,
  21, 22, 23, 24, 25, 26,
  31, 32, 33, ...
  ...
  61, 62, 63, 64, 65, 66,
  111, 112, 113, ...

Non appartengono alla sequenza tutti i numeri che contengono almeno una cifra tra 0, 7, 8 e 9.
Esempi:
  6    -> sì
  61   -> sì
  123  -> sì
  6661 -> sì
  10   -> no (contiene 0)
  17   -> no (contiene 7)
  908  -> no (contiene 9,0,8)

La lunghezza del numero corrisponde al numero di lanci del dado.
Esistono esattamente (6^n) numeri di n cifre ottenibili, perché ogni posizione può assumere uno dei 6 valori:
  +-------+----------+
  | Cifre | Quantità |
  +-------+----------+
  |     1 |        6 |
  |     2 |       36 |
  |     3 |      216 |
  |     4 |     1296 |
  |     n |    (6^n) |
  +-------+----------+

Il numero totale dei valori ottenibili con al più (n) lanci vale:

  6 + 6^2 + ... + 6^n = (6*(6^n - 1))/5

Un intero positivo N appartiene alla sequenza se e solo se tutte le sue cifre sono comprese tra 1 e 6.
Equivalentemente, dividendo ripetutamente il numero per 10, ogni resto deve appartenere all'insieme (1 2 3 4 5 6). Se compare un resto pari a 0, 7, 8 o 9, il numero non è ottenibile.

(define (check? num)
  (null? (intersect '("0" "7" "8" "9") (explode (string num)))))

(filter check? (sequence 1 143))
;-> (1 2 3 4 5 6 11 12 13 14 15 16 21 22 23 24 25 26 31 32
;->  33 34 35 36 41 42 43 44 45 46 51 52 53 54 55 56 61 62
;->  63 64 65 66 111 112 113 114 115 116 121 122 123 124 125
;->  126 131 132 133 134 135 136 141 142 143)


--------------------
Ricerca esponenziale
--------------------

Data una lista ordinata e un elemento x da cercare, trovare la posizione di x nella lista.
Esempi:
  lista = 1 13 34 50 60
  x = 50
  output = 3 (indice dell'elemento 50)

  lista = 1 13 34 50 60
  x = 55
  output = nil (elemento non presente nella lista)

La ricerca esponenziale prevede due passaggi:
1) Trovare l'intervallo di indici in cui è presente l'elemento
2) Eseguire la ricerca binaria nell'intervallo trovato.

Per trovare l'intervallo in cui l'elemento potrebbe essere presente iniziamo con una lista di dimensione 1, confrontiamo il suo ultimo elemento con x, poi proviamo con una lista di dimensione 2, poi 4 e così via finché l'ultimo elemento della lista creata non è maggiore dell'elemento da cercare.
Una volta trovato un indice 'i' (dopo ripetuti raddoppi di i), sappiamo che l'elemento deve essere presente tra 'i/2' e 'i' (i/2 perché non siamo riusciti a trovare un valore maggiore nell'iterazione precedente).
Complessità temporale: O(log(n)) (non esponenziale come il nome)

Poichè la ricerca binaria utilizza molto l'indicizzazione, usiamo un vettore al posto di una lista.

(define (exp-search x vec)
  (local (out len i left right trovato mid)
    (setq out nil)
    (setq len (length vec))
    (setq i 1)
    ; ricerca dell'intervallo per la ricerca binaria
    ; raddoppiando ripetutamente i
    (while (and (< i len) (> x (vec i)))
      (setq i (* i 2)))
    ; indice sinistro dell'intervallo
    (setq left (/ i 2))
    ; indice destro dell'intervallo
    (setq right (min i (- len 1)))
    (setq trovato nil)
    ; ricerca binaria
    (while (and (<= left right) (not trovato))
      ; indice di mezzo tra left e right
      (setq mid (/ (+ left right) 2))
      (cond ((= x (vec mid)) ; elemento trovato
            (setq out mid) (setq trovato true))
            ((> x (vec mid)) ; ricercare nella parte destra
              (setq left (+ mid 1)))
            (true            ; ricercare nella parte sinistra
              (setq right (- mid 1)))))
    out))

Proviamo:

(exp-search 50 '(1 13 34 50 60))
;-> 3
(exp-search 1 '(1 13 34 50 60))
;-> 0
(exp-search 60 '(1 13 34 50 60))
;-> 4
(exp-search 0 '(1 13 34 50 60))
;-> nil
(exp-search 61 '(1 13 34 50 60))
;-> nil
(exp-search 35 '(1 13 34 50 60))
;-> nil

(exp-search 50 '(1 13 34 50 60 77))
;-> 3
(exp-search 1 '(1 13 34 50 60 77))
;-> 0
(exp-search 60 '(1 13 34 50 60 77))
;-> 4
(exp-search 0 '(1 13 34 50 60 77))
;-> nil
(exp-search 61 '(1 13 34 50 60 77))
;-> nil
(exp-search 35 '(1 13 34 50 60 77))
;-> nil

(exp-search -77 '(-77 -60 -34 -1 0 1 13 50 77)
;-> 0
(exp-search -98 '(-77 -60 -34 -1 0 1 13 50 77))
;-> nil
(exp-search -1 '(-77 -60 -34 -1 0 1 13 50 77))
;-> 3


-----------
Sort string
-----------

; Funzione che ordina i caratteri di una stringa
(define (sort-str str)
(join (sort (explode str))))

(sort-str "sdgFfsdHjg")

; Funzione che ordina i caratteri di ogni stringa di una lista
(define (sort-str-lst lst) (map sort-str lst))

(sort-str-lst '("cbaabc" "zyx" "9 8 7 6"))


------------------------------------------
Ordinamento per frequenza (Frequency Sort)
------------------------------------------

Data una lista di elementi, ordinare la lista in base alla frequenza degli elementi.
Esempio
  lista = ("c" "c" "b" "b" "a")
  output = ("b" "b" "c" "c" "a")

La funzione 'bayes-train' calcola le frequenze degli elementi di una lista:
(bayes-train '("c" "c" "b" "b" "a") 'L)
;-> 5 ; numero di elementi della lista

Il risultato si trova nel contesto L (passato come parametro)
(L)
;-> (("a" (1)) ("b" (2)) ("c" (2)))

Adesso basta ricostruire la lista ordinata:

(setq lst (L))

(define (comp x y) (>= (last x) (last y)))

(sort lst comp)
;-> (("b" (2)) ("c" (2)) ("a" (1)))

(setq sol '())

(dolist (el lst)
  (extend sol (dup (el 0) (first (el 1)) true)))
;-> ("b" "b" "c" "c" "a")

La funzione 'bayes-train' agisce in modo incrementale, se la usiamo con un'altra lista e lo stesso contesto, allora il risultato viene 'aggiunto' ai dati già presenti nel contesto.

Per eliminare un contesto usiamo 'delete':
(delete 'L)
;-> true
(L)
;-> ERR: invalid function : (L)

Per passare il contesto come parametro ad una funzione abbiamo due metodi:

1) Uso di 'eval' (il contesto va passato come parametro quotato)

(define-macro (test ctx)
  (bayes-train '("c" "c" "b" "b" "a") (eval ctx)))

(test 'A)
;-> (5)
(A)
;-> (("a" (1)) ("b" (2)) ("c" (2)))

2) Uso di '(args)' (il contesto va passato come parametro non quotato)

(define-macro (test)
  (bayes-train '("c" "c" "b" "b" "a") (args 0)))

(test B)
;-> (5)
(B)
;-> (("a" (1)) ("b" (2)) ("c" (2)))

; Funzione che ordina gli elementi di una lista
; in base alla loro frequenza e al loro valore
(define (sort-bayes lst)
  (define (comp x y) (>= (last x) (last y)))
  (let ((out '()))
    (bayes-train lst (args 0))
    (setq lst ((eval (args 0))))
    (sort lst comp)
    (setq out '())
    (dolist (el lst)
      (extend out (dup (el 0) (first (el 1)) true)))
    out))

Proviamo:

(sort-bayes '("c" "c" "b" "b" "a") 'C1)
;-> ("b" "b" "c" "c" "a")

(sort-bayes (explode "cuccurella") 'C2)
;-> ("c" "c" "c" "l" "l" "u" "u" "a" "e" "r")

Se usiamo lo stesso contesto:
(sort-bayes (explode "cuccurella") 'C2)
;-> ("c" "c" "c" "c" "c" "c" "l" "l" "l" "l" "u" "u" "u" "u"
;->  "a" "a" "e" "e" "r" "r")


------------------
"Hello" in ritardo
------------------

Scrivere una funzione che genera prende un intero N e stampa "Hello" l'N-esima volta che viene chiamata (sempre con lo stesso parametro N), altrimenti restituisce nil.

Esempio:
  (func 2)
  ;-> nil
  (func 2)
  ;-> "Hello"

;(setq idx 0)

(define (func n)
  (inc idx)
  (when (= idx n) (setq idx 0) "Hello"))

(func 3)
;-> nil
(func 3)
;-> nil
(func 3)
;-> "Hello"


----------------------
Inversioni di elementi
----------------------

Dato un vettore di interi 'vec', trovare il conteggio delle inversioni nel vettore.
Due elementi del vettore vec[i] e vec[j] formano un'inversione se risulta:

  vec[i] > vec[j] e i < j.

Il conteggio delle inversioni indica quanto il vettore è lontano (o vicino) dall'essere ordinato.
Se l'array è ordinato in modo crescente, allora il conteggio delle inversioni è 0, ma se l'array è ordinato in ordine inverso, il conteggio delle inversioni è massimo.

Metodo 1 (brute-force)
----------------------
Usiamo due cicli innestati verificando ogni possibile coppia di elementi e contando quante di queste coppie soddisfano la condizione di inversione.

(define (inv-count vec)
  (let ((len (length vec)) (inv 0))
        (for (i 0 (- len 2))
          (for (j (+ i 1) (- len 1))
            ; Se l'elemento corrente è maggiore del successivo,
            ; incrementa il contatore
            (if (> (vec i) (vec j)) (++ inv))))
        inv))

Proviamo:

(inv-count '(1 2 3 4 5 6))
;-> 0
(inv-count '(6 5 4 3 2 1))
;-> 15
(inv-count '(1 8 4 -6 9 0 -5 3 8 9 5 3 -3 2 6 5 4 3 2 -1))
;-> 98

Metodo 2 (merge-sort)
---------------------
Possiamo utilizzare l'algoritmo di merge sort per contare le inversioni in un array. Innanzitutto, dividiamo l'array in due metà: metà sinistra e metà destra.
Successivamente, contiamo ricorsivamente le inversioni in entrambe le metà.
Durante la fusione delle due metà, contiamo anche quanti elementi dell'array della metà sinistra sono maggiori degli elementi dell'array della metà destra, poiché queste rappresentano inversioni incrociate (ovvero, un elemento della metà sinistra dell'array è maggiore di un elemento della metà destra durante il processo di fusione nell'algoritmo di merge sort).
Infine, sommiamo le inversioni della metà sinistra, della metà destra e le inversioni incrociate per ottenere il numero totale di inversioni nell'array.

(define (extract obj start end)
"Extract elements from a list/string (from start to (end -1) indexes)"
  (if (nil? end)
      (slice obj start)
      (slice obj start (- end start))))

; Questa funzione unisce due sotto-array ordinati arr[l..m] e arr[m+1..r]
; e conta anche le inversioni nell'intero sotto-array arr[l..r].
(define (count-merge l m r)
  (letn ( ; Conta in due sotto-array
          (n1 (+ (- m l) 1))
          (n2 (- r m))
          ; Imposta due liste per la metà sinistra e per la metà destra
          (left (extract arr l (+ m 1)))
          (right (extract arr (+ m 1) (+ r 1)))
          ; Inizializza il conteggio delle inversioni
          ; e unisce le due metà
          (res 0)
          (i 0)
          (j 0)
          (k l))
      (while (and (< i n1) (< j n2))
          ; Nessun incremento nel conteggio delle inversioni
          ; se left[] ha un elemento minore o uguale
          (if (<= (left i) (right j))
            (begin (setf (arr k) (left i)) (++ i))
          ;else
            (begin (setf (arr k) (right j)) (++ j) (setq res (+ res (- n1 i)))))
          (++ k))
      ; Merge degli elementi rimanenti
      (while (< i n1)
          (setf (arr k) (left i))
          (++ i)
          (++ k))
      (while (< j n2)
          (setf (arr k) (right j))
          (++ j)
          (++ k))
      res))

; Funzione di base per contare le inversioni nell'array
(define (count-inv-base l r)
  (letn ((res 0)
         (m (/ (+ l r) 2)))
    (when (< l r)
      ; Conta ricorsivamente le inversioni
      ; nelle metà sinistra e destra
      (setq res (+ res (count-inv-base l m)))
      (setq res (+ res (count-inv-base (+ m 1) r)))
      ; Conta le inversioni tali che l'elemento maggiore si trovi nella
      ; metà sinistra e quello minore nella metà destra
      (setq res (+ res (count-merge l m r))))
    res))

; Funzione main
(define (conta-inv arr)
  (setq len (length arr))
  (count-inv-base 0 (- len 1)))

Proviamo:

(conta-inv '(1 2 3 4 5 6))
;-> 0
(conta-inv '(6 5 4 3 2 1))
;-> 15
(conta-inv '(1 8 4 -6 9 0 -5 3 8 9 5 3 -3 2 6 5 4 3 2 -1))
;-> 98

(setq t (rand 10 20))
(inv-count t)
;-> 104
(conta-inv t)
;-> 104


-----------------------------------
Ordinamento rigoroso di una matrice
-----------------------------------

Data una matrice MxN, ordinarla in ordine rigoroso.
Per ordine rigoroso si intende che la matrice è ordinata in modo tale che tutti gli elementi di una riga siano ordinati in ordine crescente e che, per la riga 'i', dove 1 <= i <= m-1, il primo elemento sia maggiore o uguale all'ultimo elemento della riga 'i-1'.

Esempio:
matrice = 
          | 5 4 7 |
matrice = | 1 3 8 |
          | 2 9 6 |

          | 1 2 3 |
output =  | 4 5 6 |
          | 7 8 9 |

(define (sort-matrix matrix)
  (explode (sort (flat matrix)) 3))

(setq m '((5 4 7) (1 3 8) (2 9 6)))

(sort-matrix m)
;-> ((1 2 3) (4 5 6) (7 8 9))


--------------------------------------
Ordinare numeri da 1 a N (Ciclyc Sort)
--------------------------------------

Data una lista composta da numeri interi distinti da 1 a N, ordinare la lista senza utilizzare spazio aggiuntivo.

La funzione 'sort' è sicuramente il metodo più veloce, ma usa spazio aggiuntivo.
Allora usiamo il metodo dell'ordinamento ciclico (Cyclic Sort, O(n) Tempo, O(1) Spazio).

Algoritmo Cyclic Sort algorithm
1) Attraversare la lista.
2) Se un elemento si trova nella sua posizione corretta, non fare nulla.
3) Altrimenti, scambia l'elemento con l'elemento nella sua posizione corretta.

(define (ciclyc lst)
    (let ((len (length lst)) (i 0))
      (while (< i len)
        ; se l'elemento corrente non si trova nella posizione corretta,
        ; allora lo scambia
        (if-not (= (lst (- (lst i) 1)) (lst i))
            (swap (lst i) (lst (- (lst i) 1)))
            ;else
            ; 'i' viene incrementata solo quando (lst i) ha il valore corretto
            (++ i)))
      lst))

Proviamo:

(ciclyc '(6 5 4 3 2 1))
;-> (1 2 3 4 5 6)
(ciclyc (randomize (sequence 1 20)))
;-> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)


------------------------------------------
Bilanciamento parentesi in una espressione
------------------------------------------

Data una stringa S, composta da diverse combinazioni di '(' , ')', '{', '}', '[', ']', determinare se l'espressione è bilanciata o meno.
Un'espressione è bilanciata se:
- Ogni parentesi aperta ha una parentesi chiusa corrispondente dello stesso tipo.
- Le parentesi aperte devono essere chiuse nell'ordine corretto.

Nel caso generale bisogna permettere qualsiasi annidamento, purché ogni parentesi venga chiusa con quella corrispondente.
Esempio:
  S = "( [ { } ] [ ] } ( { } ) )"

La soluzione più semplice è utilizzare una pila (stack) delle parentesi aperte.

(define (par-check1 s)
  (local (out stack ch)
    (setq out true)
    (setq stack '())
    (dostring (c s (= out nil))
      (setq ch (char c))
      (cond
        ((= ch "(") (push "(" stack))
        ((= ch "[") (push "[" stack))
        ((= ch "{") (push "{" stack))
        ((= ch ")")
          (if (or (null? stack) (!= (first stack) "("))
              (setq out nil)
              (pop stack)))
        ((= ch "]")
          (if (or (null? stack) (!= (first stack) "["))
              (setq out nil)
              (pop stack)))
        ((= ch "}")
          (if (or (null? stack) (!= (first stack) "{"))
              (setq out nil)
              (pop stack)))))
    ;(println stack)
    (if (and out (null? stack))
        true
        nil)))

Proviamo:

(par-check1 "( [ { } ] )")
;-> true
(par-check1 "{ [ ( ) ] }")
;-> true
(par-check1 "{ [ ( ] ) }")
;-> nil
(par-check1 "{ ( [ ] ) }")
;-> true
(par-check1 "([{}])")
;-> true
(par-check1 "({[]})")
;-> true
(par-check1 "([)]")
;-> nil
(par-check1 "{[(])}")
;-> nil
(par-check1 "{")
;-> nil
(par-check1 "}")
;-> nil
(par-check1 "")
;-> true
(par-check1 "{ { ( [ ] ) } }")
;-> true
(par-check1 "")
; true
(par-check1 "()")
; true
(par-check1 "[]")
; true
(par-check1 "{}")
; true
(par-check1 "{[]()}")
; true
(par-check1 "([)]")
; nil
(par-check1 "{[(])}")
; nil
(par-check1 "{[()]}")
; true
(par-check1 "{([])}")
; nil
(par-check1 "{{{{}}}}")
; true
(par-check1 "(((( ))))")
; true
(par-check1 "((((}")
; nil
(par-check1 "}}")
; nil
(par-check1 "{{}}]")
; nil

Questa versione è facilmente estendibile: se vogliamo aggiungere altri tipi di delimitatori (es. '< >', ecc.), basta aggiungere i relativi casi senza modificare l'algoritmo, che rimane sempre basato sulla pila delle parentesi aperte.

Un altro metodo impone il seguente ordine alle parentesi:

  { ... [ ... ( ... ) ... ] ... }

cioè:
- "(" può comparire solo all'interno di "[".
- "[" può comparire solo all'interno di "{".
- "{" non può comparire all'interno di "(" o "[".

Questo è diverso dalla funzione 'par-check1' che effettua il classico controllo delle parentesi bilanciate.

(define (par-check2 s op)
  (local (out p1o p2o p3o ch)
    (set 'p1o 0 'p2o 0 'p3o 0)
    (setq out true)
    (dostring (c s (nil? out))
      (setq ch (char c))
      (cond 
        ((= ch "(") (++ p1o))
        ((= ch "[")
          ; esiste una par "(" non chiusa
          (if (> p1o 0) (setq out nil) (++ p2o)))
        ((= ch "{")
          ; esiste una par "(" o "[" non chiusa
          (if (or (> p1o 0) (> p2o 0)) (setq out nil) (++ p3o)))
        ((= ch ")")
          ; nessuna par "(" da chiudere
          (if (= p1o 0) (setq out nil) (-- p1o)))
        ((= ch "]")
          ; esiste una par ")" da chiudere OR
          ; nessuna par "[" da chiudere
          (if (or (> p1o 0) (= p2o 0)) (setq out nil) (-- p2o)))
        ((= ch "}")
          ; esiste una par ")" da chiudere OR
          ; esiste una par "]" da chiudere OR
          ; nessuna par "{" da chiudere
          (if (or (> p1o 0) (> p2o 0) (= p3o 0)) (setq out nil) (-- p3o)))))
    ; controllo accoppiamento parentesi ed errore
    ;(println p1o { } p2o { } p3o)
    (if (and (zero? p1o) (zero? p2o) (zero? p3o) (= out true))
      true
      nil)))

Proviamo:

(par-check2 "{ { ( [ [ ( ) ] ] ) } }")
;-> nil
(par-check2 "{ { ( [ [ ( ( ) ] ] ) } }")
;-> nil
(par-check2 "{ { [ [ [ ( ) ] ] ] } }")
;-> true
(par-check2 "{ { [ [ } } [ ( ) ] ] ]")
;-> nil
(par-check2 "{ { [ [ [ ( ) ] ] ] } { [ ( ) ] } }")
;-> true
(par-check2 "{ { [ [ [ ( [ ] ) ] ] ] } { [ ( ) ] } }")
;-> nil
(par-check2 "")
; true
(par-check2 "()")
; true
(par-check2 "[]")
; true
(par-check2 "{}")
; true
(par-check2 "{[]()}")
; true
(par-check2 "([)]")
; nil
(par-check2 "{[(])}")
; nil
(par-check2 "{[()]}")
; true
(par-check2 "{([])}")
; nil
(par-check2 "{{{{}}}}")
; true
(par-check2 "(((( ))))")
; true
(par-check2 "((((}")
; nil
(par-check2 "}}")
; nil
(par-check2 "{{}}]")
; nil


----------------------
find-floor e find-ceil
----------------------

Data una lista di interi scrivere le seguenti funzioni:
1) (find-floor x lst)
Cerca su lst il numero più vicino o uguale a x e minore o uguale a x.
2) (find-ceil x lst)
Cerca su lst il numero più vicino o uguale a x e maggiore o uguale a x.

Algoritmo:
Attraversiamo tutta la lista una volta mantenendo il valore più vicino trovato (minore o maggiore di x).
Tempo: O(n), Spazio: O(1)

(define (find-floor x lst)
  (let ((out nil) (diff 1e8))
    (dolist (el lst)
      (if (and (<= el x) (< (- x el) diff))
          (set 'out el 'diff (- x el))))
    out))

(find-floor 4 '(-2 4 6 8))
;-> 4
(find-floor 1 '(-2 4 6 8))
;-> -2
(find-floor 9 '(-2 4 6 8))
;-> 8
(find-floor -3 '(-2 4 6 8))
;-> nil

(define (find-ceil x lst)
  (let ((out nil) (diff 1e8))
    (dolist (el lst)
      (if (and (>= el x) (< (- el x) diff))
          (set 'out el 'diff (- el x))))
    out))

(find-ceil 4 '(-2 4 6 8))
;-> 4
(find-ceil 1 '(-2 4 6 8))
;-> 4
(find-ceil 9 '(-2 4 6 8))
;-> nil
(find-ceil -3 '(-2 4 6 8))
;-> -2

============================================================================

