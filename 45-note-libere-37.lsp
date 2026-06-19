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

La funzione potrebbe quindi restituire nil se trova una contraddizione, oppure (row col candidati) se esiste almeno una cella vuota, oppure un valore speciale (ad esempio `'solved`) se non esistono più celle vuote e il Sudoku è completato.

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

In questo caso la funzione 'func2-aux' non può essere chiamata esternamente.
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

============================================================================

