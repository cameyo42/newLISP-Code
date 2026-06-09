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
Questo serve a separare la parte "dispari" da quella con fattori di 2.

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

8) Perché funziona
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

Il parametro 'bases' diventa concettualmente un insieme di test indipendenti
Ogni base è un "controllo" della struttura modulare di n.
Più basi aggiungiamo più riduciamo la probabilità di errore, ma aumentiamo anche il costo computazionale.

============================================================================

