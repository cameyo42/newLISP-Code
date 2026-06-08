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

============================================================================

