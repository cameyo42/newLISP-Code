================

 NOTE LIBERE 29

================

  "If you deeply observe, everything is your teacher" Buddha

-----------
Hello world
-----------

(define (func)
  (join (map char (map (fn(x) (int x 0 2))
  (explode (0 -1 (string (* 1L 2 2 5 5 27447227 127990789037
  28494230600320532022190669342279752095349559349083778599))) 7)))))

(func)
;-> "Hello World"


--------------------------------------------
Lista dei caratteri mancanti in una funzione
--------------------------------------------

Scrivere una funzione che genera tutti i caratteri ASCII (32..126) che la funzione non utilizza.
Per esempio la seguente la funzione:

(define (test a b)
  (println "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (+ a b))

Non contiene i seguenti caratteri:

("!" "#" "$" "%" "&" "'" "*" "," "-" "." "/"
 "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
 ":" ";" "<" "=" ">" "?" "@" "[" "\\" "]"
 "^" "_" "`" "g" "h" "j" "k" "m" "o" "q" "s" "v" "w" "x" "y" "z"
 "{" "|" "}" "~")

(source 'test)
"(define (test a b)\r\n  (+ a b))\r\n\r\n"
(explode (source 'test))

Funzione che restituisce una lista con tutti i caratteri ASCII che la funzione non utilizza:

(define (function)
  ; differenza tra tutti i caratteri ASCII (da 32 a 126)
  ; e tutti i caratteri della funzione
  (difference (map char (sequence 32 126)) (explode (source 'function))))

(function)
;-> ("!" "\"" "#" "$" "%" "&" "*" "+" "," "-" "." "/" "0" "4" "5" "7"
;->  "8" "9" ":" ";" "<" "=" ">" "?" "@" "A" "B" "C" "D" "E" "F" "G" "H"
;->  "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y"
;->  "Z" "[" "\\" "]" "^" "_" "`" "b" "g" "j" "k" "v" "w" "y" "z" "{"
;->  "|" "}" "~")

Possiamo anche scrivere una funzione che calcola i caratteri mancanti di una funzione passata come parametro:

(define (missing func)
  ; differenza tra tutti i caratteri ASCII (da 32 a 126)
  ; e tutti i caratteri della funzione passata come parametro
  (difference (map char (sequence 32 126)) (explode (source (quote func)))))

(missing function)
;-> ("!" "\"" "#" "$" "%" "&" "*" "+" "," "-" "." "/" "0" "4" "5" "7"
;->  "8" "9" ":" ";" "<" "=" ">" "?" "@" "A" "B" "C" "D" "E" "F" "G" "H"
;->  "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y"
;->  "Z" "[" "\\" "]" "^" "_" "`" "b" "g" "j" "k" "v" "w" "y" "z" "{"
;->  "|" "}" "~")

(missing test)
;-> ("!" "#" "$" "%" "&" "'" "*" "," "-" "." "/" "0" "1" "2" "3" "4" "5"
;->  "6" "7" "8" "9" ":" ";" "<" "=" ">" "?" "@" "[" "\\" "]" "^" "_"
;->  "`" "g" "h" "j" "k" "m" "o" "q" "s" "v" "w" "x" "y" "z" "{" "|" "}"
;->  "~")

(missing missing)
;-> ("!" "\"" "#" "$" "%" "&" "'" "*" "+" "," "-" "." "/" "0" "4" "5"
;->  "7" "8" "9" ":" ";" "<" "=" ">" "?" "@" "A" "B" "C" "D" "E" "F" "G"
;->  "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X"
;->  "Y" "Z" "[" "\\" "]" "^" "_" "`" "b" "g" "j" "k" "v" "w" "y" "z"
;->  "{" "|" "}" "~")


---------------------
Il numero di Khinchin
---------------------

Il numero di Khinchin è una costante matematica che ha la proprietà di essere il limite, per quasi tutti i numeri reali, della media geometrica dei primi n quozienti parziali della loro frazione continua.
Questa costante, chiamata K0, è stata dimostrata da Aleksandr Yakovlevich Khinchin.

In formule, per ogni numero reale x:
                     1   
  x = a0 + ----------------------
                        1 
           a1 + -----------------
                          1
                a2 + ------------
                             1
                     a3 + -------
                     ...  
               
E' quasi sempre vero che:

  lim(a1*a2*a3*...)^(1/n) = K0
  n->Inf  

Il suo valore è:

  K0 = 2.68545200106530644530971483548179569382038229399446
         2953051152345557218859537152002801141174931847697...

Sequenza OEIS A002210:
Decimal expansion of Khinchin's constant.
  2, 6, 8, 5, 4, 5, 2, 0, 0, 1, 0, 6, 5, 3, 0, 6, 4, 4, 5, 3, 0, 9, 7, 1,
  4, 8, 3, 5, 4, 8, 1, 7, 9, 5, 6, 9, 3, 8, 2, 0, 3, 8, 2, 2, 9, 3, 9, 9,
  4, 4, 6, 2, 9, 5, 3, 0, 5, 1, 1, 5, 2, 3, 4, 5, 5, 5, 7, 2, 1, 8, 8, 5,
  9, 5, 3, 7, 1, 5, 2, 0, 0, 2, 8, 0, 1, 1, 4, 1, 1, 7, 4, 9, 3, 1, 8, 4,
  7, 6, 9, 7, 9, 9, 5, 1, 5, ...

Esistono diverse formule per calcolare questa costante, useremo la seguente:

                                1 
  K0 = Prod[i=1..Inf] (1 + -----------)^(log2(i))
                            i*(i + 2)

(define (khinchin iter)
  (let (out 1)
    (for (i 1 iter)
      (setq out (mul out (pow (add 1 (div (mul i (add i 2)))) (log i 2))))
      ;(println out)
    )
    out))

Proviamo:

(time (println (map khinchin '(1e2 1e3 1e4 1e5 1e6 1e7 1e8))))
;-> (2.479449507190302 
;->  2.655030716354817
;->  2.681499686663015
;->  2.684967264898293
;->  2.685394602203196
;->  2.685445368986610
;->  2.685451312659584)
;-> 38470.089

(time (println (khinchin 1e9)))
;-> 2.685451312659584 ;solo 5 cifre decimali corrette
;-> 276860.202

La formula converge lentamente al valore della costante.


------------------
Il numero di Somos
------------------

Il numero di Somos è una constante definita come espressione di infinite radici quadrate annidate:
Questa costante, chiamata S (sigma), è stata definita Michael Somos.

  S = sqrt(1 * sqrt(2 * sqrt(3 * sqrt(4 * ...)))) = 
    = 1.6616879496335941212958...

Sequenza OEIS A112302:
Decimal expansion of quadratic recurrence constant sqrt(1 * sqrt(2 * sqrt(3 * sqrt(4 * ...)))).
  1, 6, 6, 1, 6, 8, 7, 9, 4, 9, 6, 3, 3, 5, 9, 4, 1, 2, 1, 2, 9, 5, 8,
  1, 8, 9, 2, 2, 7, 4, 9, 9, 5, 0, 7, 4, 9, 9, 6, 4, 4, 1, 8, 6, 3, 5,
  0, 2, 5, 0, 6, 8, 2, 0, 8, 1, 8, 9, 7, 1, 1, 1, 6, 8, 0, 2, 5, 6, 0,
  9, 0, 2, 9, 8, 2, 6, 3, 8, 3, 7, 2, 7, 9, 0, 8, 3, 6, 9, 1, 7, 6, 4,
  1, 1, 4, 6, 1, 1, 6, 7, 1, 5, 5, 2, 8, ...

Esistono diverse formule per calcolare questa costante, useremo la seguente:

                           1 
  S = Prod[k=1..Inf] (1 + ---)^(1/2^k)
                           k

(define (somos iter)
  (let (out 1)
    (for (k 1 iter)
      (setq out (mul out (pow (add 1 (div k)) (div (pow 2 k)))))
      ;(println out)
    )
    out))

(time (println (map somos '(10 100 1000))))
;-> (1.661556666627748
;->  1.661687949633594
;->  1.661687949633594)
;-> 0

(somos 50)
;-> 1.661687949633594 ; 15 cifre decimali corrette

La formula converge velocemente al valore della costante.


-----------------------------
Pipistrello intorno alla Luna
-----------------------------

Stampare il giro di un pipistrello intorno alla Luna.

Pipistrello =  ^o^
Luna = 
         mmm 
       mmmmmmm
      mmmmmmmmm
       mmmmmmm
         mmm

Giro del pipistrello:

     ^o^        
     mmm            mmm^o^         mmm            mmm            mmm
   mmmmmmm        mmmmmmm        mmmmmmm^o^     mmmmmmm        mmmmmmm
  mmmmmmmmm      mmmmmmmmm      mmmmmmmmm      mmmmmmmmm^o^   mmmmmmmmm
   mmmmmmm        mmmmmmm        mmmmmmm        mmmmmmm        mmmmmmm^o^
     mmm            mmm            mmm            mmm            mmm


     mmm            mmm            mmm            mmm            mmm
   mmmmmmm        mmmmmmm        mmmmmmm        mmmmmmm        mmmmmmm
  mmmmmmmmm      mmmmmmmmm      mmmmmmmmm      mmmmmmmmm   ^o^mmmmmmmmm
   mmmmmmm        mmmmmmm        mmmmmmm     ^o^mmmmmmm        mmmmmmm
     mmm^o^         mmm         ^o^mmm            mmm            mmm
                    ^o^

                                   ^o^
     mmm         ^o^mmm            mmm
^o^mmmmmmm        mmmmmmm        mmmmmmm
  mmmmmmmmm      mmmmmmmmm      mmmmmmmmm
   mmmmmmm        mmmmmmm        mmmmmmm
     mmm            mmm            mmm


(define (fly)
  (local (bat emme spaceR spaceL)
    (setq emme '(0 3 7 9 7 3 0))
    (setq spaceR '(6 6 4 3 4 6 6))
    (setq spaceL '(6 3 1 0 1 3 6))
    (for (pos 0 6)
      (for (i 0 6)
        (if (= i pos) 
          (setq bat "^o^")
          (setq bat ""))
        (println (append (dup " " (spaceR i)) (dup "m" (emme i)) bat))
      )
    )
    (for (pos 1 6)
      (for (i 6 0)
        (if (= i pos) 
          (setq bat "^o^")
          (setq bat "   "))
        (println (append (dup " " (spaceL i)) bat (dup "m" (emme i))))
      )
    )
    '>))
(fly)

Versione compatta:

(define (fly)
 (for (p 0 6)
   (for (i 0 6)
     (setq b (if (= i p) "^o^" ""))
     (println (append (dup " " ('(6 6 4 3 4 6 6) i)) (dup "m" ('(0 3 7 9 7 3 0) i)) b))))
 (for (p 1 6)
   (for (i 6 0)
     (setq b (if (= i p) "^o^" "   "))
     (println (append (dup " " ('(6 3 1 0 1 3 6) i)) b (dup "m" ('(0 3 7 9 7 3 0) i))))))'>)

(fly)

============================================================================

