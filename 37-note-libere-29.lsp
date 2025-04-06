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

Versione animata compatta:

(define (fly pause)
 (for (p 0 6)
   (print "\027[H\027[2J")
   (for (i 0 6)
     (setq b (if (= i p) "^o^" ""))
     (println (append (dup " " ('(6 6 4 3 4 6 6) i)) (dup "m" ('(0 3 7 9 7 3 0) i)) b)))
     (sleep pause))
 (for (p 1 6)
   (print "\027[H\027[2J")
   (for (i 6 0)
     (setq b (if (= i p) "^o^" "   "))
     (println (append (dup " " ('(6 3 1 0 1 3 6) i)) b (dup "m" ('(0 3 7 9 7 3 0) i)))))
     (sleep pause))'>)

(fly 200)


------------------------
Facce opposte di un dado
------------------------

In un dado standard a 6 facce (come quelli usati nei giochi da tavolo), le facce opposte sono disposte in modo che la somma dei numeri su due facce opposte sia sempre 7.
Quindi, le coppie di facce opposte sono: (1 6) (2 5) (3 4)
Questa disposizione è comune nei dadi standard per garantire un equilibrio simmetrico.

Calcolare il valore medio atteso se lanciando due dadi consideriamo il seguente valore:
valore = somma del prodotto di ogni dado con la sua faccia opposta

In altre parole, per ogni dado, se otteniamo 1 o 6 abbiamo il valore 1 * 6 = 6,
se otteniamo 2 o 5 abbiamo il valore 2 * 5 = 10,
se otteniamo 3 o 4 abbiamo il valore 3 * 4 = 12.
Quindi il valore di ogni lancio è dato dalla somma dei prodotti delle facce opposte di ogni dado.

Esempio:
dado1 = 4  --> valore del prodotto = 4 * 3 = 12
dado2 = 1  --> valore del prodotto = 1 * 6 = 6
valore del lancio = 12 + 6 = 18

(define (dadi iter)
  (let ( (totale 0) (somma 0) (prod '(0 6 10 12 12 10 6)) )
    (for (i 1 iter)
      (setq d1 (prod (+ 1 (rand 6))))
      (setq d2 (prod (+ 1 (rand 6))))
      (setq totale (+ totale d1 d2))
    )
    (div totale iter)))

Proviamo:

(map dadi '(1e3 1e4 1e5 1e6 1e7))
;-> (18.662 18.717 18.66298 18.668098 18.6666276)

Dal punto di vista matematico:

Vogliamo calcolare il valore medio atteso della seguente quantità quando si lanciano due dadi a 6 facce:

  valore = (x * x') + (y * y')

Dove:
- x e y sono i risultati dei due dadi,
- x' è la faccia opposta di x ,
- y' è la faccia opposta di y ,
- Per un dado standard: x' = 7 - x , y' = 7 - y 

Quindi:

  valore = x*(7 - x) + y*(7 - y)

Essendo i due dadi indipendenti e identici, il valore atteso totale sarà:

  E[valore] = 2 * E[x(7 - x)]

Ora calcoliamo:

  E[x(7 - x)] = (1/6)*Sum[x=1..6](x*(7-x))

Calcoliamo i singoli termini:

  1 * 6 = 6 
  2 * 5 = 10 
  3 * 4 = 12 
  4 * 3 = 12 
  5 * 2 = 10 
  6 * 1 = 6 

  Somma: 6 + 10 + 12 + 12 + 10 + 6 = 56 

Quindi:

  E[x(7 - x)] = 56/6 = 28/3 = 9.333333333333334

Di conseguenza il valore medio atteso vale:

  E[valore] = 2 * 28/3 = 56/3 = 18.66666666666667


---------------
ASCII animation
---------------

Vediamo alcuni esempi di animazione ASCII in un terminale.

Primo esempio:

(define animazioni
  '(
    "   o\n  /|\\\n  / \\"       ; frame 1
    "   o\n  \\|/\n  / \\"       ; frame 2
    "   o\n  _|_\n  / \\"))      ; frame 3

(define (mostra-animazione)
  (for (i 0 9)
    (print "\027[H\027[2J") ; pulisce lo schermo
    (println (animazioni (% i (length animazioni))))
    (sleep 200)
  ))

(mostra-animazione)

Secondo esempio:

(define animazioni
  '(
    "     ^\n    / \\\n   |---|\n   |   |\n    |_|"
    "    ^\n   / \\\n  |---|\n  |   |\n   |_| \n    *"
    "   ^\n  / \\\n |---|\n |   |\n  |_| \n   ***"
    "  ^\n / \\\n|---|\n|   |\n |_| \n  *****"
    " ^\n/ \\\n---|\n   |\n_|_ \n*******"))

(define (mostra-animazione)
  (for (i 0 20)
    (print "\027[H\027[2J") ; pulisce lo schermo
    (println (animazioni (% i (length animazioni))))
    (sleep 200)
  ))

(mostra-animazione)

Terzo esempio:

(define pista "____|     |____")
(define aereo "--o--")
(define righe-schermo 10)
(define riga-pista 9)
(define colonna-finale 5)
(define colonna-iniziale 14)

(define (stampa-frame riga-aereo colonna-aereo)
  (print "\027[H\027[2J") ; pulisci schermo
  (for (i 0 (- righe-schermo 1))
    (cond
      ((= i riga-aereo)
        (println (string (dup " " colonna-aereo) aereo)))
      ((= i riga-pista)
        (println pista))
      (true
        (println "")))
  )
  (sleep 250)
)

(define (animazione-atterraggio)
  (let ((passi (- colonna-iniziale colonna-finale)))
    (for (i 0 (- passi 1))
      (stampa-frame i (- colonna-iniziale i))
    )
    ; Ultimo frame: aereo integrato nella pista
    (print "\027[H\027[2J")
    (for (i 0 (- riga-pista 1)) (println))
    (println "____|--o--|____")
  )
  '>)

(animazione-atterraggio)

============================================================================

