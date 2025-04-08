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
  (sleep 400)
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


----------------------------------
Somma delle cifre di un fattoriale
----------------------------------

Dato un numero intero positivo N, scrivere la funzione più corta possibile per trovare la somma delle cifre del fattoriale di N.

Esempio:
N = 8
fatoriale(8) = 40320
Somma delle cifre = 4 + 0 + 3 + 2 + 0 = 9

Sequenza OEIS A004152:
Sum of digits of n!.
  1, 1, 2, 6, 6, 3, 9, 9, 9, 27, 27, 36, 27, 27, 45, 45, 63, 63, 54, 45,
  54, 63, 72, 99, 81, 72, 81, 108, 90, 126, 117, 135, 108, 144, 144, 144,
  171, 153, 108, 189, 189, 144, 189, 180, 216, 207, 216, 225, 234, 225,
  216, 198, 279, 279, 261, 279, 333, 270, 288, ...

Scriviamo prima una funzione che risolve il problema.

(define (fact-i num)
"Calculates the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

(define (digit-sum num)
"Calculates the sum of the digits of an integer"
  (let (out 0)
    (while (!= num 0)
      (setq out (+ out (% num 10)))
      (setq num (/ num 10))
    )
    out))

(define (sum-digit-fact N) (digit-sum (fact-i N)))

Proviamo:

(sum-digit-fact 8)
;-> 9

(map sum-digit-fact (sequence 1 20))
;-> (1 2 6 6 3 9 9 9 27 27 36 27 27 45 45 63 63 54 45 54)

(map sum-digit-fact (sequence 1 58))
;-> (1 2 6 6 3 9 9 9 27 27 36 27 27 45 45 63 63 54 45 54
;->  63 72 99 81 72 81 108 90 126 117 135 108 144 144 144
;->  171 153 108 189 189 144 189 180 216 207 216 225 234 225
;->  216 198 279 279 261 279 333 270 288)

Per scrivere una funzione più breve utilizziamo:

(apply * (map bigint (sequence 1 N)))
per calcolare il fattoriale di N.

Poi usiamo:

(apply + (map int (chop (explode (string (fattoriale N))))))
per calcolare la somma delle cifre del fattoriale di N.

Funzione compatta (89 caratteri):

(define(f N)(apply +(map int(chop(explode(string(apply *(map bigint(sequence 1 N)))))))))

(map f (sequence 1 20))
;-> (1 2 6 6 3 9 9 9 27 27 36 27 27 45 45 63 63 54 45 54)

(map f (sequence 1 58))
;-> (1 2 6 6 3 9 9 9 27 27 36 27 27 45 45 63 63 54 45 54
;->  63 72 99 81 72 81 108 90 126 117 135 108 144 144 144
;->  171 153 108 189 189 144 189 180 216 207 216 225 234 225
;->  216 198 279 279 261 279 333 270 288)


-------------------------
Disuguaglianze e poligoni
-------------------------

Le seguenti disuguaglianze:

  ax + by <= c
  dx + ey <= f
  gx + hy <= i
  jx + ky <= l

racchiudono una regione a forma di poligono convesso.
Note: per ipotesi le disuguaglianze (che rappresentano anche delle rette) non sono in parallelo tra loro.
Scrivere una funzione che, dati i valori a,b,c,d,e,f,g,h,i,j,k, determina i vertici che racchiudono il poligono convesso.

Esempio:
   2.20x − 4.1y  <= −0.87
   2.15x − 2.2y  <= −3.47
  −7.50x + 4y    <= 30
   1x    + 0.24y <= 0.47

Le coordinate dei vertici di questa regione sono approssimativamente:

(−0.91724 5.78017) (0.07408 1.64967) (−3.09761 −1.44994) (−5.4451 −2.70957)

Per calcolare tutti i vertici del poligono definito dalle quattro disuguaglianze lineari nel piano cartesiano, possiamo seguire questi passaggi:

1. Rappresentare le disuguaglianze come equazioni: 
Ogni disuguaglianza può essere scritta come un'equazione di una retta.

2. Calcolare le intersezioni tra le rette: 
Risolvere i sistemi di equazioni corrispondenti alle coppie di rette per trovare i punti di intersezione.

3. Verificare l'appartenenza dei punti alla regione definita dalle disuguaglianze:
Assicurarsi che ogni punto di intersezione soddisfi tutte le disuguaglianze.

4. Ordinare i vertici:
Disporre i punti in ordine orario o antiorario, formando il perimetro del poligono.

Funzione che calcola l'intersezione tra le rette.
Calcola l'intersezione tra due rette definite dalle equazioni ax + by = c e dx + ey = f.
Se le rette sono parallele (determinante zero), restituisce 'nil'.

(define (intersezione a b c d e f)
  (let ((deter (sub (mul a e) (mul b d))))
    (if (= deter 0)
        nil
        (letn (
               (x (div (sub (mul c e) (mul b f)) deter))
               (y (div (sub (mul a f) (mul c d)) deter))
               (x (if (= x 0) 0 x))
               (y (if (= y 0) 0 y)))
          (list x y)))))

(define epsilon 1e-10)  ; epsilon di errore

Funzione che verifica l'appartenenza di un punto al poligono:
Verifica se un punto (x, y) soddisfa tutte le disuguaglianze definite dalle rette.

(define (valido? x y dis)
  (for-all (fn (ineq)
           (<= (add (mul (ineq 0) x) (mul (ineq 1) y)) (add (ineq 2) epsilon)))
         dis))

Funzione che calcola i vertici del poligono convesso:
- Definisce le disuguaglianze come una lista di liste.
- Calcola tutte le possibili intersezioni tra le coppie di disuguaglianze.
- Verifica la validità di ogni punto di intersezione.
- Memorizza i vertici validi in una lista e la restituisce.

(define (calcola-vertici)
  (let (
        (disuguaglianze (list
                          (list a b c)
                          (list d e f)
                          (list g h i)
                          (list j k l)))
        (vertici '()))
        
    (for (i 0 2)
      (for (j (add i 1) 3)
        (letn (
               (eq1 (disuguaglianze i))
               (eq2 (disuguaglianze j))
               (p (intersezione (eq1 0) (eq1 1) (eq1 2)
                                (eq2 0) (eq2 1) (eq2 2))))
          (if (and p (valido? (p 0) (p 1) disuguaglianze) (not (ref p vertici)))
              (push p vertici -1)))))
    vertici))

Esempio 1:

  x <= 3
  y <= 3
  -x <= 0 --> x >= 0
  -y <= 0 --> y >= 0

(set 'a 1 'b 0 'c 3) 
(set 'd 0 'e 1 'f 3) 
(set 'g -1 'h 0 'i 0)
(set 'j 0 'k -1 'l 0)

(calcola-vertici)
;-> ((3 3) (3 0) (0 3) (0 0))

Esempio 2:

   2.20x − 4.1y  <= −0.87
   2.15x − 2.2y  <= −3.47
  −7.50x + 4y    <= 30
   1x    + 0.24y <= 0.47

(set 'a 2.2 'b −4.1 'c −0.87)
(set 'd 2.15 'e −2.2 'f −3.47)
(set 'g −7.5  'h 4 'i 30)
(set 'j 1 'k 0.24 'l 0.47)

(calcola-vertici)
;-> ((-3.097610062893083 -1.44993710691824)
;->  (-5.445102505694761 -2.709567198177677)
;->  (0.07407952871870399 1.649668630338733)
;->  (-0.9172413793103448 5.780172413793103))

Per finire ecco una funzione che ordina una lista di vertici nel senso antiorario attorno al baricentro (solo per poligoni convessi):
- Calcola il centroide cx, cy della nuvola di punti.
- Ordina i vertici usando atan2 per l'angolo rispetto al centro.
- Il confronto è <, quindi l'ordine è antiorario.

(define (ordina-antiorario vertici)
  (letn ( (n (length vertici))
          (cx (div (apply add (map first vertici)) n))
          (cy (div (apply add (map last vertici)) n)) )
    (sort vertici (fn (a b)
      (< (atan2 (sub (last a) cy) (sub (first a) cx))
         (atan2 (sub (last b) cy) (sub (first b) cx)))))))

(ordina-antiorario (calcola-vertici))
;-> ((-5.445102505694761 -2.709567198177677)
;->  (-3.097610062893083 -1.44993710691824)
;->  (0.07407952871870399 1.649668630338733)
;->  (-0.9172413793103448 5.780172413793103))


---------------------------
Quadrato magico di prodotti
---------------------------

La seguente matrice è un quadrato magico di prodotti:

   2 36  3
   9  6  4
  12  1 18

cioè, il prodotto di ogni riga, di ogni colonna e delle diagonali maggiori vale 216.

Prodotti righe:
   2 * 3 * 36 = 216
   9 * 6 *  4 = 216
  12 * 1 * 18 = 216

Prodotti colonne:
   2 * 9 * 12 = 216
  36 * 6 *  1 = 216
  12 * 1 * 18 = 216

Prodotti diagonali:
   2 * 6 * 18 = 216
   3 * 6 * 12 = 216

Come scrivere una funzione per determinare un quadrato magico di prodotti?

Il primo tentativo è quello di scrivere 9 cicli for innestati che vanno da 1 a N (pessima idea):

(define (magic1 n)
  (let ((result '()))
    (for (a 1 n)
      (for (b 1 n)
        (for (c 1 n)
          (for (d 1 n)
            (for (e 1 n)
              (for (f 1 n)
                (for (g 1 n)
                  (for (h 1 n)
                    (for (i 1 n)
                      (when (and (= (length (unique (list a b c d e f g h i))) 9)
                                 (= (* a b c) (* d e f) (* g h i) (* a d g) (* b e h) (* c f i) (* a e i) (* c e g)))
                        (println a { } b { } c)
                        (println d { } e { } f)
                        (println g { } h { } i)
                        (println)
                      )
                    ))))))))) '>))

Proviamo con N = 7 (cioè 10^7 iterazioni):

(time (magic1 7))
;-> 37519.203

Con N = 36 il programma effettuare 36^9 = 101559956668416 iterazioni, quindi questa funzione è inutilizzabile.

Invece di usare la frza bruta dobbiamo usare qualche criterio matematico per selezionare i numeri candidati.
Dopo diversi tentativi mi è venuto in mente di scegliere i numeri che hanno 9 divisori.

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

Calcoliamo i numeri fino a 1000 che hanno esattamente 9 divisori:

(define (divisori9 limite)
  (let (divisori '())
    (for (i 1 limite)
      (setq divisori (divisors i))
      (if (= (length divisori) 9)
          (println i { } divisori)))))

(divisori9 100000)
;-> 36 (1 2 3 4 6 9 12 18 36)
;-> 100 (1 2 4 5 10 20 25 50 100)
;-> 196 (1 2 4 7 14 28 49 98 196)
;-> 225 (1 3 5 9 15 25 45 75 225)
;-> 256 (1 2 4 8 16 32 64 128 256)
;-> 441 (1 3 7 9 21 49 63 147 441)
;-> 484 (1 2 4 11 22 44 121 242 484)
;-> 676 (1 2 4 13 26 52 169 338 676)

Adesso prendiamo una delle liste con 9 divisori (numeri candidati), calcoliamo tutte le permutazioni e verifichiamo se ogni permutazione è un quadrato magico di prodotti.

(define (perm lst)
"Generates all permutations without repeating from a list of items"
  (local (i indici out)
    (setq indici (dup 0 (length lst)))
    (setq i 0)
    ; aggiungiamo la lista iniziale alla soluzione
    (setq out (list lst))
    (while (< i (length lst))
      (if (< (indici i) i)
          (begin
            (if (zero? (% i 2))
              (swap (lst 0) (lst i))
              (swap (lst (indici i)) (lst i))
            )
            ;(println lst);
            (push lst out -1)
            (++ (indici i))
            (setq i 0)
          )
          (begin
            (setf (indici i) 0)
            (++ i)
          )
       )
    )
    out))
(magic2 '(1 2 4 157 314 628 24649 49298 98596))
Impostiamo le liste dei numeri candidati:

(setq nums1 '(1 2 3 4 6 9 12 18 36))
(setq nums2 '(1 2 3 4 6 8 12 16 24 48))
(setq nums3 '(1 2 4 5 10 20 25 50 100))
(setq nums4 '(1 2 4 7 14 28 49 98 196))
(setq nums5 '(1 3 5 9 15 25 45 75 225))

Funzione che verifica se una lista di numeri può essere un quadrato magico di prodotti:

(define (magic2 nums)
  (dolist (el (perm nums))
    (set 'a (el 0) 'b (el 1) 'c (el 2) 'd (el 3) 'e (el 4) 'f (el 5) 'g (el 6) 'h (el 7) 'i (el 8))
    (when (= (* a b c) (* d e f) (* g h i) (* a d g) (* b e h) (* c f i) (* a e i) (* c e g))
      (println a { } b { } c)
      (println d { } e { } f)
      (println g { } h { } i)
      (println)))'>)

Proviamo:

(time (magic2 nums1))
;-> 12 1 18
;-> 9 6 4
;-> 2 36 3
;-> ...
;-> 2 36 3
;-> 9 6 4
;-> 12 1 18
;-> 468.632

(magic2 nums2)
;-> nil

(magic2 nums3)
;-> 5 100 2
;-> 4 10 25
;-> 50 1 20
;-> ...
;-> 2 100 5
;-> 25 10 4
;-> 20 1 50

(magic2 nums4)
;-> 7 196 2
;-> 4 14 49
;-> 98 1 28
;-> ...
;-> 2 196 7
;-> 49 14 4
;-> 28 1 98

(magic2 nums5)
;-> 45 1 75
;-> 25 15 9
;-> 3 225 5
;-> ...
;-> 3 225 5
;-> 25 15 9
;-> 45 1 75

Divisori di 98596: (1 2 4 157 314 628 24649 49298 98596)

(magic2 '(1 2 4 157 314 628 24649 49298 98596))
;-> 157 98596 2
;-> 4 314 24649
;-> 49298 1 628
;-> ...
;-> 2 98596 157
;-> 24649 314 4
;-> 628 1 49298


---------------------------------
The Curious Case of Steve Ballmer
---------------------------------

https://codegolf.stackexchange.com/questions/123971/the-curious-case-of-steve-ballmer
Nota:
Tutto il contenuto dei siti di Stack Exchange è rilasciato sotto la licenza CC BY-SA 4.0 (Creative Commons Attribution-ShareAlike 4.0).

Scrivere la funzione più corta che stampa le seguenti 4 stringhe (senza i doppi apici):

"Steve Ballmer still does not know."
"Steve Ballmer still does not know what he did."
"Steve Ballmer still does not know what he did wrong."
"Steve Ballmer still does not know what he did wrong with mobile."

(define (f)
  (let (s "")
    (map (fn(x) (println (push x s -2))) '("Steve Ballmer still does not know." " what he did" " wrong" " with mobile"))'>))

(f)
;-> Steve Ballmer still does not know.
;-> Steve Ballmer still does not know what he did.
;-> Steve Ballmer still does not know what he did wrong.
;-> Steve Ballmer still does not know what he did wrong with mobile.

Versione compatta (132 caratteri):

(define(f)(let(s"")(map(fn(x)(println(push x s -2)))'("Steve Ballmer still does not know."" what he did"" wrong"" with mobile"))'>))

(f)
;-> Steve Ballmer still does not know.
;-> Steve Ballmer still does not know what he did.
;-> Steve Ballmer still does not know what he did wrong.
;-> Steve Ballmer still does not know what he did wrong with mobile.


-------------
Biscotti Oreo
-------------

Un biscotto Oreo è formato da è formato da due biscotti circolari a base di cacao e uno strato interno di crema.
Esempio di biscotto Oreo:

  ******    biscotto circolare
  ------    strato di crema
  ******    biscotto circolare

Supponiamo di avere una pila di N strati che si alternano tra biscotto circolare e strato di crema e in cui lo strato in fondo alla pila è sempre un biscotto circolare.

Quanti biscotti Oreo possiamo confezionare con N strati?
Quanti strati rimangono inutilizzati con N strati?

Esempio:
N = 2
  ------    strato di crema
  ******    biscotto circolare
Nessun biscotto Oreo
2 strati inutilizzati

Esempio:
N = 5
  ******
  ------
  ******
  ------
  ******
Un biscotto Oreo.
1 strato inutilizzato

Esempio:
N = 7
  ******
  ------
  ******
  ------
  ******
  ------
  ******
Due biscotti Oreo.
1 strato inutilizzato.

Possiamo fare un biscotto ogni quattro strati (3 strati + 1 in eccesso) tranne che il primo che usa solo 3 strati, per 3*int((N+1)/4 biscotti.

Quindi gli strati che rimangono sono:

  strati-rimasti = N - 3*int((N+1)/4)

  biscotti-confezionati = (N - strati-rimasti)/3

Sequenza OEIS A110657: 
a(n) = A028242(A028242(n)) (strati rimasti).
  0, 1, 2, 0, 1, 2, 3, 1, 2, 3, 4, 2, 3, 4, 5, 3, 4, 5, 6, 4, 5, 6, 7, 5,
  6, 7, 8, 6, 7, 8, 9, 7, 8, 9, 10, 8, 9, 10, 11, 9, 10, 11, 12, 10, 11,
  12, 13, 11, 12, 13, 14, 12, 13, 14, 15, 13, 14, 15, 16, 14, 15, 16, 17,
  15, 16, 17, 18, 16, 17, 18, 19, 17, 18, 19, 20, 18, 19, 20, 21, 19, 20,
  21, ...

(define (oreo N)
  (setq resto (- N (* 3 (int (/ (+ N 1) 4)))))
  (list (/ (- N resto) 3) resto))

Proviamo:

(map oreo (sequence 0 10))
;-> ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (2 4))

(map last (map oreo (sequence 0 50)))
;-> (0 1 2 0 1 2 3 1 2 3 4 2 3 4 5 3 4 5 6 4 5 6 7 5
;->  6 7 8 6 7 8 9 7 8 9 10 8 9 10 11 9 10 11 12 10 11
;->  12 13 11 12 13 14)

(map first (map oreo (sequence 0 50)))
;-> (0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6
;->  7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 12 12 12 12)


-----------------------------
The Midpoint Circle Algorithm
-----------------------------

The Midpoint Circle Algorithm è un algoritmo per disegnare un cerchio in una griglia con coordinate intere (pixel dello schermo).

Vedi "The Midpoint Circle Algorithm Explained Step by Step"
https://www.youtube.com/watch?v=hpiILbMkF9w

(define (draw-circle cx cy r)
  (let ( (pts '()) (x 0) (y (- r)) (p (- r)) )
    (while (< x (- y))
      (if (> p 0) 
        (begin (++ y) (setq p (+ p (* 2 (+ x y)) 1)))
        ;else
        (setq p (+ p (* 2 x) 1))
      )
      (push (list (+ cx x) (+ cy y)) pts -1)
      (push (list (- cx x) (+ cy y)) pts -1)
      (push (list (+ cx x) (- cy y)) pts -1)
      (push (list (- cx x) (- cy y)) pts -1)
      (push (list (+ cx y) (+ cy x)) pts -1)
      (push (list (+ cx y) (- cy x)) pts -1)
      (push (list (- cx y) (+ cy x)) pts -1)
      (push (list (- cx y) (- cy x)) pts -1)
      (++ x)
    )
  pts))

Proviamo:

(draw-circle 0 0 100)

(setq circle (draw-circle 100 100 100))

Conversione della lista di punti in immagine con ImageMagick:

(define (list-IM lst file-str)
"Creates a graphic file (RGBA 8 bit) from a list of coords (with ImageMagick)"
  (local (outfile x-width y-height line)
    ; remove duplicate pixel
    (setq lst (sort (unique lst)))
    ; open output file
    (setq outfile (open file-str "write"))
    ;(print outfile { }) ; debug
    ; Calculates image dimension (width and height)
    (setq x-width (add 1 (apply max (map (fn(c) (c 0)) lst))))
    (setq y-height (add 1 (apply max (map (fn(c) (c 1)) lst))))
    ; write file (ImageMagick format: rgba, 8bit)
    (write-line outfile (string "# ImageMagick pixel enumeration: "
                (string x-width) "," (string y-height) ",256,rgba"))
    (setq mode (length (lst 0)))
    (cond ((= mode 2) ; only coords
            (dolist (el lst)
              (setq line (string (string (el 0)) ", " (string (el 1))
                    ": (0,0,0,255)")) ; color black with alpha=100%
              (write-line outfile line)))
          ((= mode 5) ; coords and r g b
            (dolist (el lst)
              (setq line (string (string (el 0)) ", " (string (el 1)) ": ("
                    (string (el 1)) ", " (string (el 2)) ", " (string (el 3))
                    ", 255)")) ; color r g b with alpha=100%
              (write-line outfile line)))
          ((= mode 6) ; coords and r g b a
            (dolist (el lst)
              (setq line (string (string (el 0)) ", " (string (el 1)) ": ("
                    (string (el 1)) ", " (string (el 2)) ", " (string (el 3))
                    ", " (string (el 4)))) ; color r g b with alpha=100%
              (write-line outfile line)))
          (true (println "Error: only 2, 5 or 6 elements are allowed")))
    (close outfile)))

(list-IM circle "cerchio.txt")
;-> true

cerchio.txt
# ImageMagick pixel enumeration: 201,201,256,rgba
0, 90: (0,0,0,255)
0, 91: (0,0,0,255)
0, 92: (0,0,0,255)
...
200, 108: (0,0,0,255)
200, 109: (0,0,0,255)
200, 110: (0,0,0,255)

(exec "convert cerchio.txt -background white -flatten cerchio.png")

Vedi immagine "cerchio.png" nella cartella "data".

============================================================================

