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


-----------
Rubik ASCII
-----------

Scrivere una funzione per stampare la seguente figura (cubo di Rubik):
      _ _ _
    /_/_/_/\
   /_/_/_/\/\
  /_/_/_/\/\/\
  \_\_\_\/\/\/
   \_\_\_\/\/
    \_\_\_\/

Modifichiamo i caratteri '\' della figura con '\\'.

(define (rubik) (println
"      _ _ _
    /_/_/_/\\
   /_/_/_/\\/\\
  /_/_/_/\\/\\/\\
  \\_\\_\\_\\/\\/\\/
   \\_\\_\\_\\/\\/
    \\_\\_\\_\\/") '>)

(rubik)
;->      _ _ _
;->    /_/_/_/\
;->   /_/_/_/\/\
;->  /_/_/_/\/\/\
;->  \_\_\_\/\/\/
;->   \_\_\_\/\/
;->    \_\_\_\/


-----------------------------
Output lungo come la funzione
-----------------------------

Scrivere la funzione più corta possibile che restituisce un output lungo come la funzione (cioè con lo stesso numero di caratteri).
La funzione stessa non è valida come output (no quine).

Funzione 1
----------
Lunghezza funzione: 69 caratteri
(define (funzione) (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
(funzione)
;-> ERR: illegal parameter type : 2
;-> called from user function (funzione)
Lunghezza output: 69 caratteri (\r\n in windows)

Funzione 2
----------
Lunghezza funzione: 27 caratteri
(define(f) (sequence 0 11))
(f)
;-> (0 1 2 3 4 5 6 7 8 9 10 11)
Lunghezza output: 27 caratteri

Funzione 3
----------
Lunghezza funzione: 25 caratteri
(define (f) (dup "1" 23))
(f)
;-> "11111111111111111111111"
Lunghezza output: 25 caratteri

Funzione 4
----------
Lunghezza funzione: 15 caratteri
(define(f)1e14)
(f)
;-> 100000000000000
Lunghezza output: 15 caratteri


---------------
Numeri in scala
---------------

Dati due numeri interi positivi 'a' e 'b' con a < b, scrivere una funzione che stampa i numeri in scala e in sequenza.

Esempio:
a = 1, b = 5
Output:
1
 2
  3
   4
    5
Esempio:
a = 98, b = 102
Output:
98
  99
    100
       101
          102

(define (scale a b)
  (let (space "")
    (for (x a b)
      (println space x)
      (extend space (dup " " (length x))))'>))

Proviamo:

(scale 1 5)
;-> 1
;->  2
;->   3
;->    4
;->     5

(scale 98 102)
;-> 98
;->   99
;->     100
;->        101
;->           102

(scale 998 1005)
;-> 998
;->    999
;->       1000
;->           1001
;->               1002
;->                   1003
;->                       1004
;->                           1005


--------------------------------------
Bresenham per Linee, Cerchi ed Ellissi
--------------------------------------

Al seguente indirizzo web si trovano funzioni in C per la rasterizzazione di alcune primitive grafiche:

https://zingl.github.io/bresenham.html

Le funzioni sono tutte basate sull'algoritmo di Bresenham.

Riportiamo il codice C per il disegno di Linee, Cerchi ed Ellissi e la relativa conversione in newLISP.

LINEE
-----

void plotLine(int x0, int y0, int x1, int y1)
{
   int dx =  abs(x1-x0), sx = x0<x1 ? 1 : -1;
   int dy = -abs(y1-y0), sy = y0<y1 ? 1 : -1;
   int err = dx+dy, e2; /* error value e_xy */

   for(;;){  /* loop */
      setPixel(x0,y0);
      if (x0==x1 && y0==y1) break;
      e2 = 2*err;
      if (e2 >= dy) { err += dy; x0 += sx; } /* e_xy+e_x > 0 */
      if (e2 <= dx) { err += dx; y0 += sy; } /* e_xy+e_y < 0 */
   }
}

La funzione 'plotLine' disegna una linea tra due punti '(x0, y0)' e '(x1, y1)' usando una versione dell'algoritmo di Bresenham per linee, ma estesa per gestire tutte le direzioni e inclinazioni, non solo quelle a pendenza ≤ 1.
Questa funzione disegna una linea rasterizzata da '(x0, y0)' a '(x1, y1)' con un'ottima approssimazione visiva, senza usare float, divisioni o radici quadrate. È molto efficiente e precisa anche per linee diagonali, verticali, orizzontali o qualsiasi pendenza.

int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
int dy = -abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
int err = dx + dy, e2;

- 'dx': distanza orizzontale tra i due punti.
- 'dy': distanza verticale negata tra i due punti (questo è importante per il calcolo dell'errore).
- 'sx', 'sy': direzioni di avanzamento su x e y (possono essere '1' o '-1' a seconda di dove si trova il punto finale).
- 'err = dx + dy': valore d'errore iniziale.

for(;;) {
   setPixel(x0, y0);
   if (x0 == x1 && y0 == y1) break;
   e2 = 2 * err;
   if (e2 >= dy) { err += dy; x0 += sx; }
   if (e2 <= dx) { err += dx; y0 += sy; }
}

- Disegna il pixel corrente con 'setPixel(x0, y0)'.
- Se il punto corrente è il punto finale, esce dal ciclo.
- Calcola 'e2 = 2 * err' per confrontare separatamente gli errori lungo x e y:
  - Se 'e2 >= dy', si avanza in 'x' (cioè si fa uno step orizzontale).
  - Se 'e2 <= dx', si avanza in 'y' (cioè si fa uno step verticale).
- Così la linea risulta sempre continua, evitando buchi o salti.

Versione in newLISP:

Funzione che genera i punti di una linea tra due punti '(x0, y0)' e '(x1, y1)' con l'algoritmo di Bresenham:

(define (line x0 y0 x1 y1)
  (letn ( (dx (abs (- x1 x0)))
          (sx (if (< x0 x1) 1 -1))
          (dy (- (abs (- y1 y0))))
          (sy (if (< y0 y1) 1 -1))
          (err (+ dx dy))
          (e2 0) (done nil) (pts '()) )
    (until done
      (push (list x0 y0) pts -1)
      ; termina il ciclo se siamo arrivati al punto finale
      (cond ((and (= x0 x1) (= y0 y1)) (setq done true))
            (true ; altrimenti calcola il prossimo punto
              (setq e2 (* 2 err))
              (when (>= e2 dy) (setq err (+ err dy)) (setq x0 (+ x0 sx)))
              (when (<= e2 dx) (setq err (+ err dx)) (setq y0 (+ y0 sy))))
      )
    )
    pts))

Proviamo:

(line 1 1 10 6)
;-> ((1 1) (2 2) (3 2) (4 3) (5 3) (6 4) (7 4) (8 5) (9 5) (10 6))

(line -4 5 10 -12)
;-> ((-4 5) (-3 4) (-2 3) (-2 2) (-1 1) (0 0) (1 -1) (2 -2) (3 -3) (3 -4) (4 -5) (5 -6)
;->  (6 -7) (7 -8) (8 -9) (8 -10) (9 -11) (10 -12))

(line 0 0 0 0)
;-> ((0 0))

(line 0 0 0 5)
;-> ((0 0) (0 1) (0 2) (0 3) (0 4) (0 5))

CERCHI
------

void plotCircle(int xm, int ym, int r)
{
   int x = -r, y = 0, err = 2-2*r; /* II. Quadrant */
   do {
      setPixel(xm-x, ym+y); /*   I. Quadrant */
      setPixel(xm-y, ym-x); /*  II. Quadrant */
      setPixel(xm+x, ym-y); /* III. Quadrant */
      setPixel(xm+y, ym+x); /*  IV. Quadrant */
      r = err;
      if (r <= y) err += ++y*2+1;           /* e_xy+e_y < 0 */
      if (r > x || err > y) err += ++x*2+1; /* e_xy+e_x > 0 or no 2nd y-step */
   } while (x < 0);
}

La funzione 'plotCircle' disegna un cerchio centrato in '(xm, ym)' con raggio 'r' utilizzando un algoritmo senza moltiplicazioni né radici quadrate, ottimizzato per grafica raster (tipico nei disegni su schermo pixel per pixel). Questo è una variante dell'algoritmo di Bresenham per cerchi (o Midpoint Circle Algorithm).

void plotCircle(int xm, int ym, int r)

- 'xm, ym': coordinate del centro del cerchio.
- 'r': raggio del cerchio.
- Non restituisce nulla ('void').
- Usa una funzione 'setPixel(x, y)' (non definita nel codice) per disegnare un pixel sullo schermo.

int x = -r, y = 0, err = 2 - 2 * r;

- Inizializza 'x = -r' per partire dalla sinistra estrema del cerchio.
- 'y = 0': parte da y = 0, quindi dal centro orizzontale.
- 'err' è una variabile che tiene traccia dell'errore dell'approssimazione del cerchio perfetto.

do {
   setPixel(xm-x, ym+y); // I quadrante
   setPixel(xm-y, ym-x); // II quadrante
   setPixel(xm+x, ym-y); // III quadrante
   setPixel(xm+y, ym+x); // IV quadrante

- Dato un punto '(x, y)' calcolato nel secondo quadrante (x negativo), calcola simmetricamente i pixel negli altri quadranti.
- In pratica, disegna otto punti per ogni iterazione, grazie alla simmetria del cerchio.

r = err;
if (r <= y) err += ++y*2+1;
if (r > x || err > y) err += ++x*2+1;

- Varia le coordinate 'x' e 'y' in modo tale da seguire il bordo del cerchio, correggendo l'errore man mano che ci si muove.
- Il primo 'if' incrementa 'y' e aggiorna l'errore.
- Il secondo 'if' incrementa 'x' (che è negativo e tende verso zero), gestendo quando serve fare uno "step diagonale".

} while (x < 0);

- Il ciclo termina quando 'x' raggiunge 0, cioè quando si è completato il disegno di mezzo cerchio (l'altra metà è simmetrica).

In sintesi questa funzione:
- Usa simmetria per ridurre il numero di calcoli.
- È veloce perché usa solo somme, sottrazioni e shift.
- Disegna un cerchio di raggio 'r' centrato in '(xm, ym)' usando il rasterizzazione per pixel.

(define (circle xc yc r)
  (let ( (x (- 0 r)) (y 0) (err (- 2 (* 2 r))) (pts '()) )
    (do-while (< x 0)
      (push (list (- xc x) (+ yc y)) pts -1) ; I quadrante
      (push (list (- xc y) (- yc x)) pts -1) ; II quadrante
      (push (list (+ xc x) (- yc y)) pts -1) ; III quadrante
      (push (list (+ xc y) (+ yc x)) pts -1) ; IV quadrante
      (setq r err)
      (if (<= r y) (setq err (+ err (+ (* 2 (++ y)) 1))))
      (if (or (> r x) (> err y)) (setq err (+ err (+ (* 2 (++ x)) 1))))
    )
    pts))

Proviamo:

(circle 0 0 5)
;-> ((5 0) (0 5) (-5 0) (0 -5) (5 1) (-1 5) (-5 -1) (1 -5) (5 2) (-2 5)
;->  (-5 -2) (2 -5) (4 3) (-3 4) (-4 -3) (3 -4) (3 4) (-4 3) (-3 -4) (4 -3)
;->  (2 5) (-5 2) (-2 -5) (5 -2) (1 5) (-5 1) (-1 -5) (5 -1))

ELLISSI
-------

void plotEllipseRect(int x0, int y0, int x1, int y1)
{
   int a = abs(x1-x0), b = abs(y1-y0), b1 = b&1; /* values of diameter */
   long dx = 4*(1-a)*b*b, dy = 4*(b1+1)*a*a; /* error increment */
   long err = dx+dy+b1*a*a, e2; /* error of 1.step */

   if (x0 > x1) { x0 = x1; x1 += a; } /* if called with swapped points */
   if (y0 > y1) y0 = y1; /* .. exchange them */
   y0 += (b+1)/2; y1 = y0-b1;   /* starting pixel */
   a *= 8*a; b1 = 8*b*b;

   do {
       setPixel(x1, y0); /*   I. Quadrant */
       setPixel(x0, y0); /*  II. Quadrant */
       setPixel(x0, y1); /* III. Quadrant */
       setPixel(x1, y1); /*  IV. Quadrant */
       e2 = 2*err;
       if (e2 <= dy) { y0++; y1--; err += dy += a; }  /* y step */
       if (e2 >= dx || 2*err > dy) { x0++; x1--; err += dx += b1; } /* x step */
   } while (x0 <= x1);

   while (y0-y1 < b) {  /* too early stop of flat ellipses a=1 */
       setPixel(x0-1, y0); /* -> finish tip of ellipse */
       setPixel(x1+1, y0++);
       setPixel(x0-1, y1);
       setPixel(x1+1, y1--);
   }
}

Note:
1) 'b1 = b & 1' è equivalente a 'b1 = (b % 2)' (b è pari o dispari?).
2) 'err += dy += a' è una forma compatta (e un po' criptica) di scrittura in C, che usa assegnazioni concatenate.
In pratica effettua le seguenti operazoni:
dy += a;        // prima: aggiorna dy
err += dy;      // poi: aggiorna err usando il nuovo valore di dy

La funzione 'plotEllipseRect' implementa una versione ottimizzata dell'algoritmo di Bresenham per il disegno di ellissi, noto anche come algoritmo midpoint per ellissi.

- È una variante dell'algoritmo di Bresenham per il disegno di cerchi ed ellissi.
- Utilizza solo interi (niente funzioni trigonometriche o float), rendendolo molto efficiente.
- Divide il disegno in quattro quadranti e sfrutta la simmetria dell'ellisse per disegnare solo un quarto e replicare i punti negli altri tre.
- Usa errori incrementali ('err', 'dx', 'dy') per decidere quando muoversi lungo l'asse x o y, mantenendo l'ellisse il più vicino possibile alla forma ideale.

- La funzione prende due punti '(x0, y0)' e '(x1, y1)' che rappresentano i vertici opposti del rettangolo che contiene l'ellisse.
- Calcola i semiassi 'a' (larghezza) e 'b' (altezza).
- I valori 'dx', 'dy', e 'err' sono utilizzati per determinare la direzione e la quantità dell'errore accumulato nel disegno.
- La parte finale del ciclo 'while (y0-y1 < b)' serve a correggere la forma nel caso di ellissi molto piatte (dove 'a = 1'), dove il ciclo principale può terminare troppo presto.

Versione newLISP:

(define (ellipse x0 y0 x1 y1)
  (local (a b b1 dx dy err e2 pts)
    (setq pts '())
    (setq a (abs (- x1 x0)))  ; semi-asse orizzontale
    (setq b (abs (- y1 y0)))  ; semi-asse verticale
    (setq b1 (% b 2))         ; b è dispari?
    (setq dx (* 4 (- 1 a) (* b b)))  ; incremento errore x
    (setq dy (* 4 (+ b1 1) (* a a))) ; incremento errore y
    (setq err (+ dx dy (* b1 (* a a))))  ; errore iniziale
    (setq e2 0)
    (when (> x0 x1) (setq x0 x1) (setq x1 (+ x1 a)))  ; se i punti sono scambiati
    (if (> y0 y1) (setq y0 y1))  ; scambio y
    (setq y0 (+ y0 (/ (+ b 1) 2)))
    (setq y1 (- y0 b1))  ; pixel iniziale
    (setq a (* 8 a a))
    (setq b1 (* 8 b b))
    (do-while (<= x0 x1)
      (push (list x1 y0) pts -1)  ; I Quadrante
      (push (list x0 y0) pts -1)  ; II Quadrante
      (push (list x0 y1) pts -1)  ; III Quadrante
      (push (list x1 y1) pts -1)  ; IV Quadrante
      (setq e2 (* 2 err))
      (if (<= e2 dy)
          (begin (setq y0 (+ y0 1)) (setq y1 (- y1 1))  (setq dy (+ dy a)) (setq err (+ err dy)))) ; step su y
      (if (or (>= e2 dx) (> (* 2 err) dy))
          (begin (setq x0 (+ x0 1)) (setq x1 (- x1 1)) (setq dx (+ dx b1)) (setq err (+ err dx))))
    )
    (while (< (- y0 y1) b) ; arresto troppo anticipato delle ellissi piatte a=1
      (push (list (- x0 1) y0) pts -1)   ; completamento della punta dell'ellisse
      (push (list (+ x1 1) (++ y0)) pts -1)
      (push (list (- x0 1) y1) pts -1)
      (push (list (+ x1 1) (-- y1)) pts -1)
    )
  pts))

Proviamo:

(ellipse 30 30 70 50)
;-> ((12 4) (0 4) (0 4) (12 4) (12 5) (0 5) (0 3) (12 3) (12 6) (0 6) (0 2)
;->  (12 2) (11 7) (1 7) (1 1) (11 1) (11 8) (1 8) (1 0) (11 0) (11 9) (1 9)
;->  (1 -1) (11 -1) (11 10) (1 10) (1 -2) (11 -2) (10 11) (2 11) (2 -3) (10 -3)
;->  (10 12) (2 12) (2 -4) (10 -4) (9 13) (3 13) (3 -5) (9 -5) (8 14) (4 14)
;->  (4 -6) (8 -6) (7 15) (5 15) (5 -7) (7 -7) (6 16) (6 16) (6 -8) (6 -8))

Proviamo a generare alcune figure in un unica immagine:
(Per la funzione "list-IM" vedi la libreria "yo.lsp".)

(setq linea1 (line 10 10 200 200))
(setq linea2 (line 150 150 25 25))
(setq quad (append (line 10 250 50 250) (line 50 250 50 350)
                   (line 50 350 10 350) (line 10 350 10 250)))
(setq cerchio (circle 100 100 100))
(setq ellisse (ellipse 80 50 250 100))
(setq limiti '((0 0) (400 0) (400 400) (0 400)))

(setq draw (append limiti linea1 linea2 quad cerchio ellisse))
(list-IM draw "draw.txt")
;-> true
(exec "convert draw.txt -background white -flatten draw.png")
;-> ()

L'immagine "draw.png" si trova nella cartella "data".

Vedi anche "Algoritmo di Bresenham" su "Note libere 4".


----------------------------------
Avanti e indietro in un intervallo
----------------------------------

Consideriamo i numeri interi in un intervallo [a,b] con (b > a >= 0) e una stringa binaria.
Scrivere una funzione per simulare il seguente processo:
1. Partire da un numero predefinito dell'intervallo
2. Attraversare tutta la stringa:
   se incontriamo "0" allora ci spostiamo a sinistra dell'intervallo (-1)
   se incontriamo "1" allora ci spostiamo a destra dell'intervallo (+1)
   Comunque la posizione deve sempre rimanere nell'intervallo:
   (cioè +1 da 'b' rimane 'b' e -1 da 'a' rimane 'a').
3. Restituire la posizione finale

(define (left-right a b bin x)
  (let (pos (or x a))
    (dostring (c bin)
      (if (= c 48) ; (char "0") = 48
        (setq pos (if (= pos a) a (- pos 1)))
        (setq pos (if (= pos b) b (+ pos 1)))
      )
      ;(print c { } pos) (read-line)
    )
    pos))

Proviamo:

(left-right 0 5 "101010111111100000000011")
;-> 2

(left-right 100 105 "101010111111100000000011" 105)
;-> 102

(left-right 100 110 "101010111111100000000011" 105)
;-> 103


---------------------------------
Tante volte quanto vale il numero
---------------------------------

Problema 1
----------
Dato un intero positivo N, scrivere una funzione che restituisce una lista che contiene ogni numero intero da 1 a N ripetuto x volte, dove x è il valore di ciascun numero intero.
Per esempio, con N = 5 la funzione deve restituire la seguente lista:
(1 2 2 3 3 3 4 4 4 4 5 5 5 5 5)
Cioè, 1 per una volta, 2 per due volte, 3 per tre volte, 4 per quattro volte e 5 per cinque volte.

Sequenza OEIS A002024:
k appears k times. a(n) = floor(sqrt(2n) + 1/2).
  1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6,
  7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
  11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
  12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, ...

In questo caso il parametro 'n' della nostra funzione rappresenta il numero finale della sequenza (per esempio, n = 3 produce la sequenza 1 2 2 3 3 3).

Somma dei primi N numeri naturali: (N+1)*N/2

(define (seq1 n)
  (let (out '())
    ; Quantità di numeri da generare
    (setq n (/ (* (+ n 1) n) 2))
    ; Generazione dei numeri con la formula: a(n) = floor(sqrt(2n) + 1/2)
    (for (i 1 n)
      (push (floor (add (sqrt (mul 2 i)) 0.5)) out -1))
    out))

Proviamo:

(seq1 3)
;-> (1 2 2 3 3 3)

(seq1 5)
;-> (1 2 2 3 3 3 4 4 4 4 5 5 5 5 5)

Problema 2
----------
Dato un intero positivo N, scrivere una funzione che restituisce una lista che contiene ogni numero intero da 1 a N ripetuto x volte, dove x è il valore di ciascun numero intero.
Inoltre, la lista di output non deve avere due interi adiacenti uguali.

(define (seq2 n)
  (let (x '())
    (for (i n 1 -1)
      (setq x (append x (sequence i n))))))

Proviamo:

(seq2 3)
;-> (3 2 3 1 2 3)

(seq2 5)
(5 4 5 3 4 5 2 3 4 5 1 2 3 4 5)

Verifichiamo che i numeri sono gli stessi:

(= (seq1 10) (sort (seq2 10)))
;-> true

Come funziona?
Facciamo un esempio con n=5:
La sequenza OEIS (generata da (seq1 5)) vale:
(1 2 2 3 3 3 4 4 4 4 5 5 5 5 5)

Partiamo da una lista vuota:
(setq x '())

Estendiamo la lista vuota con l'espressione (setq (append x (sequence i n)) all'interno di un ciclo for con il contatore 'i' che va da n a 1:
(setq n 5)
(setq i 5)
;(setq (append x (sequence i n))
(setq (append x (sequence 5 5)) ;(sequence 5 5) --> (5)
;-> (5)
(setq i 4)
(setq (append x (sequence 4 5)) ;(sequence 4 5) --> (4 5)
;-> (5 4 5)
(setq i 3)
(setq (append x (sequence 3 5)) ;(sequence 3 5) --> (3 4 5)
;-> (5 4 5 3 4 5)
(setq i 2)
(setq (append x (sequence 2 5)) ;(sequence 2 5) --> (2 3 4 5)
;-> (5 4 5 3 4 5 2 3 4 5)
(setq i 1)
(setq (append x (sequence 1 5)) ;(sequence 1 5) --> (1 2 3 4 5)
;-> (5 4 5 3 4 5 2 3 4 5 1 2 3 4 5)


------------
cowsay e tux
------------

'cowsay' è un programma, scritto in Perl da Tony Monroe, che genera immagini di una mucca (ASCII art) con un messaggio (passato come parametro).
Al posto di una mucca è possibile generare il pinguino Tux (la mascotte di Linux).

Il comando ha diversi parametri opzionali che trascureremo nella nostra funzione.
Inoltre, per semplicità, il messaggio è una stringa di una sola linea (non multilinea).

Esempi:

comando: cowsay "Messaggio dalla mucca!"
output:
 ________________________
< Messaggio dalla mucca! >
 ------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

comando: cowsay "Messaggio dal pinguino!" true
 _________________________
< Messaggio dal pinguino! >
 -------------------------
   \
    \
        .--.
       |o_o |
       |:_/ |
      //   \ \
     (|     | )
    /'\_   _/`\
    \___)=(___/

Modifichiamo i caratteri '\' della mucca e del pinguino con '\\'.

(define (cowsay str fig)
  (let (stra "")
    (setq text (string " " (dup "_" (+ (length str) 2)) "\n"
                      "< " str " >" "\n"
                      " " (dup "-" (+ (length str) 2)) "\n"))
    (print text)
    (cond
      ((true? fig) ; pinguino
        (println "   \\")
        (println "    \\")
        (println "        .--.")
        (println "       |o_o |")
        (println "       |:_/ |")
        (println "      //   \\ \\")
        (println "     (|     | )")
        (println "    /'\\_   _/`\\")
        (println "    \\___)=(___/"))
      ; mucca
      (true
        (println "        \\   ^__^")
        (println "         \\  (oo)\\_______")
        (println "            (__)\\       )\\/\\")
        (println "                ||----w |")
        (println "                ||     ||"))) '>))

(cowsay "Buongiorno a te...")
;->  ____________________
;-> < Buongiorno a te... >
;->  --------------------
;->         \   ^__^
;->          \  (oo)\_______
;->             (__)\       )\/\
;->                 ||----w |
;->                 ||     ||

(cowsay "Buonanotte a me." true)
;->  __________________
;-> < Buonanotte a me. >
;->  ------------------
;->    \
;->     \
;->         .--.
;->        |o_o |
;->        |:_/ |
;->       //   \ \
;->      (|     | )
;->     /'\_   _/`\
;->     \___)=(___/


-------------------------------------------------
Calcolo di pi greco (Formula di Rabinowitz-Wagon)
-------------------------------------------------

Nel 1995, Stanley Rabinowitz e Stan Wagon scoprirono un algoritmo per generare le cifre di pi greco una alla volta senza memorizzare i risultati precedenti.

La formula generale utilizzata è la seguente:

                     1        2        3            k
  pi greco = 2*(1 + ---*(1 + ---*(1 + ---*(1 + ...------*(1+ ...))))
                     3        5        7           2k+1

pi = 3.14159265358979323846264338327950288419716939937510582097494459230781...

Implementazione della formula:

(define (calcola-pi k)
  (let (pi (add 1.0 (div k (add (mul 2 k) 1.0)))) ; base: (1 + k/(2k+1))
    (for (i (- k 1) 1 -1)
      (setq pi (add 1.0 (mul (div i (add (mul 2 i) 1.0)) pi))))
    (mul 2 pi)))

Proviamo:

(map (fn(x) (list x (calcola-pi x))) '(1 5 10 20 30 40 50 60))
;-> ((1 2.666666666666667)
;->  (5 3.121500721500722)
;->  (10 3.141106021601377)
;->  (20 3.141592298740339)
;->  (30 3.14159265330116)
;->  (40 3.141592653589546)
;->  (50 3.141592653589793)
;->  (60 3.141592653589793))

Vedi anche "Calcolo di Pi greco" su "Rosetta code".
Vedi anche "Calcolo di pi greco con il metodo spigot" su "Note libere 24".


-----------------------
Concatenare n con n + 1
-----------------------

Consideriamo la sequenza di numeri la cui espansione decimale è una concatenazione di 2 numeri consecutivi crescenti non negativi.
In altre parole, ogni numero nella sequenza è formato mettendo insieme n con n+1.

Sequenza OEIS A127421:
Numbers whose decimal expansion is a concatenation of 2 consecutive increasing nonnegative numbers.
  1, 12, 23, 34, 45, 56, 67, 78, 89, 910, 1011, 1112, 1213, 1314, 1415,
  1516, 1617, 1718, 1819, 1920, 2021, 2122, 2223, 2324, 2425, 2526, 2627,
  2728, 2829, 2930, 3031, 3132, 3233, 3334, 3435, 3536, 3637, 3738, 3839,
  3940, 4041, 4142, 4243, 4344, 4445, 4546, ...

Scrivere una funzione che calcola la sequenza fino ad determinato numero di elementi.

(define (concat n) (int (string n (+ n 1))))

(define (concatena num)
  (let ( (out '()) (idx 0) )
  (until (>= idx num)
    (push (concat idx) out -1)
    (++ idx))
    out))

(concatena 46)
;-> (1 12 23 34 45 56 67 78 89 910 1011 1112 1213 1314 1415 1516 1617 1718
;->  1819 1920 2021 2122 2223 2324 2425 2526 2627 2728 2829 2930 3031 3132
;->  3233 3334 3435 3536 3637 3738 3839 3940 4041 4142 4243 4344 4445 4546)

Funzione compatta (60 caratteri):

(define(c n)(map(fn(x)(int(string(- x 1)x)))(sequence 1 n)))

(c 46)
;-> (1 12 23 34 45 56 67 78 89 910 1011 1112 1213 1314 1415 1516 1617 1718
;->  1819 1920 2021 2122 2223 2324 2425 2526 2627 2728 2829 2930 3031 3132
;->  3233 3334 3435 3536 3637 3738 3839 3940 4041 4142 4243 4344 4445 4546)


----------------------
Teorema di Vantieghems
----------------------

Il teorema di Vantieghem è una condizione necessaria e sufficiente affinché un numero sia primo.
Afferma che, affinché un numero naturale n sia primo, il prodotto di (2^i - 1), dove 0 < i < n, è congruente a n mod (2^n - 1).
In altre parole, un numero n è primo se e solo se:

  Prod[i=1..(n-1)](2^i - 1) congruente n mod (2^n - 1)

Un altro modo per enunciare il teorema di cui sopra è:

se (2^n - 1) divide Prod[i=1..(n-1)](2^i - 1) - n, allora n è primo.

(define (product n) ; funzione non usata
  (let (p 1)
  (for (i 1 (- n 1))
    (setq p (* p (- (<< 1 i) 1))))
  p))

(define (vantieghems limite)
  (cond ((< limite 2) '())
        ((= limite 2) '(2))
        (true
          (let ( (prod 3L) (out '(2)) )
            (for (n 3 (- limite 1))
              ;(println "n: " n { --- } "prod: " prod)
              ; verifica la condizione di primalità
              (if (zero? (% (- prod n) (- (pow 2 n) 1)))
                  (push n out -1))
              ; aggiorna la produttoria
              (setq prod (* prod (- (pow 2 n) 1)))
            )
            (println "Cifre produttoria = " (length prod))
            out))))

Proviamo:

(vantieghems 20)
;-> Cifre produttoria = 57
;-> (2 3 5 7 11 13 17 19)

(vantieghems 100)
;-> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61) ; output errato
;-> Cifre produttoria = 1290

Modi di calcolare 2^n:
  +-----------+----------+-------------+
  | Modo      | Velocità | Numeri      |
  +-----------+----------+-------------+
  | (<< 1 n)  | veloce   | int64       |
  |------------------------------------|
  | (pow 2 n) | normale  | float       |
  |------------------------------------|
  | (** 2 n)  | lenta    | big integer |
  +------------------------------------+

Proviamo con i big-integer.

(define (** num power)
"Calculates the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (vantieghems limite)
  (cond ((< limite 2) '())
        ((= limite 2) '(2))
        (true
          (let ( (prod 3L) (out '(2)) )
            (for (n 3 (- limite 1))
              ;(println "n: " n { --- } "prod: " prod)
              ; verifica la condizione di primalità
              (if (zero? (% (- prod n) (- (** 2 n) 1)))
              ;(if (zero? (% (- prod n) (- (<< 1 n) 1)))
              ;(if (zero? (% (- prod n) (- (pow 2 n) 1)))
                  (push n out -1))
              ; aggiorna la produttoria
              (setq prod (* prod (- (** 2 n) 1)))
              ;(setq prod (* prod (- (<< 1 n) 1)))
              ;(setq prod (* prod (- (pow 2 n) 1)))
            )
            (println "Cifre produttoria = " (length prod))
            out))))

(vantieghems 20)
;-> Cifre produttoria = 57
;-> (2 3 5 7 11 13 17 19)

(vantieghems 100)
;-> Cifre produttoria = 1490
;-> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)

(time (vantieghems 1000))
;-> Cifre produttoria = 150364
;-> 1786.247

Il teorema ha valore teorico, ma in pratica non è utilizzabile per calcolare i numeri primi.


--------------------------
Teorema di Hardy-Ramanujan
--------------------------

Il teorema di Hardy Ramanujan afferma che il numero di fattori primi distinti di n sarà approssimativamente log(log(n)) per la maggior parte dei numeri naturali n.
Nota: 'log' è il logaritmo naturale.

Esempi:

Numero: 5192
Fattori primi: 2 2 2 11 59
Fattori primi distinti: 11 59
Numero di fattori primi distinti: 2
(log (log 5192)) = 2.146501209209714

Numero: 51242183
Fattori primi: 19 23 117259
Fattori primi distinti: 19 23 117259
Numero di fattori primi distinti: 3
(log (log 51242183)) = 2.876502333746978

Funzione che calcola log(log(n) di un numero:

(define (HR n) (log (log n)))

(define (once lst)
"Return the elements of a list that appear only once"
  (local (out unici all)
    (setq out '())
    (setq unici (unique lst))
    ; conta le occorrenze di ogni elemento
    (setq all (map list unici (count unici lst)))
    ; filtra gli elementi che compaiono solo una volta (occorrenza = 1)
    (dolist (el all)
      (if (= (el 1) 1) (push (el 0) out -1))
    )
    out))

Funzione che calcola il numero esatto di fattori primi distinti di un numero:

(define (num-distinct-factor n) (length (once (factor n))))

Proviamo:

(num-distinct-factor 5192)
;-> 2
(HR 5192)
;-> 2.146501209209714

(num-distinct-factor 51242183)
;-> 3
(HR 51242183)
;-> 2.876502333746978

Per alcuni numeri i risutati sono molto differenti:

(num-distinct-factor 30030)
;-> 6
(HR 30030)
;-> 2.333109657957713

(num-distinct-factor 510510)
;-> 7
(HR 510510)
;-> 2.575901890058003

Vediamo per quali numeri abbiamo il massimo e il minimo scostamento tra il valore reale e il valore di Hardy-Ramanujan dei fattori primi distinti:

(define (test limite)
  (let ((out '()))
    (for (n 2 limite)
      (push (list (abs (sub (num-distinct-factor n) (HR n))) n) out -1)
    )
    (sort out)
    (println "Errore minimo: " (out 0 0) " per il Numero: " (out 0 1))
    (println "Errore massimo: " (out -1 0) " per il Numero: " (out -1 1))))

(time (test 1e7))
;-> Errore minimo: 1.488716955821978e-005 per il Numero: 1618
;-> Errore massimo: 5.221950932354477 per il Numero: 9699690
;-> 52094.829


-----------------------
Numeri Belli (Beatiful)
-----------------------

Un numero intero positivo si dice "Bello" (Beatiful) se il prodotto delle sue cifre è divisibile per la somma delle sue cifre.

Sequenza OEIS A038367:
Numbers n with property that (product of digits of n) is divisible by (sum of digits of n).
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 22, 30, 36, 40, 44, 50, 60, 63, 66,
  70, 80, 88, 90, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
  120, 123, 130, 132, 138, 140, 145, 150, 154, 159, 160, 167, 170, 176,
  180, 183, 189, 190, 195, 198, 200, 201, 202, 203, ...

(define (digit-prod num)
"Calculates the product of the digits of an integer"
  (if (zero? num)
      0
      (let (out 1)
        (while (!= num 0)
          (setq out (* out (% num 10)))
          (setq num (/ num 10))
        )
    out)))

(define (digit-sum num)
"Calculates the sum of the digits of an integer"
  (let (out 0)
    (while (!= num 0)
      (setq out (+ out (% num 10)))
      (setq num (/ num 10))
    )
    out))

(define (bello? num) (zero? (% (digit-prod num) (digit-sum num))))

(filter bello? (sequence 1 203))
;-> (1 2 3 4 5 6 7 8 9 10 20 22 30 36 40 44 50 60 63 66
;->  70 80 88 90 100 101 102 103 104 105 106 107 108 109 110
;->  120 123 130 132 138 140 145 150 154 159 160 167 170 176
;->  180 183 189 190 195 198 200 201 202 203)


-------------
Numeri Radice
-------------

Un numero intero positivo si dice "Radice" se la somma ripetuta delle cifre del prodotto delle sue cifre è uguale alla somma ripetuta delle cifre della somma delle sue cifre.

Esempi:
Numero = 36
Prodotto delle cifre = 18
Somma ripetuta di 18 = 9
Somma delle cifre = 9
Somma ripetuta di 9 = 9
Quindi 36 è un numero radice.

Numero = 26
Prodotto delle cifre = 12
Somma ripetuta di 12 = 3
Somma delle cifre = 8
Somma ripetuta di 8 = 8
Quindi 26 non è un numero radice.

(define (digit-root num)
"Calculates the repeated sum of the digits of an integer"
    (+ 1 (% (- (abs num) 1) 9)))

(digit-root 10)

(define (radice? num)
  (let ( (sum 0) (prod 1) (tmp num) )
    (while (!= num 0)
      (setq prod (* prod (% num 10)))
      (setq sum (+ sum (% num 10)))
      (setq num (/ num 10))
    )
    (= (digit-root sum) (digit-root prod))))

(filter radice? (sequence 1 500))
;-> (1 2 3 4 5 6 7 8 9 22 36 58 63 85 99 123 132 156 165 189 198 213 231 246
;->  264 279 297 312 321 333 348 357 369 375 384 396 426 438 459 462 483 495)

Nota: questa sequenza non esiste su OEIS.

(map digit-sum  (filter radice? (sequence 1 500)))
;-> (1 2 3 4 5 6 7 8 9 4 9 13 9 13 18 6 6 12 12 18 18 6 6 12 12 18 18
;->  6 6 9 15 15 18 15 15 18 12 15 18 12 15 18)

(map digit-prod (filter radice? (sequence 1 500)))
;-> (1 2 3 4 5 6 7 8 9 4 18 40 18 40 81 6 6 30 30 72 72 6 6 48 48 126
;->  126 6 6 27 96 105 162 105 96 162 48 96 180 48 96 180)


--------------------------
Numeri con k cifre diverse
--------------------------

Quanti sono i numeri con k cifre diverse?

Il numero di numeri con k cifre diverse (cioè tutte le cifre devono essere distinte) si calcola considerando alcune regole fondamentali:

- Le cifre disponibili sono 10 da '0' a '9' (0 1 2 3 4 5 6 7 8 9).
- Un numero non può iniziare con 0, quindi bisogna escludere i numeri con lo zero come prima cifra.

Per 'k' cifre distinte:

- Se 'k = 1', ci sono 9 numeri: da 1 a 9.
- Se 'k > 1', il primo posto (la cifra più significativa) può essere una delle cifre da 1 a 9: 9 scelte.
  Poi si sceglie una cifra diversa per il secondo posto: 9 scelte (incluso 0 ma escludendo la prima cifra),
  poi 8 per il terzo, 7 per il quarto, ecc.

Quindi per k cifre diverse (k <= 10), il totale è:

  9 * P(9, k - 1)

dove P(9, k - 1) è la permutazione di (k-1) cifre tra 9 possibili (dopo aver fissato la prima cifra diversa da 0).

  f(n) = 9 * (10 - 1) * (10 - 2) * ... * (10 - n + 1)
       = 9 * (9) * (8) * ... * (10 - n + 1)

Oppure, usando la notazione delle permutazioni:

  f(n) = 9 * P(9, n - 1) = 9 * (9! / (9 - (n - 1))!)

Esempi:

- k = 1: 9 numeri (1–9)
- k = 2: 9 * 9 = 81 (es. 10, 12, ..., 98)
- k = 3: 9 * 9 * 8 = 648
- k = 4: 9 * 9 * 8 * 7 = 4536

(define (fact-i num)
"Calculates the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

Funzione che calcola quanti numeri esistono con k cifre diverse:

(define (numeri k)
  (cond
    ((> k 10) 0)
    ((= k 1) 9)
    (true (* 9 (/ (fact-i 9) (fact-i (- 10 k)))))))

Proviamo:

(map numeri (sequence 0 12))
;-> (0 9 81 648 4536 27216 136080 544320 1632960 3265920 3265920 0 0)

Nota: il numero di numeri con 9 cifre diverse è uguale a quello con 10 cifre diverse (3265920).

Inoltre risulta che:

  Numeri con k cifre tutte diverse (senza zeri iniziali) =
  Tutte le permutazioni di k cifre prese tra le 10 'meno' quante di quelle iniziano con 0

Infatti:
- Le permutazioni delle combinazioni: P(10, k) = 10! / (10 - k)!
- Di queste, quante hanno '0' come prima cifra?
  - Fissiamo 0 come prima cifra e restano (k - 1) cifre da scegliere tra le altre 9
  - P(9, k - 1) = 9! / (9 - (k - 1))!

  Numeri con k cifre diverse (validi) = P(10, k) - P(9, k - 1)

Che è equivalente all'espressione: (* 9 (/ (fact-i 9) (fact-i (- 10 k))))
Infatti:
- Sceglie una prima cifra diversa da 0: 9 scelte
- Poi permuta le rimanenti: P(9, k-1)

Scriviamo una funzione che genera la lista di tutti i numeri diversi con k cifre.

(define (comb k lst (r '()))
"Generates all combinations of k elements without repetition from a list of items"
  (if (= (length r) k)
    (list r)
    (let (rlst '())
      (dolist (x lst)
        (extend rlst (comb k ((+ 1 $idx) lst) (append r (list x)))))
      rlst)))

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

Funzione che genera la lista di tutti i numeri diversi con k cifre:

(define (lista-numeri k)
  (let (out '())
    (dolist (c (comb k '(0 1 2 3 4 5 6 7 8 9)))
      (dolist (p (perm c))
        (if (!= (first p) 0)
            (push (int (join (map string p))) out -1))))
    out))

Proviamo:

(lista-numeri 2)
;-> (10 20 30 40 50 60 70 80 90 12 21 13 31 14 41 15 51 16 61 17 71 18
;->  81 19 91 23 32 24 42 25 52 26 62 27 72 28 82 29 92 34 43 35 53 36
;->  63 37 73 38 83 39 93 45 54 46 64 47 74 48 84 49 94 56 65 57 75 58
;->  85 59 95 67 76 68 86 69 96 78 87 79 97 89 98)
(length  (lista-numeri 2))
;-> 81

(sort (lista-numeri 3))
;-> (102 103 104 105 106 107 108 109 120 123 124 125 126 127 128 129 130
;->  132 134 135 136 137 138 139 140 142 143 145 146 147 148 149 150 152
;->  153 154 156 157 158 159 160 162 163 164 165 167 168 169 170 172 173
;->  ...
;->  947 948 950 951 952 953 954 956 957 958 960 961 962 963 964 965 967
;->  968 970 971 972 973 974 975 976 978 980 981 982 983 984 985 986 987)
(length (lista-numeri 3))
;-> 648
(length (unique (lista-numeri 3)))
;-> 648

(time (println (map length (map lista-numeri (sequence 1 10)))))
;-> (9 81 648 4536 27216 136080 544320 1632960 3265920 3265920)
;-> 35989.719

Vedi anche "Conteggio dei numeri con cifre tutte diverse" su "Note libere 26".


-----------------------------------------------
Problema di Brocard-Ramanujan (Numeri di Brown)
-----------------------------------------------

Il problema di Brocard consiste nel risolvere la seguente equazione diofantea:

 n! + 1 = m^2

Cioè trovare per quali interi n, l'espressione n! + 1 è un quadrato perfetto.
Si congettura che ciò avvenga solo per n uguale a 4, 5 o 7.
In altre parole, non è noto se esistano altre soluzioni (n, m) dell'equazione diofantea a parte le coppie (4, 5), (5, 11) e (7, 71) che vengono chiamate numeri di Brown.
Henri Brocard (1876) e Srinivasa Ramanujan (1913).

Sequenza OEIS A146968:
Brocard's problem: positive integers n such that n!+1 = m^2.
  4, 5, 7
No other terms below 10^9.

Generalizzazione:
  n! + k = m^2

(define (fact-i num)
"Calculates the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

(define (square? num)
"Check if an integer is a perfect square"
  (let (a (bigint num))
    (while (> (* a a) num)
      (setq a (/ (+ a (/ num a)) 2))
    )
    (= (* a a) num)))

(define (brocard limite)
  (for (num 1 limite)
    (if (square? (+ (fact-i num) 1))
      (println num { } (sqrt (+ (fact-i num) 1))))))

(brocard 100)
;-> 4 5
;-> 5 11
;-> 7 71

(time (brocard 1e3))
;-> 4 5
;-> 5 11
;-> 7 71
;-> 204881.11

(define (brocard+ limite-n limite-k)
  (for (num 1 limite-n)
    (for (k 1 limite-k)
      (if (square? (+ (fact-i num) k))
      (println num { } k { } (sqrt (+ (fact-i num) k)))))))

(brocard+ 100 100)
;-> 1 3 2
;-> 1 8 3
;-> 1 15 4
;-> 1 24 5
;-> 1 35 6
;-> 1 48 7
;-> 1 63 8
;-> 1 80 9
;-> 1 99 10
;-> 2 2 2
;-> 2 7 3
;-> 2 14 4
;-> 2 23 5
;-> ...
;-> 4 40 8
;-> 4 57 9
;-> 4 76 10
;-> 4 97 11
;-> 5 1 11
;-> 5 24 12
;-> 5 49 13
;-> 5 76 14
;-> 6 9 27
;-> 6 64 28
;-> 7 1 71
;-> 8 81 201


-----------
Text to DNA
-----------

Una sequenza di DNA è una successione di lettere che rappresentano la struttura primaria di una molecola di DNA, con la capacità di veicolare informazione.
Le lettere sono A, C, G e T e rappresentano le quattro basi nucleotidi adenina, citosina, guanina e timina.

https://codegolf.stackexchange.com/questions/79169/golf-text-into-dna
Nota:
Tutto il contenuto dei siti di Stack Exchange è rilasciato sotto la licenza CC BY-SA 4.0 (Creative Commons Attribution-ShareAlike 4.0).

Convertire un testo ASCII in una sequenza DNA con il seguente algoritmo:

1) Convertire i caratteri del testo in codici ASCII
(esempio: codegolf -> (99 111 100 101 103 111 108 102))

2) Unire i codici ASCII
99111100101103111108102

3) Convertire in binario
10100111111001101001011010001000011001101011011110000110010111111011000000110

4) Aggiungere uno 0 alla fine per ottenere un numero pari di caratteri
101001111110011010010110100010000110011010110111100001100101111110110000001100

5) Sostituire 00 con A, 01 con C, 10 con G e 11 con T
GGCTTGCGGCCGGAGACGCGGTCTGACGCCTTGTAAATA

Esempi:
  codegolf --> GGCTTGCGGCCGGAGACGCGGTCTGACGCCTTGTAAATA
  ppcg     --> GGCTAATTGTCGCACTT
  }        --> TTGG

Sostituzioni:
00 -> A (Adenine, Adenina)
01 -> C (Cytosine Citosina)
10 -> G (Guanine, Guanina)
11 -> T (Thymine, Timina)

(define (bits-i num)
"Convert a decimal integer (big integer) to binary string"
  (setq max-int (pow 2 62)) ; put this outside when doing a lot of calls
  (define (prep s) (string (dup "0" (- 62 (length s))) s))
  (if (<= num max-int) (bits (int num))
      (string (bits-i (/ num max-int))
              (prep (bits (int (% num max-int)))))))

Funzione che converte un testo in una sequenza DNA:

(define (text-dna str)
  (local (a1 a2 a3 a4 a5)
    ; Calcolo codici ASCII
    (setq a1 (map char (explode str)))
    ; Calcolo unione dei codici ASCII
    ; eval-string crea un big integer da una stringa
    ; che rappresenta un numero big integer (anche senza L)
    (setq a2 (eval-string (join (map string a1))))
    ; Conversione in binario
    (setq a3 (dec-binL a2))
    ; Pad '0' per numeri dispari
    (if (even? (length a3))
        (setq a4 a3)
        (setq a4 (push "0" a3 -1)))
    ; Sostituzioni con caratteri DNA
    (setq a5 (explode a4 2))
    (replace "00" a5 "A")
    (replace "01" a5 "C")
    (replace "10" a5 "G")
    (replace "11" a5 "T")
    (join a5)))

Proviamo:

(text-dna "codegolf")
;-> "GGCTTGCGGCCGGAGACGCGGTCTGACGCCTTGTAAATA"
(text-dna "ppcg")
;-> "GGCTAATTGTCGCACTT"
(text-dna "}")
;-> "TTGG"


------------------------------------
Sequenza di numeri naturali infiniti
------------------------------------

Una sequenza di numeri naturali infiniti è una sequenza che contiene ogni numero naturale infinite volte.
Scrivere la funzione più breve che stampa tutti i numeri naturali da 1 a N per tutti gli N che appartengono ai Numeri Naturali.

Esempio 1:
1
1 2
1 2 3
1 2 3 4
1 2 3 4 5
1 2 3 4 5 6
1 2 3 4 5 6 7
...

Esempio 2:
1 1 2 1 2 3 1 2 3 4 1 2 3 4 5 1 2 3 4 5 6 1 2 3 4 5 6 7 ...

Versione con limite (k):

(define (seq1 k)
  (for (i 1 k) (for (j 1 i) (print j { }))))

(seq1 5)
;-> 1 1 2 1 2 3 1 2 3 4 1 2 3 4 5

Versione infinita (read-line):

(define (seq2 i)
  (until nil (inc i) (for (j 1 i) (print j { })) (read-line)))

(seq2)
;-> 1
;-> 1 2
;-> 1 2 3
;-> 1 2 3 4
;-> 1 2 3 4 5
;-> 1 2 3 4 5 6
;-> ...

Versione infinita compatta (56 caratteri):
(define(f i)(until nil(inc i)(for(j 1 i)(print j { }))))

(f)
;-> 1 1 2 1 2 3 1 2 3 4 1 2 3 4 5 1 2 3 4 5 6 1 2 3 4 5 6 7 ...


------------------
Sum of Modulo Sums
------------------

https://codegolf.stackexchange.com/questions/102685/sum-of-modulo-sums

Tutto il contenuto dei siti di Stack Exchange è rilasciato sotto la licenza CC BY-SA 4.0 (Creative Commons Attribution-ShareAlike 4.0).

Given an integer n > 9, for each possible insertion between digits in that integer, insert an addition + and evaluate.
Then, take the original number modulo those results.
Output the sum total of these operations.

An example with n = 47852:

47852 % (4785+2) = 4769
47852 % (478+52) =  152
47852 % (47+852) =  205
47852 % (4+7852) =  716
                  -----
                   5842

Funzione che divide un numero in coppie per ogni indice (1..len(n) - 1):

(define (divide-numero n)
  (let ( (s (string n)) (len (length (string n))) (out '()) )
    (for (i 1 (- len 1))
      (push (list (int (slice s 0 i) 0 10) (int (slice s i) 0 10)) out))
    out))

(divide-numero 12345)
;-> ((1234 5) (123 45) (12 345) (1 2345))

(divide-numero 1200345)
;-> ((120034 5) (12003 45) (1200 345) (120 345) (12 345) (1 200345))

(define (modsum n)
  (letn ( (s (string n)) (len (length s)) (out 0) )
    (for (i 1 (- len 1))
      (++ out (% n (+ (int (slice s 0 i) 0 10) (int (slice s i) 0 10)))))
    out))

Proviamo:

(modsum 47852)
;-> 5842
(modsum 13)
;-> 1
(modsum 111)
;-> 6
(modsum 12345)
;-> 2097
(modsum 54321)
;-> 8331
(modsum 3729105472)
;-> 505598476


----------------
Bit manipulation
----------------

newLISP mette a disposizione i seguenti operatori per le operazioni sui bit dei numeri interi:

Shift Left, Shift Right
-----------------------
<<, >>
syntax: (<< int-1 int-2 [int-3 ... ])
syntax: (>> int-1 int-2 [int-3 ... ])
syntax: (<< int-1)
syntax: (>> int-1)

The number int-1 is arithmetically shifted to the left or right by the number of bits given as int-2, then shifted by int-3 and so on.
For example, 64-bit integers may be shifted up to 63 positions.
When shifting right, the most significant bit is duplicated (arithmetic shift):

(>> 0x8000000000000000 1)  ;->  0xC000000000000000  ; not 0x0400000000000000!

(<< 1 3)      ;->   8
(<< 1 2 1)    ;->   8
(>> 1024 10)  ;->   1
(>> 160 2 2)  ;->  10

(<< 3)        ;->   6
(>> 8)        ;->   4

When int-1 is the only argument << and >> shift by one bit.

Bitwise AND
-----------
&
syntax: (& int-1 int-2 [int-3 ... ])
A bitwise and operation is performed on the number in int-1 with the number in int-2, then successively with int-3, etc.

(& 0xAABB 0x000F)  ;->  11  ; which is 0xB

Bitwise OR
----------
|
syntax: (| int-1 int-2 [int-3 ... ])
A bitwise or operation is performed on the number in int-1 with the number in int-2, then successively with int-3, etc.

(| 0x10 0x80 2 1)  ;-> 147

Bitwise XOR
-----------
^
syntax: (^ int-1 int-2 [int-3 ... ])
A bitwise xor operation is performed on the number in int-1 with the number in int-2, then successively with int-3, etc.

(^ 0xAA 0x55)  ;-> 255

Bitwise NOT
-----------
~
syntax: (~ int)
A bitwise not operation is performed on the number in int, reversing all of the bits.

(format "%X" (~ 0xFFFFFFAA))  ;-> "55"
(~ 0xFFFFFFFF)                ;-> 0

Vediamo quattro funzioni per la manipolazione dei bit di un numero intero:

- (get-bit    num idx) Recupera il valore del bit all'indice idx di num
- (set-bit    num idx) Imposta a 1 il bit all'indice idx di num
- (clear-bit  num idx) Imposta a 0 il bit all'indice idx di num
- (toggle-bit num idx) Inverte il bit all'indice idx di num

Esempio:
  Numero intero = 142
  Numero binario = "10001110"
  Indice: 7 6 5 4 3 2 1 0
  Bit:    1 0 0 0 1 1 1 0

Funzione che restituisce il bit all'indice idx di un numero intero num:

(define (get-bit num idx)
  (if (zero? (& num (<< 1 idx))) 0 1))

(bits 142)
;-> "10001110"

(map (fn(x) (get-bit 142 x)) (sequence 8 0))
;-> (0 1 0 0 0 1 1 1 0)

Funzione che imposta a 1 il bit all'indice idx di un numero intero num:

(define (set-bit num idx) (| num (<< 1 idx)))

(bits 142)
;-> "10001110"

(set-bit 142 0)
;-> 143
(bits 143)
;-> "10001111"

(set-bit 142 6)
;-> 206
(bits 206)
;-> "11001110"

Funzione che imposta a 0 il bit all'indice idx di un numero intero num:

(define (clear-bit num idx)
  (let (mask (~ (<< 1 idx)))
    (& num mask)))

(bits 142)
;-> "10001110"
(clear-bit 142 1)
;-> 140
(bits 140)
;-> 10001100
(clear-bit 142 7)
;-> 14
(bits 14)
;-> "1110"

Funzione che inverte il bit all'indice idx di un numero intero num:

(define (toggle-bit num idx)
  (^ num (<< 1 idx)))

(bits 45)
;-> "101101"
(toggle-bit 45 3)
;-> 37
(bits 37)
;-> "100101"

Ecco una lista sintetica del comportamento degli operatori bitwise sui numeri interi (considerando sempre interi con segno, come in newLISP e molti linguaggi C-like):

Operatori bitwise in forma simbolica
------------------------------------
| Operatore | Nome               | Effetto su un intero positivo N               | Esempio     |
|-----------|--------------------|-----------------------------------------------|-------------|
| ~N        | NOT bitwise        | -(N + 1)                                      | ~5 = -6     |
| &         | AND bitwise        | Bit a 1 solo se entrambi hanno il bit a 1     | 5 & 3 = 1   |
| |         | OR bitwise         | Bit a 1 se almeno uno ha il bit a 1           | 5 | 3 = 7   |
| ^         | XOR bitwise        | Bit a 1 se uno solo dei due ha il bit a 1     | 5 ^ 3 = 6   |
| << N      | shift left         | Moltiplica per 2^N                            | 3 << 1 = 6  |
| >> N      | shift right arit.  | Divide per 2^N (tronca verso zero)            | 6 >> 1 = 3  |

Dettagli ed equivalenze matematiche
-----------------------------------
1. NOT (~N)
   - Operazione: inverte tutti i bit.
   - Equivalente a: ~N = -(N + 1)
   - Esempio: ~7 = -8, ~0 = -1

2. AND (A & B)
   - Bit per bit: 1 solo se entrambi i bit sono 1.
   - Esempio: 6 & 3 = 2 (in binario: 110 & 011 = 010)

3. OR (A | B)
   - Bit per bit: 1 se almeno uno è 1.
   - Esempio: 6 | 3 = 7 (in binario: 110 | 011 = 111)

4. XOR (A ^ B)
   - Bit per bit: 1 se i bit sono diversi.
   - Esempio: 6 ^ 3 = 5 (in binario: 110 ^ 011 = 101)

5. Shift sinistro (A << N)
   - Equivalente a: A * 2^N
   - Esempio: 2 << 3 = 16 (perché 2 * 2^3 = 16)

6. Shift destro (A >> N) (aritmetico)
   - Equivalente a: floor(A / 2^N) per A >= 0
   - Esempio: 17 >> 2 = 4 (perché 17 / 4 = 4.25, troncato)

Alcune formule comode
---------------------
- (<< 1 N) = 2 * N
- (>> 1 N) = floor(N / 2)
- (<< 2 N) = 4 * N = N * 2^2
- (<< N 1) = 2^N (se N è il numero da shiftare, non lo shift)

Nota: in newLISP (<< 2 3) significa "shiftare 2 di 3 bit a sinistra", quindi 2 << 3 = 16, che è 2^4, non 2^3.

Riassunto per numeri positivi
-----------------------------
- ~N       --> cambia segno e aggiunge 1 negativo
- << N     --> moltiplica per 2^N
- >> N     --> divide per 2^N
- << 1     --> per 2
- >> 1     --> diviso 2
- << k     --> N * 2^k
- (<< 1 k) --> 2^k
- (<< 2 k) --> 2 * 2^k = 2^{k+1}

Ecco una tabella con il comportamento degli operatori bitwise per numeri negativi (con segno, a complemento a due, come in newLISP o C).

Tabella operatori bitwise con N negativo
----------------------------------------
Assumiamo N = -X, con X > 0

| Operatore  | Significato          | Descrizione (N negativo)                         | Esempio         |
|------------|----------------------|--------------------------------------------------|-----------------|
| ~N         | NOT bitwise          | ~(-X) = X - 1                                    | ~(-5) = 4       |
| &          | AND bitwise          | Bit per bit tra negativi e/o positivi            | (-6) & 3 = 2    |
| |          | OR bitwise           | Bit per bit tra negativi e/o positivi            | (-6) \| 3 = -5  |
| ^          | XOR bitwise          | Bit per bit tra negativi e/o positivi            | (-6) ^ 3 = -7   |
| N << k     | shift sinistro       | (-X) << k = -X * 2^k                             | (-3) << 2 = -12 |
| N >> k     | shift destro arit.   | (-X) >> k = floor(-X / 2^k) con segno mantenuto  | (-9) >> 1 = -5  |

Approfondimenti
---------------
1. NOT (~N) con N negativo:
- Inverte tutti i bit --> risultato positivo.
- ~(-X) = X - 1
- Esempio:
  - ~(-8) = 7
  - ~(-1) = 0

2. Shift sinistro (<<):
- Anche su negativi, moltiplica per 2^k, segno incluso.
- Esempio: (-3) << 2 = -12

3. Shift destro (>>):
- In aritmetico mantiene il segno (in newLISP).
- Si comporta come divisione intera "floor verso -∞"
- Esempi:
  - (-9) >> 1 = -5  (perché -9 / 2 = -4.5 --> floor(-4.5) = -5)
  - (-4) >> 1 = -2

Riassunto per N negativo
------------------------
| Operatore | Risultato                                 |
|-----------|-------------------------------------------|
| ~N        | -N - 1                                    |
| << k      | N * 2^k                                   |
| >> k      | floor(N / 2^k)                            |
| A & B     | confronta bit per bit (complemento a due) |
| A | B     | confronta bit per bit                     |
| A ^ B     | confronta bit per bit                     |

Facciamo gli esempi per:

- ~(-5)
- (-5) >> 1

Esempio 1 — Calcolare ~(-5) in binario
--------------------------------------
1. Partiamo da 5 in binario su 8 bit: 00000101

2. Scriviamo -5 in complemento a due:
   - Prima invertiamo i bit:   11111010
   - Poi aggiungiamo 1:        11111011
   Quindi -5 è:                11111011

3. Ora applichiamo il bitwise NOT (~):
   Invertiamo tutti i bit di -5: ~11111011 = 00000100

4. 00000100 in decimale è 4.

Quindi:

~(-5) = 4

Esempio 2 — Calcolare (-5) >> 1 in binario
------------------------------------------
1. Partiamo di nuovo da -5, che è: 11111011

2. Facciamo uno shift a destra aritmetico (>> 1):
   - Manteniamo il bit di segno (quindi il primo bit resta 1)
   - Shiftiamo tutto verso destra di 1:
     Prima: 11111011
     Dopo:  11111101

3. 11111101 in complemento a due è:
   - Invertiamo i bit: 00000010
   - Aggiungiamo 1:    00000011
   Quindi il valore è -3.

Però ATTENZIONE: lo shift aritmetico in newLISP/C fa il floor verso il basso.
Facciamo il conto:

-5 / 2 = -2.5 --> floor(-2.5) = -3

Quindi:

(-5) >> 1 = -3

Riepilogo finale
----------------
| Espressione | Risultato | Binario intermedio |
|-------------|-----------|--------------------|
| ~(-5)       | 4         | 00000100           |
| (-5) >> 1   | -3        | 11111101           |


---------------------------
Bit manipulation (Funzioni)
---------------------------

Elenco delle funzioni implementate:

 1) Calcolare il bit più basso (LSB) di un numero
 2) Azzerare il bit più basso (LSB) di un numero
 3) Calcolare il bit più alto (MSB) di un numero
 4) Azzerare il bit più alto (MSB) di un numero
 5) Verificare se un numero è una potenza di due
 6) Verificare se un numero ha tutti i bit settati
 7) Verificare se un numero ha i bit alternati
 8) Verificare se due numeri hanno segno opposto
 9) Scambiare i valori di due numeri
10) Verificare se due numeri sono uguali
11) Algoritmo di Euclide per Massimo Comun Divisore (GCD)
12) Copia tra due numeri di bit-set in un intervallo
13) Verificare se la rappresentazione binaria di un numero è palindroma
14) Rotazione di bit

Nota: il parametro (num) delle funzioni deve essere un numero intero positivo.

1) Calcolare il bit più basso (LSB) di un numero
------------------------------------------------
Questo si ottiene usando l'espressione 'X &(-X)'.
Vediamolo con un esempio: Sia X = 00101100.
Quindi ~X(complemento a 1) sarà '11010011' e il complemento a 2 sarà (~X+1 o -X), ovvero '11010100'.
Quindi, se eseguiamo l'operazione 'AND' del numero originale 'X' con il suo complemento a 2, che è '-X', otteniamo il bit più basso:

    00101100
  & 11010100
  -----------
    00000100

(define (lsb num) (& num (- num)))

(lsb 36)
;-> 4
(bits 36)
;-> "100100"
(bits 4)
;-> "100"

2) Azzerare il bit più basso (LSB) di un numero
-----------------------------------------------
Con la seguente funzione imposto a 0 il LSB di un numero intero:

(define (clear-lsb) (& num (- num 1)))

(clear-lsb 312)
;-> 304
(bits 312)
;-> "100111000"
(bits 304)
;-> "100110000"

3) Calcolare il bit più alto (MSB) di un numero
-----------------------------------------------
Convertiamo in newlisp la seguente funzione in C che calcola il MSB di un numero intero.

int MSB(int n)
{
  // Below steps set bits after MSB (including MSB)
  // Suppose n is 273 (binary is 100010001).
  // It does following: 100010001 | 010001000 = 110011001
  n |= n >> 1;

  // This makes sure 4 bits (From MSB and including MSB) are set.
  // It does following: 110011001 | 001100110 = 111111111
  n |= n >> 2;
  n |= n >> 4;
  n |= n >> 8;
  n |= n >> 16;

  // The naive approach would increment n by 1,
  // so only the MSB+1 bit will be set.
  // So now n theoretically becomes 1000000000.
  // All the would remain is a single bit right shift:
  //    n = n + 1;
  //    return (n >> 1);
  //
  // ... however, this could overflow the type.
  // To avoid overflow, we must retain the value
  // of the bit that could overflow:
  //     n & (1 << ((sizeof(n) * CHAR_BIT)-1))
  // and OR its value with the naive approach:
  //     ((n + 1) >> 1)
  n = ((n + 1) >> 1) |
      (n & (1 << ((sizeof(n) * CHAR_BIT)-1)));
  return n;
}

(define (msb num)
  (let (INT_SIZE 64)
    (setq num (| num (>> num 1)))
    (setq num (| num (>> num 2)))
    (setq num (| num (>> num 4)))
    (setq num (| num (>> num 8)))
    (setq num (| num (>> num 16)))
    (setq num (| (>> (add num 1) 1)
              (& num (<< 1 (- INT_SIZE 1)))))
    num))

(msb 21)
;-> 16
(bits 21)
;-> "10101"
(bits 16)
;-> "10000"

(msb 273)
;-> 256
(bits 273)
;-> "100010001"
(bits 256)
;-> "100000000"

(msb 1001)
;-> 512
(bits 1001)
;-> "1111101001"
(bits 512)
;-> "1000000000"

4) Azzerare il bit più alto (MSB) di un numero
----------------------------------------------
Per azzerare il MSB (Most Significant Bit) di un numero, occorre:
- Trovare qual è il bit più alto impostato a 1.
- Creare una maschera che ha 0 in quella posizione e 1 negli altri.
- Fare l'operazione AND tra il numero e la maschera.

(define (clear-msb num)
  (let ( (n num) (msb 1) )
    (while (> n 1)
      (setq n (>> n 1))
      (setq msb (<< msb 1)))
    (& num (- msb 1))))

Come funziona?
Partiamo da:
  n = num
  msb = 1
Ciclo:
  Finché n > 1:
    n = n >> 1 --> sposta i bit verso destra (scala verso il bit più significativo).
    msb = msb << 1 --> sposta il bit "acceso" a sinistra per inseguire il MSB.
Fine ciclo:
  Ora msb contiene solo il bit corrispondente al MSB di num.
Maschera:
  msb - 1 genera una maschera con tutti 1 sotto il MSB.
Risultato:
  Calcolare (& num (- msb 1)) per togliere il MSB.

(clear-msb 312)
;-> 56
(bits 312)
;-> "100111000"
(bits 56)
;-> "111000"

(clear-msb 30)
;-> 14
(bits 30)
;-> "11110"
(bits 14)
;-> "1110"

Anche la seguente funzione azzera il MSB (ed è più veloce):

(define (clear-msb2 num)
  (& num (^ (<< 1 (- (length (bits num)) 1)) -1)))

Come funziona?

- (bits num)
   --> converte num in stringa binaria ("100111000" per 312).
- (length (bits num))
   --> conta il numero di cifre binarie (senza zeri iniziali).
- (- (length (bits num)) 1)
   --> posizione dello zero-based MSB (tipo posizione massima).
- (<< 1 ...)
   --> costruisce un numero con solo il MSB acceso.
- (^ ... -1)
  --> fa il NOT bitwise: tutti 1 tranne il MSB.
- (& num ...)
   --> applica la maschera per azzera il MSB.

(clear-msb2 312)
;-> 56
(clear-msb2 30)
;-> 14

Test di correttezza:
(= (map clear-msb nums) (map clear-msb2 nums))

Test di velocità:
(setq nums (rand 1e6 1e3))
(time (map clear-msb nums) 1e3)
;-> 1859.41
(time (map clear-msb2 nums) 1e3)
;-> 310.998

5) Verificare se un numero è una potenza di due
-----------------------------------------------
Notiamo che se n è una potenza di 2, allora la sua rappresentazione binaria ha esattamente un bit impostato a 1, e n-1 avrà tutti i bit a destra di quel bit impostati a 1.
Quindi, n & (n-1) sarà sempre 0 per le potenze di 2 (tranne che per num=0).

(define (power2? num)
 (and (> num 0) (zero? (& num (- num 1)))))
 (num > 0) and ((num & (num - 1)) == 0))

(filter power2? (sequence 1 100))
;-> (1 2 4 8 16 32 64)

6) Verificare se un numero ha tutti i bit settati
-------------------------------------------------
Un numero ha tutti i bit a 1 se risulta: (((n + 1) & n) == 0)

Esempi:
  num = 88       --> 1011000
  (num + 1) = 89 --> 1011001
  (& 88 89) = 88 --> 1011000 (non ha tutti i bit a 1)
     1011001
   & 1011000
   ---------
     1011000

  num = 63  --> 111111
  (num + 1) = 64 --> 1000000
  (& 63 64) = 0

(define (allbitset? num) (zero? (& (+ num 1) num)))

(filter allbitset? (sequence 1 100))
;-> (1 3 7 15 31 63)
(map bits '(1 3 7 15 31 63))
;-> ("1" "11" "111" "1111" "11111" "111111")

7) Verificare se un numero ha i bit alternati
---------------------------------------------
Il numero 42 ha bit alternati:
(bits 42)
;-> "101010"
Il numero 5 ha bit alternati:
(bits 5)
;-> "101"

Il numero 12 non ha bit alternati:
(bits 12)
;-> "1100"

Algoritmo
- Calcolare l'XOR bit a bit (XOR indicato con ^) di n e (n >> 1).
- Se n ha uno schema alternato, l'operazione n ^ (n >> 1) produrrà un numero con tutti i bit impostati a 1.

(define (allbitset? num) (zero? (& (+ num 1) num)))

(define (alternatebitorder? num) (allbitset? (^ num (>> num 1))))

(int "101010" 0 2)
;-> 42
(int "10101" 0 2)
;-> 21

(int "111111" 0 2)
;-> 63
(int "101110" 0 2)
;-> 46

(filter alternatebitorder? (sequence 1 100))
;-> (1 2 5 10 21 42 85)
(map bits '(1 2 5 10 21 42 85))
;-> ("1" "10" "101" "1010" "10101" "101010" "1010101")

8) Verificare se due numeri hanno segno opposto
-----------------------------------------------
L'operazione XOR (^) tra due numeri con segni opposti avrà sempre il bit più a sinistra (il bit di segno) impostato su 1.
Ciò significa che se (a ^ b) < 0, allora a e b hanno segni opposti.

(define (opposite-sign? n1 n2) (< (^ n1 n2) 0))

(opposite-sign? 2 3)
;-> nil
(opposite-sign? -2 -3)
;-> nil
(opposite-sign? -2 3)
;-> true
(opposite-sign? 2 -3)
;-> true

9) Scambiare i valori di due numeri
-----------------------------------
Usiamo le proprietà dello XOR:

a = a ^ b: Memorizza lo XOR di 'a' e 'b' in 'a'.
           Ora, 'a' contiene il risultato di (a ^ b).
b = a ^ b: Esegue lo XOR bit a bit del nuovo valore di 'a' con 'b' per ottenere il valore originale di 'a'.
           Questo ci dà, b = (a ^ b) ^ b = a.
a = a ^ b: Esegue lo XOR del nuovo valore di 'a' con il nuovo valore di 'b' (che è l'originale a) per ottenere il valore originale di b.
           Questo ci dà, a = (a ^ b) ^ a = b.

(define (exchange n1 n2)
  (setq n1 (^ n1 n2))
  (setq n2 (^ n1 n2))
  (setq n1 (^ n1 n2))
  (list n1 n2))

(exchange -20 10)
;-> (10 -20)

10) Verificare se due numeri sono uguali
----------------------------------------
Lo XOR di due numeri è 0 se i numeri sono uguali, altrimenti è diverso da zero.

(define (equal n1 n2) (zero? (^ n1 n2)))

(equal 2 4)
;-> nil

(equal 3 3)
;-> true


11) Algoritmo di Euclide per Massimo Comun Divisore (GCD)
---------------------------------------------------------
Funzione standard per il calcolo del GCD con l'algoritmo di Euclide:

(define (gcd-e a b)
  (if (zero? a) b
      (gcd (% b a) a)))

Utilizzando gli operatori bitwise:
- possiamo trovare x/2 usando x>>1.
- possiamo verificare se x è pari o dispari usando x&1.

gcd(a, b) = 2*gcd(a/2, b/2) se sia a che b sono pari.
gcd(a, b) = gcd(a/2, b) se a è pari e b è dispari.
gcd(a, b) = gcd(a, b/2) se a è dispari e b è pari.

(define (gcd-bit a b)
  (cond
    ((or (zero? b) (= a b)) a)
    ((zero? a) b)
    ; If both a and b are even, divide both a and b by 2.
    ; And multiply the result with 2
    ((and (zero? (& a 1)) (zero? (& b 1)))
      (<< (gcd-bit (>> a 1) (>> b 1)) 1))
    ; If a is even and b is odd, divide a by 2
    ((and (zero? (& a 1)) (!= (& b 1) 0))
      (gcd-bit (>> a 1) b))
    ; If a is odd and b is even, divide b by 2
    ((and (!= (& a 1) 0) (zero? (& b 1)))
      (gcd-bit a (>> b 1)))
    ; If both are odd, then apply normal subtraction algorithm.
    ; Note that odd-odd case always converts odd-even case after one recursion
    (true
      (if (> a b) (gcd-bit (- a b) b) (gcd-bit a (- b a))))))

(gcd-bit 28 120)
;-> 4

Test di correttezza:

(define (test iter)
  (setq a (+ (rand 1e5) 1))
  (setq b (+ (rand 1e5) 1))
  (if (> b a) (swap a b))
  (if (!= (gcd-bit a b) (gcd a b))
      (println "Errore: " a { } b { } (gcd-bit a b) { } (gcd a b))))

12) Copia tra due numeri di bit-set in un intervallo
----------------------------------------------------
Dati due numeri x e y e un intervallo [l, r] in cui 1 <= l, r <= 64, scrivere una funzione che copia i bit di y che valgono 1 (bit-set) nell'intervallo da l a r compresi e li copia in x.

Esempio:

x = 102 --> "1100110"
y = 56  -->  "111000"
                   5 3
l = 3, r = 5 --> "111000" --> "110"

Copia su x dei bit a 1 di y nell'intervallo:

 1100110
   110
---------
 1111110 --> 126

(int "1111110" 0 2)
;-> 126

(define (copy-bits x y l r)
  (cond ((or (< l 1) (> r 64)) x)
        (true
          ; get the length of the mask
          (setq masklen (int (- (<< 1 (+ r 1 (- l))) 1)))
          ; Shift the mask to the required position
          ; "&" with y to get the set bits at between
          ; l ad r in y
          (setq mask (& (<< masklen (- l 1)) y))
          (setq x (| x mask)))))

Proviamo:

(copy-bits 102 56 4 6)
;-> 126

(copy-bits 10 13 2 3)
;-> 14

13) Verificare se la rappresentazione binaria di un numero è palindroma

(define (binario-palindromo? num)
  (setq out true)
  ; Find the number of bits
  (setq len (+ (int (log num 2)) 1))
  (setq l 0)
  (setq r (- len 1))
  (while (and (< l r) out)
    ;Compare bits at positions l and r
    (when (!= (& (>> num l) 1) (& (>> num r) 1))
          (setq out nil))
    (++ l)
    (-- r)
  )
  out)

(binario-palindromo? 9)
;-> true
(binario-palindromo? 10)
;-> nil

(filter binario-palindromo? (sequence 1 100))
;-> (1 3 5 7 9 15 17 21 27 31 33 45 51 63 65 73 85 93 99)

(map bits '(1 3 5 7 9 15 17 21 27 31 33 45 51 63 65 73 85 93 99))
;-> ("1" "11" "101" "111" "1001" "1111" "10001" "10101"
;->  "11011" "11111" "100001" "101101" "110011" "111111"
;->  "1000001" "1001001" "1010101" "1011101" "1100011")

14) Rotazione di bit
--------------------
Una rotazione (o spostamento circolare) è un'operazione simile allo spostamento, con la differenza che i bit che escono da un'estremità vengono rimessi all'altra estremità.
Nella rotazione sinistra, i bit che escono dall'estremità sinistra vengono rimessi all'estremità destra.
Nella rotazione destra, i bit che escono dall'estremità destra vengono rimessi all'estremità sinistra.

Nota:
(setq MAX-INT 9223372036854775807)
(bits MAX-INT)
;-> "111111111111111111111111111111111111111111111111111111111111111"
(length (bits MAX-INT))
;-> 63

(setq MIN-INT -9223372036854775808)
(bits MIN-INT)
;-> "1000000000000000000000000000000000000000000000000000000000000000"
(length (bits MIN-INT))
;-> 64

(define (rotate-left num d)
    # Rotation of 63 is same as rotation of 0
    (setq d (% d 63))
    # Picking the rightmost (63 - d) bits
    (setq mask (~ (- (<< 1 (- 63 d)) 1)))
    (setq shift (& num mask))
    # Moving the remaining bits to their new location
    (setq num (<< num d))
    # Adding removed bits at leftmost end
    (setq num (+ num (>> shift (- 63 d))))
    # Ensuring 63-bit constraint
    (& num  (- (<< 1 63) 1)))

(define (rotate-right num d)
    # Rotation of 63 is same as rotation of 0
    (setq d (% d 63))
    # Picking the leftmost d bits
    (setq mask (- (<< 1 d) 1))
    (setq shift (& num mask))
    # Moving the remaining bits to their new location
    (setq num (>> num d))
    # Adding removed bits at rightmost end
    (setq num (+ num (<< shift (- 63 d))))
    # Ensuring 63-bit constraint
    (& num (- (<< 1 63) 1)))

(define (test n d)
  (list (rotate-left n d) (rotate-right n d)))

(test 28 2)
;-> (112 7)
(test 29 2)
;-> (116 2305843009213693959)
(test 11 10)
;-> (11264 99079191802150912)


--------------------------
Big-endian e Little-endian
--------------------------

Cosa significa Endianness?
--------------------------
La parola "Endianness" si riferisce all'ordine in cui i byte sono disposti in memoria.
L'endianness si presenta in due forme principali: Big-endian (BE) e Little-endian (LE).
I computer memorizzano i byte in una di queste due forme:

Big-endian (BE): Memorizza per primo il byte più significativo (il "big end").
Ciò significa che il primo byte (all'indirizzo di memoria più basso) è il più grande, il che ha più senso per chi legge da sinistra a destra.

Little-endian (LE): Memorizza per primo il byte meno significativo (il "little end").
Ciò significa che il primo byte (all'indirizzo di memoria più basso) è il più piccolo, il che ha più senso per chi legge da destra a sinistra.

Cos'è il Big-endian (BE)?
-------------------------
In un sistema big-endian, il byte più significativo (MSB) viene memorizzato all'indirizzo di memoria più basso. Ciò significa che il "big end" (la parte più significativa del dato) viene memorizzato per primo.
Ad esempio, un intero a 32 bit 0x12345678 verrebbe memorizzato in memoria come segue in un sistema big-endian:

Indirizzo: 00   01   02   03
Dati:         12   34   56    78
Qui, 0x12 è il byte più significativo, posizionato all'indirizzo più basso (00), seguito da 0x34, 0x56 e 0x78 all'indirizzo più alto (03).

Cos'è il Little-endian?
-----------------------
Un sistema little-endian memorizza il byte meno significativo (LSB) all'indirizzo di memoria più basso. Il "little end" (la parte meno significativa dei dati) viene prima. Per lo stesso intero a 32 bit 0x12345678, un sistema little-endian lo memorizzerebbe come:

Indirizzo: 00   01   02   03
Dati:         78   56   34   12
Qui, 0x78 è il byte meno significativo, posizionato all'indirizzo più basso (00), seguito da 0x56, 0x34 e 0x12 all'indirizzo più alto (03).

Significato del byte più significativo (MSbyte) in Little e Big Endian
----------------------------------------------------------------------
Per chiarire meglio il concetto di endianness vediamo il concetto di byte più significativo (MSbyte).
Consideriamo il numero decimale 2984.
Cambiando la cifra da 4 a 5, il numero aumenta di 1, mentre cambiando la cifra da 2 a 3, il numero aumenta di 1000.
Questo concetto si applica anche a byte e bit.

Byte più significativo (MSbyte): il byte che contiene il valore di posizione più alto.
Byte meno significativo (LSbyte): il byte che contiene il valore di posizione più basso.

Nel formato big-endian, l'MSbyte viene memorizzato per primo.
Nel formato little-endian, l'MSbyte viene memorizzato per ultimo.

Quando l'endianness potrebbe essere un problema?
------------------------------------------------
L'endianness deve essere presa in considerazione quando sistemi con ordini di byte diversi devono comunicare o condividere dati.


------------------------
Int32 e Int64 (IEEE 754)
------------------------

La rappresentazione IEEE 754 a 64 bit (double precision) differisce da quella a 32 bit (single precision) nei seguenti punti chiave:

Struttura del numero IEEE 754
-----------------------------
Tipo       Bit Totali   Segno   Esponente   Mantissa (o significando)
---------------------------------------------------------------------
Float 32   32           1       8           23
Float 64   64           1       11          52

Differenze principali
---------------------
Aspetto          32 bit (float)       64 bit (double)
-----------------------------------------------------
Bit di segno     1                    1
Bit esponente    8 (bias 127)         11 (bias 1023)
Bit mantissa     23                   52
Bias             127                  1023
Precisione       ~7 cifre decimali    ~15-17 cifre decimali


----------------------------------------
Addizione di due numeri binari (stringa)
----------------------------------------

(define (add-binary s1 s2)
  ; Trim Leading Zeros
  (setq s1 (trim s1 "0" ""))
  (setq s2 (trim s2 "0" ""))
  (setq n (length s1))
  (setq m (length s2))
  ; Swap the strings if s1 is of smaller length
  (when (< n m)
    (swap n m)
    (swap s1 s2))
  (setq j (- m 1))
  (setq carry 0)
  (setq out "")
  ; Traverse both strings from the end
  (for (i (- n 1) 0 -1)
    (setq bit1 (int (s1 i)))
    (setq bit-sum (+ bit1 carry))
    ; If there are remaining bits in s2, then add them to the bitSum
    (when (>= j 0)
            # Current bit of s2
            (setq bit2 (int (s2 j)))
            (setq bit-sum (+ bit-sum bit2))
            (-- j))
    ; Calculate the out bit and update carry
    (setq bit (% bit-sum 2))
    (setq carry (/ bit-sum 2))
    ; Update the current bit in out
    (push (string bit) out)
  )
  ; If there's any carry left, prepend it to the out
  (if (> carry 0) (push "1" out))
  out)

Proviamo:

13 + 7 = 20  --> "1101" + "111" = "10100"
(add-binary "01101" "00111")
;-> 10100
(int "01101" 0 2)
;-> 13
(int "00111" 0 2)
;-> 7
(int "10100" 0 2)
;-> 20

Test di correttezza con 1e5 addizioni:

(for (i 1 1e5)
  (setq s1 (bits (rand 1e6)))
  (setq s2 (bits (rand 1e6)))
  (when (!= (int (add-binary s1 s2) 0 2) (+ (int s1 0 2) (int s2 0 2)))
    (println "Errore: " s1 { } s2)
    (println (int (add-binary s1 s2) 0 2) { } (+ (int s1 0 2) (int s2 0 2)))))
;-> nil


-----------------------------------------------------------------
Maggiore e minore di tre interi (senza operatori di comparazione)
-----------------------------------------------------------------

Trovare il più grande e il più piccolo di tre numeri interi positivi a, b e c senza utilizzare operatori di comparazione.

Algoritmo (numero maggiore)
Sottrarre 1 da tutti i numeri fino a che l'ultimo di loro diventa 0.
Il numero di sottrazioni rappresenta il numero maggiore.

(define (maggiore a b c)
  (let (conta 0)
    (while (or (> a 0) (> b 0) (> c 0))
      (-- a) (-- b) (-- c)
      (++ conta))
    conta))

(maggiore 3 6 2)
;-> 6

Algoritmo (numero minore)
Sottrarre 1 da tutti i numeri fino a che uno di loro diventa 0.
Il numero di sottrazioni rappresenta il numero minore.

(define (minore a b c)
  (let (conta 0)
    (while (and (> a 0) (> b 0) (> c 0))
      (-- a) (-- b) (-- c)
      (++ conta))
    conta))

(minore 3 6 2)
;-> 2


--------------------------------------------
Addizione di due interi (check for overflow)
--------------------------------------------

Scrivere una funzione che addiziona due numeri interi e rileva l'overflow durante l'addizione.
Se la somma non causa un overflow, restituire la loro somma.
In caso contrario, restituire nil (overflow).

Il ragionamento è che l'overflow può verificarsi solo se entrambi i numeri hanno lo stesso segno, ma la somma ha segno opposto.
Infatti nella tipica rappresentazione di numeri interi, il superamento del limite massimo o minimo causa un effetto di wraparound, invertendo il segno.
Verificando se si verifica questa inversione di segno, possiamo determinare con precisione l'overflow e restituire un indicatore di errore.

(setq MAX-INT  9223372036854775807)
(setq MIN-INT -9223372036854775808)

(+ MAX-INT 1)
;-> -9223372036854775808

(+ MIN-INT -1)
;-> 9223372036854775807

(define (add-overflow a b)
  (let (sum (+ a b))
    (if (or (and (> a 0) (> b 0) (< sum 0))
            (and (< a 0) (< b 0) (> sum 0)))
      nil
      sum)))

(add-overflow MAX-INT 1)
;-> nil
(add-overflow MIN-INT -1)
;-> nil

(add-overflow 9223372036854775800 7)
;-> 9223372036854775807

(add-overflow -9223372036854775800 -8)
;-> -9223372036854775808


-------------------
Fabbisogno calorico
-------------------

Equazioni di Harris-Benedict
----------------------------
Una delle formule più comuni per calcolare il fabbisogno calorico è la formula di Harris-Benedict, che tiene conto dei parametri peso corporeo, altezza, età, sesso e livello di attività fisica.

1) Calcolo del metabolismo basale
Iniziamo calcolando il metabolismo basale (per uomini e donne):

MB(uomini) = 66.4730 + (13.7516 * peso in kg) + (5.0033 * altezza in cm) – (6.7550 * età in anni)
MB(donne)  = 655.0955 + (9.5634 * peso in kg) + (1.8496 * altezza in cm) – (4.6756 * età in anni)

Le formule restituiscono un valore in kcal (kilo-calorie).

2) Calcolo del livello di attività fisica
Adesso considerariamo il livello di attività fisica, moltiplicando il metabolismo basale per un coefficiente che rappresenta il grado di attività:

Sedentari: MB * 1.45
attività svolte da seduti o in piedi senza spostamenti, spostamenti con veicoli a motore.

Leggermente attivi: MB * 1.60
attività con spostamento del corpo per tempo non troppo lungo. Attività fisica 3-5 volte a settimana per 1 ora.

Attivi: MB * 1.75
attività con movimento di tutto il corpo ogni giorno per diverse ore o attività sportive svolte tutti i giorni per 2 ore.

Molto attivi: MB * 2.10
sportivi agonisti, attività sportive tutti i giorni per più di 2 ore, attività con movimento di tutto il corpo e impiego di intensa attività muscolare.

Questi valori si riferiscono in particolare a persone di età inferiore a 60 anni.

A seconda dell’obiettivo da raggiungere, si possono aggiungere o sottrarre calorie dal risultato:
- per mantenere il peso: mantenere il fabbisogno calorico come calcolato
- per dimagrire: ridurre il fabbisogno calorico totale del 10-25%
- per aumentare la massa muscolare: aumentare il fabbisogno calorico del 10-20%

(define (calorico peso altezza eta attivita genere)
  (local (mb fb)
    (cond ((or (= genere "M") (= genere 'M))
            (setq mb (add 66.4730 (mul 13.7516 peso) (mul 5.0033 altezza) (mul -6.7550 eta)))
            (setq fb (mul mb attivita))
            (println "Maschio"))
          (true
            (setq mb (add 655.0955 (mul 9.5634 peso) (mul 1.8496 altezza) (mul -4.6756 eta)))
            (setq fb (mul mb attivita))
            (println "Femmina")))
    (println "Metabolismo basale: " (int mb))
    (println "Fabbisogno calorico: " (int fb)) '>))

Proviamo:

(calorico 74 180 42 1.6 'M)
;-> Maschio
;-> Metabolismo basale: 1700
;-> Fabbisogno calorico: 2721

(calorico 74 180 42 1.6 'F)
;-> Femmina
;-> Metabolismo basale: 1499
;-> Fabbisogno calorico: 2398

Equazioni di Harris-Benedict riviste da Roza e Shizgal (1984)
-------------------------------------------------------------
MB(uomini) = (13.397 * peso in kg) + (4.799 * altezza in cm) – (5.677 * età in anni) + 88.362
MB(donne)  = (9.247 * peso in kg) + (3.098 * altezza in cm) – (4.330 * età in anni) + 447.593

L'intervallo di confidenza al 95% per gli uomini è di +/-213 kcal/giorno e di +/-201 kcal/giorno per le donne.

Equazioni di Harris-Benedict riviste da Mifflin e St Jeor (1990)
----------------------------------------------------------------
MB(uomini) = (10 * peso in kg) + (6.25 * altezza in cm) – (5 * età in anni) + 5
MB(donne)  = (10 * peso in kg) + (6.25 * altezza in cm) – (5 * età in anni) – 161

Funzione che calcola il fabbisogno calorico con tutti e tre i metodi:

(define (calorico peso altezza eta attivita genere)
  (local (mb fb)
    (cond ((or (= genere "M") (= genere 'M))
            (setq mb1 (add 66.4730 (mul 13.7516 peso) (mul 5.0033 altezza) (mul -6.755 eta)))
            (setq mb2 (add 88.362 (mul 13.397 peso) (mul 4.799 altezza) (mul -5.677 eta)))
            (setq mb3 (add 5 (mul 10 peso) (mul 6.25 altezza) (mul -5 eta)))
            (setq fb1 (mul mb1 attivita))
            (setq fb2 (mul mb2 attivita))
            (setq fb3 (mul mb3 attivita))
            (println "Maschio"))
          (true
            (setq mb1 (add 655.0955 (mul 9.5634 peso) (mul 1.8496 altezza) (mul -4.6756 eta)))
            (setq mb2 (add 447.593 (mul 9.247 peso) (mul 3.098 altezza) (mul -4.330 eta)))
            (setq mb3 (add (- 161) (mul 10 peso) (mul 6.25 altezza) (mul -5 eta)))
            (setq fb1 (mul mb1 attivita))
            (setq fb2 (mul mb2 attivita))
            (setq fb3 (mul mb3 attivita))
            (println "Femmina")))
    (println "Metabolismo basale (1): " (int mb1))
    (println "Metabolismo basale (2): " (int mb2))
    (println "Metabolismo basale (3): " (int mb3))
    (println "Fabbisogno calorico (1): " (int fb1))
    (println "Fabbisogno calorico (2): " (int fb2))
    (println "Fabbisogno calorico (3): " (int fb3)) '>))

Proviamo:
(calorico 74 180 42 1.6 'M)
;-> Maschio
;-> Metabolismo basale (1): 1700
;-> Metabolismo basale (2): 1705
;-> Metabolismo basale (3): 1660
;-> Fabbisogno calorico (1): 2721
;-> Fabbisogno calorico (2): 2728
;-> Fabbisogno calorico (3): 2656

(calorico 74 180 42 1.6 'F)
;-> Femmina
;-> Metabolismo basale (1): 1499
;-> Metabolismo basale (2): 1507
;-> Metabolismo basale (3): 1494
;-> Fabbisogno calorico (1): 2398
;-> Fabbisogno calorico (2): 2412
;-> Fabbisogno calorico (3): 2390

Nota: Il valore calcolato dalle formule è solo una stima del "fabbisogno calorico".
Per un calcolo preciso occorre rivolgersi ad uno specialista che terrà conto di molti altri fattori (indice di massa grassa, condizioni di salute, ecc.)


---------------------------------
Somma di numeri con bit invertiti
---------------------------------

Dato un numero N determinare la seguente somma:
1) Inizialmente la somma vale N
2) Considerare la rappresentazione binaria di N
3) Aggiungere alla somma tutti i numeri che si ottengono invertendo un bit di N

Esempio:
  N = 21
  Somma = 21
  N(base 2) = 10101
  Cominciando dal bit di destra:
    Inversione bit 0: 10101 --> 10100 = 20
    Inversione bit 1: 10101 --> 10111 = 23
    Inversione bit 2: 10101 --> 10001 = 17
    Inversione bit 3: 10101 --> 11101 = 29
    Inversione bit 4: 10101 --> 00101 = 5
  Somma = 21 + 20 + 23 + 17 + 29 + 5 = 115 

Metodo 1 (stringa binaria)
--------------------------

(define (somma1 num)
  (letn ( (sum num) (binary (bits num)) (len (length binary)) )
    ;(println "  " num { } binary)
    (for (idx 0 (- len 1))
      (setq tmp binary)
      ; flip idx-th bit, (flip 48 <--> 49, "0" --> "1")
      (setf (tmp idx) (char (- (+ 49 48) (char (tmp idx)))))
      ;(println idx { } (int tmp 0 2) { } tmp)
      (setq sum (+ sum (int tmp 0 2)))
    )
    sum))

Proviamo:

(somma1 21)
;-> 115

(map somma1 (sequence 0 30))
;-> (1 1 5 6 15 17 19 21 39 42 45 48 51 54 57 60 95 99 103 107 111 115
;->  119 123 127 131 135 139 143 147 151)

Metodo 2 (bit manipulation)
---------------------------
Funzione che inverte il bit all'indice idx di un numero intero num:

(define (toggle-bit num idx) (^ num (<< 1 idx)))

(define (somma2 num)
  (let ( (sum num) (len (length (bits num))) )
    ;(println "  " num { } (bits num))
    (for (idx 0 (- len 1))
      ;(setq sum (+ sum (toggle-bit num idx)))
      (setq sum (+ sum (^ num (<< 1 idx))))
      ;(println idx { } (^ num (<< 1 idx)) { } (bits (^ num (<< 1 idx))))
    )
  sum))

Proviamo:

(somma2 21)
;-> 115

(map somma2 (sequence 0 50))
;-> (1 1 5 6 15 17 19 21 39 42 45 48 51 54 57 60 95 99 103 107 111 115
;->  119 123 127 131 135 139 143 147 151)

Test di correttezza:

(= (map somma1 (sequence 0 1e5)) (map somma2 (sequence 0 1e5)))
;-> true

Test di velocità:

(time (map somma1 (sequence 1 1e4)))
;-> 109.379
(time (map somma1 (sequence 1 1e5)))
;-> 1250.04

(time (map somma2 (sequence 1 1e4)))
;-> 15.621
(time (map somma2 (sequence 1 1e5)))
;-> 156.483

La seconda funzione è circa 8 volte più veloce.

Metodo 3 (matematica)
---------------------
Analizziamo la sequenza:

(setq S (map somma2 (sequence 0 256)))
;-> (1 1 5 6 15 17 19 21 39 42 45 48 51 54 57 60 95 99 103 107 111 115
;->  119 123 127 131 135 139 143 147 151 155 223 228 233 238 243 248 253
;->  258 263 268 273 278 283 288 293 298 303 308 313 318 323 328 333 338
;->  343 348 353 358 363 368 373 378 511 517 523 529 535 541 547 553 559
;->  565 571 577 583 589 595 601 607 613 619 625 631 637 643 649 655 661
;->  667 673 679 685 691 697 703 709 715 721 727 733 739 745 751 757 763
;->  769 775 781 787 793 799 805 811 817 823 829 835 841 847 853 859 865
;->  871 877 883 889 1151 1158 1165 1172 1179 1186 1193 1200 1207 1214
;->  1221 1228 1235 1242 1249 1256 1263 1270 1277 1284 1291 1298 1305
;->  1312 1319 1326 1333 1340 1347 1354 1361 1368 1375 1382 1389 1396
;->  1403 1410 1417 1424 1431 1438 1445 1452 1459 1466 1473 1480 1487
;->  1494 1501 1508 1515 1522 1529 1536 1543 1550 1557 1564 1571 1578
;->  1585 1592 1599 1606 1613 1620 1627 1634 1641 1648 1655 1662 1669
;->  1676 1683 1690 1697 1704 1711 1718 1725 1732 1739 1746 1753 1760
;->  1767 1774 1781 1788 1795 1802 1809 1816 1823 1830 1837 1844 1851
;->  1858 1865 1872 1879 1886 1893 1900 1907 1914 1921 1928 1935 1942
;->  1949 1956 1963 1970 1977 1984 1991 1998 2005 2012 2019 2026 2033
;->  2040 2559)

Calcoliamo la differenza tra elementi consecutivi della sequenza S(i) - S(i-1):

(setq diff (map - (rest S) (chop S)))
;-> (0 4 1 9 2 2 2 18 3 3 3 3 3 3 3 35 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 68
;->  5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 133
;->  6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
;->  6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 262 7
;->  7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
;->  7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
;->  7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
;->  7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 519)

Senza considerare i primi quattro termini della sequenza (0 4 1 9), le ripetizioni di numeri uguali hanno le seguenti lunghezze:

(count '(2 3 4 5 6 7) diff)
;-> (3 7 16 31 63 127)
Il 4 ha 16 occorrenze perchè conta anche quello alla posizione 1.

  2 2 2 --> lunghezza = 3
  3 3 3 3 3 3 3 ---> lunghezza = 7
  4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ---> lunghezza = 15
  6 6 6 6 6 6 6 ... 6 6 6 6 6 6 ---> lunghezza = 31
  7 7 7 7 7 7 7 ... 7 7 7 7 7 7 ---> lunghezza = 63

Partendo con k = 2, queste lunghezze valgono: 2^k - 1

I numeri 'diversi' che compaiono dopo le sequenze di numeri ripetuti valgono:

  18 35 68 133 262 519 ...

Notiamo che questi numeri valgono: 2^(w+1) + w - 1) (dove w = k + 1)

(define (cur-val w) (+ (pow 2 (+ w 1)) w -1))
(map cur-val (sequence 3 8))
;-> (18 35 68 133 262 519)

A questo punto siamo in grado di definire una funzione che calcola i primi n numeri di S(n).

Prima scriviamo la funzione che calcola i primi n numeri della sequenza delle differenze D(i):

(define (D n)
  (local (diff idx k duplica cur)
    ; funzione che calcola elemento 'diverso'
    (define (cur-val k) (+ (pow 2 (+ k 1)) k -1))
    ; sequenza iniziale
    (setq diff '(0 4 1 9))
    ; indice iniziale
    (setq idx 4)
    ; blocco iniziale
    (setq k 2)
    ; valore iniziale da duplicare
    (setq duplica 2)
    ; ciclo per creare la sequenza D(n)
    (while (< idx n)
      ; elementi duplicati da aggiungere
      (setq cur (dup duplica (- (pow 2 k) 1)))
      ; aggiunge elementi duplicati alla sequenza
      (extend diff cur)
      ; calcola e aggiunge elemento 'diverso'
      (push (cur-val (+ k 1)) diff -1)
      ; aggiorna indici e valori per il prossimo inserimento
      (setq idx (+ idx (pow 2 (- k 1))))
      (++ k)
      (++ duplica)
    )
    ; seleziona solo i primi n elementi di D(n)
    (slice diff 0 n)))

(D 30)
;-> (0 4 1 9 2 2 2 18 3 3 3 3 3 3 3 35 4 4 4 4 4 4 4 4 4 4 4 4 4 4)

Adesso scriviamo la funzione che calcola i primi n numeri della sequenza S(i):

(define (S n)
  (local (base seq diff)
    ; elemento/numero di partenza
    (setq base 1)
    ; sequenza di partenza
    (setq seq '(1))
    ; calcola sequenza delle differenze D(n)
    (setq diff (D n))
    ; crea S(n) tramite somma cumulativa
    (dolist (el diff)
      (setq base (+ base el))
      (push base seq -1)
    )
    seq))

Proviamo:

(S 30)
;-> (1 1 5 6 15 17 19 21 39 42 45 48 51 54 57 60 95 99 103 107 111 115 119
;->  123 127 131 135 139 143 147 151)

Test di correttezza:

(= (map somma2 (sequence 0 1e5)) (S 1e5))
;-> true

Test di velocità:

(time (map somma2 (sequence 0 1e5)) 10)
;-> 1640.848
(time (S 1e5) 10)
;-> 156.154

La funzione con il metodo matematico è 10 volte più veloce.

Nota: questa sequenza non esiste in OEIS.


------------------------------------
Numeri con una cifra diversa da zero
------------------------------------

Scrivere una funzione che calcola la sequenza di numeri che contengono solo una cifra diversa da zero.

Sequenza OEIS A037124:
Numbers that contain only one nonzero digit.
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100,
  200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000,
  6000, 7000, 8000, 9000, 10000, 20000, 30000, 40000, 50000, 60000, 70000,
  80000, 90000, 100000, ...

Formula: a(n) = (((n - 1) mod 9) + 1) * 10^floor((n - 1)/9)

(define (** num power)
"Calculates the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (A n)
  (* (+ 1L (% (- n 1) 9)) (** 10 (/ (- n 1) 9))))

Proviamo:

(map A (sequence 1 50))
;-> (1L 2L 3L 4L 5L 6L 7L 8L 9L 10L 20L 30L 40L 50L 60L 70L 80L 90L 100L
;->  200L 300L 400L 500L 600L 700L 800L 900L 1000L 2000L 3000L 4000L 5000L
;->  6000L 7000L 8000L 9000L 10000L 20000L 30000L 40000L 50000L 60000L 70000L
;->  80000L 90000L 100000L 200000L 300000L 400000L 500000L)


-------------------------------
newLISP e BC (Basic Calculator)
-------------------------------

bc sta per "basic calculator" (calcolatrice di base), è "un linguaggio di calcolo a precisione arbitraria" con una sintassi simile al linguaggio di programmazione C.
bc è in genere utilizzato come linguaggio di scripting matematico.
Un tipico utilizzo interattivo consiste nel digitare il comando bc su un prompt dei comandi e inserire un'espressione matematica, come (1 + 3) * 2, che restituirà 8.
Sebbene bc possa funzionare con una precisione arbitraria, in realtà imposta di default zero cifre dopo la virgola, quindi l'espressione 2/3 restituisce 0 (i risultati vengono troncati, non arrotondati).
L'opzione -l di bc imposta la scala predefinita (precisione) a 20 e aggiunge diverse funzioni matematiche aggiuntive al linguaggio.

Per interrompere un calcolo in esecuzione (perché sta impiegando più tempo del previsto), premere Ctrl C.
Per uscire e uscire da bc, digitare il comando quit o premere Ctrl D.

Nota: per windows occorre scaricare il programma da
https://gnuwin32.sourceforge.net/packages/bc.htm

Informazioni utili su bc:
http://www.phodd.net/gnu-bc/
http://www.numbertheory.org/gnubc/bc_programs.html
https://org.coloradomesa.edu/~mapierce2/bc/
https://github.com/mikepierce/GNU-bc-Functions
https://embedeo.org/ws/command_line/bc_dc_calculator_windows/

Opzioni della linea di comando
------------------------------
bc accetta le seguenti opzioni dalla riga di comando:

  -h, --help
  Print the usage and exit.
  -l, --mathlib
  Define the standard math library (scale = 20).
  -w, --warn
  Give warnings for extensions to POSIX bc.
  -s, --standard
  Process exactly the POSIX bc language.
  -q, --quiet
  Do not print the normal GNU bc welcome.
  -v, --version
  Print the version number and copyright and quit.

Libreria matematica (Math library)
-----------------------------------
Oltre al supporto per operazioni aritmetiche di base come addizione, moltiplicazione, elevamento a potenza di interi, ecc., l'unica funzione matematica integrata in bc è sqrt().
Caricando la libreria matematica di bc con l'opzione -l vengono definite le seguenti funzioni aggiuntive:

  s(x), the sine of x, for x in radians.
  c(x), the cosine of x, for x in radians.
  a(x), the arctangent of x, for x in radians.
  l(x), the natural logarithm of x.
  e(x), the exponential function of raising e to the value x.
  j(n,x), the bessel function of integer order n of x.

Funzioni di base
----------------
Possiamo scrivere delle funzioni matematiche in un file (es. functions.bc) da poter caricare al lancio di bc:

  bc -l functions.bc

################
# functions.bc #
################

#################################
# Helpful Constants and Functions
#################################

pi=4*a(1)
ex=e(1)
define sgn(x) { if(x>0) return 1; if(x<0) return -1; }
define abs(x) { return sgn(x)*x }
define heavyside(x) { return (x>0) }

# Return the integer-part of a number (not the floor)
define int(x) { auto s; s=scale; scale=0; x/=1; scale=s; return x; }

###########################
# Logarithms & Exponentials
###########################

define ln(x) { return l(x) }
define log(x) { return l(x)/l(10) }
define logb(x,b) { return l(x)/l(b) }
define pow(x,n) { return e(n*l(x)) }

##############
# Trigonometry
##############

define rad2deg(x) { return x*(45/a(1)) }
define deg2rad(x) { return x*(a(1)/45) }

define cos(x) { return c(x) }
define sin(x) { return s(x) }
define tan(x) { return s(x)/c(x) }
define sec(x) { return 1/c(x) }
define csc(x) { return 1/s(x) }
define cot(x) { return c(x)/s(x) }
define arccos(x) {
    pi = 4*a(1)
    if(x == 1) return 0
    if(x == -1) return pi 
    return pi/2-a(x/sqrt(1-(x^2)))
}
define arcsin(x) { 
    pi = 4*a(1)
    if(x == 1) return pi/2
    if(x == -1) return -pi/2
    return sgn(x)*a(sqrt((1/(1-(x^2)))-1))
}
define arctan(x) { return a(x) }
define atan2(y,x) { 
    pi = 4*a(1)
    if(x == 0) return sgn(y)*pi/2
    if(x < 0) {
        if(y < 0)
            return a(y/x)-pi
        return a(y/x)+pi
    }
    return a(y/x)
}
define arcsec(x) { return arccos(1/x) }
define arccsc(x) { return arcsin(1/x) }
define arccot(x) { return (4*a(1))/2-a(x) }

define cosh(x) { return (e(x)+e(-x))/2 }
define sinh(x) { return (e(x)-e(-x))/2 }
define tanh(x) { return sinh(x)/cosh(x) }
define sech(x) { return 1/cosh(x) }
define csch(x) { return 1/sinh(x) }
define coth(x) { return 1/tanh(x) }

define arcosh(x) { return ln(x+sqrt(x^2-1)) }
define arsinh(x) { return ln(x+sqrt(x^2+1)) }
define artanh(x) { return (1/2)*ln((1+x)/(1-x))}
define arsech(x) { return arcosh(1/x) }
define arcsch(x) { return arsinh(1/x) }
define arcoth(x) { return artanh(1/x) }

bc per Windows
--------------
Oltre a GNU bc, per Windows è anche possibile scaricare il file bc-1.07.1-win32-embedeo-02.zip (~135 KB) da:
https://embedeo.org/ws/command_line/bc_dc_calculator_windows/
Contiene i binari predefiniti di GNU bc e dc, eseguibili direttamente su Windows.
È quindi possibile decomprimere il file e aggiungere il percorso della sottodirectory bin alla variabile PATH.
L'utilizzo di bc in modalità interattiva su Windows è molto simile all'esempio precedente.
Esempio di uso di bc in modalità non interattiva:
D:\test\bc>set MYVAR=2048

D:\test\bc>echo %MYVAR%
2048

D:\test\bc>echo sqrt (%MYVAR% / 2)  | bc -l
32.00000000000000000000

D:\test\bc>echo 100 + 22 / 7 > tmp.txt

D:\test\bc>type tmp.txt
100 + 22 / 7

D:\test\bc>bc -l < tmp.txt
103.14285714285714285714

Utilizzo di bc con newLISP
--------------------------
Prerequisiti:
1) Installazione di bc
2) Percorso di bc.exe nel PATH oppure bc.exe nella cartella di lavoro corrente di newLISP.

bc non accetta un espressione nella riga di comando, ma prende un file di testo.
Quindi possiamo creare un file con i comandi che devono essere eseguiti da bc.

Esempio (test.bc):

# file: test.bc
# line comment
# set input and output base to 10 (A=10)
obase=ibase=A
# simple operation
3 + 4
# set precision (at start scale = 0)
scale=50
# simple operation
10345 / 232844
scale=20
# define a user function
define phi() {return((1+sqrt(5))/2)} ; phi = phi()
# call user function
phi;
# set a variable
conta = 0
# print a variable
print "conta = ", a
# quit bc
quit

Cambiamo la cartella di lavoro di newLISP:

(change-dir "f:\\Lisp-Scheme\\BC")
;-> true

Eseguiamo il programma bc con il nostro file:

(exec "bc.exe -l test.bc")
;-> ("7" ".04442888801085705450859803129992613080002061466045"
;->  "1.61803398874989484820" "conta = 0")

I risultati (di tipo stringa) vengono restituiti come lista.


------------------------------------------
Numero massimo e minimo con gli stessi bit
------------------------------------------

Dato un numero N determinare il numero piu grande e il numero più piccolo che ha lo stesso numero di bit a 1 e a 0 di N.

Il numero più grande con lo stesso numero di bit a 1 (e 0) di N si ottiene spostando tutti i bit a 1 verso le posizioni più significative (a sinistra).

Il numero più piccolo con lo stesso numero di bit a 1 (e 0) di N si ottiene spostando tutti i bit a 1 verso le posizioni più significative (a destra).

Algoritmo:
1) Usiamo (bits N) per convertire N in una stringa binaria (es. N=22 --> "10110").
2) Usiamo (find-all "1") per trovare tutte le occorrenze di "1", poi (length) per contare 'ones' quanti sono i bit a 1.
3) 'len' è la lunghezza della rappresentazione binaria di N.
4) Costruiamo un numero con 'ones' bit a 1: (pow 2 ones) - 1, (es. 7 per 3 bit a 1 (111).
5) Per trovare il numero massimo:
   Poi lo shiftiamo verso sinistra per mettere gli 1 nelle posizioni più alte (es. 11100 --> N = 28).
6) Per trovare il numero minimo:
(pow 2 ones) - 1 genera il numero con 'ones' bit a 1 tutti a destra (es. 00111 --> N = 7).

(define (max-same-bits n)
  (letn ( (binary (bits n))
          (ones (length (find-all "1" binary)))
          (len (length binary)) )
    (<< (- (pow 2 ones) 1) (- len ones))))

Proviamo:

(max-same-bits 22)
;-> 28
(bits 22)
;-> 10110
(bits 28)
;-> "11100"

(max-same-bits 111)
;-> 126
(bits 111)
;-> "1101111"
(bits 126)
;-> "1111110"

(max-same-bits 7)
;-> 7

(map max-same-bits (sequence 0 55))
;-> (0 1 2 3 4 6 6 7 8 12 12 14 12 14 14 15 16 24 24 28
;->  24 28 28 30 24 28 28 30 28 30 30 31 32 48 48 56 48 56
;->  56 60 48 56 56 60 56 60 60 62 48 56 56 60 56 60 60 62)

Sequenza OEIS A073138:
Largest number having in its binary representation the same number of 0's and 1's as n.
  0, 1, 2, 3, 4, 6, 6, 7, 8, 12, 12, 14, 12, 14, 14, 15, 16, 24, 24, 28,
  24, 28, 28, 30, 24, 28, 28, 30, 28, 30, 30, 31, 32, 48, 48, 56, 48, 56,
  56, 60, 48, 56, 56, 60, 56, 60, 60, 62, 48, 56, 56, 60, 56, 60, 60, 62,
  56, 60, 60, 62, 60, 62, 62, 63, 64, 96, 96, 112, 96, 112, 112, ...

(define (min-same-bits n)
  (letn ( (binary (bits n))
          (ones (length (find-all "1" binary)))
          (len (length binary)) )
    (- (pow 2 ones) 1)))

Proviamo:

(min-same-bits 22)
;-> 7
(bits 22)
;-> "10110"
(bits 7)
;-> "111"

(min-same-bits 12)
;-> 3
(bits 12)
;-> "1100"
(bits 3)
;-> "11"

(map min-same-bits (sequence 0 55))
;-> (0 1 1 3 1 3 3 7 1 3 3 7 3 7 7 15 1 3 3 7 3 7 7 15 3 7 7 15 7 15 15 31
;->  1 3 3 7 3 7 7 15 3 7 7 15 7 15 15 31 3 7 7 15 7 15 15 31)

Sequenza OEIS A073137:
a(n) is the least number whose binary representation has the same number of 0's and 1's as n.
  0, 1, 2, 3, 4, 5, 5, 7, 8, 9, 9, 11, 9, 11, 11, 15, 16, 17, 17, 19, 17,
  19, 19, 23, 17, 19, 19, 23, 19, 23, 23, 31, 32, 33, 33, 35, 33, 35, 35,
  39, 33, 35, 35, 39, 35, 39, 39, 47, 33, 35, 35, 39, 35, 39, 39, 47, 35,
  39, 39, 47, 39, 47, 47, 63, 64, 65, 65, 67, 65, 67, 67, 71, 65, ...

Come si nota la sequenza OEIS è diversa da quella generata dalla funzione "min-same-bits", questo perchè la sequenza OEIS calcola il numero più piccolo che abbia lo stesso numero di 0 e 1 nella rappresentazione binaria di N, ma il numero deve avere lo stesso numero totale di bit della rappresentazione di N.
Esempio:
N = 12 = "1100", 2 zero e 2 uno --> minimo = "1001" = 9 (4 bit).
La funzione (min-same-bits 12) invece restituisce 3 = "11" (2 bit).

Trovare il numero più piccolo con lo stesso numero di bit 0 e 1 (nella stessa lunghezza binaria di N), assicurandosi che il primo bit sia 1 (cioè, nessuno zero iniziale), e con gli altri 1 spostati più a destra possibile.

Funzione che calcola correttamente la sequenza OEIS:

(define (minimo N)
  (if (zero? N) 0
    (letn ( (bin (bits N)) ; binario di N
            (len (length bin)) ; lunghezza binario di N
            (zeros (length (find-all "0" bin))) ; numero di 0
            (unos (- len zeros)) ; numero di 1
            ; crea la stringa binaria minima della stessa lunghezza
            (binary (string "1" (dup "0" zeros) (dup "1" (- unos 1)))) )
      (int binary 0 2))))

Proviamo:

(minimo 12)
;-> 9
(bits 12)
;-> "1100"
(bits 9)
;-> "1001"

(map minimo (sequence 0 72))
;-> (0 1 2 3 4 5 5 7 8 9 9 11 9 11 11 15 16 17 17 19 17
;->  19 19 23 17 19 19 23 19 23 23 31 32 33 33 35 33 35 35
;->  39 33 35 35 39 35 39 39 47 33 35 35 39 35 39 39 47 35
;->  39 39 47 39 47 47 63 64 65 65 67 65 67 67 71 65)


----------------
Coppie e singoli
----------------

Data una lista di numeri interi, determinare tutte le coppie e tutti gli elementi singoli.

Esempio:
  lista = 1 1 2 2 2 5 4 5 4 3
  coppie = (1 1) (2 2) (4 4) (5 5)
  singoli = (2 3)

(define (coppie-singoli lst)
  (local (pair single unici link num rep)
    (setq pair '())
    (setq single '())
    ; crea la lista dei numeri unici
    (setq unici (unique lst))
    ; crea una lista con coppie (numero ripetizioni)
    (setq link (sort (map list unici (count unici lst))))
    (dolist (el link)
      (setq num (el 0))
      (setq rep (el 1))
      (cond
        ((= rep 1)
          ; una sola ripetizione --> aggiunge il numero a single
          (push num single -1))
        ((even? rep)
          ; ripetizioni pari --> aggiunge ripetizioni/2 coppie a pair
          (extend pair (dup (list num num) (/ rep 2))))
        ((odd? rep)
          ; ripetizioni dispari --> aggiunge ripetizioni/2 coppie a pair e
          (extend pair (dup (list num num) (/ rep 2)))
          ; aggiunge il numero a single
          (push num single -1))))
    (list pair single)))

Proviamo:

(setq a '(1 1 2 2 2 5 4 5 4 3))
(coppie-singoli a)
;-> (((1 1) (2 2) (4 4) (5 5)) (2 3))
(coppie-singoli '(1 1 2 2))
;-> (((1 1) (2 2)) ())
(coppie-singoli '(1 2 3))
;-> (() (1 2 3))
(coppie-singoli '())
;-> (() ())


------------------------
Numeri binari palindromi
------------------------

Scrivere una funzione che restituisce la sequenza dei numeri binari palindromi.

Sequenza OEIS A006995:
Binary palindromes: numbers whose binary expansion is palindromic.
  0, 1, 3, 5, 7, 9, 15, 17, 21, 27, 31, 33, 45, 51, 63, 65, 73, 85, 93,
  99, 107, 119, 127, 129, 153, 165, 189, 195, 219, 231, 255, 257, 273,
  297, 313, 325, 341, 365, 381, 387, 403, 427, 443, 455, 471, 495, 511,
  513, 561, 585, 633, 645, 693, 717, 765, 771, 819, 843, ...

Funzione che restituisce la sequenza dei numeri binari palindromi:

(define (bin-pal limite binary)
  (let ( (out '()) (num-pali 0) (num 0) (bin "") )
    (until (= num-pali limite)
      (setq bin (bits num))
      ;(when (= bin (reverse (bits num))) ; slower
      (when (= bin (reverse (copy bin)))
        (if binary 
            (push bin out -1)
            (push num out -1))
        (++ num-pali))
      (++ num))
    out))

Proviamo:

(bin-pal 58)
;-> (0 1 3 5 7 9 15 17 21 27 31 33 45 51 63 65 73 85 93
;->  99 107 119 127 129 153 165 189 195 219 231 255 257 273
;->  297 313 325 341 365 381 387 403 427 443 455 471 495 511
;->  513 561 585 633 645 693 717 765 771 819 843)

(bin-pal 58 true)
;-> ("0" "1" "11" "101" "111" "1001" "1111" "10001" "10101" "11011"
;->  "11111" "100001" "101101" "110011" "111111" "1000001" "1001001"
;->  "1010101" "1011101" "1100011" "1101011" "1110111" "1111111"
;->  "10000001" "10011001" "10100101" "10111101" "11000011" "11011011"
;->  "11100111" "11111111" "100000001" "100010001" "100101001" "100111001"
;->  "101000101" "101010101" "101101101" "101111101" "110000011" "110010011"
;->  "110101011" "110111011" "111000111" "111010111" "111101111" "111111111"
;->  "1000000001" "1000110001" "1001001001" "1001111001" "1010000101"
;->  "1010110101" "1011001101" "1011111101" "1100000011" "1100110011"
;->  "1101001011")

La funzione è molto lenta.
(time (bin-pal 1e4))
;-> 6578.668

Un altro metodo per trovare i primi n numeri palindromi binari è quello di costruire i palindromi dal primo all'n-esimo come stringhe.

L'algoritmo usato si basa su una "generazione incrementale" dei numeri binari palindromi tramite una coda (queue).
L'idea è di partire da un palindromo semplice ("11") e costruirne di nuovi inserendo cifre nel mezzo delle stringhe già palindrome, mantenendo sempre la simmetria.

1. Caso base: il primo palindromo binario è "1", corrispondente al numero decimale 1.

2. Coda: iniziamo con ("11") e ogni ciclo rimuove un elemento dalla fine della lista e inserisce nuovi palindromi all'inizio, simulando una coda FIFO.

3. Generazione dei nuovi palindromi:
   - Se la lunghezza è pari, si inserisce "0" e "1" al centro della stringa per generare due nuovi palindromi.
   - Se la lunghezza è dispari, si duplica il carattere centrale per ottenere un nuovo palindromo.

4. Ripetere finché non abbiamo generato l'n-esimo palindromo.

Funzione che restituisce i primi n numeri binari palindromi:

(define (bin-pal2 n)
  (cond
    ; Casi base: 0 e 1 (il primo palindromo è "1")
    ((= n 0) '(0))
    ((= n 1) '(0 1))
    ; Casi n > 1:
    (true
    (local (q len curr midChar result trovato left right mid out)
      (setq out '(0 1)) ; Lista delle soluzioni (binari palindromi)
      (setq n (- n 1))  ; Decrementa n perché abbiamo già gestito il primo
      (setq q '("11"))  ; Inizia con il primo palindromo di lunghezza 2
      (setq trovato nil)
      ; Continua finché non trova l'n-esimo palindromo
      (while (and (not trovato) (> (length q) 0))
        (setq curr (pop q -1)) ; Estrae il prossimo palindromo dalla fine della coda
        (push (int curr 0 2) out -1) ; Inserisce il palindromo corrente nelle soluzioni
        (setq n (- n 1))
        (if (= n 0)
            ; Se è il palindromo cercato, converte in numero decimale
            (begin
              (setq result (int curr 0 2))
              (setq trovato true))
            ; Altrimenti genera nuovi palindromi da inserire in coda
            (begin
              (setq len (length curr))
              (setq left (slice curr 0 (/ len 2))) ; Parte sinistra
              (setq right (slice curr (/ len 2)))  ; Parte destra
              (if (= (mod len 2) 0)
                  ; Se lunghezza pari: inserisce "0" e "1" al centro
                  (begin
                    (push (string left "0" right) q 0)
                    (push (string left "1" right) q 0))
                  ; Se lunghezza dispari: duplica il carattere centrale
                  (begin
                    (setq mid (slice curr (/ len 2) 1))
                    (push (string left mid right) q 0))))))
        out))))

Proviamo:

(bin-pal2 58)
;-> (0 1 3 5 7 9 15 17 21 27 31 33 45 51 63 65 73 85 93
;->  99 107 119 127 129 153 165 189 195 219 231 255 257 273
;->  297 313 325 341 365 381 387 403 427 443 455 471 495 511
;->  513 561 585 633 645 693 717 765 771 819 843 891)

(time (bin-pal2 1e4))
;-> 100.228
(time (bin-pal2 1e5))
;-> 9858.858


-------------------------------------------------------------------------
Somma, differenza, prodotto di numeri massimi e minimi con gli stessi bit
-------------------------------------------------------------------------

Dato un numero N determinare la differenza, la somma e il prodotto del numero più grande e del più piccolo che in rappresentazione binaria ha lo stesso numero di 0 e 1 di n.

Sequenza OEIS A073139:
Difference between the largest and smallest number having in binary representation the same number of 0's and 1's as n.
  0, 0, 0, 0, 0, 1, 1, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 7, 7, 9, 7, 9, 9,
  7, 7, 9, 9, 7, 9, 7, 7, 0, 0, 15, 15, 21, 15, 21, 21, 21, 15, 21, 21,
  21, 21, 21, 21, 15, 15, 21, 21, 21, 21, 21, 21, 15, 21, 21, 21, 15,
  21, 15, 15, 0, 0, 31, 31, 45, 31, 45, 45, 49, 31, 45, 45, 49, 45, 49,
  49, 45, 31, ...

Sequenza OEIS A073140:
Sum of the largest and smallest number having in binary representation the same number of 0's and 1's as n.
  0, 2, 4, 6, 8, 11, 11, 14, 16, 21, 21, 25, 21, 25, 25, 30, 32, 41, 41,
  47, 41, 47, 47, 53, 41, 47, 47, 53, 47, 53, 53, 62, 64, 81, 81, 91, 81
  , 91, 91, 99, 81, 91, 91, 99, 91, 99, 99, 109, 81, 91, 91, 99, 91, 99,
  99, 109, 91, 99, 99, 109, 99, 109, 109, 126, 128, 161, 161, 179, ...

Sequenza OEIS A073141:
Product of the largest and smallest number having in binary representation the same number of 0's and 1's as n.
  0, 1, 4, 9, 16, 30, 30, 49, 64, 108, 108, 154, 108, 154, 154, 225,
  256, 408, 408, 532, 408, 532, 532, 690, 408, 532, 532, 690, 532, 690,
  690, 961, 1024, 1584, 1584, 1960, 1584, 1960, 1960, 2340, 1584, 1960,
  1960, 2340, 1960, 2340, 2340, 2914, 1584, 1960, ...

(define (max-same-bits n)
  (letn ( (binary (bits n))
          (ones (length (find-all "1" binary)))
          (len (length binary)) )
    (<< (- (pow 2 ones) 1) (- len ones))))

(define (min-same-bits n)
  (if (zero? n) 0
    (letn ( (bin (bits n)) ; binario di n
            (len (length bin)) ; lunghezza binario di n
            (zeros (length (find-all "0" bin))) ; numero di 0
            (unos (- len zeros)) ; numero di 1
            ; crea la stringa binaria minima della stessa lunghezza
            (binary (string "1" (dup "0" zeros) (dup "1" (- unos 1)))) )
      (int binary 0 2))))

(define (A num tipo)
  (let (seq (sequence 0 num))
   (cond ((= tipo 0) ; differenza
           (map - (map max-same-bits seq)
                  (map min-same-bits seq)))
        ((= tipo 1) ; somma
           (map + (map max-same-bits seq)
                  (map min-same-bits seq)))
        ((= tipo 2) ; prodotto
           (map * (map max-same-bits seq)
                  (map min-same-bits seq))))))

Proviamo:

(A 65 0)
;-> (0 0 0 0 0 1 1 0 0 3 3 3 3 3 3 0 0 7 7 9 7 9 9 7 7 9 9 7 9 7 7 0 0
;->  15 15 21 15 21 21 21 15 21 21 21 21 21 21 15 15 21 21 21 21 21 21
;->  15 21 21 21 15 21 15 15 0 0 31)

(A 65 1)
;-> (0 2 4 6 8 11 11 14 16 21 21 25 21 25 25 30 32 41 41 47 41 47 47 53
;->  41 47 47 53 47 53 53 62 64 81 81 91 81 91 91 99 81 91 91 99 91 99
;->  99 109 81 91 91 99 91 99 99 109 91 99 99 109 99 109 109 126 128 161)

(A 65 2)
;-> (0 1 4 9 16 30 30 49 64 108 108 154 108 154 154 225 256 408 408 532
;->  408 532 532 690 408 532 532 690 532 690 690 961 1024 1584 1584 1960
;->  1584 1960 1960 2340 1584 1960 1960 2340 1960 2340 2340 2914 1584
;->  1960 1960 2340 1960 2340 2340 2914 1960 2340 2340 2914 2340 2914
;->  2914 3969 4096 6240)

Vedi anche "Numero massimo e minimo con gli stessi bit" su "Note libere 29".


-----------------------------------
Padding di liste e stringhe binarie
-----------------------------------

Data una lista o una stringa binaria e un numero di bit B, scrivere una funzione che restituisce una lista con B elementi.
Se la lista è più corta di B, allora occorre aggiungere a sinistra tanti 0 quanti ne sono necessari per avere una lista/stringa con B bit.
Se la lista è più lunga di B, allora prendere B elementi a destra della lista/stringa.

Esempi:

  lista binaria (1 1 0)
  bit = 8
  Output = (0 0 0 0 0 1 1 0)

  stringa binaria = "101011"
  bit = 8 
  output = "00101011"
  
  stringa binaria = "110010"  
  bit = 4
  output = "0010"

(define (pad-binary binary bit)
  (let (len (length binary))
    (cond ((= len bit) binary)
          ((> len bit) (slice binary (- bit) bit))
          (true
            (if (list? binary)
                (extend (dup 0 (- bit len)) binary)
                (extend (dup "0" (- bit len)) binary))))))

Proviamo:

(pad-binary '(1 1 0 1) 8)
;-> (0 0 0 0 1 1 0 1)
(pad-binary '(1 1 0 1) 4)
;-> (1 1 0 1)
(pad-binary '(1 1 0 1) 2)
;-> (0 1)

(pad-binary "1101" 8)
;-> "00001101"
(pad-binary "1101" 4)
;-> "1101"
(pad-binary "1101" 2)
;-> "01"

(length (pad-binary '(1 0 1) 32))
;-> (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1)
(length (pad-binary '(1 0 1) 32))
;-> 32

Per le stringhe possiamo usare anche la funzione "format":

(setq fmt (string "%0" 16 "s"))
(format fmt "010101")
;-> "0000000000010101"

(define (pad-binary-string binary bit) (format (string "%0" bit "s") binary))

(pad-binary-string "101010111" 16)
;-> "0000000101010111"

Vediamo la velocità delle funzioni:

(time (pad-binary "01010101010101010101011111111" 64) 1e6)
;-> 510.126
(time (pad-binary-string "01010101010101010101011111111" 64) 1e6)
;-> 1259.932

La funzione che usa "format" è più lenta.


-----------------
Lista della spesa
-----------------

Data una lista della spesa determinare il costo totale.
Vediamo un esempio di lista della spesa:

  Oggetto     Quantità     Prezzo unitario
  Mele        10           1
  Carne       2            15
  Gelato      4            2
  Vino        5            10

Lista della spesa = (Mele 10 1) (Carne 2 15) (Gelato 4 2) (Vino 5 10)

(define (costo lst)
  (apply add (map (fn(x) (mul (x 1) (x 2))) lst)))

(setq a '(('Mele 10 1) ('Carne 2 15) ('Gelato 4 2) ('Vino 5 10)))
(costo a)
;-> 98

Componiamo le due liste Quantità e Prezzo: q = (10 2 4 5) e p = (1 15 2 10).
Adesso la soluzione è il prodotto scalare delle due liste:

  q(0)*p(0) + q(1)*p(1) + ... + q(n-1)*p(n-1)

(define (dot-product x y)
"Calculates the dot-product of two list/array of arbitrary length"
  (apply add (map mul x y)))

(dot-product '(10 2 4 5) '(1 15 2 10))
;-> 98


------------------------------------------------------------------
Distanza dalla più grande potenza di 2 minore o uguale a un numero
------------------------------------------------------------------

Per determinare la distanza dalla più grande potenza di 2 minore o uguale a un numero possiamo scrivi n in binario, cambiare la prima cifra in zero e poi riconvertirlo in decimale.

Sequenza OEIS A053645:
Distance to largest power of 2 less than or equal to n.
Write n in binary, change the first digit to zero, and convert back to decimal.
  0, 0, 1, 0, 1, 2, 3, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7,
  8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
  12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,
  29, 30, 31, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  17, 18, 19, 20, ...

MSB = Most Significative Bit

Funzione che resetta il MSB di un numero (bit operation):

(define (clear-msb num)
  (let ( (n num) (msb 1) )
    (while (> n 1)
      (setq n (>> n 1))
      (setq msb (<< msb 1)))
    (& num (- msb 1))))

(map clear-msb (sequence 1 64))
;-> (0 0 1 0 1 2 3 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
;->  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
;->  26 27 28 29 30 31 0)

Funzione che resetta il MSB di un numero (matematica):

(define (a n) (- n (pow 2 (floor (log n 2)))))

(map a (sequence 1 64))
;-> (0 0 1 0 1 2 3 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
;->  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
;->  26 27 28 29 30 31 0)


-------------------------------------
Confronto tra gli elementi di N liste
-------------------------------------

Date N liste con K numeri interi, costruire una lista con il metodo seguente:

- Considera le N-ple di numeri con lo stesso indice.
- Per ogni N-pla:
- se tutti i numeri hanno lo stesso valore,
    allora il valore corrente è la somma di tutti i numeri
    altrimenti il valore corrente è il massimo di tutti i numeri

Esempio:
 Lista 1 = (1 6 5 9 2 2 3)
 Lista 2 = (2 5 1 7 2 8 1)
 Lista 3 = (9 6 4 7 2 2 2)
 Output  = (9 6 5 9 6 8 3)

(define (confronta)
  (map (fn(x) (if (apply = x) (apply + x) (apply max x))) 
       (transpose (apply list (args)))))

Proviamo:

(setq a '(1 6 5 9 2 2 3))
(setq b '(2 5 1 7 2 8 1))
(setq c '(9 6 4 7 2 2 2))

(confronta a b c)
;-> (9 6 5 9 6 8 3)

(confronta a b)
;-> (2 6 5 9 4 8 3)


---------
Chisanbop
---------

Il Chisanbop è un metodo semplice per contare da zero a novantanove con due mani, inventato da Sung Jin Pai e rivisitato da suo figlio Hang Young Pai.
Per comporre i numeri, si sollevano zero, uno o più cifre e si sommano i loro valori.
Il pollice sinistro vale cinquanta. Le altre dita della sinistra valgono dieci.
Il pollice destro vale cinque. Le altre dita della destra valgono uno.
Le dita più vicine al pollice della mano si sollevano prima delle altre della stessa mano.

  Mano sinistra        Mano destra

  10 10 10 10 50       5  1  1  1  1
   |  |  |  |  |       |  |  |  |  |
   |  |  |  |  |       |  |  |  |  |
   n  a  m  i  p       p  i  m  a  n

  p = pollice
  i = indice
  m = medio
  a = anulare
  n = mignolo

Esempio
N = 18 ---> (SX i) (DX p i m a)
N = 26  ---> (SX m i)   (DX p i)

Scrivere una funzione che prende un numero intero e restituisce la lista delle dita necessarie per rappresentare quel numero mano sinistra e mano destra).

Utilizziamo un approccio greedy (avido), cioè proviamo a a costruire la somma aggiungendo i numeri più grandi possibili dalla lista, scartando quelli che farebbero superare N.
Inoltre, poiché i valori sono limitati in quantità, è necessario tenere traccia degli elementi ancora disponibili.

Rappresentiamo tutte le dita con una lista: (50 10 10 10 10 5 1 1 1 1)
I primi 5 valori sono la mano sinistra.
Gli ultimi 5 valori sono la mano destra.
La relativa lista delle dita vale: ("p" "i" "m" "a" "n" "p" "i" "m" "a" "n")

(define (chisanbop N)
  (let ( (sorted '(50 10 10 10 10 5 1 1 1 1))
         (finger '("p" "i" "m" "a" "n" "p" "i" "m" "a" "n"))
         (somma 0)
         (valori '())
         (hands '())
         (sx '())
         (dx '())
       )
    ; approccio greedy
    (dolist (x sorted)
      (if (<= (+ somma x) N)
        (begin
          ; prendiamo il valore corrente
          (push x valori -1)
          ; prendiamo l'indice del valore corrente
          (push $idx hands -1)
          (setq somma (+ somma x)))))
    ; se abbiamo creato il numero N...
    (when (= somma N)
        ; seleziona gli indici della mano sinistra
        (setq sx (filter (fn(x) (< x 5)) hands))
        ; seleziona gli indici della mano sinistra
        (setq dx (filter (fn(x) (>= x 5)) hands))
        ; seleziona le dita della mano sinistra
        (setq sx (select finger sx))
        ; seleziona le dita della mano destra
        (setq dx (select finger dx)))
    (list sx dx)))

Proviamo:

(chisanbop 26)
;-> (("i" "m") ("p" "i"))
(chisanbop 99)
;-> (("p" "i" "m" "a" "n") ("p" "i" "m" "a" "n"))
(chisanbop 0)
;-> (() ())

(for (i 0 99) (print i { } (chisanbop i)) (read-line))
;-> 1 (() ("i"))
;-> 2 (() ("i" "m"))
;-> 3 (() ("i" "m" "a"))
;-> 4 (() ("i" "m" "a" "n"))
;-> 5 (() ("p"))
;-> 6 (() ("p" "i"))
;-> 7 (() ("p" "i" "m"))
;-> 8 (() ("p" "i" "m" "a"))
;-> 9 (() ("p" "i" "m" "a" "n"))
;-> 10 (("i") ())
;-> 11 (("i") ("i"))
;-> 12 (("i") ("i" "m"))
;-> ...
;-> 94 (("p" "i" "m" "a" "n") ("i" "m" "a" "n"))
;-> 95 (("p" "i" "m" "a" "n") ("p"))
;-> 96 (("p" "i" "m" "a" "n") ("p" "i"))
;-> 97 (("p" "i" "m" "a" "n") ("p" "i" "m"))
;-> 98 (("p" "i" "m" "a" "n") ("p" "i" "m" "a"))
;-> 99 (("p" "i" "m" "a" "n") ("p" "i" "m" "a" "n"))

Possiamo scrivere la funzione inversa che prende l'output della funzione "chisanbop" restituisce il numero che rappresenta.

(define (pobnasihc lst)
  (let ( (link-sx '(("n" 10) ("a" 10) ("m" 10) ("i" 10) ("p" 50)))
         (link-dx '(("p" 5) ("i" 1) ("m" 1) ("a" 1) ("n" 1)))
         (sx (lst 0))
         (dx (lst 1))
       )
    (+ (apply + (map (fn(x) (lookup x link-sx)) sx))
       (apply + (map (fn(x) (lookup x link-dx)) dx)))))

Proviamo:

(pobnasihc '(("i" "m") ("p" "i")))
;-> 26
(pobnasihc '(("p" "i" "m" "a" "n") ("p" "i" "m" "a" "n")))
;-> 99
(pobnasihc '(() ()))
;-> 0

(for (i 0 99)
  (print i { } (chisanbop i) { } (pobnasihc (chisanbop i))) (read-line))
;-> 0 (() ()) 0
;-> 1 (() ("i")) 1
;-> 2 (() ("i" "m")) 2
;-> 3 (() ("i" "m" "a")) 3
;-> 4 (() ("i" "m" "a" "n")) 4
;-> 5 (() ("p")) 5
;-> ...
;-> 96 (("p" "i" "m" "a" "n") ("p" "i")) 96
;-> 97 (("p" "i" "m" "a" "n") ("p" "i" "m")) 97
;-> 98 (("p" "i" "m" "a" "n") ("p" "i" "m" "a")) 98
;-> 99 (("p" "i" "m" "a" "n") ("p" "i" "m" "a" "n")) 99


----------------------------
Primo primo che non divide N
----------------------------

Dato un numero intero positivo N, calcolare il primo primo che non divide N.
(Il più piccolo primo è coprimo con N).

Sequenza OEIS A053669:
Smallest prime not dividing n.
  2, 3, 2, 3, 2, 5, 2, 3, 2, 3, 2, 5, 2, 3, 2, 3, 2, 5, 2, 3, 2, 3, 2, 5,
  2, 3, 2, 3, 2, 7, 2, 3, 2, 3, 2, 5, 2, 3, 2, 3, 2, 5, 2, 3, 2, 3, 2, 5,
  2, 3, 2, 3, 2, 5, 2, 3, 2, 3, 2, 7, 2, 3, 2, 3, 2, 5, 2, 3, 2, 3, 2, 5,
  2, 3, 2, 3, 2, 5, 2, 3, 2, 3, 2, 5, 2, 3, 2, 3, 2, 7, 2, 3, 2, 3, 2, 5,
  2, 3, 2, 3, 2, 5, 2, 3, 2, ...

(define (primes-to num)
"Generates all prime numbers less than or equal to a given number"
  (cond ((= num 1) '())
        ((= num 2) '(2))
        (true
          (let ((lst '(2)) (arr (array (+ num 1))))
            (for (x 3 num 2)
              (when (not (arr x))
                (push x lst -1)
                (for (y (* x x) num (* 2 x) (> y num))
                  (setf (arr y) true)))) lst))))

(setq primi (primes-to 10000))

(define (A num)
  (let ( (sol nil) (solve nil) )
    (dolist (p primi solve)
      (if (!= (% (% num p) 7) 0)
        (set 'solve true 'sol p)
      )
    )
    sol))

Proviamo:

(A 0)
;-> nil
(A 1)
;-> 2
(A 6)
(map A (sequence 1 101))
;-> (2 3 2 3 2 5 2 3 2 3 2 5 2 3 2 3 2 5 2 3 2 3 2 5
;->  2 3 2 3 2 7 2 3 2 3 2 5 2 3 2 3 2 5 2 3 2 3 2 5
;->  2 3 2 3 2 5 2 3 2 3 2 7 2 3 2 3 2 5 2 3 2 3 2 5
;->  2 3 2 3 2 5 2 3 2)


------------------------------------
Ordinamento basato sul numero di bit
------------------------------------

Costruire una lista ordinata in base al numero di bit a 1 di tutti i numeri a N bit.

Funzione che conta il numero di bit a 1 del binario di un intero:

(define (pop-count1 num)
  (let (counter 0)
    (while (> num 0)
      (setq num (& num (- num 1)))
      (++ counter)
    )
    counter))

Per N bit abbiamo 2^N valori: da 0 a (2^N - 1).

(define (ordina bit)
  (let ( (coppie '()) (max-val (- (pow 2 bit) 1)) )
    (for (i 0 max-val)
      ; lista con elementi: (numero di 1, numero)
      (push (list (pop-count1 i) i) coppie))
    ; ordina ina base al numero di 1 e poi prende il numero
    (map last (sort coppie))))

Proviamo:

(ordina 4))
;-> (0 1 2 4 8 16 3 5 6 9 10 12 7 11 13 14 15)

(ordina 8)
;-> (0 1 2 4 8 16 32 64 128 3 5 6 9 10 12 17 18 20 24 33 34 36 40 48 65
;->  66 68 72 80 96 129 130 132 136 144 160 192 7 11 13 14 19 21 22 25
;->  26 28 35 37 38 41 42 44 49 50 52 56 67 69 70 73 74 76 81 82 84 88
;->  97 98 100 104 112 131 133 134 137 138 140 145 146 148 152 161 162
;->  164 168 176 193 194 196 200 208 224 15 23 27 29 30 39 43 45 46 51
;->  53 54 57 58 60 71 75 77 78 83 85 86 89 90 92 99 101 102 105 106 108
;->  113 114 116 120 135 139 141 142 147 149 150 153 154 156 163 165 166
;->  169 170 172 177 178 180 184 195 197 198 201 202 204 209 210 212 216
;->  225 226 228 232 240 31 47 55 59 61 62 79 87 91 93 94 103 107 109
;->  110 115 117 118 121 122 124 143 151 155 157 158 167 171 173 174 179
;->  181 182 185 186 188 199 203 205 206 211 213 214 217 218 220 227 229
;->  230 233 234 236 241 242 244 248 63 95 111 119 123 125 126 159 175
;->  183 187 189 190 207 215 219 221 222 231 235 237 238 243 245 246 249
;->  250 252 127 191 223 239 247 251 253 254 255)

(length (ordina 8))
;-> 256
(length (ordina 16))
;-> 

Vediamo la velocità della funzione:

(time (ordina 20))
;-> 1941.576
(time (ordina 22))
;-> 9565.949
(time (ordina 24))
;-> 42591.29

(setq seq (sequence 2 22 2))
(map list seq (map (fn(x) (time (ordina x))) seq))
;-> ((2 0) (4 0) (6 0) (8 0) (10 0.998) (12 4.986) (14 20.182)
;->  (16 91.2) (18 458.293) (20 2200.744) (22 9936.172))


-------------------------------
Somma numero e numero invertito
-------------------------------

Dato un numero intero N maggiore di 0, scrivere una funzione che crea i numeri (1..N) e (N..1) concatenando le cifre e poi li somma.

Esempi:
N = 5
numero1 (1..5) = 12345
numero2 (5..1) = 54321

N = 11
numero1 (1..11) = 1234567891011
numero2 (11..1) = 1110987654321
somma = 1234567891011 + 1110987654321 = 2345555545332

Sequenza OEIS A078262:
Sum of the forward and reverse concatenations of 1 to n.
  2, 33, 444, 5555, 66666, 777777, 8888888, 99999999, 1111111110,
  23333333231, 2345555545332, 244567776755433, 25466789897765534,
  2647689001998775635, 274869910212099785736, 28497092031222200795837,
  2950719304132232301805938, ...

Dobbiamo trattare i numeri come big-integer.

(define (string-int str)
"Convert a numeric string to big-integer"
  (let (num 0L)
    (cond ((= (str 0) "-")
            (pop str)
            (dolist (el (explode str)) (setq num (+ (* num 10) (int el))))
            (* num -1))
          (true
            (dolist (el (explode str)) (setq num (+ (* num 10) (int el))))))))

(define (somma n)
  (let ( (n1 (string-int (join (map string (sequence 1 n)))))
         (n2 (string-int (join (map string (sequence n 1))))) )
    (+ n1 n2)))

Proviamo:

(somma 5)
;-> 66666L

(somma 11)
;-> 2345555545332L

(map somma (sequence 1 18))
;-> (2L 33L 444L 5555L 66666L 777777L 8888888L 99999999L 1111111110L
;->  23333333231L 2345555545332L 244567776755433L 25466789897765534L
;->  2647689001998775635L 274869910212099785736L 28497092031222200795837L
;->  2950719304132232301805938L 305172940514233242402816039L)


-----------------------------------------------------------------
Convergenza della somma di prodotti digitali consecutivi ripetuti
-----------------------------------------------------------------

Dato un intero positivo N (es. 122344666889) applicare il seguente algoritmo:
1) Separare in sequenze di cifre consecutive:
   (1 22 3 44 666 88 9)
2) Eseguire il prodotto numerico di ogni sequenza:
   (1 2*2 3 4*4 6*6*6 8*8 9) = (1 4 3 16 216 64 9)
3) Sommare tutti i numeri ottenuti:
   1 + 4 + 3 + 16 + 216 + 64 + 9 = 313
4) Ripetere da 1) finché il risultato non converge a un singolo numero:
   122344666889
   313
   7
5) Restituire l'ultimo numero: 7

(define (split-sum num)
  (local (palo conta out)
    (setq lst (map int (explode (string num))))
    ; controllo se ultimo elemento è 'nil' (da L di big-integer)
    (if (nil? (last lst)) (pop lst -1))
    (setq out '())
    (setq palo (first lst))
    (setq prodotto 1L)
    (dolist (el lst)
      ; se l'elemento è uguale al precedente moltiplichiamo
      (if (= el palo) (setq prodotto (* prodotto palo))
          ; altrimenti aggiorniamo il risultato
          (begin (push prodotto out -1)
                  (setq palo el)
                  (setq prodotto (bigint palo))
          )
      )
    )
    ; aggiungiamo l'ultimo valore
    (push prodotto out -1)
    ; controllo se ultimo elemento è 'nil' (da L di big-integer)
    (if (nil? (last out)) (pop out -1))
    ;(println out)
    (apply + out)))

Proviamo:

(split-sum 122344666889)
;-> 313L
(split-sum 313)
;-> 7L
(split-sum 7)
;-> 7L

(split-sum 12345678911234567899)
;-> 162
(split-sum 99999999999999999999)
;-> 12157665459056928801L
(split-sum 199999999999999999999)
;-> 12157665459056928802L

(define (converge num)
  (while (> (length num) 1)
    (setq num (string (split-sum num)))
    (if (= "L" (last num)) (pop num -1))
    (println num)
  )
  (int num 0 10) '>)

Proviamo:

(converge 122344666889)
;-> 313
;-> 7
(converge 99999999999999999999)
;-> 12157665459056928801
;-> 162
;-> 9
(converge 199999999999999999999)
12157665459056928802
;-> 163
;-> 10
;-> 1
(converge 12345678901234567890)
;-> 90
;-> 9
(converge 11111111)
;-> 1
(converge "111222333444555666777888999000")
;-> 2025
;-> 9


-------------------------------
Coppie di dadi con somma uguale
-------------------------------

Lanciando quattro dadi (con numeri da 1 a 6), calcolare la probabilità di poter suddividere i dadi in due coppie in cui ciascuna coppia abbia la stessa somma?


Metodo con simulazione
-----------------------

(define (equal-sum? lst)
  (or (= (+ (lst 0) (lst 1)) (+ (lst 2) (lst 3)))
      (= (+ (lst 0) (lst 2)) (+ (lst 1) (lst 3)))
      (= (+ (lst 0) (lst 3)) (+ (lst 1) (lst 2)))))

(equal-sum? '(1 2 3 4))
;-> true
(equal-sum? '(5 1 3 4))
;-> nil

(define (dice-lst n) (map (curry + 1) (rand 6 n)))

(dice-lst 4)
;-> (3 3 6 5)

(define (simula limite)
  (let (conta 0)
    (for (i 1 limite)
      (if (equal-sum? (dice-lst 4)) (++ conta)))
    (div conta limite)))

Proviamo:

(simula 1e4)
;-> 0.2603
(simula 1e5)
;-> 0.26107
(simula 1e6)
;-> 0.259072
(simula 1e7)
;-> 0.2592237

Metodo con calcolo esaustivo
----------------------------
Lanciando 4 dadi possiamo avere 6x6x6x6 = 1296 risultati diversi.
Calcoliamo tutti risultati e vediamo quanti soddisfano la condizione.
Quindi la probabilità vale:

          numero casi positivi
  prob = ----------------------
                 1296 (tutti i casi)

Funzione che calcola tutti i risultati possibili del lancio di 4 dadi:

(define (calcola)
  (let (out '())
    (for (i 1 6)
      (for (j 1 6)
        (for (k 1 6)
          (for (l 1 6)
            (push (list i j k l) out))))) out))

(length (calcola))
;-> 1296

Funzione che calcola la probabilità esatta:

(define (prob)
  (let ( (conta 0) (lanci (calcola)) )
    (dolist (el lanci)
      (if (equal-sum? el) (++ conta)))
      (println conta)
    (div conta (length lanci))))

(prob)
;-> 336
;-> 0.2592592592592592

Si puo dimostrare che i casi favorevoli sono così suddivisi:

1) {a, a, a, a}: 4 dadi uguali = 6 casi
2) {a, a, a, b}: 3 dadi uguali e 1 diverso = nessun caso
3) {a, a, b, c}: 2 dadi uguali e 2 diversi = 72 casi
4) {a, a, b, b}: 2 dadi uguali e 2 dadi uguali = 90 casi
5) {a, b, c, d}: 4 dadi diversi = 168 casi

  6 + 72 + 90 + 38 = 336

  prob  = 336/1296 = 7/27 = 0.2592592592592592


-------------------
Mancini e destrorsi
-------------------

Ci sono 100 persone in una stanza.
Il 99% delle persone è mancino.
Quante persone mancine devono uscire dalla stanza affinché ci sia il 98% di mancini?

Bisogna risolvere la seguente espressione:

  (99 - x) : (100 - x) = 98 : 100

Passaggi:
  (99 - x)/(100 - x) = 98/100
  100*(99 - x)/(100 - x) = 98
  100*(99 - x) = 98*(100 - x)
  9900 - 100x = 9800 - 98x
  9900 - 9800 = 100x - 98x
  100 = 2x --> x = 100/2 = 50

Risolviamo il problema con una funzione:

(define (solve)
  (setq found nil)
  (setq lh 99)
  (setq rh 1)
  (for (i 1 99 1 found)
    (-- lh)
    (when (= 0.98 (div lh (+ lh rh)))
      (println (- 99 lh))
      (setq found true))
  )
  (println (string lh) "/" (string (+ lh rh)) " = " (div lh (+ lh rh))) '>)

(solve)
;-> 50
;-> 49/50 = 0.98


-------------------------
Merge di k liste ordinate
-------------------------

Data una lista di liste ordinate in modo crescente, scrivere una funzione che unisce (merge) le liste in un unica lista ordinata.

Per fare il merge di k liste ordinate in modo efficiente, l'approccio classico è usare una min-heap (coda di priorità) per tenere traccia del minimo corrente tra le teste di ciascuna lista. 
Questo riduce la complessità a O(N*log(k)) dove N è il numero totale di elementi.

In newLISP, possiamo simulare una coda di priorità con una lista (più lenta di un heap vero, ma funziona bene per k piccoli).

(define (merge-k-liste liste)
  (let ((risultato '())
        (heap '()))
    ;; inizializza il "heap" con il primo elemento di ciascuna lista
    (for (i 0 (- (length liste) 1))
      (if (liste i)
          (push (list (liste i 0) i 0) heap -1))) ; (valore, indice-lista, indice-interno)
    ;; ciclo principale
    (while heap
      ;; trova il minimo
      (set 'heap (sort heap (fn (a b) (< (a 0) (b 0)))))
      (let ((minimo (pop heap)))
        (push (minimo 0) risultato -1)
        ;; se la lista da cui proveniva ha altri elementi, aggiungi il successivo
        (let ((i (minimo 1))
              (j (minimo 2)))
          (if (< (+ j 1) (length (liste i)))
              (push (list ((liste i) (+ j 1)) i (+ j 1)) heap -1)))))
    risultato))

Proviamo:

(merge-k-liste '((1 4 7) (2 5 8) (3 6 9)))
;-> (1 2 3 4 5 6 7 8 9)
(merge-k-liste '((1 4 5) (1 3 4) (2 6)))
;-> (1 1 2 3 4 4 5 6)
(merge-k-liste '((1 4) (1 3 4) (2 6)))
;-> (1 1 2 3 4 4 6)
(merge-k-liste '((1 4) () (2 6)))
;-> (1 2 4 6)

Vediamo un metodo con le primitive di newLISP (molto più corto):

(define (merge-k lst) (sort (flat lst)))

(merge-k '((1 4 7) (2 5 8) (3 6 9)))
;-> (1 2 3 4 5 6 7 8 9)
(merge-k '((1 4 5) (1 3 4) (2 6)))
;-> (1 1 2 3 4 4 5 6)
(merge-k '((1 4) (1 3 4) (2 6)))
;-> (1 1 2 3 4 4 6)
(merge-k '((1 4) () (2 6)))
;-> (1 2 4 6)


Test di correttezza:

(silent 
  (setq a (sort (rand 100 10000)))
  (setq b (sort (rand 50 10000)))
  (setq c (sort (rand 150 10000)))
  (setq all (list a b c)))

(= (merge-k all) (merge-k-liste all))
;-> true

Test di velocità:

(time (merge-k-liste all))
;-> 5750.289
(time (merge-k all))
;-> 4.998

In questo caso le funzioni primitive sono estremamente più veloci dell'algoritmo più efficiente.


------------------------------------------
Compressione/Espansione di liste di interi
------------------------------------------

Data una lista di interi non negativi, generare una lista in cui lista(k) è il numero di volte in cui il numero k appare nella lista data.

Esempio:
lista = (1 1 3 5 5 0 4 0)
output = (2 2 0 1 1 2)  -->  2 zero, 2 uno, 0 due, 1 tre, 1 quattro, 2 cinque

(define (comprime lst)
  (letn ( (max-val (apply max lst))
          (seq (sequence 0 max-val)) )
    (count seq lst)))

(comprime '(1 1 3 5 5 0 4 0))
;-> (2 2 0 1 1 2)

Adesso scriviamo la funzione inversa.

(define (espande lst)
  (let (out '())
    (dolist (el lst)
      (extend out (dup $idx el)))))

(espande '(2 2 0 1 1 2))
;-> (0 0 1 1 3 4 5 5)

Test di correttezza:

(silent (setq t (rand 100 100000)))
(setq c (comprime t))
(= (espande c) (sort t))
;-> true


-------------------------------
Gioco degli zeri in una stringa
-------------------------------

Due persone si sfidano al seguente gioco:
- Si parte da una stringa S formata solo da cifre (0..9).
- Il turno del primo giocatore consiste nel togliere una sottostringa da S che contiene un numero "dispari" di zeri (0).
- Il turno del secondo giocatore consiste nel togliere una sottostringa da S che contiene un numero "pari" di zeri (0).
- Il primo giocatore che non può effettuare mosse ha perso.

Ulteriori regole:
- La sottostringa S non può essere vuota (no "").
- Il numero 0 è pari.

Data una stringa iniziale è possibile determinare il vincitore supponendo che entrambi giochino in modo ottimale?

Chiamiamo P1 e P2 il primo e il secondo giocatore.
Sia k il numero di zeri in S, abbiamo i seguenti casi:
1) Se k = 0, allora vince P2 poiché P1 non ha zeri da scegliere.
2. Se k % 2 = 1 (k è dispari), P1 vince poiché può scegliere l'intera stringa.
3. Se k % 2 = 0 (k è pari), P1 vince poiché può scegliere (k - 1) zeri, allora P2 sceglierà una sottostringa contenente 0 zeri, con il risultato che P1 sceglierà l'intera stringa rimanente, oppure P2 non potrà scegliere affatto (l'ultimo zero).

(define (winner str)
  (let (zeri (length (find-all "0" str)))
    (if (= zeri 0) 'P2
        (even? zeri) 'P1
        (odd? zeri) 'P1)))

(winner "12345")
;-> P2
(winner "12030405")
;-> P1
(winner "102030405")
;-> P1

In modo breve:

(define (winner str) (if (find "0" str) 'P1 'P2))

(winner "12345")
;-> P2
(winner "12030405")
;-> P1
(winner "102030405")
;-> P1


--------------
Random walk 1D
--------------

Un punto parte da una posizione w (della linea degli interi (-inf ... -3 -2 -1 0 1 2 3... +inf)) e si muove in modo casuale e con probabilità uguali a sinistra (-1) o a destra (+1) della linea.
Calcolare il numero di passi atteso affinchè il punto visiti due punti x e y posti lungo la linea.
I punti w, x e y sono tutti diversi.

Esempi:

1) w interno all'intervallo (x..y)
w = 0, x = -3, y = 4

        x       w           y
... -4 -3 -2 -1 0 +1 +2 +3 +4 ...

2) w esterno all'intervallo (x..y)
w = -2, x = 1, y = 3

           w       x     y
... -4 -3 -2 -1 0 +1 +2 +3 +4 ...

Dati gli interi 'w', 'x' e 'y' (tutti distinti), qual è il numero previsto di passi prima che una passeggiata casuale simmetrica (passo +/-1 con uguale probabilità) che inizia da 'w' visiti sia 'x' che 'y'?

Poniamo
  a = min(x, y) ('a' si trova a sinistra di 'b')
  b = max(x, y) ('b' di trova a destra di 'a')

Il punto parte da 'w' e deve visitare sia 'a' che 'b'.

Caso 1: w interno all'intervallo (a..b)  -->  a < w < b

Il numero di passi previsti per raggiungere entrambi i punti finali di un intervallo (partendo dal suo interno) vale:

  E(w,a,b) = (b - a)^2 - (w - a)*(b - w)

w=2, a = 0, b = 4, :

E = (4 - 0)^2 - (2 - 0)(4 - 2) = 16 - 2×2 = 12

Caso 2: w esterno all'intervallo (a..b)  -->  (w < a) oppure (w > b)

Ipotesi
1. Raggiungiamo prima l'estremità più vicina (diciamo 'a').
2. Quindi, da 'a', andiamo al punto 'b'.
Questo richiede **linearità delle aspettative** e conoscenza dei risultati attesi.

Passi attesi per andare da 'a' o 'b' a 'w':
  E(w -> a o b) = (a - w)(b - w)

Dopo aver raggiunto 'a', i passi attesi per andare da 'a' a 'b':

  E(a -> b) = (b - a)^2

Il totale di passi attesi vale:

  E(w, a, b) = (a - w)(b - w) + (b - a)^2, per w < a

  E(w, a, b) = (w - a)(w - b) + (b - a)^2, per w > b (simmetria)

(define (E w x y)
  (let ((a (min x y))
        (b (max x y)))
    (if (and (> w a) (< w b))
        (- (* (- b a) (- b a)) (* (- w a) (- b w)))
        (+ (* (abs (- w a)) (abs (- w b))) (* (- b a) (- b a))))))

Proviamo:

caso: (a < w < b)
(E 2 0 4)
;-> 12

caso: (w < a)
(E -3 0 5)
;-> 49

caso: (w > b)
(E 8 0 5)
;-> 49

Funzione che simula una passeggiata:

(define (passi w x y)
  (let ( (pos w) (steps 0)
         (a-visit nil) (b-visit nil)
         (a (min x y)) (b (max x y)) )
    (until (and a-visit b-visit)
      (setq pos (if (zero? (rand 2)) (++ pos) (-- pos)))
      (++ steps)
      (if (= pos a) (setq a-visit true))
      (if (= pos b) (setq b-visit true))
    )
    steps))

(passi 2 0 4)
;-> 20
(passi 2 0 4)
;-> 22

Adesso possiamo stimare il valore atteso calcolando la media di più prove:

(define (media-passi w x y limite)
  (let (totale 0)
    (for (i 1 limite)
      (++ totale (passi w x y)))
    (div totale limite)))

Proviamo:

(media-passi 2 0 4 100)
;-> 10117.66

Forse ci vuole un pò di tempo...
(media-passi 2 0 4 1000) ; 
;-> 52663.136

Il risultato è molto diverso da quello calcolato matematicamente (12).

Simulazione vs Matematica
-------------------------
Il calcolo matematico dell'aspettativa fornisce la media reale.
Comunque la media simulata può essere fortemente distorta se:
1) Non si simulano abbastanza prove.
2) Non si pone un limite ai valori anomali estremi (outlier).
In questo caso si parla di distribuzioni "heavy-tailed".

Vediamo i valori dei passi di tutte le simulazioni:

(define (crea-passi w x y limite)
  (let (out '())
    (for (i 1 limite)
      (push (passi w x y) out))
    out))

(crea-passi 2 0 4 100)
;-> (36 20 1190 38 270 20 16 8 12 46 176 8 42 22 80 22 20 10522
;->  22 30 12 376 486 128 6 444 18 14 60 22 6 8 28 10 12 6 8 20
;->  30 346 18 34 52 34 60 124 24 10 102 26 1428 12 92 64 28 84
;->  230 64 80 58 52 12 114 22 96 10 24 14 46 34 46 30 3736 30 
;->  872 34 4712 10 32 14 400 74 20 12 20 58 358 14 20 40 34 68
;->  384 66 1160 298208 14 72 40 38)

I numeri grandi 1190, 10522, 4712, 298208 sono dovuti al fatto che il punto si è allontanato molto dai punti x e y, quindi ha bisogno di molti passi per ritornare.
Questo comporta che il calcolo della media simulata non produce risultati corretti.

Per tenere conto degli outlier dovremmo eliminarli dalla lista dei passi, ma questo è molto difficile perchè il limite di taglio superiore dipende dai valori w, x e y.

Un altro metodo consiste nel calcolare altri parametri statistici oltre alla media aritmetica.

Nelle distribuzioni heavi-tailed come questa, che misura il tempo di cammino casuale 1D per raggiungere due punti, la moda (ovvero il numero di passi più frequente osservato) spesso fornisce un valore più tipico o rappresentativo della media (che può essere distorta da rare passeggiate molto lunghe).
La Media è sensibile ai valori anomali, mentre la Moda, che rappresenta il numero di passi più probabile (tipico) è più stabile.

Utilizzando la Moda ci si chiede:
"Quanti passi saranno necessari nella maggior parte dei casi?"

Funzione che calcola la Media di una lista di numeri:

(define (media lst)
  (div (apply add lst) (length lst)))

Funzione che calcola la Mediana di una lista di numeri:

(define (mediana lst)
  (let (len (length lst))
    (sort lst)
    (if (odd? len)
        (lst (/ len 2))
        (div (add (lst (- (/ len 2) 1)) (lst (/ len 2))) 2))))

Funzione che calcola la Moda di una lista di numeri:

(define (moda lst)
  (letn ((uniq (unique lst))
        (conta (count uniq lst)))
    (uniq (find (apply max conta) conta))))

Funzione simula un dato numero di passeggiate e poi calcola la Media, la Mediana e la Moda:

(define (lista-passi w x y limite)
  (let (out '())
    (for (i 1 limite)
      (push (passi w x y) out))
    (println "media: " (media out))
    (println "mediana: " (mediana out))
    (println "moda: " (moda out))
    (println "E: " (E w x y)) '>))

Proviamo:

(lista-passi 2 0 4 10)
;-> media: 500.6
;-> mediana: 45
;-> moda: 6
;-> E: 12
(lista-passi 2 0 4 100)
;-> media: 1062.48
;-> mediana: 32
;-> moda: 12
;-> E: 12

(lista-passi -3 0 5 10)
;-> media: 3170.2
;-> mediana: 115
;-> moda: 46
;-> E: 49

(lista-passi -3 0 5 100)
;-> media: 28843.78
;-> mediana: 122
;-> moda: 50
;-> E: 49

Sembra che la Moda produca risposte in linea con i risultati matematici.

Comunque gli outlier continuano a generare un problema: rallentano enormemente il tempo di esecuzione della simulazione.

(time (lista-passi -3 0 5 1000))
;-> media: 65637.664
;-> mediana: 122
;-> moda: 20
;-> E: 49
;-> 12719.725

Il problema aumenta con l'aumento del numero di prove e con l'aumentare della distanza tra w, x e y.

(time (lista-passi 5 -10 10 100))
;-> media: 11370010.34
;-> mediana: 722
;-> moda: 399
;-> E: 325
;-> 219817.51

Proviamo ad utilizzare il seguente criterio per gestire gli outlier:
- eliminare i valori che sono maggiori di K volte il valore atteso E(w x y).

Riscriviamo la funzione "passi" (poniamo K=5):

(define (passiK w x y)
  (let ( (pos w) (steps 0)
         (a-visit nil) (b-visit nil)
         (a (min x y)) (b (max x y))
         (soglia (* 5 (E w x y))) )
    (until (or (and a-visit b-visit) (> steps soglia))
      (setq pos (if (zero? (rand 2)) (++ pos) (-- pos)))
      (++ steps)
      (if (= pos a) (setq a-visit true))
      (if (= pos b) (setq b-visit true))
    )
    ; inserisce nil al posto del valore di un outlier
    (if (> steps soglia) nil steps)))

Riscriviamo la funzione "lista-passi":

(define (lista-passiK w x y limite)
  (let (out '())
    (for (i 1 limite)
      (push (passiK w x y) out))
    ; elimina i nil (outlier)
    (setq out (clean nil? out))
    (println "media: " (media out))
    (println "mediana: " (mediana out))
    (println "moda: " (moda out))
    (println "E: " (E w x y)) '>))

Proviamo:

(lista-passiK 2 0 4 1e4)
;-> media: 23.27149627623561
;-> mediana: 20
;-> moda: 10
;-> E: 12

(lista-passiK -3 0 5 1e4)
;-> media: 80.06938507070043
;-> mediana: 60
;-> moda: 30
;-> E: 49

(time (lista-passiK 0 -10 10 1e5))
;-> media: 569.7778715503418
;-> mediana: 472
;-> moda: 262
;-> E: 300
;-> 22078.061

Per capire meglio come varia la distribuzione si potrebbe costruire un istogramma dei valori delle passeggiate.

(define (histogram lst hmax)
"Print the histogram of a list of integer numbers"
  (local (valori len-max-val unici ind-val val-ind f-lst hm scala linee fmt)
    ; lista ordinata dei valori unici
    (setq valori (sort (unique lst)))
    ; lunghezza del numero massimo (fmt)
    (setq len-max-val (length (apply max valori)))
    ; calcola quanti numeri diversi ci sono nella lista
    (setq unici (length valori))
    ; lista: (indice valore)
    (setq ind-val (map list (sequence 0 (- unici 1)) valori))
    ; lista: (valore indice)
    (setq val-ind (map list valori (sequence 0 (- unici 1))))
    ; crea la lista delle frequenze
    (setq f-lst (array unici '(0)))
    ; calcolo dei valori delle frequenze
    (dolist (el lst) (++ (f-lst (lookup el val-ind))))
    ; valore massimo delle frequenze
    (setq hm (apply max f-lst))
    ; fattore di scala
    (setq scala (div hm hmax))
    ; calcolo delle lunghezze delle colonne dell'istogramma
    (setq linee (map (fn (x) (round (div x scala))) f-lst))
    ; stampa dell'istogramma (orizzontale)
    (dolist (el linee)
      (setq fmt (string "%" (+ len-max-val 1) "d %s %4d"))
      (println (format fmt (lookup $idx ind-val) (dup "*" el) (f-lst $idx)))
    )'>))

Proviamo con gli outlier (quindi molto lenta...forse):

(define (lista-passi w x y limite)
  (let (out '())
    (for (i 1 limite)
      (push (passi w x y) out))
    (println "media: " (media out))
    (println "mediana: " (mediana out))
    (println "moda: " (moda out))
    (println "E: " (E w x y))
    out))

(setq p (lista-passi 2 0 4 100))
;-> media: 317.64
;-> mediana: 47
;-> moda: 16
;-> E: 12
;-> (144 380 196 58 78 48 708 46 106 34 54 736 22 128 68 6 62 10 460 20 32
;->  1374 12 284 18 8 16 12 80 10 22 468 314 54 122 16 32 14 36 36 28 20 6
;->  74 246 24 8 616 2322 14 30 3386 56 946 4098 22 294 24 230 10 102 2370
;->  10 52 696 462 5274 6 36 200 10 22 20 38 14 16 88 156 640 72 16 16 94 
;->  128 68 8 524 14 1484 12 18 20 16 102 8 14 12 50 86 12)

(histogram p 60)
;->     6 ******************************    3
;->     8 ****************************************    4
;->    10 **************************************************    5
;->    12 **************************************************    5
;->    14 **************************************************    5
;->    16 ************************************************************    6
;->    18 ********************    2
;->    20 ****************************************    4
;->    22 ****************************************    4
;->    24 ********************    2
;->    28 **********    1
;->    30 **********    1
;->    32 ********************    2
;->    34 **********    1
;->    36 ******************************    3
;->    38 **********    1
;->    46 **********    1
;->    48 **********    1
;->    50 **********    1
;->    52 **********    1
;->    54 ********************    2
;->   ... 
;->   736 **********    1
;->   946 **********    1
;->  1374 **********    1
;->  1484 **********    1
;->  2322 **********    1
;->  2370 **********    1
;->  3386 **********    1
;->  4098 **********    1
;->  5274 **********    1

(setq p (lista-passi -3 0 5 1000))
;-> media: 10489.458
;-> mediana: 134
;-> moda: 32
;-> E: 49
;-> (3432 164 208 10856 42 36 ...)

(histogram p 60)
;->        8 *************    5
;->       10 *****************************   11
;->       12 *********************    8
;->       14 *****************************   11
;->       16 ***********************************************   18
;->       18 *******************************   12
;->       20 ***************************************   15
;->       22 *********************    8
;->       24 *****************************   11
;->       26 ****************************************************   20
;->       28 *************************************   14
;->       30 **************************************************   19
;->       32 ************************************************************   23
;->       34 ******************************************   16
;->       36 **********************************   13
;->       38 *****************************   11
;->       40 *********************    8
;->       42 *****************************   11
;->       44 *********************************************************   22
;->       46 *****************************   11
;->       48 **************************   10
;->       50 ******************    7
;->       52 **************************   10
;->       54 **************************   10
;->       56 ***********************    9
;->       58 **********    4
;->       60 ***************************************   15
;->      ...
;->   670076 ***    1
;->  1016888 ***    1
;->  1256318 ***    1
;->  1298484 ***    1
;->  2160690 ***    1

Proviamo senza gli outlier:

(define (lista-passiK w x y limite)
  (let (out '())
    (for (i 1 limite)
      (push (passiK w x y) out))
    ; elimina i nil (outlier)
    (setq out (clean nil? out))
    (println "media: " (media out))
    (println "mediana: " (mediana out))
    (println "moda: " (moda out))
    (println "E: " (E w x y))
    out))

(silent (setq p (lista-passiK 2 0 4 1e4)))
;-> media: 23.38477157360406
;-> mediana: 20
;-> moda: 10
;-> E: 12
(histogram p 60)
;->   6 *************************************  296
;->   8 **********************************************************  465
;->  10 ************************************************************  485
;->  12 *********************************************************  461
;->  14 **********************************************************  466
;->  16 **********************************************  368
;->  18 *****************************************  330
;->  20 ***************************************  316
;->  22 *************************************  299
;->  24 *********************************  264
;->  26 **************************  214
;->  28 **************************  208
;->  30 ***********************  185
;->  32 ******************  147
;->  34 *******************  156
;->  36 ******************  148
;->  38 ***************  124
;->  40 *************  104
;->  42 **************  114
;->  44 ***************  121
;->  46 ***********   91
;->  48 ************   99
;->  50 **********   77
;->  52 ***********   88
;->  54 ***********   87
;->  56 ********   66
;->  58 ********   68
;->  60 ********   63


-----------------------------------------------------
Elementi che formano gruppi con determinate proprietà
-----------------------------------------------------

Abbiamo una lista con N proprietà (es. (A B C D E)).

Abbiamo K elementi ognuno con una lista delle proprietà possedute:
  elemento(1) = (A D)
  elemento(2) = (C)
  ...
  elemento(k) = (A B C)

Determinare tutti i gruppi con il minor numero di elementi che insieme hanno tutte le N proprietà della lista iniziale.

Esempio
    proprieta = (D E C)
  elemento(1) = (A C)
  elemento(2) = (C)
  elemento(3) = (D E)
  elemento(4) = (B C)

Gruppi con il minor numero di elementi (2) che soddisfano tutte le proprieta:

  gruppo 1 --> ((A C) (D E)) 2 elementi (1 e 4)
  gruppo 2 --> ((C) (D E))   2 elementi (2 e 3)
  gruppo 3 --> ((D E) (B C)) 2 elementi (3 e 4)

Funzione che calcola tutti i sottoinsiemi di una lista:
(Numero elementi del 'powerset' di una lista con N elementi = 2^N)

(define (powerset lst)
  (if (empty? lst)
      (list '())
      (let ((element (first lst))
            (p (powerset (rest lst))))
        (append (map (fn (subset) (cons element subset)) p) p))))

(powerset '(1 2 3))
;-> ((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ())

Fuznione che calcola tutti i gruppi con il minor numero di elementi che insieme hanno tutte le proprietà richieste:

(define (gruppi-minimi pro ele)
  (local (gruppi gruppi-ok numero-elementi minimo indici sol)
    ; calcolo di tutti i sottoinsiemi possibili
    (setq gruppi (powerset ele))
    ; calcolo di tutti i gruppi di elementi che soddisfano la proprieta
    (setq gruppi-ok '())
    (dolist (g gruppi)
      ; verifica che l'unione del gruppo copra tutte le proprietà richieste
      (if (= (difference pro (flat g)) '())
          (push g gruppi-ok -1)
      )
    )
    ;(println gruppi-ok)
    (cond ((= gruppi-ok '()) '()) ; nessuna soluzione
          ; calcolo dei gruppi con il minor numero di elementi
          (true
            ; lista dei numeri di elementi di gruppi-ok
            (setq numero-elementi (map length gruppi-ok))
            ; valore minimo di elementi
            (setq minimo (apply min numero-elementi))
            ; calcolo degli indici degli elementi con valore minimo
            (setq indici (ref-all minimo numero-elementi))
            ; elementi del gruppo-ok che hanno il minor numero di elementi
            (setq sol (select gruppi-ok (flat indici)))
            sol))))

Proviamo:

(setq elementi '((A C) (C) (D E) (B C)))
(setq proprieta '(D E C))
(gruppi-minimi proprieta elementi)
;-> (((A C) (D E)) ((C) (D E)) ((D E) (B C)))

(setq proprieta '(A B C D E))
(gruppi-minimi proprieta elementi)
;-> (((A C) (D E) (B C)))

(setq proprieta '(A B C))
(gruppi-minimi proprieta elementi)
;-> (((A C) (B C)))

(setq proprieta '(F))
(gruppi-minimi proprieta elementi)
;-> ()

Con questo stesso metodo possiamo selezionare i gruppi che hanno la somma minore delle proprieta richieste.

(define (gruppi-peso-minimo pro ele)
  (local (gruppi gruppi-ok pesi minimo indici sol)
    (setq gruppi (powerset ele))
    (setq gruppi-ok '())
    ; trova tutti i gruppi validi
    (dolist (g gruppi)
      (if (= (difference pro (flat g)) '())
          (push g gruppi-ok -1)))
    (cond
      ((= gruppi-ok '()) '())
      (true
        ; somma del numero di proprietà per ogni gruppo
        (setq pesi (map (fn (g) (length (flat g))) gruppi-ok))
        (setq minimo (apply min pesi))
        (setq indici (ref-all minimo pesi))
        (select gruppi-ok (flat indici))))))

Proviamo:

(setq elementi '((A C) (C) (D E) (B C)))
(setq proprieta '(D E C))
(gruppi-peso-minimo proprieta elementi)
;-> (((C) (D E)))

(setq proprieta '(A B C D E))
(gruppi-peso-minimo proprieta elementi)
;-> (((A C) (D E) (B C)))

(setq proprieta '(A B C))
(gruppi-peso-minimo proprieta elementi)
;-> (((A C) (B C)))

(setq proprieta '(F))
(gruppi-peso-minimo proprieta elementi)
;-> ()

(setq elementi '((A C) (B C) (A) (B)))
(setq proprieta '(A B))
(gruppi-minimi proprieta elementi)
;-> (((A C) (B C)) ((A C) (B)) ((B C) (A)) ((A) (B)))
(gruppi-peso-minimo proprieta elementi)
;-> (((A) (B)))

Nota: funziona direttamente con liste di simboli, interi o stringhe.

============================================================================

