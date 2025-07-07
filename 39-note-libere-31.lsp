================

 NOTE LIBERE 31

================

    "L'ora del coglione passa per tutti"
    "No escaping from dumbass moment"

----------------------------------------
Inversioni crescenti dei numeri naturali
----------------------------------------

Sequenza OEIS A061579:
Reverse one number (0), then two numbers (2,1), then three (5,4,3), then four (9,8,7,6), etc.
  0, 2, 1, 5, 4, 3, 9, 8, 7, 6, 14, 13, 12, 11, 10, 20, 19, 18, 17, 16, 15,
  27, 26, 25, 24, 23, 22, 21, 35, 34, 33, 32, 31, 30, 29, 28, 44, 43, 42,
  41, 40, 39, 38, 37, 36, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 65, 64,
  63, 62, 61, 60, 59, 58, 57, 56, 55, 77, 76, 75, 74, 73, 72, 71, 70, 69,
  68, 67, 66, ...

a(n) = floor(sqrt(2n+1)-1/2) * floor(sqrt(2n+1)+3/2) - n

(define (seq0 n)
  (sub (mul (floor (sub (sqrt (add (mul 2 n) 1)) 0.5))
            (floor (add (sqrt (add (mul 2 n) 1)) 1.5)))
         n))

(map seq0 (sequence 0 60))
;-> (0 2 1 5 4 3 9 8 7 6 14 13 12 11 10 20 19 18 17 16 15
;->  27 26 25 24 23 22 21 35 34 33 32 31 30 29 28 44 43 42
;->  41 40 39 38 37 36 54 53 52 51 50 49 48 47 46 45 65 64
;->  63 62 61 60)

Sequenza OEIS A038722:
Take the sequence of natural numbers (A000027) and reverse successive subsequences of lengths 1,2,3,4,...
  1, 3, 2, 6, 5, 4, 10, 9, 8, 7, 15, 14, 13, 12, 11, 21, 20, 19, 18, 17,
  16, 28, 27, 26, 25, 24, 23, 22, 36, 35, 34, 33, 32, 31, 30, 29, 45, 44,
  43, 42, 41, 40, 39, 38, 37, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 66,
  65, 64, 63, 62, 61, 60, 59, 58, 57, 56, 78, 77, 76, ...

Formula:
  a(n) = floor(sqrt(2n-1) - 1/2)*floor(sqrt(2n-1) + 3/2) - n + 2
       = A061579(n-1) + 1

(define (seq1 n) (+ (seq0 (- n 1)) 1))

(map seq1 (sequence 1 60))
;-> (1 3 2 6 5 4 10 9 8 7 15 14 13 12 11 21 20 19 18 17
;->  16 28 27 26 25 24 23 22 36 35 34 33 32 31 30 29 45 44
;->  43 42 41 40 39 38 37 55 54 53 52 51 50 49 48 47 46 66
;->  65 64 63 62)


------------------------------------
Generatore di schemi per Cercaparole
------------------------------------

Il "Cercaparole" è un gioco che consiste nel cercare alcune parole inglobate in una matrice di caratteri.
Vedi "Cercaparole" su "Note libere 3".

Scriviamo una funzione che prende una lista di parole, le dimensioni larghezza e altezza e genera una matrice di caratteri con le parole date.

Nota: nella lista delle parole nessuna parola deve essere sottostringa di un'altra parola.

(define (genera-cercaparole parole larghezza altezza)
  (local (matrice direzioni max-len)
    (setq max-len (apply max (map length parole)))
    (if (< larghezza max-len)
      (println "Attenzione: larghezza(" larghezza ") > lunghezza massima parole(" max-len ")."))
    (if (< altezza max-len)
      (println "Attenzione: altezza(" altezza ") > lunghezza massima parole(" max-len ")."))
    ; ordina le parole 
    ; (diminuisce la probabilità di mancati inserimenti di parole)
    (sort parole (fn(x y) (>= (length x) (length y))))
    ; inizializza la matrice vuota
    (setq matrice (array-list (array altezza larghezza '(nil))))
    ; definisce le 8 direzioni possibili: orizzontale, verticale, diagonale
    (setq direzioni '((1 0) (-1 0) (0 1) (0 -1) (1 1) (-1 -1) (-1 1) (1 -1)))
    ; verifica se una parola può essere inserita
    ; partendo da (x, y) in direzione (dx, dy)
    (define (puo-inserire? parola x y dx dy)
      (let ((len (length parola)) (ok true))
        (for (i 0 (- len 1))
          (let ((cx (+ x (* i dx)))
                (cy (+ y (* i dy))))
            (if (or (< cx 0) (>= cx larghezza) (< cy 0) (>= cy altezza))
                (setq ok nil))
            (let ((cella (and (< cy altezza) (< cx larghezza) (matrice cy cx))))
              (if (and cella (!= cella (parola i)))
                  (setq ok nil)))))
        ok))
    ; inserisce una parola nella matrice, con massimo 10000 tentativi
    (define (inserisci parola)
      (let ((inserita nil) (tentativi 0) (max-tentativi 10000))
        (while (and (not inserita) (< tentativi max-tentativi))
          (++ tentativi)
          (letn ((dir (direzioni (rand (length direzioni))))
                (dx (dir 0)) (dy (dir 1))
                (x (rand larghezza)) (y (rand altezza)))
            (if (puo-inserire? parola x y dx dy)
                (begin
                  (for (i 0 (- (length parola) 1))
                    (let ((cx (+ x (* i dx)))
                          (cy (+ y (* i dy))))
                      (setf (matrice cy cx) (parola i))))
                  (setq inserita true)))))
        (if (not inserita)
            (println "Errore: non riesco a inserire la parola: " parola))))
    ; inserisce tutte le parole nella matrice
    (dolist (p parole) (inserisci p))
    ; rimpiazza le celle non usate con spazi
    (for (y 0 (- altezza 1))
      (for (x 0 (- larghezza 1))
        (if (= (matrice y x) nil)
            (setf (matrice y x) " "))))
    ; stampa la matrice soluzione (parole + spazi)
    (println "Matrice soluzione:")
    (dolist (riga matrice)
      (println (join (map string riga) " ")))
    ; riempie gli spazi vuoti con lettere A-Z casuali
    (for (y 0 (- altezza 1))
      (for (x 0 (- larghezza 1))
        (if (= (matrice y x) " ")
            (setf (matrice y x) (char (+ 65 (rand 26)))))))
    ; stampa la matrice problema (da cercare)
    (println "Matrice problema:")
    (dolist (riga matrice)
      (println (join (map string riga) " "))) matrice))

(genera-cercaparole '("CANE" "GATTO" "UVA" "LIMONE" "MELA") 8 8)
;-> E
;->   N           C
;->     O         A
;->     G M       N
;->     A E I   U E
;->     T L   L V
;->     T A     A
;->     O
;-> Matrice problema:
;-> E I I O S J P A
;-> U N S Q Z X U C
;-> I N O T L Q F A
;-> C K G M L K S N
;-> F P A E I Q U E
;-> U H T L G L V J
;-> K U T A L Q A Y
;-> K Z O C W B W Q

(setq parole '("ASSEMBLY" "FORTRAN" "LISP" "ALGOL" "BASIC" "COBOL"
               "ADA" "DELPHI" "SMALLTALK" "JAVA" "PHP" "LOGO" "PASCAL"
               "PERL" "PYTHON"))

(setq mtx (genera-cercaparole parole 12 12))
;-> Matrice soluzione:
;->   D F O R T R A N
;-> A E       A
;->   L       S       A O
;->   P G P A S C A L D G
;-> N H   O   E       A O
;-> O I     L M P       L
;-> H P       B E P S I L C
;-> T   H     L R         I
;-> Y     P   Y L         S
;-> P K L A T L L A M S   A
;->         C O B O L     B
;->         A V A J
;-> Matrice problema:
;-> Z D F O R T R A N V C Q
;-> A E G F T A S N T Z Q E
;-> N L I C Q S D Q Y A O B
;-> Y P G P A S C A L D G G
;-> N H K O T E M F G A O H
;-> O I W R L M P Q S R L W
;-> H P P Y G B E P S I L C
;-> T Z H O E L R S N V N I
;-> Y P C P A Y L Q T N O S
;-> P K L A T L L A M S C A
;-> A H N A C O B O L J O B
;-> K L J Q A V A J O A M W

(cercaparole mtx parole)
;-> ((("ASSEMBLY" 2 6 "S") ("FORTRAN" 1 3 "E") ("LISP" 7 11 "O")
;->   ("ALGOL" 2 1 "SE") ("BASIC" 11 12 "N") ("COBOL" 11 5 "E")
;->   ("ADA" 2 6 "SE") ("DELPHI" 1 2 "S") ("SMALLTALK" 10 10 "O")
;->   ("JAVA" 12 8 "O") ("PHP" 7 2 "SE") ("LOGO" 6 11 "N")
;->   ("PASCAL" 4 4 "E") ("PERL" 6 7 "S") ("PYTHON" 10 1 "N")) nil)

La funzione "cercaparole" si trova in "Cercaparole" su "Note libere 3".

Caso delle dimensioni inferiori alla lunghezza delle parole:

(genera-cercaparole parole 8 8)
;-> Attenzione: larghezza(8) > lunghezza massima parole(9).
;-> Attenzione: altezza(8) > lunghezza massima parole(9).
;-> Errore: non riesco a inserire la parola: SMALLTALK
;-> Errore: non riesco a inserire la parola: PASCAL
;-> Errore: non riesco a inserire la parola: PERL
;-> Errore: non riesco a inserire la parola: PYTHON
;-> Matrice soluzione:
;-> L   A   N     O
;->   I S J A V A G
;->   L S   R I   O
;-> L O E P T H B L
;-> O B M P R P A A
;-> G O B H O L S D
;-> L C L P F E I A
;-> A   Y     D C
;-> Matrice problema:
;-> L O A N N I D O
;-> R I S J A V A G
;-> L L S I R I P O
;-> L O E P T H B L
;-> O B M P R P A A
;-> G O B H O L S D
;-> L C L P F E I A
;-> A B Y D E D C S

Con la funzione "matrice2html" (vedi "Matrice --> HTML" su "Note libere 31" possiamo salvare lo schema generato come pagina html:

; crea la pagina html (stringa)
(setq html (matrice2html "Cerca Parole" mtx parole))
; salva su file
(write-file "matrice.html" html)
; visualizza il file html sul browser predefinito
(exec "matrice.html")


----------------
Matrice --> HTML
----------------

Conversione di una matrice in una pagina HTML.

La funzione prende tre parametri:
  1) Titolo (posizionato in al centro)
  2) Matrice (al centro, sotto al Titolo)
  3) Lista parole (al centro sotto alla matrice)
e genera una variabile stringa che contiene la matrice come pagina HTML.

(define (matrice2html titolo matrice parole)
  (let ((html '()))
    ; inizio documento HTML
    (push "<!DOCTYPE html>" html -1)
    (push "<html><head><meta charset='UTF-8'>" html -1)
    (push (string "<title>" titolo "</title>") html -1)
    (push "</head><body>" html -1)
    ; titolo centrato
    (push (string "<h1 style='text-align:center;'>" titolo "</h1>") html -1)
    ; inizio tabella centrata con bordo
    (push "<table style='margin:auto; border-collapse:collapse;'>" html -1)
    ; per ogni riga della matrice
    (dolist (riga matrice)
      (push "<tr>" html -1)
      (dolist (cella riga)
        (push (string "<td style='border:1px solid black; padding:5px; text-align:center;'>"
                      cella
                      "</td>") html -1))
      (push "</tr>" html -1))
    ; fine tabella
    (push "</table>" html -1)
    (when parole
      ; inserisce la lista parole dopo la tabella
      (push "<div style='text-align:center; margin-top:20px;'>" html -1)
      ;(push (join (map string parole) ", ") html -1)
      ; grassetto
      (push (string "<b>" (join (map string parole) ", ") "</b>") html -1)
      (push "</div>" html -1))
    ; chiusura documento
    (push "</body></html>" html -1)
    ; restituisce l'HTML come stringa
    (join html "\n")))

Proviamo:

(setq matrix '(
  ("A" "B" "C")
  ("D" "E" "F")
  ("G" "H" "I")))

; crea la pagina html (stringa)
(setq html (matrice2html "Matrice di Test" matrix '("uno" "due" "tre"))
; salva su file
(write-file "matrice.html" html)
; visualizza il file html sul browser predefinito
(exec "matrice.html")


-------------
Brazilian CPF
-------------

https://codegolf.stackexchange.com/questions/269151/validate-a-cpf-number

Nota:
Tutto il contenuto dei siti di Stack Exchange è rilasciato sotto la licenza CC BY-SA 4.0 (Creative Commons Attribution-ShareAlike 4.0).

Ogni cittadino brasiliano ha un numero di identificazione nazionale associato, denominato CPF.
Questo numero ha 11 cifre (formattate come XXX.XXX.XXX-XX) ed è convalidato dal seguente algoritmo (reale):
  1) Prendi le 9 cifre più significative del numero. Chiamalo c.
  2) Moltiplica l'n-esima cifra meno significativa di c per n+1, dove 1<=n<=9.
  3) Somma tutti i prodotti e calcola il resto della divisione tra questo e 11.
  4) Se il resto r è minore di 2, considera 0. 
     Altrimenti, considera 11 - r. 
     Aggiorna 'c' per includerlo come cifra meno significativa aggiunta.
  5) Prima volta al passaggio 4?
     Torna al passaggio 1, ma ora 1<=n<=10.
     Altrimenti, vai al passaggio 5.
  6) 'c' è uguale al tuo numero iniziale?
     In caso affermativo, il tuo CPF è valido.

Esempio:
Prendiamo 111.444.777-35.

Prendiamo le 9 cifre più significative del numero. Chiamiamolo 'c':
c = 111444777

Moltiplica l'n-esima cifra meno significativa di 'c' per n+1, dove 1 ≤ n ≤ 9:
     c   1 1 1  4  4  4  7  7  7
 (n+1)  10 9 8  7  6  5  4  3  2
result  10 9 8 28 24 20 28 21 14

Somma tutti i prodotti e calcola il resto della divisione tra questo e 11:
10+9+8+28+24+20+28+21+14 = 162 mod 11 = 8

Se il resto r è minore di 2, considera 0.
Altrimenti, considera 11 - r.
Aggiorna 'c' aggiungendo la cifra meno significativa:
Considera 11 - 8 = 3, c = 1114447773.

Torna al punto 1, ma ora 1 ≤ n ≤ 10.
     c   1  1 1  4  4  4  7  7  7 3
 (n+1)  11 10 9  8  7  6  5  4  3 2
result  11 10 9 32 28 24 35 28 21 6

11+10+9+32+28+24+35+28+21+6 = 204 mod 11 = 6

Quindi 11 - 6 = 5, c = 11144477735.

'c' è uguale al numero iniziale? Sì. Pertanto, il CPF è valido.

(define (cpf? code)
  (local (str cifre somma-prodotti val)
    ; creazione numero/stringa 'c' a 9 cifre
    (setq str (select code '(0 1 2 4 5 6 8 9 10)))
    ;(setq num (int str 0 10))
    ; Primo giro
    ; creazione lista delle cifre
    (setq cifre (map (fn(x) (int x 0 10)) (explode str)))
    ; somma dei prodotti cifra*(n+1)
    (setq somma-prodotti (apply + (map * cifre '(10 9 8 7 6 5 4 3 2))))
    ; calcolo del modulo
    ; e aggiunta del valore al numero/stringa --> nuovo numero/stringa
    (if (< (% somma-prodotti 11) 2)
        (push "0" str -1)
        (push (string (- 11 (% somma-prodotti 11))) str -1))
    ; Secondo giro
    ; creazione lista delle cifre del nuovo numero
    (setq cifre (map (fn(x) (int x 0 10)) (explode str)))
    ; somma dei prodotti cifra*(n+1)
    (setq somma-prodotti (apply + (map * cifre '(11 10 9 8 7 6 5 4 3 2))))
    ; calcolo del modulo
    ; e aggiunta del valore al numero/stringa --> nuovo numero/stringa
    (if (< (% somma-prodotti 11) 2)
        (push "0" str -1)
        (push (string (- 11 (% somma-prodotti 11))) str -1))
    ; aggiunge "." "." e "-" al numero-stringa
    (push "." str 3)
    (push "." str 7)
    (push "-" str 11)
    (= code str)))

(cpf? "111.444.777-35") ;-> true
(cpf? "523.986.800-02") ;-> true
(cpf? "058.300.335-42") ;-> true
(cpf? "068.650.863-76") ;-> true
(cpf? "393.852.580-01") ;-> true
(cpf? "392.906.007-80") ;-> true
(cpf? "018.718.637-59") ;-> true
(cpf? "033.373.634-66") ;-> true
(cpf? "663.778.052-92") ;-> true
(cpf? "714.628.314-35") ;-> true
(cpf? "546.692.758-95") ;-> true
(cpf? "240.299.015-57") ;-> true
(cpf? "831.901.302-05") ;-> true
(cpf? "977.278.105-09") ;-> true
(cpf? "701.595.919-45") ;-> true
(cpf? "687.304.519-57") ;-> true
(cpf? "558.298.493-30") ;-> true
(cpf? "301.623.419-03") ;-> true
(cpf? "566.847.323-65") ;-> true
(cpf? "605.053.766-60") ;-> true
(cpf? "295.350.637-31") ;-> true

(cpf? "001.338.908-38") ;-> nil
(cpf? "637.940.265-42") ;-> nil
(cpf? "351.161.559-40") ;-> nil
(cpf? "781.618.495-93") ;-> nil
(cpf? "103.413.164-75") ;-> nil
(cpf? "255.341.928-32") ;-> nil
(cpf? "764.835.030-56") ;-> nil
(cpf? "413.953.767-24") ;-> nil
(cpf? "238.849.696-53") ;-> nil
(cpf? "287.101.226-91") ;-> nil
(cpf? "669.784.801-84") ;-> nil
(cpf? "514.627.048-28") ;-> nil
(cpf? "148.932.528-80") ;-> nil
(cpf? "957.015.430-39") ;-> nil
(cpf? "117.182.278-24") ;-> nil
(cpf? "896.383.465-78") ;-> nil
(cpf? "713.315.098-39") ;-> nil
(cpf? "301.031.051-83") ;-> nil
(cpf? "473.829.973-76") ;-> nil

Possiamo semplificare la funzione nel modo seguente:
1) Usiamo filter per ottenere solo cifre, evitando l'uso manuale di select.
2) Usiamo una funzione 'digit-check' che calcola le cifre finali in modo parametrico.
3) Funziona anche se il CPF è dato in formato stringa senza punti/lineette.

(define (cpf? code)
  (local (digits c sum r)
    ;----------------------
    (define (digit-check n)
      (setq c (slice digits 0 n))
      (setq sum (apply + (map * c (sequence (+ 1 n) 2 -1))))
      (setq r (% sum 11))
      (if (< r 2) 0 (- 11 r)))
    ;----------------------
    ; crea una lista di cifre rimuovendo 
    ; tutti i caratteri non numerici
    (setq digits (map int (filter (fn(x) (<= "0" x "9")) (explode code))))
    (and (= (length digits) 11) ; numero-cifre = 11 ?
         ; prima cifra calcolata = prima cifra ?
         (= (digits 9) (digit-check 9)) 
         ; seconda cifra calcolata = seconda cifra ?
         (= (digits 10) (digit-check 10)))))

Versione code-golf (214 caratteri):

(define(z e)(define(k n)(setq r(%(apply +(map *(slice d 0 n)(sequence(+ 1 n)2 -1)))11))(if(< r 2)0(- 11 r)))
(setq d(map int(filter(fn(x)(<= "0" x "9"))(explode e))))
(and(=(length d)11)(=(d 9)(k 9))(=(d 10)(k 10))))


-------------------------
Messaggio binario segreto
-------------------------

Scriviamo due funzioni "encode" e "decode" per spedire un messaggio segreto.
Le funzioni prendono due parametri:
a) la stringa da codificare/decodificare
b) la stringa dei comandi ("lrfu")

La stringa dei comandi ha il seguente significato:
  "l" --> ruota la stringa di 1 bit a sinistra
  "r" --> ruota la stringa di 1 bit a destra
  "f" --> inverte ogni bit nella stringa di bit
  "u" --> inverte la stringa di bit
I comandi possono essere in qualsiasi ordine, qualche comando può mancare e alcuni possono essere multipli.

La codifica viene effettuata con il seguente algoritmo:
1) Convertire ogni carattere della stringa nel relativo numero ASCII a 7bit.
2) Convertire ogni numero in binario (con 7 caratteri)
3) Unire tutti i numeri binari in un unica stringa
4) Per ogni comando della stringa comandi:
   Se "l" --> ruota la stringa di 1 bit a sinistra
   Se "r" --> ruota la stringa di 1 bit a destra
   Se "f" --> inverte ogni bit nella stringa di bit
   Se "u" --> inverte la stringa di bit

(define (flip-bits bin-str)
  (let (out "")
    (dostring (b bin-str)
      (if (= b 48) ; "0"
          (push "1" out -1)
          (push "0" out -1)))))

(define (encode str cmd)
  (let (binary (join (map (fn(x) (format "%07s" (bits (char x)))) (explode str))))
    (dostring (ch cmd)
      (cond ((= ch 108) (rotate binary -1)) ;l (rotate left)
            ((= ch 114) (rotate binary +1)) ;r (rotate right)
            ((= ch 102) (setq binary (flip-bits binary))) ;f (flip)
            ((= ch 117) (reverse binary)))) ;u (reverse)
    binary))

(setq crypt (encode "newLISP" "lufr"))
;-> "0011110100011010011011011001100001000010110010001"

La decodifica viene effettuata con il seguente algoritmo:
1) Per ogni comando della stringa comandi:
   Se "l" --> ruota la stringa di 1 bit a sinistra
   Se "r" --> ruota la stringa di 1 bit a destra
   Se "f" --> inverte ogni bit nella stringa di bit
   Se "u" --> inverte la stringa di bit
2) Unire tutti i numeri binari in un unica stringa
3) Dividere la stringa in numeri binari da 7 bit.
4) Convertire i numeri binari in caratteri.

Per la decodifica dobbiamo conoscere la stringa dei comandi con cui abbiamo codificato la stringa.
Per costruire la stringa dei comandi della decodifica partendo dalla stringa dei comandi della codifica bisogna effetture i seguenti passaggi:
1) convertire ogni "l" in "r" e ogni "r" in "l"
2) invertire la stringa

Esempio:
codifica comandi = "lufr"
decodifica comandi (scambia l/r) --> "rufl" (inverte stringa) --> "lfur"

(define (decode-cmd cmd)
  (let (out "")
    (for (i 0 (- (length cmd) 1))
      (cond ((= (cmd i) "l") (push "r" out -1))
            ((= (cmd i) "r") (push "l" out -1))
            ((= (cmd i) "u") (push "u" out -1))
            ((= (cmd i) "f") (push "f" out -1))))
    (reverse out)))

(decode-cmd "lufr")
;-> "lfur"

(define (decode str dmc)
  (dostring (ch dmc)
    (cond ((= ch 108) (rotate str -1)) ;l (rotate left)
          ((= ch 114) (rotate str +1)) ;r (rotate right)
          ((= ch 102) (setq str (flip-bits str))) ;f (flip)
          ((= ch 117) (reverse str)))) ;u (reverse)
    (join (map (fn(x) (char (int x 0 2))) (explode str 7))))

(decode crypt (decode-cmd "lufr"))
;-> "newLISP"

(decode (encode "sanpierdarena" "llufr") (decode-cmd "llufr"))
;-> "sanpierdarena"

Vedi anche "Codifica e decodifica di una stringa" su "Note libere 24".

============================================================================

