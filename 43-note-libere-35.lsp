================

 NOTE LIBERE 35

================

  "Prima di parlare, lascia che le tue parole passino attraverso tre porte:
   È vero? È necessario? È gentile?".

---------------------------------------------------
Massima differenza tra elementi successivi ordinati
---------------------------------------------------

Data una lista di interi, restituire la differenza massima tra due elementi successivi nella sua forma ordinata.
Se la lista contiene meno di due elementi, restituire 0.
Possiamo scrivere un algoritmo che viene eseguito in tempo lineare e utilizza uno spazio extra lineare?

Scriviamo prima una funzione con le primitive di newLISP:

(define (max-diff1 lst)
  (sort lst)
  (apply max (map - (rest lst) (chop lst))))

Proviamo:

(max-diff1 '(3 6 9 1))
;-> 3
(max-diff1 '(9 8 7 6 11 1 2 3 4 5))
;-> 2

Questa funzione non viene eseguita in tempo lineare perchè attraversa la lista 3 volte:
1) (sort lst) --> O(n*log(n))
2) (map ...)  --> O(n)
3) (apply max ...) --> O(n)

Per implementare un algoritmo lineare possiamo utilizzare i principi di bucket sort.
Algoritmo:
1) Trovare i valori minimo e massimo nella lista.
2) Creare bucket con dimensione del bucket calcolata in base all'intervallo (max - min) / (n - 1).
3) Distribuire gli elementi nei bucket e tracciare i valori minimo e massimo in ciascun bucket.
4) Il gap massimo deve essere compreso tra l'elemento massimo di un bucket e l'elemento minimo del bucket successivo non vuoto (non all'interno dello stesso bucket a causa del principio del pigeonhole).
5) Iterare i bucket per trovare il gap massimo tra bucket successivi non vuoti.
Questo metodo garantisce che il gap massimo venga trovato in modo efficiente senza ordinare esplicitamente l'intero lista.

(define (max-diff2 lst)
  (local (MAX-INT MIN-INT len min-val max-val bucket-dim bucket-len
          buckets bucket-idx prev-max max-diff)
  ; Inizializza le costanti
  (setq MAX-INT 9223372036854775807)
  (setq MIN-INT -9223372036854775808)
  ; numero di elementi nella lista
  (setq len (length lst))
  ; Trova i valori minimo e massimo della lista
  (setq min-val MAX-INT)
  (setq max-val MIN-INT)
  (setq min-val (apply min lst))
  (setq max-val (apply max lst))
  ; Calcola la dimensione del bucket usando il principio pigeonhole
  ; La distanza minima possibile del gap massimo vale:
  ; (max-val - min-val) / (len - 1)  
  (setq bucket-dim (max 1 (/ (- max-val min-val) (- len 1))))
  (setq bucket-len (+ 1 (/ (- max-val min-val) bucket-dim)))
  ; Initializza i buckets: ogni bucket ha i valori (min max)
  (setq buckets (array bucket-len (list (list MAX-INT MIN-INT))))
  ; Distribuisce i numeri nei buckets in base ai loro valori
  (dolist (num lst)
    (setq bucket-idx (/ (- num min-val) bucket-dim))
    (setf ((buckets bucket-idx) 0) (min ((buckets bucket-idx) 0) num))
    (setf ((buckets bucket-idx) 1) (max ((buckets bucket-idx) 1) num)))
  ; Trova la differenza massima confrontando i bucket adiacenti non vuoti    
  (setq prev-max MAX-INT)
  (setq max-diff 0)
  (dolist (bucket buckets)
    ; Processiamo solo i bucket non vuoti: (bucket 0) <= (bucket 1).
    ; I bucket vuoti sono quelli in cui risulta: 
    ; (min-val > max-val) dopo l'inizializzazione.
    (when (<= (bucket 0) (bucket 1))
        ; Calcola la differenza il minimo del bucket corrente e
        ; il massimo del bucket precedente
        (setq max-diff (max max-diff (- (bucket 0) prev-max)))
        (setq prev-max (bucket 1))))
  max-diff))

Proviamo:

(max-diff2 '(3 6 9 1))
;-> 3
(max-diff2 '(9 8 7 6 11 1 2 3 4 5))
;-> 2

Test di correttezza:

(for (i 1 1000)
  (setq a (rand 10000 100))
  (if-not (= (max-diff1 a) (max-diff2 a))
    (println a { } (max-diff1 a) { } (max-diff2 a))))
;-> nil

Test di velocità:

(setq b (rand 100000 10000))
(time (max-diff1 b) 1e3)
;-> 2718.869
(time (max-diff2 b) 1e3)
;-> 7281.574

La funzione con le primitive 'max-diff1' è più veloce di quella con l'algoritmo lineare 'max-diff2' (ed è anche più semplice).

============================================================================

