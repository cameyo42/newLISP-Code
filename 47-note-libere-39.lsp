================

 NOTE LIBERE 39

================

  "Esiste una sequenza per ogni cosa..."

------------------------
Analisi di lanci di dadi
------------------------

Per calcolare tutti i risultati (come liste) del lancio di N dadi con numero di facce non necessariamente uguali M1, M2,...MN occorre generare il 'prodotto cartesiano' degli insiemi dei possibili valori di ciascun dado.

Se i dadi hanno rispettivamente M1, M2, ..., MN facce, la funzione deve generare tutte le tuple:
  (1 1 ... 1)
  (1 1 ... 2)
  ...
  (M1 M2 ... MN)

Il numero totale dei risultati vale: M1 * M2 * ... * MN.

Esempio:
  Con 2 dadi: D1 = (1 2) e D1 = (1 2 3),
  abbiamo M = (2 3) e si ottiene: ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3)).

(define (cartesian)
"Calculate the cartesian product of lists"
  (let (out '(()) )
    (dolist (lst (args))
      (let (tmp '())
        (dolist (parz out)
          (dolist (el lst)
            (push (append parz (list el)) tmp -1)))
        (setq out tmp)))
    out))

; Analizza il risultato di tutti i possibili lanci di N dadi qualunque
(define (analyze dadi)
  (local (lanci totale-lanci valori unici frequenze)
    ; ordinamento dei dadi
    (setq dadi (sort (map sort dadi)))
    ; lista con tutti i lanci possibili (lista di liste)
    (setq lanci (apply cartesian dadi))
    ; numero totale di lanci
    (setq totale-lanci (length lanci))
    ; lista con i valori di tutti i lanci
    (setq valori (map (fn(x) (apply + x)) lanci))
    ; lista con i valori unici di tutti i lanci
    (setq unici (unique valori))
    ; lista con le frequenze/occorrenze dei valori unici
    (setq frequenze (count unici valori))
    ; lista di output: (numero frequenza totale-lanci probabilità)
    (map (fn(x y) (list x y totale-lanci
                  (float (format "%3.4f" (div y totale-lanci)))))
         unici frequenze)))

Proviamo:

(setq D (sequence 1 6))
(analyze (list D D))
;-> ((2 1 36 0.0278) (3 2 36 0.0556) (4 3 36 0.0833) (5 4 36 0.1111)
;->  (6 5 36 0.1389) (7 6 36 0.1667) (8 5 36 0.1389) (9 4 36 0.1111)
;->  (10 3 36 0.0833) (11 2 36 0.0556) (12 1 36 0.0278))
(sort (analyze (list D D)) (fn(x y) (>= (last x) (last y))))
;-> ((7 6 36 0.1667) (6 5 36 0.1389) (8 5 36 0.1389) (5 4 36 0.1111)
;->  (9 4 36 0.1111) (4 3 36 0.0833) (10 3 36 0.0833) (3 2 36 0.0556)
;->  (11 2 36 0.0556) (2 1 36 0.0278) (12 1 36 0.0278))

(setq D1 '(1 3 5 7))
(setq D2 '(2 4 6 8))
(analyze (list D1 D2))
;-> ((3 1 16 0.0625) (5 2 16 0.125) (7 3 16 0.1875) (9 4 16 0.25)
;->  (11 3 16 0.1875) (13 2 16 0.125) (15 1 16 0.0625))

(setq D1 '(1 2 3))
(setq D2 '(4 5 6))
(setq D3 '(7 8 9))
(analyze (list D1 D2 D3))
;-> ((12 1 27 0.037) (13 3 27 0.1111) (14 6 27 0.2222) (15 7 27 0.2593)
;->  (16 6 27 0.2222) (17 3 27 0.1111) (18 1 27 0.037))

(setq D1 '(1 5 9))
(setq D2 '(2 4 8))
(setq D3 '(3 6 7))
(analyze (list D1 D2 D3))
;-> ((6 1 27 0.037) (9 1 27 0.037) (10 2 27 0.0741) (8 1 27 0.037)
;->  (11 1 27 0.037) (12 3 27 0.1111) (15 2 27 0.0741) (16 4 27 0.1481)
;->  (13 1 27 0.037) (14 2 27 0.0741) (19 2 27 0.0741) (20 3 27 0.1111)
;->  (17 1 27 0.037) (18 1 27 0.037) (23 1 27 0.037) (24 1 27 0.037))

============================================================================

