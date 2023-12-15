(in-package :skyline-tool)

(declaim (optimize (debug 3)))

(define-constant +tia-voices+
    '( ;; Waveform 0 = silent
      (0)
      (1 ;; Waveform 1 = “Buzzy”
       (2096 2080)
       (1048 1040)
       (698.7  693.3)
       (524  520)
       (419.2  416)
       (349.3  346.7)
       (299.4  297.1)
       (262    260)
       (232.9  231.1)
       (209.6  208)
       (190.5  189.1)
       (174.7  173.3)
       (161.2  160)
       (149.7  148.6)
       (139.7  138.7)
       (131    130)
       (123.3  122.4)
       (116.4  115.6)
       (110.3  109.5)
       (104.8  104)
       (99.8    99)
       (95.3    94.5)
       (91.1    90.4)
       (87.3    86.7)
       (83.8    83.2)
       (80.6    80)
       (77.6    77)
       (74.9    74.3)
       (72.3    71.7)
       (69.9    69.3)
       (67.6    67.1)
       (65.5    65))

      (2 ;; Waveform 2 = lower-frequency buzz/rumble
       ( 67.6 67.1)
       ( 33.8 33.5)
       ( 22.5 22.4)
       ( 16.9 16.8)
       ( 13.5 13.4)
       ( 11.3 11.2)
       ( 9.7 9.6)
       ( 8.5 8.4)
       ( 7.5 7.5)
       ( 6.8 6.7)
       ( 6.1 6.1)
       ( 5.6 5.6)
       ( 5.2 5.2)
       ( 4.8 4.8)
       ( 4.5 4.5)
       ( 4.2 4.2)
       ( 4 4)
       ( 3.8 3.7)
       ( 3.6 3.5)
       ( 3.4 3.4)
       ( 3.2 3.2)
       ( 3.1 3)
       ( 3 2.9)
       ( 2.8 2.8)
       ( 2.7 2.7)
       ( 2.6 2.6)
       ( 2.5 2.5)
       ( 2.4 2.4)
       ( 2.3 2.3)
       ( 2.3 2.2)
       ( 2.2 2.2)
       ( 2.1 2.1))

      (;; Waveform 3 = Flangy “UFO”
       ( 67.6 67.1)
       ( 33.8 33.5)
       ( 22.5 22.4)
       ( 16.9 16.8)
       ( 13.5 13.4)
       ( 11.3 11.2)
       ( 9.7 9.6)
       ( 8.5 8.4)
       ( 7.5 7.5)
       ( 6.8 6.7)
       ( 6.1 6.1)
       ( 5.6 5.6)
       ( 5.2 5.2)
       ( 4.8 4.8)
       ( 4.5 4.5)
       ( 4.2 4.2)
       ( 4 4)
       ( 3.8 3.7)
       ( 3.6 3.5)
       ( 3.4 3.4)
       ( 3.2 3.2)
       ( 3.1 3)
       ( 3 2.9)
       ( 2.8 2.8)
       ( 2.7 2.7)
       ( 2.6 2.6)
       ( 2.5 2.5)
       ( 2.4 2.4)
       ( 2.3 2.3)
       ( 2.3 2.2)
       ( 2.2 2.2)
       ( 2.1 2.1))

      (;; Waveform 4 = Pure tone
       (7860 7800)
       (5240 5200)
       (3930 3900)
       (3144 3120)
       (2620 2600)
       (2245.7 2228.6)
       (1965 1950)
       (1746.7 1733.3)
       (1572 1560)
       (1429.1 1418.2)
       (1310 1300)
       (1209.2 1200)
       (1122.9 1114.3)
       (1048 1040)
       (982.5 975)
       (924.7 917.6)
       (873.3 866.7)
       (827.4 821.1)
       (786 780)
       (748.6 742.9)
       (714.5 709.1)
       (683.5 678.3)
       (655 650)
       (628.8 624)
       (604.6 600)
       (582.2 577.8)
       (561.4 557.1)
       (542.1 537.9)
       (507.1 503.2)
       (491.3 487.5)
       )
      (;; Waveform 5 = Pure tone
       (7860 7800)
       (5240 5200)
       (3930 3900)
       (3144 3120)
       (2620 2600)
       (2245.7 2228.6)
       (1965 1950)
       (1746.7 1733.3)
       (1572 1560)
       (1429.1 1418.2)
       (1310 1300)
       (1209.2 1200)
       (1122.9 1114.3)
       (1048 1040)
       (982.5 975)
       (924.7 917.6)
       (873.3 866.7)
       (827.4 821.1)
       (786 780)
       (748.6 742.9)
       (714.5 709.1)
       (683.5 678.3)
       (655 650)
       (628.8 624)
       (604.6 600)
       (582.2 577.8)
       (561.4 557.1)
       (542.1 537.9)
       (507.1 503.2)
       (491.3 487.5)
       )

      (;; Waveform 6 = Somewhere between Pure & Buzzy
       (   1014.2     987.8 )
       (      507.1     493.9 )
       (      338.1     329.6 )
       (      253.5     246.9 )
       (      202.83    207.7 )
       (      169       164.8 )
       (      144.9     146.8 )
       (      126.8     123.5 )
       (      112.7     110   )
       (      101.42    103.8 )
       (      92.22     92.5 )
       (      84.5      82.4 )
       (      78  2     77.8 )
       (      72.4      73.4 )
       (      67.62     69.3 )
       (      63.4      61.7 )
       (      59.71     58.3 )
       (      56.3      55   )
       (      53.41     51.9 )
       (      50.71     51.9 )
       (      48.3      49   )
       (      46.11     46.2 )
       (      44.1      43.7 )
       (      42.3      41.2 )
       (      40.6      41.2 )
       (      39  1     38.9 )
       (      37.6      36.7 )
       (      36.2      36.7 )
       (      35  1     34.6 )
       (      33.81     34.6 )
       (      32.7      32.7 )
       (      31.7      30.9 )
       )(;; Waveform 7 = Somewhere between Pure & Buzzy
       (   1014.2     987.8 )
       (      507.1     493.9 )
       (      338.1     329.6 )
       (      253.5     246.9 )
       (      202.83    207.7 )
       (      169       164.8 )
       (      144.9     146.8 )
       (      126.8     123.5 )
       (      112.7     110   )
       (      101.42    103.8 )
       (      92.22     92.5 )
       (      84.5      82.4 )
       (      78  2     77.8 )
       (      72.4      73.4 )
       (      67.62     69.3 )
       (      63.4      61.7 )
       (      59.71     58.3 )
       (      56.3      55   )
       (      53.41     51.9 )
       (      50.71     51.9 )
       (      48.3      49   )
       (      46.11     46.2 )
       (      44.1      43.7 )
       (      42.3      41.2 )
       (      40.6      41.2 )
       (      39  1     38.9 )
       (      37.6      36.7 )
       (      36.2      36.7 )
       (      35  1     34.6 )
       (      33.81     34.6 )
       (      32.7      32.7 )
       (      31.7      30.9 )
       )
      (;;Waveform 8 = white noise
       (     61.5       61.7  )
       (      30.8       30.9  )
       (      20.5       20.6  )
       (      15.4  )
       (      12.3             )
       (      10.3             )
       (      8.8             )
       (      7.7             )
       (      6.8             )
       (      6.2             )
       (      5.6             )
       (      5.1             )
       (      4.7             )
       (      4.4             )
       (      4.1             )
       (      3.8             )
       (      3.6             )
       (      3.4             )
       (      3.2             )
       (      3.1             )
       (      2.9             )
       (      2.8             )
       (      2.7             )
       (      2.6             )
       (      2.5             )
       (      2.4             )
       (      2.3             )
       (      2.2             )
       (      2.1             )
       (      2               )
       (      2               )
       (      1.9)
       )

      (;; Waveform 9 = Somewhere between Pure & Buzzy
       (   1014.2     987.8 )
       (      507.1     493.9 )
       (      338.1     329.6 )
       (      253.5     246.9 )
       (      202.83    207.7 )
       (      169       164.8 )
       (      144.9     146.8 )
       (      126.8     123.5 )
       (      112.7     110   )
       (      101.42    103.8 )
       (      92.22     92.5 )
       (      84.5      82.4 )
       (      78  2     77.8 )
       (      72.4      73.4 )
       (      67.62     69.3 )
       (      63.4      61.7 )
       (      59.71     58.3 )
       (      56.3      55   )
       (      53.41     51.9 )
       (      50.71     51.9 )
       (      48.3      49   )
       (      46.11     46.2 )
       (      44.1      43.7 )
       (      42.3      41.2 )
       (      40.6      41.2 )
       (      39  1     38.9 )
       (      37.6      36.7 )
       (      36.2      36.7 )
       (      35  1     34.6 )
       (      33.81     34.6 )
       (      32.7      32.7 )
       (      31.7      30.9 )
       )(;; Waveform 10 = Somewhere between Pure & Buzzy
       (   1014.2     987.8 )
       (      507.1     493.9 )
       (      338.1     329.6 )
       (      253.5     246.9 )
       (      202.83    207.7 )
       (      169       164.8 )
       (      144.9     146.8 )
       (      126.8     123.5 )
       (      112.7     110   )
       (      101.42    103.8 )
       (      92.22     92.5 )
       (      84.5      82.4 )
       (      78  2     77.8 )
       (      72.4      73.4 )
       (      67.62     69.3 )
       (      63.4      61.7 )
       (      59.71     58.3 )
       (      56.3      55   )
       (      53.41     51.9 )
       (      50.71     51.9 )
       (      48.3      49   )
       (      46.11     46.2 )
       (      44.1      43.7 )
       (      42.3      41.2 )
       (      40.6      41.2 )
       (      39  1     38.9 )
       (      37.6      36.7 )
       (      36.2      36.7 )
       (      35  1     34.6 )
       (      33.81     34.6 )
       (      32.7      32.7 )
       (      31.7      30.9 ))


      (;; Waveform 12 = lower-pitch pure tones
       )

      (;; Waveform 14 = low-pitch electronic tones
       )

      (;; Waveform 15 = low-pitch electronic tones

       ))
  :test #'equalp
  :documentation "NTSC and PAL/SECAM sound values for each frequency code

Format: NTSC followed by PAL/SECAM frequency values for each AUDF value")

(define-constant +vic-notes+
    '()
  :test 'equalp)

(defgeneric midi-to-sound-binary (output-coding machine-type midi-notes sound-chip)
  (:method (output-coding machine-type midi-notes sound-chip)
    (error "No handler for output coding ~s (machine ~s, sound chip ~s); ~
skipping MIDI music with ~:d track~:p"
           output-coding machine-type sound-chip (length midi-notes))))

(defun best-tia-ntsc-note-for (freq &optional (voice 1))
  (let ((notes (mapcar #'first
                       (cdr (elt +tia-voices+ voice)))))
    (when-let (freq-code (position (first (sort (copy-list notes) #'<
                                                :key (curry #'frequency-distance freq)))
                                   notes :test #'=))
      (list voice freq-code (frequency-distance freq (elt notes freq-code))))))

(defun best-tia-pal-note-for (freq &optional (voice 1))
  (let ((notes (mapcar #'second
                       (cdr (elt +tia-voices+ voice)))))
    (when-let (freq-code (position (first (sort (copy-list notes) #'<
                                                :key (curry #'frequency-distance freq)))
                                   notes :test #'=))
      (list voice freq-code (frequency-distance freq (elt notes freq-code))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ooxml->string (xml)
    (if (consp xml)
        (format nil "~{~a~}" (mapcar #'ooxml->string (cddr xml)))
        xml))

  (defun ooxml-cell-repeats (cell)
    (if-let (repeat-index (and (consp (second cell))
                               (consp (first (second cell)))
                               (position-if (lambda (attr) (equal (first attr) "number-columns-repeated"))
                                            (second cell))))
      (parse-integer (second (elt (second cell) repeat-index)))
      1))

  (defun ooxml-repeated-cell (cell string)
    (loop repeat (ooxml-cell-repeats cell)
          collect string))

  (defun ods-table-rows->list (table)
    (mapcar (lambda (row)
              (loop for cell in (remove-if-not (lambda (el) (equal (caar el) "table-cell"))
                                               row)
                    append (ooxml-repeated-cell cell (ooxml->string cell))))
            (mapcar #'rest (remove-if-not (lambda (el)
                                            (equal (caar el) "table-row"))
                                          table))))

  (defun midi->note-name (note)
    (multiple-value-bind (octave letter) (floor (- note 9) 12)
      (format nil "~a ~d" (elt '("A" "A♯" "B" "C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯") letter)
              octave)))

  (defun note->midi-note-number (octave note-name)
    (+ 9
       (* 12 octave)
       (or (position note-name '("A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#")
                     :test #'string-equal)
           (position note-name '("A" "A♯" "B" "C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯")
                     :test #'string-equal)
           (let ((index (position note-name '( "A♭" "A" "B♭" "B" "C" "D♭" "D" "E♭" "E" "F" "G♭" "G")
                                  :test #'string-equal)))
             (return-from note->midi-note-number (+ 8 (* 12 octave) index))))))

  (assert (= 60 (note->midi-note-number 4 "C")) ()
          "Note->MIDI-Note-Number is not tuned correctly")

  (defun interpret-pokey-sheet1 (sheet1)
    (assert (eql 0 (search "POKEY table" (aref sheet1 2 0))) (sheet1)
            "POKEY tables not in expected format~%~s" (aref sheet1 2 0))
    (assert (equal "C" (aref sheet1 0 3)) ()
            "Table does not start with C~%~s" (aref sheet1 0 3))
    (loop for row from 3 below 112
          for out-row from 0
          with notes = (make-array (list 18 109) :element-type '(or null (unsigned-byte 16)))
          with octave = 1
          do (progn
               (when (eql 0 (search "OCTAVE " (aref sheet1 35 row)))
                 (setf octave (parse-integer (subseq (aref sheet1 35 row) 7))))
               (setf (aref notes 0 out-row) (note->midi-note-number
                                             octave
                                             (string-trim " " (aref sheet1 0 row))))
               (loop for column from 1 below 35 by 2
                     for out-column from 1
                     do (let ((number$ (aref sheet1 column row)))
                          (setf (aref notes out-column out-row)
                                (if (and (not (emptyp number$))
                                         (every #'digit-char-p number$))
                                    (parse-integer number$)
                                    nil)))))
          finally (return notes)))

  (defun interpret-pokey-sheet2 (sheet2)
    (return-from interpret-pokey-sheet2 nil)
    (assert (eql 0 (search "POKEY table" (aref sheet2 2 0))) (sheet2)
            "POKEY tables not in expected format~%~s" (aref sheet2 2 0))
    (assert (equal "C" (aref sheet2 0 3)) ()
            "Table does not start with C~%~s" (aref sheet2 0 3))
    (loop for row from 3 below 112
          for out-row from 0
          with notes = (make-array (list 18 109) :element-type '(or null (unsigned-byte 16)))
          with octave = 1
          do (progn
               (when (eql 0 (search "OCTAVE " (aref sheet2 35 row)))
                 (setf octave (parse-integer (subseq (aref sheet2 35 row) 7))))
               (setf (aref notes 0 out-row) (note->midi-note-number
                                             octave
                                             (string-trim " " (aref sheet2 0 row))))
               (loop for column from 1 below 35 by 2
                     for out-column from 1
                     do (let ((number$ (aref sheet2 column row)))
                          (setf (aref notes out-column out-row)
                                (if (and (not (emptyp number$))
                                         (every #'digit-char-p number$))
                                    (parse-integer number$)
                                    nil)))))
          finally (return notes)))

  (defun interpret-pokey-tables (tables)
    (destructuring-bind (sheet1 sheet2 sheet3 sheet4) tables
      (declare (ignore sheet3 sheet4))
      (interpret-pokey-sheet2 sheet2) ; TODO, ignored
      (interpret-pokey-sheet1 sheet1)))

  (defun lists->2d-array (lists)
    (let ((rows (length lists))
          (cols (reduce #'max (mapcar #'length lists))))
      (loop with array = (make-array (list cols rows))
            for row in lists
            for r from 0
            do (loop for column in row
                     for c from 0
                     do (setf (aref array c r) column))
            finally (return array))))

  (defun read-pokey.ods ()
    (format *trace-output* "~&Reading Synthpopalooza's POKEY tables spreadsheet… ")
    (prog1
        (zip:with-zipfile (zip (make-pathname
                                :name "Synthpopalooza POKEY tables"
                                :type "ods"
                                :directory (append (pathname-directory
                                                    (asdf:system-source-directory :skyline-tool))
                                                   (list "data")))
                           :force-utf-8 t)
          (let ((xml (xmls:parse-to-list
                      (map 'string #'code-char
                           (zip:zipfile-entry-contents
                            (zip:get-zipfile-entry "content.xml" zip))))))
            (assert (and (consp (first xml))
                         (equal (car (first xml)) "document-content")
                         (equal (cdr (first xml))
                                "urn:oasis:names:tc:opendocument:xmlns:office:1.0"))
                    (xml)
                    "ODS file seems to be malformed: document-content tag missing or invalid~%~s"
                    (first xml))
            (let ((body (first (remove-if-not (lambda (el) (equal (caar el) "body")) (rest xml)))))
              (assert (and (consp (caaddr body))
                           (equal (car (caaddr body)) "spreadsheet")
                           (equal (cdr (caaddr body)) "urn:oasis:names:tc:opendocument:xmlns:office:1.0"))
                      (xml)
                      "ODS is not a spreadsheet?~%~s"
                      (first body))
              (let* ((tables (mapcar #'cdr
                                     (remove-if-not (lambda (el) (equal (caar el) "table"))
                                                    (subseq (caddr body) 2)))))
                (mapcar #'ods-table-rows->list tables)))))
      (format *trace-output* " done.")))

  (defun read-pokey-tables ()
    (interpret-pokey-tables (mapcar #'lists->2d-array (read-pokey.ods)))))

(define-constant +pokey-notes-table+
    (read-pokey-tables)
  :test 'equalp)

(defun pokey-distortion-column (distortion bits)
  (+ (ecase bits
       (16 0)
       (8 1))
     (ecase distortion
       (:10 1)
       (:2 4)
       (:12a 7)
       (:12b 10)
       (:8 12)
       (:4b 14)
       (:4a 16))))

(assert (= 97 (aref +pokey-notes-table+ (pokey-distortion-column :12a 8) 5))
        () "POKEY notes table seems to be incorrect")

(defun best-pokey-note-for (midi-note-number distortion bits)
  (loop for row from 0
        when (= midi-note-number (aref +pokey-notes-table+ 0 row))
          do (let ((value (aref +pokey-notes-table+ (pokey-distortion-column distortion bits) row)))
               (when (not (null value))
                 (assert (< value (expt 2 bits))))
               (return value))
        finally (return nil)))

(defun null-if-zero-note (n)
  (if (or (null n) (zerop (lastcar n))) nil n))

(defun best-tia-note-for-ntsc (note)
  (labels ((nearest (&rest set)
             (first (sort set #'< :key #'third))))
    (nearest (null-if-zero-note (best-tia-ntsc-note-for note 4))
             (null-if-zero-note (best-tia-ntsc-note-for note 5))
             (null-if-zero-note (best-tia-ntsc-note-for note 2))
             (null-if-zero-note (best-tia-ntsc-note-for note 6))
             (null-if-zero-note (best-tia-ntsc-note-for note 8))
             (list 0 0 most-positive-fixnum))))

(defun best-tia-note-for-pal (note)
  (labels ((nearest (&rest set)
             (first (sort set #'< :key #'third))))
    (nearest (null-if-zero-note (best-tia-pal-note-for note 4))
             (null-if-zero-note (best-tia-pal-note-for note 5))
             (null-if-zero-note (best-tia-pal-note-for note 2))
             (null-if-zero-note (best-tia-pal-note-for note 6))
             (null-if-zero-note (best-tia-pal-note-for note 8))
             (list 0 0 most-positive-fixnum))))

(defun array<-7800-tia-notes-list (notes tv-type)
  (let ((frame-rate (ecase tv-type (:ntsc 60) (:pal 50))))
    (coerce (sort (adjust-note-timing-for-frame-rate (merge-tia-voices notes) frame-rate)
                  #'< :key #'second)
            'vector)))

(defun array<-tia-notes-list (list output-coding)
  (let ((array (make-array (list (length list) 5))))
    (loop for note in list
          for i from 0
          for (control freq)
            =
            (if-let (note (elt note 2))
              (ecase output-coding
                (:ntsc
                 (best-tia-note-for-ntsc note))
                (:pal
                 (best-tia-note-for-pal note)))
              (list nil nil))
          do (setf (aref array i 0) (when (elt note 0)
                                      (floor (max (/ (elt note 0) +midi-duration-divisor+)
                                                  1))) ; duration
                   (aref array i 1) control            ;control
                   (aref array i 2) freq               ;frequency
                   (aref array i 3) (when (elt note 3)
                                      (floor (elt note 3))) ; volume
                   (aref array i 4) (elt note 4)))          ;comment
    array))

(defun midi->2600-tia (notes)
  (let ((volume 8)
        (output (list)))
    (loop for note in notes
          for i from 0
          for (note/rest . info) = note
          do (ecase note/rest
               (:note
                (push (make-array 5 :initial-contents (list (getf info :duration)
                                                            0
                                                            (freq<-midi-key (getf info :key))
                                                            volume
                                                            (nth-value 2 (key<-midi-key (getf info :key)))))
                      output))
               (:rest (push (make-array 5 :initial-contents (list (getf info :duration) 0 0 0 nil))
                            output))
               (:text (push (make-array 5 :initial-contents (list nil nil nil nil info))
                            output))))
    (reverse output)))

(defun find-tia-distortion (params)
  (warn "Not finding best TIA distortion for ~a" params)
  1)

(defun midi->7800-tia (midi-notes tv)
  (loop for track in midi-notes
        with distortion
        with tia-voices = (make-array 2 :initial-element nil)
        append
        (loop for (kind . params) in track
              collecting (ecase kind
                           (:text (setf distortion (find-tia-distortion params)))
                           (:rest)
                           (:note (destructuring-bind (&key time key duration) params
                                    (typecase distortion
                                      (null
                                       (warn "Note without knowing instrument: dropping note ~a at time ~d"
                                             (midi->note-name key) time))
                                      (number
                                       (let ((voice (find-free-voice tia-voices time)))
                                         (if voice
                                             (setf (aref tia-voices voice)
                                                   (cons (list time (best-tia-note-for key distortion tv)
                                                               duration distortion)
                                                         (aref tia-voices voice)))
                                             (warn "Too much polyphony: dropping note ~a at time ~d (TIA 7800)"
                                                   (midi->note-name key) time))))
                                      (t (error "Unhandled DISTORTION ~s for TIA only" distortion)))))))
        finally (return tia-voices)))

(defun voice-free-at-time-p (voice time)
  (or (emptyp voice)
      (notany (lambda (note)
                (destructuring-bind (start key duration distortion) note
                  (declare (ignore key distortion))
                  (<= start time (+ start duration))))
              voice)))

(defun find-free-voice (voices time)
  (position-if (lambda (voice) (voice-free-at-time-p voice time))
               voices))

(defun find-pokey-distortion (text)
  (cond ((equal (first text) "Piano") :10)
        ((equal (first text) "Drums") :12a)
        (t (warn "Ignored text in MIDI: ~s" text))))

(defun best-tia-note-for (key distortion tv)
  (let ((freq (freq<-midi-key key)))
    (elt (ecase tv
           (:ntsc (or (best-tia-ntsc-note-for freq distortion)
                      (best-tia-note-for-ntsc freq)))
           (:pal (or (best-tia-pal-note-for freq distortion)
                     (best-tia-note-for-pal freq))))
         0)))

(defun midi->pokey (midi-notes tv)
  (loop for track in midi-notes
        with distortion
        with voices = (make-array 4 :initial-element nil)
        with tia-voices = (make-array 2 :initial-element nil)
        append
        (loop for object in midi-notes
              collecting (destructuring-bind (kind . params) object
                           (ecase kind
                             (:text (setf distortion (find-pokey-distortion params)))
                             (:rest)
                             (:note (destructuring-bind (&key time key duration) params
                                      (typecase distortion
                                        (null
                                         (warn "Note without knowing instrument: dropping note ~a at time ~d (POKEY)"
                                               (midi->note-name key) time))
                                        (symbol
                                         (let ((voice (find-free-voice voices time)))
                                           (if voice
                                               (if-let ((note (best-pokey-note-for key distortion 8)))
                                                 (setf (aref voices voice)
                                                       (cons (list time note duration distortion)
                                                             (aref voices voice)))
                                                 (warn "No POKEY note for ~a at time ~d"
                                                       (midi->note-name key) time))
                                               (warn "Too much polyphony: dropping POKEY note ~a at time ~d"
                                                     (midi->note-name key) time))))
                                        (number
                                         (let ((voice (find-free-voice tia-voices time)))
                                           (if voice
                                               (if-let (note (best-tia-note-for key distortion tv))
                                                 (setf (aref voices voice)
                                                       (cons (list time note duration distortion)
                                                             (aref voices voice)))
                                                 (warn "No TIA note for ~a at time ~d"
                                                       (midi->note-name key) time))
                                               (warn "Too much polyphony: dropping TIA note ~a at time ~d"
                                                     (midi->note-name key) time))))
                                        (t (error "Unhandled DISTORTION ~s" distortion))))))))
        finally (return (list voices tia-voices))))

(defun merge-pokey-tia-voices (notes)
  (destructuring-bind (pokey tia) notes
    (append
     (loop for voice below 4
           append (mapcar (lambda (note)
                            (cons voice note))
                          (aref pokey voice)))
     (loop for voice below 2
           for voice-code from #x80
           append (mapcar (lambda (note)
                            (cons voice-code note))
                          (aref tia voice))))))

(defun merge-tia-voices (notes)
  (loop for voice below 2
        for voice-code from #x80
        append (mapcar (lambda (note) (cons voice-code note))
                       (aref notes voice))))

(defun adjust-note-timing-for-frame-rate (notes frame-rate)
  (loop for (voice time key duration distortion) in notes
        ;; 454 = convert 1⁄4 note to seconds at 120bpm
        collecting (list voice (floor (* (/ time 454) 1/2 frame-rate))
                         key
                         (floor (* (/ duration 454) 1/2 frame-rate))
                         distortion)))

(defun array<-pokey-notes-list (notes tv-type)
  (let ((frame-rate (ecase tv-type (:ntsc 60) (:pal 50))))
    (coerce (sort (adjust-note-timing-for-frame-rate (merge-pokey-tia-voices notes) frame-rate)
                  #'< :key #'second)
            'vector)))

(defmethod midi-to-sound-binary (output-coding machine-type midi-notes (sound (eql :pokey)))
  (array<-pokey-notes-list (midi->pokey (car midi-notes) output-coding) output-coding))

(defmethod midi-to-sound-binary (output-coding (machine-type (eql 2600)) midi-notes (sound (eql :tia)))
  (array<-tia-notes-list (midi->2600-tia (car midi-notes)) output-coding))

(defmethod midi-to-sound-binary (output-coding (machine-type (eql 7800)) midi-notes (sound (eql :tia)))
  (array<-7800-tia-notes-list (midi->7800-tia midi-notes output-coding) output-coding))

(defun collect-midi-texts (midi)
  (loop for track in (midi:midifile-tracks midi)
        for track-number from 0
        appending (loop for chunk in track
                        when (typep chunk 'midi::text-message)
                          collect (format nil "~& ~d. ~:(~a~) “~a”"
                                          track-number (type-of chunk)
                                          (slot-value chunk 'midi::text)))))

(defun midi-track-notes-count (track)
  (count-if (lambda (chunk)
              (typep chunk 'midi::note-on-message))
            track))

(defun midi-tracks-with-music (midi)
  (remove-if-not (lambda (track)
                   (plusp (midi-track-notes-count track)))
                 (midi:midifile-tracks midi)))

(defconstant +a4/hz+ 440
  "The frequency (Hz) of the A in octave 4; by convention, 440Hz.")

(define-constant +semitone+ (expt 2 1/12)
  :documentation "The ratio of each semitone in equal temperment"
  :test #'=)

(defun midi-distance-from-a4 (key)
  (- key 69))

(defun freq<-midi-key (key)
  (* (expt 2 (/ (midi-distance-from-a4 key) 12)) +a4/hz+))

(defun frequency-distance (a b)
  (cond ((or (null a) (null b)) most-positive-fixnum)
        ((> b a)
         (frequency-distance b a))
        ((zerop b) most-positive-fixnum)
        (t
         (log (/ a b) 2))))

(defun read-midi (file)
  (let ((midi (midi:read-midi-file file)))
    (let ((parts/quarter (midi::midifile-division midi))
          (texts (collect-midi-texts midi))
          (real-tracks (midi-tracks-with-music midi)))
      (when (zerop (length real-tracks))
        (error "File ~a contains no tracks with actual music?
Gathered text:~{~% • ~a~}"
               file texts))
      (map 'list (lambda (track) (midi-track-decode track parts/quarter))
           real-tracks))))

(defun import-music-for-playlist (output-coding line catalog comments-catalog
                                  &key ((:sound-chip sound-chip$)))
  (let ((sound-chip (make-keyword sound-chip$)))
    (when (or (not (find #\= line))
              (char= #\; (first-elt (string-trim #(#\Space #\Tab) line))))
      (return-from import-music-for-playlist nil))
    (destructuring-bind (symbol-name$ midi-file-name$)
        (split-string line :separator "=")
      (let ((symbol-name (make-keyword (string-trim " " symbol-name$)))
            (midi-file-name (string-trim " " midi-file-name$)))
        (format *trace-output* "~&Converting MIDI file ~a to ~a format for ~a…"
                midi-file-name sound-chip output-coding)
        (multiple-value-bind (numbers comments)
            (midi-to-sound-binary output-coding
                                  *machine*
                                  (read-midi midi-file-name)
                                  sound-chip)
          (format *trace-output* "~& - Generated ~:d sound value~:p…" (first (array-dimensions numbers)))
          (when (plusp (first (array-dimensions numbers)))
            (setf (gethash symbol-name catalog) numbers
                  (gethash symbol-name comments-catalog) comments)))))))

(defun edit-long-notes (table)
  (let ((list (loop for note from 0 below (array-dimension table 0)
                    if (< (aref table note 0) #x80)
                      collect (list (aref table note 0)
                                    (aref table note 2))
                    else
                      append (list (list (floor (aref table note 0) #x80)
                                         (aref table note 2))
                                   (list (mod (aref table note 0) #x80)
                                         (aref table note 2))))))
    (make-array (list (length list) 2)
                :element-type '(unsigned-byte 8)
                :initial-contents list)))

(defvar *room-available-for-music%* 2000) ; FIXME

(defmacro memoize (var &body fn)
  `(or (and (boundp ',var) ,var)
       (setf ,var (progn ,@fn))))

(defun room-available-for-music ()
  (memoize *room-available-for-music%*
    (- #x1000
       (find-size-of-bank)
       #x100)))

(defun increment-name (name)
  (check-type name symbol)
  (let ((name$ (string name)))
    (if (digit-char-p (last-elt name$))
        (let ((number-starts (1+ (position-if-not #'digit-char-p
                                                  name$
                                                  :from-end t))))
          (intern (concatenate
                   'string
                   (subseq name$ 0 number-starts)
                   (princ-to-string
                    (1+ (parse-integer
                         (subseq name$ number-starts)))))))
        (intern (concatenate 'string name$ "-2")
                (symbol-package name)))))

(defun fill-array² (destination source start-index end-index)
  (loop
    for x₀ from (first start-index) upto (first end-index)
    for x₁ from 0
    do (loop
         for y₀ from (second start-index) upto (second end-index)
         for y₁ from 0
         do (setf (aref destination x₁ y₁) (aref source x₀ y₀))))
  destination)

(defun song-split (array bytes)
  (let ((split-length (floor bytes 2)))
    (let ((before (fill-array² (make-array (list (1+ split-length) 2)
                                           :element-type '(unsigned-byte 8)
                                           :initial-element #xff)
                               array
                               '(0 0) (list (1- split-length) 1)))
          (after
            (fill-array² (make-array (list (- (array-dimension array 0)
                                              split-length)
                                           2)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)
                         array (list split-length 1)
                         (mapcar #'1- (array-dimensions array)))))
      (setf (aref before split-length 0) #xff
            (aref before split-length 1) #xff)
      (values before after))))

(defun split-long-songs (songs)
  (loop
    with room-used = 0
    for (title . notes) = (pop songs)
    for song-bytes = (* 2 (array-dimension notes 0))
    collect
    (if (< (room-available-for-music)
           (+ room-used song-bytes))
        ;; split the song into two with a continuation flag
        (progn
          (format *trace-output* "~&Splitting song “~a” after ~:d bytes …"
                  title (- (room-available-for-music)
                           room-used
                           2))
          (multiple-value-bind (before after)
              (song-split notes (- (room-available-for-music)
                                   room-used
                                   2))
            (check-type before (array (unsigned-byte 8) (* 2)))
            (check-type after (array (unsigned-byte 8) (* 2)))
            (prog1 (cons title before)
              (push (cons (increment-name title) after)
                    songs)
              (setf room-used 0))))
        (prog1
            (cons title notes)
          (incf room-used song-bytes)))))

(defun song-bytes (song)
  (check-type song cons)
  (check-type (car song) symbol)
  (check-type (cdr song) (array * (* 2)))
  (* 2 (array-dimension (cdr song) 1)))


(defun assign-repertoire-to-banks (repertoire)
  (loop
    with assignments = nil
    for music-bank downfrom 128
    while repertoire
    do (loop with room = (room-available-for-music)
             for length = (song-bytes (first repertoire))
             while (> room length)
             do (progn
                  (decf room length)
                  (push (list music-bank (pop repertoire)) assignments)))
    finally
       (return assignments)))

(defconstant +midi-duration-divisor+ 4)

(defun write-song-data-to-file (title notes source-file)
  (format source-file "~%;;;~|~%~a:" (assembler-label-name title))
  (fresh-line *trace-output*)
  (loop for i below (array-dimension notes 0)
        do (let ((duration (aref notes i 0))
                 (control (aref notes i 1))
                 (frequency (aref notes i 2))
                 (volume (aref notes i 3))
                 (comment (aref notes i 4)))
             (princ "♪" *trace-output*)
             (finish-output *trace-output*)
             (if (or (null duration) (< duration +midi-duration-divisor+))
                 (if (= i (1- (array-dimension notes 0)))
                     (format source-file "~%	.sound 0, 0, 0, 0, 1	; ~a" comment)
                     (format source-file "~%	;; ~a" comment))
                 (format source-file "~%	.sound $~x, $~x, $~2,'0x, ~3d, ~d	; ~a"
                         volume
                         control
                         frequency
                         (round (min (max 1 (/ duration +midi-duration-divisor+)) #xff))
                         (if (= i (1- (array-dimension notes 0))) ; last note
                             1
                             0)
                         comment))
             (finish-output source-file)))
  (format source-file "~2%;;; end of ~a" (assembler-label-name title)))

(defgeneric write-song-data-to-binary (notes object machine sound-chip)
  (:method (notes object machine sound-chip)
    (error "Unimplemented: Cannot write notes for machine ~a, sound-chip ~a" machine sound-chip)))

(defmethod write-song-data-to-binary (notes object (machine (eql 2600)) (sound-chip (eql :TIA)))
  (loop for i below (array-dimension notes 0)
        do (let ((duration (aref notes i 0))
                 (control (aref notes i 1))
                 (frequency (aref notes i 2))
                 (volume (aref notes i 3))
                 (comment (aref notes i 4)))
             (unless (or (symbolp comment) (emptyp comment))
               (format *trace-output* "~&;;; ~a" comment))
             (if (or (null duration) (< duration +midi-duration-divisor+))
                 (when (= i (1- (array-dimension notes 0)))
                   (write-bytes #(0 0 0 0 1) object))
                 (write-bytes (vector
                               volume control frequency
                               (round (min (max 1 (/ duration +midi-duration-divisor+)) #xff))
                               (if (= i (1- (array-dimension notes 0))) ; last note
                                   1
                                   0))
                              object)))))

(defun pokey-distortion-code (distortion) ; FIXME
  (parse-integer (symbol-name distortion) :junk-allowed t))

(defmethod write-song-data-to-binary (data object (machine (eql 7800)) sound-chip)
  (loop for note across data
        for now = 0
        with off-notes = (list)
        do (labels
               ((emit-off-notes (until next-voice)
                  (when-let (offs (remove-if-not (lambda (off) (< (first off) until))
                                                 off-notes))
                    (dolist (off (sort offs #'< :key #'first))
                      (destructuring-bind (time voice) off
                        (assert (>= time now))
                        (unless (and (= until time)
                                     (= voice next-voice))
                          (let ((delay (- time now)))
                            (assert (<= 0 delay))
                            (loop while (> delay #xff)
                                  do (progn
                                       (format *trace-output* "~& delay $ff")
                                       (write-bytes (vector #xff #x40 #xff 0) object)
                                       (decf delay #xff)
                                       (incf now #xff)))
                            (assert (<= 0 delay))
                            (format *trace-output* "~& Delay ~d silence voice ~d" delay voice)
                            (write-bytes (vector delay
                                                 #x40
                                                 voice
                                                 0)
                                         object)
                            (incf now delay))))))))
             (destructuring-bind (voice time key duration distortion) note
               (emit-off-notes time voice)
               (let ((delay (- time now)))
                 (loop while (> delay #xff)
                       do (progn
                            (write-bytes (vector #xff #x40 #xff 0) object)
                            (decf delay #xff)
                            (incf now #xff)))
                 (format *trace-output* "~& Delay ~d voice ~d key ~d (+ distortion)" delay voice key)
                 (write-bytes (vector delay voice key
                                      (if (< voice #x80)
                                          (ecase sound-chip
                                            (:pokey (pokey-distortion-code distortion)))
                                          distortion))
                              object)
                 (setf now time)
                 (appendf off-notes (list (list (+ time duration) voice))))))))

(defun assigned-song-bank-and-title (assignment)
  (list (car assignment)
        (assembler-label-name
         (second assignment))))

(defun default-playlist-pathname (file-name playlist-name)
  (make-pathname :defaults playlist-name
                 :name file-name
                 :type "playlist"))

(defun max-of-cars (assignments)
  (reduce #'max (mapcar #'car assignments)))

(defun numbered-bank-filename (basename bank)
  (make-pathname :name (concatenate 'string
                                    (pathname-name basename)
                                    "."
                                    (princ-to-string bank))
                 :defaults basename))

(defun build-repertoire-from-raw-catalog (catalog)
  (split-long-songs
   (loop for symbol-name being the hash-keys of catalog
         collect (cons symbol-name
                       (edit-long-notes
                        (gethash symbol-name catalog))))))

(defun write-music-bank-header (assignments source-file)
  (format source-file ";;; -*- asm -*-
;;; This file generated by Skyline-Tool; editing it is futile.

.include \"preamble.s\"
.include \"music-bank.s'

;;; start of encoded music data from MIDI files
MusicBanks:
~{~%          .byte ~d ; ~a~}
Music:~:*
~{~%          .word ~*~a~} ~%"
          (mapcar #'assigned-song-bank-and-title
                  assignments)))

(defun import-songs-from-playlist-to-catalog (&key
                                                playlist-file-name
                                                output-coding
                                                catalog
                                                comments-catalog)
  (with-input-from-file (playlist-file playlist-file-name)
    (loop for line = (read-line playlist-file nil)
          while line
          do (import-music-for-playlist output-coding line
                                        catalog comments-catalog)))
  (format *trace-output* "~&End of playlist; collected ~r song~:p~
~:*~[~:; in “~a” coding~]."
          (hash-table-count catalog) output-coding))

(defun import-song-to-catalog (&key song-file-name
                                    output-coding
                                    catalog
                                    comments-catalog
                                    (sound-chip "TIA"))
  (import-music-for-playlist output-coding
                             (format nil
                                     "Song_~a=~a"
                                     (pathname-name song-file-name) song-file-name)
                             catalog comments-catalog
                             :sound-chip sound-chip)
  (format *trace-output* "~&Collected ~r song~:p~
~:*~[~:; in “~a” coding~] for ~a."
          (hash-table-count catalog) output-coding sound-chip))

(defun compile-music-7800 (object-name midi-name sound-chip output-coding)
  (let ((*machine* 7800)
        (catalog (make-hash-table))
        (comments-catalog (make-hash-table)))
    (with-output-to-file (object object-name :element-type '(unsigned-byte 8)
                                             :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing ~a…" object-name)
      (import-song-to-catalog :song-file-name midi-name
                              :sound-chip sound-chip
                              :output-coding output-coding
                              :catalog catalog
                              :comments-catalog comments-catalog)
      (loop for symbol being the hash-keys of catalog
            for notes = (gethash symbol catalog)
            do (write-song-data-to-binary notes object *machine* sound-chip)))))

(defun compile-music-2600 (source-out-name in-file-name)
  (let ((catalog (make-hash-table))
        (comments-catalog (make-hash-table)))
    (with-output-to-file (source-out source-out-name :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing ~a…" source-out-name)
      (format source-out ";;; Music compiled from ~a;
;;; do not bother editing (generated file will be overwritten)"
              in-file-name)
      (dolist (output-coding '(:NTSC :PAL))
        (format *trace-output* "Music encoded for ~a TV standard…" output-coding)
        (when (eql :NTSC output-coding)
          (format source-out "~%	.if TV == NTSC~2%"))
        (when (eql :PAL output-coding)
          (format source-out "~%	.else ; PAL or SECAM"))
        (import-song-to-catalog
         :song-file-name in-file-name
         :output-coding output-coding
         :catalog catalog
         :comments-catalog comments-catalog)
        (loop for symbol being the hash-keys of catalog
              for notes = (gethash symbol catalog)
              do (write-song-data-to-file (string symbol) notes source-out)))
      (format source-out "~2%	.fi~%"))
    (format *trace-output* "~&… done.~%")
    (finish-output)))

(defun compile-music (source-out-name in-file-name
                      &optional (machine-type$ "2600")
                                (sound-chip "TIA")
                                (output-coding "NTSC"))
  (let ((*machine* (parse-integer machine-type$)))
    (format *trace-output* "~&Writing music from playlist ~a…" in-file-name)
    (ecase *machine*
      (2600 (compile-music-2600 source-out-name in-file-name))
      (7800 (compile-music-7800 source-out-name in-file-name
                                (make-keyword (string-upcase sound-chip))
                                (make-keyword (string-upcase output-coding)))))))

(defun midi-track-decode (track parts/quarter)
  (declare (ignore parts/quarter))
  (let ((current-time 0)
        (current-note/rest (list :rest))
        (output (list)))
    (labels ((start-note/rest (info)
               #+ (or)  (format *trace-output* "	start ~a" info)
               (when current-note/rest
                 (end-note/rest (getf (cdr info) :time)))
               (setf current-note/rest info))
             (end-note/rest (time)
               #+ (or)  (format *trace-output* "	end ~a at ~d (duration ~d)" current-note/rest time (- time current-time))
               (push (append current-note/rest (list :duration (- time current-time)))
                     output)
               (setf current-note/rest nil)
               (setf current-time time)))
      (loop for chunk in track
            with time-signature-num = 4
            with time-signature-den = 4
            ;; with tempo = 120
            ;; with sec/quarter-note =
            do (typecase chunk
                 (midi::text-message
                  (push (list :text
                              (remove-if (lambda (char) (zerop (char-code char)))
                                         (slot-value chunk 'midi::text)))
                        output)
                  nil)
                 (midi::time-signature-message
                  (setf time-signature-num (midi::message-numerator chunk)
                        time-signature-den (expt 2 (midi::message-denominator chunk)))
                  nil)
                 (midi::tempo-message nil)
                 (midi::control-change-message nil)
                 (midi::note-on-message
                  #+ (or) (format *trace-output* "~&~s" chunk)
                  (with-slots ((key midi::key) (time midi::time)
                               (velocity midi::velocity))
                      chunk
                    (if (plusp velocity)
                        (start-note/rest (list :note :time time :key key))
                        (start-note/rest (list :rest :time time)))))
                 (midi:key-signature-message nil)
                 (midi::reset-all-controllers-message nil)
                 (midi:program-change-message nil)
                 (midi::midi-port-message nil)
                 (t (format t "~&Ignored (unsupported) chunk ~s" chunk)))
            finally (end-note/rest current-time))
      (reverse output))))

(defun key<-midi-key (key)
  (multiple-value-bind (octave-ish note-in-octave) (floor key 12)
    (let ((note-name (nth note-in-octave '(c c♯ d d♯ e f f♯ g g♯ a a♯ b))))
      (values (1- octave-ish) note-in-octave note-name))))

(defconstant +a4/hz+ 440
  "The frequency (Hz) of the A in octave 4; by convention, 440Hz.")

(define-constant +semitone+ (expt 2 1/12)
  :documentation "The ratio of each semitone in equal temperment"
  :test #'=)
