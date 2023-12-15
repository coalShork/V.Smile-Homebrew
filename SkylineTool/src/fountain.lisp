(in-package :skyline-tool)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning))

  (defun all-cdrs (expr)
    "Collect all the CDRs of every CONS child of EXPR"
    (loop for x in (cdr expr)
          collecting (if (consp x)
                         (all-cdrs x)
                         x)))

  (defun variable-refs-p (expr)
    "Does EXPR contain a variable reference (other than PI or E)?"
    (some #'symbolp
          (remove 'pi (remove 'e (all-cdrs expr)))))

  (defun eval-expr (expr)
    "If EXPR is a simple arithmetic expression, reduce it to a constant"
    (assert (member (car expr) '(+ - / * expt sqrt log )))
    (apply (car expr) (mapcar (lambda (term)
                                (if (consp term)
                                    (eval-expr term)
                                    term))
                              (cdr expr))))

  (defun maybe-do-math (expr)
    "If EXPR can be reduced to a constant, do so"
    (etypecase expr
      (symbol (ecase expr
                (pi pi)
                (e (exp 1))))
      (number expr)
      (cons (if (variable-refs-p expr)
                (cons (car expr)
                      (loop for term on (cdr expr)
                            collecting (maybe-do-math term)))
                (eval-expr expr)))))

  (defgeneric cross-quarter-direction (north/south east/west)
    (:documentation "Given a symbol NORTH or SOUTH and EAST or WEST,
return the symbol for the cross-quarter direction, e.g. NORTHEAST")
    (:method ((north (eql 'north)) (east (eql 'east)))
      'northeast)
    (:method ((north (eql 'north)) (west (eql 'west)))
      'northwest)
    (:method ((south (eql 'south)) (east (eql 'east)))
      'southeast)
    (:method ((south (eql 'south)) (west (eql 'west)))
      'southwest)
    (:method ((nor/sou t) (eas/wes t))
      (error "Cross-quarter direction cannot be found from ~s ~s"
             nor/sou eas/wes)))

  (defun stage/when (when conditional comma clauses stop)
    "Stage direction: when CONDITIONAL is true, perform CLAUSES"
    (declare (ignore when comma stop))
    (list 'if conditional clauses))

  (defun stage/if-as-when (if conditional comma clauses stop)
    "Stage direction: a WHEN masquerading as a one-branch IF"
    (declare (ignore if comma stop))
    (list 'if conditional clauses))

  (defun stage/if-otherwise (if conditional comma clauses sem
                                otherwise otherwise-clauses stop)
    "Stage direction: iff CONDITIONAL is true, perform CLAUSES, otherwise, OTHERWISE-CLAUSES"
    (declare (ignore if comma sem otherwise stop))
    (list 'if conditional clauses otherwise-clauses))

  (defun stage/unless (unless conditional comma clauses stop)
    "Stage direction: Unless CONDITIONAL is true, perform CLAUSES."
    (declare (ignore unless comma stop))
    (list 'if conditional nil clauses))

  (defun stage/repeat (repeat numeric times comma clauses)
    "Stage direction: Repeat CLAUSES NUMERIC times"
    (declare (ignore repeat times comma))
    (list 'repeat numeric clauses))

  (defun stage/jump-to-file (continued in file in_ folder)
    "Stage direction: script is continued in another file/asset"
    (declare (ignore continued in in_))
    ;; XXX could use symbolic script IDs here for legibility of generated code
    (list 'jump (find-script-id (concatenate 'string folder "/" file))))

  (defun stage/semicolon-clauses (a sem b)
    "Stage direction: form a program from A followed by B"
    (declare (ignore sem))
    (list 'progn a b))

  (defun stage/do-block (do colon statements done stop)
    "Stage direction: treat STATEMENTS as one program"
    (declare (ignore do colon done stop))
    (list 'progn statements))

  (defun stage/statements-list (statements statement)
    "Stage direction: create a program from STATEMENTS followed by STATEMENT"
    (list 'progn (concatenate 'list statements statement)))

  (defun stage/truck-left/right (truck left/right)
    "Stage direction: Truck the camera LEFT/RIGHT by one tile"
    (declare (ignore truck))
    (list 'camera-move left/right 1))

  (defun stage/dolly-up/down (dolly up/down)
    "Stage direction: Dolly the camera UP/DOWN by one tile"
    (declare (ignore dolly))
    (list 'camera-move up/down 1))

  (defun stage/truck-numeric-left/right (truck numeric left/right)
    "Stage direction: truck the camera LEFT/RIGHT by NUMERIC tiles"
    (declare (ignore truck))
    (list 'camera-move left/right numeric))

  (defun stage/dolly-numeric-up/down (dolly numeric up/down)
    "Stage direction: dolly the camera UP/DOWN by NUMERIC tiles"
    (declare (ignore dolly))
    (list 'camera-move up/down numeric))

  (defun stage/camera-include (move direction to include actor/location)
    "Stage direction: truck/dolly the camera to include ACTOR/LOCATION"
    (declare (ignore move direction to include))
    (list 'camera-include actor/location))

  (defun stage/camera-center (move direction to center
                              on actor/location)
    "Stage direction: truck/dolly the camera to center on ACTOR/LOCATION"
    (declare (ignore move direction to center on))
    (list 'camera-center actor/location))

  (defun stage/camera-frame (frame actor/location and other)
    "Stage direction: truck/dolly the camera to frame both ACTOR/LOCATION and (when possible) also OTHER."
    (declare (ignore frame and))
    (list 'camera-frame actor/location other))

  (defun stage/camera-close (close on actor/location)
    "Stage direction: Center the camera on ACTOR/LOCATION"
    (declare (ignore close on))
    (list 'camera-center actor/location))

  (defun stage/one-beat (beat)
    "Stage direction: Wait one beat"
    (declare (ignore beat))
    (list 'wait 'beat 1))

  (defun stage/numeric-beats (numeric beat)
    "Stage direction: Wait NUMERIC beats"
    (declare (ignore beat))
    (list 'wait 'beat numeric))

  (defun stage/wait-secs (wait for numeric second)
    "Stage direction: Wait NUMERIC seconds"
    (declare (ignore wait for second))
    (list 'wait 'second numeric))

  (defun stage/wait-beats (wait for numeric beat)
    "Stage direction: Wait NUMERIC beats"
    (declare (ignore wait for beat))
    (list 'wait 'beat numeric))

  (defun stage/go (go quoted)
    "Stage direction: Go to QUOTED in the script"
    (declare (ignore go))
    (list 'go quoted))

  (defun stage/go-to (go to quoted)
    "Stage direction: Go to QUOTED in the script"
    (declare (ignore go to))
    (list 'go quoted))

  (defun stage/we-hear-sound (we hear sound)
    "Stage direction: Play SOUND"
    (declare (ignore we hear))
    (list 'hear sound))

  (defun stage/song-starts-playing (song starts playing)
    "Stage direction: Begin playing SONG"
    (declare (ignore starts playing))
    (list 'music 'start song))

  (defun stage/song-stops-playing (song stops playing)
    "Stage direction: Stop SONG if it is playing"
    (declare (ignore stops playing))
    (list 'music 'stop song))

  (defun stage/music-stops (the music stops)
    "Stage direction: Stop all music"
    (declare (ignore the music stops))
    (list 'music 'stop t))

  (defun stage/set-var-to-val (set var to num)
    "Stage direction: assign VAR the value NUM"
    (declare (ignore set to))
    (list 'set (list 'var var) num))

  (defun stage/var←val (var from num)
    "Stage direction: assign VAR the value NUM"
    (declare (ignore from))
    (list 'set (list 'var var) num))

  (defun stage/var<-val (var less tack num)
    (declare (ignore less tack))
    (list 'set (list 'var var) num))

  (defun stage/set-state-to-val (set state to num)
    (declare (ignore set to))
    (list 'set state num))

  (defun stage/state←val (state from num)
    (declare (ignore from))
    (list 'set state num))

  (defun stage/state<-val (state less tack num)
    (declare (ignore less tack))
    (list 'set state num))

  (defun stage/set-flag-to-bool (set flag to bool)
    (declare (ignore set to))
    (list 'set flag bool))

  (defun stage/ship-can-go (the ship can go to place)
    (declare (ignore the ship can go to))
    (list 'ship '+ place))

  (defun stage/ship-can-not-go (the ship can not go to place)
    (declare (ignore the ship can not go to))
    (list 'ship '- place))

  (defun stage/player-gains/loses-crowns
      (the player gains/loses numeric crowns)
    (declare (ignore the player crowns))
    (list 'alter (list 'player 'crowns) gains/loses numeric))

  (defun stage/player-gains/loses-arrows
      (the player gains/loses numeric arrows)
    (declare (ignore the player arrows))
    (list 'alter (list 'player 'arrows) gains/loses numeric))

  (defun stage/player-gains/loses-karma
      (the player gains/loses numeric karma)
    (declare (ignore the player karma))
    (list 'alter (list 'player 'karma) gains/loses numeric))

  (defun stage/player-gains/loses-item (the player gains/loses the* item)
    (declare (ignore the player the*))
    (list 'inventory gains/loses item))

  (defun stage/player-gains-armor (the player gains armor numeric)
    (declare (ignore the player gains armor))
    (list 'inventory '+
          (ecase numeric
            (1 'armor1) (2 'armor2) (3 'armor3) (4 'armor4))))

  (defun stage/player-gains-ring (the player gains ring numeric)
    (declare (ignore the player gains ring))
    (list 'inventory '+
          (ecase numeric
            (1 'ring1) (2 'ring2) (3 'ring3) (4 'ring4))))

  (defun stage/player-gains/loses-quest-item
      (the player gains/loses quoted)
    (declare (ignore the player))
    (list 'inventory gains/loses quoted))

  (defun stage/gives-item (one gives the item to other)
    (declare (ignore gives the to))
    (list 'give one other item))

  (defun stage/inc/dec-state-1 (inc/dec state)
    (list inc/dec state 1 (ecase inc/dec
                            (inc #xff)
                            (dec 0))))

  (defun stage/inc/dec-state-by-n (inc/dec state by numeric)
    (declare (ignore by))
    (list inc/dec state numeric (ecase inc/dec
                                  (inc #xff)
                                  (dec 0))))

  (defun stage/inc/dec-state-by-n/limit
      (inc/dec state by numeric comma limit limiter)
    (declare (ignore by comma limit))
    (list inc/dec state numeric limiter))

  (defun stage/inc/dec-state-1/limit
      (inc/dec player-state comma limit numeric)
    (declare (ignore comma limit))
    (list inc/dec player-state 1 numeric))

  (defun stage/all-of (all of a comma and b)
    (declare (ignore all of comma and))
    (list 'every a b))

  (defun stage/any-of (any of a comma or b)
    (declare (ignore any of comma or))
    (list 'some a b))

  (defun stage/none-of (none of a comma nor b)
    (declare (ignore none of comma nor))
    (list 'not-any a b))

  (defun stage/either-or (either a comma or b)
    (declare (ignore either comma or))
    (list 'or a b))

  (defun stage/neither-nor (neither a comma not b)
    (declare (ignore neither comma not))
    (list 'not (list 'or a b)))

  (defun stage/both-and (both a comma and b)
    (declare (ignore both comma and))
    (list 'and a b))

  (defun stage/is-less-than (a is less than b)
    (declare (ignore is less than))
    (list '< a b))

  (defun stage/is-greater-than (a is gt than b)
    (declare (ignore is gt than))
    (list '> a b))

  (defun stage/is-less-than-or-equal-to (a is less than or eq to b)
    (declare (ignore is less than or eq to))
    (list '≤ a b))

  (defun stage/is-greater-than-or-equal-to (a is gt than or eq to b)
    (declare (ignore is gt than or eq to))
    (list '≥ a b))

  (defun stage/is-equal-to (a is eq to b)
    (declare (ignore is eq to))
    (list '= a b))

  (defun stage/num-is-num (a is b)
    (declare (ignore is))
    (list '= a b))

  (defun stage/is-zero (number is zero)
    (declare (ignore is zero))
    (list '= number 0))

  (defun stage/is-not-less-than (a is not less than b)
    (declare (ignore is not less than))
    (list '≥  a b))

  (defun stage/is-not-greater-than (a is not gt than b)
    (declare (ignore is not gt than))
    (list '≤ a b))

  (defun stage/is-not-equal-to (a is not eq to b)
    (declare (ignore is not eq to))
    (list '≠ a b))

  (defun stage/is-not-zero (a is not zero)
    (declare (ignore is not zero))
    (list '≠ a 0))

  (defun stage/is-pos-or-zero (a is pos or zero)
    (declare (ignore is pos or zero))
    (list '≥ a 0))

  (defun stage/is-zero-or-pos (a is zero or pos)
    (declare (ignore is zero or pos))
    (list '≥ a 0))

  (defun stage/is-pos (a is positive)
    (declare (ignore is positive))
    (list '> a 0))

  (defun stage/is-neg (a is neg)
    (declare (ignore is neg))
    (list '< a 0))

  (defun stage/< (a lt b)
    (declare (ignore lt))
    (list '< a b))

  (defun stage/> (a gt b)
    (declare (ignore gt))
    (list '> a b))

  (defun stage/<= (a lt eq b)
    (declare (ignore lt eq))
    (list '≤ a b))

  (defun stage/≤ (a lteq b)
    (declare (ignore lteq))
    (list '≤ a b))

  (defun stage/>= (a gt eq b)
    (declare (ignore gt eq))
    (list '≥ a b))

  (defun stage/≥ (a gteq b)
    (declare (ignore gteq))
    (list '≥ a b))

  (defun stage/= (a eq b)
    (declare (ignore eq))
    (list '= a b))

  (defun stage//= (a not eq b)
    (declare (ignore not eq))
    (list '≠ a b))

  (defun stage/≠ (a neq b)
    (declare (ignore neq))
    (list '≠ a b))

  (defun stage/is-between (a is between min and max)
    (declare (ignore is between and))
    (list '< min a max))

  (defun stage/is-from-to (a is from min to max)
    (declare (ignore is from to))
    (list '≤ min a max))

  (defun stage/parens (lparen expr rparen)
    (declare (ignore lparen rparen))
    expr)

  (defun stage/conditionals-list (cond comma more)
    (declare (ignore comma))
    (list 'progn cond more))

  (defun stage/exit (someone exits)
    (declare (ignore exits))
    (list 'exit someone))

  (defun stage/faces (someone faces dir)
    (declare (ignore faces))
    (list 'face someone dir))

  (defun stage/faces-to (someone faces to the dir)
    (declare (ignore faces to the))
    (list 'face someone dir))

  (defun stage/walks-relative (someone walks relative)
    (declare (ignore walks))
    (list 'walk someone relative))

  (defun stage/relative-to (to loc)
    (declare (ignore to))
    loc)

  (defun stage/relative-steps (rel comma then step)
    (declare (ignore comma then))
    (list 'progn rel step))

  (defun stage/num-to-the-dir (num to the dir)
    (declare (ignore to the))
    (list 'δ num dir))

  (defun stage/num-dir (num dir)
    (list 'δ num dir))

  (defun stage/dir-num (dir num)
    (list 'δ num dir))

  (defun stage/dir-by-num (dir by num)
    (declare (ignore by))
    (list 'δ num dir))

  (defun stage/num-ud-lr (num nor eas)
    (declare (ignore nor eas))
    (list 'δ num 'south num 'east))

  (defun stage/ud-lr-by-num (nor eas by num)
    (declare (ignore nor eas by))
    (list 'δ num 'south num 'east))

  (defun stage/relative-ud-lr (n1 up/down and n2 left/right)
    (declare (ignore and))
    (list 'δ n1 up/down n2 left/right))

  (defun stage/relative-lr-ud (n1 left/right and n2 up/down)
    (declare (ignore and left/right up/down))
    (list 'δ n1 'east n2 'south))

  (defun stage/speech-params
      (actor speaks with pitch p comma speed s)
    (declare (ignore speaks with pitch comma speed))
    (list 'voice actor p s))

  (defun stage/special-appearance (actor looks like special)
    (declare (ignore looks like))
    (list 'look actor special))

  (defun stage/looks-like (actor has desc)
    (declare (ignore has))
    (list 'look actor desc))

  (defun stage/npc-desc-2 (d1 and d2)
    (declare (ignore and))
    (list d1 d2))

  (defun stage/npc-desc-3 (d1 comma1 d2 comma2 and d3)
    (declare (ignore comma1 comma2 and))
    (list d1 d2 d3))

  (defun stage/npc-desc-4 (d1 c1 d2 c2 d3 c3 and d4)
    (declare (ignore c1 c2 c3 and))
    (list d1 d2 d3 d4))

  (defun stage/color-hair (color hair)
    (declare (ignore hair))
    (list 'hair color))

  (defun stage/color-skin (color skin)
    (declare (ignore skin))
    (list 'skin color))

  (defun stage/color-tunic (a color tunic)
    (declare (ignore a tunic))
    (list 'tunic color))

  (defun stage/color-robe (a color robe)
    (declare (ignore a robe))
    (list 'robe color))

  (defun stage/head-num (head number)
    (declare (ignore head))
    (list 'head number))

  (defun stage/num-dir-of-place (num dir of quoted)
    (declare (ignore of))
    (list 'place quoted num dir))

  (defun stage/num-lr-up-of-place (num lr and num2 ud of quoted)
    (declare (ignore and of))
    (list 'place quoted num lr num2 ud))

  (defun stage/num-up-lr-of-place (num ud and num2 lr of quoted)
    (declare (ignore and of))
    (list 'place quoted num2 lr num ud))

  (defun stage/raw-coords (lparen x comma y rparen)
    (declare (ignore lparen comma rparen))
    (list 'place nil x 'east y 'south))

  (defun stage/complex (lparen real plus imag i rparen)
    (declare (ignore lparen plus i rparen))
    (complex real imag))

  (defun stage/sum-of-n+n (the sum of n1 plus n2)
    (declare (ignore the sum of plus))
    (list '+ n1 n2))

  (defun stage/diff-of-n---n (the diff of n1 minus n2)
    (declare (ignore the diff of minus))
    (list '- n1 n2))

  (defun stage/quot-of-n÷n (the quot of n1 div n2)
    (declare (ignore the quot of div))
    (list '/ n1 n2))

  (defun stage/prod-of-n×n (the prod of n1 times n2)
    (declare (ignore the prod of times))
    (list '* n1 n2))

  (defun stage/num-ash-num (the res of n1 shif by n2)
    (declare (ignore the res of shif by))
    (list 'ash n1 n2))

  (defun stage/num-expt-num (the res of n1 raised to the2 n2 pow)
    (declare (ignore the res of raised to the2 pow))
    (list 'expt n1 n2))

  (defun stage/log-base-n-of-n (the log base n2 of n1)
    (declare (ignore the log base of))
    (list 'log n1 n2))

  (defun stage/base-n-log-of-n (the base n2 log of n1)
    (declare (ignore the base log of))
    (list 'log n1 n2))

  (defun stage/nat-log-of-n (the nat log of n)
    (declare (ignore the nat log of))
    (list 'log n))

  (defun stage/sqrt-of-n (the sq rt of num)
    (declare (ignore the sq rt of))
    (list 'sqrt num))

  (defun stage/ceiling-n (the ceil val of num)
    (declare (ignore the ceil val of))
    (list 'ceiling num))

  (defun stage/floor-n (the floor val of num)
    (declare (ignore the floor val of))
    (list 'floor num))

  (defun stage/round-n (the round val of num)
    (declare (ignore the round val of))
    (list 'round num))

  (defun stage/realpart-n (the real part of num)
    (declare (ignore the real part of))
    (list 'realpart num))

  (defun stage/imagpart-n (the imag part of num)
    (declare (ignore the imag part of))
    (list 'imagpart num))

  (defun stage/abs-n (the abs val of num)
    (declare (ignore the abs val of))
    (list 'abs num))

  (defun stage/bool-and (bool n1 and n2)
    (declare (ignore bool and))
    (list 'logand n1 n2))

  (defun stage/bool-or (bool n1 or n2)
    (declare (ignore bool or))
    (list 'logior n1 n2))

  (defun stage/bool-xor (bool ex n1 or n2)
    (declare (ignore bool ex or))
    (list 'logxor n1 n2))

  (defun stage/logior (bool incl n1 or n2)
    (declare (ignore bool incl or))
    (list 'logior n1 n2))

  (defun stage/call-with-regs (call subroutine with registers)
    (declare (ignore call with))
    (list 'progn registers (list 'jsr subroutine)))

  (defun stage/call (call subroutine)
    (declare (ignore call))
    (list 'jsr subroutine))

  (defun stage/regs-list (regs comma reg)
    (declare (ignore comma))
    (list 'progn regs reg))
  
  (defun stage/move-into-reg (reg eq num)
    (declare (ignore eq))
    (list 'mv reg num))
  
  (defun stage/enter (enter someone at place)
    (declare (ignore enter at))
    (list 'enter someone place))
  
  (defun stage/empty-boat (the ship-name appears in _the east/west headed for actor/location)
    (declare (ignore the appears in _the headed for))
    (list 'boat ship-name east/west actor/location nil)) 
  
  (defun stage/full-boat (the ship-name appears in _the east/west with actors aboard
                          headed to/for actor/location)
    (declare (ignore the appears in _the with aboard headed to/for))
    (list 'boat ship-name east/west actor/location actors))
  
  (defun stage/sail-away (the ship-name gets under weigh to _the east/west)
    (declare (ignore the gets under weigh to _the))
    (list 'sail-away ship-name east/west))
  
  (defun stage/embarks (actor embarks/boards the ship-name)
    (declare (ignore embarks/boards the))
    (list 'embark actor ship-name))
  
  (defun stage/disembarks (actor disembarks* the ship-name)
    (declare (ignore disembarks* the))
    (list 'disembark actor ship-name)))

(define-constant +stage-direction-words+
    (mapcar (lambda (x) (intern (symbol-name x) #.*package*))
            '(|(| |)| + |,| - |.| /  × ÷ |:|
              a aboard above absolute all amulet and appears arrow arrows armor at
              base beat beats below black blue boards boolean boots
              both bow brown buckler by
              can captain caspar catamaran ceiling chalice close continued crown crowns cut
              difference disembarks divided do dolly done down durbat
              e earl east either elderembarks enter enters equal exclusive exit exits
              faces floor for frame
              gains gets glass go grand grappling-hook gray greater green
              hair hammer has head headed hear
              if imaginary in include inclusive is
              knife
              left less like logand logarithm logical
              logior logxor looks loses
              magic mask minus moves
              natural nefertem negative none nor north not
              of on or
              part peach pi pirate pitch player player-armor-color
              player-hair-color player-skin-color playing plus
              potion positive power princess product  purple
              quotient
              raise raised real repeat right ring robe root round rowboat
              second seconds set shadow shield shift ship silver
              skin sloop staff south sum sword
              square starts stops
              than the then times to torch tranh truck tunic
              ulluk under unless up upon
              value vizier
              wait walks wand we weigh west when white with wrench
              zero))
  :test 'equalp
  :documentation "Words recognized specially by the stage directions parser")

(setf *stage-direction-parser* nil)

(yacc:define-parser *stage-direction-parser*
    (:start-symbol directions)
  (:terminals #.(concatenate 'list +stage-direction-words+
                             '(number quoted actor variable)))
  (:precedence ((:right -) (:left + -) (:left * /)))
  (directions statement
              (directions statement))

  (statement call-expr
             (when conditional |,| clauses |.|
                   #'stage/when)
             (if conditional |,| clauses |.|
                 #'stage/if-as-when)
             (if conditional |,| clauses |;| otherwise |,| clauses |.|
                 #'stage/if-otherwise)
             (unless conditional |,| clauses |.|
                     #'stage/unless)
             (clauses |.| (lambda (clauses stop)
                            (declare (ignore stop))
                            clauses))
             (repeat numeric times |,| clauses
                     #'stage/repeat))
  (clauses clause
           sem-clauses
           do/done-block)
  (sem-clauses (clause |;| clause
                       #'stage/semicolon-clauses)
               (sem-clauses |;| clause
                            #'stage/semicolon-clauses))
  (clause beat-clause
          enter-clause
          exit-clause
          walk-clause
          facing-clause
          audio-clause
          appearance-clause
          assignment-clause
          go-to-clause
          truck/dolly
          ship-clause
          (cut to include actor/location)
          (cut to center on actor/location)
          (at numeric / second |,| truck/dolly)
          jump-to-other-file-clause)
  
  (to/for to for)
  
  (ship-name quoted)
  
  (east/west (east (lambda (_) 'east)) (west (lambda (_) 'west)))
  
  (actor-list (actor |,| actor)
              (actor-list |,| actor))
  
  (actors actor
          (actor and actor)
          (actor |,| and actor)
          (actor-list |,| and actor))

  (call-expr (call quoted
                   #'stage/call)
             (call quoted with registers
                   #'stage/call-with-regs))
  (registers register
             (registers |,| register
                        #'stage/regs-list))
  (register (reg-name = numeric
                      #'stage/move-into-reg))
  (reg-name a x y)

  (do/done-block (do |:| statements done |.|
                   #'stage/do-block))
  (statements statement
              (statements statement
                          #'stage/statements-list))

  (truck/dolly (truck left/right
                      #'stage/truck-left/right)
               (dolly up/down
                      #'stage/dolly-up/down)
               (truck numeric left/right
                      #'stage/truck-numeric-left/right)
               (dolly numeric up/down
                      #'stage/dolly-numeric-up/down)
               (dolly/truck direction to include actor/location
                            #'stage/camera-include)
               (dolly/truck direction to center on actor/location
                            #'stage/camera-center)
               (frame actor/location and actor/location
                      #'stage/camera-frame)
               (close on actor/location
                      #'stage/camera-close))
  (actor/location someone location)
  (beat-clause (beat #'stage/one-beat)
               (numeric beat
                        #'stage/numeric-beats)
               (numeric beats
                        #'stage/numeric-beats)
               (wait for numeric second
                     #'stage/wait-secs)
               (wait for numeric seconds
                     #'stage/wait-secs)
               (wait for numeric beat
                     #'stage/wait-beats)
               (wait for numeric beats
                     #'stage/wait-beats))
  (go-to-clause (go quoted
                    #'stage/go)
                (go to quoted
                    #'stage/go-to))
  (audio-clause (we hear quoted
                    #'stage/we-hear-sound)
                (quoted starts playing
                        #'stage/song-starts-playing)
                (quoted stops playing
                        #'stage/song-stops-playing)
                (the music stops
                     #'stage/music-stops)
                (silence
                 #'first))
  (assignment-clause (set variable to numeric
                          #'stage/set-var-to-val)
                     (variable ← numeric
                               #'stage/var←val)
                     (variable < - numeric
                               #'stage/var<-val)
                     (set actor-state to numeric
                          #'stage/set-state-to-val)
                     (actor-state ← numeric
                                  #'stage/state←val)
                     (actor-state < - numeric
                                  #'stage/state<-val)
                     (set player-state to numeric
                          #'stage/set-state-to-val)
                     (player-state ← numeric
                                   #'stage/state←val)
                     (player-state < - numeric
                                   #'stage/state<-val)
                     (set quoted to true/false
                          #'stage/set-flag-to-bool)
                     (the ship can go to quoted
                          #'stage/ship-can-go)
                     (the ship can not go to quoted
                          #'stage/ship-can-not-go)
                     inc/dec-expr)
  (true/false (true (constantly t))
              (yes (constantly t))
              (on (constantly t))
              (false (constantly nil))
              (no (constantly nil))
              (off (constantly nil)))
  (inc/dec-expr (the player gains/loses numeric crowns
                     #'stage/player-gains/loses-crowns)
                (the player gains/loses numeric arrows
                     #'stage/player-gains/loses-arrows)
                (the player gains/loses numeric karma
                     #'stage/player-gains/loses-karma)
                (the player gains/loses the item
                     #'stage/player-gains/loses-item)
                (the player gains armor numeric
                     #'stage/player-gains-armor)
                (the palyer gains ring numeric
                     #'stage/player-gains-ring)
                (the player gains/loses quoted
                     #'stage/player-gains/loses-quest-item)
                (someone gives the item to someone
                         #'stage/gives-item)
                (inc/dec player-state
                         #'stage/inc/dec-state-1)
                (inc/dec player-state by numeric
                         #'stage/inc/dec-state-by-n)
                (inc/dec player-state by numeric |,| limit numeric
                         #'stage/inc/dec-state-by-n/limit)
                (inc/dec player-state |,| limit numeric
                         #'stage/inc/dec-state-1/limit))
  (item knife buckler hammer amulet potion sword shield bow
        torch chalice staff wand grappling-hook glass wrench boots
        mask glove)
  (gains/loses (gains (constantly '+))
               (loses (constantly '-)))
  (inc/dec (increment (constantly 'inc))
           (increase (constantly 'inc))
           (decrement (constantly 'dec))
           (decrease (constantly 'dec)))
  (conditional simple-conditional
               compound-conditional)
  (compound-conditional (all of conditionals |,| and simple-conditional
                             #'stage/all-of)
                        (any of conditionals |,| or simple-conditional
                             #'stage/any-of)
                        (none of conditionals |,| nor simple-conditional
                              #'stage/none-of)
                        (either simple-conditional |,| or simple-conditional
                                #'stage/either-or)
                        (neither simple-conditional |,| nor simple-conditional
                                 #'stage/neither-nor)
                        (both simple-conditional |,| and simple-conditional
                              #'stage/both-and))
  (simple-conditional (numeric is less than numeric
                               #'stage/is-less-than)
                      (numeric is greater than numeric
                               #'stage/is-greater-than)
                      (numeric is less than or equal to numeric
                               #'stage/is-less-than-or-equal-to)
                      (numeric is greater than or equal to numeric
                               #'stage/is-greater-than-or-equal-to)
                      (numeric is equal to numeric
                               #'stage/is-equal-to)
                      (numeric is number
                               #'stage/num-is-num)
                      (numeric is zero
                               #'stage/is-zero)
                      (numeric is not less than numeric
                               #'stage/is-not-less-than)
                      (numeric is not greater than numeric
                               #'stage/is-not-greater-than)
                      (numeric is not equal to numeric
                               #'stage/is-not-equal-to)
                      (numeric is not zero
                               #'stage/is-not-zero)
                      (numeric is positive or zero
                               #'stage/is-pos-or-zero)
                      (numeric is zero or positive
                               #'stage/is-zero-or-pos)
                      (numeric is positive
                               #'stage/is-pos)
                      (numeric is negative
                               #'stage/is-neg)
                      (numeric < numeric
                               #'stage/<)
                      (numeric > numeric
                               #'stage/>)
                      (numeric < = numeric
                               #'stage/<=)
                      (numeric ≤ numeric
                               #'stage/≤)
                      (numeric > = numeric
                               #'stage/>=)
                      (numeric ≥ numeric
                               #'stage/≥)
                      (numeric = numeric
                               #'stage/=)
                      (numeric / = numeric
                               #'stage//=)
                      (numeric ≠ numeric
                               #'stage/≠)
                      (numeric is between numeric and numeric
                               #'stage/is-between)
                      (numeric is from numeric to numeric
                               #'stage/is-from-to)
                      (|(| compound-conditional |)|
                           #'stage/parens))
  (conditionals (simple-conditional |,| conditionals
                                    #'stage/conditionals-list))
  (enter-clause (skyline-tool::enter someone at location
                                     #'stage/enter))
  (ship-clause
   (the ship-name appears in the east/west headed for actor/location
        #'stage/empty-boat)
   (the ship-name appears in the east/west with actors aboard headed to/for actor/location
        #'stage/full-boat)
   (the ship-name gets under weigh to the east/west
        #'stage/sail-away)
   (actor embarks/boards the ship-name #'stage/embarks)
   (actor disembarks from the ship-name #'stage/disembarks))
  
  (embarks/boards boards (embarks upon) (embarks on))
  
  (exit-clause (someone exits #'stage/exit)
               (exit someone (lambda (e s) (stage/exit s e))))
  (jump-to-other-file-clause (continued in quoted in quoted #'stage/jump-to-file))
  (facing-clause (someone faces direction
                          #'stage/faces)
                 (someone faces to the direction
                          #'stage/faces-to))
  (walk-clause (someone walks relative-position
                        #'stage/walks-relative)
               (someone moves relative-position
                        #'stage/walks-relative))
  (relative-position step-distance
                     (to location
                         #'stage/relative-to)
                     (relative-position |,| then step-distance
                                        #'stage/relative-steps))
  (step-distance (numeric to the direction
                          #'stage/num-to-the-dir)
                 (numeric direction
                          #'stage/num-dir)
                 (direction numeric
                            #'stage/dir-num)
                 (direction by numeric
                            #'stage/dir-by-num)
                 (numeric up/down left/right
                          #'stage/num-ud-lr)
                 (up/down left/right by numeric
                          #'stage/ud-lr-by-num)
                 (numeric up/down and numeric left/right
                          #'stage/relative-ud-lr)
                 (numeric left/right and numeric up/down
                          #'stage/relative-lr-ud))
  (appearance-clause (actor* speaks with pitch numeric |,| speed numeric
                             #'stage/speech-params)
                     (actor* looks like special-actor
                             #'stage/special-appearance)
                     (actor* has npc-description
                             #'stage/looks-like))
  (actor* actor special-actor)
  (special-actor (the princess (constantly 'aisling))
                 (aisling (constantly 'aisling))

                 (the earl (constantly 'ulluk))
                 (ulluk (constantly 'ulluk))

                 (the elder (constantly 'tranh))
                 (tranh (constantly 'tranh))

                 (nefertem (constantly 'nefertem))

                 (the captain (constantly 'caspar))
                 (caspar (constantly 'caspar))

                 (the vizier (constantly 'vizier))

                 (the player (constantly 'player))
                 (player (constantly 'player))

                 (shadow player (constantly 'shadow-player)))
  (npc-description (npc-descriptor)
                   (npc-descriptor and npc-descriptor
                                   #'stage/npc-desc-2)
                   (npc-descriptor |,| npc-descriptor |,| and npc-descriptor
                                   #'stage/npc-desc-3)
                   (npc-descriptor |,| npc-descriptor |,| npc-descriptor |,|
                                   and npc-descriptor
                                   #'stage/npc-desc-4))
  (npc-descriptor (color hair
                         #'stage/color-hair)
                  (color skin
                         #'stage/color-skin)
                  (a color tunic
                     #'stage/color-tunic)
                  (a color robe
                     #'stage/color-robe)
                  (head number
                        #'stage/head-num))
  (color purple green peach silver blue brown black white gray)
  (location (quoted (lambda (place)
                      (list 'place place 0 'north 0 'east)))
            (numeric direction of/from quoted
                     #'stage/num-dir-of-place)
            (numeric left/right and numeric up/down of/from quoted
                     #'stage/num-lr-up-of-place)
            (numeric up/down and numeric left/right of/from quoted
                     #'stage/num-up-lr-of-place)
            (|(| numeric |,| numeric |)|
                 #'stage/raw-coords))
  (of/from of from)
  (direction left/right up/down)
  (left/right (left (constantly 'west))
              (right (constantly 'east))
              (east (constantly 'east))
              (west (constantly 'west)))
  (up/down (up (constantly 'north))
           (down (constantly 'south))
           (north (constantly 'north))
           (south (constantly 'south)))
  (numeric (variable (lambda (var)
                       (list 'var (subseq var 1))))
           number
           pi
           (e (constantly (exp 1)))
           actor-state
           player-state
           (- number)
           (|(| number + number i |)|
                #'stage/complex)
           (|(| numeric |)|
                #'stage/parens)
           (the sum of numeric plus* numeric
                #'stage/sum-of-n+n)
           (the difference of numeric minus* numeric
                #'stage/diff-of-n---n)
           (the quotient of numeric division* numeric
                #'stage/quot-of-n÷n)
           (the product of numeric times* numeric
                #'stage/prod-of-n×n)
           (the result of numeric shifted by numeric
                #'stage/num-ash-num)
           (the result of numeric raised to the numeric power
                #'stage/num-expt-num)
           (the logarithm base numeric of numeric
                #'stage/log-base-n-of-n)
           (the base numeric logarithm of numeric
                #'stage/base-n-log-of-n)
           (the natural logarithm of n
                #'stage/nat-log-of-n)
           (the square root of numeric
                #'stage/sqrt-of-n)
           (the ceiling value of numeric
                #'stage/ceiling-n)
           (the floor value of numeric
                #'stage/floor-n)
           (the round value of numeric
                #'stage/round-n)
           (the real part of numeric
                #'stage/realpart-n)
           (the imaginary part of numeric
                #'stage/imagpart-n)
           (the absolute value of numeric
                #'stage/abs-n)
           (boolean numeric and numeric
                    #'stage/bool-and)
           (boolean numeric or numeric
                    #'stage/bool-or)
           (boolean exclusive numeric or numeric
                    #'stage/bool-xor)
           (boolean inclusive numeric or numeric
                    #'stage/logior))
  (actor-state
   (the x position of actor)
   (the y position of actor)
   (the hit points of actor)
   (the max hit points of actor))
  (player-state
   (the x position of the player)
   (the y position of the player)
   (the hit points of the player)
   (the max hit points of the player)
   (the karma of the player)
   (the magic points of the player)
   (the max magic points of te player)
   (the arrows of the player)
   (the crowns of the player))
  (plus* plus +)
  (minus* minus - less)
  (division* (divided by) / ÷)
  (times* times ✕ ×)
  (someone actor special-actor))

(defvar *fountain-state* nil)

(defun regex-match (regex string)
  "Returns a matching string to REGEX from STRING"
  (cl-ppcre:do-matches-as-strings (x regex string)
    (when x (return-from regex-match x)))
  nil)

(defun parse-stage-directions (string)
  "Parse stage directions in STRING into assembly code"
  (with-input-from-string (stream string)
    (yacc:parse-with-lexer
     (lambda () (stage-direction-lexer stream))
     *stage-direction-parser*)))

(defun fountain-lexer/parse-line (line)
  "Parse a LINE from a Fountain script"
  #+() (format *trace-output* "~%~5tPARSE: (~s) ~s" (car *fountain-state*) line)
  (block nil
    (when (null line)
      (return (list 'end nil)))
    (ecase (car *fountain-state*)
      (:continued-note
       (labels ((append-line ()
                  (concatenate 'string
                               (cdr *fountain-state*)
                               #.(format nil "~%~10t;; ")
                               line)))
         (cond
           ((search "]]" line)
            (return (prog1
                        (list 'comment (append-line))
                      (setf *fountain-state* nil))))
           (t (setf (cdr *fountain-state*) (append-line))
              (return nil)))))
      (:continued-metadata
       (cond
         ((emptyp (string-trim #(#\Space #\Tab) line))
          (return (list 'metadata
                        (prog1 (cdr *fountain-state*)
                          (setf *fountain-state* nil)))))
         ((char= #\Space (char line 0))
          (setf (cdr *fountain-state*)
                (concatenate 'string (cdr *fountain-state*)
                             (if (or (char= #\Space (last-elt (cdr *fountain-state*)))
                                     (char= #\: (last-elt (cdr *fountain-state*))))
                                 " "
                                 "; ")
                             (string-trim #(#\Space #\Tab) line)))
          (return nil))
         (t (setf *fountain-state* nil)
            (fountain-lexer/parse-line line))))
      (:stage
       (cond
         ((emptyp (string-trim #(#\Space #\Tab) line))
          (return (prog1
                      (let ((*last-values* nil))
                        (list 'stage
                              (parse-stage-directions (cdr *fountain-state*))))
                    (setf *fountain-state* nil))))
         (t (setf (cdr *fountain-state*)
                  (concatenate 'string (cdr *fountain-state*) " " line))
            (return nil))))
      (:branch
       (cond
         ((or (emptyp (string-trim #(#\Space #\Tab) line))
              (and (char= #\( (char line 0))
                   (char= #\) (last-elt line))))
          (error "Branch to ~s missing statement" (cdr *fountain-state*)))
         (t (return (prog1
                        (list 'branch (cons (cdr *fountain-state*) line))
                      (setf *fountain-state* (cons :speech "")))))))
      (:speech
       (cond
         ((or (null line) (emptyp (string-trim #(#\Space #\Tab) line)))
          (return (list 'speech (prog1 (cdr *fountain-state*)
                                  (setf *fountain-state* nil)))))
         ((and (char= #\( (char line 0))
               (char= #\) (last-elt line)))
          (return (prog1
                      (if (string-equal "(to " (subseq line 0 4))
                          (let ((label (subseq line 4 (1- (length line)))))
                            (setf *fountain-state* (cons :branch label))
                            (return nil))
                          (error "Emotes are not supported, saw ~s" line)))))
         (t (setf (cdr *fountain-state*)
                  (if (emptyp (cdr *fountain-state*))
                      line
                      (concatenate 'string (cdr *fountain-state*)
                                   (string #\Space) line)))
            (return nil))))
      ((nil)
       (cond
         ((null line)
          (return (list 'end nil)))
         ((emptyp (string-trim #(#\Space #\Tab) line))
          nil)
         ((char= #\# (char line 0))
          (return (list 'label (string-trim #(#\Space #\Tab)
                                            (string-upcase (subseq line 1))))))
         ((char= #\= (char line 0))
          (return (list 'comment (concatenate 'string "Synopsis: "
                                              (subseq line 1)))))
         ((and (position #\: line)
               (destructuring-bind (key value) (split-sequence #\: line :count 2)
                 (declare (ignore value))
                 (member key (list "title" "credit" "author" "format"
                                   "source" "date" "contact")
                         :test #'string-equal)))
          (destructuring-bind (key value) (split-sequence #\: line :count 2)
            (declare (ignore key))
            (if (emptyp value)
                (return (prog1
                            nil
                          (setf *fountain-state* (cons :continued-metadata line))))
                (return (list 'metadata line)))))
         ((char= #\> (char line 0))
          (cond ((and (< 4 (length line))
                      (string-equal ">TO " (subseq line 0 4)))
                 (return (list 'go (string-trim #(#\Space #\Tab) (subseq line 4)))))
                ((and (< 9 (length line))
                      (string-equal ">FADE TO " (subseq line 0 9)))
                 (return (list 'fade-to (remove #\. (subseq line 9)))))
                ((or (member line '(">FINIS." ">THE END." ">END.") :test #'string-equal))
                 (return (list 'end (remove-if-not #'alpha-char-p line))))
                (t (warn "Ignoring “~a” …" line))))
         ((search "[[" line)
          (if (search "]]" line)
              (return (list 'comment line))
              (progn (setf *fountain-state* (cons :continued-note line))
                     (return nil))))
         ((member (subseq line 0 4) '("INT " "EXT ") :test #'string-equal)
          (let ((scene-name (mapcar (lambda (part)
                                      (cl-change-case:pascal-case
                                       (string-trim #(#\Space #\Tab) part)))
                                    (split-sequence
                                     #\-
                                     (subseq line 4 (position #\# line))
                                     :count 2))))
            (with-simple-restart (check-for-scene-again "Check again for ~{~a~^/~}.tmx" scene-name)
              (unless (and (= 2 (length scene-name))
                           (probe-file (make-pathname
                                        :directory (list :relative "Source" "Maps" (first scene-name))
                                        :name (second scene-name)
                                        :type "tmx")))
                (error "Scene map file not found: Source/Maps/~{~a~^/~}.tmx ?"
                       scene-name)))
            (return (list 'scene scene-name))))
         ((every (lambda (char) (char= char (char-upcase char))) line)
          (destructuring-bind (kind name)
              (if (find #\( line)
                  (cl-ppcre:register-groups-bind (name parens)
                      ("^(.*?)[:space:]*\\((.+)\\)$" (string-trim #(#\Space #\Tab) line))
                    (assert (member parens '("OC" "O/C" "VO" "V/O")
                                    :test #'string-equal)
                            (parens)
                            "Parentheses after speaker's name ~
are only allowed to be used for off-camera (O/C) labels, but got “~a” in “~a”"
                            parens line)
                    (list 'speaker-oc (string-trim #(#\Space #\Tab) name)))
                  (list 'speaker line))
            (setf *fountain-state* (cons :speech ""))
            (return (list kind name))))
         (t (setf *fountain-state* (cons :stage line))
            (return nil))))
      (t (error "Unknown Fountain state ~s" *fountain-state*)))))

(defun format-terminals-for-error (expected-terminals)
  (mapcar (lambda (x)
            (if (null x) "nothing at all" (princ-to-string x)))
          (sort (copy-list expected-terminals) #'string<)))

(defmethod print-object ((condition yacc:yacc-parse-error) stream)
  "Print a YACC:YACC-PARSE-ERROR nicely to STREAM"
  (let ((terminal (yacc:yacc-parse-error-terminal condition))
        (value (yacc:yacc-parse-error-value condition))
        (expected-terminals (yacc:yacc-parse-error-expected-terminals condition)))
    (when *line-number*
      (format stream "When reading line ~:d:~%~4t" *line-number*))
    (when *last-values*
      (format stream "After reading “~{~a~^ ~}”:~%~4t" *last-values*))
    (if terminal
        (format stream "Read a token of type “~a” (value “~a”) unexpectedly."
                terminal value)
        (format stream "Read nil (end of stage directions?) unexpectedly."))
    (let ((len (length (remove-if #'null expected-terminals))))
      (cond
        ((zerop len)
         (format stream " Was not expecting anything further."))
        ((= 1 len)
         (format stream " Only expected “~a.”"
                 (let ((s (princ-to-string (first expected-terminals))))
                   (if (= 1 (length s))
                       (format nil "~@c (~a)"
                               (first-elt s)
                               (cl-change-case:sentence-case
                                (char-name (first-elt s))))
                       s))))
        ((= 2 len)
         (format stream " Expected either ~{“~a” or “~a.”~}"
                 (format-terminals-for-error expected-terminals)))
        ((< len 10)
         (format stream " Expected one of these:~{~%~10t • ~a~}"
                 (format-terminals-for-error expected-terminals)))
        (t
         (format stream " Expected one of these:~% ~{“~a~^,”~:_ ~}”"
                 (format-terminals-for-error expected-terminals)))))))

(defvar *line-number* nil)
(defvar *current-pathname* nil)
(defvar *last-values* nil)

(defun fountain-lexer (stream)
  "A lexer for Fountain script source in STREAM"
  (loop for line = (read-line stream nil nil)
        for values = (fountain-lexer/parse-line line)
        until values
        do (incf *line-number*)
        finally (return (values-list values))))

(defun presence (sequence)
  "Returns NIL if SEQUENCE is `EMPTYP', else returns SEQUENCE"
  (if (emptyp sequence) nil sequence))

(defun actor-name-char-p (char)
  "Returns generally true if it's possible for CHAR to be part of an actor name"
  (or (alpha-char-p char) (char= #\- char)))

(defun numeric-char-p (char)
  "Returns generally true if it's possible for CHAR to be part of a number. [0-9./]"
  (or (digit-char-p char) (char= #\. char) (char= #\/ char)))

(defun stage-direction-lexer (stream)
  "A lexer for stage directions in STREAM"
  (labels ((rewind-stream (&optional (n 1))
             (file-position stream (- (file-position stream) n)))
           (token-values (string)
             (cond
               ((emptyp string) (multiple-value-list (stage-direction-lexer stream)))
               ((member string +stage-direction-words+ :test #'string-equal)
                (list (intern (string-upcase string) #.*package*) string))
               ((and (every #'digit-char-p (subseq string 0 (1- (length string))))
                     (member (last-elt string) '(#\. #\/) :test #'char=))
                (rewind-stream 2)
                (list 'number (parse-number (subseq string 0 (1- (length string))))))
               ((every #'numeric-char-p string)
                (list 'number (parse-number string)))
               ((every #'actor-name-char-p string)
                (list 'actor string))
               ((and (char= #\$ (char string 0))
                     (every (lambda (ch) (digit-char-p ch 16)) string)
                     (= 5 (length string)))
                (list 'memory (parse-number (subseq string 1) :radix 16)))
               ((and (char= #\$ (char string 0))
                     (every #'actor-name-char-p (subseq string 1)))
                (list 'variable string))
               ((char= #\" (char string 0))
                (list 'quoted (cl-change-case:pascal-case (subseq string 1))))
               (t (list 'string string)))))
    (let ((parsed (loop with word = ""
                        for char = (read-char stream nil nil)
                        unless char
                          return (unless (emptyp word)
                                   (token-values word))
                        do (cond
                             ((member char (list #\Space #\Tab #\Newline))
                              (return (prog1 (token-values word)
                                        (setf word ""))))
                             ((and (char= char #\$)
                                   (emptyp word))
                              (setf word "$"))
                             ((char= char #\$)
                              (error "$ must introduce a variable name"))
                             ((and (char= char #\")
                                   (emptyp word))
                              (return (token-values
                                       (apply #'concatenate 'string
                                              #(#\quotation_mark)
                                              (loop for qq = (read-char stream nil nil)
                                                    until (or (char= qq #\") (null qq))
                                                    collecting (string qq))))))
                             ((and (actor-name-char-p char)
                                   (or (every #'actor-name-char-p word)
                                       (and (char= #\$ (char word 0))
                                            (every #'actor-name-char-p (subseq word 1)))))
                              (setf word (concatenate 'string word (string char))))
                             ((and (numeric-char-p char)
                                   (every #'numeric-char-p word))
                              (if (digit-char-p char)
                                  (setf word (concatenate 'string word (string char)))
                                  (if (or (find #\. word) (find #\/ word))
                                      (progn (rewind-stream 1)
                                             (return (token-values word)))
                                      (setf word (concatenate 'string word (string char))))))
                             ((emptyp word)
                              (return (list (intern (string char)) (string char))))
                             (t (rewind-stream)
                                (return (token-values word)))))))
      (when parsed
        (destructuring-bind (token value) parsed
          (when (or token value)
            (appendf *last-values*
                     (list
                      (if (string-equal (princ-to-string value)
                                        (princ-to-string token))
                          (princ-to-string token)
                          (format nil "~a (~a)" value token))))
            (when (< 10 (length *last-values*))
              (setf *last-values* (subseq *last-values* 1)))
            (return-from stage-direction-lexer (values token value))))))))

(defun make-fountain-lexer (stream)
  "Creates a `FOUNTAIN-LEXER' bound to STREAM"
  (lambda () (fountain-lexer stream)))

(defvar *dialogue* nil)

(defun prepare-dialogue (string)
  "Prepare STRING for encoding into Minifont for the game console"
  (let ((prepared
          (string-trim #(#\Space #\Tab)
                       (cl-ppcre:regex-replace-all
                        "[ \\t\\n]+"
                        (cl-ppcre:regex-replace-all
                         "(…|\\.\\.\\.+)"
                         (cl-ppcre:regex-replace-all
                          "(\\b[A-Za-z0-9-']+\\b) *\\[.*?\\] *"
                          (cl-ppcre:regex-replace-all
                           "\\'"
                           string
                           "’")
                          " \\1 ")
                         "….")
                        " "))))
    (let ((no~ (remove #\~ prepared)))
      (assert (string-equal no~
                            (ignore-errors (minifont->unicode
                                            (unicode->minifont no~))))
              (prepared)
              "This text contains character(s) which cannot be displayed on the game console:
“~a”

“~a”"
              prepared
              (string-downcase (ignore-errors (minifont->unicode (unicode->minifont no~))))))
    prepared))

(defvar *atarivox-dictionary* nil
  "A cache for the AtariVox (SpeakJet) dictionary from Source/Tables/SpeakJet.dic")

(defun ensure-atarivox-dictionary ()
  "Ensure that *ATARIVOX-DICTIONARY* has been loaded

May call `LOAD-ATARIVOX-DICTIONARY' if not already cached"
  (or *atarivox-dictionary*
      (setf *atarivox-dictionary*
            (load-atarivox-dictionary))))

(define-constant +all-phonemes+
    '(
      "Pause0" "Pause1" "Pause2" "Pause3" "Pause4" "Pause5" "Pause6"
      "Fast" "Slow" "Stress" "Relax" "Wait" "Soft" "Volume" "Speed" "Pitch" "Bend" "PortCtr"
      "Port" "Repeat" "CallPhrase" "GotoPhrase" "Delay" "Reset"
      "IY" "IH" "EY" "EH" "AY" "AX" "UX" "OH" "AW" "OW" "UH" "UW"
      "MM" "NE" "NO" "NGE" "NGO" "LE" "LO" "WW"
      "RR" "IYRR" "EYRR" "AXRR" "AWRR" "OWRR"
      "EYIY" "OHIY" "OWIY" "OHIH" "IYEH" "EHLE" "IYUW" "AXUW" "IHWW" "AYWW" "OWWW"
      "JH" "VV" "ZZ" "ZH" "DH" "BE" "BO" "EB" "OB"
      "DE" "DO" "ED" "OD" "GE" "GO" "EG" "OG"
      "CH" "HE" "HO" "WH" "FF" "SE" "SO" "SH" "TH"
      "TT" "TU" "TS" "KE" "KO" "EK" "OK" "PE" "PO"
      "R0" "R1" "R2" "R3" "R4" "R5" "R6" "R7" "R8" "R9"
      "A0" "A1" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
      "B0" "B1" "B2" "B3" "B4" "B5" "B6" "B7" "B8" "B9"
      "C0" "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9"
      "DTMF_0" "DTMF_1" "DTMF_2" "DTMF_3" "DTMF_4" "DTMF_5"
      "DTMF_6" "DTMF_7" "DTMF_8" "DTMF_9" "DTMF_STAR" "DTMF_HASH"
      "M0" "M1" "M2" "EndOfPhrase"
      "PlayerNamePlace"
      "PronounThey" "PronounThem" "PronounTheir" "PronounTheirs"
      "PronounSibling" "PronounPerson" "PronounAre" "PronounWere" "PronounHave"
      "ButtonILabel" "ButtonIILabel" "ButtonIIILabel")
  :test #'equalp)

(defun phonemes-from-string (string)
  "Convert the AtariVox (SpeakJet) dictionary escape code phonemes in STRING into tokens"
  (flatten
   (loop for phoneme in (split-sequence #\Space string
                                        :remove-empty-subseqs t)
         do (restart-case
                (unless (char= #\\ (char phoneme 0))
                  (error "Expected SpeakJet phoneme code, beginning with \\, but got ~s" phoneme))
              (continue () :report "Continue with a gunshot sound"
                "M1"))
         do (restart-case
                (unless (or (numberp (ignore-errors (parse-number (subseq phoneme 1))))
                            (member (subseq phoneme 1) +all-phonemes+ :test #'string=))
                  (error "Expected SpeakJet phoneme code, but got ~s" phoneme))
              (continue () :report "Continue with a gunshot sound"
                "M1"))
         collect (concatenate 'string "SpeakJet." (subseq phoneme 1)))))

(defun load-atarivox-dictionary ()
  "Load the AtariVox (SpeakJet) dictionary from Source/Tables/SpeakJet.dic"
  (with-input-from-file (speakjet.dic #p"Source/Tables/SpeakJet.dic")
    (assert (equalp "[words]" (read-line speakjet.dic nil nil)) ()
            "SpeakJet.dic must begin with [words] magic cookie")
    (format *trace-output* "~&Reading SpeakJet.dic file … ")
    (loop with dict = (make-hash-table :test 'equalp)
          for line = (read-line speakjet.dic nil nil)
          while line
          do (unless (or (emptyp line) (char= #\# (char line 0)))
               (destructuring-bind (word phonemes$)
                   (mapcar (lambda (bit) (string-trim #(#\Space #\Tab) bit))
                           (split-sequence #\= line))
                 (setf (gethash word dict)
                       (phonemes-from-string phonemes$))))
          finally (progn
                    (format *trace-output* " Done, ~:d word~:p"
                            (hash-table-count dict))
                    (return dict)))))

(defun reload-atarivox-dictionary ()
  "Reload the AtariVox (SpeakJet) dictionary into *ATARIVOX-DICTIONARY*"
  (setf *atarivox-dictionary* (load-atarivox-dictionary))
  (length (hash-table-keys *atarivox-dictionary*)))

(defun speakjet-pause+ (x y)
  "Sum together the SpeakJet pauses X and Y into one longer pause"
  (format nil "SpeakJet.Pause~d"
          (+ (parse-integer x :start 14)
             (parse-integer y :start 14))))

(defun log-missing-word-for-speakjet (word)
  (with-output-to-file (missing-words #p"Object/SpeakJet.missing.words"
                                      :if-exists :append
                                      :if-does-not-exist :create)
    (fresh-line missing-words)
    (princ word missing-words)
    (fresh-line missing-words)))

(defun fixup-exclamations (seq)
  (loop
     (if-let (bang (position-if (lambda (n) (member n '(:bang :query))) seq))
       (progn
         (assert (plusp bang) ()
                 "Exclamation mark cannot begin a sentence")
         (setf seq
               (let* ((alteration (elt seq bang))
                      (phrase-start
                        (or (let ((n (position-if
                                      (lambda (tok)
                                        (and (stringp tok)
                                             (starts-with-subseq "SpeakJet.Pause" tok)))
                                      seq
                                      :end bang :from-end t)))
                              (when n (1+ n)))
                            0))
                      (before (subseq seq 0 phrase-start))
                      (phrase (subseq seq phrase-start bang))
                      (after (when (< bang (length seq))
                               (subseq seq (1+ bang))))
                      (phrase-length (length phrase)))
                 (assert (plusp phrase-length) ()
                         "Exclamation mark cannot modify a zero-phoneme-long phrase")
                 (ecase alteration
                   (:bang
                    (warn "handling of “!” is poor")
                    (reduce (curry #'concatenate 'list)
                            (list
                             before
                             (list "SpeakJet.Bend" "$04")
                             (mapcan (lambda (phoneme)
                                       (list "SpeakJet.Stress" phoneme))
                                     phrase)
                             (list "SpeakJet.Bend" "$05")
                             after)))
                   (:query
                    (warn "handling of “?” is poor")
                    (reduce (curry #'concatenate 'list)
                            (list
                             before
                             (case (length phrase)
                               (1 (list "SpeakJet.Bend" "$08" (car phrase)))
                               (2 (list "SpeakJet.Bend" "$06" (first phrase)
                                        "SpeakJet.Bend" "$08" (second phrase)))
                               (3 (list "SpeakJet.Bend" "$06" (first phrase)
                                        "SpeakJet.Bend" "$08" (second phrase)
                                        "SpeakJet.Bend" "$0a" (third phrase)))
                               (4 (list "SpeakJet.Bend" "$06" (first phrase)
                                        "SpeakJet.Bend" "$08" (second phrase)
                                        "SpeakJet.Bend" "$0a" (third phrase)
                                        "SpeakJet.Bend" "$08" (fourth phrase)))
                               (otherwise
                                (cons (subseq phrase 0 (- (length phrase) 5))
                                      (list "SpeakJet.Bend" "$06" (elt phrase (- (length phrase) 5))
                                            "SpeakJet.Bend" "$08" (elt phrase (- (length phrase) 4))
                                            "SpeakJet.Bend" "$0a" (elt phrase (- (length phrase) 3))
                                            "SpeakJet.Bend" "$0c" (elt phrase (- (length phrase) 2))
                                            "SpeakJet.Bend" "$09" (elt phrase (- (length phrase) 1))))))
                             (list "SpeakJet.Bend" "$05")
                             after)))))))
       (return-from fixup-exclamations seq))))

(defun convert-for-atarivox (string)
  "Convert STRING into a list of tokens for AtariVox (SpeakJet)"
  (ensure-atarivox-dictionary)
  (when (emptyp string) (return-from convert-for-atarivox nil))
  (let ((string (cl-ppcre:regex-replace-all
                 "\\b[A-Za-z0-9-']+\\b *\\[(.*?)\\]"
                 string
                 " \\1 "))
        (words nil))
    (cl-ppcre:do-scans (start end reg-starts reg-ends
                        "( +|\\~[A-Za-z]+|[A-Za-z\\']+|[^A-Za-z\\' ]+)" string)
      (let ((word (string-trim #(#\Space #\Tab #\Newline)
                               (subseq string start end) )))
        (push word words)))
    (reversef words)
    (let ((bytes (loop
                   with index = 0
                   while (< index (length string))
                   for word in words
                   append (cond ((emptyp word) (list "SpeakJet.Pause1"))
                                ((equal word "?!") (list :bang :query))
                                ((equal word "!") (list :bang))
                                ((equal word "?") (list :query))
                                ((member word '("-" "“" "”") :test #'string-equal)
                                 nil)
                                (t (or (prog1
                                           (gethash word *atarivox-dictionary*)
                                         (incf index (length word)))
                                       (progn
                                         (log-missing-word-for-speakjet word)
                                         (cerror "Continue with a gunshot sound"
                                                 "Word not in dictionary: “~a” not found"
                                                 word)
                                         (list "SpeakJet.M1"))))))))
      (flatten
       (remove-if #'null
                  (fixup-exclamations
                   (append (loop for i from 0 below (1- (length bytes))
                                 for a = (elt bytes i)
                                 for b = (elt bytes (1+ i))
                                 if (and (stringp a)
                                         (stringp b)
                                         (starts-with-subseq "SpeakJet.Pause" a)
                                         (starts-with-subseq "SpeakJet.Pause" b))
                                   collect (prog1 (speakjet-pause+ a b)
                                             (incf i))
                                 else
                                   collect a)
                           (list (unless (and (stringp (last-elt bytes))
                                              (starts-with-subseq "SpeakJet.Pause"
                                                                  (last-elt bytes)))
                                   (last-elt bytes))
                                 "SpeakJet.EndOfPhrase"))))))))

(defun compile-fountain-script (pathname)
  "Compile the Fountain script in PATHNAME into source code (to *STANDARD-OUTPUT*)"
  (with-input-from-file (fountain pathname)
    (format t ";;; Compiled from ~a" pathname)
    (let ((*current-pathname* pathname))
      (compile-fountain-stream fountain))))

(defgeneric npc-interpret-field (value field &key name kind)
  (:method (value (field t) &key name kind)
    (declare (ignore name kind))
    (warn "Using default NPC-INTERPRET-FIELD for ~s" field)
    (etypecase value
      (string (parse-number value))
      (number value)
      (null nil))))

(defun npc-interpret-color (moniker)
  (cond
    ((emptyp moniker)
     (cerror "Use WHITE for now" "Missing a color value")
     'white)
    ((member moniker '("peach" "purple" "brown"
                       "green" "white" "gray" "blue")
             :test #'string-equal)
     (intern (format nil "PaletteColor~:(~a~)" moniker)))
    (t
     (cerror "Use WHITE for now" "Unrecognized color value “~a”" moniker)
     'white)))

(defmethod npc-interpret-field (moniker (field (eql :hair-color)) &key name kind)
  (declare (ignore name kind))
  (when moniker (npc-interpret-color moniker)))

(defmethod npc-interpret-field (moniker (field (eql :skin-color)) &key name kind)
  (declare (ignore name kind))
  (when moniker (npc-interpret-color moniker)))

(defmethod npc-interpret-field (moniker (field (eql :clothes-color)) &key name kind)
  (declare (ignore name kind))
  (when moniker (npc-interpret-color moniker)))

(defmethod npc-interpret-field (head (field (eql :head)) &key kind name)
  (when (or (null kind) (eql kind 'human))
    (if (emptyp head)
        (progn (cerror "Use head 0"
                       "Head number must be 0-9~@[ for actor ~:(~a~)~]" name)
               0)
        (parse-integer head))))

(defmethod npc-interpret-field (body (field (eql :body)) &key kind name)
  (cond
    ((not (eql kind 'human))
     0)
    ((emptyp body)
     (cerror "Use tunic body"
             "Body type should be “tunic” or “robe” but got “~a”~@[ for actor ~:(~a~)~]"
             body name)
     0)
    ((string-equal "tunic" body) 0)
    ((string-equal "robe" body) 1)
    (t (cerror "Use tunic body"
               "Body type should be “tunic” or “robe” but got “~a”~@[ for actor ~:(~a~)~]"
               body name)
       0)))

(defmethod npc-interpret-field (speech-color (field (eql :speech-color)) &key name kind)
  (declare (ignore kind))
  (if (emptyp speech-color)
      (progn
        (cerror "Use gray"
                "Speech balloon color not specified~@[ for actor ~:(~a~)~]" name)
        "GRAY")
      speech-color))

(defmethod npc-interpret-field (voice-pitch (field (eql :voice-pitch)) &key name kind)
  (declare (ignore kind))
  (if (emptyp voice-pitch)
      (progn
        (cerror "Use generic male voice pitch 96"
                "Voice pitch not specified~@[ for actor ~:(~a~)~]" name)
        96)
      (parse-integer voice-pitch)))

(defmethod npc-interpret-field (voice-speed (field (eql :voice-speed)) &key name kind)
  (declare (ignore kind))
  (or (unless (emptyp voice-speed)
        (parse-integer voice-speed))
      (progn
        (cerror "Use default voice speed 100"
                "Voice speed not specified~@[ for actor ~:(~a~)~]" name)
        100)))

(defun load-actor (actor)
  (tagbody top
     (restart-case
         (destructuring-bind  (&key name decal body head hair skin clothing
                                    voice-pitch voice-speed speech-color
                                    comment hp ac character-id nicks
                               &allow-other-keys)
             (find-npc-stats actor)
           (declare (ignore comment))
           (unless character-id
             (error "Actor ~:(~a~) could not be found in NPC stats file" actor))
           #+ () (warn "Loading actor ~:(~a~)~%~8tStats: ~s" actor (find-npc-stats actor))
           (let* ((kind (cond ((emptyp decal)
                               (cerror "Continue, using HUMAN as the decal kind"
                                       "Decal kind not given for “~:(~a~)”/“~a”~%~s"
                                       actor name decal))
                              ((member decal '("human" "Vizier" "Nefertem"
                                               "Earl" "Captain" "Princess" "Elder" "phantom")
                                       :test #'string-equal)
                               (intern (string-upcase decal)))
                              (t (cerror "Use generic HUMAN decal"
                                         "Decal kind “~a” not recognized (for “~:(~a~)”/“~a” in NPC stats)"
                                         decal actor name))))
                  (record (append
                           (list :kind kind
                                 :hp (unless (emptyp hp) (parse-integer hp))
                                 :ac (unless (emptyp ac) (parse-integer ac))
                                 :pitch (npc-interpret-field voice-pitch :voice-pitch :name name)
                                 :speed (npc-interpret-field voice-speed :voice-speed :name name)
                                 :speech-color (npc-interpret-field speech-color
                                                                    :speech-color :name name)
                                 :character-id character-id
                                 :nicks nicks)
                           (when (eql kind 'human)
                             (list :hair-color (npc-interpret-field hair :hair-color :name name)
                                   :skin-color (npc-interpret-field skin :skin-color :name name)
                                   :clothes-color (npc-interpret-field clothing :clothes-color
                                                                       :name name)
                                   :head (npc-interpret-field head :head :kind kind :name name)
                                   :body (npc-interpret-field body :body :kind kind :name name))))))
             (if-let (i (position-if
                         (lambda (actor)
                           (and (consp actor)
                                (< 1 (length actor))
                                (getf (subseq actor 1) :character-id)
                                (= character-id (getf (subseq actor 1) :character-id))))
                         *actors*))
               (progn #+ () (warn "Updating actor record, from~%~8t~s~%…to…~%~8t~s"
                                  (elt *actors* i) record)
                      (setf (elt *actors* i) record))
               #+ () (warn "Actor record not in *ACTORS* so no update~%~8t~s" record))
             (return-from load-actor record))))))

(defun find-location (location)
  "Given an Entrance name LOCATION, return (LIST X Y)"
  (unless *current-scene*
    (error "No scene is started; cannot find a location in the void (Looking for ~s)"
           location))
  (destructuring-bind  (locale-id x y) (find-entrance-by-name
                                        (load-other-map *current-scene*)
                                        *current-scene* location)
    (declare (ignore locale-id))
    (list x y)))

(defvar *current-scene* nil)
(defvar *actors* nil)

(defgeneric compile-stage-direction (fun args)
  (:method ((fun t) (args t))
    (error "Unhandled stage direction function ~s" fun)))

(defmacro defstage (fun (&rest args) &body body)
  (let ((argv (gensym "ARGS-")))
    `(defmethod compile-stage-direction ((f (eql ',fun)) (,argv cons))
       (block nil
         (destructuring-bind (,@args) ,argv
           ,@body)))))

(defun interpret-place (place)
  (destructuring-bind (kind entrance-name &optional n1 d1 n2 d2) place
    (ecase kind
      (place (destructuring-bind (locale-id x y) (find-entrance-by-name
                                                  (locale-xml *current-scene*)
                                                  entrance-name
                                                  *current-scene*)
               (declare (ignore locale-id))
               (ecase d1
                 (north (decf y n1))
                 (south (incf y n1))
                 (east (incf x n1))
                 (west (decf x n1))
                 ((nil) nil))
               (ecase d2
                 (north (decf y n2))
                 (south (incf y n2))
                 (east (incf x n2))
                 (west (decf x n2))
                 ((nil) nil))
               (list :absolute x y)))
      (δ (destructuring-bind (δ n1 d1 &optional n2 d2) place
           (declare (ignore δ))
           (let ((δx 0) (δy 0))
             (ecase d1
               (north (decf δy n1))
               (south (incf δy n1))
               (east (incf δx n1))
               (west (decf δx n1))
               ((nil) nil))
             (ecase d2
               (north (decf δy n2))
               (south (incf δy n2))
               (east (incf δx n2))
               (west (decf δx n2))
               ((nil) nil))
             (list :relative δx δy)))))))

(defun color-index (name)
  (ecase name
    (purple 1)
    (green 2)
    (peach 3)
    (silver 5)
    (blue 6)
    (brown 7)
    (black 9)
    (white 10)
    (gray 11)
    (player-skin 13)
    (special 14)
    (player-armor 15)
    ((nil) nil)))

(defstage camera-center (where)
  (destructuring-bind (abs/rel x y) (interpret-place where)
    (assert (eql abs/rel :absolute))
    (format t "
~10t.mva DestX, #~d
~10t.mva DestY, #~d
~10tjsr Lib.CenterCamera
"
            x y)))

(defstage jump (script-id)
  (format t "
~10t.mvaw NextScript, $~4,'0x
~10tlda # <JLoadScript
~10tjmp Lib.LoadAsset
"
          script-id))

(defstage wait (beat/second duration)
  (let* ((secs (ecase beat/second
                 (beat (/ duration 2))
                 (second duration))))
    (format t "
~10tlda # ~7f * FramesPerSecond
~10tsta FrameCounter
~a:
~10tlda FrameCounter
~10tbne ~:*~a
"
            secs
            (script-auto-label))))

(defvar *dict-words* nil)

(defun read-entire-dictionary (&optional (dict-file #p"/usr/share/dict/words"))
  (or *dict-words*
      (let ((dict (make-hash-table :test 'equal)))
        (with-input-from-file (file dict-file)
          (loop for line = (read-line file nil nil)
                while line
                do (setf (gethash line dict) t)))
        (setf *dict-words* dict))))

(defun genlabel (prefix)
  (read-entire-dictionary)
  (apply #'format nil "~a_~a~^_~a~^_~a~^_~a"
         prefix
         (remove-if-not (lambda (n) (and (<= 3 (length n) 8)
                                         (every #'alpha-char-p n)))
                        (loop repeat 8 collecting (random-elt (hash-table-keys *dict-words*))))))

(defstage exit (who)
  (let ((not-found-character-label
          (genlabel "CharacterNotFound")))
    (destructuring-bind (name &key &allow-other-keys)
        (require-actor who)
      (format t "
~10tlda # CharacterID_~a
~10tjsr Lib.FindCharacter~32t; get object index in X

~10tbcs ~a

~10tjsr Lib.ExitCharacter

~:*~a:"
              (cl-change-case:pascal-case (string name))
              not-found-character-label))))

(defstage enter (who where)
  (let ((actor (find-or-load-actor who)))
    (push actor *actors*)
    (destructuring-bind (abs/rel x y) (interpret-place where)
      (assert (eql abs/rel :absolute))
      (destructuring-bind (name &key kind hp ac pitch speed
                                     hair-color skin-color clothes-color
                                     speech-color
                                     head body character-id
                           &allow-other-keys)
          (require-actor who)
        (declare (ignore kind hp ac pitch speed
                         hair-color skin-color clothes-color
                         speech-color head body character-id))
        (if (string-equal name 'player)
            (format t "
~10tlda # CharacterID_Player
~10tjsr Lib.FindCharacter

~10t.mva DestX, #~d
~10t.mva DestY, #~d
~10tjsr Lib.DoWalk
"
                    x y)
            (format t "
~10t.mvaw Proto, Character_~a
~10t.mva DestX, #~d
~10t.mva DestY, #~d
~10tjsr Lib.EnterCharacter
"
                    (cl-change-case:pascal-case (string name)) x y))))))

(defvar *boat-ids* nil)
(defvar *boat-classes* nil)

(defstage boat (ship-name east/west target actors)
  (with-simple-restart (reload-boats "Reload Boats.ods and retry")
    (load-boats)
    (let ((boat-id (gethash ship-name *boat-ids*))
          (boat-class (gethash ship-name *boat-classes*)))
      (format t "~%~10t;; Boat ~:(~a~) appears" ship-name)
      (format t "~%~10t.mvaw Proto, 0")
      (ecase east/west
        (east (format t "~%~10tlda MapWidth~%~10tlsr a~%~10tlsr a~%~10tlsr a~%~10tsta DestX"))
        (west (format t "~%~10t.mva DestX, #-25")))
      (destructuring-bind (kind x y) (interpret-place target)
        (assert (eql kind :absolute) (kind)
                "KIND of location for positioning a boat must be absolute, but got ~s" kind)
        (format t "~%~10t.mva DestY, #~d" y)
        (format t "~%~10tldx #~d~32t; Boat “~a”" boat-id ship-name)
        (format t "~%~10tldy # Boat~:(~a~)" boat-class)
        (format t "~%~10tjsr Lib.MakeBoat~%")
        ;; TODO put people on the boat
        (format t "~%~10t.mva DestX, #~@d" (ecase east/west (east -1) (west 1)))
        (format t "~%~10tjsr Lib.MoveBoat~%")))))

(defstage sail-away (ship-name east/west)
  (with-simple-restart (reload-boats "Reload Boats.ods and retry")
    (load-boats)
    (let ((boat-id (gethash ship-name *boat-ids*)))
      (format t "~%~10tldx #~d~32t; Boat “~a”" boat-id ship-name)))
  (format t "~%~10tjsr Lib.FindBoat~%")
  (format t "~%~10tbcc +~%~10t.DebugBreak \"dwmb\" ; \"Dude, where's my boat?\"~%+~%")
  (ecase east/west
    (east (format t "~%~10t.mva DestX, MapWidth~%~10tlsr a~%~10tlsr a~%~10tlsr a"))
    (west (format t "~%~10t.mva DestX, #-25")))
  (format t "~%~10tjsr Lib.MoveBoat~%"))

(defstage embarks (actor ship-name)
  (format t "~%~10t;; TODO actor ~a embarks upon ship ~a" actor ship-name))

(defstage disembarks (actor ship-name)
  (format t "~%~10t;; TODO actor ~a disembarks from ship ~a" actor ship-name))

(defstage walk (who where)
  (if (eql who 'player)
      (format t "~%~10t;; TODO: Move the player") ; TODO #151
      (destructuring-bind (name &key &allow-other-keys)
          (require-actor who)
        (destructuring-bind (abs/rel x y) (interpret-place where)
          (format t "
~10tlda # CharacterID_~a"
                  (cl-change-case:pascal-case (string name)))
          (format t "~%~10tjsr Lib.FindCharacter~%")
          (format t "~%~10t.mva DestX, #~d" x)
          (format t "~%~10t.mva DestY, #~d" y)
          (ecase abs/rel
            (:absolute (format t "~%~10tjsr Lib.DoWalk~%"))
            (:relative (format t "~%~10tjsr Lib.DoWalkRelative~%")))))))

(defstage look (who how)
  (loop for i from 0
        for npc in *npc-stats*
        when (string-equal (getf npc :name) who)
          do
             (loop for fact in how
                   do
                      (destructuring-bind (key value) fact
                        (ecase key
                          (skin (setf (getf npc :skin)
                                      (npc-interpret-field value :skin-color :name who)))
                          (hair (setf (getf npc :hair)
                                      (npc-interpret-field value :hair-color :name who)))
                          (tunic (setf (getf npc :clothing)
                                       (npc-interpret-field value :clothes-color :name who)
                                       (getf npc :body) 0))
                          (robe (setf (getf npc :clothing)
                                      (npc-interpret-field value :clothes-color :name who)
                                      (getf npc :body) 1))
                          (head (setf (getf npc :head)
                                      (let ((n (parse-integer value)))
                                        (check-type n (integer 0 9)
                                                    "the number of a head (0-9)")
                                        n)))))
                      (return how))))

(defun stage-facing-value (direction &key (playerp nil))
  (declare (ignore playerp))
  (ecase direction
    (north "CharacterFacingUp")
    (south "CharacterFacingDown")
    (east "CharacterFacingRight")
    (west "CharacterFacingLeft")))

(defstage face (who where)
  (destructuring-bind (name &key character-id &allow-other-keys)
      (require-actor who)
    (declare (ignore character-id))
    (format t "
~10tlda # CharacterID_~a
~10tjsr Lib.FindCharacter
~10tlda # ~a
~10tldy # ActorFacing
~10tsta (Self), y
"
            (cl-change-case:pascal-case (string name))
            (stage-facing-value where))))

(defgeneric compile-stage-math (fun args)
  (:method ((fun t) (args t))
    (error "Unimplemented math function ~s" fun)))

(defmacro defmath (fun (&rest args) &body body)
  `(defmethod compile-stage-math ((f (eql ',fun)) args)
     (block nil
       (destructuring-bind (,@args) args
         ,@body))))

(defun asm-number (n)
  (format nil "#$~2,'0x~20t; ~:*~d" n))

(defmacro define-simple-math ((fun) &body asm)
  (let ((asm* (gensym "ASM-")))
    `(defmath ,fun (a b)
       (let ((a* (stage/constant-value a))
             (b* (stage/constant-value b))
             (,asm* ',asm))
         (cond
           ((and a* b*)
            (return (,fun a* b*)))
           (a*
            (stage-directions->acc b)
            (dolist (asm ,asm*)
              (format t asm (asm-number  a*))))
           (b*
            (stage-directions->acc a)
            (dolist (asm ,asm*)
              (format t asm (asm-number b*))))
           (t
            (stage-directions->acc a)
            (format t "~%~10tsta ScriptTemp0")
            (stage-directions->acc b)
            (dolist (asm ,asm*)
              (format t asm "ScriptTemp0"))))))))

(define-simple-math (+) "~%~10tadc ~a")
(define-simple-math (-) "~%~10tsbc ~a")
(define-simple-math (logand) "~%~10tand ~a")
(define-simple-math (logior) "~%~10tora ~a")
(define-simple-math (logxor) "~%~10teor ~a")

(defgeneric compile-time-math (fun args)
  (:method ((fun t) (args t))
    (error "No compile-time math function for ~s ~s" fun args)))

(defmacro define-compiler-math (fun (&rest args) &body body)
  (let ((args* (loop for arg in args
                     if (char= #\& (char (string arg) 0))
                       collect arg
                     else
                       collect (intern (format nil "~a*" arg))))
        (argv (gensym "ARGS-")))
    `(defmethod compile-time-math ((fun (eql ',fun)) (,argv cons))
       (destructuring-bind (,@args*) ,argv
         (let (,@(loop for arg in args
                       for arg* in args*
                       unless (char= #\& (char (string arg) 0))
                         collect (list arg (list 'stage/constant-value arg*))))
           (if (some #'null (list ,@(remove-if (lambda (arg) (char= #\& (char (string arg) 0)))
                                               args)))
               (error "Compile-time maths require constant expressions, got ~s"
                      ,argv)
               (progn ,@body)))))))

(define-compiler-math + (a b) (+ a b))
(define-compiler-math - (a &optional b) (if b (- a b) (- a)))
(define-compiler-math × (a b) (* a b))
(define-compiler-math ÷ (a b) (/ a b))
(define-compiler-math √ (a) (sqrt a))
(define-compiler-math expt (a b) (expt a b))
(define-compiler-math log (a b) (log a b))

(defun stage/constant-value (expression)
  (block constant
    (etypecase expression
      (number (values expression t))
      (cons
       (let ((args (mapcar #'stage/constant-value (cdr expression))))
         (when (some #'null args)
           (return-from constant (values nil nil)))
         (values (compile-time-math (car expression)
                                    (cdr expression))
                 t)))
      (symbol (values nil nil))
      (string (values nil nil)))))

(defun script-auto-label ()
  (string (genlabel "ScriptAutoLabel")))

(defgeneric compile-stage-directions-test (fun args)
  (:method ((fun t) (args t))
    (error "No comparison/test for ~s ~s" fun args)))

(defmacro defcomparison (fun (&rest args) &body body)
  `(defmethod compile-stage-directions-test ((fun (eql ',fun)) args)
     (destructuring-bind (,@args) args
       (block nil ,@body))))

(defun stage-directions->acc (expr)
  (etypecase expr
    (number
     (check-type expr (integer (0) (#xff)))
     (format t "~%~10tlda #$~2,'0x~20t; ~d" expr expr))
    (cons
     (let ((const (stage/constant-value expr)))
       (cond
         ((and const
               (typep const '(integer 0 #xff)))
          (format t "~%~10tlda #$~2,'0x~20t; ~d" const const))
         (const
          (error "Cannot fit the value ~s (from ~s) into a byte"
                 const expr))
         ((eql 'var (car expr))
          (format t "~%~10tlda ~a" (field->label expr)))
         (t (error "Unknown expression kind: ~s" expr)))))))

(defun stage/constant-zero-p (x)
  (let ((n (stage/constant-value x)))
    (and n (zerop n))))

(defcomparison = (a b)
  (cond
    ((stage/constant-zero-p a)
     (stage-directions->acc b)
     (return "beq"))
    ((stage/constant-zero-p b)
     (stage-directions->acc a)
     (return "beq"))
    (t (stage-directions->acc a)
       (format t "~10tsta ScriptTemp0")
       (stage-directions->acc b)
       (format t "~10tcmp ScriptTemp0")
       (return "beq"))))

(defun invert-branch (branch)
  (when-let (inverted (cdr (assoc branch
                                  '((beq bne) (bne beq)
                                    (bpl bmi) (bmi bpl)
                                    (bvc bvs) (bvs bvc)
                                    (bcc bcs) (bcs bcc))
                                  :test #'string-equal)))
    (string-downcase (car inverted))))

(defun stage-directions->test (expr true false)
  (cond
    ((and (null true) (null false)) nil)
    ((null false)
     (let ((label (script-auto-label))
           (branch (invert-branch (compile-stage-directions-test
                                   (car expr) (cdr expr)))))
       (format t "~%~10t~a ~a~%" branch label)
       (stage-directions->code true)
       (format t "~%~a:" label)))
    ((null true)
     (let ((label (script-auto-label))
           (branch (compile-stage-directions-test (car expr) (cdr expr))))
       (format t "~%~10t~a ~a" branch label)
       (stage-directions->code false)
       (format t "~%~a:" label)))
    (t (let ((label1 (script-auto-label))
             (label2 (script-auto-label))
             (branch (compile-stage-directions-test (car expr) (cdr expr))))
         (format t "~%~10t~a ~a~%" branch label1)
         (stage-directions->code false)
         (format t "~%~10tjmp ~a~%" label2)
         (format t "~2%~a:" label1)
         (when true
           (stage-directions->code true))
         (format t "~2%~a:" label2)))))

(defstage if (condition true &optional false)
  (multiple-value-bind (val constantp) (stage/constant-value condition)
    (if constantp
        (if val
            (stage-directions->code true)
            (stage-directions->code false))
        (stage-directions->test condition true false))))

(defun field->label (field)
  (cond
    ((and (consp field)
          (eql 'var (car field)))
     (format nil "ScriptVar_~a"
             (cl-change-case:pascal-case (second field))))
    (t (error "Unknown field type ~s" field))))

(defstage set (var value)
  (stage-directions->acc value)
  (format t "~%~10tsta ~a" (field->label var)))

(defun stage-directions->code (tree)
  (unless tree (return-from stage-directions->code nil))
  (destructuring-bind (fun &rest args) tree
    (cond
      ((consp fun)
       (dolist (el tree)
         (stage-directions->code el)))
      ((symbolp fun)
       (compile-stage-direction fun args))
      (t (error "Unexpected tree function name ~s" fun)))))

(defun find-actor (actor)
  (or (and (string-equal actor 'player)
           (list "Player" :kind 'player :character-id #xff))
      (and (string-equal actor 'narrator)
           (list "Narrator" :kind 'narrator :character-id #xfe))
      (when-let (i (position actor *actors*
                             :key #'first
                             :test #'string-equal))
        (elt *actors* i))
      (loop for match in *actors*
            for i from 0
            do (when (member actor (getf (subseq match 1) :nicks) :test #'string-equal)
                 (return (cons i match))))))

(defun find-or-load-actor (actor)
  (or (find-actor actor)
      (let ((record (load-actor actor)))
        (when record (cons actor record)))
      (error "Unable to find or load actor ~:(~a~)" actor)))

(defun require-actor (actor)
  (tagbody top
     (let* ((record (find-actor actor))
            #+ () (name (and (consp record) (< 1 (length record)) (second record)))
            (deets (and (consp record) (< 1 (length record)) (subseq record 1))))
       (unless record
         (error "Could not find actor ~:(~a~) in this scene~@[ (in ~a)~].
Did you forget to have them Enter the scene?
~:[No actors are present.~;Actors present: ~{~:(~a~)~^, ~}.~]"
                actor *current-scene* (sort (copy-list (mapcar #'car *actors*)) #'string<)))
       (unless (getf deets :character-id)
         #+ () (warn "No details on ~:(~a~), checking NPC stats file …" actor)
         (setf record (load-actor actor)))
       (unless (and deets (getf deets :character-id))
         (error "Actor ~:(~a~) does not seem to be found in the NPC Stats file.
~:[Nothing is known but their name.~;Info about actor is limited to:~{~%~8t~20a: ~a~}~]"
                actor deets))
       (return-from require-actor record))))

(defun fountain/write-scene-start (value)
  (setf *current-scene*
        (etypecase value
          (cons (concatenate 'string
                             (cl-change-case:pascal-case (first value))
                             "/"
                             (cl-change-case:pascal-case (second value))))
          (string (format nil "~{~a~^/~}"
                          (mapcar #'cl-change-case:pascal-case
                                  (split-sequence #\/ value))))))
  (format t "
~10t.mva NextMap, # Map_~a_ID
~10tlda # <JLoadMap
~10tjsr Lib.LoadAsset
~%" (substitute #\_ #\/ *current-scene*)))

(defun fountain/write-speech (value)
  "Write the speech data for VALUE in text and SpeakJet forms"
  (restart-case
      (push (format nil "
~10t.weak
~10t  Defined_Dialogue_P_~a~:* := false
~10t.endweak
~10t.if !Defined_Dialogue_P_~a~:*
~10tDefined_Dialogue_P_~a~:* := true
Dialogue_~a:
~10t.ptext ~s
Speech_~a:~
~{~%~10t.byte ~15a~^, ~15a~^, ~15a~^, ~15a~}
~10t.fi
"
                    (dialogue-hash value)
                    (prepare-dialogue value)
                    (dialogue-hash value)
                    (convert-for-atarivox value))
            *dialogue*)
    (reload-dictionary ()
      :report "Reload the AtariVox (SpeakJet) dictionary"
      (reload-atarivox-dictionary)
      (fountain/write-speech value)))
  (format t "
~10t.mvaw DialoguePointer, Dialogue_~d~32t ; ~s
~10t.mvaw SpeechPointer, Speech_~0@*~d~*
~10tjsr Lib.DoDialogue~%"
          (dialogue-hash value)
          value))

(defun dialogue-hash (text)
  (let ((s (format nil "~36r" (sxhash text))))
    (subseq s 0 (min (length s) 6))))

(defun fountain/write-speech-branch (option text)
  "Write the speech data for TEXT in text and SpeakJet forms as the label for script destination OPTION"
  (tagbody top
     (restart-case
         (push (format nil
                       "
~10t.weak
~10t  Defined_Dialogue_P_~a~:* := false
~10t.endweak
~10t.if !Defined_Dialogue_P_~a~:*
~10tDefined_Dialogue_P_~a~:* := true
Dialogue_~a:
~10t.ptext ~s
Speech_~a:~
~{~%~10t.byte ~15a~^, ~15a~^, ~15a~^, ~15a~}
~10t.fi
"
                       (dialogue-hash text)
                       (prepare-dialogue text)
                       (dialogue-hash text)
                       (convert-for-atarivox text))
               *dialogue*)
       (reload-dictionary ()
         :report "Reload the AtariVox (SpeakJet) dictionary"
         (reload-atarivox-dictionary)
         (go top))))
  (format t "
~10t.mvaw DialoguePointer, Dialogue_~a~32t ; ~s
~10t.mvaw SpeechPointer, Speech_~0@*~d~*
~10t.mvaw Pointer2, ~a
~10tjsr Lib.DoBranchingDialogue~%"
          (dialogue-hash text)
          text
          (if (string-equal "CONTINUE" option)
              "0 ; (continue)"
              (concatenate 'string "ScriptLabel_"
                           (cl-change-case:pascal-case option))))
  (return-from fountain/write-speech-branch
    (dialogue-hash text)))

(defun compile-fountain-stream (fountain)
  "Compile the contents of the stream FOUNTAIN into assembly language"
  (format t "~&~10t.enc \"minifont\"~2%Script_~a_~a:~10t.block"
          (cl-change-case:pascal-case (last-elt (pathname-directory
                                                 *current-pathname*)))
          (cl-change-case:pascal-case (pathname-name *current-pathname*)))
  (let ((lexer (make-fountain-lexer fountain))
        (*dialogue* nil)
        (*actors* nil)
        (*line-number* 0)
        (*fountain-state* nil))
    (loop (multiple-value-bind (sym value)
              (restart-case (funcall lexer)
                (continue ()
                  :report (lambda (s) (format s "End the script there"))
                  (values 'abend)))
            #+() (format *trace-output* "~%~s: ~s" sym value)
            (case sym
              (stage (stage-directions->code value))
              (jump
               (format t "~&;;; Script continues in another asset…")
               (compile-stage-direction 'jump value))
              (metadata
               (format t "~%;;; ~a" value))
              (comment
                (format t "~%~10t;; ~a" value))
              (label
               (format t "~%ScriptLabel_~a:"
                       (cl-change-case:pascal-case value)))
              (scene
               (fountain/write-scene-start value))
              (speaker
               (destructuring-bind (actor-name &rest _) (require-actor value)
                 (declare (ignore _))
                 (format t "
~10tlda # CharacterID_~a
~10tjsr Lib.FindCharacter
~10tstx DialogueSpeakerDecal
~10tlda DecalXH, x
~10tsta DialogueSpeakerX"
                         (cl-change-case:pascal-case (string actor-name)))))
              (speech (fountain/write-speech value))
              (end
               (format t "~%~10tjmp Lib.ScriptFinis~32t; ~a~%~10t.bend~2%"
                       (or value "End of file."))
               (return))
              (speaker-oc
               (format t "~&
~10tldx #$ff
~10tstx DialogueSpeakerDecal
~10tstx DialogueSpeakerX")
               ;; FIXME speaker's name not passed in #202
               )
              (fade-to
               (format t "~2&~10tlda # ~a"
                       (cond
                         ((string= "WHITE" value) "CoLu(COLGRAY, $f)")
                         ((string= "BLACK" value) "CoLu(COLGRAY, 0)")
                         (t (error "Fade to black or white only"))))
               (format t "~%~10tjsr Lib.FadeBrightness~%"))
              (branch
               (destructuring-bind (option . text) value
                 (fountain/write-speech-branch option text)))
              (go (format t "~%~10tjmp ScriptLabel_~a~%"
                          (cl-change-case:pascal-case value)))
              (otherwise
               (cerror "Insert a no-op"
                       "An unhandled feature was encountered in the script. ~
 Symbol: ~s Value: ~s"
                       sym value)
               (format nil "~2%~10tnop~32t;; ~s ~s~2%"
                       sym value)))
            sym))
    (dolist (actor *actors*)
      (destructuring-bind (name &key kind hp ac pitch speed
                                     hair-color skin-color clothes-color
                                     speech-color
                                     head body character-id
                           &allow-other-keys)
          actor
        (unless (string-equal 'player name)
          (format t "
~10t.weak
~10t  Character_Defined_P_~0@*~a := false
~10t.endweak
~10t.if !Character_Defined_P_~0@*~a
~10tCharacter_Defined_P_~0@*~a := true
~10tCharacterID_~0@*~a = $~18@*~2,'0x
Character_~0@*~a:
~10t.word CharacterClass
* = Character_~0@*~a + EntityDecal~1@*
~10t.byte $~2,'0x ~32t; Decal ID is needed
* = Character_~0@*~a + ActorHP~2@*
~10t.word ~d ~32t; HP
* = Character_~0@*~a + ActorMaxHP~3@*
~10t.word ~d ~32t; Max HP
* = Character_~0@*~a + ActorAction~4@*
~10t.byte ~a
* = Character_~0@*~a + ActorFacing ~5@*
~10t.byte CharacterFacing~a
* = Character_~0@*~a + ActorFlags ~6@*
~10t.byte ~d ~32t; flags
* = Character_~0@*~a + ActorAttrs ~7@*
~10t.byte ~{$~2,'0x~^, ~} ~32t; attributes …
* = Character_~0@*~a + CharacterDecalKind ~8@*
~10t.byte DecalKind~a
* = Character_~0@*~a + CharacterSkinColor ~9@*
~10t.byte PaletteColor_~:(~a~)
* = Character_~0@*~a + CharacterHairColor ~10@*
~10t.byte PaletteColor_~:(~a~)
* = Character_~0@*~a + CharacterClothesColor ~11@*
~10t.byte PaletteColor_~:(~a~)
* = Character_~0@*~a + CharacterHead ~12@*
~10t.byte ~d ~32t; head
* = Character_~0@*~a + CharacterBody ~13@*
~10t.byte ~d ~32t; body
* = Character_~0@*~a + CharacterShield ~14@*
~10t.byte 0 ~32t; shield
* = Character_~0@*~a + CharacterEquipment ~15@*
~10t.byte 0 ~32t; equipment item
* = Character_~0@*~a + CharacterAuxItem ~16@*
~10t.byte 0 ~32t; auxiliary item
* = Character_~0@*~a + CharacterArmorClass ~17@*
~10t.byte ~d ~32t; armor class
* = Character_~0@*~a + CharacterCharacterID ~18@*
~10t.byte $~2,'0x ~32t; character ID
* = Character_~0@*~a + CharacterSpeechPitch ~19@*
~10t.byte ~d ~32t; speech pitch
* = Character_~0@*~a + CharacterSpeechSpeed ~20@*
~10t.byte ~d ~32t; speech speed
* = Character_~0@*~a + CharacterSpeechColor ~21@*
~10t.byte CoLu(COL~:@(~a~), $f) ~32t; speech color
* = Character_~0@*~a + CharacterNameLength ~22@*
~10t.ptext \"~a\" ~32t; name
~10t;; This relies upon the knowledge that the CharacterName
~10t;; is the final field, so * will be pointing at the first
~10t;; free byte (after using the bare minimum for the actual
~10t;; length of the character name, so probably at least a
~10t;; couple of bytes less than CharacterSize)
~10t.fi
"
                  (cl-change-case:pascal-case (string name))
                  ;; --- Entity
                  ;; Decal
                  #xff
                  ;; --- Actor
                  ;; HP
                  (or hp 10)
                  ;; Max HP
                  (or hp 10)
                  ;; Action
                  "ActionIdle"
                  ;; Facing
                  "Down"
                  ;; Flags
                  0
                  ;; Attrs
                  (list 0 0 0 0 0)
                  ;; --- Character
                  ;; DecalKind
                  (cl-change-case:pascal-case
                   (string kind))
                  ;; SkinColor
                  (or skin-color "PEACH")
                  ;; HairColor
                  (or hair-color "BLACK")
                  ;; ClothesColor
                  (or clothes-color "BLUE")
                  ;; Head
                  (or head 1)
                  ;; Body
                  (or body 1)
                  ;; Shield
                  0
                  ;; Equipment
                  0
                  ;; AuxItem
                  0
                  ;; ArmorClass
                  (or ac 10)
                  ;; CharacterID
                  character-id
                  ;; SpeechPitch
                  (or pitch 80)
                  ;; SpeechSpeed
                  (or speed 100)
                  ;; SpeechColor
                  (cl-change-case:pascal-case
                   (or speech-color "Gray"))
                  ;; NameLength + Name
                  name))))
    (setf *dialogue* (reverse *dialogue*))
    (dolist (dlg *dialogue*)
      (fresh-line)
      (princ dlg)))
  (format t "~2%;;; end of script file.~%"))

(defun compile-fountain-string (string)
  "Compile Fountain script in STRING "
  (with-input-from-string (fountain string)
    (let ((*current-pathname* (format nil "(a ~:d character-long string)"
                                      (length string))))
      (compile-fountain-stream fountain))))

(defun compile-script (from to)
  "Compile Fountain file with pathname FROM into assembly source code file with pathname TO"
  (tagbody top
     (let ((*actors* (list nil)))
       (restart-case
           (with-output-to-file (*standard-output* to :if-does-not-exist :create
                                                      :if-exists :supersede)
             (compile-fountain-script from))
         (reload-script ()
           :report (lambda (s) (format s "Reload script ~a" (enough-namestring from)))
           (go top))
         (reload-npc-stats () :report "Reload NPC stats from Source/Tables/NPCStats.ods"
           (load-npc-stats)
           (go top))))))

(defvar *npc-stats* nil)

(defun find-npc-stats (name)
  (unless *npc-stats* (load-npc-stats))
  (loop for npc in *npc-stats*
        when (or (string-equal (getf npc :name) name)
                 (member name (getf npc :nicks) :test #'string-equal))
          do (return npc)))

(defun load-npc-stats (&optional (pathname "Source/Tables/NPCStats.ods"))
  "Load the NPC stats table from PATHNAME"
  (format *trace-output* "~&Reading NPC stats from “~a” …"
          (enough-namestring pathname))
  (let ((lol (ss->lol (first (read-ods-into-lists pathname)))))
    (loop for i from 0 below (length lol)
          do (setf (getf (elt lol i) :character-id) i
                   (getf (elt lol i) :nicks)
                   (remove-if #'null
                              (mapcar (lambda (n)
                                        (and n (string-trim " " n)))
                                      (split-sequence #\,
                                                      (getf (elt lol i) :nicks))))))
    (setf *npc-stats* (remove-if (lambda (char) (emptyp (getf char :name))) lol))
    (format *trace-output* " … now I know about ~:d non-player character~:p"
            (length *npc-stats*))
    *npc-stats*))

(defun load-boats (&optional (pathname "Source/Tables/Boats.ods"))
  "Load the registry of boats from PATHNAME"
  (unless (and *boat-ids* *boat-classes*)
    (format *trace-output* "~&Reading boats from “~a” …" (enough-namestring pathname))
    (let ((lol (ss->lol (first (read-ods-into-lists pathname))))
          (boat-ids (make-hash-table :test 'equalp))
          (boat-classes (make-hash-table :test 'equalp)))
      (dotimes (i (length lol))
        (setf (gethash (getf (elt lol i) :boat) boat-ids)
              i
              (gethash (getf (elt lol i) :boat) boat-classes)
              (getf (elt lol i) :class)))
      (setf *boat-ids* boat-ids
            *boat-classes* boat-classes))
    (format *trace-output* " … now I know about ~:d boat~:p" (hash-table-count *boat-ids*))))

(defun find-script-id (script-moniker)
  "Find the script ID for SCRIPT-MONIKER, defined by a scene number or the hash of its name"
  (let* ((path (mapcar #'cl-change-case:pascal-case
                       (flatten (mapcar (curry #'split-sequence #\/)
                                        (split-sequence #\- script-moniker)))))
         (dir (mapcar (lambda (el)
                        (cl-change-case:pascal-case
                         (string-trim #(#\Space #\Tab) el)))
                      (butlast path)))
         (dir (if (equal "Scripts" (first dir))
                  (subseq dir 1)
                  dir))
         (title (cl-change-case:pascal-case (string-trim #(#\Space #\Tab)
                                                         (last-elt path))))
         (pathname (make-pathname
                    :directory (append (list :relative "Source" "Scripts") dir)
                    :name title
                    :type "fountain"))
         (dir-hash (ash
                    (reduce #'logxor
                            (mapcar (lambda (ch) (- (char-code ch) (char-code #\A)))
                                    (remove-if-not #'upper-case-p
                                                   (coerce (first dir) 'list))))
                    11)))
    (unless (probe-file pathname)
      (error "Could not find expected script file “~a” for script named “~a”"
             (enough-namestring pathname) script-moniker))
    (with-input-from-file (fountain pathname)
      (loop for line = (read-line fountain nil nil)
            while line
            do (when-let (matches (cl-ppcre:all-matches "^(INT|EXT).*#[0-9]+#" line))
                 (let* ((scene-number (parse-integer
                                       (subseq line (1+ (position #\# line)))
                                       :junk-allowed t))
                        (id (logior dir-hash scene-number)))
                   (check-type scene-number (integer 0 #x7ff)
                               "a scene number integer between 0 and 2,047")
                   (format *trace-output* "~&//* Script “~a” is scene ~:d; id $~4,'0x"
                           script-moniker scene-number id)
                   (return-from find-script-id id))))
      (let ((id (logior dir-hash (logand #x7ff (sxhash title)))))
        #+ () (warn "Script file “~a” is missing a Scene number line. ~
The very first scene line (INT/EXT line) may have an unique scene number affixed to that line. ~
This number is needed only on the very first scene and must be unique among all script files ~
in a certain locale (e.g. island). Lacking a manually-provided one, I'll use $~4,'0x (Scene ~:d)"
                    (enough-namestring pathname) id (logand #xff (sxhash title)))
        (format *trace-output* "~&//* Script “~:(~a~)” is scene ~:d; id $~4,'0x"
                script-moniker (logand #x7ff id) id)
        id))))
