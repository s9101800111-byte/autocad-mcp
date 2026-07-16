;;; mcp_dispatch.lsp — File-based IPC dispatcher for AutoCAD MCP v3.1
;;;
;;; Protocol:
;;;   1. Python writes command JSON to C:/temp/autocad_mcp_cmd_{id}.json
;;;   2. Python types "(c:mcp-dispatch)" + Enter
;;;   3. This function reads cmd, dispatches via command map, writes result JSON
;;;   4. Python polls for C:/temp/autocad_mcp_result_{id}.json
;;;
;;; SECURITY: No raw eval — dispatcher uses a command whitelist/map.
;;; Compatible with AutoCAD LT 2024+.

;; Load dependencies
(if (not report-error)
  (defun report-error (msg) (princ (strcat "\nERROR: " msg)))
)

;; IPC directory
(setq *mcp-ipc-dir* "C:/temp/")

;; -----------------------------------------------------------------------
;; JSON-like output helpers (minimal, no external library)
;; -----------------------------------------------------------------------

(defun mcp-write-result (filepath request-id ok-flag payload error-msg / fp)
  "Write a result JSON file. Atomic: write to .tmp then rename."
  (setq tmp-path (strcat filepath ".tmp"))
  (setq fp (open tmp-path "w"))
  (if fp
    (progn
      (write-line "{" fp)
      (write-line (strcat "  \"request_id\": \"" request-id "\",") fp)
      (if ok-flag
        (progn
          (write-line "  \"ok\": true," fp)
          (write-line (strcat "  \"payload\": " payload) fp)
        )
        (progn
          (write-line "  \"ok\": false," fp)
          (write-line (strcat "  \"error\": \"" (mcp-escape-string error-msg) "\"") fp)
        )
      )
      (write-line "}" fp)
      (close fp)
      ;; Rename .tmp to final path (atomic on NTFS)
      (vl-file-rename tmp-path filepath)
    )
    (princ (strcat "\nMCP: Cannot open result file: " tmp-path))
  )
)

(defun mcp-escape-string (s / result i ch)
  "Escape quotes and backslashes in a string for JSON."
  (if (null s) (setq s ""))
  (setq result "" i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (cond
      ((= ch "\"") (setq result (strcat result "\\\"")))
      ((= ch "\\") (setq result (strcat result "\\\\")))
      (t (setq result (strcat result ch)))
    )
    (setq i (1+ i))
  )
  result
)

(defun mcp-read-file-lines (filepath / fp line lines)
  "Read all lines from a file into a single string."
  (setq fp (open filepath "r"))
  (if (not fp) (progn (princ (strcat "\nMCP: Cannot read: " filepath)) nil)
    (progn
      (setq lines "")
      (while (setq line (read-line fp))
        (setq lines (strcat lines line))
      )
      (close fp)
      lines
    )
  )
)

;; -----------------------------------------------------------------------
;; Simple JSON parser (extracts string values by key)
;; -----------------------------------------------------------------------

(defun mcp-json-get-string (json key / search-str pos end-pos value)
  "Extract a string value for a given key from JSON text."
  (setq search-str (strcat "\"" key "\""))
  (setq pos (vl-string-search search-str json))
  (if (null pos) nil
    (progn
      ;; Find the colon after key
      (setq pos (vl-string-search ":" json pos))
      (if (null pos) nil
        (progn
          ;; Find opening quote of value
          (setq pos (vl-string-search "\"" json (1+ pos)))
          (if (null pos) nil
            (progn
              (setq pos (+ pos 2))  ; 0-based search result + 2 = 1-based position after quote
              ;; Find closing quote (skip escaped quotes)
              (setq end-pos pos)
              (while (and (<= end-pos (strlen json))
                          (or (= end-pos pos)
                              (/= (substr json end-pos 1) "\"")))
                ;; Handle escaped characters
                (if (= (substr json end-pos 1) "\\")
                  (setq end-pos (+ end-pos 2))
                  (setq end-pos (1+ end-pos))
                )
              )
              (substr json pos (- end-pos pos))
            )
          )
        )
      )
    )
  )
)

(defun mcp-json-get-number (json key / search-str pos num-start num-end ch)
  "Extract a number value for a given key from JSON text."
  (setq search-str (strcat "\"" key "\""))
  (setq pos (vl-string-search search-str json))
  (if (null pos) nil
    (progn
      (setq pos (vl-string-search ":" json pos))
      (if (null pos) nil
        (progn
          (setq pos (+ pos 2))  ; 0-based search result + 2 = 1-based position after colon
          ;; Skip whitespace
          (while (and (<= pos (strlen json))
                      (member (substr json pos 1) '(" " "\t" "\n")))
            (setq pos (1+ pos))
          )
          ;; Read number
          (setq num-start pos num-end pos)
          (while (and (<= num-end (strlen json))
                      (or (member (substr json num-end 1) '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "." "-" "+"))
                      ))
            (setq num-end (1+ num-end))
          )
          (atof (substr json num-start (- num-end num-start)))
        )
      )
    )
  )
)

;; -----------------------------------------------------------------------
;; String splitting utility (used by semicolon-delimited encodings)
;; -----------------------------------------------------------------------

(defun mcp-split-string (str delim / pos result token)
  "Split a string by single-char delimiter. Returns a list of strings."
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq token (substr str 1 pos))
    (setq result (append result (list token)))
    (setq str (substr str (+ pos 2)))
  )
  (setq result (append result (list str)))
  result
)

;; -----------------------------------------------------------------------
;; Command dispatcher — WHITELIST ONLY, no eval
;; -----------------------------------------------------------------------

(defun mcp-dispatch-command (cmd-name params-json / result)
  "Dispatch a command by name. Returns (ok . payload-or-error)."
  (cond
    ;; --- Ping ---
    ((= cmd-name "ping")
     (cons T "\"pong\""))

    ;; --- Freehand LISP execution ---
    ((= cmd-name "execute-lisp")
     (mcp-cmd-execute-lisp params-json))

    ;; --- Undo / Redo ---
    ((= cmd-name "undo")
     (command "_.UNDO" "1") (cons T "\"undone\""))

    ((= cmd-name "redo")
     (command "_.REDO") (cons T "\"redone\""))

    ;; --- Drawing info ---
    ((= cmd-name "drawing-info")
     (mcp-cmd-drawing-info))

    ((= cmd-name "drawing-get-units-and-base")
     (mcp-cmd-drawing-get-units-and-base))

    ((= cmd-name "drawing-set-insertion-base")
     (mcp-cmd-drawing-set-insertion-base params-json))

    ;; --- Layer operations ---
    ((= cmd-name "layer-list")
     (mcp-cmd-layer-list))

    ((= cmd-name "layer-create")
     (mcp-cmd-layer-create params-json))

    ((= cmd-name "layer-set-current")
     (mcp-cmd-layer-set-current params-json))

    ((= cmd-name "layer-set-properties")
     (mcp-cmd-layer-set-properties params-json))

    ((= cmd-name "layer-freeze")
     (mcp-cmd-layer-freeze params-json))

    ((= cmd-name "layer-thaw")
     (mcp-cmd-layer-thaw params-json))

    ((= cmd-name "layer-lock")
     (mcp-cmd-layer-lock params-json))

    ((= cmd-name "layer-translate")
     (mcp-cmd-layer-translate params-json))

    ((= cmd-name "layer-unlock")
     (mcp-cmd-layer-unlock params-json))

    ;; --- Entity creation ---
    ((= cmd-name "create-line")
     (mcp-cmd-create-line params-json))

    ((= cmd-name "create-circle")
     (mcp-cmd-create-circle params-json))

    ((= cmd-name "create-polyline")
     (mcp-cmd-create-polyline params-json))

    ((= cmd-name "create-rectangle")
     (mcp-cmd-create-rectangle params-json))

    ((= cmd-name "create-text")
     (mcp-cmd-create-text params-json))

    ((= cmd-name "create-arc")
     (mcp-cmd-create-arc params-json))

    ((= cmd-name "create-ellipse")
     (mcp-cmd-create-ellipse params-json))

    ((= cmd-name "create-mtext")
     (mcp-cmd-create-mtext params-json))

    ((= cmd-name "create-hatch")
     (mcp-cmd-create-hatch params-json))

    ;; --- Entity queries ---
    ((= cmd-name "entity-count")
     (mcp-cmd-entity-count params-json))

    ((= cmd-name "entity-list")
     (mcp-cmd-entity-list params-json))

    ((= cmd-name "entity-get")
     (mcp-cmd-entity-get params-json))

    ((= cmd-name "entity-get-selection")
     (mcp-cmd-entity-get-selection params-json))

    ((= cmd-name "entity-query")
     (mcp-cmd-entity-query params-json))

    ((= cmd-name "entity-find-text")
     (mcp-cmd-entity-find-text params-json))

    ((= cmd-name "entity-export-geometry")
     (mcp-cmd-entity-export-geometry params-json))

    ((= cmd-name "entity-erase")
     (mcp-cmd-entity-erase params-json))

    ;; --- Entity modification ---
    ((= cmd-name "entity-move")
     (mcp-cmd-entity-move params-json))

    ((= cmd-name "entity-copy")
     (mcp-cmd-entity-copy params-json))

    ((= cmd-name "entity-rotate")
     (mcp-cmd-entity-rotate params-json))

    ((= cmd-name "entity-scale")
     (mcp-cmd-entity-scale params-json))

    ((= cmd-name "entity-mirror")
     (mcp-cmd-entity-mirror params-json))

    ((= cmd-name "entity-offset")
     (mcp-cmd-entity-offset params-json))

    ((= cmd-name "entity-array")
     (mcp-cmd-entity-array params-json))

    ((= cmd-name "entity-fillet")
     (mcp-cmd-entity-fillet params-json))

    ((= cmd-name "entity-chamfer")
     (mcp-cmd-entity-chamfer params-json))

    ;; --- View ---
    ((= cmd-name "zoom-extents")
     (command "_.ZOOM" "_E")
     (cons T "\"zoomed to extents\""))

    ((= cmd-name "zoom-window")
     (progn
       (setq x1 (mcp-json-get-number params-json "x1"))
       (setq y1 (mcp-json-get-number params-json "y1"))
       (setq x2 (mcp-json-get-number params-json "x2"))
       (setq y2 (mcp-json-get-number params-json "y2"))
       (command "_.ZOOM" "_W" (list x1 y1 0) (list x2 y2 0))
       (cons T "\"zoomed to window\"")))

    ;; --- Drawing file ops ---
    ((= cmd-name "drawing-save")
     (progn
       (setq path (mcp-json-get-string params-json "path"))
       (if (and path (> (strlen path) 0))
         (progn
           (setvar "FILEDIA" 0)
           (command "_.SAVEAS" "" path)
           (setvar "FILEDIA" 1)
           (cons T (strcat "\"saved to: " (mcp-escape-string path) "\"")))
         (progn (command "_.QSAVE") (cons T "\"saved\"")))))

    ((= cmd-name "drawing-save-as-dxf")
     (progn
       (setq path (mcp-json-get-string params-json "path"))
       (if path
         (progn (command "_.SAVEAS" "DXF" path) (cons T (strcat "\"" path "\"")))
         (cons nil "Save path required"))))

    ((= cmd-name "drawing-purge")
     (command "_.-PURGE" "_ALL" "*" "_N")
     (cons T "\"purged\""))

    ((= cmd-name "drawing-open")
     (progn
       (setq path (mcp-json-get-string params-json "path"))
       (if path
         (progn
           (setvar "FILEDIA" 0)
           (command "_.OPEN" path)
           (setvar "FILEDIA" 1)
           (cons T (strcat "\"opened: " (mcp-escape-string path) "\"")))
         (cons nil "Path required"))))

    ;; --- P&ID ---
    ((= cmd-name "pid-setup-layers")
     (if c:setup-pid-layers
       (progn (c:setup-pid-layers) (cons T "\"P&ID layers created\""))
       (cons nil "pid_tools.lsp not loaded")))

    ((= cmd-name "pid-insert-symbol")
     (mcp-cmd-pid-insert-symbol params-json))

    ((= cmd-name "pid-draw-process-line")
     (mcp-cmd-pid-draw-process-line params-json))

    ((= cmd-name "pid-connect-equipment")
     (mcp-cmd-pid-connect-equipment params-json))

    ((= cmd-name "pid-add-flow-arrow")
     (mcp-cmd-pid-add-flow-arrow params-json))

    ((= cmd-name "pid-add-equipment-tag")
     (mcp-cmd-pid-add-equipment-tag params-json))

    ((= cmd-name "pid-add-line-number")
     (mcp-cmd-pid-add-line-number params-json))

    ((= cmd-name "pid-insert-valve")
     (mcp-cmd-pid-insert-valve params-json))

    ((= cmd-name "pid-insert-instrument")
     (mcp-cmd-pid-insert-instrument params-json))

    ((= cmd-name "pid-insert-pump")
     (mcp-cmd-pid-insert-pump params-json))

    ((= cmd-name "pid-insert-tank")
     (mcp-cmd-pid-insert-tank params-json))

    ;; --- Block operations ---
    ((= cmd-name "block-list")
     (mcp-cmd-block-list))

    ((= cmd-name "block-insert")
     (mcp-cmd-block-insert params-json))

    ((= cmd-name "block-insert-with-attributes")
     (mcp-cmd-block-insert-with-attribs params-json))

    ((= cmd-name "block-extract-attributes")
     (mcp-cmd-block-extract-attributes params-json))

    ((= cmd-name "block-get-attributes")
     (mcp-cmd-block-get-attributes params-json))

    ((= cmd-name "block-update-attribute")
     (mcp-cmd-block-update-attribute params-json))

    ((= cmd-name "block-define")
     (cons nil "block-define not available via IPC (use ezdxf backend)"))

    ;; --- Annotation ---
    ((= cmd-name "create-dimension-linear")
     (mcp-cmd-create-dimension-linear params-json))

    ((= cmd-name "create-dimension-aligned")
     (mcp-cmd-create-dimension-aligned params-json))

    ((= cmd-name "create-dimension-angular")
     (mcp-cmd-create-dimension-angular params-json))

    ((= cmd-name "create-dimension-radius")
     (mcp-cmd-create-dimension-radius params-json))

    ((= cmd-name "annotation-find-replace")
     (mcp-cmd-annotation-find-replace params-json))

    ((= cmd-name "create-leader")
     (mcp-cmd-create-leader params-json))

    ;; --- Drawing management ---
    ((= cmd-name "drawing-create")
     (mcp-cmd-drawing-create params-json))

    ((= cmd-name "drawing-get-variables")
     (mcp-cmd-drawing-get-variables params-json))

    ((= cmd-name "drawing-plot-pdf")
     (mcp-cmd-drawing-plot-pdf params-json))

    ((= cmd-name "drawing-wblock-by-regions")
     (mcp-cmd-drawing-wblock-by-regions params-json))

    ;; --- P&ID list symbols ---
    ((= cmd-name "pid-list-symbols")
     (mcp-cmd-pid-list-symbols params-json))

    ;; --- Unknown ---
    (t (cons nil (strcat "Unknown command: " cmd-name)))
  )
)

;; -----------------------------------------------------------------------
;; Command implementations
;; -----------------------------------------------------------------------

(defun mcp-cmd-drawing-info ( / count layers layer-list)
  "Return drawing info: entity count, layers, extents."
  (setq count 0)
  (setq ent (entnext))
  (while ent
    (setq count (1+ count))
    (setq ent (entnext ent))
  )
  (setq layer-list "")
  (setq layers (tblnext "LAYER" T))
  (while layers
    (if (> (strlen layer-list) 0)
      (setq layer-list (strcat layer-list ",\"" (cdr (assoc 2 layers)) "\""))
      (setq layer-list (strcat "\"" (cdr (assoc 2 layers)) "\""))
    )
    (setq layers (tblnext "LAYER"))
  )
  (cons T (strcat "{\"entity_count\":" (itoa count) ",\"layers\":[" layer-list "]}"))
)

(defun mcp-cmd-layer-list ( / layers layer-list name)
  "Return all layers as JSON array."
  (setq layer-list "")
  (setq layers (tblnext "LAYER" T))
  (while layers
    (setq name (cdr (assoc 2 layers)))
    (if (> (strlen layer-list) 0)
      (setq layer-list (strcat layer-list ",{\"name\":\"" name "\",\"color\":" (itoa (cdr (assoc 62 layers))) "}"))
      (setq layer-list (strcat "{\"name\":\"" name "\",\"color\":" (itoa (cdr (assoc 62 layers))) "}"))
    )
    (setq layers (tblnext "LAYER"))
  )
  (cons T (strcat "{\"layers\":[" layer-list "]}"))
)

(defun mcp-cmd-layer-create (params / name color linetype)
  (setq name (mcp-json-get-string params "name"))
  (setq color (mcp-json-get-string params "color"))
  (setq linetype (mcp-json-get-string params "linetype"))
  (if (not color) (setq color "white"))
  (if (not linetype) (setq linetype "CONTINUOUS"))
  (ensure_layer_exists name color linetype)
  (cons T (strcat "{\"name\":\"" name "\"}"))
)

(defun mcp-cmd-layer-set-current (params / name)
  (setq name (mcp-json-get-string params "name"))
  (setvar "CLAYER" name)
  (cons T (strcat "{\"current_layer\":\"" name "\"}"))
)

(defun mcp-cmd-create-line (params / x1 y1 x2 y2 layer)
  (setq x1 (mcp-json-get-number params "x1"))
  (setq y1 (mcp-json-get-number params "y1"))
  (setq x2 (mcp-json-get-number params "x2"))
  (setq y2 (mcp-json-get-number params "y2"))
  (setq layer (mcp-json-get-string params "layer"))
  (if layer
    (progn (ensure_layer_exists layer "white" "CONTINUOUS") (set_current_layer layer))
  )
  (command "_LINE" (list x1 y1 0.0) (list x2 y2 0.0) "")
  (cons T (strcat "{\"entity_type\":\"LINE\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}"))
)

(defun mcp-cmd-create-circle (params / cx cy radius layer)
  (setq cx (mcp-json-get-number params "cx"))
  (setq cy (mcp-json-get-number params "cy"))
  (setq radius (mcp-json-get-number params "radius"))
  (setq layer (mcp-json-get-string params "layer"))
  (if layer
    (progn (ensure_layer_exists layer "white" "CONTINUOUS") (set_current_layer layer))
  )
  (command "_CIRCLE" (list cx cy 0.0) radius)
  (cons T (strcat "{\"entity_type\":\"CIRCLE\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}"))
)

(defun mcp-cmd-create-polyline (params / pts-str closed layer pairs pt-str cx cy)
  (setq pts-str (mcp-json-get-string params "points_str"))
  (setq closed (mcp-json-get-string params "closed"))
  (setq layer (mcp-json-get-string params "layer"))
  (if layer (progn (ensure_layer_exists layer "white" "CONTINUOUS") (set_current_layer layer)))
  (if (not pts-str)
    (cons nil "points_str required (format: x1,y1;x2,y2;...)")
    (progn
      (command "_PLINE")
      (setq pairs (mcp-split-string pts-str ";"))
      (foreach pt-str pairs
        (setq cx (atof (car (mcp-split-string pt-str ","))))
        (setq cy (atof (cadr (mcp-split-string pt-str ","))))
        (command (list cx cy 0.0))
      )
      (if (= closed "1") (command "_C") (command ""))
      (cons T (strcat "{\"entity_type\":\"LWPOLYLINE\",\"handle\":\""
                      (cdr (assoc 5 (entget (entlast)))) "\"}"))
    )
  )
)

(defun mcp-cmd-create-rectangle (params / x1 y1 x2 y2 layer)
  (setq x1 (mcp-json-get-number params "x1"))
  (setq y1 (mcp-json-get-number params "y1"))
  (setq x2 (mcp-json-get-number params "x2"))
  (setq y2 (mcp-json-get-number params "y2"))
  (setq layer (mcp-json-get-string params "layer"))
  (if layer
    (progn (ensure_layer_exists layer "white" "CONTINUOUS") (set_current_layer layer))
  )
  (command "_RECTANG" (list x1 y1 0.0) (list x2 y2 0.0))
  (cons T (strcat "{\"entity_type\":\"LWPOLYLINE\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}"))
)

(defun mcp-cmd-create-text (params / x y text height rotation layer)
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq text (mcp-json-get-string params "text"))
  (setq height (mcp-json-get-number params "height"))
  (setq rotation (mcp-json-get-number params "rotation"))
  (if (not height) (setq height 2.5))
  (if (not rotation) (setq rotation 0.0))
  (setq layer (mcp-json-get-string params "layer"))
  (if layer
    (progn (ensure_layer_exists layer "white" "CONTINUOUS") (set_current_layer layer))
  )
  (command "_TEXT" "J" "M" (list x y 0.0) height rotation text)
  (cons T (strcat "{\"entity_type\":\"TEXT\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}"))
)

(defun mcp-cmd-entity-count (params / layer count ent ent-data)
  (setq layer (mcp-json-get-string params "layer"))
  (setq count 0 ent (entnext))
  (while ent
    (setq ent-data (entget ent))
    (if (or (not layer) (= (cdr (assoc 8 ent-data)) layer))
      (setq count (1+ count))
    )
    (setq ent (entnext ent))
  )
  (cons T (strcat "{\"count\":" (itoa count) "}"))
)

(defun mcp-cmd-entity-list (params / layer entities ent ent-data etype handle elayer)
  (setq layer (mcp-json-get-string params "layer"))
  (setq entities "" ent (entnext))
  (while ent
    (setq ent-data (entget ent))
    (setq etype (cdr (assoc 0 ent-data)))
    (setq handle (cdr (assoc 5 ent-data)))
    (setq elayer (cdr (assoc 8 ent-data)))
    (if (or (not layer) (= elayer layer))
      (progn
        (if (> (strlen entities) 0)
          (setq entities (strcat entities ","))
        )
        (setq entities (strcat entities "{\"type\":\"" etype "\",\"handle\":\"" handle "\",\"layer\":\"" elayer "\"}"))
      )
    )
    (setq ent (entnext ent))
  )
  (cons T (strcat "{\"entities\":[" entities "]}"))
)

(defun mcp-cmd-entity-erase (params / entity-id ent)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (if (= entity-id "last")
    (progn
      (setq ent (entlast))
      (if ent (progn (entdel ent) (cons T "\"erased last entity\""))
        (cons nil "No entity to erase")))
    (progn
      (setq ent (handent entity-id))
      (if ent (progn (entdel ent) (cons T (strcat "\"erased " entity-id "\"")))
        (cons nil (strcat "Entity not found: " entity-id))))
  )
)

(defun mcp-cmd-entity-move (params / entity-id dx dy ent)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (setq dx (mcp-json-get-number params "dx"))
  (setq dy (mcp-json-get-number params "dy"))
  (if (= entity-id "last")
    (setq ent (entlast))
    (setq ent (handent entity-id))
  )
  (if ent
    (progn
      (command "_.MOVE" ent "" '(0 0 0) (list dx dy 0))
      (cons T "\"moved\""))
    (cons nil "Entity not found")
  )
)

;; --- Freehand LISP execution ---

(defun mcp-cmd-execute-lisp (params / code-file result old-secureload)
  (setq code-file (mcp-json-get-string params "code_file"))
  (if (not code-file)
    (cons nil "code_file parameter required")
    (if (not (findfile code-file))
      (cons nil (strcat "Code file not found: " code-file))
      (progn
        ;; Suppress SECURELOAD dialog for MCP temp files
        (setq old-secureload (getvar "SECURELOAD"))
        (setvar "SECURELOAD" 0)
        (setq result (vl-catch-all-apply 'load (list code-file)))
        (setvar "SECURELOAD" old-secureload)
        (if (vl-catch-all-error-p result)
          (cons nil (strcat "LISP error: " (vl-catch-all-error-message result)))
          (cons T (strcat "\"" (mcp-escape-string (vl-princ-to-string result)) "\""))
        )
      )
    )
  )
)

;; --- Drawing create implementation ---

(defun mcp-cmd-drawing-create (params / ss)
  "Reset current drawing to a clean state (erase all, purge, reset to layer 0).
   Using _.NEW would create a new document tab with a fresh LISP namespace,
   breaking the IPC dispatcher. This approach preserves the dispatcher."
  (if (setq ss (ssget "_X"))
    (progn (command "_.ERASE" ss "") (setq ss nil))
  )
  (setvar "CLAYER" "0")
  (command "_.-PURGE" "_ALL" "*" "_N")
  (cons T (strcat "{\"drawing\":\"" (mcp-escape-string (getvar "DWGNAME")) "\"}"))
)

;; --- P&ID command implementations ---

(defun mcp-cmd-pid-insert-symbol (params / category symbol x y scale rotation)
  (setq category (mcp-json-get-string params "category"))
  (setq symbol (mcp-json-get-string params "symbol"))
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq scale (mcp-json-get-number params "scale"))
  (setq rotation (mcp-json-get-number params "rotation"))
  (if (not scale) (setq scale 1.0))
  (if (not rotation) (setq rotation 0.0))
  (if c:insert-pid-block
    (progn
      (c:insert-pid-block category symbol x y scale rotation)
      (cons T (strcat "{\"symbol\":\"" symbol "\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}"))
    )
    (cons nil "pid_tools.lsp not loaded")
  )
)

(defun mcp-cmd-pid-draw-process-line (params / x1 y1 x2 y2)
  (setq x1 (mcp-json-get-number params "x1"))
  (setq y1 (mcp-json-get-number params "y1"))
  (setq x2 (mcp-json-get-number params "x2"))
  (setq y2 (mcp-json-get-number params "y2"))
  (if c:draw-process-line
    (progn (c:draw-process-line x1 y1 x2 y2) (cons T "\"process line drawn\""))
    (cons nil "pid_tools.lsp not loaded")
  )
)

(defun mcp-cmd-pid-connect-equipment (params / x1 y1 x2 y2)
  (setq x1 (mcp-json-get-number params "x1"))
  (setq y1 (mcp-json-get-number params "y1"))
  (setq x2 (mcp-json-get-number params "x2"))
  (setq y2 (mcp-json-get-number params "y2"))
  (if c:connect-equipment
    (progn (c:connect-equipment x1 y1 x2 y2) (cons T "\"equipment connected\""))
    (cons nil "pid_tools.lsp not loaded")
  )
)

(defun mcp-cmd-pid-add-flow-arrow (params / x y rotation)
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq rotation (mcp-json-get-number params "rotation"))
  (if (not rotation) (setq rotation 0.0))
  (if c:add-flow-arrow
    (progn (c:add-flow-arrow x y rotation) (cons T "\"flow arrow added\""))
    (cons nil "pid_tools.lsp not loaded")
  )
)

(defun mcp-cmd-pid-add-equipment-tag (params / x y tag description)
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq tag (mcp-json-get-string params "tag"))
  (setq description (mcp-json-get-string params "description"))
  (if (not description) (setq description ""))
  (if c:add-equipment-tag
    (progn (c:add-equipment-tag x y tag description) (cons T (strcat "\"tagged: " tag "\"")))
    (cons nil "pid_tools.lsp not loaded")
  )
)

(defun mcp-cmd-pid-add-line-number (params / x y line-num spec)
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq line-num (mcp-json-get-string params "line_num"))
  (setq spec (mcp-json-get-string params "spec"))
  (if c:add-line-number
    (progn (c:add-line-number x y line-num spec) (cons T (strcat "\"line number: " line-num "\"")))
    (cons nil "pid_tools.lsp not loaded")
  )
)

(defun mcp-cmd-pid-insert-valve (params / x y valve-type rotation)
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq valve-type (mcp-json-get-string params "valve_type"))
  (setq rotation (mcp-json-get-number params "rotation"))
  (if (not rotation) (setq rotation 0.0))
  (if c:insert-valve-on-line
    (progn (c:insert-valve-on-line x y valve-type rotation) (cons T (strcat "\"valve: " valve-type "\"")))
    (cons nil "pid_tools.lsp not loaded")
  )
)

(defun mcp-cmd-pid-insert-instrument (params / x y inst-type rotation tag-id range-value)
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq inst-type (mcp-json-get-string params "instrument_type"))
  (setq rotation (mcp-json-get-number params "rotation"))
  (setq tag-id (mcp-json-get-string params "tag_id"))
  (setq range-value (mcp-json-get-string params "range_value"))
  (if (not rotation) (setq rotation 0.0))
  (if c:insert-instrument
    (progn
      (c:insert-instrument x y inst-type rotation)
      (if (and tag-id (> (strlen tag-id) 0))
        (c:insert-instrument-with-tag x y inst-type tag-id (if range-value range-value ""))
      )
      (cons T (strcat "\"instrument: " inst-type "\"")))
    (cons nil "pid_tools.lsp not loaded")
  )
)

(defun mcp-cmd-pid-insert-pump (params / x y pump-type rotation)
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq pump-type (mcp-json-get-string params "pump_type"))
  (setq rotation (mcp-json-get-number params "rotation"))
  (if (not rotation) (setq rotation 0.0))
  (if c:insert-pump
    (progn (c:insert-pump x y pump-type rotation) (cons T (strcat "\"pump: " pump-type "\"")))
    (cons nil "pid_tools.lsp not loaded")
  )
)

(defun mcp-cmd-pid-insert-tank (params / x y tank-type scale)
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq tank-type (mcp-json-get-string params "tank_type"))
  (setq scale (mcp-json-get-number params "scale"))
  (if (not scale) (setq scale 1.0))
  (if c:insert-tank
    (progn (c:insert-tank x y tank-type scale) (cons T (strcat "\"tank: " tank-type "\"")))
    (cons nil "pid_tools.lsp not loaded")
  )
)

;; --- Additional entity creation ---

(defun mcp-cmd-create-arc (params / cx cy radius sa ea layer)
  (setq cx (mcp-json-get-number params "cx"))
  (setq cy (mcp-json-get-number params "cy"))
  (setq radius (mcp-json-get-number params "radius"))
  (setq sa (mcp-json-get-number params "start_angle"))
  (setq ea (mcp-json-get-number params "end_angle"))
  (setq layer (mcp-json-get-string params "layer"))
  (if layer (progn (ensure_layer_exists layer "white" "CONTINUOUS") (set_current_layer layer)))
  (command "_ARC" "_C" (list cx cy 0.0) (list (+ cx radius) cy 0.0) "_A" (- ea sa))
  (cons T (strcat "{\"entity_type\":\"ARC\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}"))
)

(defun mcp-cmd-create-ellipse (params / cx cy mx my ratio layer)
  (setq cx (mcp-json-get-number params "cx"))
  (setq cy (mcp-json-get-number params "cy"))
  (setq mx (mcp-json-get-number params "major_x"))
  (setq my (mcp-json-get-number params "major_y"))
  (setq ratio (mcp-json-get-number params "ratio"))
  (setq layer (mcp-json-get-string params "layer"))
  (if layer (progn (ensure_layer_exists layer "white" "CONTINUOUS") (set_current_layer layer)))
  (command "_ELLIPSE" "_C" (list cx cy 0.0) (list mx my 0.0) ratio)
  (cons T (strcat "{\"entity_type\":\"ELLIPSE\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}"))
)

(defun mcp-cmd-create-mtext (params / x y width text height layer)
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq width (mcp-json-get-number params "width"))
  (setq text (mcp-json-get-string params "text"))
  (setq height (mcp-json-get-number params "height"))
  (if (not height) (setq height 2.5))
  (setq layer (mcp-json-get-string params "layer"))
  (if layer (progn (ensure_layer_exists layer "white" "CONTINUOUS") (set_current_layer layer)))
  (command "_MTEXT" (list x y 0.0) "_H" height "_W" width text "")
  (cons T (strcat "{\"entity_type\":\"MTEXT\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}"))
)

(defun mcp-cmd-create-hatch (params / entity-id pattern ent)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (setq pattern (mcp-json-get-string params "pattern"))
  (if (not pattern) (setq pattern "ANSI31"))
  (if (= entity-id "last")
    (setq ent (entlast))
    (setq ent (handent entity-id))
  )
  (if ent
    (progn
      (command "_HATCH" "_P" pattern "" "_S" ent "" "")
      (cons T (strcat "{\"entity_type\":\"HATCH\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}")))
    (cons nil "Entity not found for hatching")
  )
)

;; --- Entity query: get ---

(defun mcp-cmd-entity-get (params / entity-id ent ent-data etype handle elayer result)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (if (= entity-id "last")
    (setq ent (entlast))
    (setq ent (handent entity-id))
  )
  (if (not ent)
    (cons nil (strcat "Entity not found: " entity-id))
    (progn
      (setq ent-data (entget ent))
      (setq etype (cdr (assoc 0 ent-data)))
      (setq handle (cdr (assoc 5 ent-data)))
      (setq elayer (cdr (assoc 8 ent-data)))
      (setq result (strcat "{\"type\":\"" etype "\",\"handle\":\"" handle "\",\"layer\":\"" elayer "\""))
      ;; Add type-specific info
      (cond
        ((= etype "LINE")
         (setq result (strcat result
           ",\"start\":[" (rtos (car (cdr (assoc 10 ent-data))) 2 6) "," (rtos (cadr (cdr (assoc 10 ent-data))) 2 6) "]"
           ",\"end\":[" (rtos (car (cdr (assoc 11 ent-data))) 2 6) "," (rtos (cadr (cdr (assoc 11 ent-data))) 2 6) "]")))
        ((= etype "CIRCLE")
         (setq result (strcat result
           ",\"center\":[" (rtos (car (cdr (assoc 10 ent-data))) 2 6) "," (rtos (cadr (cdr (assoc 10 ent-data))) 2 6) "]"
           ",\"radius\":" (rtos (cdr (assoc 40 ent-data)) 2 6))))
      )
      (setq result (strcat result "}"))
      (cons T result)
    )
  )
)

;; --- Entity query: get_selection + query (shared summary helpers) ---

(defun mcp-ent->summary (ent / ed etype handle elayer p txt s)
  "Build a compact JSON object for one entity: type, handle, layer, point?, text?."
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (setq handle (cdr (assoc 5 ed)))
  (setq elayer (cdr (assoc 8 ed)))
  (setq s (strcat "{\"type\":\"" etype
                  "\",\"handle\":\"" handle
                  "\",\"layer\":\"" (mcp-escape-string elayer) "\""))
  ;; reference point (DXF group 10 — center / start / insertion / first vertex)
  (setq p (cdr (assoc 10 ed)))
  (if p
    (setq s (strcat s ",\"point\":[" (rtos (car p) 2 6) "," (rtos (cadr p) 2 6) "]"))
  )
  ;; text content (DXF group 1) for text-bearing entities
  (setq txt (cdr (assoc 1 ed)))
  (if (and txt (member etype '("TEXT" "MTEXT" "ATTRIB" "ATTDEF")))
    (setq s (strcat s ",\"text\":\"" (mcp-escape-string txt) "\""))
  )
  (strcat s "}")
)

(defun mcp-ss->entities-json (ss n / i ent result)
  "Serialize the first n entities of a selection set to a JSON array body."
  (setq result "" i 0)
  (while (< i n)
    (setq ent (ssname ss i))
    (if (> (strlen result) 0) (setq result (strcat result ",")))
    (setq result (strcat result (mcp-ent->summary ent)))
    (setq i (1+ i))
  )
  result
)

(defun mcp-ss->result-json (ss limit / total returned)
  "Envelope: {count = total matched, returned, truncated, entities[]}.
   limit nil or <= 0 means no cap. Building the JSON via strcat is O(n^2),
   so an unbounded query on a large drawing will blow the IPC timeout —
   the cap is reported, never silent."
  (setq total (if ss (sslength ss) 0))
  (setq returned (if (and limit (> limit 0) (< limit total)) limit total))
  (strcat "{\"count\":" (itoa total)
          ",\"returned\":" (itoa returned)
          ",\"truncated\":" (if (< returned total) "true" "false")
          ",\"entities\":[" (mcp-ss->entities-json ss returned) "]}")
)

(defun mcp-json-get-limit (params / v)
  "Read an integer limit param; nil when absent."
  (setq v (mcp-json-get-number params "limit"))
  (if v (fix v))
)

(defun mcp-build-filter (layer etype text / flt)
  "Build an ssget filter assoc-list from optional params (nil = match all)."
  (setq flt '())
  (if (and etype (> (strlen etype) 0)) (setq flt (cons (cons 0 etype) flt)))
  (if (and layer (> (strlen layer) 0)) (setq flt (cons (cons 8 layer) flt)))
  (if (and text  (> (strlen text)  0)) (setq flt (cons (cons 1 text)  flt)))
  flt
)

(defun mcp-cmd-entity-get-selection (params / ss)
  "Return the current implied (pickfirst/grip) selection set."
  (setq ss (ssget "_I"))
  (cons T (mcp-ss->result-json ss (mcp-json-get-limit params)))
)

(defun mcp-cmd-entity-query (params / layer etype text window-str mode flt pts p1 p2 ss)
  "Filter entities by layer/type/text and optional spatial window."
  (setq layer (mcp-json-get-string params "layer"))
  (setq etype (mcp-json-get-string params "etype"))
  (setq text (mcp-json-get-string params "text"))
  (setq window-str (mcp-json-get-string params "window_str"))
  (setq mode (mcp-json-get-string params "mode"))
  (setq flt (mcp-build-filter layer etype text))
  (if (and window-str (> (strlen window-str) 0))
    (progn
      (setq pts (mcp-split-string window-str ","))
      (setq p1 (list (atof (nth 0 pts)) (atof (nth 1 pts))))
      (setq p2 (list (atof (nth 2 pts)) (atof (nth 3 pts))))
      (if (and mode (= (strcase mode) "INSIDE"))
        (setq ss (if flt (ssget "_W" p1 p2 flt) (ssget "_W" p1 p2)))
        (setq ss (if flt (ssget "_C" p1 p2 flt) (ssget "_C" p1 p2)))
      )
    )
    (setq ss (if flt (ssget "_X" flt) (ssget "_X")))
  )
  (cons T (mcp-ss->result-json ss (mcp-json-get-limit params)))
)

;; --- Entity query: find_text (plain-text search incl. block attributes) ---

(defun mcp-mtext-plain (s / p)
  "Strip common MTEXT formatting so text reads (and matches) as plain text.
   Drops leading {\\f...; style runs and trailing braces, turns \\P into a
   space. Pragmatic, not a full MTEXT parser — stacked text (\\S) and
   inline overrides mid-string may survive."
  (if (null s) (setq s ""))
  (setq s (mcp-str-replace s "\\P" " "))
  (while (and (>= (strlen s) 2) (= (substr s 1 2) "{\\"))
    (setq p (vl-string-search ";" s))
    (if p (setq s (substr s (+ p 2))) (setq s (substr s 3)))
  )
  (while (and (> (strlen s) 0) (= (substr s (strlen s) 1) "}"))
    (setq s (substr s 1 (1- (strlen s))))
  )
  s
)

(defun mcp-text-of (ent / ed etype s)
  "Plain text of a text-bearing entity, nil for anything else."
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (setq s (cdr (assoc 1 ed)))
  (cond
    ((null s) nil)
    ((= etype "MTEXT") (mcp-mtext-plain s))
    (t s)
  )
)

(defun mcp-text-match (s pattern icase)
  "AutoCAD wildcard match (wcmatch), not regex. Optional case folding."
  (and s (> (strlen s) 0)
       (if icase
         (wcmatch (strcase s) (strcase pattern))
         (wcmatch s pattern)))
)

(defun mcp-ssget-scoped (types layer window-str mode / flt pts p1 p2)
  "ssget for the given DXF types, optionally scoped by layer and window."
  (setq flt (mcp-build-filter layer types nil))
  (if (and window-str (> (strlen window-str) 0))
    (progn
      (setq pts (mcp-split-string window-str ","))
      (setq p1 (list (atof (nth 0 pts)) (atof (nth 1 pts))))
      (setq p2 (list (atof (nth 2 pts)) (atof (nth 3 pts))))
      (if (and mode (= (strcase mode) "INSIDE"))
        (ssget "_W" p1 p2 flt)
        (ssget "_C" p1 p2 flt))
    )
    (ssget "_X" flt)
  )
)

(defun mcp-find->summary (rec / ent blk ed etype handle elayer p txt s tag)
  "JSON for one find_text hit. rec = (ename block-name-or-nil)."
  (setq ent (car rec) blk (cadr rec))
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (setq handle (cdr (assoc 5 ed)))
  (setq elayer (cdr (assoc 8 ed)))
  (setq s (strcat "{\"type\":\"" etype
                  "\",\"handle\":\"" handle
                  "\",\"layer\":\"" (mcp-escape-string elayer) "\""))
  (setq p (cdr (assoc 10 ed)))
  (if p
    (setq s (strcat s ",\"point\":[" (rtos (car p) 2 6) "," (rtos (cadr p) 2 6) "]"))
  )
  (setq txt (mcp-text-of ent))
  (if txt (setq s (strcat s ",\"text\":\"" (mcp-escape-string txt) "\"")))
  (setq tag (cdr (assoc 2 ed)))
  (if (and tag (= etype "ATTRIB"))
    (setq s (strcat s ",\"tag\":\"" (mcp-escape-string tag) "\""))
  )
  (if blk (setq s (strcat s ",\"block\":\"" (mcp-escape-string blk) "\"")))
  (strcat s "}")
)

(defun mcp-recs->result-json (recs limit / total returned body i)
  "Envelope for a list of find_text records (same shape as query)."
  (setq total (length recs))
  (setq returned (if (and limit (> limit 0) (< limit total)) limit total))
  (setq body "" i 0)
  (while (< i returned)
    (if (> (strlen body) 0) (setq body (strcat body ",")))
    (setq body (strcat body (mcp-find->summary (nth i recs))))
    (setq i (1+ i))
  )
  (strcat "{\"count\":" (itoa total)
          ",\"returned\":" (itoa returned)
          ",\"truncated\":" (if (< returned total) "true" "false")
          ",\"entities\":[" body "]}")
)

(defun mcp-cmd-entity-find-text (params / pattern layer window-str mode limit icase
                                 inc-attribs ss i n ent ed recs blkname sub sed setype)
  "Search TEXT/MTEXT/ATTDEF and (optionally) block ATTRIBs for a wildcard."
  (setq pattern (mcp-json-get-string params "pattern"))
  (if (or (null pattern) (= pattern ""))
    (cons nil "pattern parameter required")
    (progn
      (setq layer (mcp-json-get-string params "layer"))
      (setq window-str (mcp-json-get-string params "window_str"))
      (setq mode (mcp-json-get-string params "mode"))
      (setq limit (mcp-json-get-limit params))
      (setq icase (mcp-json-get-number params "ignore_case"))
      (setq icase (if (null icase) T (/= icase 0)))
      (setq inc-attribs (mcp-json-get-number params "include_attribs"))
      (setq inc-attribs (if (null inc-attribs) T (/= inc-attribs 0)))
      (setq recs '())

      ;; Pass A — standalone text entities
      (setq ss (mcp-ssget-scoped "TEXT,MTEXT,ATTDEF" layer window-str mode))
      (if ss
        (progn
          (setq i 0 n (sslength ss))
          (while (< i n)
            (setq ent (ssname ss i))
            (if (mcp-text-match (mcp-text-of ent) pattern icase)
              (setq recs (cons (list ent nil) recs))
            )
            (setq i (1+ i))
          )
        )
      )

      ;; Pass B — ATTRIBs, which ssget cannot select directly: walk each
      ;; INSERT's sub-entities. Group 66 = "attributes follow"; without that
      ;; guard entnext would walk on into unrelated database entities.
      ;; Note: the layer filter matches the INSERT, not the ATTRIB itself.
      (if inc-attribs
        (progn
          (setq ss (mcp-ssget-scoped "INSERT" layer window-str mode))
          (if ss
            (progn
              (setq i 0 n (sslength ss))
              (while (< i n)
                (setq ent (ssname ss i))
                (setq ed (entget ent))
                (if (and (assoc 66 ed) (= 1 (cdr (assoc 66 ed))))
                  (progn
                    (setq blkname (cdr (assoc 2 ed)))
                    (setq sub (entnext ent))
                    (while sub
                      (setq sed (entget sub))
                      (setq setype (cdr (assoc 0 sed)))
                      (if (= setype "SEQEND")
                        (setq sub nil)
                        (progn
                          (if (and (= setype "ATTRIB")
                                   (mcp-text-match (mcp-text-of sub) pattern icase))
                            (setq recs (cons (list sub blkname) recs))
                          )
                          (setq sub (entnext sub))
                        )
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
            )
          )
        )
      )
      (cons T (mcp-recs->result-json (reverse recs) limit))
    )
  )
)

;; --- Raw geometry export (normalisation happens Python-side) ---

(defun mcp-num (v) (rtos v 2 6))

(defun mcp-deg (a)
  "DXF angles come back from entget in radians; emit degrees."
  (rtos (/ (* a 180.0) pi) 2 6)
)

(defun mcp-verts->json (pts / s p)
  (setq s "")
  (foreach p pts
    (if (> (strlen s) 0) (setq s (strcat s ",")))
    (setq s (strcat s "[" (mcp-num (car p)) "," (mcp-num (cadr p)) "]"))
  )
  (strcat "[" s "]")
)

(defun mcp-ent->geom-json (ent / ed etype handle elayer s p q r)
  "Raw geometry for one entity. Only the shapes worth normalising later —
   anything else reports its type so the caller knows it was skipped."
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (setq handle (cdr (assoc 5 ed)))
  (setq elayer (cdr (assoc 8 ed)))
  (setq s (strcat "{\"type\":\"" etype
                  "\",\"handle\":\"" handle
                  "\",\"layer\":\"" (mcp-escape-string elayer) "\""))
  (cond
    ((= etype "LINE")
     (setq p (cdr (assoc 10 ed)) q (cdr (assoc 11 ed)))
     (setq s (strcat s ",\"start\":[" (mcp-num (car p)) "," (mcp-num (cadr p)) "]"
                       ",\"end\":[" (mcp-num (car q)) "," (mcp-num (cadr q)) "]")))
    ((= etype "LWPOLYLINE")
     (setq s (strcat s ",\"closed\":"
                     (if (and (assoc 70 ed) (= 1 (logand 1 (cdr (assoc 70 ed))))) "true" "false")
                     ",\"verts\":" (mcp-verts->json (mcp-poly-vertices ent)))))
    ((= etype "CIRCLE")
     (setq p (cdr (assoc 10 ed)))
     (setq s (strcat s ",\"center\":[" (mcp-num (car p)) "," (mcp-num (cadr p)) "]"
                       ",\"radius\":" (mcp-num (cdr (assoc 40 ed))))))
    ((= etype "ARC")
     (setq p (cdr (assoc 10 ed)))
     (setq s (strcat s ",\"center\":[" (mcp-num (car p)) "," (mcp-num (cadr p)) "]"
                       ",\"radius\":" (mcp-num (cdr (assoc 40 ed)))
                       ",\"start_angle\":" (mcp-deg (cdr (assoc 50 ed)))
                       ",\"end_angle\":" (mcp-deg (cdr (assoc 51 ed))))))
    ((= etype "INSERT")
     (setq p (cdr (assoc 10 ed)))
     (setq r (cdr (assoc 50 ed)))
     (setq s (strcat s ",\"name\":\"" (mcp-escape-string (cdr (assoc 2 ed))) "\""
                       ",\"insert\":[" (mcp-num (car p)) "," (mcp-num (cadr p)) "]"
                       ",\"rotation\":" (mcp-deg (if r r 0.0))
                       ",\"scale\":[" (mcp-num (cdr (assoc 41 ed))) ","
                                      (mcp-num (cdr (assoc 42 ed))) "]")))
  )
  (strcat s "}")
)

(defun mcp-cmd-entity-export-geometry (params / layer etype window-str mode limit
                                       ss total returned body i)
  "Dump raw geometry by layer/type. The rectangle and centreline maths runs
   Python-side: AutoLISP has no arrays and its strcat is O(n^2), which makes
   it the wrong place for geometry."
  (setq layer (mcp-json-get-string params "layer"))
  (setq etype (mcp-json-get-string params "etype"))
  (if (or (null etype) (= etype "")) (setq etype "LINE,LWPOLYLINE,CIRCLE,ARC,INSERT"))
  (setq window-str (mcp-json-get-string params "window_str"))
  (setq mode (mcp-json-get-string params "mode"))
  (setq limit (mcp-json-get-limit params))
  (setq ss (mcp-ssget-scoped etype layer window-str mode))
  (setq total (if ss (sslength ss) 0))
  (setq returned (if (and limit (> limit 0) (< limit total)) limit total))
  (setq body "" i 0)
  (while (< i returned)
    (if (> (strlen body) 0) (setq body (strcat body ",")))
    (setq body (strcat body (mcp-ent->geom-json (ssname ss i))))
    (setq i (1+ i))
  )
  (cons T (strcat "{\"count\":" (itoa total)
                  ",\"returned\":" (itoa returned)
                  ",\"truncated\":" (if (< returned total) "true" "false")
                  ",\"entities\":[" body "]}"))
)

;; --- Drawing units / insertion base / UCS ---

(defun mcp-pt->json (p)
  "JSON array for a 2D or 3D point sysvar, null when absent."
  (if p
    (strcat "[" (rtos (car p) 2 6) "," (rtos (cadr p) 2 6)
            (if (caddr p) (strcat "," (rtos (caddr p) 2 6)) "") "]")
    "null"
  )
)

(defun mcp-cmd-drawing-get-units-and-base ( / ucsname)
  "Report the values that decide whether a CAD file lands correctly when
   linked or inserted: units, insertion base, and the active UCS."
  (setq ucsname (getvar "UCSNAME"))
  (if (/= (type ucsname) 'STR) (setq ucsname ""))
  (cons T (strcat
    "{\"dwgname\":\"" (mcp-escape-string (getvar "DWGNAME"))
    "\",\"insunits\":" (itoa (getvar "INSUNITS"))
    ",\"measurement\":" (itoa (getvar "MEASUREMENT"))
    ",\"lunits\":" (itoa (getvar "LUNITS"))
    ",\"luprec\":" (itoa (getvar "LUPREC"))
    ",\"aunits\":" (itoa (getvar "AUNITS"))
    ",\"auprec\":" (itoa (getvar "AUPREC"))
    ",\"insbase\":" (mcp-pt->json (getvar "INSBASE"))
    ",\"ucsname\":\"" (mcp-escape-string ucsname)
    "\",\"ucsorg\":" (mcp-pt->json (getvar "UCSORG"))
    ",\"ucsxdir\":" (mcp-pt->json (getvar "UCSXDIR"))
    ",\"ucsydir\":" (mcp-pt->json (getvar "UCSYDIR"))
    ",\"extmin\":" (mcp-pt->json (getvar "EXTMIN"))
    ",\"extmax\":" (mcp-pt->json (getvar "EXTMAX"))
    ",\"limmin\":" (mcp-pt->json (getvar "LIMMIN"))
    ",\"limmax\":" (mcp-pt->json (getvar "LIMMAX"))
    ",\"ctab\":\"" (mcp-escape-string (getvar "CTAB"))
    "\",\"tilemode\":" (itoa (getvar "TILEMODE"))
    "}"))
)

(defun mcp-cmd-drawing-set-insertion-base (params / x y z)
  "Set INSBASE for the current space."
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq z (mcp-json-get-number params "z"))
  (if (null z) (setq z 0.0))
  (if (and x y)
    (progn
      (setvar "INSBASE" (list x y z))
      (cons T (strcat "{\"insbase\":" (mcp-pt->json (getvar "INSBASE")) "}"))
    )
    (cons nil "x and y are required")
  )
)

;; --- Annotation find/replace ---

(defun mcp-str-replace-ci (s old new / out lu pos)
  "Case-insensitive replace-all. Splices from the original string so the
   untouched parts keep their original casing."
  (cond
    ((null s) "")
    ((or (null old) (= old "")) s)
    (t
     (progn
       (if (null new) (setq new ""))
       (setq out "" lu (strcase old))
       (while (setq pos (vl-string-search lu (strcase s)))
         (setq out (strcat out (substr s 1 pos) new))
         (setq s (substr s (+ pos 1 (strlen old))))
       )
       (strcat out s)
     )
    )
  )
)

(defun mcp-ent-replace-text (ent find repl icase dry / ed etype newed pair k v nv changed)
  "Substring-replace inside one text entity. Returns T if it changed.

   Runs over the raw DXF text so MTEXT formatting survives the edit.
   Group 1 is the text on TEXT/MTEXT/ATTRIB/ATTDEF; group 3 is only text on
   MTEXT (it holds the earlier chunks when the string exceeds 250 chars) —
   on ATTDEF group 3 is the prompt, which must not be touched."
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (setq newed '() changed nil)
  (foreach pair ed
    (setq k (car pair) v (cdr pair))
    (if (and (= (type v) 'STR)
             (or (= k 1) (and (= k 3) (= etype "MTEXT"))))
      (progn
        (setq nv (if icase (mcp-str-replace-ci v find repl) (mcp-str-replace v find repl)))
        (if (/= nv v) (setq changed T))
        (setq newed (cons (cons k nv) newed))
      )
      (setq newed (cons pair newed))
    )
  )
  (setq newed (reverse newed))
  (if (and changed (not dry))
    (progn (entmod newed) (entupd ent))
  )
  changed
)

(defun mcp-ent-raw-text (ent / ed etype out)
  "Raw group-1 (+ group-3 chunks for MTEXT), as stored."
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (setq out "")
  (foreach pair ed
    (if (and (= (type (cdr pair)) 'STR)
             (or (= (car pair) 1) (and (= (car pair) 3) (= etype "MTEXT"))))
      (setq out (strcat out (cdr pair)))
    )
  )
  out
)

(defun mcp-cmd-annotation-find-replace (params / find repl layer window-str mode limit
                                        icase inc-attribs dry ss i n ent ed blkname sub
                                        sed setype recs before after changed total body
                                        returned)
  "Substring find/replace across drawing text, optionally including ATTRIBs."
  (setq find (mcp-json-get-string params "find"))
  (if (or (null find) (= find ""))
    (cons nil "find parameter required")
    (progn
      (setq repl (mcp-json-get-string params "replace"))
      (if (null repl) (setq repl ""))
      (setq layer (mcp-json-get-string params "layer"))
      (setq window-str (mcp-json-get-string params "window_str"))
      (setq mode (mcp-json-get-string params "mode"))
      (setq limit (mcp-json-get-limit params))
      (setq icase (mcp-json-get-number params "ignore_case"))
      (setq icase (if (null icase) T (/= icase 0)))
      (setq inc-attribs (mcp-json-get-number params "include_attribs"))
      (setq inc-attribs (if (null inc-attribs) T (/= inc-attribs 0)))
      (setq dry (mcp-json-get-number params "dry_run"))
      (setq dry (if (null dry) nil (/= dry 0)))
      (setq recs '())

      ;; Pass A — standalone text
      (setq ss (mcp-ssget-scoped "TEXT,MTEXT,ATTDEF" layer window-str mode))
      (if ss
        (progn
          (setq i 0 n (sslength ss))
          (while (< i n)
            (setq ent (ssname ss i))
            (setq before (mcp-ent-raw-text ent))
            (if (mcp-ent-replace-text ent find repl icase dry)
              (setq recs (cons (list ent before) recs))
            )
            (setq i (1+ i))
          )
        )
      )

      ;; Pass B — ATTRIBs inside attributed INSERTs (ssget cannot select them)
      (if inc-attribs
        (progn
          (setq ss (mcp-ssget-scoped "INSERT" layer window-str mode))
          (if ss
            (progn
              (setq i 0 n (sslength ss))
              (while (< i n)
                (setq ent (ssname ss i))
                (setq ed (entget ent))
                (if (and (assoc 66 ed) (= 1 (cdr (assoc 66 ed))))
                  (progn
                    (setq sub (entnext ent))
                    (while sub
                      (setq sed (entget sub))
                      (setq setype (cdr (assoc 0 sed)))
                      (if (= setype "SEQEND")
                        (setq sub nil)
                        (progn
                          (if (= setype "ATTRIB")
                            (progn
                              (setq before (mcp-ent-raw-text sub))
                              (if (mcp-ent-replace-text sub find repl icase dry)
                                (setq recs (cons (list sub before) recs))
                              )
                            )
                          )
                          (setq sub (entnext sub))
                        )
                      )
                    )
                    (if (not dry) (entupd ent))
                  )
                )
                (setq i (1+ i))
              )
            )
          )
        )
      )

      (setq recs (reverse recs))
      (setq total (length recs))
      (setq returned (if (and limit (> limit 0) (< limit total)) limit total))
      (setq body "" i 0)
      (while (< i returned)
        (setq ent (car (nth i recs)))
        (setq before (cadr (nth i recs)))
        ;; in dry mode nothing was written, so show what the edit would produce
        (setq after (if dry
                      (if icase (mcp-str-replace-ci before find repl)
                                (mcp-str-replace before find repl))
                      (mcp-ent-raw-text ent)))
        (setq ed (entget ent))
        (if (> (strlen body) 0) (setq body (strcat body ",")))
        (setq body (strcat body
          "{\"handle\":\"" (cdr (assoc 5 ed))
          "\",\"type\":\"" (cdr (assoc 0 ed))
          "\",\"layer\":\"" (mcp-escape-string (cdr (assoc 8 ed)))
          "\",\"before\":\"" (mcp-escape-string before)
          "\",\"after\":\"" (mcp-escape-string after) "\"}"))
        (setq i (1+ i))
      )
      (cons T (strcat "{\"dry_run\":" (if dry "true" "false")
                      ",\"count\":" (itoa total)
                      ",\"returned\":" (itoa returned)
                      ",\"truncated\":" (if (< returned total) "true" "false")
                      ",\"entities\":[" body "]}"))
    )
  )
)

;; --- Layer translate (mapping table: vendor layers -> company standard) ---

(defun mcp-layer-move-ss (ss to / i n ent ed cnt)
  "Move every entity of a selection set onto layer `to`.
   entmod, not CHPROP: the low-level API is unaffected by layer locking
   and lets us count exactly what moved. Returns the number moved.
   Note: ssget _X does not reach entities inside block definitions, so
   geometry within a block keeps its own layer."
  (setq cnt 0)
  (if ss
    (progn
      (setq i 0 n (sslength ss))
      (while (< i n)
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        (if (entmod (subst (cons 8 to) (assoc 8 ed) ed))
          (setq cnt (1+ cnt))
        )
        (setq i (1+ i))
      )
    )
  )
  cnt
)

(defun mcp-layer-exists-matching (pat / ld found)
  "Is any layer in the table matching pat (wildcards ok) still present?
   tblsearch alone cannot answer this: a wildcard never matches a literal
   table entry, so it would report 'gone' without checking anything."
  (setq found nil)
  (setq ld (tblnext "LAYER" T))
  (while (and ld (not found))
    (if (wcmatch (strcase (cdr (assoc 2 ld))) (strcase pat)) (setq found T))
    (setq ld (tblnext "LAYER"))
  )
  found
)

(defun mcp-cmd-layer-translate (params / map-str dry purge pairs pair parts from to
                                results total cnt existed purged created ss)
  "Rename/merge layers from a mapping table. map_str = \"OLD>NEW;OLD2>NEW2\"
   (';' and '>' are illegal in AutoCAD layer names, so they are safe
   delimiters)."
  (setq map-str (mcp-json-get-string params "map_str"))
  (if (or (null map-str) (= map-str ""))
    (cons nil "map parameter required")
    (progn
      (setq dry (mcp-json-get-number params "dry_run"))
      (setq dry (if (null dry) nil (/= dry 0)))
      (setq purge (mcp-json-get-number params "purge"))
      (setq purge (if (null purge) T (/= purge 0)))
      (setq results "" total 0)
      (foreach pair (mcp-split-string map-str ";")
        (setq parts (mcp-split-string pair ">"))
        (setq from (nth 0 parts))
        (setq to (nth 1 parts))
        (if (and from to (> (strlen from) 0) (> (strlen to) 0))
          (progn
            (setq existed (if (tblsearch "LAYER" to) T nil))
            (setq ss (ssget "_X" (list (cons 8 from))))
            (setq cnt (if ss (sslength ss) 0))
            (setq created nil purged nil)
            ;; nothing matched -> do nothing at all; creating the target for a
            ;; no-op translate would be an unasked-for side effect
            (if (and (not dry) (> cnt 0))
              (progn
                (if (not existed)
                  (progn (ensure_layer_exists to "white" "CONTINUOUS") (setq created T))
                )
                (setq cnt (mcp-layer-move-ss ss to))
                ;; purge only once the source is empty; it also fails harmlessly
                ;; while a block definition still references the layer
                (if (and purge (not (wcmatch (strcase to) (strcase from))))
                  (progn
                    (if (wcmatch (strcase (getvar "CLAYER")) (strcase from))
                      (setvar "CLAYER" to)
                    )
                    (command "_.-PURGE" "_LA" from "_N")
                    (setq purged (not (mcp-layer-exists-matching from)))
                  )
                )
              )
            )
            (setq total (+ total cnt))
            (if (> (strlen results) 0) (setq results (strcat results ",")))
            (setq results (strcat results
              "{\"from\":\"" (mcp-escape-string from)
              "\",\"to\":\"" (mcp-escape-string to)
              "\",\"entities\":" (itoa cnt)
              ",\"target_created\":" (if created "true" "false")
              ",\"source_purged\":" (if purged "true" "false") "}"))
          )
        )
      )
      (cons T (strcat "{\"dry_run\":" (if dry "true" "false")
                      ",\"total_entities\":" (itoa total)
                      ",\"results\":[" results "]}"))
    )
  )
)

;; --- Block attribute extraction ---

(defun mcp-ssget-inserts (blk layer window-str mode / flt pts p1 p2)
  "ssget for INSERTs, optionally scoped by block name / layer / window.
   The has-attributes test (group 66) is done per entity in the caller —
   group 66 is not reliably filterable through ssget."
  (setq flt (list (cons 0 "INSERT")))
  (if (and blk (> (strlen blk) 0)) (setq flt (cons (cons 2 blk) flt)))
  (if (and layer (> (strlen layer) 0)) (setq flt (cons (cons 8 layer) flt)))
  (if (and window-str (> (strlen window-str) 0))
    (progn
      (setq pts (mcp-split-string window-str ","))
      (setq p1 (list (atof (nth 0 pts)) (atof (nth 1 pts))))
      (setq p2 (list (atof (nth 2 pts)) (atof (nth 3 pts))))
      (if (and mode (= (strcase mode) "INSIDE"))
        (ssget "_W" p1 p2 flt)
        (ssget "_C" p1 p2 flt))
    )
    (ssget "_X" flt)
  )
)

(defun mcp-insert->attrs-json (ent / ed blk handle elayer p sub sed setype attrs s v)
  "JSON for one attributed INSERT: block, handle, layer, point, attributes{}."
  (setq ed (entget ent))
  (setq blk (cdr (assoc 2 ed)))
  (setq handle (cdr (assoc 5 ed)))
  (setq elayer (cdr (assoc 8 ed)))
  (setq p (cdr (assoc 10 ed)))
  (setq attrs "")
  (setq sub (entnext ent))
  (while sub
    (setq sed (entget sub))
    (setq setype (cdr (assoc 0 sed)))
    (if (= setype "SEQEND")
      (setq sub nil)
      (progn
        (if (= setype "ATTRIB")
          (progn
            (setq v (mcp-text-of sub))
            (if (> (strlen attrs) 0) (setq attrs (strcat attrs ",")))
            (setq attrs (strcat attrs
                                "\"" (mcp-escape-string (cdr (assoc 2 sed))) "\":"
                                "\"" (mcp-escape-string v) "\""))
          )
        )
        (setq sub (entnext sub))
      )
    )
  )
  (setq s (strcat "{\"block\":\"" (mcp-escape-string blk)
                  "\",\"handle\":\"" handle
                  "\",\"layer\":\"" (mcp-escape-string elayer) "\""))
  (if p
    (setq s (strcat s ",\"point\":[" (rtos (car p) 2 6) "," (rtos (cadr p) 2 6) "]"))
  )
  (strcat s ",\"attributes\":{" attrs "}}")
)

(defun mcp-cmd-block-extract-attributes (params / blk layer window-str mode limit
                                         ss i n ent ed recs total returned body)
  "Collect every attributed INSERT with its tag/value pairs."
  (setq blk (mcp-json-get-string params "block"))
  (setq layer (mcp-json-get-string params "layer"))
  (setq window-str (mcp-json-get-string params "window_str"))
  (setq mode (mcp-json-get-string params "mode"))
  (setq limit (mcp-json-get-limit params))
  (setq ss (mcp-ssget-inserts blk layer window-str mode))
  (setq recs '())
  (if ss
    (progn
      (setq i 0 n (sslength ss))
      (while (< i n)
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        (if (and (assoc 66 ed) (= 1 (cdr (assoc 66 ed))))
          (setq recs (cons ent recs))
        )
        (setq i (1+ i))
      )
    )
  )
  (setq recs (reverse recs))
  (setq total (length recs))
  (setq returned (if (and limit (> limit 0) (< limit total)) limit total))
  (setq body "" i 0)
  (while (< i returned)
    (if (> (strlen body) 0) (setq body (strcat body ",")))
    (setq body (strcat body (mcp-insert->attrs-json (nth i recs))))
    (setq i (1+ i))
  )
  (cons T (strcat "{\"count\":" (itoa total)
                  ",\"returned\":" (itoa returned)
                  ",\"truncated\":" (if (< returned total) "true" "false")
                  ",\"blocks\":[" body "]}"))
)

;; --- Entity modification commands ---

(defun mcp-cmd-entity-copy (params / entity-id dx dy ent new-handle)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (setq dx (mcp-json-get-number params "dx"))
  (setq dy (mcp-json-get-number params "dy"))
  (if (= entity-id "last") (setq ent (entlast)) (setq ent (handent entity-id)))
  (if ent
    (progn
      (command "_.COPY" ent "" '(0 0 0) (list dx dy 0))
      (setq new-handle (cdr (assoc 5 (entget (entlast)))))
      (cons T (strcat "{\"handle\":\"" new-handle "\"}")))
    (cons nil "Entity not found")
  )
)

(defun mcp-cmd-entity-rotate (params / entity-id cx cy angle ent)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (setq cx (mcp-json-get-number params "cx"))
  (setq cy (mcp-json-get-number params "cy"))
  (setq angle (mcp-json-get-number params "angle"))
  (if (= entity-id "last") (setq ent (entlast)) (setq ent (handent entity-id)))
  (if ent
    (progn (command "_.ROTATE" ent "" (list cx cy 0) angle) (cons T "\"rotated\""))
    (cons nil "Entity not found")
  )
)

(defun mcp-cmd-entity-scale (params / entity-id cx cy factor ent)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (setq cx (mcp-json-get-number params "cx"))
  (setq cy (mcp-json-get-number params "cy"))
  (setq factor (mcp-json-get-number params "factor"))
  (if (= entity-id "last") (setq ent (entlast)) (setq ent (handent entity-id)))
  (if ent
    (progn (command "_.SCALE" ent "" (list cx cy 0) factor) (cons T "\"scaled\""))
    (cons nil "Entity not found")
  )
)

(defun mcp-cmd-entity-mirror (params / entity-id x1 y1 x2 y2 ent new-handle)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (setq x1 (mcp-json-get-number params "x1"))
  (setq y1 (mcp-json-get-number params "y1"))
  (setq x2 (mcp-json-get-number params "x2"))
  (setq y2 (mcp-json-get-number params "y2"))
  (if (= entity-id "last") (setq ent (entlast)) (setq ent (handent entity-id)))
  (if ent
    (progn
      (command "_.MIRROR" ent "" (list x1 y1 0) (list x2 y2 0) "_N")
      (setq new-handle (cdr (assoc 5 (entget (entlast)))))
      (cons T (strcat "{\"handle\":\"" new-handle "\"}")))
    (cons nil "Entity not found")
  )
)

(defun mcp-cmd-entity-offset (params / entity-id distance ent new-handle)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (setq distance (mcp-json-get-number params "distance"))
  (if (= entity-id "last") (setq ent (entlast)) (setq ent (handent entity-id)))
  (if ent
    (progn
      (command "_.OFFSET" distance ent (list 0 0 0) "")
      (setq new-handle (cdr (assoc 5 (entget (entlast)))))
      (cons T (strcat "{\"handle\":\"" new-handle "\"}")))
    (cons nil "Entity not found")
  )
)

(defun mcp-cmd-entity-array (params / entity-id rows cols row-dist col-dist ent)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (setq rows (fix (mcp-json-get-number params "rows")))
  (setq cols (fix (mcp-json-get-number params "cols")))
  (setq row-dist (mcp-json-get-number params "row_dist"))
  (setq col-dist (mcp-json-get-number params "col_dist"))
  (if (= entity-id "last") (setq ent (entlast)) (setq ent (handent entity-id)))
  (if ent
    (progn
      (command "_.ARRAY" ent "" "_R" rows cols row-dist col-dist)
      (cons T (strcat "{\"rows\":" (itoa rows) ",\"cols\":" (itoa cols) "}")))
    (cons nil "Entity not found")
  )
)

(defun mcp-cmd-entity-fillet (params / id1 id2 radius ent1 ent2)
  (setq id1 (mcp-json-get-string params "id1"))
  (setq id2 (mcp-json-get-string params "id2"))
  (setq radius (mcp-json-get-number params "radius"))
  (setq ent1 (handent id1))
  (setq ent2 (handent id2))
  (if (and ent1 ent2)
    (progn
      (command "_.FILLET" "_R" radius)
      (command "_.FILLET" ent1 ent2)
      (cons T "\"filleted\""))
    (cons nil "One or both entities not found")
  )
)

(defun mcp-cmd-entity-chamfer (params / id1 id2 dist1 dist2 ent1 ent2)
  (setq id1 (mcp-json-get-string params "id1"))
  (setq id2 (mcp-json-get-string params "id2"))
  (setq dist1 (mcp-json-get-number params "dist1"))
  (setq dist2 (mcp-json-get-number params "dist2"))
  (setq ent1 (handent id1))
  (setq ent2 (handent id2))
  (if (and ent1 ent2)
    (progn
      (command "_.CHAMFER" "_D" dist1 dist2)
      (command "_.CHAMFER" ent1 ent2)
      (cons T "\"chamfered\""))
    (cons nil "One or both entities not found")
  )
)

;; --- Layer operations ---

(defun mcp-cmd-layer-set-properties (params / name color linetype lineweight)
  (setq name (mcp-json-get-string params "name"))
  (setq color (mcp-json-get-string params "color"))
  (setq linetype (mcp-json-get-string params "linetype"))
  (setq lineweight (mcp-json-get-string params "lineweight"))
  (if color (command "_.-LAYER" "_COLOR" color name ""))
  (if linetype (command "_.-LAYER" "_LTYPE" linetype name ""))
  (if lineweight (command "_.-LAYER" "_LWEIGHT" lineweight name ""))
  (cons T (strcat "{\"name\":\"" name "\"}"))
)

(defun mcp-cmd-layer-freeze (params / name)
  (setq name (mcp-json-get-string params "name"))
  (command "_.-LAYER" "_FREEZE" name "")
  (cons T (strcat "{\"name\":\"" name "\",\"frozen\":true}"))
)

(defun mcp-cmd-layer-thaw (params / name)
  (setq name (mcp-json-get-string params "name"))
  (command "_.-LAYER" "_THAW" name "")
  (cons T (strcat "{\"name\":\"" name "\",\"frozen\":false}"))
)

(defun mcp-cmd-layer-lock (params / name)
  (setq name (mcp-json-get-string params "name"))
  (command "_.-LAYER" "_LOCK" name "")
  (cons T (strcat "{\"name\":\"" name "\",\"locked\":true}"))
)

(defun mcp-cmd-layer-unlock (params / name)
  (setq name (mcp-json-get-string params "name"))
  (command "_.-LAYER" "_UNLOCK" name "")
  (cons T (strcat "{\"name\":\"" name "\",\"locked\":false}"))
)

;; --- Block operations (insert-with-attributes, get-attributes, update-attribute) ---

(defun mcp-cmd-block-insert-with-attribs (params / name x y scale rotation attributes ent)
  (setq name (mcp-json-get-string params "name"))
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq scale (mcp-json-get-number params "scale"))
  (setq rotation (mcp-json-get-number params "rotation"))
  (if (not scale) (setq scale 1.0))
  (if (not rotation) (setq rotation 0.0))
  (if (tblsearch "BLOCK" name)
    (progn
      ;; Insert with ATTREQ=1 to fill attributes
      (command "_.INSERT" name (list x y 0.0) scale scale rotation)
      ;; Note: attribute values are applied separately via update-attribute
      (cons T (strcat "{\"entity_type\":\"INSERT\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}")))
    (cons nil (strcat "Block '" name "' not found"))
  )
)

(defun mcp-cmd-block-get-attributes (params / entity-id ent sub-ent ent-data attribs)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (if (= entity-id "last") (setq ent (entlast)) (setq ent (handent entity-id)))
  (if (not ent)
    (cons nil "Entity not found")
    (progn
      (setq attribs "" sub-ent (entnext ent))
      (while sub-ent
        (setq ent-data (entget sub-ent))
        (if (= (cdr (assoc 0 ent-data)) "ATTRIB")
          (progn
            (if (> (strlen attribs) 0) (setq attribs (strcat attribs ",")))
            (setq attribs (strcat attribs "\"" (cdr (assoc 2 ent-data)) "\":\"" (mcp-escape-string (cdr (assoc 1 ent-data))) "\""))
          )
        )
        (if (= (cdr (assoc 0 ent-data)) "SEQEND")
          (setq sub-ent nil)
          (setq sub-ent (entnext sub-ent))
        )
      )
      (cons T (strcat "{\"attributes\":{" attribs "}}"))
    )
  )
)

(defun mcp-cmd-block-update-attribute (params / entity-id tag value ent)
  (setq entity-id (mcp-json-get-string params "entity_id"))
  (setq tag (mcp-json-get-string params "tag"))
  (setq value (mcp-json-get-string params "value"))
  (if (= entity-id "last") (setq ent (entlast)) (setq ent (handent entity-id)))
  (if (not ent)
    (cons nil "Entity not found")
    (progn
      (if c:update-block-attribute
        (progn (c:update-block-attribute ent tag value)
               (cons T (strcat "{\"tag\":\"" tag "\",\"value\":\"" (mcp-escape-string value) "\"}")))
        ;; Inline fallback if attribute_tools.lsp not loaded
        (progn
          (set_attribute_value ent tag value)
          (cons T (strcat "{\"tag\":\"" tag "\",\"value\":\"" (mcp-escape-string value) "\"}")))
      )
    )
  )
)

;; --- Annotation commands ---

(defun mcp-cmd-create-dimension-linear (params / x1 y1 x2 y2 dim-x dim-y)
  (setq x1 (mcp-json-get-number params "x1"))
  (setq y1 (mcp-json-get-number params "y1"))
  (setq x2 (mcp-json-get-number params "x2"))
  (setq y2 (mcp-json-get-number params "y2"))
  (setq dim-x (mcp-json-get-number params "dim_x"))
  (setq dim-y (mcp-json-get-number params "dim_y"))
  (command "_.DIMLINEAR" (list x1 y1 0) (list x2 y2 0) (list dim-x dim-y 0))
  (cons T "{\"entity_type\":\"DIMENSION\"}")
)

(defun mcp-cmd-create-dimension-aligned (params / x1 y1 x2 y2 offset)
  (setq x1 (mcp-json-get-number params "x1"))
  (setq y1 (mcp-json-get-number params "y1"))
  (setq x2 (mcp-json-get-number params "x2"))
  (setq y2 (mcp-json-get-number params "y2"))
  (setq offset (mcp-json-get-number params "offset"))
  ;; Place dimension line at offset distance
  (command "_.DIMALIGNED" (list x1 y1 0) (list x2 y2 0)
    (list (+ (/ (+ x1 x2) 2.0) offset) (+ (/ (+ y1 y2) 2.0) offset) 0))
  (cons T "{\"entity_type\":\"DIMENSION\"}")
)

(defun mcp-cmd-create-dimension-angular (params / cx cy x1 y1 x2 y2)
  (setq cx (mcp-json-get-number params "cx"))
  (setq cy (mcp-json-get-number params "cy"))
  (setq x1 (mcp-json-get-number params "x1"))
  (setq y1 (mcp-json-get-number params "y1"))
  (setq x2 (mcp-json-get-number params "x2"))
  (setq y2 (mcp-json-get-number params "y2"))
  (command "_.DIMANGULAR" (list cx cy 0) (list x1 y1 0) (list x2 y2 0) "")
  (cons T "{\"entity_type\":\"DIMENSION\"}")
)

(defun mcp-cmd-create-dimension-radius (params / cx cy radius angle px py)
  (setq cx (mcp-json-get-number params "cx"))
  (setq cy (mcp-json-get-number params "cy"))
  (setq radius (mcp-json-get-number params "radius"))
  (setq angle (mcp-json-get-number params "angle"))
  ;; Need a circle/arc entity first, use entity at center
  (setq px (+ cx (* radius (cos (* angle (/ pi 180.0))))))
  (setq py (+ cy (* radius (sin (* angle (/ pi 180.0))))))
  (command "_.DIMRADIUS" (list px py 0) "")
  (cons T "{\"entity_type\":\"DIMENSION\"}")
)

(defun mcp-cmd-create-leader (params / text pts-str pairs pt-str)
  (setq text (mcp-json-get-string params "text"))
  (setq pts-str (mcp-json-get-string params "points_str"))
  (if (not pts-str)
    (cons nil "points_str required (format: x1,y1;x2,y2;...)")
    (progn
      (command "_.LEADER")
      (setq pairs (mcp-split-string pts-str ";"))
      (foreach pt-str pairs
        (command (list (atof (car (mcp-split-string pt-str ",")))
                       (atof (cadr (mcp-split-string pt-str ","))) 0))
      )
      (command "" text "")
      (cons T "{\"entity_type\":\"LEADER\"}")
    )
  )
)

;; --- Drawing management ---

(defun mcp-cmd-drawing-get-variables (params / names-str result var-list var-name var-val first-var)
  (setq names-str (mcp-json-get-string params "names_str"))
  (if (or (not names-str) (= names-str ""))
    ;; Default set when no specific names requested
    (progn
      (setq result "{")
      (setq result (strcat result "\"ACADVER\":\"" (getvar "ACADVER") "\""))
      (setq result (strcat result ",\"DWGNAME\":\"" (mcp-escape-string (getvar "DWGNAME")) "\""))
      (setq result (strcat result ",\"CLAYER\":\"" (getvar "CLAYER") "\""))
      (setq result (strcat result "}"))
      (cons T result)
    )
    ;; Parse semicolon-delimited variable names
    (progn
      (setq var-list (mcp-split-string names-str ";"))
      (setq result "{" first-var T)
      (foreach var-name var-list
        (setq var-val (getvar var-name))
        (if (not first-var) (setq result (strcat result ",")))
        (setq first-var nil)
        (if (not var-val)
          (setq result (strcat result "\"" var-name "\":null"))
          (cond
            ((= (type var-val) 'STR)
             (setq result (strcat result "\"" var-name "\":\"" (mcp-escape-string var-val) "\"")))
            ((= (type var-val) 'INT)
             (setq result (strcat result "\"" var-name "\":" (itoa var-val))))
            ((= (type var-val) 'REAL)
             (setq result (strcat result "\"" var-name "\":" (rtos var-val 2 6))))
            (t
             (setq result (strcat result "\"" var-name "\":\"" (mcp-escape-string (vl-princ-to-string var-val)) "\"")))
          )
        )
      )
      (setq result (strcat result "}"))
      (cons T result)
    )
  )
)

(defun mcp-cmd-drawing-plot-pdf (params / path)
  (setq path (mcp-json-get-string params "path"))
  (if path
    (progn
      (command "_.-PLOT" "_Y" "" "DWG To PDF.pc3"
        "ANSI_A_(8.50_x_11.00_Inches)" "_Inches" "_Landscape"
        "_N" "_Extents" "_Fit" "_Y" "acad.ctb" "_Y" "_N" "_Y" path "_Y")
      (cons T (strcat "{\"path\":\"" (mcp-escape-string path) "\"}")))
    (cons nil "Plot path required")
  )
)

;; --- P&ID list symbols ---

(defun mcp-cmd-pid-list-symbols (params / category dir-path files result)
  (setq category (mcp-json-get-string params "category"))
  (setq dir-path (strcat "C:/PIDv4-CTO/" category "/"))
  (setq files (vl-directory-files dir-path "*.dwg" 1))
  (setq result "")
  (if files
    (foreach f files
      (if (> (strlen result) 0) (setq result (strcat result ",")))
      ;; Remove .dwg extension
      (setq result (strcat result "\"" (substr f 1 (- (strlen f) 4)) "\""))
    )
  )
  (cons T (strcat "{\"category\":\"" category "\",\"symbols\":[" result "],\"count\":" (itoa (length (if files files '()))) "}"))
)

;; --- Block operations ---

(defun mcp-cmd-block-list ( / blk block-list)
  (setq block-list "" blk (tblnext "BLOCK" T))
  (while blk
    (if (not (= (substr (cdr (assoc 2 blk)) 1 1) "*"))
      (progn
        (if (> (strlen block-list) 0)
          (setq block-list (strcat block-list ",\"" (cdr (assoc 2 blk)) "\""))
          (setq block-list (strcat "\"" (cdr (assoc 2 blk)) "\""))
        )
      )
    )
    (setq blk (tblnext "BLOCK"))
  )
  (cons T (strcat "{\"blocks\":[" block-list "]}"))
)

(defun mcp-cmd-block-insert (params / name x y scale rotation block-id)
  (setq name (mcp-json-get-string params "name"))
  (setq x (mcp-json-get-number params "x"))
  (setq y (mcp-json-get-number params "y"))
  (setq scale (mcp-json-get-number params "scale"))
  (setq rotation (mcp-json-get-number params "rotation"))
  (setq block-id (mcp-json-get-string params "block_id"))
  (if (not scale) (setq scale 1.0))
  (if (not rotation) (setq rotation 0.0))
  (if (tblsearch "BLOCK" name)
    (progn
      (command "_.INSERT" name (list x y 0.0) scale scale rotation)
      (if (and block-id (> (strlen block-id) 0))
        (set_attribute_value (entlast) "ID" block-id)
      )
      (cons T (strcat "{\"entity_type\":\"INSERT\",\"handle\":\"" (cdr (assoc 5 (entget (entlast)))) "\"}"))
    )
    (cons nil (strcat "Block '" name "' not found"))
  )
)

;; -----------------------------------------------------------------------
;; Main dispatcher — called by "(c:mcp-dispatch)" from Python
;; -----------------------------------------------------------------------

(defun c:mcp-dispatch ( / cmd-files cmd-file json-text request-id cmd-name params-str result result-file)
  "Find pending command file, dispatch, write result."
  ;; Find first pending command file
  (setq cmd-files (vl-directory-files *mcp-ipc-dir* "autocad_mcp_cmd_*.json" 1))
  (if (not cmd-files)
    (progn (princ "\nMCP: No pending commands") (princ))
    (progn
      ;; Process first command
      (setq cmd-file (strcat *mcp-ipc-dir* (car cmd-files)))
      (setq json-text (mcp-read-file-lines cmd-file))

      (if (not json-text)
        (princ "\nMCP: Cannot read command file")
        (progn
          ;; Parse command
          (setq request-id (mcp-json-get-string json-text "request_id"))
          (setq cmd-name (mcp-json-get-string json-text "command"))

          (if (not cmd-name)
            (princ "\nMCP: No command in payload")
            (progn
              (princ (strcat "\nMCP: Dispatching " cmd-name " [" request-id "]"))

              ;; Execute via whitelist dispatcher
              (setq result
                (vl-catch-all-apply
                  'mcp-dispatch-command
                  (list cmd-name json-text)
                )
              )

              ;; Handle error from vl-catch-all-apply
              (if (vl-catch-all-error-p result)
                (setq result (cons nil (vl-catch-all-error-message result)))
              )

              ;; Write result
              (setq result-file (strcat *mcp-ipc-dir* "autocad_mcp_result_" request-id ".json"))
              (if (car result)
                (mcp-write-result result-file request-id T (cdr result) nil)
                (mcp-write-result result-file request-id nil nil (cdr result))
              )

              (princ (strcat "\nMCP: Done " cmd-name))
            )
          )

          ;; Clean up command file
          (vl-file-delete cmd-file)
        )
      )
    )
  )
  (princ)
)

;; -----------------------------------------------------------------------
;; Utility helpers (defined if not already loaded from external files)
;; -----------------------------------------------------------------------

(if (not ensure_layer_exists)
  (defun ensure_layer_exists (name color linetype)
    "Create layer if it doesn't exist."
    (if (not (tblsearch "LAYER" name))
      (command "_.-LAYER" "_NEW" name "_COLOR" color name "_LTYPE" linetype name "")
    )
  )
)

(if (not set_current_layer)
  (defun set_current_layer (name)
    "Set a layer as current."
    (setvar "CLAYER" name)
  )
)

(if (not set_attribute_value)
  (defun set_attribute_value (ent tag value / sub-ent ent-data)
    "Set an attribute value on a block insert by tag name."
    (setq sub-ent (entnext ent))
    (while sub-ent
      (setq ent-data (entget sub-ent))
      (if (and (= (cdr (assoc 0 ent-data)) "ATTRIB")
               (= (strcase (cdr (assoc 2 ent-data))) (strcase tag)))
        (progn
          (entmod (subst (cons 1 value) (assoc 1 ent-data) ent-data))
          (entupd sub-ent)
          (setq sub-ent nil)  ; stop
        )
        (if (= (cdr (assoc 0 ent-data)) "SEQEND")
          (setq sub-ent nil)
          (setq sub-ent (entnext sub-ent))
        )
      )
    )
  )
)

;; -----------------------------------------------------------------------
;; WBLOCK by regions — batch export by closed polylines on a layer
;; -----------------------------------------------------------------------

(defun mcp-poly-vertices (ent / edata pts pair)
  "Return 2D vertex list of an LWPOLYLINE entity as ((x y) ...)."
  (setq edata (entget ent) pts nil)
  (foreach pair edata
    (if (= (car pair) 10)
      (setq pts (cons (list (cadr pair) (caddr pair)) pts))))
  (reverse pts))

(defun mcp-str-replace (s old-str new-str / result pos len-old)
  "Replace all occurrences of old-str with new-str in s."
  (cond
    ((null s) "")
    ((or (null old-str) (= old-str "")) s)
    (t
     (progn
       (if (null new-str) (setq new-str ""))
       (setq result "" len-old (strlen old-str))
       (while (setq pos (vl-string-search old-str s))
         (setq result (strcat result (substr s 1 pos) new-str))
         (setq s (substr s (+ pos len-old 1))))
       (strcat result s)))))

(defun mcp-pad-int (n width / s)
  "Zero-pad integer n to width characters."
  (setq s (itoa n))
  (while (< (strlen s) width)
    (setq s (strcat "0" s)))
  s)

(defun mcp-sanitize-filename (s / result i ch)
  "Replace Windows-illegal filename characters with underscore."
  (if (null s) (setq s ""))
  (setq result "" i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (member ch '("<" ">" ":" "\"" "/" "\\" "|" "?" "*"))
      (setq result (strcat result "_"))
      (setq result (strcat result ch)))
    (setq i (1+ i)))
  result)

(defun mcp-find-text-in-region (vertices text-layer / filter ss ent edata text-val result)
  "Return text content of first TEXT/MTEXT fully inside the polygon vertices."
  (setq filter (list '(0 . "TEXT,MTEXT")))
  (if (and text-layer (> (strlen text-layer) 0))
    (setq filter (append filter (list (cons 8 text-layer)))))
  (setq ss (ssget "_WP" vertices filter))
  (setq result "")
  (if ss
    (progn
      (setq ent (ssname ss 0))
      (setq edata (entget ent))
      (setq text-val (cdr (assoc 1 edata)))
      (if text-val (setq result text-val))))
  result)

(defun mcp-cmd-drawing-wblock-by-regions (params /
       boundary-layer output-dir name-template index-start-num index-pad-num text-layer
       index-start-i index-pad-i
       ss-boundary i n ent edata vertices ss-region filter
       region-text region-name region-name-clean out-path
       exported skipped result)
  (setq boundary-layer  (mcp-json-get-string params "boundary_layer"))
  (setq output-dir      (mcp-json-get-string params "output_dir"))
  (setq name-template   (mcp-json-get-string params "name_template"))
  (setq index-start-num (mcp-json-get-number params "index_start"))
  (setq index-pad-num   (mcp-json-get-number params "index_pad"))
  (setq text-layer      (mcp-json-get-string params "text_layer"))
  (cond
    ((or (null boundary-layer) (= boundary-layer ""))
     (cons nil "boundary_layer required"))
    ((or (null output-dir) (= output-dir ""))
     (cons nil "output_dir required"))
    (t
     (progn
       (if (or (null name-template) (= name-template ""))
         (setq name-template "region_{index}"))
       (if (null index-start-num) (setq index-start-num 1.0))
       (if (null index-pad-num)   (setq index-pad-num 2.0))
       (setq index-start-i (fix index-start-num))
       (setq index-pad-i   (fix index-pad-num))
       (if (< index-pad-i 1) (setq index-pad-i 1))
       (if (not (or (= (substr output-dir (strlen output-dir)) "/")
                    (= (substr output-dir (strlen output-dir)) "\\")))
         (setq output-dir (strcat output-dir "/")))
       (setq filter (list '(0 . "LWPOLYLINE")
                          (cons 8 boundary-layer)
                          '(-4 . "&=") '(70 . 1)))
       (setq ss-boundary (ssget "_X" filter))
       (if (null ss-boundary)
         (cons nil (strcat "No closed LWPOLYLINE on layer: " boundary-layer))
         (progn
           (setq n (sslength ss-boundary))
           (setq exported "" skipped "" i 0)
           (while (< i n)
             (setq ent (ssname ss-boundary i))
             (setq edata (entget ent))
             (setq vertices (mcp-poly-vertices ent))
             (if (< (length vertices) 3)
               (progn
                 (if (> (strlen skipped) 0) (setq skipped (strcat skipped ",")))
                 (setq skipped (strcat skipped
                   "\"index " (itoa (+ index-start-i i)) ": vertices<3\"")))
               (progn
                 (setq ss-region (ssget "_CP" vertices))
                 (if (null ss-region)
                   (progn
                     (if (> (strlen skipped) 0) (setq skipped (strcat skipped ",")))
                     (setq skipped (strcat skipped
                       "\"index " (itoa (+ index-start-i i)) ": empty\"")))
                   (progn
                     (setq region-text "")
                     (if (vl-string-search "{text}" name-template)
                       (setq region-text (mcp-find-text-in-region vertices text-layer)))
                     (setq region-name name-template)
                     (setq region-name (mcp-str-replace region-name "{index}"
                       (mcp-pad-int (+ index-start-i i) index-pad-i)))
                     (setq region-name (mcp-str-replace region-name "{text}" region-text))
                     (setq region-name (mcp-str-replace region-name "{handle}"
                       (cdr (assoc 5 edata))))
                     (setq region-name (mcp-str-replace region-name "{layer}"
                       (cdr (assoc 8 edata))))
                     (setq region-name-clean (mcp-sanitize-filename region-name))
                     (setq out-path (strcat output-dir region-name-clean ".dwg"))
                     (if (findfile out-path) (vl-file-delete out-path))
                     (setvar "FILEDIA" 0)
                     (command "_.-WBLOCK" out-path "" '(0.0 0.0 0.0) ss-region "")
                     (setvar "FILEDIA" 1)
                     ;; -WBLOCK erases source objects by default; OOPS restores
                     ;; the last erased batch. Must be called after each iteration.
                     (command "_.OOPS")
                     (if (> (strlen exported) 0) (setq exported (strcat exported ",")))
                     (setq exported (strcat exported
                       "\"" (mcp-escape-string out-path) "\""))))))
             (setq i (1+ i)))
           (setq result (strcat "{\"count\":" (itoa n)
                                ",\"exported\":[" exported "]"
                                ",\"skipped\":[" skipped "]}"))
           (cons T result)))))))

;; -----------------------------------------------------------------------
;; Startup message
;; -----------------------------------------------------------------------

(princ "\n=== MCP Dispatch v3.1 loaded ===")
(princ "\nIPC directory: ")
(princ *mcp-ipc-dir*)
(princ "\nReady for commands via (c:mcp-dispatch)")
(princ)
