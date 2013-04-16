;	$Id: eclipse.el,v 1.19 2004/12/14 16:44:29 ks15 Exp $   

;;; eclipse.el --- major mode for editing and running ECLiPSe under Emacs

;; Copyright (C) 1986, 1987, 2001, 2002, 2003, 2004
;;   Free Software Foundation, Inc.

;; Author: Thorsten Winterer <t.winterer@icparc.ic.ac.uk>
;; based on the ECLiPSe mode from
;; Helmut Simonis <Helmut.Simonis@parc-technologies.com>
;; which was based on the prolog mode from
;; Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This package provides a major mode for editing ECLiPSe. It knows
;; about ECLiPSe syntax and comments, and can send regions to an inferior
;; ECLiPSe interpreter process.

;; The main difference to the previous Prolog mode is the extensive syntax
;; colouring done for ECLiPSe.
;; It supports in particular the comment/2 facility by highlighting
;; sections and pieces of the text

;; the system also knows about C-style comments
;; and indents properly after a ';'

;; Function keys:

;; TAB indents the current line. If the line is already correctly indented,
;;   a TAB character will be inserted
;; M-TAB or M-C-\ indent a region
;;   ( M-TAB, since it's more mnemonic than M-C-\,
;;     and Lisp completions are not needed in ECLiPSe )
;; S-TAB inserts TAB character. Also \C-c<SPC>
;; C-c-p-TAB indents the current predicate
;; C-c-q-TAB indents the current clause
;; C-c-b-TAB indents the current clause

;; M-[, M-], C-c[, C-c] all call dabbrev-expand or dabbrev-completion,
;;    but check predefined ECLiPSe keywords first
;;    however, there are slight differences in there behaviour:
;; M-[ calls dabbrev-expand, and returns the predicate template
;; M-] calls dabbrev-completion, and returns the predicate template
;; C-c-[ calls dabbrev-expand, and returns the predicate name + (
;; C-c-] calls dabbrev-completion, and returns the predicate name + (

;; C-l re-centres and re-fontifies
;; C-c-c comments out a region
;; C-c-r uncomments a region
;; C-c-i inverts the commenting of a region
;; C-c-C-f toggles the auto-line-break mode
;; C-c-C-j toggles the auto-indent mode

;; C-c-m-b marks the buffer
;; C-c-m-p marks the predicate
;; C-c-m-q marks the clause

;; M-C-a jumps to the next beginning of a predicate
;; M-C-e jumps to the next end of a predicate
;; M-a jumps to the next beginning of a clause
;; M-e jumps to the next end of a clause
;; C-c-C-z toggles the quick-jumps-mode

;; C-c-t inserts the template of the current predicate
;; C-c-s inserts the specs of the current predicate
;; M-RET inserts a new clause head
;; M-C-i inserts a new clause head without arguments

;; C-c-/ insert a short comment/2 template
;; C-c-\ insert a full comment/2 template

;; C-c-a anonymises the variables in the region
;; C-c-C-a replaces the variables in the region with anonymous variables

;; C-c-C-e starts an inferior ECLiPSe process
;; C-c-C-b compiles the buffer
;; C-c-C-v compiles a region
;; C-c-C-y compiles a region and switches to the ECLiPSe process
;; C-c-C-g passes a (region as) command to the ECLiPSe process
;; C-c-C-q stops the ECLiPSe process
;; C-c-C-k kills the ECLiPSe process
;; C-c-C-t starts the ECLiPSe-Tk-Tools

;; C-c-v-b checks the buffer for syntax errors by compiling
;;    it in an ECLiPSe process
;; C-c-v-v checks the current region
;; C-c-v-p checks the current predicate
;; C-c-v-c checks the current clause

;; C-c-h highlights the current word
;; C-c-d removes any highlighting
;; C-c-> moves to next occurrence of highlighted word
;; C-c-< moves to last occurrence of highlighted word

;; C-c-C-h calls the ECLiPSe help function

;; C-c-@-@ marks the current outline subtree
;; C-c-@-n jumps to the next visible outline heading
;; C-c-@-p jumps to the previous visible outline heading
;; C-c-@-u jumps to the outline heading one level above
;; C-c-@-f jumps to the next outline heading on the same level
;; C-c-@-b jumps to the previous outline heading on the same level
;; C-c-@-h hides all predicates
;; C-c-@-t hides the current predicate
;; C-c-@-c hides all clause bodies
;; C-c-@-e hides the current clause body
;; C-c-@-l hides the current block
;; C-c-@-a shows all
;; C-c-@-s shows all predicates (synonymous with C-c-@-a)
;; C-c-@-r shows the current predicate
;; C-c-@-d shows all clauses (synonymous with C-c-@-a)
;; C-c-@-m shows the current clause
;; C-c-@-k shows the current block

;; Variables:

;; eclipse-indent-width   Describes the number of space characters inserted
;;    when increasing the indentation. the default value is 4.
;; eclipse-tab-width   Describes the number of space characters inserted at
;;    the beginning of a line, if its indented. The default is 8.
;; eclipse-indent-to-parenthesis   If non-nil, indentation of the body of
;;    if-then-clauses or for-loops is calculated from the preceding opening
;;    paranthesis. Otherwise is calculated from the column of the 
;;    if-clause/for-clause.
;; eclipse-tab-mode   If non-nil, tabs are used for indentation, otherwise
;;    space characters only. The default is nil.
;; eclipse-autolinebreak-selected   If non-nil, auto-line-break is used.
;;    The default is t.
;; eclipse-autoindent-selected   If non-nil, auto-indent is used.
;;    The default is t.
;; eclipse-quick-jumps-selected   If non-nil, quick jumps are used.
;;    The default is t.
;; eclipse-font-lock-default  Contains the default level for the
;;    fontification. The default is 3.
;; eclipse-backtab   Describes the key-sequence for "backtab". This seems to
;;    depend on what Emacs and what GUI you use. If [backtab] does not work,
;;    try [S-kp-tab] or [S-tab]. Or use whatever you fancy: with this key,
;;    additional tab characters (or equivalent space characters) are inserted
;; for XEmacs: the colours for the fontification are defined in variables
;;    eclipse-*-face-val

;; There are more customisable variables, but they are less likely to be
;; changed. Check the customisation options for group eclipse.

;; Running of ECLiPSe in an inferior mode has not been thoroughly tested,
;; I normally use the tkeclipse environment.

;; Opening the speedbar from within the ECLiPSe mode will automatically add
;; .ecl to the supported extensions.
;; If you want to load the speedbar automatically when starting Emacs, add
;;
;; (speedbar)
;; (speedbar-add-supported-extension ".ecl")
;;
;; to your .emacs file.
;; If you do not load speedbar automatically but open one before loading
;; the first ECLiPSe file, you have to add .ecl to the list of supported
;; extensions by either calling (speedbar-add-supported-extension ".ecl")
;; or add
;;
;;(custom-set-variables
;;  '(speedbar-supported-extension-expressions
;;     (quote (".ecl" <whatever is in the variable now>))))
;;
;; to your .emacs file.

;; This version has been tested on GNU emacs 20.7.1 for Solaris 2.7,
;;    and on GNU emacs 21.1.1 for Linux and Solaris 2.7.; also to some
;;    extent on XEmacs 21.1.14 for Linux and Solaris 2.7.
;; Your mileage may vary.


;; New:
;; auto-completion of ECLiPSe keywords
;; ECLiPSe help function call
;; highlighting

;;; NOTE: If there is a problem with entering commands in the inferior 
;;; ECLiPSe process window, disable the line
;;;               (define-key map "\r" 'eclipse-next-line)
;;; in the definition of function eclipse-mode-commands


;; To do:
;; waiting for the new ECLiPSe compiler, so that source code tracing
;; can be added

;;; Code:

;; what Emacs is it?

(defvar eclipse-emacs-21 (equal (substring (version) 0 12) "GNU Emacs 21"))
(defvar eclipse-xemacs (equal (substring (version) 0 6) "XEmacs"))

;; Definitions...

(defvar eclipse-mode-syntax-table nil)
(defvar eclipse-mode-abbrev-table nil)
(defvar eclipse-mode-map nil)

(defgroup eclipse nil
  "Major mode for editing and running ECLiPSe under Emacs."
  :group 'languages)

(defconst eclipse-mode-version 6.2)

(defvar eclipse-version 0.0
  "Variable is set when ECLiPSe process is started")

;; path definitions and program calls

(defcustom eclipse-path ""
  "Path where ECLiPSe can be found.

Change only, if ECLiPSe path is not in environment variable PATH"
  :type 'string
  :group 'eclipse)

(defconst eclipse-program-name "eclipse"
  "Program name for invoking an inferior ECLiPSe with `run-eclipse'.")

(defconst eclipse-version-call "get_flag(version,Version).\n"
  "ECLiPSe command to get the version number.")

(defconst eclipse-tktools-name "tktools"
  "Program name for invoking Tcl/Tk-based tools for ECLiPSe.")

(defconst eclipse-program-call
  (concat eclipse-path eclipse-program-name)
  "Program call for invoking an inferior ECLiPSe with `run-eclipse'.")

(defconst eclipse-tktools-call
  (concat eclipse-path eclipse-tktools-name)
  "Program call for invoking Tcl/Tk-based tools for ECLiPSe.")

(defconst eclipse-tktools-lib-name "remote_tools"
  "ECLiPSe library for invoking Tcl/Tk-based tools for ECLiPSe.")

(defconst eclipse-tktools-lib-pred "lib"
  "How remote_tools.pl shall be loaded: as library (lib) or module
(use_module).")

(defconst eclipse-tktools-lib-call
  (concat eclipse-tktools-lib-pred "(" eclipse-tktools-lib-name "), ")
  "ECLiPSe tktools library call.")

(defconst eclipse-54-tktools-call 
  (concat eclipse-tktools-lib-call "attach_tools(Host/Port,block,writeln([Host,Port])).\n")
  "ECLiPSe command for invoking Tcl/Tk-based tools for ECLiPSe 5.4 and later.

The first parameter of attach_tools/3 returns the Host and Port, 
the second parameter is the timeout in seconds (or 'block' for no timeout),
the third parameter is a predicate called after establishing the connection.")

(defconst eclipse-53-tktools-call
  (concat eclipse-tktools-lib-call "attach_tools.\n")
  "ECLiPSe command for invoking Tcl/Tk-based tools for ECLiPSe 5.3 and earlier.")

(defconst eclipse-run-tktools-func 'eclipse-run-tktools
  "Elisp function to extract Host and Port values from output and start
tktools.

Is added to 'comint-preoutput-filter-functions, and must remove itself from
this list when the output line containing host and port is processed.")

(defconst eclipse-reconsult-string "reconsult(user).\n"
  "*(Re)Consult mode (for C-Prolog and Quintus Prolog).")

(defconst eclipse-consult-string "consult(user).\n"
  "*Consult mode.")

(defconst eclipse-compile-string "compile(user).\n"
  "*Compile mode.")

(defconst eclipse-eof-string "end_of_file.\n"
  "*String that represents end of file for eclipse.
nil means send actual operating system end of file.")

(defconst eclipse-halt-string "halt.\n"
  "*Command that stops the eclipse process.")

(defconst eclipse-help-call1 
  (concat eclipse-program-call " -e \"help(")
  "First part of help call to ECLiPSe system.")

(defconst eclipse-help-call2 ").\""
  "Second part of help call to ECLiPSe system.")

(defconst eclipse-check-call
  (concat eclipse-program-call " -e \"[user].\"")
  "Call to ECLiPSe system to check syntax.")

(defvar eclipse-check-libraries nil
  "List of libraries to be loaded for syntax check.")

;; indentation definitions

(defcustom eclipse-indent-width 4
  "Standard additional indentation in ECLiPSe buffers."
  :type 'integer
  :group 'eclipse)

(defcustom eclipse-tab-width 8
  "Minimum indentation in ECLiPSe buffers."
  :type 'integer
  :group 'eclipse)

(defvar eclipse-old-tab-width eclipse-tab-width)

(defcustom eclipse-indent-mode nil
  "If set to t, indentation will always increase/decrease by `eclipse-indent-width'."
  :type 'integer
  :group 'eclipse)

(defcustom eclipse-indent-to-parenthesis t
  "Indentation if if-then-clauses and for-loops calculated from column of preceding opening parenthesis."
  :type 'integer
  :group 'eclipse)

(defcustom eclipse-first-line-std-indent t
  "Always indent the first line of a predicate using `eclipse-tab-width'."
  :group 'eclipse)

(defcustom eclipse-backtab [backtab]
  "Definition of \"backtab\" for insertion of additional tabs."
  :type 'array
  :group 'eclipse)

(defcustom eclipse-tab-mode nil
  "Indentation in ECLiPSe buffers with spaces or tabs?
Set this variable to nil to insert only space characters.
To change the behaviour during editing, use \\[eclipse-tab-mode-toggle]."
  :group 'eclipse)

(defcustom eclipse-autolinebreak-selected t
  "Automatic line-break in ECLiPSe buffer."
  :group 'eclipse)

(defcustom eclipse-autoindent-selected t
  "Automatic indentation in ECLiPSe buffer."
  :group 'eclipse)

(defcustom eclipse-indent-clause-heads nil
  "If t, clause heads will be indented to column 0.
If nil, indentation of clause heads will not be changed."
  :group 'eclipse)

(defcustom eclipse-quick-jumps-selected nil
  "If t, the 'go to' commands determine the place to jump to by the next
empty line. If nil, the correct place to jump to is computed correctly, but
this may be slow if the buffer text is long."
  :group 'eclipse)

(defcustom eclipse-indent-timeout 2
  "Timeout for indentation in seconds."
  :type 'integer
  :group 'eclipse)

;; speedbar support definitions

(defvar eclipse-speedbar-selected nil)
  ;; Variable to store the status of the speedbar.
  ;; If t, the speedbar is running, if nil, the speedbar is off.

(defvar eclipse-speedbar-supported nil)
  ;; Variable is t, if speedbar supports .ecl extension.

(defconst eclipse-imenu-generic-expression
  (list (list nil (purecopy "^\\([a-z].*\\)\\([:?]-\\|\n\\)") 1)
 	(list "Directives" "^\\([ \t]*[:?]-[ \t]*\\)\\([a-z].*\\)\\(.\\|\n\\)" 2))
  "Imenu generic expression for ECLiPSe mode. See `imenu-generic-expression'.")

(defconst eclipse-imenu-prev-index-position-function
  ;; my own function to find the previous index function
  'eclipse-goto-prev-index-position)

(defconst eclipse-imenu-create-index-function
  ;; my own function to create the imenu index
  'eclipse-create-index)

(defconst eclipse-imenu-extract-index-name-function
  ;; my own function to extract the name for the index function
  'eclipse-extract-index-name)

(defvar eclipse-overlays
  ;; list of highlighting overlays
  nil)

(defvar eclipse-highlighted
  ;; currently highlighted word
  nil)

(defcustom eclipse-highlight-face-bg-val "cornflower blue"
    "Type face background for highlighting."
    :type 'string
    :group 'eclipse)

(defcustom eclipse-highlight-face-fg-val "white"
    "Type face foreground for highlighting."
    :type 'string
    :group 'eclipse)

;; make face for highlighting
(make-face 'eclipse-highlight-face)
(cond (eclipse-xemacs
       (set-face-property 'eclipse-highlight-face
			  'background eclipse-highlight-face-bg-val)
       (set-face-property 'eclipse-highlight-face
			  'foreground eclipse-highlight-face-fg-val))
      (t
       (set-face-background 'eclipse-highlight-face eclipse-highlight-face-bg-val)
       (set-face-foreground 'eclipse-highlight-face eclipse-highlight-face-fg-val)))

;; font lock definitions

(defcustom eclipse-font-lock-default 3
  "The default level for the fontification."
  :type 'integer
  :group 'eclipse)  

(when eclipse-xemacs
  ;; set colours to GNU-Emacs-like values!
  (defcustom eclipse-builtin-face-val "magenta"
    "Builtin face for XEmacs."
    :type 'string
    :group 'eclipse)
  (defcustom eclipse-warning-face-val "red"
    "Warning face for XEmacs."
    :type 'string
    :group 'eclipse)
  (defcustom eclipse-keyword-face-val "purple"
    "Keyword face for XEmacs."
    :type 'string
    :group 'eclipse)
  (defcustom eclipse-function-name-face-val "blue"
    "Function name face for XEmacs."
    :type 'string
    :group 'eclipse)
  (defcustom eclipse-variable-name-face-val "darkorange"
    "Variable name face for XEmacs."
    :type 'string
    :group 'eclipse)
  (defcustom eclipse-comment-face-val "red3"
    "Comment face colour for XEmacs."
    :type 'string
    :group 'eclipse)
  ;; for whatever reason, XEmacs does not always use this value...
  (defcustom eclipse-string-face-val "salmon"
    "String face colour for XEmacs."
    :type 'string
    :group 'eclipse)
  (defcustom eclipse-constant-face-val "aquamarine4"
    "Constant face for XEmacs."
    :type 'string
    :group 'eclipse)
  (defcustom eclipse-type-face-val "forestgreen"
    "Type face for XEmacs."
    :type 'string
    :group 'eclipse)
  ;; create the faces that XEmacs doesn't know
  (make-face 'font-lock-builtin-face)
  (make-face 'font-lock-constant-face)
  (make-face 'font-lock-warning-face)
  (make-face 'font-lock-keyword-face)
  (make-face 'font-lock-function-name-face)
  (make-face 'font-lock-variable-name-face)
  (make-face 'font-lock-comment-face)
  (make-face 'font-lock-string-face)
  (make-face 'font-lock-type-face)
  (set-face-property 'font-lock-builtin-face
		     'foreground eclipse-builtin-face-val)
  (set-face-property 'font-lock-warning-face
		     'foreground eclipse-warning-face-val)
  (set-face-property 'font-lock-keyword-face
		     'foreground eclipse-keyword-face-val)
  (set-face-property 'font-lock-function-name-face
		     'foreground eclipse-function-name-face-val)
  (set-face-property 'font-lock-variable-name-face
		     'foreground eclipse-variable-name-face-val)
  (set-face-property 'font-lock-comment-face
		     'foreground eclipse-comment-face-val)
  (set-face-property 'font-lock-string-face
		     'foreground eclipse-string-face-val)
  (set-face-property 'font-lock-constant-face
		     'foreground eclipse-constant-face-val)
  (set-face-property 'font-lock-type-face
		     'foreground eclipse-type-face-val))

(unless eclipse-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 14b" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "." table)
    (setq eclipse-mode-syntax-table table)))

(define-abbrev-table 'eclipse-mode-abbrev-table ())

(defconst eclipse-font-lock-colondash
  (list 
   ;; :- not necessarily at end of line
   '("[:?]-" 0 font-lock-builtin-face)))

(defconst eclipse-font-lock-basic
  (list
   ;; quoted atoms
   '("'\\(\\\\'\\|[^'\n]\\)*'" 0 font-lock-string-face)
   ;; show variables
   '("\\<\\([_A-Z][a-zA-Z0-9_]*\\)\\>" 1 font-lock-variable-name-face)
   ;; predicate definitions (that can be fontified) always start at bol
   '("^[a-z][a-zA-Z0-9_]*" 0 font-lock-function-name-face)
   ;; critical builtins with arity 0 or prefix op
   ;; special for cut, not   (\\?-\\s-*$)???
   '("\\(\\<\\(abort\\|exit\\|fa\\(il\\|lse\\)\\|halt\\|make\\|not\\|pause\\)\\>\\|[ \t(]\\(\\\\\\+\\|~\\)[ \t(]\\|!\\|\\\\\\+\\)" 0 font-lock-warning-face)
   ;; true
   '("\\<true\\>" 0 font-lock-constant-face)))

(defconst eclipse-font-lock-basic-infix
  (list
   ;; operators at end of line & infix built-ins & arithmetic built-ins &
   ;; semicolon
   '("\\(\\([:?]-\\)\\s-*$\\|\\<is\\>\\|=\\.\\.\\|==\\|=\\\\=\\|=:=\\|\\\\==\\|\\\\=\\|>=\\|=<\\|<=\\|<\\|=\\|\\\\=\\|~=\\|[ \t(]\\([-*+/^]\\|//\\)(\\|;\\)" 0 font-lock-builtin-face)
   '("\\([^?-]\\)\\(-?>\\)" 2 font-lock-builtin-face)))

(defconst eclipse-font-lock-low-infix
  (list
   ;; operators at end of line & infix built-ins & arithmetic built-ins &
   ;; semicolon
   '("\\(\\(\\<do\\|[:?]-\\)\\s-*$\\|\\<is\\>\\|->\\|=\\.\\.\\|==\\|=\\\\=\\|=:=\\|\\\\==\\|\\\\=\\|#<=>\\|#<=\\|#\\\\\\+\\|[#$&]?\\(>=\\|>\\|=<\\|<\\|=\\)\\|@\\(>=\\|>\\|=<\\|<\\)\\|[#$]\\\\=\\|#\\(#\\|\\\\/\\|/\\\\\\|::\\)\\|#\\|::\\|&::\\|`::\\|`<>\\|`\\(<\\|=\\)\\|~=<\\|~=\\|[ \t(]\\([-*+/^~]\\|//\\|/\\\\\\|<<\\|>>\\|\\\\/\\)(\\|;\\)" 0 font-lock-builtin-face)
   '("\\([^?-]\\)\\(-?>\\)" 2 font-lock-builtin-face)))

(defconst eclipse-font-lock-low-builtins
  (list
   ;; normal built-ins & control : non-critical
   '("\\<\\(once\\|nl\\)\\>" 0 font-lock-builtin-face)
   '("\\<\\(a\\(bs\\|cos\\|ppend\\(_strings\\)?\\|rg[cv]?\\|sin\\|t\\(_eof\\|an\\|om\\(_\\(length\\|string\\)\\|ic\\)?\\)?\\)\\|b\\(ag\\(_\\(create\\|dissolve\\|enter\\)\\|of\\)\\|etween\\|ind\\|lock\\|ytes_to_term\\)\\|c\\(a\\(ll\\(_c\\)?\\|nonical_path_name\\)\\|d\\|eiling\\|har_\\(code\\|int\\)\\|l\\(ause\\|ose\\)\\|o\\(mp\\(are\\|ile\\(_\\(stream\\|term\\)\\)?\\|ound\\)\\|n\\(cat_\\(atoms?\\|strings?\\)\\|nect\\)\\|py_term\\(_vars\\)?\\|s\\|unt\\|verof\\)\\|putime\\|reate_module\\)\\|d\\(ate\\|e\\(l\\(ayed_goals\\(_number\\)?\\)\\|mon\\|i\\(m\\|splay\\)\\|omain\\)\\|e\\(val\\|x\\(\\|i\\(sts\\|t_block\\)\\|p\\(and_goal\\)?\\)\\)\\|f\\(i\\(ndall\\|x\\)\\|l\\(o\\(at\\|or\\)\\|ush\\)\\|or\\(each\\(arg\\|elem\\|index\\)?\\)?\\|r\\(andom\\|ee\\|omto\\)\\|unctor\\)\\|g\\(et\\(_\\(char\\|flag\\|priority\\|s\\(tream\\(_info\\)?\\|uspension_data\\)\\|var_\\(bounds\\|info\\)\\)\\)?\\|round\\)\\|help\\|i\\(n\\(line\\|stance\\|teger\\(_atom\\)?\\)\\)\\|join_string\\|k\\(eysort\\|ill\\)\\|l\\(ength\\|n\\|o\\(ad\\|oop_name\\)\\)\\|m\\(ax\\|e\\(mber\\(chk\\)?\\|rge\\)\\|in\\|od\\|ultifor\\)\\|n\\(l\\|o\\(n\\(ground\\|var\\)\\|t_unify\\)\\|um\\(ber\\(_string\\)?\\)\\)\\|o\\(pen\\|nce\\)\\|p\\(a\\(ram\\|thname\\)\\|ipe\\|lus\\|r\\(ed\\|intf?\\)\\|ut\\(_char\\)?\\)\\|r\\(a\\(ndom\\|tional\\)\\|e\\(a\\(d\\(_\\(directory\\|string\\|token\\)\\|var\\)\\|[dl]\\)\\|corded\\(_list\\)?\\|verse\\)\\|ound\\)\\|s\\(etof\\|h\\(elf_\\(abolish\\|create\\|get\\|set\\)\\)?\\|in\\|ort\\)\\|plit_string\\|qrt\\|t\\(ring\\(_l\\(ength\\|ist\\)\\)?\\)\\|u\\(b\\(call\\|string\\)\\|m\\|spensions\\)\\|ystem\\)\\|t\\(an\\|erm_\\(string\\|variables\\)\\|ype_of\\)\\|var\\|w\\(rite\\(clause\\|ln\\|q\\)?\\)\\)(" 1 font-lock-builtin-face)))

(defconst eclipse-font-lock-medium-builtins
  (list
   ;; normal built-ins & control : non-critical
   '("\\([ \t]^[ \t]\\|\\<\\(env\\|garbage_collect\\|listing\\|once\\|nl\\|peer_multitask_\\(confirm\\|terminate\\)\\|statistics\\|trimcore\\)\\>\\)" 0 font-lock-builtin-face)
   '("\\<\\('C'\\|a\\(bs\\|c\\(cept\\|os\\|yclic_term\\)\\|dd_attribute\\|l\\(arm\\|s\\)\\|ppend\\(_strings\\)?\\|rg[cv]?\\|sin\\|t\\(_eof\\|an\\|om\\(_\\(length\\|string\\)\\|ic\\)?\\|tached_suspensions\\)?\\)\\|b\\(ag\\(_\\(abolish\\|c\\(ount\\|reate\\)\\|dissolve\\|e\\(nter\\|rase\\)\\|retrieve\\)\\|of\\)\\|etween\\|ind\\|lock\\|real\\(_\\(bounds\\|from_bounds\\|m\\(ax\\|in\\)\\)\\)?\\|ytes_to_term\\)\\|c\\(a\\(ll\\(_\\(c\\|priority\\)\\)?\\|n\\(cel_after_event\\|onical_path_name\\)\\)\\|d\\|eiling\\|har_\\(code\\|int\\)\\|l\\(ause\\|ose\\|rbit\\)\\|o\\(mp\\(are\\(_instances\\)?\\|ile\\(_\\(stream\\|term\\)\\|d_stream\\)?\\|ound\\)\\|n\\(cat_\\(atoms?\\|strings?\\)\\|nect\\)\\|py_term\\(_vars\\)?\\|s\\|unt\\|verof\\)\\|putime\\|reate_module\\|urrent_\\(a\\(fter_events?\\|rray\\|tom\\)\\|built_in\\|compiled_file\\|domain\\|error\\|functor\\|host\\|interrupt\\|m\\(acro\\|odule\\(_predicate\\)?\\)\\|op\\|predicate\\|record\\|s\\(t\\(ore\\|r\\(eam\\|uct\\)\\)\\|uspension\\)\\)\\)\\|d\\(ate\\|e\\(layed_goals\\(_number\\)?\\|mon\\|nominator\\)\\|i\\(m\\|splay\\)\\|omain_index\\)\\|e\\(nter_suspension_list\\|rr\\(no_id\\|or_id\\)\\|val\\|x\\(ec\\(_group\\)?\\|i\\(st\\(ing_file\\|s\\)\\|t_block\\)\\|p\\(and_goal\\)?\\|ternal\\)\\)\\|f\\(i\\(ndall\\|x\\)\\|l\\(o\\(at\\|or\\)\\|ush\\)\\|or\\(each\\(arg\\|elem\\|index\\)?\\|k\\)?\\|r\\(andom\\|ee\\|omto\\)\\|unctor\\)\\|g\\(et\\(_\\(ch\\(ar\\|tab\\)\\|e\\(rror_handler\\|vent_handler\\)\\|f\\(ile_info\\|lag\\)\\|interrupt_handler\\|leash\\|module_info\\|priority\\|s\\(tream\\(_info\\)?\\|uspension_data\\)\\|var_\\(bounds\\|info\\)\\)\\|bit\\|cwd\\|env\\|val\\)?\\|round\\)\\|help\\|i\\(n\\(line\\|stance\\|teger\\(s\\|_atom\\)\\)\\|s_\\(built_in\\|dynamic\\|event\\|handle\\|predicate\\|record\\|suspension\\)\\)\\|join_string\\|keysort\\|l\\(ength\\|isten\\|n\\|o\\(ad\\|c\\(cal_time\\(_string\\)?\\|k\\)\\|op_name\\)\\)\\|m\\(ax\\|e\\(mber\\(chk\\)?\\|rge\\|ta\\(_\\(attribute\\|bind\\)\\)?\\)\\|in\\|kdir\\|od\\|sort\\|u\\(ltifor\\|tex\\(_init\\)?\\)\\)\\|n\\(ame\\|ew_socket_server\\|l\\|o\\(n\\(ground\\|var\\)\\|t_unify\\)\\|um\\(ber\\(_string\\)?\\|erator\\)\\)\\|o\\(ccurs\\|nce\\|pen\\|s_file_name\\)\\|p\\(a\\(ram\\|thname\\)\\|eer\\(_\\(d\\(eregister_multitask\\|o_multitask\\)\\|get_property\\|queue_\\(c\\(lose\\|reate\\)\\|get_property\\)\\|register_multitask\\)\\)?\\|hrase\\|ipe\\|lus\\|ortray_goal\\|r\\(ed\\|intf?\\|ofile\\|une_instances\\)\\|ut\\(_char\\)?\\)\\|r\\(a\\(ndom\\|tional\\)\\|e\\(a\\(d\\(_\\(directory\\|exdr\\|string\\|t\\(erm\\|oken\\)\\)\\|var\\)?\\|l\\)\\|corded\\(_list\\)?\\|ferenced_record\\|mote_\\(connect\\(_\\(accept\\|setup\\)\\)?\\|disconnect\\|yield\\)\\|name\\|verse\\)\\|ound\\)\\|s\\(e\\(e[dk]\\|lect\\|t\\(_stream\\(_property\\)?\\|of\\)\\)\\|gn\\|h\\(elf_\\(abolish\\|create\\|get\\|set\\)\\)?\\|in\\|leep\\|o\\(cket\\|rt\\)\\|plit_string\\|qrt\\|t\\(atistics\\|ore\\(_\\(c\\(o\\(ntains\\|unt\\)\\|reate\\)\\|delete\\|erase\\|get\\|inc\\|set\\)\\|d_keys\\(_and_values\\)?\\)?\\|ring\\(_\\(code\\|l\\(ength\\|ist\\)\\)\\)?\\)\\|u\\(b\\(call\\|s\\(cript\\|tring\\)\\)\\|m\\|spensions\\)\\|ystem\\)\\|t\\(an\\|erm_\\(hash\\|string\\|to_bytes\\|variables\\)\\|imes\\|ool_body\\|y\\(pe_of\\|[io]\\)\\)\\|un\\(get\\|lock\\)\\|var\\(ia\\(ble\\|nt\\)\\)?\\|w\\(ait\\|rite\\(_\\(canonical\\|exdr\\|term\\)\\|clause\\|ln\\|q\\)?\\)\\|xor\\|yield\\)(" 1 font-lock-builtin-face)))

(defconst eclipse-font-lock-low
  (list
   ;; comments
   '("/\\*[^*]*\\*/" 0 font-lock-comment-face t)
   ;; critical builtins
   '("\\<\\(a\\(ssert[az]?\\|ttach_suspensions\\)\\|call_priority\\|de\\(cval\\|fine_macro\\|lete\\)\\|e\\(r\\(ase\\(_\\(a\\(ll\\|rray\\)\\|m\\(acro\\|odule\\)\\)\\)?\\|ror\\)\\|vent\\(_\\(after\\(_every\\)?\\|create\\|retrieve\\)\\|s_after\\)?\\)\\|in\\(c\\(lude\\|val\\)\\|it_suspension_list\\|sert_suspension\\)\\|kill\\(_suspension\\)?\\|m\\(ake_suspension\\|erge_suspension_list\\)\\|notify_constrained\\|re\\(cord[az]?\\|record\\|set_e\\(vent\\|rror\\)_handler\\|tract\\(_all\\)?\\)\\|s\\(chedule_suspensions\\|et\\(_\\(chtab\\|default_error_handler\\|e\\(rror_handler\\|vent_handler\\)\\|flag\\|interrupt_handler\\|suspension_data\\|var_bounds\\)\\|arg\\|bit\\|val\\)\\|uspend\\)\\|t\\(est_and_setval\\|rigger\\)\\|update_struct\\|x\\(get\\|set\\)\\)(" 1 font-lock-warning-face)
   '("\\<\\(reset_error_handlers\\|trimcore\\)\\>" 0 font-lock-warning-face)))

(defconst eclipse-font-lock-medium
  (list
   ;; base operator
   '("\\([0-9]\\)\\('\\)\\([a-zA-Z0-9]+\\)" 2 font-lock-builtin-face)
   ;; directives
   '("^:-\\s-*\\(comment\\|d\\(emon\\|ynamic\\)\\|export\\|import\\|p\\(arallel\\|ragma\\)\\|reexport\\)\\>" 0 font-lock-keyword-face)
   ;; highlight mode/tool declaration
   '("^\\(:-\\s-*mode\\)\\([^.]*\\)\\."
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
   '("^\\(:-\\s-*tool\\)(\\([a-z][a-zA-Z0-9_]*/[0-9]+\\),[ ]*\\([a-z][a-zA-Z0-9_]*/[0-9]+\\))\\."
     (1 font-lock-keyword-face) (2 font-lock-constant-face)
     (3 font-lock-constant-face))
   '("^:-\\s-*\\(mode\\|tool\\)\\>" 0 font-lock-keyword-face)
   ;; module names & structures
   '("^\\(:-\\s-*\\(module\\|module_interface\\|begin_module\\|use_module\\|lib\\)\\)(\\([a-zA-Z_0-9]+\\))"
     (1 font-lock-keyword-face) (3 font-lock-constant-face))
   '("^\\(:-\\s-*ensure_loaded\\)(\\([a-zA-Z_0-9. ']+\\))"
     (1 font-lock-keyword-face)) ; (2 font-lock-constant-face))
   '("^\\(:-\\s-*\\(local\\|export\\)\\)[ \t\n]*\\(s\\(helf\\|t\\(ore\\|ruct\\)\\|yntax_option\\)\\|macro\\|op\\|portray\\|re\\(cord\\|ference\\)\\|variable\\|array\\|domain\\|chtab\\|initialization\\|finalization\\)("
     (1 font-lock-keyword-face) (3 font-lock-keyword-face t))
   '("\\<\\(s\\(helf\\|truct\\|yntax_option\\)\\|macro\\|op\\|portray\\|reference\\|variable\\|array\\|domain\\|chtab\\|initialization\\|finalization\\)("
     (1 font-lock-keyword-face t))
   '("\\<\\(store\\|record\\)([a-z][a-zA-Z0-9_]*)"
     (1 font-lock-keyword-face t))
   '("^\\(:-\\s-*local\\)[ \t]"
     (1 font-lock-keyword-face))
   ;; import from module
   '("\\(from\\)\\s-+\\([a-z0-9_]+\\)" (1 font-lock-keyword-face)
     (2 font-lock-constant-face))
   ;; special case for structures   
   '("\\<[a-z][a-zA-Z0-9_]*[ \t]+\\(of [a-z][a-zA-Z0-9_]*\\|with\\)\\>" 1 font-lock-constant-face)
   ;; structure elements and calls from other module
   '("\\<[a-z][a-zA-Z0-9_]*:" 0 font-lock-constant-face)
   ;; calls "as if inside other module"
   '("@[ \t]*[A-Za-z0-9_]+" 0 font-lock-constant-face)
   ;; critical builtins with arity 0 or prefix op
   '("\\(\\<\\(re\\(peat\\|set_error_handlers\\)\\|wake\\)\\>\\|-->\\|-\\?->\\|\\?-\\)" 0 font-lock-warning-face)
   ;; critical builtins
   '("\\<\\(arr_create\\|current_array\\|erase_array\\|local_array\\|meta_attribute\\)(" 1 font-lock-warning-face)
   ;; debugging
   '("\\<\\(debug\\(_compile\\|ging\\)?\\|no\\(debug\\|spy\\|trace\\)\\|s\\(kipped\\|py\\(_\\(term\\|var\\)\\)?\\)\\|trace\\(_exit_port\\)?\\)\\>" 0 font-lock-constant-face)
   '("\\<\\(debug\\|[gs]et_leash\\|leash\\|\\(kill\\|make\\)_display_matrix\\|trace\\(_\\(call\\|parent\\|point\\)port\\|able\\)?\\|un\\(skipped\\|traceable\\)\\)(" 1 font-lock-constant-face)
   ;; some other stuff: constants, etc.
   '("\\<\\(a\\(fter_event_timer\\|ll\\(sols\\|_dynamic\\)\\|nti_first_fail\\|rrays\\)\\|b\\(ased_bignums\\|bs\\|lanks_in_nil\\|rea\\(k_level\\|l_exceptions\\)\\)\\|c\\(o\\(mplete\\|ntrol\\|routine\\)\\|redit\\|wd\\)\\|d\\(bs\\|e\\(bug\\(_compile\\|ging\\)\\|fault_language\\|nse_output\\)\\|oubled_quote_is_quote\\)\\|e\\(clipse_\\(info\\|object\\)_suffix\\|n\\(able_interrupts\\|d_of_file\\)\\|xtension\\)\\|f\\(irst_fail\\|loat_precision\\)\\|g\\(c\\(_\\(interval\\(_dict\\)?\\|policy\\)\\)?\\|oal_expansion\\)\\|host\\(arch\\|id\\|name\\)\\|i\\(gnore_eof\\|n\\(domain\\(_\\(interval\\|m\\(ax\\|edian\\|i\\(ddle\\|n\\)\\)\\|random\\|split\\)\\)?\\|put_order\\|stallation_directory\\)\\|so_\\(base_prefix\\|escapes\\)\\)\\|l\\(a\\(rgest\\|st_errno\\)\\|ds\\|i\\(brary_path\\|mit_arg_precedence\\)\\|oaded_library\\)\\|m\\(a\\(cro_expansion\\|x_\\(global_trail\\|local_control\\|predicate_arity\\|regret\\)\\)\\|ost_constrained\\)\\|n\\(ested_commentsl_in_quotes\\|o_\\(array_subscripts\\|blanks\\)\\)\\|o\\(bject_suffix\\|ccur\\(_check\\|rence\\)\\|utput_mode\\)\\|p\\(p?id\\|r\\(efer_rationals\\|int_depth\\|olog_suffix\\)\\)\\|remote_protocol_version\\|s\\(mallest\\|yntax_option\\)\\|t\\(mp_dir\\|oplevel_module\\)\\|unix_time\\|v\\(ariable_names\\|ersion\\(_as_list\\)?\\)\\|w\\(m_window\\|orker\\(ids\\|s\\)?\\)\\)\\>" 0 font-lock-constant-face)
   ;; 'with attributes'/2
   '("'with attributes'" 0 font-lock-builtin-face t)))

(defconst eclipse-comment-font-lock
  (list
   ;; fontification of comment predicate
   '("\\(^\\|comment(.*\\)\\s-*[[]?\\s-*\\(a\\(lias\\|mode\\|rgs\\|uthor\\)\\|copyright\\|d\\(ate\\|esc\\)\\|e\\(g\\|xceptions\\)\\|f\\(ail_if\\|ields\\)\\|in\\(clude\\|dex\\)\\|resat\\|s\\(ee_also\\|ummary\\)\\|template\\)\\s-*[,:]" 2 font-lock-type-face)
   ;; special case for comment/2 predicate
   '("^:-\\s-*comment(\\([a-z_][a-z0-9_]*\\s-*/\\s-*[0-9]+\\)" 1 font-lock-function-name-face)
   ;; predicate definitions in comment/2
   '("\\(<[Pp]>\\|</[Pp]>\\|<[Bb]>\\|</[Bb]>\\|<[Ll][Ii]>\\|</[Ll][Ii]>\\|<[Uu][Ll]>\\|</[Uu][Ll]>\\|<[Aa][^>]*>\\|</[Aa]>\\|<[Ii]>\\|</[Ii]>\\|<[Dd][Ll]>\\|</[Dd][Ll]>\\|<[Dd][Tt]>\\|</[Dd][Tt]>\\|<[Dd][Dd]>\\|</[Dd][Dd]>\\|<[Tt][Tt]>\\|</[Tt][Tt]>\\|<[Ee][Mm]>\\|</[Ee][Mm]>\\|<[Pp][Rr][Ee]>\\|</[Pp][Rr][Ee]>\\)" 0 font-lock-function-name-face t)
   ;; override html markup in strings and comments
   ;; show variables in args field of comment, overrides comments
   '("\\(^\\s-*\\|[[]\\)\"\\([_A-Z][a-zA-Z0-9_]*\\)\"\\s-*:" 2 font-lock-variable-name-face t))
  "Font lock description of additional comment/2 expressions.")

(defconst eclipse-font-lock-keywords-1
  (append
   eclipse-font-lock-basic
   eclipse-font-lock-basic-infix
   eclipse-font-lock-colondash)
  "Basic (Prolog) expressions for font-lock mode.")

(defconst eclipse-font-lock-keywords-2
  (append
   eclipse-font-lock-low
   eclipse-font-lock-basic
   eclipse-font-lock-low-builtins
   eclipse-font-lock-low-infix
   eclipse-font-lock-colondash)
  "Essential ECLiPSe expressions for font lock mode.")

(defconst eclipse-font-lock-keywords-3
  (append
   eclipse-font-lock-low
   eclipse-font-lock-basic
   eclipse-font-lock-medium-builtins
   eclipse-font-lock-medium
   eclipse-font-lock-low-infix
   eclipse-font-lock-colondash)   
  "Highlights ECLiPSe expressions except comment/2.")

(defconst eclipse-font-lock-keywords-4
  (append
   eclipse-font-lock-keywords-3
   eclipse-comment-font-lock)
  "Highlights all ECLiPSe expressions.")

(defconst eclipse-font-lock-keywords
  (cond ((= eclipse-font-lock-default 0) nil)
	((= eclipse-font-lock-default 1) eclipse-font-lock-keywords-1)
	((= eclipse-font-lock-default 2) eclipse-font-lock-keywords-2)
	((= eclipse-font-lock-default 3) eclipse-font-lock-keywords-3)
	((= eclipse-font-lock-default 4) eclipse-font-lock-keywords-4))
  "Additional expressions to highlight in ECLiPSe mode.")

(if eclipse-xemacs
    (put 'eclipse-mode 'font-lock-keywords '(eclipse-font-lock-keywords nil))
  (put 'eclipse-mode 'font-lock-defaults '(eclipse-font-lock-keywords nil)))

;; make mode map
(defun eclipse-mode-variables ()
  (set-syntax-table eclipse-mode-syntax-table)
  (setq local-abbrev-table eclipse-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "%%\\|$\\|" page-delimiter)) ;'%%..'
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'eclipse-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-column)
  (setq comment-column 0)
  (make-local-variable 'imenu-case-fold-search)
  (setq imenu-case-fold-search nil)
  (setq case-fold-search nil)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression eclipse-imenu-generic-expression)
  (make-local-variable 'imenu-syntax-alist)
  (setq imenu-syntax-alist '(("+-*/.<>=?!$%_&~^:" . "w")))
  (make-local-variable 'imenu-prev-index-position-function)
  (setq imenu-prev-index-position-function
	eclipse-imenu-prev-index-position-function)
  (make-local-variable 'imenu-extract-index-name-function)
  (setq imenu-extract-index-name-function
	eclipse-imenu-extract-index-name-function)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'eclipse-indent-line)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function eclipse-imenu-create-index-function)
  (setq imenu-sort-function 'imenu--sort-by-name))

(defun eclipse-mode-commands (map)
  "Contains the key-bindings for the major ECLiPSe mode.
The following commands are available:

\\{eclipse-mode-map}"
  (define-key map "\r" 'eclipse-next-line)
  (define-key map "\t" 'eclipse-indent-line)
  (define-key map "\M-\C-\\" 'eclipse-indent-region)
  (define-key map "\M-\t" 'eclipse-indent-region)
  (define-key map "\M-[" 'eclipse-dabbrev-expand)
  (define-key map "\M-/" 'eclipse-dabbrev-expand0)
  (define-key map "\M-]" 'eclipse-dabbrev-completion)
  (define-key map "\C-c[" 'eclipse-dabbrev-expand1)
  (define-key map "\C-c]" 'eclipse-dabbrev-completion1)
  (define-key map "\C-cb\t" 'eclipse-indent-buffer)
  (define-key map "\C-cp\t" 'eclipse-indent-predicate)
  (define-key map "\C-cq\t" 'eclipse-indent-clause)
; <backtab> seems to be defined differently in almost every window
; manager. so you've got to customize it...
  (define-key map "\C-c " 'eclipse-insert-tab)
  (define-key map eclipse-backtab 'eclipse-insert-tab)
  (define-key map "\C-l" 'eclipse-recenter)
  (define-key map "\C-c\C-e" 'run-eclipse)
  (define-key map "\C-c\C-q" 'stop-eclipse)
  (define-key map "\C-c\C-k" 'kill-eclipse)
  (define-key map "\C-c\C-t" 'eclipse-start-tools)
  (define-key map "\C-c\C-g" 'eclipse-run-region)
  (define-key map "\C-c\C-b" 'eclipse-compile-buffer)
  (define-key map "\C-c\C-v" 'eclipse-compile-region)
  (define-key map "\C-c\C-y" 'eclipse-compile-region-and-go)
  (define-key map "\C-cvb" 'eclipse-check-buffer)
  (define-key map "\C-cvv" 'eclipse-check-region)
  (define-key map "\C-cvp" 'eclipse-check-predicate)
  (define-key map "\C-cvc" 'eclipse-check-clause)
  (define-key map "\C-c\C-h" 'eclipse-call-help)
  (define-key map "\C-cc" 'eclipse-comment-region)
  (define-key map "\C-cr" 'eclipse-uncomment-region)
  (define-key map "\C-ci" 'eclipse-invert-comment-region)
  (define-key map "\C-c\C-f" 'eclipse-autolinebreak-toggle)
  (define-key map "\C-c\C-j" 'eclipse-autoindent-toggle)
  (define-key map "\C-ca" 'eclipse-anonymise-variables)
  (define-key map "\C-c\C-a" 'eclipse-anonymous-variables)
  (define-key map "\C-cmb" 'eclipse-mark-buffer)
  (define-key map "\C-cmp" 'eclipse-mark-predicate)
  (define-key map "\C-cmq" 'eclipse-mark-clause)
  (define-key map "\M-\C-a" 'eclipse-goto-predicate-begin)
  (define-key map "\M-\C-e" 'eclipse-goto-predicate-end)
  (define-key map "\M-a" 'eclipse-goto-clause-begin)
  (define-key map "\M-e" 'eclipse-goto-clause-end)
  (define-key map "\C-c\C-z" 'eclipse-quick-jumps-toggle)
  (define-key map "\C-ct" 'eclipse-insert-predicate-template)
  (define-key map "\C-cs" 'eclipse-insert-predicate-spec)
  (define-key map "\C-c/" 'eclipse-insert-comment-pred-short)
  (define-key map "\C-c\\" 'eclipse-insert-comment-pred-full)
  (define-key map "\M-\C-m" 'eclipse-insert-clause-head)
  (define-key map "\M-\C-i" 'eclipse-insert-clause-head-empty)
  (define-key map "\C-ch" 'eclipse-highlight)
  (define-key map "\C-cd" 'eclipse-dehighlight)
  (define-key map "\C-c>" 'eclipse-goto-highlight-forward)
  (define-key map "\C-c<" 'eclipse-goto-highlight-backward))

(defun eclipse-outline-define-map (map)
  (define-key map "\C-c@@" 'eclipse-outline-mark-subtree)
  (define-key map "\C-c@n" 'eclipse-outline-next-visible-heading)
  (define-key map "\C-c@p" 'eclipse-outline-previous-visible-heading)
  (define-key map "\C-c@u" 'eclipse-outline-up-heading)
  (define-key map "\C-c@f" 'eclipse-outline-forward-same-level)
  (define-key map "\C-c@b" 'eclipse-outline-backward-same-level)
  (define-key map "\C-c@h" 'eclipse-hide-predicates)
  (define-key map "\C-c@t" 'eclipse-hide-predicate)
  (define-key map "\C-c@c" 'eclipse-hide-clauses)
  (define-key map "\C-c@e" 'eclipse-hide-clause)
  (define-key map "\C-c@l" 'eclipse-hide-block)
  (define-key map "\C-c@a" 'eclipse-show-all)
  (define-key map "\C-c@s" 'eclipse-show-predicates)
  (define-key map "\C-c@r" 'eclipse-show-predicate)
  (define-key map "\C-c@d" 'eclipse-show-clauses)
  (define-key map "\C-c@m" 'eclipse-show-clause)
  (define-key map "\C-c@k" 'eclipse-show-block))

(unless eclipse-mode-map
  (setq eclipse-mode-map (make-sparse-keymap))
  (eclipse-mode-commands eclipse-mode-map))

;; define menus
(easy-menu-define
 eclipse-process-menu eclipse-mode-map
 "ECLiPSe-Process Menu in ECLiPSe mode.
Contains commands that are associated with an inferior ECLiPSe process."
 '("ECLiPSe-Process"
   ["Run ECLiPSe" run-eclipse t]
   ["Stop ECLiPSe" stop-eclipse t]
   ["Kill ECLiPSe" kill-eclipse t]
   "--"
   ["Compile buffer" eclipse-compile-buffer t]
   ["Compile region" eclipse-compile-region t]
   ["Run region" eclipse-run-region t]
   "--"
   ["Start TkTools" eclipse-start-tools t]
   "--"
   ["Call ECLiPSe help" eclipse-call-help t]))

(easy-menu-define
 eclipse-edit-menu eclipse-mode-map
 "ECLiPSe-Edit Menu in ECLiPSe mode.
Contains commands that are associated with editing an ECLiPSe file."
 '("ECLiPSe-Edit"
   ("Indent"
    ["Indent line" eclipse-indent-line t]
    ["Indent region" eclipse-indent-region t]
    ["Indent buffer" eclipse-indent-buffer t]
    ["Indent predicate" eclipse-indent-predicate t]
    ["Indent clause" eclipse-indent-clause t])
   ("Mark"
    ["Mark buffer" eclipse-mark-buffer t]
    ["Mark predicate" eclipse-mark-predicate t]
    ["Mark clause" eclipse-mark-clause t])
   ("Comment"
    ["Comment out region" eclipse-comment-region t]
    ["Uncomment region" eclipse-uncomment-region t]
    ["Invert commenting of region" eclipse-invert-comment-region t])
   ("Text"
    ["Go to beginning of predicate" eclipse-goto-predicate-begin t]
    ["Go to end of predicate" eclipse-goto-predicate-end t]
    ["Go to beginning of clause" eclipse-goto-clause-begin t]
    ["Go to end of clause" eclipse-goto-clause-end t]
    "--"
    ["Highlight current word" eclipse-highlight t]
    ["Remove highlighting" eclipse-dehighlight t]
    "--"
    ["Re-fontify & re-center" eclipse-recenter t])
   ("Edit"
    ["Anonymise variables in region" eclipse-anonymise-variables t]
    ["Replace with anonymous variables" eclipse-anonymous-variables t]
    "--"
    ["Insert predicate template" eclipse-insert-predicate-template t]
    ["Insert predicate specification" eclipse-insert-predicate-spec t]
    ["Insert clause head" eclipse-insert-clause-head t]
    "--"
    ["Insert comment/2 template" eclipse-insert-comment-pred-short t]
    ["Insert comment/2 template with arguments" eclipse-insert-comment-pred-full t])
   ("Check"
    ["Check buffer" eclipse-check-buffer t]
    ["Check region" eclipse-check-region t]
    ["Check predicate" eclipse-check-predicate t]
    ["Check clause" eclipse-check-clause t]
    "--"
    ["List libraries" eclipse-check-load-libraries t])
   ("Outline"
    ("Hide"
     ["Hide All Predicates" eclipse-hide-predicates t]
     ["Hide Predicate" eclipse-hide-predicate t]
     ["Hide All Clauses" eclipse-hide-clauses t]
     ["Hide Clause" eclipse-hide-clause t]
     ["Hide Block" eclipse-hide-block t])
    ("Show"
     ["Show All Predicates" eclipse-show-predicates t]
     ["Show Predicate" eclipse-show-predicate t]
     ["Show All Clauses" eclipse-show-clauses t]
     ["Show Clause" eclipse-show-clause t]
     ["Show Block" eclipse-show-block t]
     ["Show All" eclipse-show-all t])
    ("Headings"
     ["Previous Same Level" eclipse-outline-backward-same-level t]
     ["Next Same Level" eclipse-outline-forward-same-level t]
     ["Previous" eclipse-outline-previous-visible-heading t]
     ["Next" eclipse-outline-next-visible-heading t]
     ["Up" eclipse-outline-up-heading t])
    "--"
    ["Speedbar on/off" eclipse-speedbar-toggle
     :style toggle
     :selected eclipse-speedbar-selected])
   "--"
   ("Preferences"
     ["Auto-line-break on/off" eclipse-autolinebreak-toggle
      :style toggle
      :selected eclipse-autolinebreak-selected]
     ["Auto-indent on/off" eclipse-autoindent-toggle
      :style toggle
      :selected eclipse-autoindent-selected]
     ["'Classic' indentation on/off" eclipse-indent-toggle
      :style toggle
      :selected eclipse-indent-mode]
     ["Quick jumps on/off" eclipse-quick-jumps-toggle
      :style toggle
      :selected eclipse-quick-jumps-selected]
     "--"
     ("Font Lock"
      ["Font Lock Off" eclipse-font-lock-0 t]
      ["Font Lock Level 1 (Basic)" eclipse-font-lock-1 t]
      ["Font Lock Level 2 (Low)" eclipse-font-lock-2 t]
      ["Font Lock Level 3 (Medium)" eclipse-font-lock-3 t]
      ["Font Lock Level 4 (High)" eclipse-font-lock-4 t]))))

;;;###autoload
(defun eclipse-mode ()
  "Major mode for editing ECLiPSe code.

Commands:
\\{eclipse-mode-map}

Entry to this mode calls the value of `eclipse-mode-hook' if that value is
non-nil.

The auto-line-break mode is set to on on start-up. It can be toggled by
calling \\[eclipse-autolinebreak-toggle], or customised by setting the variable 
`eclipse-autolinebreak-selected'.
A non-nil value means that auto-line-break is on.

The auto-indent mode is set to on on start-up. It can be toggled by calling
\\[eclipse-autoindent-toggle], or customised by setting the variable `eclipse-autoindent-selected'.
A non-nil value means that auto-indent is on.

The tab mode is set to \"use space characters\" on start-up. It can be
toggled by calling \\[eclipse-tab-mode-toggle], or customised by setting the
variable `eclipse-tab-mode'. A non-nil value means that tab characters are
used is possible, otherwise space characters only.

The width of the initial indentation at the beginning of a line is stored in
the variable `eclipse-tab-width'. Further indentations, after open brackets,
use the value stored in `eclipse-indent-width'.

If `eclipse-indent-mode' is set to a non-nil value, indentation will always
increase/decrease by `eclipse-indent-width'.
The default is nil. Toggling the variable will also set `eclipse-tab-width' to `eclipse-indent-width'.

If `eclipse-first-line-std-indent' is set to a non-nil value, the first line
in a clause will always be indented using `eclipse-tab-width'. The default
value is nil.

The key for inserting additional tab characters (or the equivalent number of
space characters) is defined in `eclipse-backtab'.

Text can be indented using \\[eclipse-indent-line], \\[eclipse-indent-region], \\[eclipse-indent-predicate], and \\[eclipse-indent-clause].
Note that the indentation of regions can be slow.

Regions can be marked using \\[eclipse-mark-buffer], \\[eclipse-mark-predicate], and \\[eclipse-mark-clause].

The text can be navigated with \\[eclipse-goto-clause-begin], \\[eclipse-goto-clause-end], \\[eclipse-goto-predicate-begin], and \\[eclipse-goto-predicate-end]
If `eclipse-quick-jumps-selected' is non-nil, the functions jump to the next
empty line. Otherwise, the correct position for the jump is computed. Since
this may be slow, the default value for the variable is t.

\\[eclipse-recenter] re-centers and re-fontifies the buffer.

Regions can be commented out with \\[eclipse-comment-region].
\\[eclipse-uncomment-region] deletes leading '%' characters, and
\\[eclipse-invert-comment-region] combines the two previous actions.

Variables in a region can be anonymised with \\[eclipse-anonymise-variables]: '_' will be added to each
variable. Variables in a region can be replaced by '_' with \\[eclipse-anonymous-variables].

The function \\[eclipse-insert-predicate-template] adds a template for the preceding predicate in the style
'foo(,,)' and jumps to the position of the first ',' (if present).
\\[eclipse-insert-predicate-spec] adds the specification for the preceding predicate in the style 'foo/3'.

\\[eclipse-insert-clause-head] adds an empty clause head for the current (preceding) predicate in the
style 'foo(,,) :-' and jumps to the first ',' (if present).

\\[eclipse-insert-comment-pred-short] insert ':- comment(,).'.
\\[eclipse-insert-comment-pred-full] inserts a full 'comment/2' entry, including the arguments, for the
following predicate.

Other variables not listed in this description shouldn't be changed.

The fontification can be set to four different levels:
\\[eclipse-font-lock-1], \\[eclipse-font-lock-2], \\[eclipse-font-lock-3],
\\[eclipse-font-lock-4], or shut off using \\[eclipse-font-lock-0].
The default value is stored in `eclipse-font-lock-default'. The default value
is 3.

In XEmacs, the colours for the fontification are set in the
`eclipse-*-face-val' variables.

The ECLiPSe mode supports outlining text:

To hide text use \\[eclipse-hide-predicates] (hide all predicates), \\[eclipse-hide-predicate] (hide current
predicate), \\[eclipse-hide-clauses] (hide all clauses), \\[eclipse-hide-clause] (hide current clause), and
\\[eclipse-hide-block] (hide current block).

To show text use \\[eclipse-show-all] (show the whole buffer), \\[eclipse-show-predicate] (show the current
predicate), \\[eclipse-show-clause] (show the current clause), and \\[eclipse-show-block] (show the current
block).

\\[eclipse-outline-mark-subtree] marks the current subtree, and \\[eclipse-outline-headers-as-kill]
copies the headers in the region onto the kill ring.

To navigate between headings use \\[eclipse-outline-next-visible-heading] (next visible heading),
\\[eclipse-outline-previous-visible-heading] (previous visible heading), \\[eclipse-outline-up-heading] (previous heading of upper level),
\\[eclipse-outline-forward-same-level] (next heading of same level), and \\[eclipse-outline-backward-same-level] (previous heading of same
level).

There are more functions, e.g. dynamic expansion of ECLiPSe keywords.
Please look at the list of keybindings."
  (interactive)
  (kill-all-local-variables)
  (use-local-map eclipse-mode-map)
  (setq major-mode 'eclipse-mode
	mode-name "Eclipse")
  (eclipse-mode-variables)
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(eclipse-font-lock-keywords nil)
	indent-tabs-mode eclipse-tab-mode)
  (easy-menu-add eclipse-edit-menu)
  (easy-menu-add eclipse-process-menu)
  ;; always start with auto-line-break mode
  (when eclipse-autolinebreak-selected
    (auto-fill-mode 1)
    (setq auto-fill-function 'eclipse-auto-fill-function))
  ;; start outline minor mode
  (eclipse-outline eclipse-mode-map)
  ;; start speedbar, if selected
  (eclipse-check-speedbar-supported)
  (eclipse-start-speedbar-if-selected)
  (run-hooks 'eclipse-mode-hook))

;;
;; Font lock toggle
;;

(defun eclipse-font-lock-0 ()
  "Switch font lock off."
  (interactive)
  (font-lock-mode 0))

(defun eclipse-font-lock-1 ()
  "Switch font lock to basic level."
  (interactive)
  (font-lock-mode 1)
  (setq font-lock-keywords eclipse-font-lock-keywords-1)
  (font-lock-fontify-buffer))

(defun eclipse-font-lock-2 ()
  "Switch font lock to low level."
  (interactive)
  (font-lock-mode 1)
  (setq font-lock-keywords eclipse-font-lock-keywords-2)
  (font-lock-fontify-buffer))

(defun eclipse-font-lock-3 ()
  "Switch font lock to medium level."
  (interactive)
  (font-lock-mode 1)
  (setq font-lock-keywords eclipse-font-lock-keywords-3)
  (font-lock-fontify-buffer))

(defun eclipse-font-lock-4 ()
  "Switch font lock to high level."
  (interactive)
  (font-lock-mode 1)
  (setq font-lock-keywords eclipse-font-lock-keywords-4)
  (font-lock-fontify-buffer))

;;
;; ECLiPSe speedbar support
;;

(defun eclipse-speedbar-toggle()
  "Toggle speedbar on/off.

If necessary, the extension '.ecl' is added to the list of supported
extensions."
  (interactive)
  (setq eclipse-speedbar-selected (not eclipse-speedbar-selected))
  (if (not eclipse-speedbar-selected)
      (speedbar -1)
    (require 'speedbar)
    (speedbar)
    (unless eclipse-speedbar-supported
      (eclipse-add-speedbar-support)
      (speedbar-refresh))))

(defun eclipse-start-speedbar-if-selected ()
  ;; start speedbar if variable eclipse-speedbar-selected is t at startup
  (cond ((eclipse-speedbar-loaded)
	 (setq eclipse-speedbar-selected t)
	 (unless eclipse-speedbar-supported
	   (eclipse-add-speedbar-support)
	   (speedbar-refresh)))
	((not eclipse-speedbar-selected) t)
	(t
	 (speedbar)
	 (unless eclipse-speedbar-supported
	   (eclipse-add-speedbar-support)
	   (speedbar-refresh)))))

(defun eclipse-speedbar-loaded ()
  ;; check if speedbar is already loaded
  (let ((list (buffer-list)) (flag nil))
    (while (and list (not flag))
      (if (string-match "SPEEDBAR" (buffer-name (car list)))
	  (setq flag t)
	(setq list (cdr list))))
    flag))

(defun eclipse-add-speedbar-support ()
  ;; add .ecl to list of supported extensions if not in list
  (unless (eclipse-check-speedbar-supported)
    (speedbar-add-supported-extension ".ecl")
    (setq eclipse-speedbar-supported t)))

(defun eclipse-check-speedbar-supported ()
  ;; check if .ecl is supported speedbar extension. if not, add it
  (if (not (eclipse-speedbar-loaded))
      t ; speedbar not loaded: do nothing
    (let ((list speedbar-supported-extension-expressions) (flag nil) el)
      (while (and list (not flag))
	(setq el (car list)
	      list (cdr list))
	(when (string-match (concat "\\" el) ".ecl")
	  (message el)
	  (setq flag t
		eclipse-speedbar-supported t)))
      flag)))

;;
;; ECLiPSe mode auto-fill
;;

(defun eclipse-autolinebreak-toggle ()
  "Toggle auto-line-break on/off in ECLiPSe mode.

When auto-line-break is on, strings, comments, and expressions are broken up
if they get too long to fit into a line."
  (interactive)
  (setq eclipse-autolinebreak-selected (not eclipse-autolinebreak-selected))
  (if (not auto-fill-function)
      (setq auto-fill-function 'eclipse-auto-fill-function)
    (setq auto-fill-function nil))
  (force-mode-line-update))

(defun eclipse-autoindent-toggle ()
  "Toggle auto-indent on/off in ECLiPSe mode.

When auto-indent is on, lines are automatically indented after pressing <RET>."
  (interactive)
  (setq eclipse-autoindent-selected (not eclipse-autoindent-selected)))

(defun eclipse-indent-toggle ()
  "Toggle fixed indentation indent on/off in ECLiPSe mode.

When fixed indentation is on, indentation increases/decreases by `eclipse-indent-width' columns. When toggled on, `eclipse-tab-width' is set to `eclipse-indent-width', when toggled off, the variable is set to its previous value."
  (interactive)
  (setq eclipse-indent-mode (not eclipse-indent-mode))
  (if eclipse-indent-mode
      (setq eclipse-old-tab-width eclipse-tab-width
	    eclipse-tab-width eclipse-indent-width)
    (setq eclipse-tab-width eclipse-old-tab-width)))

(defun eclipse-quick-jumps-toggle ()
  "Toggle quick jumps on/off in ECLiPSe mode.

When quick jumps are on, the 'go to' commands jump to the next empty lines.
Otherwise, the correct target for the jump is computed, which can be quite
slow."
  (interactive)
  (setq eclipse-quick-jumps-selected (not eclipse-quick-jumps-selected)))

(defun eclipse-next-line ()
  "This function is called when <RET> is pressed.
If auto-indent is on, the next line is automatically indented."
  ;; handle process marks myself, since older comint.el do not have
  ;; functions-comint-set process-mark and comint-goto-process-mark
  (interactive)
  (let (string (flag nil) proc beg aux end (name (buffer-name)))
    (cond ((string-equal name "*eclipse*")
	   ;; in eclipse process buffer?
	   (cond
	    ;; normal eclipse process command?
	    ((save-excursion
	       (when (eobp)
		 (backward-char))
	       (skip-chars-backward " \t\n")
	       (setq beg (point))
	       (backward-char)
	       (looking-at "[^.]\\."))
	     (goto-char (+ 1 beg))
	     (setq end (point))
	     (goto-char (process-mark (get-buffer-process (current-buffer))))
	     (setq beg (point))
	     (eclipse-end-of-clause)
	     (setq string (buffer-substring beg (point)))
	     (while (> end (point))
	       (forward-line)
	       (beginning-of-line)
	       (skip-chars-forward " \t\n")
	       (setq aux (point))
	       (eclipse-end-of-clause)
	       (setq string (concat string (buffer-substring aux (point)))))
	     (insert "\n")
	     (setq string (concat string "\n"))
	     (eclipse-set-process-mark)
	     (when eclipse-emacs-21
	       (eclipse-change-face beg))
	     (process-send-string "eclipse" string)
	     (if eclipse-emacs-21
		 (comint-add-to-input-history string)
	       (ring-insert comint-input-ring string)))
	    ;; eclipse debugger command?
	    ((save-excursion
	       (when eclipse-emacs-21
		 (beginning-of-line)
		 (backward-char))
	       (beginning-of-line)
	       (looking-at ".*%>"))
	     (save-excursion
	       (search-backward "%>")
	       (forward-char 3)
	       (setq beg (point)))
	     (cond ((eq beg (point))
		    (eclipse-set-process-mark)
		    (process-send-string "eclipse" "\x0d"))
		   (t
		    (setq string (buffer-substring beg (point)))
		    (when eclipse-emacs-21
		      (eclipse-change-face beg))
		    (insert "\n")
		    (eclipse-set-process-mark)
		    (process-send-string "eclipse" string))))
	    ;; eclipse interruption command?
	    ((save-excursion
	       (when eclipse-emacs-21
		 (beginning-of-line)
		 (backward-char))
	       (beginning-of-line)
	       (looking-at "interruption"))
	     (save-excursion
	       (search-backward "?")
	       (forward-char 2)
	       (setq beg (point)))
	     (setq string (buffer-substring beg (point)))
	     (if eclipse-emacs-21 (eclipse-change-face beg))
	     (insert "\n")
	     (eclipse-set-process-mark)
	     (process-send-string "eclipse" string))
	    ;; eclipse tracer command?
	    ((save-excursion
	       (when eclipse-emacs-21
		 (beginning-of-line)
		 (backward-char))
	       (beginning-of-line)
	       (looking-at ".*[]:?]")
	       (unless (looking-at ".*\\[")
		 (when (looking-at "\\(set\\|.*[?]\\)")
		   (setq flag t)))
	       (setq proc (get-buffer-process (current-buffer))
		     beg (process-mark proc)))
	     (setq string (buffer-substring beg (point)))
	     (when eclipse-emacs-21
	       (eclipse-change-face beg))
	     (insert "\n")
	     (when flag
	       (setq string (concat string "\n")))
	     (eclipse-set-process-mark)
	     (process-send-string "eclipse" string))
	    ;; else: regular line
	    (t
	     (insert "\n")
	     (if eclipse-emacs-21
		 (insert "\t") ;; if emacs 21, do not attempt to indent
	       (eclipse-indent-line)))))
	  (t
	   ;; else in eclipse code buffer
	   (newline)
	   (if eclipse-autoindent-selected (eclipse-indent-line t))))))

;; the next two functions are copied & adapted from comint.el --- general
;; command interpreter in a window stuff
;; Copyright (C) 1988, 90, 92, 93, 94, 95, 96, 97, 98, 99, 2000, 2001
;;	Free Software Foundation, Inc.
;; Authors: Olin Shivers <shivers@cs.cmu.edu>
;;	    Simon Marshall <simon@gnu.org>)
(defun eclipse-set-process-mark ()
  (let ((proc (get-buffer-process (current-buffer))))
    (set-marker (process-mark proc) (point))))

(defun eclipse-change-face (beg)
  (let ((over (make-overlay beg (point) nil nil t)))
    (overlay-put over 'field 'input)
    (overlay-put over 'face 'comint-highlight-input)))

;; The autofill function was copied & adapted from simple.el --- basic
;; editing commands for Emacs
;; Copyright (C) 1985, 86, 87, 93, 94, 95, 96, 97, 98, 99, 2000, 2001
;;        Free Software Foundation, Inc.
;;
;; the breaking-up of strings, quoted atoms, and comments needs special
;;   handling
;; comments partly deleted: look up in simple.el
(defun eclipse-auto-fill-function ()
  "Auto-fill function for ECLiPSe mode.

Example:
pred(Var) :-
        another_pred(
                        \"This is a very long string that will be\"
                        \" automatically wrapped around\", %% This is a
                                                         %% comment
                        A + Really + Very + Long + And + Nonsensical +
                        Expression
                    )."
  (let (fc give-up (ep nil))
    (if (or (null (setq fc (current-fill-column)))
	    (<= (current-column) fc))
	nil ;; Auto-filling not required
      (while (and (not give-up) (> (current-column) fc))
	;; Determine where to split the line.
	(let* (bol-point first-break-point atom-break-point
	       (fill-point
		(let ((opoint (point)))
		  (save-excursion
		    (beginning-of-line)
		    (setq bol-point (point))
		    (move-to-column (1+ fc))
		    ;; Move back to the point where we can break the line.
		    (re-search-backward "[-/+*=<>:? \t,;]")
		    (cond ((looking-at "[-/+*=<>:?]")
			   (re-search-forward "[^-/+*=<>:?]" (point-max) t)
			   (backward-char))
			  ((looking-at "[,;]")
			   (forward-char))
			  (t t))
		    ;; check if we're inside a quoted atom
		    ;; if so, break before the start of the quoted atom
		    (setq first-break-point (point))
		    (beginning-of-line)
		    (let ((cc nil) (ac nil) (sc nil))
		      (while (< (point) first-break-point)
			(forward-char 1)
			(cond ((looking-at "\"")
			       (or ac cc (setq sc (not sc))))
			      ((looking-at "%")
			       (or sc ac (setq cc t)))
			      ((looking-at "'")
			       (or sc cc
				   (progn
				     (setq ac (not ac))
				     (or (not ac)
					 (save-excursion
					   (re-search-backward "[-/+*=<>:? \t,;]")
					   (when (looking-at "[-/+*=<>:?]")
					     (re-search-forward "[^-/*+=<>:?]" (point-max) t)
					     (backward-char))
					   (setq atom-break-point (point)))))))
			      (t t)))
		      (if ac (goto-char atom-break-point)))
		    ;; If we find nowhere on the line to break it,
		    ;; break after one word.
		    (cond ((bolp)
			   (re-search-forward "[ \t]" opoint t))
			  ((looking-at "[ \t]")
			   ;; Break the line at word boundary.
			   (skip-chars-backward " \t"))
			  ((looking-at "[-/*+=<>:?]")
			   (re-search-forward "[^-/+*=<>:?]" (point-max) t)
			   (backward-char))
			  ((eq (point) first-break-point) t) ;; same point: break here
			  (t (forward-char))) ;; Break the line after/before \c|.
		    (if (and enable-multibyte-characters
			     (not eclipse-xemacs)
			     (not (and (eq (charset-after (1- (point))) 'ascii)
				       (eq (charset-after (point)) 'ascii))))
			;; special function for the charset of non-ascii
			;; character to find the correct break point.
			;; Don't do in XEmacs, charset-after not available.
			(fill-find-break-point bol-point))
		    ;; move back before any whitespace here.
		    (skip-chars-backward " \t")
		    ;; that's the fill-point
		    (point)))))
	  ;; See whether the place we found is any good.
	  (if (save-excursion
		(goto-char fill-point)
		(and (not (bolp))
		     (not (save-excursion (skip-chars-forward " \t") (eolp)))))
	      ;; There is no use breaking at end of line...
	      ;; ...or beginning of line.
	      ;; (test for comment start deleted)
	      ;; Ok, we have a useful place to break the line.  Do it.
	      (let (counter (colmn nil) (prev-column (current-column)))
		;; now we must determine, if the break-point is
		;; (a) in a comment, or
		;; (b) in a string, or
		;; (c) in a regular line
		;; if (a), break the line, insert spaces until the beginning
		;;         of the comment, and insert as many percentage signs
		;; if (b), add \", break the line, indent, add \"
		;; if (c), break the line and indent
		;; quoted atoms have been dealt with while finding the
		;; break point. dealing with comments should be done at that
		;; point, too...
		(cond ((save-excursion
			 ;; inside a string?
			 (goto-char fill-point)
			 (setq counter 0
			       colmn nil)
			 (while (not (bolp))
			   (cond ((looking-at "\"")
				  (setq counter (+ counter 1)))
				 ((and (looking-at "%") (= (mod counter 2) 0))
				  (setq colmn t
					counter 0))
				 (t t))
			   (backward-char))
			 (not (and colmn (= (mod counter 2) 0)))
			 (> counter 0)
			 (= (mod counter 2) 1))
		       ;; close string before fill point,
		       ;; open string anew after indenting
		       (if (not eclipse-autoindent-selected)
			   (save-excursion
			     (goto-char fill-point)
			     (insert "\n"))
			 (save-excursion
			   (goto-char fill-point)
			   (if (save-excursion
				 (backward-char)
				 (looking-at "\""))
			       (progn (backward-char 2) (insert "\n"))
			     (insert "\"\n\"")))
			 (eclipse-indent-line nil t)
			 (skip-chars-forward " \t")))
		      ((save-excursion
			 ;; inside a comment?
			 (beginning-of-line)
			 (setq colmn nil)
			 (while (and (< (point) fill-point) (not colmn))
			   (cond ((looking-at "\"")
				  (eclipse-goto-end-of-string))
				 ((looking-at "'")
				  (eclipse-goto-end-of-quote))
				 ((looking-at "%")
				  (setq colmn (point)))
				 (t (forward-char))))
			 colmn)
		       ;; continue comment in next line
		       (if (not eclipse-autoindent-selected)
			   (save-excursion
			     (goto-char fill-point)
			     (insert "\n% "))
			 (save-excursion
			   (goto-char colmn)
			   (setq colmn (current-column)
				 counter 0)
			   (while (looking-at "%")
			     (forward-char)
			     (setq counter (+ counter 1)))
			   (goto-char fill-point)
			   (insert "\n")
			   (indent-to colmn)
			   (insert (make-string counter 37)))))
		      (t
		       (save-excursion
			 (goto-char fill-point)
			 (insert "\n")
			 (if eclipse-autoindent-selected
			     (eclipse-indent-line nil t))
			 (setq ep (point)))))
		;; If making the new line didn't reduce the hpos of
		;; the end of the line, then give up now.
		(if (>= (current-column) prev-column) (setq give-up t)))
	    ;; No good place to break => stop trying.
	    (setq give-up t))))
      (if (and ep (< (point) ep)) (goto-char ep))))) 

;;
;; ECLiPSe mode commenting in & out
;;

(defun eclipse-recenter ()
  "(Re-)Fontify the current buffer as ECLiPSe code and re-center it."
  (interactive)
  (let ((pos (- (point-max) (point))))
    (font-lock-fontify-buffer)
    (recenter)
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

(defun eclipse-change-comment-region (flag)
  ;; change commenting of current region
  (let ((pos (point)) (regionend (region-end)) (max 0) type_str msg)
    (cond ((= flag 1) (setq type_str "Commenting out"))
	  ((= flag 2) (setq type_str "Un-commenting"))
	  ((= flag 3) (setq type_str "Inverting commenting of")))
    (setq msg (concat type_str " region..."))
    (message msg)
    (goto-char (region-beginning))
    (beginning-of-line)
    (while (< (point) regionend)
      (cond ((and (or (= flag 2) (= flag 3)) (looking-at "%% "))
	     (delete-char 3)
	     (setq max (+ max 3)
		   regionend (- regionend 3)))
	    ((and (or (= flag 2) (= flag 3)) (looking-at "%%\t"))
	     (delete-char 2)
	     (setq max (+ max 2)
		   regionend (- regionend 2)))
	    ((and (or (= flag 2) (= flag 3)) (looking-at "%[ \t]"))
	     (delete-char 1)
	     (setq max (+ max 1)
		   regionend (- regionend 1)))
	    ((or (= flag 1) (= flag 3))
	     (insert "%% ")
	     (setq max (- max 3)
		   regionend (+ regionend 3)))
	    (t t))
      (end-of-line)
      (unless (eobp)
	(forward-line)
	(beginning-of-line)))
    (setq msg (concat msg "done"))
    (message msg)
    (goto-char (- pos max))))

(defun eclipse-comment-region ()
  "Comment out current region."
  (interactive)
  (eclipse-change-comment-region 1))

(defun eclipse-uncomment-region ()
  "Uncomment current region."
  (interactive)
  (eclipse-change-comment-region 2))

(defun eclipse-invert-comment-region ()
  "Invert commenting of current region."
  (interactive)
  (eclipse-change-comment-region 3))

;;
;; ECLiPSe mode indentation
;;

(defun eclipse-tab-mode-toggle ()
  "Toggle tab-mode on/off in ECLiPSe mode."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)
	eclipse-tab-mode (not eclipse-tab-mode)))

(defun eclipse-insert-tab ()
  "Insert a tab character, or, if eclipse-tab-mode is off,
`eclipse-indent-width' many space characters."
  (interactive)
  (if eclipse-tab-mode (insert "\t")
    (insert (make-string eclipse-indent-width 32))))

(defun eclipse-indent-line (&optional af flag)
  "Indent current line as ECLiPSe code."
  (interactive)
  (let* ((quotes (eclipse-count-quotes))
         (pos (- (point-max) (point))) beg)	
    ;; if inside string and auto-line-break is on
    (when (and af eclipse-autolinebreak-selected quotes)
      (save-excursion
	(forward-line -1)
	(end-of-line)
	(insert "\"")))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward "[ \t]")
    (when (and af eclipse-autolinebreak-selected quotes)
      (insert "\"")
      (backward-char))
    (eclipse-indent-region-as-block beg (+ (point) 1) flag)
    (eclipse-backward-char)
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

(defun eclipse-indent-region-line (indent1)
  ;;Indent current line as ECLiPSe code.
  (let ((pos (- (point-max) (point))) beg
	(indent (if (not indent1) 0 indent1)))
    (beginning-of-line)
    (setq beg (point))
    (cond ((and (not eclipse-tab-mode) (looking-at " *\t[ \t]*"))
	   (skip-chars-forward " \t")
	   (delete-region beg (point))
	   (indent-to indent))
	  (t
	   (skip-chars-forward " \t")
	   (unless (zerop (- indent (current-column)))
	     (delete-region beg (point))
	     (indent-to indent))))
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

(defun eclipse-indent-region (&optional msg)
  "Indent current region as ECLiPSe code."
  (interactive)
  (let ((rb (region-beginning)) (re (region-end)) rbegin rend msg-str)
    (cond ((< rb re)
	   (goto-char rb)
	   (beginning-of-line)
	   (setq rbegin (point)
		 rend re))
	  (t
	   (goto-char re)
	   (beginning-of-line)
	   (setq rbegin (point)
		 rend rb)))
    (if (not msg) (setq msg-str "Indenting region...")
      (setq msg-str (concat "Indenting " msg "...")))
    (message msg-str)
    (eclipse-indent-region-as-block rbegin rend)
    (message (concat msg-str "done"))))

(defun eclipse-indent-buffer (&optional msg)
  "Indent buffer as ECLiPSe code."
  (interactive)
  (goto-char (point-min))
  (push-mark (point-max))
  (eclipse-indent-region msg))

(defun eclipse-indent-region-as-block (begin end &optional afflag)
  ;; Indent current region as ECLiPSe code
  (let ((stack '()) pos (nlflag nil) (cmtflag nil) (n 0) (indnt 0)
	(flflag 0) (level 0) aux (lstnl t) first-type first-clmn
	first-level (idtflag nil) clmn (cmt2flag nil) auxst (eobfl 0)
	(time (+ (+ eclipse-indent-timeout 1) (car (cdr (current-time)))))
	(timeflag nil) str)
    (eclipse-goto-clause-begin t)
    (while (and (< (point) end) (< eobfl 2) (not (eq timeflag 2)))
      (cond ((and (not timeflag)
		  (<= time (car (cdr (current-time)))))
	     (beep)
	     (setq str (read-from-minibuffer
			(concat "Indentation takes more than "
				(number-to-string eclipse-indent-timeout)
				" second(s). Continue? (y/n/new timeout) ")))
	     (cond ((= (string-to-char str) 121)
		    (setq timeflag 1)
		    (message "Indenting..."))
		   ((> (string-to-number str) 0)
		    (message "Do you want to save the new timeout in your custom-set variables? (y/n) ")
		    (when (= (read-char) 121)
		      (require 'cus-edit)
		      (customize-save-variable 'eclipse-indent-timeout (string-to-number str)))
		    (setq eclipse-indent-timeout (string-to-number str)
			  time (+ (+ eclipse-indent-timeout 1) (car (cdr (current-time)))))
		    (message "Indenting..."))
		   (t
		    (message "Do you want to switch auto-indent off? (y/n) ")
		    (when (= (read-char) 121)
		      (setq eclipse-autoindent-selected nil))
		    (setq timeflag 2)
		    (goto-char end))))
	    (t
	     (when (not (or idtflag (< (point) begin)))
	       (setq idtflag t))
	     (cond ((and nlflag cmtflag (looking-at "\\([ \t]*\\)\\(%+\\)"))
		    (skip-chars-forward " \t")
		    (when idtflag
		      (setq pos (point))
		      (eclipse-indent-region-line (nth 1 (car stack)))
		      (setq end (eclipse-set-end end pos)))
		    (end-of-line)
		    (unless (eobp)
		      (forward-line)
		      (beginning-of-line)))
		   (t
		    (when nlflag
		      (setq nlflag nil
			    cmtflag nil
			    lstnl t
			    n 0))
		    (skip-chars-forward " \t")
		    (setq pos (point))
		    (when (looking-at ":- comment(")
		      (setq cmt2flag t
			    flflag 0))
		    (when (equal (nth 0 (car stack)) 'cmt)
		      (setq stack (cdr stack)))
		    (if (null stack)
			(setq first-type 'nul
			      first-clmn 0
			      first-level 0)
		      (setq first-type (nth 0 (car stack))
			    first-clmn (nth 1 (car stack))
			    first-level (nth 2 (car stack))))
		    (cond ((and cmt2flag (= flflag 2) (not (null stack)))
			   (when idtflag
			     (eclipse-indent-region-line eclipse-tab-width)
			     (setq end (eclipse-set-end end pos)))
			   (setq cmt2flag nil))
			  ((or (looking-at "\n") (eobp))
			   (when (and lstnl idtflag)
			     (cond ((null stack)
				    (eclipse-indent-region-line 0))
				   ((member first-type (list 'el 'st))
				    (setq auxst stack
					  stack (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt)))
				    (unless (equal (nth 0 (car (cdr stack))) 'cd)
				      (setq stack (cdr stack)))
				    (setq indnt (nth 1 (car stack))
					  level (nth 2 (car stack))
					  stack (cdr stack))
				    (when eclipse-indent-mode
					(setq indnt (* level eclipse-indent-width)))
				    (if (not indnt)
					(eclipse-indent-region-line eclipse-tab-width)
				      (eclipse-indent-region-line indnt))
				    (setq stack auxst))
				   (t (eclipse-indent-region3 stack flflag)))
			     (setq end (eclipse-set-end end pos)))
			   (setq nlflag t)
			   (if (= flflag 1) (setq flflag 2))
			   (unless (eobp)
			     (forward-line)
			     (beginning-of-line)))
			  ((looking-at ",")
			   (cond (lstnl 
				  (when idtflag
				    (cond ((null stack)
					   (eclipse-indent-region-line 0))
					  ((equal first-type 'el)
					   (setq stack (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'if 'sc 'do)))
					   (unless (equal (nth 0 (car (cdr stack))) 'cd)
					     (setq stack (cdr stack)))
					   (setq indnt (nth 1 (car stack))
						 level (nth 2 (car stack))
						 stack (cdr stack))
					   (when eclipse-indent-mode
					       (setq indnt (* level eclipse-indent-width)))
					   (if (not indnt)
					       (eclipse-indent-region-line eclipse-tab-width)
					     (eclipse-indent-region-line indnt)))
					  (t (eclipse-standard-indent2 stack level flflag))))
				  (setq stack (cons (list 'co (current-column) level) stack)))
				 (t
				  (setq aux (eclipse-get-last-comma stack level)
					stack (nth 0 aux)
					level (nth 1 aux)
					first-type (nth 0 (car stack))
					first-clmn (nth 1 (car stack))
					first-level (nth 2 (car stack)))
				  (unless (equal first-type 'co)
				    (setq stack (cons (list 'co (current-column) level) stack)))))
			   (forward-char))
			  ((looking-at ";")
			   (when lstnl
			     (unless (= level 1)
			       (setq level (- level 1)))
			     (when idtflag
			       (setq auxst (eclipse-get-last-b (eclipse-get-last-level-stack stack level)))
			       (cond (eclipse-indent-mode
				      (eclipse-indent-region-line (* level eclipse-indent-width)))
				     ((or (= level 1) (equal (nth 0 (car (cdr auxst))) 'cd))
				      (eclipse-indent-region-line (nth 1 (car auxst))))
				     (t (eclipse-indent-region-line (nth 1 (car (cdr auxst))))))
			       (setq end (eclipse-set-end end pos))))
			   (setq level (+ level 1)
				 stack (cons (list 'sc (current-column) -1) stack))
			   (forward-char))
			  ((looking-at "\\.[ \t\n]")
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (setq stack '()
				 level 0
				 flflag 0
				 cmt2flag nil)
			   (forward-char))
			  ((and (looking-at "\\(is\\|with\\)[ \t\n]")
				(save-excursion
				  (eclipse-backward-char)
				  (looking-at (concat "[ \t]" (match-string 0)))))
			   (setq n (1- (length (match-string 0))))
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (unless eclipse-indent-mode
			     (setq level (+ level 1)))
			   (setq stack (cons (list 'inf (current-column) -1) stack))
			   (forward-char n))
			  ((and (looking-at "do[ \t\n]")
				(save-excursion
				  (eclipse-backward-char)
				  (looking-at "[ \t]do[ \t\n]")))
			   (when lstnl
			     (unless eclipse-indent-mode
			       (setq level (max 1 (- level 1))))
			     (setq stack (eclipse-get-last-level-stack stack level))
			     (when idtflag
			       (eclipse-standard-indent1 stack level (nth 1 (car stack)))
			       (setq end (eclipse-set-end end pos))))
			   (setq stack (cons (list 'do (current-column) level) stack))
			   (unless eclipse-indent-mode
			     (setq level (+ level 1)))
			   (forward-char 2))
			  ((looking-at "\\([0-9]?\\.[0-9]+\\|[a-zA-Z0-9_]+\\|(\\.)\\)")
			   (setq n (length (match-string 0)))
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el (current-column) level) stack)))
			   (forward-char n))
			  ((looking-at "[#.](")
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el (current-column) level) stack)))
			   (forward-char))
			  ((looking-at "-\\?->")
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent1 stack level eclipse-tab-width)
			     (setq end (eclipse-set-end end pos)))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el (current-column) level) stack)))
			   (setq stack (cons (list 'mt (current-column) level) stack))
			   (forward-char 4))
			  ((looking-at "\\(:- mode \\|[:?]-\\|-->\\)")
			   (setq n (length (match-string 0)))
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent1 stack level eclipse-tab-width)
			     (setq end (eclipse-set-end end pos)))
			   (setq stack (cons (list 'cd (current-column) level) stack)
				 flflag 1
				 level (+ level 1))
			   (forward-char n))
			  ((looking-at "->")
			   (when lstnl
			     (setq level (- level 1)
				   stack (cdr (eclipse-get-last-sc stack)))
			     (when idtflag
			       (eclipse-standard-indent1 stack level (nth 1 (car stack)))
			       (setq end (eclipse-set-end end pos))))
			   (cond ((eclipse-in-list stack)
				  (setq stack (cons (list 'inf (current-column) -1) stack)))
				 (t (setq stack (cons (list 'if (current-column) level) stack))))
			   (when (or lstnl (not eclipse-indent-mode))
			     (setq level (+ level 1)))
			   (forward-char 2))
			  ((looking-at "#<?=>")
			   (setq n (length (match-string 0)))
			   (when lstnl
			     (unless eclipse-indent-mode
			       (setq level (- level 1)))
			     (setq stack (eclipse-get-last-level-stack stack level))
			     (when idtflag
			       (eclipse-standard-indent1 stack level (nth 1 (car stack)))
			       (setq end (eclipse-set-end end pos))))
			   (setq stack (cons (list 'if (current-column) level) stack))
			   (unless eclipse-indent-mode
			     (setq level (+ level 1)))
			   (forward-char n))
			  ((looking-at "'")
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el (current-column) level) stack)))
			   (setq stack (cons (list 'qu (current-column) level) stack))
			   (eclipse-goto-end-of-quote))
			  ((looking-at "\"")
			   (when (and lstnl idtflag)
			     (cond (eclipse-indent-mode
				    (eclipse-indent-region-line (max eclipse-tab-width (* level eclipse-indent-width))))
				   ((equal first-type 'st)
				    (setq stack (eclipse-get-last-inf stack))
				    (eclipse-indent-region-line (nth 1 (car stack))))
				   (t (eclipse-indent-region3 stack flflag)))
			     (setq end (eclipse-set-end end pos)))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el (current-column) level) stack)))
			   (setq stack (cons (list 'st (current-column) level) stack))
			   (eclipse-goto-end-of-string))
			  ((looking-at "%+")
			   (setq n (length (match-string 0)))
			   (when (and lstnl idtflag)
			     (if (looking-at "%[^%]")
				 (eclipse-standard-indent2 stack level flflag)
			       (eclipse-indent-region-line 0))
			     (setq end (eclipse-set-end end pos)))
			   (setq stack (cons (list 'cmt (current-column) level n) stack)
				 nlflag t
				 cmtflag t)
			   (end-of-line)
			   (unless (eobp)
			     (forward-line)
			     (beginning-of-line)))
			  ((looking-at "/\\*")
			   (unless (re-search-forward "\\*/" (point-max) t)
			     (goto-char (point-max))))
			  ((looking-at "[({[]")
			   (let ((symbol (cond ((looking-at "(") 'rb)
					       ((looking-at "\\[") 'sb)
					       (t 'cb))))
			     (when (and lstnl idtflag)
			       (eclipse-standard-indent2 stack level flflag)
			       (setq end (eclipse-set-end end pos)))
			     (unless (equal (nth 0 (car stack)) 'el)
			       (setq stack (cons (list 'el (current-column) level) stack)))
			     (setq stack (cons (list symbol (current-column) level) stack)
				   level (+ level 1))
			     (forward-char)))
			  ((looking-at "[])}]")
			   (let* ((symbol (cond ((looking-at ")") 'rb)
						((looking-at "\\]") 'sb)
						(t 'cb)))
				  (auxl (cond ((looking-at ")") (list 'sb 'cb))
					      ((looking-at "\\]") (list 'rb 'cb))
					      (t (list 'rb 'sb))))
				  (auxfl (member first-type (append (list 'co 'sc 'inf 'op 'do 'if) auxl))))
			     (when auxfl
			       (when (and lstnl idtflag)
				 (eclipse-standard-indent2 stack level flflag)
				 (setq end (eclipse-set-end end pos))))
			     (setq stack (eclipse-get-last symbol stack))
			     (when (null stack)
			       (error "Empty stack. Check for '.' instead of ','"))
			     (setq indnt (nth 1 (car stack))
				   level (nth 2 (car stack))
				   stack (cdr stack))
			     (when (not auxfl)
			       (when (and lstnl idtflag)
				 (eclipse-standard-indent1 stack level indnt)
				 (setq end (eclipse-set-end end pos))))
			     (forward-char)))
			  ((looking-at "|")
			   (cond ((member first-type (list 'co 'sc 'inf 'op 'do 'if 'rb 'sb 'cb))
				  (when (and lstnl idtflag)
				    (eclipse-standard-indent2 stack level flflag)
				    (setq end (eclipse-set-end end pos))))
				 (t
				  (when (and lstnl idtflag)
				    (eclipse-standard-indent1 stack level (nth 1 (car (cdr (eclipse-get-last-b stack)))))
				    (setq end (eclipse-set-end end pos)))))
			   (setq stack (cons (list 'rs (current-column) level) stack))
			   (forward-char))
			  ((looking-at "\\(!\\|[\\+]\\+\\)")
			   (setq n (length (match-string 0)))
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el (current-column) level) stack)))
			   (forward-char n))
			  ((looking-at "\\(\\+[),]\\|~[ \t(]\\)")
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el (current-column) level) stack)))
			   (forward-char))
			  ((looking-at "\\(#\\(#\\|=<?\\|<=\\|>=?\\|\\\\\\(+\\|=\\)\\)\\|$\\(=<\\|[<>]?=\\)\\|`\\(::\\|<>?\\|=\\)\\|\\*\\(=<?\\|>=\\)\\|@\\(=?<\\|>=?\\)\\|::?\\|=\\.\\.\\|[@^&]\\|=[:\\]?=\\|[~]?=<?\\|<<?\\|>[=>]?\\|\\\\==?\\)")
			   (setq n (length (match-string 0)))
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (setq clmn (current-column))
			   (forward-char n)
			   (cond ((or (looking-at "(") (equal (nth 0 (car stack)) 'co))
				  (unless (equal (nth 0 (car stack)) 'el)
				    (setq stack (cons (list 'el clmn level) stack))))
				 (t
				  (unless eclipse-indent-mode
				    (setq level (+ level 1)))
				  (setq stack (cons (list 'inf clmn -1) stack)))))
			  ((looking-at "#\\(\\\\/\\|/\\\\\\)")
			   (setq n (length (match-string 0)))
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (setq clmn (current-column))
			   (forward-char n)
			   (cond ((or (looking-at "(") (equal (nth 0 (car stack)) 'co))
				  (unless (equal (nth 0 (car stack)) 'el)
				    (setq stack (cons (list 'el clmn level) stack))))
				 (t
				  (unless eclipse-indent-mode
				    (setq level (+ level 1)))
				  (setq stack (cons (list 'sc clmn -1) stack)))))
			  ((looking-at "\\(\\*\\|\\+\\|-\\|/[/\\]?\\|\\\\/?\\)")
			   (setq n (length (match-string 0)))
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (setq clmn (current-column))
			   (forward-char n)
			   (cond ((or (looking-at "(") (equal (nth 0 (car stack)) 'co))
				  (unless (equal (nth 0 (car stack)) 'el)
				    (setq stack (cons (list 'el clmn level) stack))))
				 (t (setq stack (cons (list 'op clmn -1) stack)))))
			  ((looking-at "\\.\\.\\.+")
			   (setq n (length (match-string 0)))
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el (current-column) level) stack)))
			   (forward-char n))
			  ((looking-at "\\.\\.")
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (unless eclipse-indent-mode
			     (setq level (+ level 1)))
			   (setq stack (cons (list 'inf (current-column) -1) stack))
			   (forward-char 2))
			  ((looking-at "\\.[^ \t\n,]")
			   (when (and lstnl idtflag)
			     (eclipse-standard-indent2 stack level flflag)
			     (setq end (eclipse-set-end end pos)))
			   (unless (equal (nth 0 (car stack)) 'el)
			     (setq stack (cons (list 'el (current-column) level) stack)))
			   (forward-char))
			  (t (forward-char)))
		    (setq lstnl nil)))))
      (cond ((and (eobp) (bolp) (not (null stack))) (setq eobfl 1))
	    ((eobp) (setq eobfl 2))
	    (t t)))
    (goto-char end)
    (cond ((equal timeflag 2)
	   (backward-char)
	   (save-excursion
	     (forward-line -1)
	     (beginning-of-line)
	     (skip-chars-forward " \t")
	     (setq indnt (current-column)))
	   (eclipse-standard-indent1 stack level indnt))
	  ((not (null stack))
	   (cond ((and (not afflag) (equal (nth 0 (car stack)) 'st))
		  (message "Inside string."))
		 ((and (not afflag) (equal (nth 0 (car stack)) 'qu))
		  (message "Inside quoted atom."))
		 ((equal timeflag 1)
		  (message "Indenting...done"))
		 (t t)))
	  ((equal timeflag 1)
	   (message "Indenting...done"))
	  (t t))))

(defun eclipse-standard-indent1 (stack level width)
  ;; standard indent cond block
  (cond ((null stack)
	 (when eclipse-indent-clause-heads
	   (eclipse-indent-region-line 0)))
	(eclipse-indent-mode
	 (eclipse-indent-region-line (max eclipse-tab-width (* level eclipse-indent-width))))
	(width (eclipse-indent-region-line width))
	(t (eclipse-indent-region-line eclipse-tab-width))))

(defun eclipse-standard-indent2 (stack level flflag)
  ;; standard indent cond block
  (cond ((null stack)
	 (when eclipse-indent-clause-heads
	   (eclipse-indent-region-line 0)))
	(eclipse-indent-mode
	 (eclipse-indent-region-line (max eclipse-tab-width (* level eclipse-indent-width))))
	(t (eclipse-indent-region3 stack flflag))))

(defun eclipse-set-end (end pos)
  ;; update end of region
  (+ end (- (point) pos)))

(defun eclipse-indent-region3 (stack flag)
  ;; standard indentation function for lines in region
  (let ((typ (nth 0 (car stack))) (column (nth 1 (car stack)))
	(level (nth 2 (car stack))) (auxst stack))
    (cond ((member typ (list 'cd 'mt))
	   (eclipse-indent-region-line eclipse-tab-width))
	  ((member typ (list 'if 'do))
	   (setq auxst (eclipse-get-last-sc auxst))
	   (when (and eclipse-indent-to-parenthesis (not (eq (nth 0 (car (cdr auxst))) 'cd)))
	     (setq auxst (cdr auxst)))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line (+ column eclipse-indent-width)))
	  ((equal typ 'sc)
	   (setq auxst (eclipse-get-last-if auxst))
	   (eclipse-indent-region-line (nth 1 (car auxst))))
	  ((member typ (list 'rb 'sb 'cb))
	   (eclipse-indent-region-line (+ column eclipse-indent-width)))
	  ((equal typ 'co)
	   (cond ((and (= flag 2) eclipse-first-line-std-indent (= level 1))
		  (setq column (min eclipse-tab-width (nth 1 (car (cdr stack)))))
		  (eclipse-indent-region-line column))
		 (t
 		  (setq column (nth 1 (car (cdr stack))))
 		  (eclipse-indent-region-line column))))
	  ((member typ (list 'el 'st 'qu))
	   (setq auxst (eclipse-get-last-level-stack auxst (- level 1)))
	   (setq column (nth 1 (car stack)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'rs)
	   (setq auxst (eclipse-get-last-b auxst))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'inf)
	   (setq auxst (eclipse-get-last-b-or-el auxst))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'op)
	   (setq auxst (eclipse-get-last-inf auxst))
	   (setq column (nth 1 (car auxst)))
	   (eclipse-indent-region-line column))
	  ((equal typ 'cmt)
	   (setq auxst (cdr auxst))
	   (eclipse-indent-region3 auxst flag)))))

(defun eclipse-in-list (stack)
  ;; returns t if stack contains element of type 'sb
  (cond ((null stack) nil)
	((equal (nth 0 (car stack)) 'sb) t)
	(t (eclipse-in-list (cdr stack)))))

(defun eclipse-get-last-comma (stack level)
  ;; return stack as it was at last level
  (let* ((last nil) (el (nth 0 (car stack)))
	 (found (member el (list 'co 'do 'if 'sc 'mt 'cd 'rb 'sb 'cb))))
    (while (not (or (null stack) found))
      (setq last (car stack)
	    stack (cdr stack)
	    el (nth 0 (car stack)))
      (cond ((and (equal el 'inf) (not eclipse-indent-mode))
	     (setq level (- level 1)))
	    ((member el (list 'co 'do 'if 'sc 'mt 'cd 'rb 'sb 'cb))
	     (setq found t))
	    (t t)))
    (while (not (or (null stack) (< (nth 2 (car stack)) level)))
      (setq last (car stack)
	    stack (cdr stack)))
    (if (not last)
	(list stack level)
      (list (cons last stack) level))))

(defun eclipse-get-last-level-stack (stack level)
  ;; return stack as it was at last level
  (while (not (or (null stack) (= (nth 2 (car stack)) level)))
    (setq stack (cdr stack)))
  stack)

(defun eclipse-get-last (typ stack)
  ;; return stack as it was at last typ
  (while (not (or (null stack) (equal (nth 0 (car stack)) typ)))
    (setq stack (cdr stack)))
  stack)

(defun eclipse-get-last-type (stack typelist)
  ;; return stack as it was at point after last element of type in list
  (let ((last nil))
    (while (not (or (null stack) (member (nth 0 (car stack)) typelist)))
      (setq last (car stack)
	    stack (cdr stack)))
    (if (not last)
	stack
      (cons last stack))))

(defun eclipse-get-last-b (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt)))

(defun eclipse-get-last-b-or-el (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'el)))

(defun eclipse-get-last-inf (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'inf)))

(defun eclipse-get-last-if (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'if)))

(defun eclipse-get-last-sc (stack)
  ;; return stack as it was at point after last element of type in list
  (eclipse-get-last-type stack (list 'rb 'sb 'cb 'cd 'mt 'sc)))

(defun eclipse-indent-predicate ()
  "Indent current predicate as ECLiPSe code."
  (interactive)
  (eclipse-mark-predicate)
  (eclipse-indent-region "predicate")
  (beginning-of-line)
  (skip-chars-forward " \t"))

(defun eclipse-indent-clause ()
  "Indent current clause as ECLiPSe code."
  (interactive)
  (eclipse-mark-clause)
  (eclipse-indent-region "clause")
  (beginning-of-line)
  (skip-chars-forward " \t"))

;;
;; Mark regions
;;

(defun eclipse-mark-buffer ()
  "Mark complete buffer."
  (interactive)
  (push-mark (point-min))
  (goto-char (point-max)))

(defun eclipse-mark-predicate ()
  "Mark current predicate."
  (interactive)
  (unless (eclipse-check-predicate-begin)
    (eclipse-goto-predicate-begin))
  (push-mark (point))
  (eclipse-goto-predicate-end))

(defun eclipse-mark-clause ()
  "Mark current clause."
  (interactive)
  (unless (eclipse-check-clause-begin)
    (eclipse-goto-clause-begin))
  (push-mark (point))
  (eclipse-goto-clause-end))

;;
;; Auxiliary functions
;;

(defun eclipse-check-predicate-begin ()
  ;; check if at beginning of predicate
  (let (aux1 aux2)
    (if (and (bolp)
	     (or (looking-at "[:?]-")
		 (and (looking-at "\\([a-z]\\|[^.\n \t]+[^.\n]*[:?]-\\)")
		      (progn
			(setq aux1 (eclipse-get-current-predicate-template t))
			(save-excursion
			  (eclipse-goto-predicate-begin)
			  (setq aux2 (eclipse-get-current-predicate-template t)))
			(not (string-equal aux1 aux2))))))
	t
      nil)))

(defun eclipse-check-clause-begin ()
  ;; check if at beginning of clause
  ;; works only under the assumption that we're likely at the beginning of
  ;; a clause, anyway!
  (if (and (or (bolp)
	       (eclipse-check-left-empty))
	   (looking-at "\\([a-z]\\|\\([^.\n \t]+[^.\n]*\\)?[:?]-\\)"))
      t
    nil))

(defun eclipse-check-left-empty ()
  ;; check if rest of the current line to the left is empty
  (let ((flag t))
    (save-excursion
      (while (and flag (not (bolp)))
	(backward-char)
	(when (not (looking-at "[ \t]"))
	  (setq flag nil))))
    flag))

(defun eclipse-backward-char (&optional n)
  ;; save backward-char. no error on bumping into beginning of buffer
  (or n (setq n 1))
  (while (and (> n 0) (not (bobp)))
    (backward-char)
    (setq n (- n 1))))

(defun eclipse-skip-comments-and-empty-lines (&optional flag)
  ;; skip forward through whitespace & comments
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (skip-chars-forward " \t")
      (cond ((looking-at "%") ;; comment: skip to next line
	     (forward-line)
	     (beginning-of-line))
	    ((looking-at "/\\*") ;; c-style comment: skip to end
	     (cond (flag
		    (unless (re-search-forward "\\*/" (point-max) t)
		      (goto-char (point-max))
		      (setq found t)))
		   (t
		    (cond ((looking-at "[^\n]*\\*/")
			   (forward-char 2)
			   (re-search-forward "\\*/" (point-max) t))
			  (t (setq found t))))))
	    ((looking-at "\n") ;; end-of-line: skip to next line
	     (forward-line)
	     (beginning-of-line))
	    (t (setq found t)))))) ;; non-empty character found

(defun eclipse-count-quotes ()
  ;; count the number of double quotes in the line
  (let ((quotes nil) (editpoint (- (point) 1)) (sq nil) (dq nil) (cmt nil))
    (save-excursion
      (forward-line -1)
      (beginning-of-line)
      (while (and (not (>= (point) editpoint)) (not cmt))
	(cond ((and (looking-at "[^\\]\"") (not sq))
	       (if quotes
		   (setq quotes nil)
		 (setq quotes (current-column)))
	       (setq dq (not dq)))
	      ((and (looking-at "0'[^\n]") (not sq))
	       (forward-char 2))
	      ((looking-at "[1-9]'[0-9a-zA-Z]+")
	       (forward-char 2))
	      ((and (looking-at "[^\\]'") (not dq))
	       (forward-char 2)
	       (setq sq (not sq)))
	      ((and (looking-at "%") (not dq) (not sq))
	       (setq cmt t))
	      ((and (looking-at "/\\*") (not dq) (not sq))
	       (forward-char 2)
	       (if (looking-at "[^\n]*\\*/")
		   (re-search-forward "\\*/" (point-max) t)
		 (setq cmt t)))
	      (t t))
	(forward-char)))
    quotes))

(defun eclipse-end-of-clause ()
  ;; go to end of clause in this line
  (let* ((eolpos (save-excursion (end-of-line) (point)))
	 (comment nil) (flag nil) (empty t))
    (beginning-of-line)
    (while (and (not (= (point) eolpos)) (not comment))
      (cond ((looking-at "0'")
	     (forward-char 2)
	     (or (looking-at "\n")
		 (forward-char)))
	    ((looking-at "[1-9]+'[0-9a-zA-Z]+")
	     (re-search-forward "[^0-9a-zA-Z']" (point-max) t)
	     (backward-char))
 	    ((looking-at "'")
	     (forward-char)
	     (search-forward "'" eolpos t)
	     (while (not (or flag comment))
	       (cond ((save-excursion
			(eclipse-backward-char)
			(looking-at "\\\\'"))
		      (forward-char)
		      (search-forward "'" eolpos t)
		      (backward-char))
		     ((looking-at "[ \t]*\n") (setq comment t))
		     (t
		      (setq flag t)
		      (forward-char))))
	     (setq flag nil))
 	    ((looking-at "\"")
	     (forward-char)
	     (search-forward "\"" eolpos t)
	     (while (not (or flag comment))
	       (cond ((save-excursion
			(eclipse-backward-char)
			(looking-at "\\\\\""))
		      (forward-char)
		      (search-forward "\"" eolpos t)
		      (backward-char))
		     ((looking-at "[ \t]*\n") (setq comment t))
		     (t
		      (setq flag t)
		      (forward-char))))
	     (setq flag nil))
	    ((looking-at "%") (setq comment t))
	    ((looking-at "\\(/\\*[^\n]*\\*/\\)?[ \t]*\n")
	     (setq comment t))
	    ((looking-at "/\\*[^\n]*\\*/")
	     (re-search-forward "\\*/" (point-max) t))
	    ((looking-at "/\\*") (setq comment t))
	    ((looking-at "\\*/[ \t]*\n")
	     ;; end of a multi-line comment: find beginning
	     (re-search-backward "/\\*")
	     (cond ((save-excursion
		      (beginning-of-line)
		      (looking-at "[ \t]*/\\*"))
		    (while empty
		      (forward-line -1)
		      (beginning-of-line)
		      (if (bobp) (setq empty nil)
			(skip-chars-forward " \t")
			(or (looking-at "\\(%\\|\n\\)")
			    (setq empty nil))))
		    (setq empty t
			  eolpos (save-excursion (end-of-line) (point))))
		   (t (setq comment t))))
	    (t (forward-char))))
    (skip-chars-backward " \t")))

(defun eclipse-jump-over-strings (&optional eobflag cmtflag)
  ;; jump over constructs "...", '...', /*...*/, %..., and whitespace
  (let ((found nil))
    (while (and (looking-at "[ \t\"'/%\n]") (not found))
      (cond ((looking-at "[ \t\n]")
	     (if (re-search-forward "[^ \t\n]" (point-max) t)
		 (backward-char)
	       (setq found t)
	       (if eobflag (goto-char (point-max)))))
	    ((looking-at "%")
	     (end-of-line)
	     (if (eobp)
		 (setq found t)
	       (forward-line)
	       (beginning-of-line)))
	    ((looking-at "/\\*")
	     (unless (re-search-forward "\\*/" (point-max) t)
	       (setq found t)
	       (if eobflag (goto-char (point-max)))))
	    ((looking-at "\"")
	     (eclipse-goto-end-of-string))
	    ((looking-at "'")
	     (if (not cmtflag)
		 (eclipse-goto-end-of-quote)
	       (setq found t)))
	    (t (forward-char))))))

(defun eclipse-goto-end-of-quote ()
  ;; goto to the end of the current quoted atom
  (eclipse-backward-char)
  (cond ((looking-at "0'")
	 (forward-char 2)
	 (or (looking-at "\n")
	     (forward-char)))
	((looking-at "[1-9]'[0-9a-zA-Z]")
	 (forward-char 3))
	(t
	 (forward-char)
	 (eclipse-goto-end-of "'"))))

(defun eclipse-goto-end-of-string ()
  ;; goto to the end of the current string
  (eclipse-goto-end-of "\""))

(defun eclipse-goto-end-of (str)
  ;; goto to the end of the current string
  (let ((str1 (concat "[^\\]" str)))
    (unless (re-search-forward str1 (point-max) t)
      (goto-char (point-max)))))

(defun eclipse-percent-message (str length last)
  ;; print a message "Str... (XX%)"
  (let ((percent (truncate (* 100 (/ (* 1.0 (point)) (* 1.0 length))))))
    (cond ((>= percent (+ last 10))
	   (message (concat str "... (" (number-to-string percent) "%%)"))
	   percent)
	  (t last))))

;;
;; Go-to commands
;;

(defun eclipse-goto-clause-begin (&optional flag)
  "Goto the beginning of the current clause."
  (interactive)
  (if (and eclipse-quick-jumps-selected (not flag))
      (let ((found nil))
	(if (bolp) (eclipse-backward-char))
	(while (and (not found) (not (bobp)))
	  (beginning-of-line)
	  (if (looking-at "[ \t]*\n")
	      (forward-line -1)
	    (setq found t)))
	(setq found nil)
	(while (and (not found) (not (bobp)))
	  (beginning-of-line)
	  (if (looking-at "[ \t]*\n")
	      (setq found t)
	    (forward-line -1)))
	(or (and (bobp) (not (looking-at "[ \t]*\n")))
	    (progn (forward-line) (beginning-of-line))))
    (let ((last (point)) (pnt (point)) (found nil) maxpnt)
      (goto-char (point-min))
      (beginning-of-line)
      (eclipse-jump-over-strings nil t)
      (setq last (point))
      (if (<= pnt last)
	  (setq maxpnt (point-max))
	(setq maxpnt pnt))
      (while (and (not found) (not (eobp)))
	(cond ((> (point) maxpnt)
	       (goto-char pnt)
	       (setq found t))
	      ((re-search-forward "[.\"'%/]" maxpnt t)
	       (eclipse-backward-char)
	       (cond ((looking-at "'")
		      (eclipse-goto-end-of-quote))
		     ((looking-at "\"")
		      (eclipse-goto-end-of-string))
		     ((looking-at "%")
		      (end-of-line)
		      (unless (eobp)
			(forward-line)
			(beginning-of-line)))
		     ((looking-at "/\\*")
		      (unless (re-search-forward "\\*/" (point-max) t)
			(goto-char (point-max))))
		     ((looking-at "[.][.]+")
		      (forward-char (length (match-string 0))))
		     ((looking-at "[.][^ \t\n,]")
		      (forward-char))
		     ((looking-at "[.]")
		      (forward-char)
		      (let ((aux (point)))
			(eclipse-jump-over-strings nil t)
			(if (not (save-excursion
				   (re-search-forward "[.\"'%/]" (point-max) t)))
			    (setq found t)
			  (cond ((eq aux (point))
				 (setq found t))
				((< (point) pnt)
				 (setq last (point)))
				(t (setq found t))))))
		     (t (forward-char))))
	      (t
	       (goto-char pnt)
	       (setq found t))))
      (goto-char last))))

(defun eclipse-goto-clause-end ()
  "Goto the end of the current clause."
  (interactive)
  (if eclipse-quick-jumps-selected
      (let ((found nil))
	(if (save-excursion (eclipse-backward-char) (looking-at ","))
	    (eclipse-skip-comments-and-empty-lines t)
	  (if (save-excursion
		(eclipse-backward-char 2)
		(looking-at "[^.].[^.0-9]"))   ; update to include "." as atom?
	      (forward-line))
	  (beginning-of-line)
	  (eclipse-skip-comments-and-empty-lines t))
	(while (and (not found) (not (eobp)))
	  (beginning-of-line)
	  (if (looking-at "[ \t]*\n")
	      (forward-line)
	    (setq found t)))
	(setq found nil)
	(while (and (not found) (not (eobp)))
	  (beginning-of-line)
	  (if (looking-at "[ \t]*\n")
	      (setq found t)
	    (forward-line)))
	(eclipse-end-of-clause)
	(if (eobp)
	    (skip-chars-backward " \t\n")
	  (forward-line -1)
	  (eclipse-end-of-clause)))
    (let ((found nil))
      (eclipse-skip-comments-and-empty-lines t)
      (unless (eclipse-check-clause-begin)
	(eclipse-goto-clause-begin))
      (while (not found)
	(if (not (re-search-forward "[.\"'%/ \t]" (point-max) t))
	    (setq found t)
	  (eclipse-backward-char)
	  (cond ((eobp) (setq found t))
		((looking-at "'")
		 (eclipse-goto-end-of-quote))
		((looking-at "\"")
		 (eclipse-goto-end-of-string))
		((looking-at "[.][.]+")
		 (forward-char (length (match-string 0))))
		((looking-at "[.][^ \t\n,]")
		 (forward-char))
		((looking-at "[.]")
		 (forward-char)
		 (setq found t))
		((looking-at "%")
		 (forward-line)
		 (beginning-of-line))
		((looking-at "/\\*")
		 (unless (re-search-forward "\\*/" (point-max) t)
		   (goto-char (point-max))))
		((looking-at "[ \t]")
		 (forward-char))
		(t (forward-char))))))))

(defun eclipse-goto-predicate-begin ()
  "Goto the beginning of the current predicate."
  (interactive)
  (if eclipse-quick-jumps-selected
      (let ((found nil) (last nil) (template nil))
	(eclipse-goto-clause-begin)
	(setq template (eclipse-get-current-predicate-template)
	      last (point))
	(while (and (not found) (not (bobp)))
	  (eclipse-goto-clause-begin)
	  (if (string-equal template (eclipse-get-current-predicate-template))
	      (setq last (point))
	    (setq found t)))
	(or (not found) (goto-char last)))
    (let ((found nil) pnt template)
      (eclipse-goto-clause-begin)
      (unless (looking-at "[:?]-")
	(setq pnt (point)
	      template (eclipse-get-current-predicate-template))
	(goto-char (point-min))
	(eclipse-jump-over-strings nil t)
	(if (string-equal template (eclipse-get-current-predicate-template)) t
	  (eclipse-jump-over-strings nil)
	  (while (not found)
	    (if (not (re-search-forward "[.\"'%/]" (point-max) t))
		(setq found t)
	      (eclipse-backward-char)
	      (cond ((looking-at "'")
		     (eclipse-goto-end-of-quote))
		    ((looking-at "\"")
		     (eclipse-goto-end-of-string))
		    ((looking-at "%")
		     (forward-line)
		     (beginning-of-line))
		    ((looking-at "/\\*")
		     (unless (re-search-forward "\\*/" (point-max) t)
		       (goto-char (point-max))))
		    ((looking-at "[.][.]+")
		     (forward-char (length (match-string 0))))
		    ((looking-at "[.][^ \t\n,]")
		     (forward-char))
		    ((looking-at "[.]")
		     (forward-char)
		     (eclipse-jump-over-strings nil t)
		     (if (string-equal template (eclipse-get-current-predicate-template))
			 (setq found t)
		       (forward-char)))
		    (t (forward-char)))))
	  (if (> (point) pnt) (goto-char pnt)))))))

(defun eclipse-goto-predicate-end ()
  "Goto the end of the current predicate."
  (interactive)
  (if eclipse-quick-jumps-selected
      (let ((found nil) (last nil) (template nil))
	(eclipse-goto-clause-end)
	(setq last (point))
	(save-excursion
	  (eclipse-goto-clause-begin)
	  (setq template (eclipse-get-current-predicate-template)))
	(while (and (not found) (not (eobp)))
	  (eclipse-goto-clause-end)
	  (cond ((eq last (point)) (setq found t))
		((save-excursion
		   (eclipse-goto-clause-begin)
		   (string-equal template (eclipse-get-current-predicate-template)))
		 (setq last (point)))
		(t (setq found t))))
	(or (not found) (goto-char last)))
    (let ((found nil) (last nil) template)
      (eclipse-skip-comments-and-empty-lines t)
      (unless (eclipse-check-clause-begin)
	(eclipse-goto-clause-begin))
      (skip-chars-forward " \t")
      (if (looking-at "[:?]-")
	  (eclipse-goto-clause-end)
	(setq template (eclipse-get-current-predicate-template))
	(while (not found)
	  (if (not (re-search-forward "[.\"'%/ \t]" (point-max) t))
	      (setq found t)
	    (eclipse-backward-char)
	    (cond ((eobp) (setq found t))
		  ((looking-at "'")
		   (eclipse-goto-end-of-quote))
		  ((looking-at "\"")
		   (eclipse-goto-end-of-string))
		  ((looking-at "%")
		   (forward-line)
		   (beginning-of-line))
		  ((looking-at "/\\*")
		   (unless (re-search-forward "\\*/" (point-max) t)
		     (goto-char (point-max))))
		  ((looking-at "[.][.]+")
		   (forward-char (length (match-string 0))))
		  ((looking-at "[.][^ \t\n,]")
		   (forward-char))
		  ((looking-at "[.]")
		   (forward-char)
		   (setq last (point))
		   (eclipse-jump-over-strings)
		   (unless (string-equal template (eclipse-get-current-predicate-template))
		     (setq found t)
		     (goto-char last)))
		  (t (forward-char)))))))))

;;
;; Speedbar support
;;

(defun eclipse-goto-prev-index-position ()
  ;; go to the previous entry in the index
  (beginning-of-line)
  (if (bobp)
      nil
    (let ((now (point)))
      (eclipse-goto-predicate-begin)
      (if (eq now (point))
	  nil
	t))))

(defun eclipse-create-index ()
  ;; creates an index for the speedbar.
  ;; this function scancs the buffer top-down, which is faster than scanning
  ;; bottom-up, as is standard in speedbar/imenu, since this way, we can use
  ;; the information, that the point is always at the beginning of a
  ;; predicate when the next predicate is searched
  (save-excursion
    (let ((index-alist '()) (index-dir-alist '())
	  (length (- (point-max) (point-min))) (last -1) (pc 0) name
	  entry)
      (message "Indexing...")
      (goto-char (point-min))
      (eclipse-goto-clause-end)
      (eclipse-goto-clause-begin)   ;; quick and dirty...
      ;; Search for the function
      (while (and (not (eobp)) (< (point) (point-max)) (not (eq last (point))))
	(eclipse-percent-message "Indexing" length pc)
	(setq last (point)
	      name (eclipse-extract-index-name))
	(setq entry (cons name last))
	(if (looking-at "[:?]-")
	    (setq index-dir-alist (cons entry index-dir-alist))
	  (setq index-alist (cons entry index-alist)))
	(eclipse-goto-predicate-end)
	(eclipse-jump-over-strings t t)
	(skip-chars-forward " \t"))
      (message "Indexing...done.")
      (and index-dir-alist
	   (setq index-alist (cons (cons "Directives" index-dir-alist) index-alist)))
      index-alist)))

(defun eclipse-extract-index-name ()
  ;; get the name to be listed in the index
  (let (start name)
    (save-excursion
      (cond ((looking-at "[:?]-")
	     (skip-chars-forward " \t")
	     (forward-char 2)
	     (re-search-forward "[a-z]" (point-max) t)
	     (setq start (- (point) 1))
	     (re-search-forward "[.\n%]" (point-max) t)
	     (backward-char)
	     (skip-chars-backward " \t([")
	     (setq name (buffer-substring-no-properties start (point))))
	    ((looking-at "[A-Z]")
	     (setq start (point))
	     (re-search-forward "\\(\n\\|[:?]-\\)" (point-max) t)
	     (backward-char)
	     (when (looking-at "-")
	       (backward-char))
	     (skip-chars-backward " \t([")
	     (setq name (buffer-substring-no-properties start (point))))
	    (t
	     (setq name (eclipse-get-current-predicate-template t)))))
    name))

;;
;; Predicate template & args and other "edit" functions
;;

(defun eclipse-get-current-predicate-template (&optional specflag)
  ;; return the template for the current predicate
  ;; if specflag = t, return the specification for the current predicate
  ;; problem: cannot handle operators in clause heads:
  ;; X = Y :- ...
  ;; ++ X :- ...
  ;; X ++ :- ...
  ;; since Emacs doesn't know about the operator definitions. And since the
  ;; arguments in the clause heads may be atoms (just like the operators),
  ;; it is impossible to guarantee the correct behaviour in this case!
  (let ((template nil) (fb (point)) fe functor args (cc 0) (found nil) (bc 0))
    (save-excursion
      (re-search-forward "\\([\n(.]\\|[:?]-\\)" (point-max) t)
      (eclipse-backward-char)
      (cond ((and (looking-at "-")
		  (save-excursion
		    (backward-char)
		    (looking-at "[:?]-")))
	     (backward-char))
	    ((looking-at "[\n.]")
	     (backward-char))
	    (t t))
      (skip-chars-backward " \t")
      (setq fe (point)
	    functor (buffer-substring-no-properties fb fe))
      (cond ((looking-at "(")
	     (while (not found)
	       (cond ((eobp) (setq found t))
		     ((looking-at "'")
		      (eclipse-goto-end-of-quote))
		     ((looking-at "\"")
		      (eclipse-goto-end-of-string))
		     ((looking-at "%")
		      (forward-line)
		      (beginning-of-line))
		     ((looking-at "/\\*")
		      (unless (re-search-forward "\\*/" (point-max) t)
			(goto-char (point-max))))
		     ((looking-at "[({[]")
		      (forward-char)
		      (setq bc (+ bc 1)))
		     ((looking-at "[]})]")
		      (forward-char)
		      (setq bc (- bc 1))
		      (when (zerop bc)
			(setq found t)))
		     ((looking-at ",")
		      (forward-char)
		      (when (= bc 1)
			(setq cc (+ cc 1))))
		     (t (forward-char))))
	     (if specflag
		 (setq args (concat "/" (number-to-string (+ cc 1))))
	       (setq args (concat "(" (make-string cc 44) ")"))))
	    (t
	     (if specflag
		 (setq args "/0")
	       (setq args ""))))
      (setq template (concat functor args))
      template)))

(defun eclipse-get-current-predicate-args ()
  ;; return the arguments for the current term
  ;; this should be improved, so that comments get stripped automatically
  (let ((args '()) fb fe (found nil) (bc 0) next (arg nil))
    (save-excursion
      (or (re-search-forward "[ \t\n(:]" (point-max) t)
	  (search-forward "."))
      (eclipse-backward-char)
      (when (looking-at "(")
	(setq bc 1)
	(forward-char)
	(setq fb (point))
	(while (not found)
	  (cond ((eobp)
		 (setq found t
		       fe (point)
		       next (buffer-substring-no-properties fb fe))
		 (if (not arg)
		     (setq args (append args (list next)))
		   (setq args (append args (list (concat arg next)))
			 arg nil)))
		((looking-at "'")
		 (eclipse-goto-end-of-quote))
		((looking-at "\"")
		 (eclipse-goto-end-of-string))
		((looking-at "%")
		 (setq fe (point)
		       next (buffer-substring-no-properties fb fe))
		 (if arg
		     (setq arg (concat arg next))
		   (setq arg next))
		 (forward-line)
		 (beginning-of-line)
		 (eclipse-skip-comments-and-empty-lines)
		 (setq fb (point)))
		((looking-at "/\\*")
		 (setq fe (point)
		       next (buffer-substring-no-properties fb fe))
		 (if arg
		     (setq arg (concat arg next))
		   (setq arg next))
		 (unless (re-search-forward "\\*/" (point-max) t)
		   (goto-char (point-max)))
		 (eclipse-skip-comments-and-empty-lines)
		 (setq fb (point)))
		((looking-at "[({[]")
		 (forward-char)
		 (setq bc (+ bc 1)))
		((looking-at "[]})]")
		 (setq bc (- bc 1))
		 (cond ((zerop bc)
			(setq found t
			      fe (point)
			      next (buffer-substring-no-properties fb fe))
			(if (not arg)
			    (setq args (append args (list next)))
			  (setq args (append args (list (concat arg next)))
				arg nil))
			(forward-char))
		       (t (forward-char))))
		((looking-at ",")
		 (cond ((= bc 1)
			(setq fe (point)
			      next(buffer-substring-no-properties fb fe))
			(if (not arg)
			    (setq args (append args (list next)))
			  (setq args (append args (list (concat arg next)))
				arg nil))
			(forward-char)
			(eclipse-skip-comments-and-empty-lines)
			(setq fb (point)))
		       (t (forward-char))))
		((looking-at "[ \t\n]")
		 (setq fe (point)
		       next (buffer-substring-no-properties fb fe))
		 (if arg
		     (setq arg (concat arg next))
		   (setq arg next))
		 (eclipse-skip-comments-and-empty-lines)
		 (setq fb (point)))
		(t (forward-char)))))
      args)))

(defun eclipse-insert-predicate-template ()
  "Insert the template of the current predicate."
  (interactive)
  (let ((template nil))
    (save-excursion
      (unless (eclipse-check-clause-begin)
	(eclipse-goto-clause-begin))
      (setq template (eclipse-get-current-predicate-template)))
    (insert template)
    (when (save-excursion (backward-char) (looking-at ")"))
      (search-backward "(")
      (forward-char))))

(defun eclipse-insert-predicate-spec ()
  "Insert the specification of the current predicate."
  (interactive)
  (let ((template nil))
    (save-excursion
      (unless (eclipse-check-clause-begin)
	(eclipse-goto-clause-begin))
      (setq template (eclipse-get-current-predicate-template t)))
    (insert template)))

(defun eclipse-insert-clause-head ()
  "Insert a new clause head of the current predicate with the arguments of the last clause."
  (interactive)
  (let ((template nil) (this (point)) spec vars functor arity aux next)
    (save-excursion
      (eclipse-goto-clause-begin)
      (setq spec (eclipse-get-current-predicate-template t)
	    vars (eclipse-get-current-predicate-args)
	    aux (split-string spec "/")
	    functor (nth 0 aux)
	    arity (string-to-number (nth 1 aux))))
    (unless (string-equal spec "/0")
      (insert (concat "\n" functor))
      (unless (zerop arity)
	(insert "(")
	(while (car vars)
	  (setq next (car vars)
		vars (cdr vars))
	  (insert next)
	  (if (car vars) (insert ",")))
	(insert ")"))
      (insert " :-\n")
      (goto-char this)
      (re-search-forward "[(:]" (point-max) t)
      (backward-char)
      (if (looking-at "(")
	  (forward-char)
	(forward-char 3)
	(eclipse-indent-line)))))

(defun eclipse-insert-clause-head-empty ()
  "Insert a new clause head of the current predicate without arguments."
  (interactive)
  (let ((template nil) (this (point)))
    (save-excursion
      (eclipse-goto-clause-begin)
      (setq template (eclipse-get-current-predicate-template)))
    (unless (string-equal "" template)
      (insert (concat "\n" template " :-\n"))
      (goto-char this)
      (re-search-forward "[(:]" (point-max) t)
      (backward-char)
      (if (looking-at "(")
	  (forward-char)
	(forward-char 3)
	(eclipse-indent-line)))))

(defun eclipse-anonymise-variables ()
  "Add _ to all variables in the current region."
  (interactive)
  (let ((rbegin (- (region-beginning) 1)) (rend (region-end)))
    (goto-char (point-min))
    (eclipse-anonymise-loop rbegin 0)
    (eclipse-anonymise-loop rend 1)
    (goto-char (+ rbegin 1))))

(defun eclipse-anonymous-variables ()
  "Replaces the variables in the current region with anonymous variables."
  (interactive)
  (let ((rbegin (- (region-beginning) 1)) (rend (region-end)))
    (goto-char (point-min))
    (eclipse-anonymise-loop rbegin 0)
    (eclipse-anonymise-loop rend 2)
    (goto-char (+ rbegin 1))))

(defun eclipse-anonymise-loop (pos flag)
  ;; anonymise variables, if flag is t
  (while (not (or (> (point) pos) (eobp)))
    (cond ((looking-at "[A-Z]")
	   (unless (zerop flag)
	     (insert "_")
	     (setq pos (+ pos 1)))
	   (when (= flag 2)
	     (while (looking-at "[_a-zA-Z0-9]")
	       (delete-char 1)
	       (setq pos (- pos 1))))
	   (re-search-forward "[^_a-zA-Z0-9]" (point-max) t)
	   (backward-char))
	  ((and (looking-at "_") (= flag 2))
	   (forward-char)
	   (while (looking-at "[_a-zA-Z0-9]")
	     (delete-char 1)
	     (setq pos (- pos 1)))
	   (re-search-forward "[^_a-zA-Z0-9]" (point-max) t)
	   (backward-char))
	  ((looking-at "'")
	   (eclipse-goto-end-of-quote))
	  ((looking-at "\"")
	   (eclipse-goto-end-of-string))
	  ((looking-at "[a-z0-9_]")
	   (re-search-forward "[^_a-zA-Z0-9']" (point-max) t)
	   (backward-char))
	  ((looking-at "%")
	   (forward-line)
	   (beginning-of-line))
	  ((looking-at "/\\*")
	   (unless (re-search-forward "\\*/" (point-max) t)
	     (goto-char (point-max))))
	  (t
	   (forward-char)
	   (if (re-search-forward "[a-zA-Z0-9_'\"%/\\]" (point-max) t)
	       (backward-char)
	     (goto-char pos)
	     (or (eobp) (forward-char)))))))

(defun eclipse-insert-comment-pred-short ()
  "Insert \":- comment(,).\" into the program text."
  (interactive)
  (insert ":- comment(,).\n")
  (forward-line -1)
  (beginning-of-line)
  (search-forward "("))

(defun eclipse-insert-comment-pred-full ()
  "Insert comment/2 call with all arguments into program text."
  (interactive)
  (let (pnt spec vars functor arity aux next)
    (eclipse-jump-over-strings)
    (setq spec (eclipse-get-current-predicate-template t))
    (message spec)
    (if (string-equal spec "/0")
	(eclipse-insert-comment-pred-short)
      (setq vars (eclipse-get-current-predicate-args)
	    aux (split-string spec "/")
	    functor (nth 0 aux)
	    arity (string-to-number (nth 1 aux)))
      (insert (concat ":- comment(" spec  ", [\n"))
      (setq pnt (point))
      (insert "        summary:,\n")
      (unless (zerop arity)
	(insert (concat "        amode:" functor (concat "(" (make-string (- arity 1) 44) ")") ",\n"
			"        args:[\n"))
	(while (car vars)
	  (setq next (car vars)
		vars (cdr vars))
	  (insert (concat "                 \"" next))
	  (if (car vars)
	      (insert "\": ,\n")
	    (insert "\": \n")))
	(insert "             ],\n"))
      (insert (concat "        desc:,\n"
		      "        fail_if:,\n"
		      "        resat:,\n"
		      "        eg:,\n"
		      "        see_also:,\n"
		      "        index:]).\n\n"))
      (goto-char pnt)
      (search-forward ":"))))

(defun eclipse-dabbrev-expand ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first. Returns expansion including arguments."
  (interactive)
  (eclipse-dabbrev-expand2 "\\(\\(\\sw\\|\\s_\\)\\(([a-zA-Z0-9_ ,+-?]*)\\)?\\|([a-zA-Z0-9_ ,+-?]*)\\)"))

(defun eclipse-dabbrev-expand1 ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first. Returns expansion without arguments."
  (interactive)
  (eclipse-dabbrev-expand2 "\\(\\sw\\|\\s_\\)(?"))

(defun eclipse-dabbrev-expand2 (reg)
  (let (aux)
    (eclipse-load-dabbrev)
    (setq aux dabbrev-search-these-buffers-only)
    ;; a bit of a hack:
    ;; we want our own keyword list searched first,
    ;; then the current buffer etc.
    (eclipse-update-dabbrev-list (current-word))
    (setq dabbrev-abbrev-char-regexp reg)
    (dabbrev-expand nil)
    (setq dabbrev-abbrev-char-regexp nil
	  dabbrev-search-these-buffers-only aux)))

(defun eclipse-dabbrev-expand0 ()
  "Automatic expansion by dabbrev.
Checks for expansions in current buffer first, then for predefined keywords."
  (interactive)
  (let (aux1 aux2)
    (eclipse-load-dabbrev)
    (setq aux1 dabbrev-search-these-buffers-only
	  aux2 dabbrev-abbrev-char-regexp
	  dabbrev-abbrev-char-regexp nil)
    (eclipse-update-dabbrev-list (current-word) t)
    (dabbrev-expand nil)
    (setq dabbrev-abbrev-char-regexp aux2
	  dabbrev-search-these-buffers-only aux1)))

(defun eclipse-dabbrev-completion ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first.
Returns list of possible expansion including arguments."
  (interactive)
  (eclipse-dabbrev-completion2 "\\(\\(\\sw\\|\\s_\\)\\(([a-zA-Z0-9_ ,+-?]*)\\)?\\|([a-zA-Z0-9_ ,+-?]*)\\)"))

(defun eclipse-dabbrev-completion1 ()
  "Automatic expansion of ECLiPSe keywords.
Checks predefined keywords first.
Returns list of possible expansion without arguments."
  (interactive)
  (eclipse-dabbrev-completion2 "\\(\\sw\\|\\s_\\)(?"))

(defun eclipse-dabbrev-completion2 (reg)
  (let (aux)
    (eclipse-load-dabbrev)
    (setq aux dabbrev-search-these-buffers-only)
    (eclipse-update-dabbrev-list (current-word))
    (setq dabbrev-abbrev-char-regexp reg)
    (dabbrev-completion nil)
    (setq dabbrev-abbrev-char-regexp nil
	  dabbrev-search-these-buffers-only aux)))

(defun eclipse-load-dabbrev ()
  ;; load dabbrev if needed, and load the keyword list
  (require 'dabbrev)
  (make-local-variable 'dabbrev-abbrev-char-regexp)
  (make-local-variable 'dabbrev-search-these-buffers-only))

(defun eclipse-update-dabbrev-list (keyword &optional flag)
  ;; get list of keywords
  (get-buffer-create "*eclipse-keywords*")
  (unless (if dabbrev--last-abbrev-location
	      (if (numberp dabbrev--last-abbrev-location)
		  ;; dabbrev--last-abbrev-location can either be number or
		  ;; marker!?
		  (= dabbrev--last-abbrev-location (point))
		(= (marker-position dabbrev--last-abbrev-location) (point))))
    (let (help-call)
      (setq help-call (concat eclipse-help-call1 (downcase keyword) eclipse-help-call2))
      (save-excursion
	(set-buffer "*eclipse-keywords*")
	(beginning-of-buffer)
	(delete-char (- (point-max) (point)))
	(insert (shell-command-to-string help-call))
	(beginning-of-buffer)
	(if (looking-at "string stream")
	    (delete-char (- (point-max) (point-min)))
	  (while (not (eobp))
	    (cond ((looking-at "----")
		   (delete-char 4)
		   (delete-blank-lines)
		   (if (looking-at "Call")
		       (delete-char (- (point-max) (point)))
		     (let ((aux1 (point)) aux2)
		       (end-of-line)
		       (search-backward ":" aux1 t)
		       (when (looking-at ":")
			 (forward-char)
			 (skip-chars-forward " \t")
			 (setq aux1 (point))
			 (beginning-of-line)
			 (delete-char (- aux1 (point))))
		       (cond ((looking-at "lib([a-z_]+)")
			      (save-excursion
				(forward-line)
				(setq aux1 (point)))
			      (delete-char (- aux1 (point)))
			      (delete-blank-lines))
			     ((looking-at "[a-z]+ [a-z]+/index")
			      (save-excursion
				(forward-line)
				(setq aux1 (point)))
			      (delete-char (- aux1 (point)))
			      (delete-blank-lines))
			     (t (forward-line)))
		       (beginning-of-line)
		       (setq aux1 (point))
		       (search-forward "----" (eobp) t)
		       (backward-char 4)
		       (setq aux2 (point))
		       (goto-char aux1)
		       (delete-char (- aux2 aux1)))))
		  ((looking-at "[ \t]*\n")
		   (delete-blank-lines))))))))
  ;; update dabbrev buffer list
  (let ((blist (if flag
		   (list (buffer-name) "*eclipse-keywords*")
		 (list "*eclipse-keywords*" (buffer-name))))
	(all-buffers (cdr (buffer-list)))
	next next-name ext)
    (while (car all-buffers)
      (setq next (car all-buffers)
	    all-buffers (cdr all-buffers)
	    next-name (buffer-name next)
	    ext (nth 1 (split-string next-name "\\.")))
      (when (member ext (list "ecl" "ECL" "pl" "PL"))
	  (setq blist (append blist (list next-name)))))
    (setq dabbrev-search-these-buffers-only blist)))

(defun eclipse-call-help ()
  "Call the ECLiPSe help.

You will be asked for the predicate name for which you need help.
The input will be passed on to the help/1 predicate.
The output will be presented in the buffer *eclipse-help*.
The format for the help call is Name for simple help and 
<Module:>Name/Arity for detailed help."
  (interactive)
  (eclipse-load-dabbrev)
  (let ((help-call (downcase (current-word))) aux)
    (setq aux (read-from-minibuffer 
	       (if help-call
		   (concat "Describe predicate (default " help-call "): ")
		 "Describe predicate: ")))
    (cond ((not help-call)
	   (unless (= 0 (string-width aux))
	     (setq help-call (concat eclipse-help-call1 aux eclipse-help-call2))
	     (shell-command help-call "*eclipse-help*")))
	  ((= 0 (string-width aux))
	   (unless (= 0 (string-width help-call))
	     (setq help-call (concat eclipse-help-call1 help-call eclipse-help-call2))
	     (shell-command help-call "*eclipse-help*")))
	  (t
	   (setq help-call (concat eclipse-help-call1 aux eclipse-help-call2))
	   (shell-command help-call "*eclipse-help*")))))

(defun eclipse-check-buffer ()
  "Send the entire buffer to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compile-result*."
  (interactive)
  (let* ((lib-call (eclipse-check-lib-call))
	 (compile-call (concat "echo \"" lib-call " " (buffer-string) "\" | " eclipse-check-call)))
    (shell-command compile-call "*eclipse-compile-result*")))

(defun eclipse-check-region ()
  "Send the current region to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compile-result*."
  (interactive)
  (let* ((lib-call (eclipse-check-lib-call))
	 (compile-call (concat "echo \"" lib-call " " (buffer-substring (region-beginning) (region-end)) "\" | " eclipse-check-call)))
    (shell-command compile-call "*eclipse-compile-result*")))

(defun eclipse-check-predicate ()
  "Send the current predicate to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compile-result*."
  (interactive)
  (save-excursion
    (let* ((lib-call (eclipse-check-lib-call))
	   (point1 (prog2
		       (eclipse-goto-predicate-begin)
		       (point)))
	   (point2 (prog2
		       (eclipse-goto-predicate-end)
		       (point)))
	   (compile-call (concat "echo \"" lib-call " " (buffer-substring point1 point2) "\" | " eclipse-check-call)))
      (shell-command compile-call "*eclipse-compile-result*"))))

(defun eclipse-check-clause ()
  "Send the current clause to an ECLiPSe process and compile it.
Warnings and Errors are returned in buffer *eclipse-compile-result*."
  (interactive)
  (save-excursion
    (let* ((lib-call (eclipse-check-lib-call))
	   (point1 (prog2
		       (eclipse-goto-clause-begin)
		       (point)))
	   (point2 (prog2
		       (eclipse-goto-clause-end)
		       (point)))
	   (compile-call (concat "echo \"" lib-call " " (buffer-substring point1 point2) "\" | " eclipse-check-call)))
      (shell-command compile-call "*eclipse-compile-result*"))))

(defun eclipse-check-load-libraries ()
  "List the libraries to be loaded for syntax check"
  (interactive)
  (let ((list eclipse-check-libraries) aux)
    (setq aux (read-from-minibuffer 
	       (if (not list)
		   "Libraries to load (use <SPACE> to seperate): "
		 (let ((list-string (car list)))
		   (setq list (cdr list))
		   (while (car list)
		     (setq list-string (concat list-string " " (car list))
			   list (cdr list)))
		   (concat "Libraries to load (default " list-string "): ")))))
    (unless (or (not aux) (= 0 (string-width aux)))
      (setq aux (split-string aux "[^a-zA_Z0-9_]")
	    eclipse-check-libraries nil)
      (while (car aux)
	(unless (= 0 (string-width (car aux)))
	  (if eclipse-check-libraries
	      (setq eclipse-check-libraries (nconc eclipse-check-libraries (list (car aux))))
	    (setq eclipse-check-libraries (list (car aux))))
	  (setq aux (cdr aux)))))))

(defun eclipse-check-lib-call ()
  ;; build the library load call
  (let ((lib-call nil) (aux eclipse-check-libraries))
    (while (car aux)
      (setq lib-call (concat lib-call ":- lib(" (car aux) ").\n")
	    aux (cdr aux)))
    lib-call))

(defun eclipse-highlight ()
  "Highlight all occurrences of the current word in the current buffer.
Any other highlighting is removed."
  (interactive)
  (when (or (looking-at "[a-zA-Z_]")
	    (and (looking-at "[0-9_]")
		 (save-excursion
		   (re-search-backward "[^a-zA-Z0-9_]" (point-min) t)
		   (forward-char)
		   (looking-at "[a-zA-Z_][a-zA-Z0-9_]+"))))
    (let* ((beg (save-excursion
		  (re-search-backward "[^a-zA-Z0-9_]" (point-min) t)
		  (forward-char)
		  (point)))
	   (end (save-excursion
		  (re-search-forward "[^a-zA-Z0-9_]" (point-max) t)
		  (backward-char)
		  (point)))
	   (str (buffer-substring beg end))
	   (len (length str))
	   ovl)
      (if (string-equal str eclipse-highlighted)
	  (eclipse-dehighlight)
	(eclipse-dehighlight)
	(setq eclipse-highlighted str)
	(save-excursion
	  (goto-char (point-min))
	  (while (not (= (point) (point-max)))
	    (when (search-forward str (point-max) 1)
	      (backward-char len)
	      (setq beg (point)
		    end (+ beg len)
		    ovl (make-overlay beg end))
	      (overlay-put ovl 'face 'eclipse-highlight-face)
	      (eclipse-add-overlay ovl)
	      (forward-char len))))))))

(defun eclipse-add-overlay (ovl)
  ;; add overlay to list of overlays
  (if eclipse-overlays
      (setq eclipse-overlays (append eclipse-overlays (list ovl)))
    (setq eclipse-overlays (list ovl))))

(defun eclipse-dehighlight ()
  "Remove any highlighting from the current buffer."
  (interactive)
  (let (ovl)
    (while eclipse-overlays
      (setq ovl (car eclipse-overlays)
	    eclipse-overlays (cdr eclipse-overlays))
      (delete-overlay ovl))
    (setq eclipse-highlighted nil)))

(defun eclipse-goto-highlight-backward ()
  "Go backwards to previous occurrence of highlighted word"
  (interactive)
  (when eclipse-highlighted
    (search-backward eclipse-highlighted (point-min) t)))

(defun eclipse-goto-highlight-forward ()
  "Go forwards to next occurrence of highlighted word"
  (interactive)
  (when eclipse-highlighted
    (when (looking-at eclipse-highlighted)
      (forward-char (length eclipse-highlighted)))
    (search-forward eclipse-highlighted (point-max) t)
    (backward-char (length eclipse-highlighted))))

;;
;; Inferior eclipse mode
;;

(defvar inferior-eclipse-mode-map nil)

(defun inferior-eclipse-mode ()
  "Major mode for interacting with an inferior ECLiPSe process.

The following commands are available:

\\{inferior-eclipse-mode-map}

Entry to this mode calls the value of `eclipse-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`eclipse-mode-hook' is called after `comint-mode-hook'.

Commands:
Return at end of buffer sends line as input.

\\[run-eclipse] opens inferior process buffer (if not already open) and starts ECLiPSe.

\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[stop-eclipse] or \\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-quit-subjob] sends quit signal. \\[kill-eclipse] sends quit signal and closes the process
buffer.

\\[eclipse-start-tools] starts the TkTools program.

You can send text to the inferior ECLiPSe process from other buffers using
the commands \\[eclipse-compile-buffer] and \\[eclipse-compile-region].
\\[eclipse-compile-region-and-go] sends the region to the inferior process and switches to the process
buffer. Use \\[eclipse-run-region] to send text as ECLiPSe commands.

If there is a problem with entering commands in the inferior ECLiPSe process
window, disable the line
     (define-key map \"\\r\" 'eclipse-next-line)
in the definition of function `eclipse-mode-commands' in the ECLiPSe mode file
eclipse.el."
  (interactive)
  (require 'comint)
  (comint-mode)
  (setq major-mode 'inferior-eclipse-mode
	mode-name "Inferior Eclipse"
	comint-prompt-regexp "\\[eclipse [1-9][0-9]*\\]: ")
  (eclipse-mode-variables)
  (unless inferior-eclipse-mode-map
    (setq inferior-eclipse-mode-map (copy-keymap comint-mode-map))
    (eclipse-mode-commands inferior-eclipse-mode-map))
  (easy-menu-define
   inferior-eclipse-process-menu inferior-eclipse-mode-map
   "ECLiPSe menu for inferior ECLiPSe process"
   '("ECLiPSe"
     ["Run ECLiPSe" run-eclipse t]
     ["Stop ECLiPSe" stop-eclipse t]
     ["Kill ECLiPSe" kill-eclipse t]
     "--"
     ["Start TkTools" eclipse-start-tools t]))
  (easy-menu-add inferior-eclipse-process-menu)
  (use-local-map inferior-eclipse-mode-map)
  (run-hooks 'eclipse-mode-hook))

;;;###autoload
(defun run-eclipse ()
  "Run an inferior ECLiPSe process, input and output via buffer *eclipse*."
  (interactive)
  (require 'comint)
  (let (eclipse-window)
    (save-excursion
      (setq eclipse-window (get-buffer-window "*eclipse*"))
      (cond ((not eclipse-window)
	     (split-window)
	     (select-window (next-window)))
	    (t (select-window eclipse-window)))
      (add-hook 'comint-preoutput-filter-functions 'eclipse-safe-output)
      (unless (> eclipse-version 0.0)
	(add-hook 'comint-preoutput-filter-functions 'eclipse-get-version))
      (switch-to-buffer (make-comint "eclipse" eclipse-program-call))
      (inferior-eclipse-mode))))

(defun eclipse-safe-output (output)
  "Make sure that output is always added at the end of the buffer"
  (end-of-buffer)
  output)

(defun eclipse-get-version (output)
  "Extract the version number of ECLiPSe."
  (when (string-match "#" output)
    (setq eclipse-version (string-to-number (nth 0 (split-string (nth 1 (split-string output "Version ")) " #"))))
    (remove-hook 'comint-preoutput-filter-functions 'eclipse-get-version))
  output)

(defun stop-eclipse ()
  "Send C-c to an inferior ECLiPSe process."
  (interactive)
  (let (eclipse-window eclipse-status)
    (save-excursion
      (setq eclipse-window (get-buffer-window "*eclipse*")
	    eclipse-status (process-status "eclipse"))
      (cond ((not eclipse-status)
	     (beep)
	     (message "No ECLiPSe process running"))
	    (t
	     (process-send-string "eclipse" "\C-c")
	     (if (not eclipse-window)
		 (switch-to-buffer "*eclipse*")
	       (select-window eclipse-window)))))))

(defun kill-eclipse ()
  "Kill an inferior ECLiPSe process."
  (interactive)
  (let (eclipse-window eclipse-status exists)
    (save-excursion
      (setq eclipse-window (get-buffer-window "*eclipse*")
	    eclipse-status (process-status "eclipse"))
      (unless (not eclipse-status)
	(process-send-string "eclipse" eclipse-halt-string))
      (setq exists (get-buffer "*eclipse*"))
      (when (bufferp exists)
	(kill-buffer "*eclipse*")) 
      (setq exists (get-buffer "*tktools*"))
      (when (bufferp exists)
	(kill-buffer "*tktools*"))
      (unless (not eclipse-window)
	(delete-window eclipse-window)))))

(defun eclipse-start-tools ()
  "Start TkTools for an inferior ECLiPSe process in an external Tcl/Tk window.
The socket number shown in the ECLiPSe process buffer has to be entered
manually into the input field \"Port\" in the external TkTools window."
  (interactive)
  (let (eclipse-window eclipse-status tools-status tools-buffer)
    (save-excursion
      (setq eclipse-status (process-status "eclipse")
	    tools-buffer (get-buffer "*tktools*")
	    tools-status (process-status "tktools"))
      (cond ((not eclipse-status)
	     (beep)
	     (message "No ECLiPSe process running"))
	    (tools-status
	     (beep)
	     (message "TkTools already running"))
	    (t
	     ;; close buffer if open from last time tools were started
	     (unless (not tools-buffer)
	       (kill-buffer tools-buffer))
	     (setq eclipse-window (get-buffer-window "*eclipse*"))
	     (if (not eclipse-window)
		 (switch-to-buffer "*eclipse*")
	       (select-window eclipse-window))
	     (cond ((or eclipse-xemacs (< eclipse-version 5.4))
		    ;; if version < 5.4 there's no automatic start-up
		    (start-process "tktools" "*tktools*" eclipse-tktools-call)
		    (insert eclipse-53-tktools-call)
		    (eclipse-set-process-mark)
		    (process-send-string "eclipse" eclipse-53-tktools-call)
		    (message "Enter socket number in TkTools input field \"Port\" and press \"OK\""))
		   (t
		    ;; add to preoutput filters
		    (add-hook 'comint-preoutput-filter-functions eclipse-run-tktools-func)
		    (insert eclipse-54-tktools-call)
		    (eclipse-set-process-mark)
		    (process-send-string "eclipse" eclipse-54-tktools-call))))))))

(defun eclipse-run-tktools (output)
  ;; Extracts Host and Port parameters from output, starts TkTools
  ;; with host and port parameters.
  ;; This command needs ECLiPSe 5.4 or later.
  (let (host port output-list list head tail)
    (cond ((string-match "\\[.*, .*\\]\n" output)
	   (setq output-list (split-string output ": "))
	   (cond ((> (length output-list) 1)
		  (setq head (nth 0 output-list)
			tail (nth 1 output-list)))
		 (t (setq head ""
			  tail (nth 0 output-list))))
	   (setq list (split-string tail "\\(\\[\\|\\]\\|, \\)")
		 host (nth 0 list)
		 port (nth 1 list))
	   (start-process "tktools" "*tktools*" eclipse-tktools-name "--" "-h" host "-p" port)
	   (remove-hook 'comint-preoutput-filter-functions eclipse-run-tktools-func)
	   head)
	  (t output))))


(defun eclipse-run-region (compile beg end)
  "Send the region to an inferior ECLiPSe process."
  (interactive "P\nr")
  (let (eclipse-window)
    (save-excursion
      (process-send-region "eclipse" beg end)
      (setq eclipse-window (get-buffer-window "*eclipse*"))
      (if (not eclipse-window)
	  (switch-to-buffer "*eclipse*")
	(select-window eclipse-window)))))

(defun eclipse-compile-buffer ()
  "Send the entire buffer to an inferior ECLiPSe process and compile it."
  (interactive)
  (save-excursion
    (process-send-string "eclipse" eclipse-compile-string)
    (process-send-string "eclipse" (buffer-string))
    (process-send-string "eclipse" "\n")		;May be unnecessary
    (if eclipse-eof-string
	(process-send-string "eclipse" eclipse-eof-string)
      (process-send-eof "eclipse")))) ;Send eof to eclipse process.

(defun eclipse-compile-region (compile beg end)
  "Send the region to an inferior ECLiPSe process and compile it."
  (interactive "P\nr")
  (save-excursion
    (process-send-string "eclipse" eclipse-compile-string)
    (process-send-region "eclipse" beg end)
    (process-send-string "eclipse" "\n")		;May be unnecessary
    (if eclipse-eof-string
	(process-send-string "eclipse" eclipse-eof-string)
      (process-send-eof "eclipse")))) ;Send eof to eclipse process.

(defun eclipse-compile-region-and-go (compile beg end)
  "Send the region to an inferior ECLiPSe process, compile it, and switch to
*eclipse* buffer."
  (interactive "P\nr")
  (eclipse-compile-region compile beg end)
  (let (eclipse-window)
    (setq eclipse-window (get-buffer-window "*eclipse*"))
    (if (not eclipse-window)
	(switch-to-buffer "*eclipse*")
      (select-window eclipse-window))))

;;
;; ECLiPSe Outline commands
;;

;; this part has been adapted from the GNU Emacs outline.el file
;; Copyright (C) 1986, 93, 94, 95, 97, 2000, 2001, 2003
;;   Free Software Foundation, Inc.

;;; Commentary:

;; This part provides outline commands inside the ECLiPSe major mode.
;; It has been adapted from the GNU Emacs outline major mode.
;; The commands are specific for editing ECLiPSe program files.

;;; Code:

(defvar eclipse-outline-regexp "[a-z]\\|:-\\|[ \t]+("
  "*Regular expression to match the beginning of a heading.
 Any line whose beginning matches this regexp is considered to start a heading.")

(defvar eclipse-outline-heading-end-regexp "[.]?[ \t]*\n"
  "*Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.")

(defvar eclipse-outline-view-change-hook nil
  "Normal hook to be run after outline visibility changes.")

(defun eclipse-outline (map)
  ;; add functions for outlining to map
  (require 'outline)
  (eclipse-outline-define-map map)
  (set (make-local-variable 'line-move-ignore-invisible) nil)
  ;; Cause use of ellipses for invisible text.
  (if eclipse-xemacs
      (setq selective-display t)
    (add-to-invisibility-spec '(outline . t))))

(defun eclipse-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line."
  (save-excursion
    (looking-at eclipse-outline-regexp)
    (if (looking-at "\\([a-z]\\|[:?]-\\)")
	0
      (- (match-end 0) (match-beginning 0)))))

(defun eclipse-outline-next-preface ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (when (re-search-forward (concat "\n\\(" eclipse-outline-regexp "\\)") nil 'move)
    (goto-char (match-beginning 0)))
  (when (and (bolp) (not (bobp)))
    (forward-char -1)))

(defun eclipse-outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (when (re-search-forward (concat "\\(/\\*\\|\n\\(" eclipse-outline-regexp "\\)\\)") nil 'move)
    (progn
      (let ((aux (1+ (match-beginning 0))))
	(goto-char (match-beginning 0))
	(cond ((looking-at "/\\*")
	       (unless (re-search-forward "\\*/" (point-max) t)
		 (goto-char (point-max)))
	       (eclipse-outline-next-heading))
	      (t (goto-char aux)))))))

(defun eclipse-outline-previous-heading ()
  "Move to the previous (possibly invisible) heading line."
  (interactive)
  (re-search-backward (concat "^\\(" eclipse-outline-regexp "\\)") nil 'move))

(defsubst eclipse-outline-invisible-p ()
  "Non-nil if the character after point is invisible."
  (get-char-property (point) 'invisible))

(defun eclipse-outline-visible ()
  "Obsolete.  Use `eclipse-outline-invisible-p'."
  (not (eclipse-outline-invisible-p)))

(defun eclipse-outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (beginning-of-line)
  (or (eclipse-outline-on-heading-p invisible-ok)
      (let (found)
	(save-excursion
	  (while (not found)
	    (or (re-search-backward (concat "^\\(" eclipse-outline-regexp "\\)") nil t)
		(error "Before first heading"))
	    (setq found (and (or invisible-ok (eclipse-outline-visible)) (point)))))
	(goto-char found)
	found)))

(defun eclipse-outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (eclipse-outline-visible))
	 (looking-at eclipse-outline-regexp))))

(defun eclipse-outline-end-of-heading ()
  ;; go to end of current heading
  (when (re-search-forward eclipse-outline-heading-end-regexp nil 'move)
    (skip-chars-backward " \t.\n")))

(defun eclipse-outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With argument, repeats or can move backward if negative.
A heading line is one that `eclipse-outline-regexp' matches."
  (interactive "p")
  (if (< arg 0)
      (beginning-of-line)
    (end-of-line))
  (while (and (not (bobp)) (< arg 0))
    (while (and (not (bobp))
		(re-search-backward (concat "^\\(" eclipse-outline-regexp "\\)") nil 'move)
		(not (eclipse-outline-visible))))
    (setq arg (1+ arg)))
  (while (and (not (eobp)) (> arg 0))
    (while (and (not (eobp))
		(re-search-forward (concat "^\\(" eclipse-outline-regexp "\\)") nil 'move)
		(not (eclipse-outline-visible))))
    (setq arg (1- arg)))
  (beginning-of-line))

(defun eclipse-outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that `outline-regexp' matches."
  (interactive "p")
  (eclipse-outline-next-visible-heading (- arg)))

(defun eclipse-outline-mark-subtree ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (eclipse-outline-on-heading-p)
	;; we are already looking at a heading
	(beginning-of-line)
      ;; else go back to previous heading
      (eclipse-outline-previous-visible-heading 1))
    (setq beg (point))
    (eclipse-outline-end-of-subtree)
    (push-mark (point))
    (goto-char beg)))

(defun eclipse-outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (let ((flag1 (if eclipse-xemacs
		   (if flag ?\^M ?\n)
		 flag)))
    (if eclipse-xemacs
	;; running XEmacs, so...
	(subst-char-in-region from to (if (= flag1 ?\n) ?\^M ?\n) flag1 t)
      ;; running Emacs proper, so we can use overlays
      (save-excursion
	(goto-char from)
	(end-of-line)
	(eclipse-outline-discard-overlays (point) to 'outline)
	(when flag1
	  (let ((o (make-overlay (point) to)))
	    (overlay-put o 'invisible 'outline)
	    (overlay-put o 'isearch-open-invisible 'eclipse-outline-isearch-open-invisible))))
      (run-hooks 'eclipse-outline-view-change-hook))))

;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `eclipse-outline-flag-region').
(defun eclipse-outline-isearch-open-invisible (overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (show-entry))

;; Exclude from the region BEG ... END all overlays
;; which have PROP as the value of the `invisible' property.
;; Exclude them by shrinking them to exclude BEG ... END,
;; or even by splitting them if necessary.
;; Overlays without such an `invisible' property are not touched.
(defun eclipse-outline-discard-overlays (beg end prop)
  (let (o)
    (when (< end beg)
      (setq beg (prog1 end (setq end beg))))
    (save-excursion
      (eclipse-dolist
       (o (overlays-in beg end))
       (when (eq (overlay-get o 'invisible) prop)
	 ;; Either push this overlay outside beg...end
	 ;; or split it to exclude beg...end
	 ;; or delete it entirely (if it is contained in beg...end).
	 (if (< (overlay-start o) beg)
	     (if (> (overlay-end o) end)
		 (progn
		   (move-overlay (eclipse-outline-copy-overlay o) (overlay-start o) beg)
		   (move-overlay o end (overlay-end o)))
	       (move-overlay o (overlay-start o) beg))
	   (if (> (overlay-end o) end)
	       (move-overlay o end (overlay-end o))
	     (delete-overlay o))))))))

;; this macro is taken from subr.el --- basic lisp subroutines for Emacs
;; Copyright (C) 1985, 86, 92, 94, 95, 99, 2000, 2001
;;   Free Software Foundation, Inc.
;;
;; defined here because it is not available in Emacs-20
(defmacro eclipse-dolist (spec &rest body)
  "(dolist (VAR LIST [RESULT]) BODY...): loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil."
  (let ((temp (make-symbol "--dolist-temp--")))
    (list 'let (list (list temp (nth 1 spec)) (car spec))
	  (list 'while temp
		(list 'setq (car spec) (list 'car temp))
		(cons 'progn (append body (list (list 'setq temp (list 'cdr temp))))))
	  (when (cdr (cdr spec))
	    (cons 'progn (cons (list 'setq (car spec) nil) (cdr (cdr spec))))))))

(defun eclipse-outline-copy-overlay (o)
;; Make a copy of overlay O, with the same beginning, end and properties.
  (let ((o1 (make-overlay (overlay-start o) (overlay-end o) (overlay-buffer o)))
	(props (overlay-properties o)))
    (while props
      (overlay-put o1 (car props) (nth 1 props))
      (setq props (cdr (cdr props))))
    o1))

(defun eclipse-hide-clause ()
  "Hide the clause directly following this heading."
  (interactive)
  (unless (eclipse-check-clause-begin)
    (eclipse-goto-clause-begin))
  (eclipse-outline-end-of-heading)
  (save-excursion
    (eclipse-outline-flag-region
     (point)
     (progn (eclipse-goto-clause-end) (point)) t)))

(defun eclipse-show-clause ()
  "Show the clause directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (unless (eclipse-check-clause-begin)
      (eclipse-goto-clause-begin))
    (eclipse-outline-flag-region
     (1- (point))
     (progn (eclipse-goto-clause-end) (point)) nil)))

(defun eclipse-hide-predicate ()
  "Hide the predicate directly following this heading."
  (interactive)
  (unless (eclipse-check-predicate-begin)
    (eclipse-goto-predicate-begin))
  (eclipse-outline-end-of-heading)
  (save-excursion
    (eclipse-outline-flag-region
     (point)
     (progn (eclipse-goto-predicate-end) (point)) t)))

(defun eclipse-show-predicate ()
  "Show the predicate directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (unless (eclipse-check-predicate-begin)
      (eclipse-goto-predicate-begin))
    (eclipse-outline-flag-region
     (1- (point))
     (progn (eclipse-goto-predicate-end) (point)) nil)))

(defun eclipse-hide-predicates ()
  "Hide all of buffer except predicate headings."
  (interactive)
  (eclipse-hide-region-body (point-min) (point-max)))

(defun eclipse-hide-clauses ()
  "Hide all of buffer except clause headings."
  (interactive)
  (eclipse-hide-region-body (point-min) (point-max) t))

(defun eclipse-hide-region-body (start end &optional flag)
  "Hide all body lines in the region, but not headings."
  ;; Nullify the hook to avoid repeated calls to `eclipse-outline-flag-region'
  ;; wasting lots of time running `lazy-lock-fontify-after-outline'
  ;; and run the hook finally.
  (let ((head (point-min)) eclipse-outline-view-change-hook
	(length (- end start)) (last 0))
    (message "Hiding...")
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(unless (eclipse-outline-on-heading-p)
	  (eclipse-goto-clause-begin))
	(eclipse-outline-end-of-heading)
	(while (not (eobp))
	  (eclipse-outline-flag-region
	   (point)
	   (progn (goto-char head)
		  (if flag
		      (eclipse-goto-clause-end)
		    (eclipse-goto-predicate-end))
		  (point)) t)
	  (setq last (eclipse-percent-message "Hiding" length last))
	  (unless (eobp)
	    (eclipse-outline-next-heading)
	    (setq head (point))
	    (eclipse-outline-end-of-heading))))))
  (message "Hiding...done.")
  (run-hooks 'eclipse-outline-view-change-hook))

(defun eclipse-show-predicates ()
  "Show all of buffer."
  (interactive)
  (eclipse-show-all))

(defun eclipse-show-clauses ()
  "Show all of buffer."
  (interactive)
  (eclipse-show-all))

(defun eclipse-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (eclipse-outline-flag-region (point-min) (point-max) nil))

(defun eclipse-hide-block ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (eclipse-outline-flag-subtree t))

(defun eclipse-show-block ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (eclipse-outline-flag-subtree nil))

(defun eclipse-outline-flag-subtree (flag)
  ;; flag subtree
  (save-excursion
    (eclipse-outline-back-to-heading)
    (eclipse-outline-end-of-heading)
    (eclipse-outline-flag-region
     (point)
     (progn (eclipse-outline-end-of-subtree) (point))
     flag)))

(defun eclipse-outline-end-of-subtree ()
  ;; go to end of subtree
  (eclipse-outline-back-to-heading)
  (if (looking-at "[ \t]+(")
      (eclipse-outline-end-of-block)
    (let ((first t)
	  (level (eclipse-outline-level)))
      (while (and (not (eobp))
		  (or first (> (eclipse-outline-level) level)))
	(setq first nil)
	(eclipse-outline-next-heading))
      (when (bolp)
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
	  (when (bolp)
	    ;; leave blank line before heading
	    (forward-char -1)))))))

(defun eclipse-outline-end-of-block ()
  ; we are looking at "[ \t]+("
  (let (level)
    (skip-chars-forward " \t")
    (forward-char)
    (setq level 1)
    (while (not (or (eobp) (= level 0)))
      (cond ((looking-at ")")
	     (setq level (- level 1))
	     (forward-char))
	    ((looking-at "(")
	     (setq level (+ level 1))
	     (forward-char))
	    ((looking-at "'")
	     (eclipse-goto-end-of-quote))
	    ((looking-at "\"")
	     (eclipse-goto-end-of-string))
	    (t (forward-char))))
    (forward-char)))

(defun eclipse-outline-up-heading-all (arg)
  "Move to the heading line of which the present line is a subheading.
This function considers both visible and invisible heading lines.
With argument, move up ARG levels."
  (eclipse-outline-back-to-heading t)
  (when (eq (eclipse-outline-level) 1)
    (error "Already at top level of the outline"))
  (while (and (> (eclipse-outline-level) 1)
	      (> arg 0)
	      (not (bobp)))
    (let ((present-level (eclipse-outline-level)))
      (while (and (not (< (eclipse-outline-level) present-level))
		  (not (bobp)))
	(eclipse-outline-previous-heading))
      (setq arg (- arg 1)))))

(defun eclipse-outline-up-heading (arg)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (eclipse-outline-back-to-heading)
  (when (eq (eclipse-outline-level) 1)
    (error "Already at top level of the outline"))
  (while (and (> (eclipse-outline-level) 1)
	      (> arg 0)
	      (not (bobp)))
    (let ((present-level (eclipse-outline-level)))
      (while (and (not (< (eclipse-outline-level) present-level))
		  (not (bobp)))
	(eclipse-outline-previous-visible-heading 1))
      (setq arg (- arg 1)))))

(defun eclipse-outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (eclipse-outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion (eclipse-outline-get-next-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No following same-level heading"))))))

(defun eclipse-outline-get-next-sibling ()
  "Move to next heading of the same level, and return point or nil if none."
  (let ((level (eclipse-outline-level)))
    (eclipse-outline-next-visible-heading 1)
    (while (and (> (eclipse-outline-level) level)
		(not (eobp)))
      (eclipse-outline-next-visible-heading 1))
    (if (< (eclipse-outline-level) level)
	nil
      (point))))

(defun eclipse-outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (eclipse-outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion (eclipse-outline-get-last-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No previous same-level heading"))))))

(defun eclipse-outline-get-last-sibling ()
  "Move to previous heading of the same level, and return point or nil if none."
  (let ((level (eclipse-outline-level)))
    (eclipse-outline-previous-visible-heading 1)
    (while (and (> (eclipse-outline-level) level)
		(not (bobp)))
      (eclipse-outline-previous-visible-heading 1))
    (if (< (eclipse-outline-level) level)
	nil
      (point))))

(defun eclipse-outline-headers-as-kill (beg end)
  "Save the visible outline headers in region at the start of the kill ring.

Text shown between the headers isn't copied.  Two newlines are
inserted between saved headers.  Yanking the result may be a
convenient way to make a table of contents of the buffer."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((buffer (current-buffer))
	    start end)
	(with-temp-buffer
	  (with-current-buffer buffer
	    ;; Boundary condition: starting on heading:
	    (when (eclipse-outline-on-heading-p)
	      (eclipse-outline-back-to-heading)
	      (setq start (point)
		    end (progn (eclipse-outline-end-of-heading) (point)))
	      (insert-buffer-substring buffer start end)
	      (insert "\n\n")))
	  (let ((temp-buffer (current-buffer)))
	    (with-current-buffer buffer
	      (while (eclipse-outline-next-heading)
		(when (eclipse-outline-visible)
		  (setq start (point)
			end (progn (eclipse-outline-end-of-heading) (point)))
		  (with-current-buffer temp-buffer
		    (insert-buffer-substring buffer start end)
		    (insert "\n\n"))))))
	  (kill-new (buffer-string)))))))


(provide 'eclipse)

;;; eclipse.el ends here
