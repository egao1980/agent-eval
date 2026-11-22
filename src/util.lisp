(in-package #:agent-eval)

;;; Small helpers shared by the E0 files: hex digests through crypto-protocol,
;;; UTF-8 through encoding-protocol, text files, UTC timestamps, fresh
;;; temporary directories, and the one Lisp-data → json-protocol value mapping
;;; used by EVAL-REPORT :JSON and COMPARISON-REPORT :JSON.

(defun %utf8 (string)
  "STRING's UTF-8 octets (encoding-protocol)."
  (encoding-protocol:encode string))

(defun %hex (octets)
  "Lowercase hex of OCTETS."
  (format nil "~(~{~2,'0x~}~)" (coerce octets 'list)))

(defun %sha256-hex (octets)
  "Lowercase hex SHA-256 of OCTETS (crypto-protocol DIGEST)."
  (%hex (crypto:digest octets :algorithm :sha256)))

(defun %concatenate-octets (parts)
  "One simple octet vector holding the octet vectors PARTS in order."
  (let ((out (make-array (reduce #'+ parts :key #'length)
                         :element-type '(unsigned-byte 8)))
        (i 0))
    (dolist (p parts out)
      (replace out p :start1 i)
      (incf i (length p)))))

(defun %read-octets (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (let ((n (read-sequence v s)))
        (if (= n (length v)) v (subseq v 0 n))))))

(defun %text-external-format ()
  "UTF-8 that decodes malformed input to U+FFFD instead of signalling, so
binary command output never breaks a trial."
  #+sbcl (list :utf-8 :replacement (code-char #xfffd))
  #-sbcl :utf-8)

(defun %read-text-file (pathname)
  "PATHNAME's text (UTF-8, lenient), or \"\" when it does not exist."
  (if (probe-file pathname)
      (uiop:read-file-string pathname :external-format (%text-external-format))
      ""))

(defun %write-text-file (pathname text)
  "Write TEXT to PATHNAME (UTF-8), making its directory. → PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
    (write-string text s))
  pathname)

(defun %random-hex (n)
  (let ((state (make-random-state t)))
    (with-output-to-string (s)
      (dotimes (i n) (format s "~(~x~)" (random 16 state))))))

(defun %fresh-directory (prefix)
  "A new empty directory under the temporary directory (its truename)."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "~a-~a" prefix (%random-hex 12))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (truename dir)))

(defun %directory-truename (designator)
  "DESIGNATOR as a directory pathname, made when missing, as its truename."
  (let ((dir (uiop:ensure-directory-pathname designator)))
    (ensure-directories-exist dir)
    (truename dir)))

(defun %utc-timestamp (&optional (universal-time (get-universal-time)) compact)
  "UTC \"YYYY-MM-DDTHH:MM:SSZ\" (COMPACT: \"YYYYMMDDTHHMMSSZ\")."
  (multiple-value-bind (s mi h d mo y) (decode-universal-time universal-time 0)
    (if compact
        (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ" y mo d h mi s)
        (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" y mo d h mi s))))

(defun %elapsed-ms (start)
  "Integer milliseconds since internal real time START."
  (values (round (* 1000 (- (get-internal-real-time) start))
                 internal-time-units-per-second)))

(defun %plist-p (x)
  "A non-empty proper list of even length with keywords in key positions."
  (and (consp x)
       (handler-case (evenp (length x)) (type-error () nil))
       (loop for (k nil) on x by #'cddr always (keywordp k))))

(defun %plist-get (plist key)
  "GETF that tolerates a malformed PLIST (→ NIL)."
  (loop for (k v) on plist by #'cddr
        when (eq k key) return v))

;;; JSON mapping (records and comparisons)

(defparameter +json-flags+ '(:passed :cost-complete-p :timed-out-p)
  "Members whose NIL is JSON false; every other NIL is JSON null.")

(defun %json-key (key)
  (etypecase key
    (string key)
    (integer (princ-to-string key))
    (symbol (substitute #\_ #\- (string-downcase (symbol-name key))))))

(defun %json-alist-p (x)
  "An alist of (ATOM . ATOM) pairs: pass@k and rewards tables."
  (and (consp x)
       (every (lambda (e) (and (consp e) (atom (car e)) (car e) (atom (cdr e)))) x)))

(defun %jsonify (value &optional key)
  "VALUE (records data: plists, alists, lists, strings, numbers, keywords) as a
json-protocol value. KEY is the member VALUE belongs to (for the flags)."
  (cond ((null value) (if (member key +json-flags+) nil :null))
        ((eq value t) t)
        ((stringp value) value)
        ((keywordp value) (string-downcase (symbol-name value)))
        ((symbolp value) (string-downcase (symbol-name value)))
        ((integerp value) value)
        ((realp value) (float value 1d0))
        ((hash-table-p value) value)
        ((%plist-p value)
         (loop for (k v) on value by #'cddr
               collect (cons (%json-key k) (%jsonify v k))))
        ((%json-alist-p value)
         (loop for (k . v) in value
               collect (cons (%json-key k) (%jsonify v k))))
        ((listp value) (map 'vector (lambda (v) (%jsonify v)) value))
        ((vectorp value) (map 'vector (lambda (v) (%jsonify v)) value))
        (t (princ-to-string value))))

(defun %json-text (value)
  "VALUE (records data) encoded as one JSON text."
  (json:encode (%jsonify value)))
