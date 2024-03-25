(in-package #:cl-pacmd-parser)

(defrule integer (+ (character-ranges (#\0 #\9)))
  (:lambda (digits)
    (parse-integer (text digits))))

(defrule infinity (and (? (or #\- #\+)) "inf")
  (:destructure (sign str)
    (if (string= sign "-")
        :-infinity
        :+infinity)))

(defrule real (and (? (or #\- #\+))
                   (+ (character-ranges ( #\0 #\9)))
                   (or #\, #\.)
                   (+ (character-ranges ( #\0 #\9))))
  (:destructure (sign decimal separator fraction)
    (declare (ignore separator))
    (/ (parse-integer (text sign decimal fraction))
       (expt 10 (length fraction)))))

(defrule ws (* (or #\Space #\Tab)))

(defrule word (+ (or (alphanumericp character) #\_)))

(defrule words (and word (* (and #\Space word)))
  (:lambda (components)
    (text components)))

(defrule identifier (+ (or word #\- #\.))
  (:lambda (components)
    (text components)))

(defun not-newline-p (arg)
  (char/= #\Newline arg))

(defrule rest-of-line (and (* (not-newline-p character)) #\Newline)
  (:lambda (chars)
    (text (car chars))))

(defrule empty-line (and ws #\Newline)
  (:constant nil))

(defun strip-delimiters (val)
  (subseq val 1 (1- (length val))))

(defrule property (and #\Tab (+ #\Tab) identifier ws #\= ws rest-of-line)
  (:destructure (tab tabs id ws1 assignment ws2 value)
    (declare (ignore tab tabs ws1 assignment ws2))
    (list id (strip-delimiters value))))

(defrule properties (and (+ #\Tab) "properties:" #\Newline (or empty-line (+ property)))
  (:destructure (tabs str nl props)
    (declare (ignore tabs str nl))
    props))

(defrule port (and #\Tab #\Tab identifier #\: ws rest-of-line properties)
  (:destructure (tab tab name colon ws1 desc props)
    (declare (ignore tab tab colon ws1))
    (list :name name :description desc :properties props)))

(defrule ports (and #\Tab "ports:" #\Newline (+ port) #\Tab "active port: " rest-of-line)
  (:destructure (tab1 str1 nl ports tab2 str2 active)
    (declare (ignore tab1 str1 nl tab2 str2))
    (let ((active (strip-delimiters active)))
      (mapcar (lambda (port)
                (append port
                        (list :active
                              (string= (getf port :name) active))))
              ports))))

(defrule channel-volume-db (and " / " (or infinity real) " dB")
  (:destructure (str1 val str2)
    (declare (ignore str1 str2))
    val))

(defrule channel-volume (and (? (and #\, ws))
                             identifier
                             ": "
                             integer
                             " / "
                             ws
                             integer
                             "%"
                             (? channel-volume-db))
  (:destructure (str1 id str2 steps str3 str4 percent str5 db)
    (declare (ignore str1 str2 str3 str4 str5))
    (list id (list steps percent db))))

(defrule volume
  (and #\Tab "volume: " (+ channel-volume) #\Newline rest-of-line)
  (:destructure (tab tag vols nl specifier)
    (declare (ignore tab tag nl specifier))
    (list "volume" vols)))

(defrule descriptor
  (and #\Tab words ": " rest-of-line (? (and #\Tab (+ #\Space) rest-of-line)))
  (:destructure (tab name colon val specifiers)
    (declare (ignore tab colon specifiers))
    (list name val)))

(defrule index (and "index: " integer #\Newline)
  (:destructure (str val nl)
    (declare (ignore str nl))
    val))

(defrule object (and (or "    " "  * ")
                     index
                     (+ (or volume descriptor))
                     properties
                     (? ports))
  (:destructure (default idx descriptors props ports)
    (list :default (equal default "  * ")
          :index idx
          :descriptors descriptors
          :properties props
          :ports ports)))

(defrule objects (and rest-of-line (+ object))
  (:destructure (num_and_type objects)
   (declare (ignore num_and_type))
    objects))

(defun parse-listing (output)
  (parse 'objects output))
