(defun company-record (line)
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :id ht) (parse-integer (first line)))
    (setf (gethash :name ht) (second line))
    (setf (gethash :country ht) (third line))
    (setf (gethash :founder ht) (cadddr line)))
  ht)

(defun spacecraft-record (line)
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :id ht (parse-integer (first line))))
    (setf (gethash :bane ht) (second line))
    (setf (gethash :type ht) (third line))
    (setf (gethash :company-id ht))))

(defun read-csv (file-hash file-tipe)
  (let ((ht (make-hash-table :test 'equal))))

  (with-open-file (stream file-hash)
    (do ((line (read-line stream nil) (read-line stream nil)))
        ((null line) t)
      (let* ((breaking (uiop:split-string line :separator ","))
             (record (case file-type
                       (:companies (company-record breaking))
                       (:spacecrafts (spacecraft-record breaking))))
             ())))))


(read-csv "c:/Users/LoPHarp/portacle/Lisp_5/companies.csv" :companies)
