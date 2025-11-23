<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Мануйлов Денис Денисович КВ-21</p>
<p align="right"><b>Рік</b>: 2025</p>  

## Загальне завдання  
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.  
1. Визначити структури та/або утиліти для створення записів з таблиць (в залежності
від типу записів, заданого варіантом).  
2. Розробити утиліту(-и) для зчитування таблиць з файлів. Значення колонок мають
бути розібрані відповідно до типу даних у них. Наприклад, рядок — це просто
рядок; числові колонки необхідно розібрати як цілі числа або числа з рухомою
крапкою.  
3. Розробити функцію `select` , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. `select` повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у `select` . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.  
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):  
структури у геш-таблиці  
геш-таблиці у асоціативні списки  
асоціативні списки у геш-таблиці  
6. Написати функцію(-ї) для "красивого" виводу записів таблиці (pretty-print).  
## Варіант 11
| Варіант | База даних | Тип записів |
| :---: | :--- | :--- |
| 11 | Космічні апарати | Геш-таблиця |  

| Назва | Таблиці | Опис |
| :--- | :--- | :--- |
| Космічні апарати | 1. Компанії<br>2. Космічні апарати | База даних космічних апаратів для зв'язку, дослідження, тощо. |
## Лістинг реалізації завдання
```lisp
(defun pretty-print (table)
  (let ((rows (if (listp table) table (list table))))
    (if (null rows)
        (format t "Input table\string is empty!~%")
        (let ((colums '()))
          (maphash (lambda (key value)
                     (declare (ignore value))
                     (push key colums))
                   (first rows))
          (setf colums (nreverse colums))
          (format t "~%")
          (dolist (colum colums) (format t "~15A" colum))
          (format t "~%")
          (dolist (colum colums)
            (declare (ignore colum))
            (format t "---------------"))
          (format t "~%")

          (dolist (row rows)
            (dolist (col colums)
              (format t "~15a" (gethash col row)))
            (format t "~%"))))
    t))

(defun save-to-file (file-path records)
  (with-open-file (stream file-path :direction :output :if-exists :supersede)
    (when records
      (let ((keys '()))
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (push key keys))
                 (first records))
        (setf keys (nreverse keys))

        (dolist (row records)
          (let ((values '()))
            (dolist (key keys)
              (push (gethash key row) values))
            (setf values (nreverse values))

            (format stream "~a" (first values))
            (dolist (value (cdr values))
              (format stream ",~a" value))
            (format stream "~%"))))))
  file-path)

(defun hash-to-alist (ht)
  (let ((alist '()))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             ht)
    alist))

(defun company-record (line)
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :id ht) (parse-integer (first line)))
    (setf (gethash :name ht) (second line))
    (setf (gethash :country ht) (third line))
    (setf (gethash :founder ht) (cadddr line))
    ht))

(defun spacecraft-record (line)
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :id ht) (parse-integer (first line)))
    (setf (gethash :name ht) (second line))
    (setf (gethash :type ht) (third line))
    (setf (gethash :company-id ht) (parse-integer (cadddr line)))
    ht))

(defun select (file-hash file-type)
  (let ((ht (make-hash-table :test 'equal)))
  (with-open-file (stream file-hash)
    (do ((line (read-line stream nil) (read-line stream nil)))
        ((null line) ht)
      (let* ((clean-line (string-trim '(#\Space #\Tab #\Return #\Newline) line))
             (breaking (uiop:split-string clean-line :separator ","))
             (record (case file-type
                       (:companies (company-record breaking))
                       (:spacecrafts (spacecraft-record breaking))))
             (record-id (gethash :id record)))
        (setf (gethash record-id ht) record))))

    (lambda (&rest keys)
      (let* ((save-path (getf keys :save))
             (test-mode (getf keys :test))
             (to-alist (getf keys :alist))
             (query-keys (copy-list keys))
             (result-rows '()))
        
        (remf query-keys :save)
        (remf query-keys :test)
        (remf query-keys :alist)

        (cond
          ((null query-keys)
           (maphash (lambda (key value)
                      (declare (ignore key))
                      (push value result-rows))
                    ht))
          ((getf query-keys :id)
           (let ((record (gethash (getf query-keys :id) ht)))
             (when record (push record result-rows))))
          (t
           (let ((search-key (first query-keys))
                 (search-value (second query-keys)))
             (maphash (lambda (key row-ht)
                        (declare (ignore key))
                        (when (equal (gethash search-key row-ht) search-value)
                          (push row-ht result-rows)))
                      ht))))
        (setf result-rows  (nreverse result-rows))

        (when save-path
          (let ((actual-path (if (eq save-path t) "output.csv" save-path)))
            (save-to-file actual-path result-rows)
            (format t "Save ~a records to ~a~%" (length result-rows) actual-path)))

        (when to-alist
          (setf result-rows (mapcar #'hash-to-alist result-rows)))
        
        (if (or test-mode to-alist)
            result-rows
            (pretty-print result-rows))))))
```
### Тестові набори та утиліти
```lisp
(defun modul-test ()
  (let* ((ht (select "c:/Users/LoPHarp/portacle/Lisp_5/companies.csv" :companies))
         (data1 (funcall ht :test t))
         (data2 (funcall ht :id 1 :test t))
         (data3 (funcall ht :name "ESA" :test t))
         (data4 (funcall ht :id 1 :alist t)))
    
    (format t "Tests with the file companies.csv~%")
    (if (= (length data1) 5)
        (format t "TEST 1 : TRUE TEST~%")
        (format t "TEST 1 : FALSE TEST~%"))
    (if (and data2 (= (gethash :id (first data2)) 1))
        (format t "TEST 2 : TRUE TEST~%")
        (format t "TEST 2 : FALSE TEST~%"))
    (if (and data3 (equal (gethash :name (first data3)) "ESA"))
        (format t "TEST 3 : TRUE TEST~%")
        (format t "TEST 3 : FALSE TEST~%"))
    (if (and data4
             (listp (first data4))
             (equal (cdr (assoc :name (first data4))) "SpaceX"))
        (format t "TEST 4 : TRUE TEST~%")
        (format t "TEST 4 : FALSE TEST~%")))
  
  (let* ((ht (select "c:/Users/LoPHarp/portacle/Lisp_5/spacecrafts.csv" :spacecrafts))
         (data1 (funcall ht :test t))
         (data2 (funcall ht :id 1 :test t))
         (data3 (funcall ht :name "Starship" :test t))
         (data4 (funcall ht :id 1 :alist t)))
    (format t "Tests with the file spacecrafts.csv~%")
    (if (= (length data1) 7)
        (format t "TEST 1 : TRUE TEST~%")
        (format t "TEST 1 : FALSE TEST~%"))
    (if (and data2 (= (gethash :id (first data2)) 1) (= (gethash :company-id (first data2)) 1))
        (format t "TEST 2 : TRUE TEST~%")
        (format t "TEST 2 : FALSE TEST~%"))
    (if (and data3 (equal (gethash :name (first data3)) "Starship")
                          (equal (gethash :type (first data3)) "Rocket")
                          (= (gethash :company-id (first data3)) 1))
        (format t "TEST 3 : TRUE TEST~%")
        (format t "TEST 3 : FALSE TEST~%"))
    (if (and data4
             (listp (first data4))
             (equal (cdr (assoc :name (first data4))) "Falcon 9"))
        (format t "TEST 4 : TRUE TEST~%")
        (format t "TEST 4 : FALSE TEST~%"))))
```
### Тестування
```lisp
CL-USER> (modul-test)
Tests with the file companies.csv
TEST 1 : TRUE TEST
TEST 2 : TRUE TEST
TEST 3 : TRUE TEST
TEST 4 : TRUE TEST
Tests with the file spacecrafts.csv
TEST 1 : TRUE TEST
TEST 2 : TRUE TEST
TEST 3 : TRUE TEST
TEST 4 : TRUE TEST
NIL
```
