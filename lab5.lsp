;;; Пункт 1. Визначення структур для записів таблиць

(defstruct project
  id
  name
  model-id)

(defstruct ai-model
  id
  name)

;;; Пункт 2. Утиліти для зчитування таблиць з файлів

(defun parse-csv-line (line)
  "Розбиває рядок CSV на поля"
  (uiop:split-string (string-trim '(#\Return) line) :separator '(#\;)))

(defun read-csv-file (file-path constructor)
  "Зчитує вміст CSV файлу, повертаючи список рядків таблиці"
  (with-open-file (stream file-path :direction :input)
    (read-line stream)
    (loop for line = (read-line stream nil)
          while line
          collect (funcall constructor (parse-csv-line line)))))

(defun create-project (fields)
  "Конструктор project"
  (make-project
   :id (parse-integer (first fields))
   :name (second fields)
   :model-id (parse-integer (third fields))))

(defun create-model (fields)
  "Конструктор ai-model"
  (make-ai-model
   :id (parse-integer (first fields))
   :name (second fields)))

;;; Пункт 3. Реалізація функції select

(defun select (file-path type &rest filters)
  "Читає таблицю з CSV-файлу і повертає функцію для вибірки записів"
  (let ((records (case type
                   (:project (read-csv-file file-path #'create-project))
                   (:ai-model (read-csv-file file-path #'create-model)))))
    (lambda (&rest filter-values)
      "Повертає всі записи або фільтровані за умовами"
      (if filters
          (remove-if-not (lambda (record)
                           (every (lambda (filter value)
                                    (equal (funcall filter record) value))
                                  filters
                                  filter-values))
                         records)
          records))))

;;; Пункт 4. Утиліти для запису вибірки у файл

(defun project-get-record (record)
  "Повертає список значень полів запису"
  (list (project-id record)
        (project-name record)
        (project-model-id record)))

(defun model-get-record (record)
  "Повертає список значень полів запису"
  (list (ai-model-id record)
        (ai-model-name record)))

(defun write-csv-file (file-path selected get-record)
  "Записує список записів у CSV файл."
  (with-open-file (stream file-path :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
    (let ((records (mapcar get-record selected)))
      (loop for record in records do
            (format stream "~{~A~^;~}~%" record)))))

;;; Пункт 5. Конвертування записів у інший тип

(defun project-get-field ()
  "Повертає список пар ключ і функція отримання значення поля"
  (list (cons :id #'project-id)
        (cons :name #'project-name)
        (cons :model-id #'project-model-id)))

(defun model-get-field ()
  "Повертає список пар ключ і функція отримання значення поля"
  (list (cons :id #'ai-model-id)
        (cons :name #'ai-model-name)))

(defun convert-to-hash (record get-field)
  "Конвертує запис у геш-таблицю."
  (let ((hash (make-hash-table :test 'equal))
        (fields (funcall get-field)))
    (mapc (lambda (field)
            (setf (gethash (car field) hash) (funcall (cdr field) record)))
          fields)
    hash))

;;; Пункт 6. "Красивий" вивід записів таблиць

(defun pretty-print-records (data type)
  "Друкує список записів у читабельному форматі"
  (let ((records (case type
                   (:project (format t "~{~12A~}~%" '("ID" "NAME" "MODEL-ID"))
                             (mapcar #'project-get-record data))
                   (:ai-model (format t "~{~12A~}~%" '("ID" "NAME"))
                              (mapcar #'model-get-record data)))))
    (dolist (record records)
      (format t "~{~12A~}~%" record))))

;;; Тестування

(defun test-read-csv-file ()
  (format t "---Testing READ-CSV-FILE function---~%")
  
  (format t "~%--PROJECTS--~%")
  (let ((projects (read-csv-file "../lab5/projects.csv" #'create-project)))
    (format t "~A~%" projects))

  (format t "~%--AI-MODELS--~%")
  (let ((models (read-csv-file "../lab5/ai-models.csv" #'create-model)))
    (format t "~A~%" models))

  (format t "-------------------------------------------~%"))

(defun test-select ()
  (format t "~%---Testing SELECT function---~%")
  
  (format t "~%--PROJECTS: all records--~%")
  (let ((project-all (funcall (select "../lab5/projects.csv"
                                      :project))))
    (format t "~A~%" project-all))
  
  (format t "~%--PROJECTS: with filter MODEL-ID = 2--~%")
  (let ((project-sort (funcall (select "../lab5/projects.csv"
                                       :project
                                       #'project-model-id)
                               2)))
    (format t "~A~%" project-sort))

  (format t "~%--AI-MODELS: all records--~%")
  (let ((model-all (funcall (select "../lab5/ai-models.csv"
                                    :ai-model))))
    (format t "~A~%" model-all))
  
  (format t "~%--AI-MODELS: with filter NAME = \"Model B\"--~%")
  (let ((model-sort (funcall (select "../lab5/ai-models.csv"
                                     :ai-model
                                     #'ai-model-name)
                             "Model B")))
    (format t "~A~%" model-sort))

  (format t "-------------------------------------------~%"))

(defun test-write-csv-file ()
  (format t "~%---Testing WRITE-CSV-FILE function---~%")
  (format t "--Writing sorted Projects to \"sorted-projects.csv\"--~%")
  
  (let ((selected (funcall (select "../lab5/projects.csv"
                                   :project
                                   #'project-model-id)
                           2))
        (output-file "../lab5/sorted-projects.csv"))
    (write-csv-file output-file selected #'project-get-record)

    (format t "~%Contents of the result file:~%~A~%"
            (uiop:read-file-string output-file)))

  (format t "-------------------------------------------~%"))

(defun test-convert-to-hash ()
  (format t "~%---Testing CONVERT-TO-HASH function---~%")
  (format t "~%--AI-MODELS--~%")

  (let ((models (read-csv-file "../lab5/ai-models.csv" #'create-model)))
    (dolist (model models)
      (let ((hash (convert-to-hash model #'model-get-field)))
        (format t "~A~%Fields:~%" hash) 
        (maphash (lambda (key value)
                   (format t "-->~A: ~A~%" key value))
                 hash))))

  (format t "-------------------------------------------~%"))

(defun test-pretty-print-records ()
  (format t "~%---Testing PRETTY-PRINT-RECORDS function---~%")
  
  (format t "~%--PROJECTS--~%")
  (let ((projects (read-csv-file "../lab5/projects.csv" #'create-project)))
    (pretty-print-records projects :project))
  
  (format t "~%--AI-MODELS--~%")
  (let ((models (read-csv-file "../lab5/ai-models.csv" #'create-model)))
    (pretty-print-records models :ai-model))

  (format t "-------------------------------------------~%"))

(defun run-all-tests ()
  (test-read-csv-file)
  (test-select)
  (test-write-csv-file)
  (test-convert-to-hash)
  (test-pretty-print-records))
