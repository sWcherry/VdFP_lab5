<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>

<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
з дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"><b>Студентка</b>: Панченко Вікторія Володимирівна КВ-12</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання

В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом. База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію `select` , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією функція і т. і. За потреби параметрів може бути кілька. `select` повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у `select` . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту):
    - структури у геш-таблиці
    - геш-таблиці у асоціативні списки
    - асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

## Варіант 4

|         База даних        | Тип записів |
|---------------------------|-------------|
|Проєкти із застосуванням ШІ| Структура   |

| Назва | Таблиці  | Опис|
|---|---|---|
| Проєкти із застосуванням ШІ | 1. Проєкти <br> 2. Моделі штучного інтелекту| База даних моделей штучного інтелекту та проєктів, в яких вони використовуються.

## Лістинг реалізації завдання

```lisp
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
```

### Тестові набори та утиліти

```lisp
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
```

### Тестування

```lisp
CL-USER> (run-all-tests)
---Testing READ-CSV-FILE function---

--PROJECTS--
(#S(PROJECT :ID 1 :NAME Project1 :MODEL-ID 2)
 #S(PROJECT :ID 2 :NAME Project2 :MODEL-ID 1)
 #S(PROJECT :ID 3 :NAME Project3 :MODEL-ID 2))

--AI-MODELS--
(#S(AI-MODEL :ID 1 :NAME Model A) #S(AI-MODEL :ID 2 :NAME Model B))
-------------------------------------------

---Testing SELECT function---

--PROJECTS: all records--
(#S(PROJECT :ID 1 :NAME Project1 :MODEL-ID 2)
 #S(PROJECT :ID 2 :NAME Project2 :MODEL-ID 1)
 #S(PROJECT :ID 3 :NAME Project3 :MODEL-ID 2))

--PROJECTS: with filter MODEL-ID = 2--
(#S(PROJECT :ID 1 :NAME Project1 :MODEL-ID 2)
 #S(PROJECT :ID 3 :NAME Project3 :MODEL-ID 2))

--AI-MODELS: all records--
(#S(AI-MODEL :ID 1 :NAME Model A) #S(AI-MODEL :ID 2 :NAME Model B))

--AI-MODELS: with filter NAME = "Model B"--
(#S(AI-MODEL :ID 2 :NAME Model B))
-------------------------------------------

---Testing WRITE-CSV-FILE function---
--Writing sorted Projects to "sorted-projects.csv"--

Contents of the result file:
1;Project1;2
3;Project3;2

-------------------------------------------

---Testing CONVERT-TO-HASH function---

--AI-MODELS--
#<HASH-TABLE :TEST EQUAL :COUNT 2 {100466CE13}>
Fields:
-->ID: 1
-->NAME: Model A
#<HASH-TABLE :TEST EQUAL :COUNT 2 {100466D683}>
Fields:
-->ID: 2
-->NAME: Model B
-------------------------------------------

---Testing PRETTY-PRINT-RECORDS function---

--PROJECTS--
ID          NAME        MODEL-ID    
1           Project1    2           
2           Project2    1           
3           Project3    2           

--AI-MODELS--
ID          NAME        
1           Model A     
2           Model B     
-------------------------------------------
NIL
```