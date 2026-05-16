(defun calculate (x y)
  (if (ready-p x)
      (save-result
       (let ((total (+ x y))
             (label (format-label total)))
         (format label total)))))
