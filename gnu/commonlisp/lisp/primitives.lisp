(defun car (x)
  (if (null x)
      nil
      (invoke (the pair x) '|getCar|)))
