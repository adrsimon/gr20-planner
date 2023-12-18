(load "parameters.cl")

(defparameter *BF* nil)

(defun removeRule (r)
	(if r
		(not (null (setq *BR* (remove r *BR*))))
		nil
	)	
)

(defun checkPremisse (p)
  (let ((element (cdr p))
        (op (car p))
        (current-value nil)
        (OK nil))

    (cond
      ((equal op 'or)
        (setq OK nil)
        (dolist (sous-premisse element OK)
          (setq OK (or (checkPremisse sous-premisse) OK))))

      ((equal op 'and)
        (setq OK T)
        (dolist (sous-premisse element OK)
          (setq OK (and (checkPremisse sous-premisse) OK))))

      ((equal op '>=)
        (setq current-value (cadr (assoc (car element) *BF*)))
        (if (>= current-value (cadr element))
          (progn
            (setq OK T))
          (setq OK nil)))
      
      ((equal op '<=)
        (setq current-value (cadr (assoc (car element) *BF*)))
        (if (<= current-value (cadr element))
          (progn
            (setq OK T))
          (setq OK nil)))

      ((equal op '>)
       (setq current-value (cadr (assoc (car element) *BF*)))
        (if (> current-value (cadr element))
          (progn
            (setq OK T))
          (setq OK nil)))
      
      ((equal op '<)
        (setq current-value (cadr (assoc (car element) *BF*)))
        (if (< current-value (cadr element))
          (progn
            (setq OK T))
          (setq OK nil)))

      (t
        (if (not (deepMember p *BF*))
          (setq OK nil)
          (progn 
            (setq OK T)
          )
          )))
  OK))

(defun add2BF (element)
	(if (listp element)
		(if (eq (length element) 2)
      (let ((existedeja nil))
        (progn
          (dolist (x *BF* nil)
            (if (equal (car x) (car element))
                (progn
                  (setq existedeja T)
                  (let ((pres nil))
                   (progn 
                    (dolist (y (cdr x))
                      (if (equal y (cadr element))
                          (setq pres T)))
                    (if (not pres)
                        (push (cadr element) (cdr (last x)))))))))
          (if (not existedeja)
              (push element *BF*))))
			(progn
				(format T "Erreur, l'élèment n'est pas de la forme (nom valeur) ~%")
				(print element)
			) 
		)
		(progn
			(format T "Erreur, l'élèment n'est pas une liste ~%")
			(print element)
		)
	) )

(defun deepMember (p bf)
  (let ((found nil))
    (dolist (x bf found)
      (if (equal (car x) (car p))
        (if (equal (cadr x) (cadr p))
          (setq found T)
          (progn
            (if (listp (cadr x))
              (setq found (deepMember p (cadr x)))
              (setq found nil))))))))

(defun declencheable (r)
	(let ((premisses (cadr r))
			  (OK T))
    (setq OK (checkPremisse premisses))
    (return-from declencheable OK)
	))

(defun candidate-rules ()
	(let
		((candidates nil))
		(dolist (r *BR* (reverse candidates))
      (if (declencheable (symbol-value r))
        (push r candidates)))
    (return-from candidate-rules candidates)))

(defun trigger-rule (r)
  (let ((conc (car (symbol-value r))))
    (if (declencheable (symbol-value r))
      (dolist (x conc)
        (add2BF x)))
    (removeRule r))
  (declencheable (symbol-value r))
)

(defun moteur ()
  (if *BF*
    (progn
      (let ((r (car (candidate-rules)))
            (target nil))
        (format T "~%Déroulement du raisonnement..... ~%")
        (loop while (candidate-rules) do
          (setq target (car (cadr (symbol-value r))))
          (trigger-rule r)
          (setq r (car (candidate-rules))))
        (if (> (length (assoc 'parcours *BF*)) 1)
          (return-from moteur (cdr (assoc 'parcours *BF*)))
          (return-from moteur (assoc 'parcours *BF*)))))))

(defun ask-niveau ()
   (let ((valids '(0 1 2 3))
         (niveau nil))
      (format t "~%Quel est votre niveau de randonnée ? (0/1/2/3)~%")
      (setq niveau (read))
      (if (member niveau valids)
         niveau
         (progn
            (format t "Niveau invalide, veuillez recommencer~%")
            (ask-niveau)
         )
      )
   ))

(defun ask-duree ()
  (let ((duree nil))
    (format t "~%Pour quelle durée souhaitez vous partir ? (Entre 1 et 16 jours)~%")
    (setq duree (read))
    (if (and (< duree 17) (> duree 0))
      duree
      (progn
        (format t "Durée invalide, veuillez recommencer~%")
        (ask-duree)
      ))))

(defun ask-paysages ()
  (let ((valids '(0 1 2 3))
       (paysage nil))
    (format t "~%Quels paysages souhaitez-vous voir ? (0/1/2/3)~%")
    (format t " - 0. Tous, peu m'importe~%")
    (format t " - 1. Points de vues exceptionnels~%")
    (format t " - 2. Lacs et rivières~%")
    (format t " - 3. Paysages dépaysants~%")
    (setq paysage (read))
    (if (member paysage valids)
      paysage
      (progn
        (format t "Paysage invalide, veuillez recommencer~%")
        (ask-paysages)
      )
    )
  ))

(defun ask-periode ()
  (let ((periode nil))
    (format t "~%Prévoyez vous de partir en hiver (de novembre à mai) ? (y/n)~%")
    (setq periode (read-line))
    (if (equal periode "y")
      T
      nil
    )))

(defun start-se ()
  (setq *BF* nil)
  (let ((niv (ask-niveau))
   (dur (ask-duree))
   (pay (ask-paysages))
   (per (ask-periode))
   (group nil))
  (add2bf (list 'condition-physique niv))
  (add2bf (list 'duree dur))
  (add2bf (list 'paysages pay))
  (add2bf (list 'hiver per))
  (setq group (moteur))
  (format t "Base de faits : ~S ~%" *BF*)
  (format t "Parcours éligibles : ~S ~%" group)
  (if (eq group nil)
    (format t "~%Aucun parcours ne correspond à vos critères, veuillez recommencer~%")
    (if (equal group (list 'parcours-0))
      (format t "~%Vous n'êtes pas assez entrainés pour le GR-20 ! Entraînez vous et réessayez.~%")
      (progn
        (dolist (parcours group)
          (format t "~%Voici la liste des étapes pour le ~S :~%" parcours)
          (affichageDetaille parcours)))))
  (format t "~%Au revoir !~%")))

(defun affichageDetaille (parcours)
  (let ((etapes-parcours (car (cdr (assoc parcours *etapes*)))))
    (dolist (i etapes-parcours)
      (format t "Etape ~A :" i)
      (let ((details (cdr (assoc i *etape*))))
        (format t " de ~A" (cadr (assoc 'trajet details)))
        (format t " en ~A km." (cadr (assoc 'distance details)))
        (format t " D+ : ~A m." (cadr (assoc 'denivele+ details)))
        (format t " D- : ~A m." (cadr (assoc 'denivele- details)))
        (format t " Durée estimée : ~A." (cadr (assoc 'duree details)))
        (format t " Paysages ~A~%" (cadr (assoc 'description details)))))))

(defun etape-in-group (etape group)
  (let ((parcours nil)
       (found nil))
    (setq parcours (getEtapes (car group)))
    (dolist (e (cadr parcours) found)
      (if (equal e etape)
        (progn
          (setq found T))
        ))
    (return-from etape-in-group found)))

(defun getEtapes (parcours)
  (let ((etapes nil))
    (setq etapes (assoc parcours *ETAPES*))
    (return-from getEtapes etapes)))

(defun launch ()
  (format t "Bienvenue dans le GR20-planner !~%")
  (format t "Des questions vont vous être posées~%afin de vous aider à planifier votre GR20.~%")
  (format t "Veuillez répondre avec honnêteté.~%")
  (format t "A la fin, vous obtiendrez un ensemble~%d'étapes personnalisé.~%")
  (format t "~%")
  (format t "C'est parti ? (y/n)~%")
  (let ((answer (read-line)))
    (format t "~%")
    (if (equal answer "y")
      (start-se)
      (format t "Une prochaine fois alors, Au revoir !~%")
    )
  ))

(defun main()
  (launch))

(main)
