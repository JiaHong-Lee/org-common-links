;;; org-common-links.el

;;; MAN
(org-add-link-type "man" 'org-man-open)
(add-hook 'org-store-link-functions 'org-man-store-link)

(defcustom org-man-command 'man
  "The Emacs command to be used to display a man page."
  :group 'org-link
  :type '(choice (const man) (const woman)))

(defun org-man-open (path)
  "Visit the manpage on PATH.
PATH should be a topic that can be thrown at the man command."
  (funcall org-man-command path))

(defun org-man-store-link ()
  "Store a link to a manpage."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, make this link
    (let* ((page (org-man-get-page-name))
           (link (concat "man:" page))
           (description (format "Manpage for %s" page)))
      (org-store-link-props
       :type "man"
       :link link
       :description description))))

(defun org-man-get-page-man ()
  "Extract the page name from the buffer name."
  ;; This works for both `Man-mode' and `Woman-mode'.
  (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot create link to this man page.")))

;;; PEP
(org-add-link-type "pep" 'org-pep-open)

(defun org-pep-open (number)
  "Visit pep NUMBER on python.org"
  (let* ((base-string "https://www.python.org/dev/peps/pep-%04d/")
         (num (string-to-number number))
         (final-url (format base-string num)))
    (browse-url final-url)))

;;; GITHUB
(org-add-link-type "github" 'org-github-open)

(defun org-github-open (REPO)
  "Visit REPO at GitHub.com."
  (let* ((base-url "https://github.com/%s")
         (issue-url "/issues/%s")
         (hashpos (string-match-p "#" REPO)))
    (if hashpos
        (browse-url
         (concat (format base-url (substring REPO nil hashpos))
                 (format issue-url (substring REPO (+ hashpos 1) nil)))) ;; extract without '#'
      (browse-url (format base-url REPO)))))

(provide 'org-common-links)

;;; org-common-links.el ends here
