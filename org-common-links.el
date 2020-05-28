;;; org-common-links.el

(defgroup org-common-link nil
  "Commonly defined org-links."
  :group 'convenience
  :prefix "org-common-link-")

;;; Wikipedia
(defcustom org-common-links-wikipedia-url "https://www.wikipedia.org/wiki/%s"
  "The wikipedia url to open topic."
  :group 'org-common-link
  :type '(string))

(defun org-common-links--open-wikipedia-link (link)
  "Open the given wikipedia org-link."
  (browse-url (format org-common-links-wikipedia-url link)))

(defun org-common-links--export-wikipedia-link (link description format)
  "Export the wikipedia LINK with DESCRIPTION for FORMAT from Org files."
  (let ((fullurl (format org-common-links-wikipedia-url link)))
    (cond ((eq format 'html)
           (format "<a href=\"%s\">%s</a>" fullurl description))
          ((eq format 'latex)
           (format "\\href{%s}{%s}" fullurl description))
          ((eq format 'ascii)
           (format "%s (%s)" fullurl description))
          (t fullurl))))

(org-link-set-parameters "wikipedia"
                         :follow #'org-common-links--open-wikipedia-link
                         :export #'org-common-links--export-wikipedia-link)
;;; MAN
(defcustom org-common-links-man-command 'man
  "The Emacs command to be used to display a man page."
  :group 'org-link
  :type '(choice (const man) (const woman)))

(defun org-common-links--open-man (path)
  "Visit the manpage on PATH.
PATH should be a topic that can be thrown at the man command."
  (funcall org-common-links-man-command path))

(defun org-common-links--man-get-page-name ()
  "Extract the page name from the buffer name."
  ;; This works for both `Man-mode' and `Woman-mode'.
  (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot create link to this man page.")))

(defun org-common-links--store-man-link ()
  "Store a link to a manpage."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, make this link
    (let* ((page (org-common-links--man-get-page-name))
           (link (concat "man:" page))
           (description (format "Manpage for %s" page)))
      (org-store-link-props
       :type "man"
       :link link
       :description description))))

(org-link-set-parameters "man"
                         :follow #'org-common-links--open-man
                         :store #'org-common-links--store-man-link)

;;; PEP
(defun org-common-links--open-pep (number)
  "Visit pep NUMBER on python.org"
  (let* ((base-string "https://www.python.org/dev/peps/pep-%04d/")
         (num (string-to-number number))
         (final-url (format base-string num)))
    (browse-url final-url)))

(defun org-common-links--export-pep (link description format)
  "Export the pep LINK with DESCRIPTION for FORMAT from Org files."
  (let* ((base-string "https://www.python.org/dev/peps/pep-%04d/")
         (num (string-to-number number))
         (fullurl (format base-string num)))
    (cond ((eq format 'html)
           (format "<a href=\"%s\">%s</a>" fullurl description))
          ((eq format 'latex)
           (format "\\href{%s}{%s}" fullurl description))
          ((eq format 'ascii)
           (format "%s (%s)" fullurl description))
          (t fullurl))))

(org-link-set-parameters "pep"
                         :follow #'org-common-links--open-pep
                         :export #'org-common-links--export-pep)

;;; GITHUB
(defun org-common-links--format-github-link (link)
  "Format PATH into a complete url."
  (let* ((base-url "https://github.com/%s")
         (issue-url "/issues/%s")
         (hashpos (string-match-p "#" link)))
    (if hashpos
        (concat (format base-url (substring link nil hashpos))
                (format issue-url (substring link (+ hashpos 1) nil))) ;; extract without '#'
      (format base-url link))))

(defun org-common-links--open-github-link (link)
  "Visit link at GitHub.com."
  (browse-url (org-common-links--format-github-link link)))

(defun org-common-links--export-github-link (link description format)
  "Export the github LINK with DESCRIPTION for FORMAT from Org files."
  (let* ((fullurl (org-common-links--format-github-link link))
         (desc (or description fullurl)))
    (cond ((eq format 'html)
           (format "<a href=\"%s\">%s</a>" fullurl desc))
          ((eq format 'latex)
           (format "\\href{%s}{%s}" fullurl desc))
          ((eq format 'ascii)
           (format "%s (%s)" fullurl desc))
          (t fullurl))))

(org-link-set-parameters "github"
                         :follow #'org-common-links--open-github-link
                         :export #'org-common-links--export-github-link)

(provide 'org-common-links)

;;; org-common-links.el ends here
