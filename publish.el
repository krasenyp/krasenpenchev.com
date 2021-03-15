(require 'ox-publish)
(require 'ox-html)

(defun read-file-contents (path)
  "Returns the contents of the file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun kp/org-publish-find-explicit-date (file project)
  "Find the date of FILE in PROJECT.
This function assumes FILE is either a directory or an Org file.
If FILE is an Org file and provides a DATE keyword use it.  In
any other case return nil.  Return
time in `current-time' format."
  (let ((file (org-publish--expand-file-name file project)))
    (if (file-directory-p file) (nth 5 (file-attributes file))
      (let ((date (org-publish-find-property file :date project)))
	;; DATE is a secondary string.  If it contains a time-stamp,
	;; convert it to internal format.
	(cond ((let ((ts (and (consp date) (assq 'timestamp date))))
		 (and ts
		      (let ((value (org-element-interpret-data ts)))
			(and (org-string-nw-p value)
			     (org-time-string-to-time value)))))))))))

(defun kp/sitemap-format-entry (entry style project)
  "Format ENTRY in a PROJECT according to STYLE."
  (let ((date (kp/org-publish-find-explicit-date entry project)))
    `(:content ,(format "<article>%s %s</article>"
                        (if date
                            (format-time-string "%d/%m/%Y" date)
                          "&nbsp;")
                        (entry-to-link-object entry project))
      :entry ,entry)))

(defun entry-to-link-object (entry project)
  "Create a link to an ENTRY of PROJECT."
  (let ((path (format "posts/%s" entry))
        (raw-link (format "file:posts/%s" entry))
        (description (org-publish-find-title entry project)))
    (org-html-link `(link (:type "file" :path ,path :raw-link ,raw-link)) description '(:html-link-org-files-as-html t :html-extension "html"))))

(defun kp/all-entries (list)
  (concat "<ul class=\"posts-list\">"
          (mapconcat (lambda (entry)
                       (format "<li class=\"posts-list-item\">%s</li>" (plist-get (car entry) :content)))
                     (cdr list)
                     "\n")
          "</ul>"))


(defun kp/sitemap (title list)
  (format "#+OPTIONS: title:nil\n
                  #+BEGIN_EXPORT html\n
                  <div class=\"posts-list\">
                  %s
                  </div>
                  #+END_EXPORT"
          (kp/all-entries list)))

(defconst html-common `(:html-doctype "html5"
                        :html-html5-fancy t
                        :html-head-include-scripts nil
                        :html-head-include-default-style nil
                        :html-head "<link rel=\"stylesheet\" href=\"/static/styles/base.css\" type=\"text/css\"/>"
                        :html-preamble ,(read-file-contents "./src/common/preamble.html")
                        :html-postamble nil))

(defconst author-common '(:email "hello@krasenpenchev.com"
                          :author "Красен Пенчев"))

(setq org-publish-project-alist
      `(("pages"
         :base-directory "src/"
         :base-extension "org"
         :recursive nil
         :publishing-directory "public/"
         :publishing-function org-html-publish-to-html
         ,@html-common
         ,@author-common)
        ("posts"
         :base-directory "src/posts/"
         :base-extension "org"
         :recursive t
         :publishing-directory "public/posts/"
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :sitemap-title "Публикации"
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry kp/sitemap-format-entry
         :sitemap-function kp/sitemap
         :sitemap-style list
         :table-of-contents nil
         :with-toc nil
         ,@html-common
         ,@author-common)
        ("static"
         :base-directory "static/"
         :base-extension "css\\|txt\\|jpg\\|gif\\|png"
         :recursive t
         :publishing-directory "public/static/"
         :publishing-function org-publish-attachment)
        ("all" :components ("pages", "posts"))))
