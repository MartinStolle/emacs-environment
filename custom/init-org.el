;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:
(use-package org
  :straight (:type built-in)
  :custom
  ;; auto indent mode for org mode just works against me!
  (electric-indent-mode -1)
  :config
  (defun make-gsep-link (issue_id)
    (browse-url (concat "https://JIRA-URL/jira/browse/" issue_id)))

  (defun make-team-link (issue_id)
    (browse-url (concat "https://GITLAB-URL/-/issues/" issue_id)))

  (defun make-jira-link (issue_id)
    (browse-url (concat "https://JIRA-URL/browse/" issue_id)))

  (defun make-asam-link (issue_id)
    (browse-url (concat "https://GITLAB-URL/-/issues/" issue_id)))

  (org-add-link-type "gsep" 'make-gsep-link)
  (org-add-link-type "team" 'make-team-link)
  (org-add-link-type "jira" 'make-jira-link)
  (org-add-link-type "asam" 'make-asam-link)
  )

(provide 'init-org)

;;; init-org.el ends here
