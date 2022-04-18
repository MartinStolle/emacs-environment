;; ORG-Mode
;; Custom Links to better manage the links to the various systems
(defun make-gsep-link (issue_id)
  (browse-url (concat "https://JIRA-URL/jira/browse/" issue_id)))

(defun make-team-link (issue_id)
  (browse-url (concat "https://GITLAB-URL/-/issues/" issue_id)))

(defun make-jira-link (issue_id)
  (browse-url (concat "https://JIRA-URL/browse/" issue_id)))

(defun make-asam-link (issue_id)
  (browse-url (concat "https://GITLAB-URL/-/issues/" issue_id)))