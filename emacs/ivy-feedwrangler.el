;;; ivy-feedwrangler.el --- An Ivy interface to the Feedwrangler RSS service
;; -*- lexical-binding: t; -*-

;; Adam Simpson <adam@adamsimpson.net>
;; Version: 0.0.1
;; Package-Requires: (ivy)
;; Keywords: rss, curl, ivy
;; URL: https://github.com/asimpson/dotfiles/

;;; Commentary:
;; This package requires that you manually retrieve your access token by using the curl command on this page: https://feedwrangler.net/developers/users

;;; Code:

(defvar ivy-feedwrangler--base-url
  "https://feedwrangler.net/api/v2/feed_items/"
  "The base URL for the API."
)

(defun ivy-feedwrangler--parse-feed(feed)
  "Returns feed items in format: 'Site Title - Post title' format."
  (mapcar (lambda (x)
    (cons (format "%s - %s" (alist-get 'feed_name x) (alist-get 'title x))
      (list :url (alist-get 'url x) :id (alist-get 'feed_item_id x)))) feed))


(defun ivy-feedwrangler--get-token()
  "Returns the feedrwrangler token from auth-source."
  (let (token entry)
    (setq entry (auth-source-search :host "feedwrangler.net" :max 1))
    (setq token (funcall (plist-get (car entry) :secret)))))

(defun ivy-feedwrangler--get-feed()
  "Make http request for feed items and parse JSON response"
  (let ((token (ivy-feedwrangler--get-token)) url)
    (setq url (concat ivy-feedwrangler--base-url "list?access_token=" token "&read=false"))
    (with-temp-buffer
      (unless (zerop (call-process "curl" nil t nil "-s" url))
        (error "Failed: 'curl -s %s'" url))
      (let* ((json nil)
            (ret (ignore-errors
                    (setq json (json-read-from-string
                                (buffer-substring-no-properties (point-min) (point-max))))
                    t)))
        (unless ret
          (error "Error: Can't get JSON response"))
        json))))

(defun ivy-feedwrangler()
  "Get latest items from feedwrangler."
  (interactive)
  (message "Loading feed...")
  (let (feed)
    (setq feed (ivy-feedwrangler--parse-feed (alist-get 'feed_items (ivy-feedwrangler--get-feed))))
    (if (equal (length feed) 0)
        (message "No new unread items")
      (ivy-read "Unread items: "
        feed
        :action (lambda (item)
          (let ((url (plist-get (cdr item) :url)))
          (if (memq system-type '(darwin))
            (start-process (concat "ivy-feedwrangler-" url) nil "open" url "-g")
          (browse-url url)))))
  )))

(ivy-set-actions
  'ivy-feedwrangler
  '(("x" (lambda (item)
    (let ((token (ivy-feedwrangler--get-token)) url id)
      (setq id (number-to-string (plist-get (cdr item) :id)))
      (setq url (concat ivy-feedwrangler--base-url "update?access_token=" token "&feed_item_id=" id "&read=true"))
      (with-temp-buffer (call-process "curl" nil t nil "-s" url)))) "Mark as Read")
    ("X" (lambda (item)
        (let ((token (ivy-feedwrangler--get-token)) url)
          (setq url (concat ivy-feedwrangler--base-url "mark_all_read?access_token=" token))
          (with-temp-buffer (call-process "curl" nil t nil "-s" url)))) "Mark all as Read")))

(provide 'ivy-feedwrangler)

;;; ivy-feedwrangler.el ends here
