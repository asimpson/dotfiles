;;; ivy-feedwrangler.el --- An Ivy interface to the Feedwrangler RSS service

;;; Commentary:
;; This package requires that you manually retrieve your access token by using the curl command on this page: https://feedwrangler.net/developers/users

;;; Code:

(defun ivy-feedwrangler--parse-feed(feed)
  (mapcar (lambda (x) (cons (format "%s - %s" (alist-get 'feed_name x) (alist-get 'title x)) (list :url (alist-get 'url x) :id (alist-get 'feed_item_id x)))) feed))

(defun ivy-feedwrangler--get-feed()
  (let (token entry url)
    (setq entry (auth-source-search :host "feedwrangler.net" :max 1))
    (setq token (funcall (plist-get (nth 0 entry) :secret)))
    (setq url (concat "https://feedwrangler.net/api/v2/feed_items/list?access_token=" token "&read=false"))
    (with-temp-buffer
      (unless (zerop (call-process "curl" nil t nil "-s" url))
        (error "Failed: 'curl -s %s'" url))
      (let* ((json nil)
            (ret (ignore-errors
                    (setq json (json-read-from-string
                                (buffer-substring-no-properties
                                (point-min) (point-max))))
                    t)))
        (unless ret
          (error "Error: Can't get JSON response"))
        json))))

(defun ivy-feedwrangler()
  "get latest items from feedwrangler"
  (interactive)
  (with-temp-message "Loading feed..."
    (let (feed)
      (setq feed (ivy-feedwrangler--parse-feed (alist-get 'feed_items (ivy-feedwrangler--get-feed))))
      (ivy-read "Unread items: "
        feed
        :action (lambda (item) (browse-url (plist-get (cdr item) :url)))))))

(ivy-set-actions
  'ivy-feedwrangler
  '(("x" (lambda (item)
    (let (token entry url id)
      (setq entry (auth-source-search :host "feedwrangler.net" :max 1))
      (setq token (funcall (plist-get (nth 0 entry) :secret)))
      (setq id (number-to-string (plist-get (cdr item) :id)))
      (setq url (concat "https://feedwrangler.net/api/v2/feed_items/update?access_token=" token "&feed_item_id=" id "&read=true"))
      (with-temp-buffer (call-process "curl" nil t nil "-s" url)))) "Mark as Read")))

(provide 'ivy-feedwrangler)

;;; ivy-feedwrangler.el ends here
