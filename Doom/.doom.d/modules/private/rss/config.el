;;; private/rss/config.el -*- lexical-binding: t; -*-

(require 'seq)
(require 'cl-lib)

(defvar +rss/feed-urls '(("Arch Linux"            "https://www.archlinux.org/feeds/news/" Linux)
                         ("LEMMiNO"               "https://www.youtube.com/feeds/videos.xml?channel_id=UCRcgy6GzDeccI7dkbbBna3Q" YouTube)
                         ("Gamer from Mars"       "https://www.youtube.com/feeds/videos.xml?channel_id=UCJ6z_yj_dDNrhn-c8ZyKV4g" YouTube)
                         ("Pop Culture Detective" "https://www.youtube.com/feeds/videos.xml?channel_id=UCHiwtz2tCEfS17N9A-WoSSw" YouTube)
                         ("Dark Sominium"         "https://www.youtube.com/feeds/videos.xml?channel_id=UC_e39rWdkQqo5-LbiLiU10g" YouTube)
                         ("Nexpo"                 "https://www.youtube.com/feeds/videos.xml?channel_id=UCpFFItkfZz1qz5PpHpqzYBw" YouTube)
                         ("Techquickie"           "https://www.youtube.com/feeds/videos.xml?channel_id=UC0vBXGSyV14uvJ4hECDOl0Q" YouTube)
                         ("3B1B"                  "https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" YouTube)))


(when (featurep! +elfeed)
  (after! elfeed
    (setq elfeed-feeds (cl-map 'list (lambda (item) (append (list (nth 1 item)) (cdr (cdr item)))) +rss/feed-urls))))

(when (featurep! +newsticker)
  (defun +rss/set-feed-urls (LIST)
    "Set the newsticker-url-list to LIST. LIST should have format =(TAG URL START_TIME INTERVAL)="
    (setq newsticker-url-list LIST))

  (defun +rss/get-newsticker-buffers ()
    "Using seq, filter the buffer list for newsticker buffers"
    (seq-remove (lambda (buffer)
                  (not (and (cl-search "*Newsticker" (buffer-name buffer))
                            (= (cl-search "*Newsticker" (buffer-name buffer))))))
                (buffer-list)))

  (defun +rss/close-newsticker()
    "Routine to close the newsticker system"
    (interactive)
    (newsticker-stop)
    (dolist (buf (+rss/get-newsticker-buffers))
      (kill-buffer buf))
    (+workspace/delete "RSS"))

  (use-package! newsticker
    :config
    (+rss/set-feed-urls ; Format is =(TAG URL START_TIME INTERVAL)=
     (cl-map 'list (lambda (item) (list (nth 0 item) (nth 1 item) nil 3600)) +rss/feed-urls))

    (defun +rss/open-newsticker ()
      "Routine to start and open the newsticker"
      (interactive)
      (newsticker-start)
      (+workspace/new "RSS")
      (newsticker-treeview))))
