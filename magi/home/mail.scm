(define-module (magi home mail)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu packages mail)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home-services mail)
  #:use-module (rde home services mail)
  #:use-module (gnu home-services version-control)
  #:use-module (rde features mail)
  #:re-export (mail-account
               mail-account-id
               mail-account-type
               mail-account-fqda
               mail-account-synchronizer
               mail-account-get-pass-cmd))

(define-public (mail-lst id fqda urls)
  (mailing-list
   (id id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define (l2md-services mail-directory mailing-lists)
  (define (get-repo-config ml)
    (let ((repo-config (mailing-list-config ml)))
      (if (eq? %unset-value (l2md-repo-maildir repo-config))
          (l2md-repo
           (inherit repo-config)
           (maildir (string-append
                     mail-directory "/lists/" (mailing-list-fqda ml) "/archive")))
          repo-config)))
  (define add-ml-tag
   (map (lambda (x)
          (format
           #f "notmuch tag +~a -- path:lists/~a/**"
           ;; TODO: Use new tag not to retag already existing entities.
           ;; Do it before new tag will be romved
           ;; TODO: Fix order of items in post-new hook
           (mailing-list-id x) (mailing-list-fqda x)))
        mailing-lists))
  (list
    (simple-service
     'l2md-add-tags-to-mailing-list
     home-notmuch-service-type
     (home-notmuch-extension
      (post-new
       (list
        #~(begin (for-each system '#$add-ml-tag))))))
    (service home-mcron-service-type
             (home-mcron-configuration
              (jobs (list #~(job '(next-hour)
                                  (lambda ()
                                     (system* "l2md")))))))
    (service
     home-l2md-service-type
     (home-l2md-configuration
      (oneshot 1)
      (repos (map get-repo-config mailing-lists))))))

(define disroot-folder-mapping
  '(("inbox" . "INBOX")
    ("sent" . "Sent")
    ("drafts" . "Drafts")
    ("trash" . "Trash")
    ("spam" . "Junk")))

(define disroot-isync-settings
  (generate-isync-serializer "disroot.org" disroot-folder-mapping))

(define isync-serializers (cons* `(disroot . ,disroot-isync-settings)
                                 %default-isync-serializers))

(define isync-global-settings
  `((Create Both)
    (Expunge Both)
    (SyncState *)
    ,#~""))

(define (isync-sync-acc mail-acc)
  (execl
   (string-append
    #$isync "/bin/mbsync "
    (symbol-string (mail-account-id mail-acc))
    "\n")))

(define (isync-synchronize mail-accounts)
  (program-file "isync-synchronize"
                #~(for-each
                   isync-sync-acc
                   mail-accounts)))

(define-public (isync-services mail-directory mail-accounts)
  (define (serialize-mail-acc mail-acc)
    ((assoc-ref isync-serializers (mail-account-type mail-acc))
     mail-directory mail-acc))
  (list
   (simple-service
    'isync-activation
    home-activation-service-type
    #~(map mkdir-p '#$ (map (lambda (acc)
                              (string-append mail-directory "/accounts/" (mail-account-fqda acc)))
                            mail-accounts)))
   (service
    home-isync-service-type
    (home-isync-configuration
     (config
      (append
       isync-global-settings
       (append-map serialize-mail-acc mail-accounts)))))))

;; (service home-mcron-service-type
;;          (home-mcron-configuration
;;           (jobs (list #~(job '(next-hour)
;;                              (lambda ()
;;                                (system* #$(isync-synchronize mail-accounts))))))))

(define msmtp-settings
  "defaults
tls on
auth on
logfile \"~/.local/var/log/msmtp.log\"\n")
(define msmtp-settings-tail
  "account default : disroot\n")

(define (msmtp-serializer provider-settings mail-account)
  (let* ((account-type (mail-account-type mail-account))
         (settings (assoc-ref provider-settings account-type)))
    (apply
     string-append
     (map (lambda (p)
            (format #f "~a ~a\n" (car p) (cdr p)))
          settings))))

(define msmtp-provider-settings
  (cons '(disroot . ((host . "disroot.org")
                     (port . 465)
                     (tls_starttls . off)))
        %default-msmtp-provider-settings))

(define-public (msmtp-services mail-directory mail-accounts)
  (list
   (simple-service 'msmtp-config
                   home-xdg-configuration-files-service-type
                   (list
                    (list
                     "msmtp/config"
                     (apply
                      mixed-text-file
                      "msmtp-config"
                      `(,msmtp-settings
                        ,@(map
                           (lambda (acc)
                             (string-append
                              "\n"
                              "account " (symbol->string (mail-account-id acc)) "\n"
                              "from " (mail-account-fqda acc) "\n"
                              "user " (mail-account-fqda acc) "\n"
                              "passwordeval " (mail-account-get-pass-cmd acc) "\n"
                              (msmtp-serializer msmtp-provider-settings acc)))
                           mail-accounts)
                        ,msmtp-settings-tail)))))
   (simple-service 'msmtp-set-git-send-email-cmd
                   home-git-service-type
                   (home-git-extension
                    (config
                     `((sendemail
                        ((sendmailcmd . ,(file-append msmtp "/bin/msmtp --read-envelope-from"))))))))
   (simple-service
    'msmpt-package
    home-profile-service-type
    (list msmtp))))

(define (get-notmuch-configuration mail-directory mail-accounts full-name)
  (let ((emails (map mail-account-fqda mail-accounts))
        (ids (map mail-account-id mail-accounts)))
    (define make-id-tag
      (map (lambda (x)
             (format
              #f "notmuch tag +~a -- path:accounts/~a/** and tag:new"
              (mail-account-id x) (mail-account-fqda x)))
           mail-accounts))

    (define tag-updates-post
      (append
       '("notmuch tag +inbox -- path:/accounts\\/.*\\/inbox/"
         "notmuch tag +draft -- path:/accounts\\/.*\\/drafts/"
         "notmuch tag +sent  -- path:/accounts\\/.*\\/sent/"
         "notmuch tag +trash -- path:/accounts\\/.*\\/trash/"
         "notmuch tag +spam  -- path:/accounts\\/.*\\/spam/"
         "notmuch tag +list  -- path:/lists\\/.*/"
         "notmuch tag +todo -inbox -sent  -- tag:inbox and tag:sent"
         ;; If file was moved out of folder on server remove respective tag
         "notmuch tag -inbox -- not path:/accounts\\/.*\\/inbox/ and tag:inbox"
         "notmuch tag -trash -- not path:/accounts\\/.*\\/trash/ and tag:trash"
         "notmuch tag -spam  -- not path:/accounts\\/.*\\/spam/  and tag:spam")
       '("notmuch tag -new -- tag:new")))

    (define (move-out-untagged-messages tag)
      "If tag was removed -> move out of the related folder."
      (format #f "for f in $(notmuch search --output=files \
'path:/.*\\/~a/ and not tag:~a' | grep '/~a/'); \
do mv -v $f \
$(echo $f | sed 's;/~a/;/archive/;' | sed 's/,U=[0-9]*:/:/'); done"
              tag tag tag tag))

    (define* (move-in-tagged-messages
              tag
              #:key (exclude-dir "nothing-will-match-this"))
      (format #f "for f in $(notmuch search --output=files \
'not path:/.*\\/~a/ and tag:~a' | grep -v \"/~a/\"); \
do mv -v $f \
$(echo $f | sed 's;/[[:alnum:]]*/cur/;/~a/cur/;' | sed 's/,U=[0-9]*:/:/'); done"
              tag tag exclude-dir tag))
    (define delete-deleted-messages
      "for f in $(notmuch search --output=files tag:deleted); do rm -v $f; done")

    (define move-rules
      (append
       (map move-out-untagged-messages '(inbox trash spam))
       (map move-in-tagged-messages '(trash spam))
       (list (move-in-tagged-messages 'inbox #:exclude-dir "archive")
             delete-deleted-messages)))
    (home-notmuch-extension
     (pre-new
      (list
       (with-imported-modules
        '((guix build utils))
        #~(begin
            #$(isync-synchronize mail-accounts)
            (for-each system '#$move-rules)))))
     (post-new
      (list
       #~(begin (for-each system '#$make-id-tag)
                (for-each system '#$tag-updates-post))))
     (config
      `((user ((name . ,full-name)
               (primary_email . ,(car emails))
               (other_email . ,(cdr emails))))
        (database ((path . ,mail-directory)
                   (mail_root . ,mail-directory)))
        (maildir ((synchronize_flags . true)))
        (search ((exclude_tags . (trash spam deleted))))
        (new ((tags . new)
              (ignore . (.mbsyncstate .uidvalidity
                                      .mbsyncstate.new .mbsyncstate.journal)))))))))

(define (notmuch-redefined-functions config)
  ;; Remove leading arrows for mails without threads
  ;; Make the width used by notmuch-jump prompt to be 80%
  `((defun rde-notmuch-tree-insert-tree (tree depth tree-status first last)
      "Insert the message tree TREE at depth DEPTH in the current thread.

A message tree is another name for a single sub-thread: i.e., a
message together with all its descendents."
      (let ((msg (car tree))
            (replies (cadr tree)))
        (cond
         ((and (< 0 depth) (not last))
          (push "├" tree-status))
         ((and (< 0 depth) last)
          (push "└" tree-status))
         ((and (eq 0 depth) first last)
          ;; Choice between these two variants is a matter of taste.
          ;; (push "─" tree-status))
          (push " " tree-status))
         ((and (eq 0 depth) first (not last))
          (push "┬" tree-status))
         ((and (eq 0 depth) (not first) last)
          (push "└" tree-status))
         ((and (eq 0 depth) (not first) (not last))
          (push "├" tree-status)))
        (unless (eq 0 depth)
          (push (concat (if replies "┬" "─") ">") tree-status))
        (setq msg (plist-put msg :first (and first (eq 0 depth))))
        (setq msg (plist-put msg :tree-status tree-status))
        (setq msg (plist-put msg :orig-tags (plist-get msg :tags)))
        (notmuch-tree-goto-and-insert-msg msg)
        (pop tree-status)
        (pop tree-status)
        (if last
            (push " " tree-status)
            (push "│" tree-status))
        (notmuch-tree-insert-thread replies (+ 1 depth) tree-status)))
    (advice-add 'notmuch-tree-insert-tree :override
                'rde-notmuch-tree-insert-tree)

    (defun rde-notmuch-jump (action-map prompt)
      "Interactively prompt for one of the keys in ACTION-MAP.

Displays a summary of all bindings in ACTION-MAP in the
minibuffer, reads a key from the minibuffer, and performs the
corresponding action.  The prompt can be canceled with C-g or
RET.  PROMPT must be a string to use for the prompt.  PROMPT
should include a space at the end.

ACTION-MAP must be a list of triples of the form
  (KEY LABEL ACTION)
where KEY is a key binding, LABEL is a string label to display in
the buffer, and ACTION is a nullary function to call.  LABEL may
be null, in which case the action will still be bound, but will
not appear in the pop-up buffer."
      (let* ((items (notmuch-jump--format-actions action-map))
             ;; Format the table of bindings and the full prompt
             (table
              (with-temp-buffer
               (notmuch-jump--insert-items
                (floor (* (frame-width) 0.8)) items)
               (buffer-string)))
             (full-prompt
              (concat table "\n\n"
                      (propertize prompt 'face 'minibuffer-prompt)))
             ;; By default, the minibuffer applies the minibuffer face to
             ;; the entire prompt.  However, we want to clearly
             ;; distinguish bindings (which we put in the prompt face
             ;; ourselves) from their labels, so disable the minibuffer's
             ;; own re-face-ing.
             (minibuffer-prompt-properties
              (notmuch-plist-delete
               (copy-sequence minibuffer-prompt-properties)
               'face))
             ;; Build the keymap with our bindings
             (minibuffer-map (notmuch-jump--make-keymap action-map prompt))
             ;; The bindings save the the action in notmuch-jump--action
             (notmuch-jump--action nil))
        ;; Read the action
        (read-from-minibuffer full-prompt nil minibuffer-map)
        ;; If we got an action, do it
        (when notmuch-jump--action
          (funcall notmuch-jump--action))))
    (advice-add 'notmuch-jump :override 'rde-notmuch-jump)))

(define notmuch-saved-searches
  '((:name "TODO" :query "tag:todo" :key "t")
    (:name "Inbox" :query "tag:inbox" :key "i")
    (:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "w")
    (:name "Drafts" :query "tag:draft" :key "d")
    (:name "Flagged" :query "tag:flagged" :key "f")
    (:name "Sent" :query "tag:sent" :key "s")
    (:name "All mail" :query "*" :key "a")))

(define-public (notmuch-services mail-directory mail-accounts full-name)
  (list
   (simple-service
    'notmuch-service
    home-notmuch-service-type
    (get-notmuch-configuration mail-directory mail-accounts full-name))))

(define-public (mail-services mail-directory mail-accounts full-name mailing-lists)
  `(,@(isync-services mail-directory mail-accounts)
    ,@(msmtp-services mail-directory mail-accounts)
    ,@(notmuch-services mail-directory mail-accounts full-name)
    ,@(l2md-services mail-directory mailing-lists)))
    
