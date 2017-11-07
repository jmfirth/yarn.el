;;; yarn.el --- Create and manage Node.JS packages with Yarn inside Emacs.

;; Author: Justin Firth
;; Keywords: languages, javascript, convenience
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; yarn.el makes it convenient to work Yarn in NodeJS projects inside Emacs.

;; With this you can:

;; - initialize a new Node.js package
;; - add and remove dependencies (project, dev, peer)
;; - view outdated dependencies and update them
;; - execute run scripts
;; - link and unlink packages
;; - version and publish your package

;;; Credits:

;; npm.el, which this package was originally forked from.

;;; Code:


(eval-when-compile
  (require 'cl))
(require 'compile)
(require 'json)

(defgroup yarn nil
  "Run yarn in Emacs"
  :group 'tools)

(defcustom yarn-executable-path nil
  "The path to the node and yarn executables.

NIL if they should be looked up from the global path"
  :type 'string
  :group 'yarn)

(defcustom yarn-display-buffer nil
  "Pop to yarn buffer when the process finishes."
  :type 'boolean
  :group 'yarn)

(defcustom yarn-finished-hook '()
  "Hooks run when the current yarn process finishes."
  :type 'hook
  :group 'yarn)

(defvar yarn-vars-name "hello-world")
(defvar yarn-vars-desc "")
(defvar yarn-vars-author (user-login-name))
(defvar yarn-vars-git-user (user-login-name))
(defvar yarn-vars-test-cmd "node test")
(defvar yarn-vars-license "BSD")
(defvar yarn-vars-main "index.js")
(defvar yarn-vars-add-dep "")
(defvar yarn-vars-link-dep "")
(defvar yarn-vars-deps "")
(defvar yarn-vars-keywords "")
(defvar yarn-vars-last-search-keyword "")
(defvar yarn-vars-version "0.0.0")
(defvar yarn-vars-last-script-name "")
(defvar yarn-vars-filename "")

(defun yarn-exec-with-path (callback &rest args)
  "Execute CALLBACK with the path set to YARN_EXECUTABLE_PATH."
  (let ((exec-path (if yarn-executable-path
                     (cons yarn-executable-path)
                     exec-path))
        (compilation-environment (if yarn-executable-path
                                   (cons (concat "PATH=" yarn-executable-path path-separator (getenv "PATH")) compilation-environment)
                                   compilation-environment)))
    (apply callback args)))

(defun yarn-git ()
  (concat "git@github.com:" yarn-vars-git-user "/" yarn-vars-name ".git"))

(defun yarn-start-process (name buffer program &rest program-args)
  "Wraps `start-process' so that we can override it."
  (when (get-buffer buffer)
    (kill-buffer buffer))
  (apply 'start-process name buffer program program-args)
  (with-current-buffer buffer
    (while (process-live-p (get-process name))
      (sit-for 0.5))
    (run-hooks 'yarn-finished-hook))
  (if yarn-display-buffer (pop-to-buffer buffer)))

(setq json-encoding-pretty-print t)

(defun flatten-list (list) (apply #'nconc list))

(defun is-dev-dependency (dp) (plist-get dp :dev))

(defun make-keyword (symbol) (intern (format ":%s" symbol)))

(defun yarn-package-json (name desc version main test-cmd keywords deps git author license)
  (let (dev-deps (content '()))
    (setq dev-deps (plist-get deps :dev))
    (setq deps (plist-get deps :deps))

    (setq content `(:license ,license))
    (plist-put content :author author)
    (plist-put content :repository `(:type "git" :url ,git))

    (if (> (length keywords) 0) (plist-put content :keywords keywords))
    (if (> (length dev-deps) 0) (plist-put content :devDependencies dev-deps))
    (if (> (length deps) 0) (plist-put content :dependencies deps))

    (plist-put content :scripts `(:test ,test-cmd))
    (plist-put content :main main)
    (plist-put content :description desc)
    (plist-put content :version version)
    (plist-put content :name name)

    (json-encode content)))

(defun yarn-format-dependency (dp)
  (let (name ver)
    (setq name (make-keyword (plist-get dp :name)))
    (setq ver (plist-get dp :ver))
    (message "ver: %S" ver)
    `(,name ,ver)))

(defun yarn-install ()
  "Install all dependencies"
  (interactive)
  (message "Installing dependencies...  (Check *yarn* for the output)")
  (yarn-exec-with-path 'yarn-start-process "yarn-install" "*yarn*" "yarn" "install"))

(defun yarn-init (path)
  "Initialize a new Node.js project"

  ;; get target directory
  (interactive "DDirectory:")
  (message "Creating project at %s" path)

  ;; get package.json info
  (interactive)
  (setq yarn-vars-name (read-from-minibuffer "Project Name: " yarn-vars-name))
  (setq yarn-vars-desc  (read-from-minibuffer "Description: " yarn-vars-desc))
  (setq yarn-vars-keywords (read-from-minibuffer "Keywords (http, parsing, etc): " yarn-vars-keywords))
  (setq yarn-vars-git (read-from-minibuffer "Git: " (yarn-git)))

  ;; scaffold project
  (let (packagejson bf project-path manifest-filename readme)
    (setq packagejson (yarn-package-json
                       yarn-vars-name
                       yarn-vars-desc
                       yarn-vars-version
                       yarn-vars-main
                       yarn-vars-test-cmd
                       (yarn-parse-keywords yarn-vars-keywords)
                       (yarn-parse-deps yarn-vars-deps)
                       yarn-vars-git
                       yarn-vars-author
                       yarn-vars-license))

    (setq readme (concat "## " yarn-vars-name "\n\n"
                         yarn-vars-desc "\n\n"
                         "## Install\n\n```bash\n$ yarn install " yarn-vars-name "\n```\n\n"
                         "## Usage\n\n ```js\n```\n\n"))

    (setq manifest-filename (concat path "/package.json"))

    (message "Creating the new directory and files...")

    (make-directory path)
    (setq bf (get-buffer-create manifest-filename))
    (switch-to-buffer bf)
    (js2-mode)
    (insert packagejson)
    (json-pretty-print-buffer)
    (write-file manifest-filename)
    (shell-command-to-string (concat "git init && git remote add origin " yarn-vars-git))
    (shell-command-to-string "echo 'node_modules\nyarn-debug.log\n.DS_Store' > .gitignore")
    (shell-command-to-string "echo 'test\ntest.js\nexample\nexamples' > .npmignore")
    (shell-command-to-string (concat "echo '" readme "' > README.md"))
    (setq bf (get-buffer-create manifest-filename))
    (yarn-install)))

(defun yarn-remove ()
  "Remove a dependency"
  (interactive)
  (let (package)
    (setq package (read-from-minibuffer "Remove dependency (e.g: minimist): " package))
    (message (concat "Remove dependency: " package))
    (yarn-exec-with-path 'yarn-start-process "yarn-remove" "*yarn*" "yarn" "remove" package)))

(defun yarn-add ()
  "Add dependency"
  (interactive)
  (setq yarn-vars-add-dep (read-from-minibuffer "Add dependency (e.g: minimist): " yarn-vars-add-dep))
  (message (concat "Adding dependency: " yarn-vars-add-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-add" "*yarn*" "yarn" "add" yarn-vars-add-dep))

(defun yarn-add-dev ()
  "Add development dependency"
  (interactive)
  (setq yarn-vars-add-dep (read-from-minibuffer "Add dev dependency (e.g: tape): " yarn-vars-add-dep))
  (message (concat "Adding dev dependency: " yarn-vars-add-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-add-dev" "*yarn*" "yarn" "add" yarn-vars-add-dep "-dev"))

(defun yarn-add-peer ()
  "Add peer dependency"
  (interactive)
  (setq yarn-vars-add-dep (read-from-minibuffer "Add peer dependency (e.g: react-dom): " yarn-vars-add-dep))
  (message (concat "Adding peer dependency: " yarn-vars-add-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-add-peer" "*yarn*" "yarn" "add" yarn-vars-add-dep "-peer"))

(defun yarn-add-optional ()
  "Add peer dependency"
  (interactive)
  (setq yarn-vars-add-dep (read-from-minibuffer "Add optional dependency (e.g: fetch): " yarn-vars-add-dep))
  (message (concat "Adding optional dependency: " yarn-vars-add-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-add-optional" "*yarn*" "yarn" "add" yarn-vars-add-dep "-optional"))

(defun yarn-add-exact ()
  "Add exact dependency"
  (interactive)
  (setq yarn-vars-add-dep (read-from-minibuffer "Add exact dependency (e.g: react-router): " yarn-vars-add-dep))
  (message (concat "Adding exact dependency " yarn-vars-add-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-add-exact" "*yarn*" "yarn" "add" yarn-vars-add-dep "-exact"))

(defun yarn-add-tilde ()
  "Add exact dependency"
  (interactive)
  (setq yarn-vars-add-dep (read-from-minibuffer "Add tilde dependency (e.g: react): " yarn-vars-add-dep))
  (message (concat "Adding tilde dependency " yarn-vars-add-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-add-tilde" "*yarn*" "yarn" "global" "add" yarn-vars-add-dep "-tilde"))

(defun yarn-global-add ()
  "Add dependency globally"
  (interactive)
  (setq yarn-vars-add-dep (read-from-minibuffer "Add global dependency (e.g: minimist): " yarn-vars-add-dep))
  (message (concat "Adding dependency: " yarn-vars-add-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-global-add" "*yarn*" "yarn" "global" "add" yarn-vars-add-dep))

(defun yarn-global-add-exact ()
  "Add exact dependency globally"
  (interactive)
  (setq yarn-vars-add-dep (read-from-minibuffer "Add global exact dependency (e.g: react-router): " yarn-vars-add-dep))
  (message (concat "Adding exact dependency " yarn-vars-add-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-global-add-exact" "*yarn*" "yarn" "global" "add" yarn-vars-add-dep "-exact"))

(defun yarn-global-add-tilde ()
  "Add exact dependency globally"
  (interactive)
  (setq yarn-vars-add-dep (read-from-minibuffer "Add global tilde dependency (e.g: react): " yarn-vars-add-dep))
  (message (concat "Adding tilde dependency " yarn-vars-add-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-global-add-tilde" "*yarn*" "yarn" "global" "add" yarn-vars-add-dep "-tilde"))

(defun yarn-parse-dependency (input)
  (let (name ver dev)
    (setq input (split-string input " "))
    (setq name (nth 0 input))
    (setq ver (nth 1 input))
    (setq dev (nth 2 input))
    `(:name ,name :ver ,ver :dev ,(not (not dev)))))

(defun yarn-parse-deps (input)
  (let (deps dev-deps)
    (setq deps (remove-if 'string-empty-p (split-string input ", ")))
    (setq deps (mapcar 'yarn-parse-dependency deps))

    (setq dev-deps (remove-if-not 'is-dev-dependency deps))
    (setq deps (remove-if 'is-dev-dependency deps))

    (setq deps (flatten-list (mapcar 'yarn-format-dependency deps)))
    (setq dev-deps (flatten-list (mapcar 'yarn-format-dependency dev-deps)))
    `(:dev ,dev-deps :deps ,deps)))

(defun yarn-parse-keywords (input)
  (remove-if 'string-empty-p (split-string input ", ")))

(defun yarn-check ()
  "Verify installed package versions"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-check" "*yarn*" "yarn" "check"))

(defun yarn-check-integrity ()
  "Verify installed package versions and their checksums"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-check-integrity" "*yarn*" "yarn" "check" "--integrity"))

(defun yarn-upgrade ()
  "Upgrade packages with Yarn"
  (interactive)
  (message "Upgrading project packages to their latest versions... (Check *yarn* for the output)")
  (yarn-exec-with-path 'yarn-start-process "yarn-upgrade" "*yarn*" "yarn" "upgrade"))

(defun yarn-outdated ()
  "Check for outdated packages with Yarn"
  (interactive)
  (message "Checking for outdated packages in project... (Check *yarn* for the output)")
  (yarn-exec-with-path 'yarn-start-process "yarn-outdated" "*yarn*" "yarn" "outdated"))

(defun yarn-clean ()
  "Clean free space by removing unnecessary packages"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-clean" "*yarn*" "yarn" "clean"))

(defun yarn-bin ()
  "Display installed executable directory"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-bin" "*yarn*" "yarn" "bin"))

(defun yarn-cache-ls ()
  "Display all cached packages"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-cache-ls" "*yarn*" "yarn" "cache" "ls"))

(defun yarn-cache-dir ()
  "Display global cache directory location"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-cache-dir" "*yarn*" "yarn" "cache" "dir"))

(defun yarn-cache-clean ()
  "Clear the local cache"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-cache-clean" "*yarn*" "yarn" "cache" "clean"))

(defun yarn-config-set ()
  "Set a local configuration value"
  (interactive)
  (let (key value)
    (setq key (read-from-minibuffer "Set which local key? (e.g: user.pass):"))
    (setq value (read-from-minibuffer "Value for local key? (e.g: bunnies):"))
    (message (concat "Setting local repository config value: " key))
    (yarn-exec-with-path 'yarn-start-process "yarn-config-set" "*yarn*" "yarn" "config" "set" key value)))

(defun yarn-config-get ()
  "Get the effective local configuration value"
  (interactive)
  (let (key)
    (setq key (read-from-minibuffer "Get which local key value? (e.g: user.pass):"))
    (message (concat "Getting local repository config value: " key))
    (yarn-exec-with-path 'yarn-start-process "yarn-config-get" "*yarn*" "yarn" "config" "get" key)))

(defun yarn-global-config-set ()
  "Set a global configuration value"
  (interactive)
  (let (key value)
    (setq key (read-from-minibuffer "Set which key globally? (e.g: user.pass):"))
    (setq value (read-from-minibuffer "Value for global key? (e.g: bunnies):"))
    (message (concat "Setting global repository config value: " key))
    (yarn-exec-with-path 'yarn-start-process "yarn-global-config-set" "*yarn*" "yarn" "config" "set" key value "--global")))

(defun yarn-config-delete ()
  "Delete local configuration value"
  (interactive)
  (let (key)
    (setq key (read-from-minibuffer "Delete which key? (e.g: user.pass):"))
    (message (concat "Deleting local repository config value: " key))
    (yarn-exec-with-path 'yarn-start-process "yarn-config-delete" "*yarn*" "yarn" "config" "delete" key)))

(defun yarn-config-list ()
  "Display current configuration"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-config-list" "*yarn*" "yarn" "config" "list"))

(defun yarn-ls ()
  "List installed packages"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-ls" "*yarn*" "yarn" "ls"))

(defun yarn-licenses-ls ()
  "List installed packages and their licenses"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-licenses-ls" "*yarn*" "yarn" "licenses" "ls"))

(defun yarn-licenses-generate-disclaimer ()
  "Generate a sorted list of licenses to *yarn-disclaimer*"
  (interactive)
  (yarn-exec-with-path 'yarn-start-process "yarn-licenses-generate-disclaimer" "*yarn-disclaimer*" "yarn" "licenses" "generate-disclaimer"))

(defun yarn-why ()
  "Information about why a package has been installed"
  (interactive)
  (let (why)
    (setq why (read-from-minibuffer "Which module do you want to know about? (e.g: jquery):"))
    (message (concat "Gathering information about why " why " was installed... (Check *yarn* for the output)"))
    (yarn-exec-with-path 'yarn-start-process "yarn-why" "*yarn*" "yarn" "why" why)))

(defun yarn-test ()
  "Run test script"
  (interactive)
  (yarn-run "test"))

(defun yarn-link ()
  "Make package linkable"
  (interactive)
  (message (concat "Making project linkable... (Check *yarn* for the output)"))
  (yarn-exec-with-path 'yarn-start-process "yarn-link" "*yarn*" "yarn" "link"))

(defun yarn-link-package ()
  "Link external package to current project"
  (interactive)
  (setq yarn-vars-link-dep (read-from-minibuffer "Link package (e.g: next): " yarn-vars-link-dep))
  (message (concat "Linking package " yarn-vars-link-dep))
  (yarn-exec-with-path 'yarn-start-process "yarn-link-package" "*yarn*" "yarn" "link" yarn-vars-link-dep))

(defun yarn-unlink ()
  "Unlink a linked package"
  (interactive)
  (message (concat "Unlinking package... (Check *yarn* for the output)"))
  (yarn-exec-with-path 'yarn-start-process "yarn-unlink" "*yarn*" "yarn" "unlink"))

(defun yarn-pack ()
  "Create a compressed gzip archive of package"
  (interactive)
  (message "Packing package")
  (yarn-run "pack"))

(defun yarn-pack-filename ()
  "Create a compressed gzip archive of package to filename"
  (interactive)
  (setq yarn-vars-filename (read-from-minibuffer "Packed file name (e.g: bundle.js): " yarn-vars-filename))
  (message (concat "Packing package to " yarn-vars-filename))
  (yarn-exec-with-path 'yarn-start-process "yarn-pack-filename" "*yarn*" "yarn" "pack" "--filename" yarn-vars-link-dep))

(defun yarn-login ()
  "Login to the npm registry and cache username for later"
  (interactive)
  (yarn-run "login"))

(defun yarn-logout ()
  "Logout of the npm registry and remove cached username"
  (interactive)
  (yarn-run "logout"))

(defun yarn-owner-ls ()
  "List all the owners of a package"
  (interactive)
  (let (package)
    (setq package (read-from-minibuffer "List all the owners of which package? (e.g: react):"))
    (message (concat "Listing all the owners of " package " (Check *yarn* for the output)"))
    (yarn-exec-with-path 'yarn-start-process "yarn-owner-ls" "*yarn*" "yarn" "owner" "ls" package)))

(defun yarn-owner-add ()
  "Add an owner to a package"
  (interactive)
  (let (user package)
    (setq user (read-from-minibuffer "Add which user as an owner? (e.g: jdoe):"))
    (setq package (read-from-minibuffer "To which package? (e.g: yarn.el):"))
    (message (concat "Adding " user " as an owner of " package " (Check *yarn* for the output)"))
    (yarn-exec-with-path 'yarn-start-process "yarn-owner-add" "*yarn*" "yarn" "owner" "ls" user package)))

(defun yarn-owner-rm ()
  "Remove an owner from a package"
  (interactive)
  (let (user package)
    (setq user (read-from-minibuffer "Remove which owner? (e.g: jmfirth):"))
    (setq package (read-from-minibuffer "From which package? (e.g: yarn.el):"))
    (message (concat "Adding " user " as an owner of " package " (Check *yarn* for the output)"))
    (yarn-exec-with-path 'yarn-start-process "yarn-owner-rm" "*yarn*" "yarn" "owner" "rm" user package)))

(defun yarn-tag-add ()
  "Add a tag for a specific version of a package"
  (interactive)
  (let (package-and-version tag)
    (setq package-and-version (read-from-minibuffer "Which package and verson do you want to tag? (e.g: <package>@<version>):"))
    (setq tag (read-from-minibuffer "With what tag? (e.g: awesome):"))
    (message (concat "Adding tag " tag " to " package-and-version " (Check *yarn* for the output)"))
    (yarn-exec-with-path 'yarn-start-process "yarn-tag-add" "*yarn*" "yarn" "tag" "add" package-and-version tag)))

(defun yarn-tag-rm ()
  "Remove a tag from a package"
  (interactive)
  (let (package tag)
    (setq package (read-from-minibuffer "Which package and verson do you want to remove a tag? (e.g: <package>):"))
    (setq tag (read-from-minibuffer "With tag? (e.g: awesome):"))
    (message (concat "Removing tag " tag " from " package " (Check *yarn* for the output)"))
    (yarn-exec-with-path 'yarn-start-process "yarn-tag-add" "*yarn*" "yarn" "tag" "remove" package tag)))

(defun yarn-tag-ls ()
  "List all tags for a package"
  (interactive)
  (let (package)
    (setq package (read-from-minibuffer "Which package do you want to list tags for? (e.g: <package>):"))
    (message (concat "Listing tags for " package " (Check *yarn* for the output)"))
    (yarn-exec-with-path 'yarn-start-process "yarn-tag-ls" "*yarn*" "yarn" "tag" "ls" package)))

(defun yarn-version ()
  "Bump package version"
  (interactive)
  (let (version)
    (setq version (read-from-minibuffer "Bump version: "))
    (message (concat "Bumping version to" version " (Check *yarn* for the output)"))
    (yarn-exec-with-path 'yarn-start-process "yarn-version" "*yarn*" "yarn" "version" "--new-version" version)))

(defun yarn-publish ()
  "Publish package"
  (interactive)
  (message "Publishing package... (Check *yarn* for the output)")
  (yarn-exec-with-path 'yarn-start-process "yarn-publish" "*yarn*" "yarn" "publish"))

(defun yarn-publish-tag ()
  "Publish package with tag"
  (interactive)
  (let (tag)
    (setq tag (read-from-minibuffer "Tag (e.g: beta): " tag))
    (message "Publishing package... (Check *yarn* for the output)")
    (yarn-exec-with-path 'yarn-start-process "yarn-publish-tag" "*yarn*" "yarn" "publish" "---tag" tag)))

(defun yarn-publish-public ()
  "Publish public package"
  (interactive)
  (message "Publishing public package... (Check *yarn* for the output)")
  (yarn-exec-with-path 'yarn-start-process "yarn-publish-public" "*yarn*" "yarn" "publish" "--access" "private"))

(defun yarn-publish-private ()
  "Publish private package"
  (interactive)
  (message "Publishing private package... (Check *yarn* for the output)")
  (yarn-exec-with-path 'yarn-start-process "yarn-publish-private" "*yarn*" "yarn" "publish" "--access" "public"))

(defun yarn-info ()
  "Find information about a package"
  (interactive)
  (setq yarn-vars-last-search-keyword (read-from-minibuffer "Pakage name: " yarn-vars-last-search-keyword))
  (message (concat "Searching for " yarn-vars-last-search-keyword))
  (yarn-exec-with-path 'yarn-start-process "yarn-info" "*yarn*" "yarn" "info" yarn-vars-last-search-keyword))

(defun yarn-info-json ()
  "Find JSON-formatted information about a package"
  (interactive)
  (setq yarn-vars-last-search-keyword (read-from-minibuffer "Pakage name: " yarn-vars-last-search-keyword))
  (message (concat "Searching for " yarn-vars-last-search-keyword))
  (yarn-exec-with-path 'yarn-start-process "yarn-info-json" "*yarn*" "yarn" "info" yarn-vars-last-search-keyword "--json"))

(defun yarn-info-readme ()
  "Get the README of a package"
  (interactive)
  (setq yarn-vars-last-search-keyword (read-from-minibuffer "Pakage name: " yarn-vars-last-search-keyword))
  (message (concat "Searching for " yarn-vars-last-search-keyword))
  (yarn-exec-with-path 'yarn-start-process "yarn-info-readme" "*yarn*" "yarn" "info" yarn-vars-last-search-keyword "readme"))

(defun yarn-self-update ()
  "Make package linkable"
  (interactive)
  (message (concat "Updating yarn... (Check *yarn* for the output)"))
  (yarn-exec-with-path 'yarn-start-process "yarn-self-update" "*yarn*" "yarn" "self-update"))

(defalias 'yarn-update 'yarn-self-update)

(defvar yarn-node-error-regexp
  "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
  "Regular expression to match NodeJS errors.
From http://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/")

(defvar yarn-node-error-regexp-alist
  `((,yarn-node-error-regexp 1 2 3)))

(defun yarn-compilation-filter ()
  "Filter function for compilation output."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(define-compilation-mode yarn-compilation-mode "Yarn"
  "Yarn compilation mode."
  (progn
    (set (make-local-variable 'compilation-error-regexp-alist) yarn-node-error-regexp-alist)
    (add-hook 'compilation-filter-hook 'yarn-compilation-filter nil t)
  ))

(defun yarn-parse-scripts (raw-scripts)
  "Parse the output of the `yarn run` command in RAW-SCRIPTS into a list of scripts."
  (delq nil
        (mapcar (lambda (script-line)
                  (when (string-match-p "^  \\w" script-line)
                    (string-trim script-line)))
                raw-scripts)))

;;;###autoload
(defun yarn-run (&optional script)
  "Run a package script.

SCRIPT can be passed in or selected from a list of scripts configured in a package.json"
  (interactive)
  (save-some-buffers (not compilation-ask-about-save)
                     (when (boundp 'compilation-save-buffers-predicate)
                       compilation-save-buffers-predicate))
  (let* ((scripts (yarn-parse-scripts (yarn-exec-with-path (process-lines "yarn" "run"))))
         (selected-script (or script (ido-completing-read "Select script to run: " scripts)))
         (script (concat "yarn run " selected-script))
         (buffer-name (concat "*yarn run: " selected-script "*")))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
      (with-current-buffer (get-buffer-create buffer-name)
        (yarn-exec-with-path 'compilation-start script 'yarn-compilation-mode (lambda (m) (buffer-name))))))


(provide 'yarn)

;;; yarn.el ends here
