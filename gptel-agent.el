;;; gptel-agent.el --- Agentic LLM use for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2025 Karthik Chikmagalur

;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (gptel "0.9.9") (yaml "1.2.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/karthink/gptel-agent

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Agentic use mode for gptel: This is a collection of tools and prompts for
;; gptel to enable easy agentic LLM use.
;;
;; How to use:
;;
;; - Set `gptel-model' and `gptel-backend' to use your preferred LLM.
;;
;; - Run M-x `gptel-agent'.  This will open a buffer with the agent preset
;;   loaded.
;;
;; - Use gptel as usual, calling `gptel-send' etc.
;;
;; - If you change the system prompt, tools or other settings in this buffer, you
;;   can reset the agent state by (re)applying the "gptel-agent" preset from
;;   gptel's menu.
;;
;; - As with gptel, you can use gptel-agent in any buffer.  Just apply the
;;   "gptel-agent" preset in the buffer.
;;
;; gptel-agent can delegate tasks to "sub-agents".  Sub-agents can be specified
;; in Markdown or Org files in a directory.  To see how to specify agents,
;; examine the "agents" directory in this package.  You can add your directory
;; of agents to `gptel-agent-dirs', which see.
;;
;; Please note: gptel-agent uses significantly more tokens than the average
;; gptel LLM interaction.
;;
;;; Code:

(require 'gptel)
(require 'gptel-agent-tools)

(defcustom gptel-agent-dirs
  (list (expand-file-name
         "./agents/" (file-name-directory
                      (or load-file-name (buffer-file-name)))))
  "Agent definition directories for gptel-agent.

Markdown (.md) and Org (.org) files in these directories will be scanned
for gptel sub-agent definitions by gptel-agent."
  :type '(repeat directory)
  :group 'gptel-agent)

(defvar gptel--agents nil)

(defun gptel-agent-update ()
  "Update `gptel--agents' with agent presets."
  (mapc (lambda (dir)
          (dolist (agent-file (cl-delete-if-not #'file-regular-p
                                                (directory-files dir 'full)))
            (let* ((agent-plist
                    (pcase (file-name-extension agent-file)
                      ("org" (gptel-agent-parse-org-properties agent-file))
                      ("md" (gptel-agent-parse-markdown-frontmatter agent-file))))
                   (name (plist-get agent-plist :name)))
              (cl-remf agent-plist :name)
              (setf (alist-get name gptel--agents nil t 'equal)
                    agent-plist)
              (when (equal name "gptel-agent")
                (apply #'gptel-make-preset 'gptel-agent agent-plist)))))
        gptel-agent-dirs))

;;; Sub-agent definition parsers for Markdown and Org

(defalias 'gptel-agent-validator-default #'always)

;; Parsing utilities for gptel subagent definition files, from
;; - Markdown files with YAML frontmatter
;; - Org files with PROPERTIES blocks

(defun gptel-agent-parse-markdown-frontmatter (file-path &optional validator)
  "Parse a markdown file with optional YAML frontmatter.

FILE-PATH is the path to a markdown file.

VALIDATOR is an optional predicate function that takes a keyword
symbol and returns t if the key is allowed, nil otherwise.
If not provided, defaults to `gptel-agent-validator-default'.

Returns a plist with:
- All YAML frontmatter keys as keywords
- :system containing the markdown body text after frontmatter

If no frontmatter block exists, returns nil.

Signals an error if:
- The frontmatter block is malformed (opening without closing delimiter)
- A key in the frontmatter is not allowed by the validator"
  (unless validator
    (setq validator #'gptel-agent-validator-default))
  (require 'yaml)

  (with-temp-buffer
    (insert-file-contents file-path)

    ;; Check if file starts with frontmatter delimiter
    (if (not (looking-at-p "^---[ \t]*$"))
        nil                             ; No frontmatter

      ;; Move past opening delimiter
      (forward-line 1)
      (let ((frontmatter-start (point)))

        ;; Search for closing delimiter
        (unless (re-search-forward "^---[ \t]*$" nil t)
          (error "Malformed frontmatter: opening delimiter '---' found but no closing delimiter"))

        ;; Extract frontmatter text (from start to beginning of closing delimiter)
        (let* ((frontmatter-end (match-beginning 0))
               (frontmatter-str (buffer-substring-no-properties frontmatter-start frontmatter-end))
               (body-start (1+ (match-end 0))))

          ;; Parse YAML frontmatter
          (let ((parsed-yaml (yaml-parse-string
                              frontmatter-str
                              :object-type 'plist
                              :object-key-type 'keyword
                              :sequence-type 'list)))

            ;; Validate all keys in the parsed YAML
            (let ((current-plist parsed-yaml))
              (while current-plist
                (let ((key (car current-plist)))
                  (unless (funcall validator key)
                    (error "Invalid frontmatter key: %s" key)))
                (setq current-plist (cddr current-plist))))

            ;; Extract body text
            (let ((body-str (buffer-substring-no-properties
                             body-start (if (eq (char-before (point-max)) 10)
                                            (1- (point-max)) (point-max)))))
              ;; Add the markdown body as :system key
              (plist-put parsed-yaml :system body-str))))))))

(defun gptel-agent-parse-org-properties (file-path &optional validator)
  "Parse an Org file with properties in a :PROPERTIES: drawer.

FILE-PATH is the path to an Org file.

VALIDATOR is an optional predicate function that takes a keyword
symbol and returns t if the key is allowed, nil otherwise.
If not provided, defaults to `gptel-agent-validator-default'.

The function expects a :PROPERTIES: block at the top of the file
(before any headlines), with keys like name, description, tools,
backend, model, etc. Property names are case-insensitive and will
be converted to lowercase keyword symbols.

Returns a plist with:
- All properties from the :PROPERTIES: drawer as keywords
- :system containing the Org file body text after the property block

If no property block exists, returns nil.

Signals an error if:
- A key in the property block is not allowed by the validator"
  (unless validator
    (setq validator #'gptel-agent-validator-default))

  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((org-inhibit-startup t))
      (delay-mode-hooks (org-mode)))

    ;; Try to get the property block at this position
    (let ((prop-range (org-get-property-block)))
      (if (not prop-range)
          nil                           ; No property block found

        ;; Extract properties as an alist
        (let* ((props-alist (org-entry-properties (point-min) 'standard))
               (props-plist nil)
               (body-start (save-excursion
                             (goto-char (cdr prop-range))
                             (forward-line 1) ; Move past the :END: line
                             (while (looking-at-p "^\\s-*$") (forward-line 1))
                             (point))))

          ;; Process each property
          (dolist (pair props-alist)
            (let* ((key-str (downcase (car pair)))
                   (key-sym (intern (concat ":" key-str)))
                   (value (cdr pair)))

              (pcase key-sym
                (:tools (setq value (split-string value))))

              ;; Skip CATEGORY property (added automatically by Org)
              (unless (string-equal key-str "category")
                ;; Validate the key
                (unless (funcall validator key-sym)
                  (error "Invalid property key: %s" key-sym))

                ;; Add to plist
                (setq props-plist (plist-put props-plist key-sym value)))))

          ;; Extract body text (everything after the property block)
          (let ((body-str (buffer-substring-no-properties
                           body-start (if (eq (char-before (point-max)) 10)
                                          (1- (point-max)) (point-max)))))
            ;; Add the body as :system key
            (plist-put props-plist :system body-str)))))))

;;; Commands

;;;###autoload
(defun gptel-agent (&optional project-dir agent-preset)
  "Start a gptel-agent session in the current project."
  (interactive
   (list (if current-prefix-arg
             (funcall project-prompter)
           (or (project-root (project-current))
               default-directory))
         'gptel-agent))
  (let ((gptel-buf
         (gptel (generate-new-buffer-name
                 (format "*gptel-agent:%s*"
                         (cadr (nreverse (file-name-split project-dir)))))
                nil
                (and (use-region-p)
                     (buffer-substring (region-beginning)
                                       (region-end)))
                'interactive)))
    (with-current-buffer gptel-buf
      (setq default-directory project-dir)
      (gptel-agent-update)              ;Update all agent definitions
      (gptel--apply-preset              ;Apply the gptel-agent preset
       'gptel-agent (lambda (sym val) (set (make-local-variable sym) val)))
      (when gptel-use-header-line
        (setcar header-line-format
                '(:eval (concat
                         (propertize " " 'display '(space :align-to 0))
                         (format "%s" (gptel-backend-name gptel-backend))
                         (propertize "[Agent]"))))))))

(provide 'gptel-agent)

;;; gptel-agent.el ends here
