;;; lsp-matlab.el --- LSP mode integration for MATLAB -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; LSP client for the MATLAB language server
;;

;;; Code:
(require 'lsp-mode)

;;; MATLAB
(defgroup lsp-matlab nil
  "Lsp support for MATLAB."
  :group 'lsp-mode
  :tag "Lsp MATLAB")

(defcustom lsp-clients-matlab-nodejs "node"
  "Node.js to launch the MATLAB language server."
  :group 'lsp-matlab
  :type '(string))

(defcustom lsp-clients-matlab-server "/usr/local/apps/matlabls/out/index.js"
  "Path to the MATLAB language server.
To setup,
- Download the language server (clone or unzip):
   - git clone https://github.com/mathworks/MATLAB-language-server.git
   or
   - Download zip from https://github.com/mathworks/MATLAB-language-server
     and unzip.
- In the downloaded directory,
    npm install
    npm run compile
    npm run package  # optional JavaScript minimization
- Set lsp-clients-matlab-server to the download directory, or
  copy the ./out and ./matlab/ directory trees to an install location, e.g.
    cp -r ./out/ /usr/local/apps/matlabls/out/
    cp -r ./matlab/ /usr/local/apps/matlabls/matlab/
  then set `lsp-clients-matlab-server' to the install location."
  :group 'lsp-matlab
  :type '(string))

(defcustom lsp-clients-matlab-server-args '("--stdio")
  "MATLAB language server arguments."
  :group 'lsp-matlab
  :risky t
  :type '(repeat string))

(defcustom lsp-clients-matlab-install-path ""
  "Path to MATLAB to use.
If not specified, then matlab is used from the system path."
  :group 'lsp-matlab
  :type '(string))

(defcustom lsp-clients-matlab-cmd-args ""
  "Extra arguments the language server should specify when starting MATLAB."
  :group 'lsp-matlab
  :type '(string))

(defcustom lsp-clients-matlab-connection-timing "onStart"
  "When to start the MATLAB language server."
  :group 'lsp-matlab
  :type '(choice
          (const "onStart")
          (const "onDemand")
          (const "never")))

(defcustom lsp-clients-matlab-index-workspace nil
  "Whether or not to use the full background indexer.

Turning this on instructs the MATLAB language server to index all
*.m files under the project root.  If there are thousands of *.m
files, then the MATLAB language server may become unresponsive,
causing hangs."
  :group 'lsp-matlab
  :type '(boolean))

;; Tell lsp-mode about MATLAB language
(add-to-list 'lsp-language-id-configuration '(matlab-mode . "MATLAB"))

(defun matlabls-command ()
  "Return matlabls launch command LIST."
  (let ((cmd (flatten-tree `(,lsp-clients-matlab-nodejs
                             ,lsp-clients-matlab-server
                             ,@lsp-clients-matlab-server-args))))
    cmd))

;; lsp-register-custom-settings plus :initialized-fn send following.
;; For available settings, see src/lifecycle/ConfigurationManager.ts
;;   Params: {
;;     "settings": {
;;       "MATLAB": {
;;         "telemetry": false,
;;         "matlabConnectionTiming": "onStart",
;;         "installPath": "/path/to/matlab",
;;         "indexWorkspace": false
;;       }
;;     }
;;   }
;;
;; Messages from the MATLAB language server:
;;   - "telemetry/logdata"
;;        The client can provide usage (telemetry) data to the server which is collected to improve
;;        the MATLAB language server. Setting telemetry to false tells the language server that the
;;        client is not sending data which is what Emacs is currently doing.
;;   - "mvmStateChange"
;;        When the MATLAB Virtual Machine (mvm) detects a change in MATLAB's state (disconnected,
;;        ready, or busy) the client will receive these messages. VS Code uses these messages for
;;        the embedded MATLAB Command Window. Currently, Emacs ignores these.
;;   - "matlab/connection/update/server”
;;        This message indicates a change in connection between the language server and MATLAB
;;        (connecting, connected, or disconnected). In our case, VS Code updates its UI affordance
;;        to show the active state of MATLAB.

(lsp-register-custom-settings
 `(("MATLAB.indexWorkspace" nil t)
   ("MATLAB.installPath" (lambda () lsp-clients-matlab-install-path))
   ("MATLAB.matlabConnectionTiming" ,lsp-clients-matlab-connection-timing)
   ("MATLAB.maxFileSizeForAnalysis" 0)
   ("MATLAB.telemetry" nil t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'matlabls-command)
                  :major-modes '(matlab-mode)
                  :ignore-messages '("readFile .*? requested by MATLAB but content not available")
                  :server-id 'matlab-ls
                  :language-id "MATLAB"
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                                        (lsp--set-configuration
                                                         (lsp-configuration-section "MATLAB"))))
                  :notification-handlers ;; See src/notifications/NotificationService.ts
                  (ht ("telemetry/logdata" #'ignore)
                      ("mvmStateChange" #'ignore)
                      ("matlab/connection/update/server" #'ignore))))

(provide 'lsp-matlab)
;;; lsp-matlab.el ends here

;; LocalWords:  defcustom nodejs matlabls npm defun fn
