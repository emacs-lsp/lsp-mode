# Adding support for languages

## Registering server

Here it is the minimal configuration that is needed for new language
server registration. Refer to the documentation of `lsp-client.el` for
the additional settings supported on registration time.
`lsp-language-id-configuration` must be updated to contain the
corresponding mode -\> language id - in this case `(python-mode .
"python")`

``` elisp
(defvar lsp-language-id-configuration
  '(...
    (python-mode . "python")
    ...))
;; if you are adding the support for your language server in separate repo use
;; (add-to-list 'lsp-language-id-configuration '(python-mode . "python"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
                  :major-modes '(python-mode)
                  :server-id 'pyls))
```

If the language server supports environment variables to control
additional behavior, you can register that by using the
`:environment-fn` function, like the Bash language client does:

``` elisp
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("bash-language-server" "start"))
                  :major-modes '(sh-mode)
                  :priority -1
                  :environment-fn (lambda ()
                                    '(("EXPLAINSHELL_ENDPOINT" . lsp-bash-explainshell-endpoint)
                                      ("HIGHLIGHT_PARSING_ERRORS" . lsp-bash-highlight-parsing-errors)))
                  :server-id 'bash-ls))
```

`lsp-bash-explainshell-endpoint` and `lsp-bash-highlight-parsing-errors`
are language client `defcustom` that expose supported server environment
settings in a type-safe way. If you change any of those variables,
restart the language server with `lsp-restart-workspace` for the changes
to be applied.

## Sections

`lsp-mode` provides tools to bridge emacs `defcustom` as a language
configuration sections properties(see [specification
workspace/configuration](https://microsoft.github.io/language-server-protocol/specification#workspace_configuration)).
In addition you may use `lsp-generate-settings` from [Generate Settings
script](https://github.com/emacs-lsp/lsp-mode/blob/master/scripts/lsp-generate-settings.el)
to generate `defcustom` from `package.json` VScode plugin manifest.
Example:

``` elisp
(defcustom lsp-foo-language-server-property "bar"
  "Demo property."
  :group 'foo-ls
  :risky t)

(lsp-register-custom-settings '(("foo.section.property" lsp-foo-language-server-property)))

(lsp-configuration-section  "foo")
;; =>  (("foo" ("settings" ("property" . "bar"))))
```

## Documentation

  - Add the new language server to the [lsp-clients.json](https://github.com/emacs-lsp/lsp-mode/blob/master/docs/lsp-clients.json) file sorted by the `full-name` key alphabetically.
  - Create a new navigation entry in [mkdocs.yml](https://github.com/emacs-lsp/lsp-mode/blob/master/mkdocs.yml#L4) file.

