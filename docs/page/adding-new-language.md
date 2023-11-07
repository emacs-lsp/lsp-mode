---
root_file: docs/page/adding-new-language.md
---
# Adding support for languages

## Registering server

Here it is the minimal configuration that is needed for new language
server registration. Refer to the documentation of `lsp-mode.el` for
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
                  :activation-fn (lsp-activate-on "python")
                  :server-id 'pyls))
```

`lsp-mode` is using `lsp-language-id-configuration` to determine what is the
buffer language. When the `major-mode` is not sufficient to determine the
language (e.g. `web-mode` is used for `javascript`, `html`, and `css`) you can put regex.

**Note:** In the above example, when a new client is created using
`make-lsp-client`, a new connection to the language server is created
using `lsp-stdio-connection`.  Please carefully check its
documentation, as the function checks for various things (e.g. testing
for the executable in PATH) and handles respective errors.  Often
while adding a server, the LSP client author might do these checks
themselves, but without handling the errors correctly.  This leads to
features like `lsp-install-server` breaking for other users; e.g. see
[this issue](https://github.com/emacs-lsp/lsp-mode/issues/3415).  This
is a common mistake that keeps reoccurring.

Here's an example of how to set up a custom language server in your `init.el` file:

```elisp
;; Use shopify-cli / theme-check-language-server for Shopify's liquid syntax
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
    '(shopify-mode . "shopify"))

  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "theme-check-language-server")
                     :activation-fn (lsp-activate-on "shopify")
                     :server-id 'theme-check)))
```

**Note:** This example assumes that you've already set up a major mode of your own either by [deriving it](https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html) from `web-mode` or perhaps by writing it yourself.

If the language server supports environment variables to control
additional behavior, you can register that by using the
`:environment-fn` function, like the Bash language client does:

``` elisp
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("bash-language-server" "start"))
                  :activation-fn (lsp-activate-on "shellscript")
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

Also, if new client support customizing language server path. It's recommended
to make a wrapper function so the user can customize the value even after the
client has been loaded.

``` elisp
(defcustom lsp-tex-executable "digestif"
  "Command to start the Digestif language server."
  :group 'lsp-tex
  :risky t
  :type 'file)

(lsp-register-client
 (make-lsp-client
  ;; instead of `:new-connection (lsp-stdio-connection lsp-tex-executable)` use
  :new-connection (lsp-stdio-connection (lambda () lsp-tex-executable))
  :activation-fn (lsp-activate-on "plaintex" "latex")
  :priority -1
  :server-id 'digestif))
```

## Sections

`lsp-mode` provides tools to bridge emacs `defcustom` as a language
configuration sections properties(see [specification
workspace/configuration](https://microsoft.github.io/language-server-protocol/specification#workspace_configuration)). In addition you may use `lsp-generate-settings`
from [Generate Settings script](https://github.com/emacs-lsp/lsp-mode/blob/master/scripts/lsp-generate-settings.el) to generate `lsp-defcustom` from `package.json`
VScode plugin manifest. Example:

``` elisp
(lsp-defcustom lsp-foo-language-server-property "bar"
  "Demo property."
  :group 'foo-ls
  :lsp-path "foo.section.property")

(lsp-configuration-section "foo")
;; =>  (("foo" ("settings" ("property" . "bar"))))
```

## Documentation

  - Add the new language server to the [lsp-clients.json](https://github.com/emacs-lsp/lsp-mode/blob/master/docs/lsp-clients.json) file sorted by the `full-name` key alphabetically.
  - Create a new navigation entry in [mkdocs.yml](https://github.com/emacs-lsp/lsp-mode/blob/master/mkdocs.yml#L4) file.
