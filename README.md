---
title: LSP Mode - Language Server Protocol support for Emacs
description: Language Server Protocol support with multiples languages support for Emacs
---

[![](https://melpa.org/packages/lsp-mode-badge.svg)](https://melpa.org/#/lsp-mode)
[![](https://stable.melpa.org/packages/lsp-mode-badge.svg)](https://stable.melpa.org/#/lsp-mode)
[![](https://badges.gitter.im/emacs-lsp/lsp-mode.svg)](https://gitter.im/emacs-lsp/lsp-mode)
[![](https://github.com/emacs-lsp/lsp-mode/workflows/CI/badge.svg)](https://github.com/emacs-lsp/lsp-mode/actions?query=workflow%3ACI)


<img src="examples/logo.png" width="240" align="right">

Language Server Protocol Support for Emacs
=========================================

![LSP mode](examples/head.png)

<hr>
<p align="center">
  <a href="https://emacs-lsp.github.io/lsp-mode"><strong>homepage</strong></a> •
  <a href="https://emacs-lsp.github.io/lsp-mode/page/installation"><strong>installation</strong></a> •
  <a href="https://emacs-lsp.github.io/lsp-mode/page/languages"><strong>languages</strong></a> •
  <a href="https://emacs-lsp.github.io/lsp-mode/page/settings"><strong>settings</strong></a> •
  <a href="https://emacs-lsp.github.io/lsp-mode/page/extensions"><strong>extensions</strong></a> •
  <a href="https://emacs-lsp.github.io/lsp-mode/page/troubleshooting"><strong>troubleshooting</strong></a> •
  <a href="https://emacs-lsp.github.io/lsp-mode/page/gallery"><strong>screenshots</strong></a> •
  <a href="https://emacs-lsp.github.io/lsp-mode/page/faq"><strong>FAQ</strong></a>
</p>
<hr>

## Why?

  - :heart: **Community Driven**
  - :gem: **Fully featured** - supports **all** features in Language Server Protocol v3.14.
  - :rocket: **Fast** - see [performance section](https://emacs-lsp.github.io/lsp-mode/page/performance).
  - :star2: **Flexible** - choose between full-blown IDE with flashy UI or minimal distraction free.
  - :gear: **Easy to configure** - works out of the box and automatically upgrades if additional packages are present.

## Overview

Client for [Language Server
Protocol](https://github.com/Microsoft/language-server-protocol/)
(v3.14). [lsp-mode](https://emacs-lsp.github.io/lsp-mode) aims to
provide IDE-like experience by providing optional integration with the
most popular Emacs packages like `company`, `flycheck` and `projectile`.

  - Non-blocking asynchronous calls
  - Real-time Diagnostics/linting via
    [flycheck](https://github.com/flycheck/flycheck) (recommended) or
    `flymake` when Emacs \> 26 (requires flymake\>=1.0.5)
  - Code completion - `company-capf` / `completion-at-point` (note that
    [company-lsp](https://github.com/tigersoldier/company-lsp) is no
    longer supported).
  - Hovers - using [lsp-ui](https://emacs-lsp.github.io/lsp-ui)
  - Code actions - via `lsp-execute-code-action`, [modeline](https://emacs-lsp.github.io/lsp-mode/page/main-features/#code-actions-on-modeline) (recommended) or [lsp-ui](https://emacs-lsp.github.io/lsp-ui) sideline.
  - Code outline - using builtin
    [imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html)
    or `helm-imenu`
  - Code navigation - using builtin
    [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html),
    [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs) tree views
    or [lsp-ui](https://emacs-lsp.github.io/lsp-ui) peek functions.
  - Code lens
  - Symbol highlights
  - Formatting
  - [Project errors](https://emacs-lsp.github.io/lsp-mode/page/main-features/#project-errors-on-modeline) on modeline
  - Debugger - [dap-mode](https://emacs-lsp.github.io/dap-mode/)
  - [Breadcrumb on headerline](https://emacs-lsp.github.io/lsp-mode/page/main-features/#breadcrumb-on-headerline)
  - Helm integration -
    [helm-lsp](https://github.com/emacs-lsp/helm-lsp/)
  - Ivy integration - [lsp-ivy](https://github.com/emacs-lsp/lsp-ivy/)
  - Treemacs integration -
    [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs)
  - Semantic highlighting as defined by LSP 3.16 (compatible language servers include recent development builds of clangd and rust-analyzer)
  - [which-key](https://github.com/justbur/emacs-which-key/) integration
    for better discovery

## Presentations/demos
  - [System Crafters](https://twitter.com/SystemCrafters) channel
    [Emacs IDE Videos](https://www.youtube.com/playlist?list=PLEoMzSkcN8oNvsrtk_iZSb94krGRofFjN)
  - [skybert](https://github.com/skybert)'s emacsconf
    [presentation](https://media.emacsconf.org/2019/19.html) (Java)
  - [thatwist](https://github.com/thatwist)'s ScalaUA Conference
    [presentation](https://www.youtube.com/watch?v=x7ey0ifcqAg&feature=youtu.be)
    (Scala)

## See also

  - [lsp-docker](https://github.com/emacs-lsp/lsp-docker/) - provide
    docker image with preconfigured language servers with corresponding
    emacs configuration.
  - [company-box](https://github.com/sebastiencs/company-box/) -
    `company` frontend with icons.
  - [dap-mode](https://github.com/emacs-lsp/dap-mode) - Debugger
    integration for `lsp-mode`.
  - [eglot](https://github.com/joaotavora/eglot) - An alternative
    minimal LSP implementation.
  - [which-key](https://github.com/justbur/emacs-which-key/) - Emacs
    package that displays available keybindings in popup
  - [projectile](https://github.com/bbatsov/projectile/) - Project
    Interaction Library for Emacs
  - [emacs-tree-sitter](https://github.com/ubolonton/emacs-tree-sitter) - Faster, fine-grained code highlighting via [tree-sitter](https://github.com/tree-sitter/tree-sitter).
  - [gccemacs](https://akrl.sdf.org/gccemacs.html) - modified Emacs capable of compiling and running Emacs Lisp as native code.

## Contributions

Contributions are very much welcome.

## Support the project

The `emacs-lsp` organization has more than **20.000 lines of code**, to keep all of this working,
we need to implement new features and help the community on a lot of issues.

You can help us keep going and improving it by **[supporting the project](https://opencollective.com/emacs-lsp)**

### Members

Here it is a list of the current `lsp-mode` members and what they are
primary working on/responsible for.

<table id="emacs-lsp-members">
  <tr>
    <td align="center">
      <div>
        <a href="https://github.com/totbwf">
          <img src="https://github.com/totbwf.png" width="100px;" style="border-radius: 50%;" alt="totbwf"/>
          <br/>
          <sub><b>totbwf</b></sub>
        </a>
        <br/>
        F#
      </div>
    </td>
    <td align="center">
      <div>
        <a href="https://github.com/brotzeit">
          <img src="https://github.com/brotzeit.png" width="100px;" style="border-radius: 50%;" alt="brotzeit"/>
          <br/>
          <sub><b>brotzeit</b></sub>
        </a>
        <br/>
        Rust
      </div>
    </td>
    <td align="center">
      <div>
        <a href="https://github.com/dsyzling">
          <img src="https://github.com/dsyzling.png" width="100px;" style="border-radius: 50%;" alt="dsyzling"/>
          <br/>
          <sub><b>dsyzling</b></sub>
        </a>
        <br/>
        Scala
      </div>
    </td>
    <td align="center">
      <div>
        <a href="https://github.com/kurnevsky">
          <img src="https://github.com/kurnevsky.png" width="100px;" style="border-radius: 50%;" alt="kurnevsky"/>
          <br/>
          <sub><b>kurnevsky</b></sub>
        </a>
        <br/>
        Scala | Rust
      </div>
    </td>
    <td align="center">
      <div>
        <a href="https://github.com/seagle0128">
          <img src="https://github.com/seagle0128.png" width="100px;" style="border-radius: 50%;" alt="seagle0128"/>
          <br/>
          <sub><b>seagle0128</b></sub>
        </a>
        <br/>
        Go | Python MS
      </div>
    </td>
  </tr>
  <tr>
    <td align="center">
      <div>
        <a href="https://github.com/sebastiansturm">
          <img src="https://github.com/sebastiansturm.png" width="100px;" style="border-radius: 50%;" alt="sebastiansturm"/>
          <br/>
          <sub><b>sebastiansturm</b></sub>
        </a>
        <br/>
        lsp-mode core | C++
      </div>
    </td>
    <td align="center">
      <div>
        <a href="https://github.com/vibhavp">
          <img src="https://github.com/vibhavp.png" width="100px;" style="border-radius: 50%;" alt="vibhavp"/>
          <br/>
          <sub><b>vibhavp</b></sub>
        </a>
        <br/>
        lsp-mode core
      </div>
    </td>
    <td align="center">
      <div>
        <a href="https://github.com/yyoncho">
          <img src="https://github.com/yyoncho.png" width="100px;" style="border-radius: 50%;" alt="yyoncho"/>
          <br/>
          <sub><b>yyoncho</b></sub>
        </a>
        <br/>
        lsp-mode core | Java
        </div>
    </td>
    <td align="center">
      <div>
        <a href="https://github.com/ericdallo">
          <img src="https://github.com/ericdallo.png" width="100px;" style="border-radius: 50%;" alt="ericdallo"/>
          <br/>
          <sub><b>ericdallo</b></sub>
        </a>
        <br/>
        Dart | Flutter | docs
      </div>
    </td>
    <td align="center">
      <div>
        <a href="https://github.com/danielmartin">
          <img src="https://github.com/danielmartin.png" width="100px;" style="border-radius: 50%;" alt="danielmartin"/>
          <br/>
          <sub><b>danielmartin</b></sub>
        </a>
        <br/>
        C++ | Swift
      </div>
    </td>
  </tr>
  <tr>
    <td align="center">
      <div>
        <a href="https://github.com/kiennq">
          <img src="https://github.com/kiennq.png" width="100px;" style="border-radius: 50%;" alt="kiennq"/>
          <br/>
          <sub><b>kiennq</b></sub>
        </a>
        <br/>
        completions | pwsh
      </div>
    </td>
  </tr>
</table>
