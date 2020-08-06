# Contributing

`emacs-lsp` is a very active organization so before you start working on 
a feature/issue make sure that there is a corresponding issue and drop a 
note that you start working on that to avoid collisions. 
Feel free to ping the maintainers in [Gitter](https://gitter.im/emacs-lsp/lsp-mode) 
chat or in the issue report if you need help/clarification on how things work.

If you discover issues, have ideas for improvements or new features,
please report them to the [issue tracker][1] of the repository or
submit a pull request. Please, try to follow these guidelines when you
do so.

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `master`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Mention your Emacs version and operating system.
* Mention the lsp-mode and related packages version info.
* Include any relevant code to the issue summary.

### Reporting bugs

When reporting bugs it's a good idea to go through the [Troubleshooting section
of the documentation][7].  Adding information like the backtrace and the *lsp-log* buffer to
the bug report makes it easier to track down bugs. Some steps to reproduce a bug
reliably would also make a huge difference.

## Pull requests

* Read [how to properly contribute to open source projects on Github][2].
* Use the same coding conventions as the rest of the project.
* Make sure that the unit tests are passing locally via `make test` or via the CI.
* Write [good commit messages][3].
* Update the [changelog][6].
* Code-style/formatting changes and typo-fixes should go into their own commits and put into the `.git-blame-ignore-revs` file to avoid thrashing the `git blame` data.

[1]: https://github.com/emacs-lsp/lsp-mode/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://github.com/emacs-lsp/lsp-mode/blob/master/CHANGELOG.md
[5]: https://emacs-lsp.github.io/lsp-mode/page/troubleshooting/
