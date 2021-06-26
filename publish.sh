#!/usr/bin/env bash
set -euo pipefail

emacs --batch --load ~/.emacs.d/init.el  --load publish.el --funcall org-publish-all
