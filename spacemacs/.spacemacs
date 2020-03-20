;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.private/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(nginx
     windows-scripts
     (typescript
      :variables
      ;; typescript-fmt-on-save t
      typescript-backend 'lsp
      typescript-fmt-tool 'typescript-formatter)
     elm
     html
     python
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; helm
     ivy

     ;; TODO: look at this settings and compare in .org
     (osx ;; :variables osx-use-option-as-meta nil
      )

     ;; version-control
     (version-control :variables
                      version-control-diff-tool 'git-gutter+)
     git
     github

     ;; syntax
     syntax-checking

     ;; auto-complete
     (auto-completion :variables
                      auto-completion-private-snippets-directory (expand-file-name "~/.private/snippets")
                      auto-completion-return-key-behavior nil
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-help-tooltip t)
     ;; programming stuff
     lsp
     dap

     ;; programming languages
     (haskell :variables
              haskell-enable-hindent t
              haskell-process-type 'stack-ghci
              haskell-completion-backend 'intero)
     ;; lisps
     emacs-lisp
     clojure
     common-lisp

     ;;(ruby :variables ruby-enable-enh-ruby-mode t ruby-version-manager 'rbenv ruby-test-runner 'rspec)
     ruby-on-rails
     ;; js
     (javascript
      :variables
      ;; javascript-backend 'tern
      javascript-backend 'lsp
      javascript-fmt-tool 'prettier
      javascript-import-tool 'import-js
      node-add-modules-path t)
     prettier
     react
     ;; random
     octave

     ;; org stuff
     (org :variables
          org-projectile-file "~/Dropbox/TODOs.org"
          ;; the package needs to be updted to support org 9.2
          ;; org-enable-reveal-js-support t
          )
     bibtex
     ;; pdf-tools

     ;; Misc needed
     csv
     markdown
     yaml
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-default-position 'bottom)
     themes-megapack
     docker

     ;; Misc useful
     command-log
     (multiple-cursors :variables multiple-cursors-backend 'evil-mc)
     spell-checking
     treemacs

     ;; Misc fun
     ;; Really unsure about this package, but might be worth trying
     (colors :variables colors-colorize-identifiers 'variables)


     erlang
     sql

     elixir
     ;; go
     ;; php
     ;; swift
     c-c++
     cscope
     semantic
     ;; python
     coffeescript

     ;; my private stuff
     my-lispyville

     ;; gtags
     gtags
     ;; my-gtags

     go
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      org-plus-contrib
                                      ztree
                                      ivy-rich
                                      interleave
                                      ruby-refactor
                                      visual-regexp
                                      ivy-bibtex
                                      dired-collapse
                                      dired-subtree
                                      idris-mode
                                      vue-mode
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (bookmarks . 5)
                                (agenda . 5)
                                (todos . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Hack"
                               :size 16.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)

  (setq lsp-clients-typescript-server "typescript-language-server"
        lsp-clients-typescript-server-args '("--stdio"))

  ;; to stop lsp to ask if it should restart the lsp server when quitting emacs
  (setq lsp-restart 'ignore)

  ;; (setq configuration-layer-elpa-archives '(("melpa" . "melpa.org/packages/")
  ;;                                           ("org" . "orgmode.org/elpa/")
  ;;                                           ("gnu" . "elpa.gnu.org/packages/")))
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (org-babel-load-file
   (expand-file-name "~/settings.org"))

  ;; experimentation area after settings has loaded, when working as expcted it should be mved to settings.org
  (add-to-list 'load-path "~/.private/local/clojure-scemantic/")
  (load "clojure.el")

  ;; temp bugfix, probably
  (global-set-key (kbd "M-x") 'counsel-M-x)


  ;; (add-to-list 'load-path "~/.private/local/ivy-rich/")
  ;; (require 'ivy-rich)
  ;; (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev)

  ;; (defun spacemacs//elixir-setup-lsp ()
  ;;   "Setup lsp backend."
  ;;   (if (configuration-layer/layer-used-p 'lsp)
  ;;       (progn
  ;;         (lsp))
  ;;     (message (concat "`lsp' layer is not installed, "
  ;;                      "please add `lsp' layer to your dotfile."))))
  ;; (spacemacs//elixir-setup-lsp)
  ;; (add-hook 'elixir-mode-hook #'spacemacs//elixir-setup-lsp)

  (add-to-list 'load-path "~/.private/local/lsp-docker/")
  ;; (load "lsp-docker")
  ;; (defvar lsp-docker-client-packages
  ;;   '(lsp-css lsp-clients lsp-bash lsp-go lsp-pyls lsp-html lsp-typescript
  ;; 	          lsp-terraform lsp-cpp))

  ;; (defvar lsp-docker-client-configs
  ;;   (list
  ;;    (list :server-id 'bash-ls :docker-server-id 'bashls-docker :server-command "bash-language-server start")
  ;;    (list :server-id 'clangd :docker-server-id 'clangd-docker :server-command "ccls")
  ;;    (list :server-id 'css-ls :docker-server-id 'cssls-docker :server-command "css-languageserver --stdio")
  ;;    (list :server-id 'dockerfile-ls :docker-server-id 'dockerfilels-docker :server-command "docker-langserver --stdio")
  ;;    (list :server-id 'gopls :docker-server-id 'gopls-docker :server-command "gopls")
  ;;    (list :server-id 'html-ls :docker-server-id 'htmls-docker :server-command "html-languageserver --stdio")
  ;;    (list :server-id 'pyls :docker-server-id 'pyls-docker :server-command "pyls")
  ;;    (list :server-id 'ts-ls :docker-server-id 'tsls-docker :server-command "typescript-language-server --stdio")))

  ;; (require 'lsp-docker)
  ;; (lsp-docker-init-clients
  ;;  :path-mappings '(("~/.private/local/lsp-docker/demo-projects" . "/projects"))
  ;;  :client-packages lsp-docker-client-packages
  ;;  :client-configs lsp-docker-client-configs)
  
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-agenda-files
     (quote
      ("~/Dropbox/notes/reading/index.org" "~/Dropbox/notes/keybindings.org" "~/Dropbox/notes/snippets.org" "~/Dropbox/notes/notes.org" "~/Dropbox/notes/beorg/TODOs.org" "~/Dropbox/notes/beorg/journal.org" "~/Dropbox/notes/beorg/inbox.org" "~/Dropbox/notes/beorg/projects.org" "~/Dropbox/notes/reading/index.org" "~/Dropbox/notes/work/work.org" "~/Dropbox/notes/keybindings.org" "~/Dropbox/notes/snippets.org" "~/Dropbox/notes/notes.org")))
   '(package-selected-packages
     (quote
      (nginx-mode utop tuareg caml ocp-indent mvn meghanada maven-test-mode lsp-java groovy-mode groovy-imports gradle-mode github-search github-clone gist gh marshal logito pcache forge ghub closql emacsql-sqlite emacsql flycheck-ocaml merlin emojify emoji-cheat-sheet-plus dune company-emoji auto-complete-rst graphviz-dot-mode dockerfile-mode docker docker-tramp helm-gtags zenburn-theme yasnippet-snippets writeroom-mode visual-fill-column winum wgrep web-mode vue-mode toc-org tide typescript-mode tao-theme sql-indent solarized-theme slime-company slime seti-theme ruby-hash-syntax rspec-mode robe rjsx-mode request pyvenv projectile-rails powershell pip-requirements paradox orgit org-ref pdf-tools helm-bibtex tablist org-projectile org-mime org-download org-brain minimal-theme lsp-ui live-py-mode lispyville lispy zoutline kaolin-themes json-navigator json-mode ivy-hydra ivy-bibtex parsebib intero inkpot-theme idris-mode hlint-refactor hl-todo highlight-numbers highlight-indentation helm-make helm helm-core gruvbox-theme google-translate go-guru git-timemachine git-link ggtags flycheck-haskell haskell-mode flycheck-elm eyebrowse evil-visual-mark-mode evil-surround evil-nerd-commenter evil-matchit evil-magit evil-goggles espresso-theme eshell-prompt-extras erlang elm-test-runner elm-mode reformatter editorconfig dumb-jump dracula-theme doom-themes doom-modeline eldoc-eval diff-hl darktooth-theme darkokai-theme dap-mode cython-mode cyberpunk-theme cquery counsel-projectile company-lsp company-go company-anaconda color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode cider-eval-sexp-fu eval-sexp-fu cider sesman clojure-mode centered-cursor-mode ccls lsp-mode browse-at-remote biblio biblio-core auto-yasnippet auto-compile apropospriate-theme anaconda-mode alect-themes aggressive-indent ace-link inf-ruby tern company counsel swiper iedit smartparens elixir-mode flycheck go-mode window-purpose imenu-list ivy yasnippet magit-popup magit transient git-commit with-editor lv markdown-mode alert pythonic simple-httpd spaceline powerline all-the-icons projectile ace-window avy f dash which-key use-package async org-plus-contrib evil goto-chg hydra ztree zen-and-art-theme yapfify yaml-mode xterm-color xcscope ws-butler white-sand-theme web-beautify vue-html-mode volatile-highlights visual-regexp vi-tilde-fringe uuidgen undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treepy treemacs-projectile treemacs-evil tree-mode toxi-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stickyfunc-enhance ssass-mode srefactor spinner spaceline-all-the-icons spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slim-mode shrink-path shell-pop seeing-is-believing scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor rubocop reverse-theme reveal-in-osx-finder restart-emacs rebecca-theme rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme queue pytest pyenv-mode py-isort purple-haze-theme pug-mode prop-menu professional-theme prettier-js popwin planet-theme pippel pipenv phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode password-generator parent-mode packed overseer osx-trash osx-dictionary organic-green-theme org-present org-pomodoro org-category-capture org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-elixir ob-coffeescript noctilux-theme naquadah-theme nameless mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest material-theme markdown-toc majapahit-theme magit-svn magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum log4e livid-mode link-hint light-soap-theme launchctl key-chord json-snatcher json-reformat js2-refactor js-doc jbeans-theme jazz-theme ivy-yasnippet ivy-xref ivy-rtags ivy-purpose ir-black-theme interleave inflections indent-guide importmagic impatient-mode hungry-delete hindent highlight-parentheses highlight hierarchy heroku-theme hemisu-theme hc-zenburn-theme haskell-snippets gruber-darker-theme graphql grandshell-theme gotham-theme google-c-style golden-ratio godoctor go-tag go-rename go-impl go-gen-test go-fill-struct go-eldoc gnuplot gntp gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-messenger git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme fuzzy font-lock+ flycheck-rtags flycheck-pos-tip flycheck-mix flycheck-credo flx-ido flatui-theme flatland-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery eziam-theme expand-region exotica-theme evil-visualstar evil-unimpaired evil-tutor evil-org evil-numbers evil-mc evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eshell-z esh-help emmet-mode elisp-slime-nav edit-indirect dotenv-mode django-theme disaster dired-subtree dired-collapse diminish darkmine-theme darkburn-theme dakrone-theme csv-mode counsel-gtags counsel-css company-web company-tern company-statistics company-rtags company-quickhelp company-cabal company-c-headers common-lisp-snippets command-log-mode column-enforce-mode coffee-mode cmm-mode clues-theme clojure-snippets clean-aindent-mode clang-format chruby cherry-blossom-theme busybee-theme bundler bui bubbleberry-theme birds-of-paradise-plus-theme bind-key badwolf-theme autothemer auto-highlight-symbol anti-zenburn-theme ample-zen-theme ample-theme alchemist afternoon-theme add-node-modules-path ac-ispell))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
