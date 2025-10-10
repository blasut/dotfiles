# ivymacs

A clean, transparent Emacs configuration using the Ivy completion framework and Evil mode, inspired by Spacemacs but with full control over the `.emacs.d` folder.

## Project Philosophy

This configuration emphasizes:

- **Clarity**: Every package and keybinding is explicit and understandable
- **Literate Programming**: Configuration organized in `settings.org` using org-babel
- **Control**: Using vanilla Emacs with full access to `.emacs.d` (unlike Spacemacs)
- **Ivy Family**: Exploring the ivy/counsel/swiper ecosystem for completion and search
- **Evil Mode**: Vim-like keybindings throughout
- **Spacemacs-Inspired**: Leader key workflow using `general.el` with `SPC` prefix

## Configuration Style

### File Structure

```
.emacs.d/
├── init.el           # Bootstrap file that loads settings.org via org-babel
├── settings.org      # Main literate configuration (ACTIVE)
├── unused_settings.org  # Disabled/reference configurations
├── customized.el     # Custom-set variables (auto-generated)
└── settings.el       # Tangled output from settings.org
```

### Literate Configuration

All configuration lives in `settings.org` as an org-mode file with embedded emacs-lisp code blocks:

```org
** package-name
   Description of what this does
   #+BEGIN_SRC emacs-lisp
   (use-package package-name :ensure t
     :config
     (setq some-setting value))
   #+END_SRC
```

To disable a section without deleting it, add `:tangle no` to the source block header.

### Package Management

- **Package Manager**: use-package with MELPA and GNU ELPA
- **Installation**: Packages are installed automatically via `:ensure t`
- **Loading**: Lazy loading preferred with `:commands`, `:after`, or `:hook`

### Keybinding Organization

Keybindings follow Spacemacs conventions using `general.el`:

- **Leader Key**: `SPC` in normal/motion/emacs modes, `C-SPC` in insert mode
- **Mnemonic Structure**: Keys grouped by first letter
  - `SPC f` - **File** operations
  - `SPC b` - **Buffer** management
  - `SPC w` - **Window** commands
  - `SPC g` - **Git** (magit)
  - `SPC p` - **Project** (projectile)
  - `SPC h` - **Help** documentation
  - `SPC e` - **Eval** elisp evaluation
  - `SPC a` - **Applications** (dired, org-capture)
  - `SPC x` - **Text** manipulation
  - `SPC z` - **Zoom** text scaling
  - `SPC m` - **Major mode** specific (context-dependent)

### Evil Mode Configuration

- **evil-escape**: `fj` sequence exits insert mode
- **evil-surround**: Vim surround operations
- **evil-org**: Org-mode integration with evil
- **evil-collection**: Not used (keeping it minimal and explicit)

### Core Features

#### Completion & Search (Ivy Family)
- **ivy**: Generic completion framework
- **counsel**: Enhanced versions of common Emacs commands
- **swiper**: Isearch replacement with overview

#### Version Control
- **magit**: Full-featured Git interface

#### Appearance
- **Theme**: Solarized Dark
- **Font**: JetBrains Mono, height 160
- **UI**: Minimal (no toolbar, menubar, scrollbar)
- **which-key**: Keybinding hints after 0.3s delay

#### Org Mode
- **Startup**: Indent mode enabled by default
- **Directory**: `~/Documents/notes`
- **Capture Templates**: Todo, Journal, Notes, Read Later, Literate
- **Visual**: Hidden emphasis markers, source blocks with native fontification
- **Babel**: Enabled for emacs-lisp and lisp

### Settings Highlights

- **Encoding**: UTF-8 everywhere
- **Indentation**: Spaces only, no tabs
- **Backups**: Centralized in `~/.backups` with version control
- **Auto-revert**: Buffers refresh when files change externally
- **Line width**: 80 characters
- **Recursive minibuffers**: Enabled with depth indication
- **GC threshold**: 10 MiB for better performance

### macOS Specific

- **Meta key**: CMD (⌘) mapped to Meta
- **Option key**: Disabled (set to nil) to allow special characters
- **Clipboard**: Enabled integration with system clipboard

### Customization Functions

- `open-config`: Opens settings.org file
- `reload-config`: Reloads init.el and opens settings.org
- `my-create-non-existent-directory`: Auto-creates parent dirs when saving

## Unused/Archived Features

The `unused_settings.org` file contains previously used configurations including:

- Complex layer system (custom abstraction)
- Language-specific modes (Clojure, Scheme, C, Lisp)
- Projectile with counsel-projectile
- Additional defuns for file operations
- Parinfer, yasnippet, flycheck
- More extensive Evil configuration
- Alternative themes

These are kept for reference but not currently loaded.

## How to Use

1. **Edit Configuration**: `SPC f c` opens `settings.org`
2. **Reload Configuration**: `SPC e c` evaluates init.el and opens settings.org
3. **Add New Package**: Add a use-package block in settings.org, save, reload
4. **Disable Feature**: Add `:tangle no` to the source block header

## Installation

```bash
cd ~/.emacs.d
# Link or copy your settings.org and init.el
emacs
# Packages will auto-install on first launch
```

## Why This Configuration?

From the settings.org header:

> "The motivation for this project is twofold, one part is to try the ivy family of packages and the other to try using stock emacs where every package and keybinding is very clear. Sometimes spacemacs feels too big and cluttered."

This is about learning and control—understanding exactly what each part of your editor does, with the elegance of Ivy's completion system and the power of Evil's modal editing.
