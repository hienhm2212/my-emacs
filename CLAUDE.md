# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Little Fox Emacs - A personal Emacs configuration using literate programming with Org-mode. The configuration is written in `lf-config.org` which tangles to `lf-config.el` on startup.

## Architecture

### Entry Points
- `early-init.el` - Startup optimizations (GC threshold, UI elements disabled early, native-comp settings)
- `init.el` - Bootstrap loader that tangles `lf-config.org` to `lf-config.el` (only when .org is newer)
- `lf-config.org` - Main literate config (~3,200 lines) containing all package configurations

### Loading Mechanism
The config uses smart caching: `init.el` checks if `lf-config.el` is newer than `lf-config.org`. If so, it loads the cached `.el` directly. Otherwise, it tangles the `.org` file first. Delete `lf-config.el` to force re-tangling.

### Package Management
Uses `use-package` with MELPA. Packages are auto-installed (`use-package-always-ensure t`). Most packages are deferred via `:defer t` or `:commands` for fast startup.

### Private Settings
`private.el` (gitignored) is loaded at startup for credentials, API keys, and personal settings.

## Key Configuration Sections in lf-config.org

1. **Package Management** - Bootstrap use-package from MELPA
2. **UI and Visual** - Fonts (Iosevka/JetBrains Mono), doom-themes, doom-modeline, nerd-icons
3. **Completion & Navigation** - Vertico, Consult, Embark, Orderless (with qualifiers: `!` exclude, `,` initialism, `=` literal, `~` flex, `%` char-fold), Corfu, Cape
4. **Programming & Languages** - Eglot (LSP), Tree-sitter, Apheleia (formatting)
5. **Email** - mu4e with multi-account Gmail/Outlook
6. **Org Configuration** - Org-mode, Denote notes, presentations (org-present, org-re-reveal)
7. **AI & Web** - GPTel (OpenAI/Claude), eww, elfeed
8. **Hydra** - Modal keybinding menus via pretty-hydra

## Common Tasks

### Regenerate Config
```bash
# Delete compiled file to force re-tangling on next Emacs start
rm ~/.emacs.d/lf-config.el
```

### Testing Changes
Edit `lf-config.org`, save, then restart Emacs. The org file will automatically tangle.

For quick testing without restart, evaluate individual blocks in Emacs with `C-c C-c` on code blocks.

## Requirements

- Emacs 29+ (native-comp recommended)
- External tools: git, ripgrep, Node.js (for TS/JS LSP), Go (for gopls)
