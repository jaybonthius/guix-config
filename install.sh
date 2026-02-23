#!/usr/bin/env bash
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
GUIX_PROFILE="$HOME/.config/guix/current"

source_guix_profile() {
    if [ -f "$GUIX_PROFILE/etc/profile" ]; then
        . "$GUIX_PROFILE/etc/profile"
    fi
}

install_guix() {
    if command -v guix >/dev/null 2>&1; then
        echo "Guix is already installed, skipping."
        return
    fi

    echo "Installing prerequisites..."
    sudo apt-get update -qq
    sudo apt-get install -y -qq wget xz-utils gnupg uidmap

    echo "Downloading Guix installer..."
    cd /tmp
    wget -q https://guix.gnu.org/guix-install.sh
    chmod +x guix-install.sh

    echo ""
    echo "NOTE: When the installer asks about AppArmor, answer 'n'."
    echo "      It is optional and fails on some Ubuntu versions."
    echo ""

    # Clean up any partial previous install
    if [ -d /var/guix ] || [ -d /gnu ]; then
        echo "Cleaning up partial previous install..."
        sudo ./guix-install.sh --uninstall || true
        cd /tmp
        wget -q https://guix.gnu.org/guix-install.sh
        chmod +x guix-install.sh
    fi

    sudo ./guix-install.sh || true
    rm -f /tmp/guix-install.sh

    if ! command -v guix >/dev/null 2>&1; then
        echo "ERROR: Guix installation failed." >&2
        exit 1
    fi

    echo "Guix installed successfully."
}

pull() {
    echo "Updating Guix (this may take a while on first run)..."
    source_guix_profile
    guix pull
    echo ""
    echo "Guix updated. Sourcing new profile..."
    source_guix_profile
}

reconfigure() {
    echo "Applying home configuration..."
    source_guix_profile
    guix home reconfigure -L "$DOTFILES_DIR" "$DOTFILES_DIR/home.scm"
}

patch_bashrc() {
    if grep -q '# Guix Home' "$HOME/.bashrc" 2>/dev/null; then
        echo "~/.bashrc already sources Guix Home profile, skipping."
        return
    fi
    echo "Patching ~/.bashrc to source Guix Home profile..."
    printf '\n# Guix Home\n[ -f ~/.profile ] && . ~/.profile\n' >> "$HOME/.bashrc"
}

compile_terminfo() {
    echo "Compiling terminfo entries..."
    if ! command -v tic >/dev/null 2>&1; then
        echo "WARNING: tic not found, skipping terminfo compilation." >&2
        echo "         Install ncurses (e.g. apt-get install ncurses-bin) and re-run." >&2
        return
    fi
    tic -x -o "$HOME/.terminfo" "$DOTFILES_DIR/terminfo/xterm-ghostty.ti"
    # Create ghostty symlink so TERM=ghostty also resolves
    mkdir -p "$HOME/.terminfo/g"
    ln -sf "../x/xterm-ghostty" "$HOME/.terminfo/g/ghostty"
    echo "Terminfo compiled: xterm-ghostty (+ ghostty symlink)"
}

fish_plugins() {
    echo "Installing fish plugins..."
    source_guix_profile
    if [ -f "$HOME/.profile" ]; then
        . "$HOME/.profile"
    fi
    fish -c 'fisher update'
}

case "${1:-setup}" in
    setup)
        install_guix
        pull
        reconfigure
        compile_terminfo
        patch_bashrc
        fish_plugins
        echo ""
        echo "Done! Log out and back in (or run 'source ~/.profile') to activate."
        ;;
    install-guix)      install_guix ;;
    pull)              pull ;;
    reconfigure)       reconfigure ;;
    compile-terminfo)  compile_terminfo ;;
    patch-bashrc)      patch_bashrc ;;
    fish-plugins)      fish_plugins ;;
    *)
        echo "Usage: $0 [setup|install-guix|pull|reconfigure|compile-terminfo|patch-bashrc|fish-plugins]"
        echo ""
        echo "  setup             Full setup from scratch (default)"
        echo "  install-guix      Install Guix package manager (requires sudo)"
        echo "  pull              Update Guix"
        echo "  reconfigure       Apply Guix Home configuration"
        echo "  compile-terminfo  Compile terminfo entries for Ghostty terminal"
        echo "  patch-bashrc      Patch ~/.bashrc to source Guix Home profile"
        echo "  fish-plugins      Install fish shell plugins"
        exit 1
        ;;
esac
