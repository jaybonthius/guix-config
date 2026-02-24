# zellij-emacs-session

Zellij WASM plugin that automatically syncs the Emacs daemon's
[easysession](https://github.com/jamescherti/easysession.el) to match the
active Zellij session.

When you switch Zellij sessions, the plugin detects the change and calls
`emacsclient --eval '(jb-zellij-switch-session "session-name")'` to save the
current easysession and load the one matching the new Zellij session.

## Prerequisites

- **Rust toolchain** with the `wasm32-wasip1` target:
  ```sh
  rustup target add wasm32-wasip1
  ```
- **C compiler** (needed by some Rust dependencies). On Guix there is no
  system `cc`, so wrap the build in:
  ```sh
  guix shell gcc-toolchain -- bash -c '...'
  ```

## Building

The default build target is set in `.cargo/config.toml`, so a plain
`cargo build --release` produces the WASM binary:

```sh
guix shell gcc-toolchain -- bash -c '
  export PATH="$HOME/.cargo/bin:$PATH"
  ln -sf "$(which gcc)" "$HOME/.cargo/bin/cc"
  cargo build --release
'
```

The output is at `target/wasm32-wasip1/release/zellij-emacs-session.wasm`.

## Updating the committed binary

After rebuilding, copy the new binary to the project root so it is tracked
by git and referenced by `home.scm`:

```sh
cp target/wasm32-wasip1/release/zellij-emacs-session.wasm .
```

## Deploying

Run Guix Home to symlink the plugin into the Zellij config directory:

```sh
GUIX_PACKAGE_PATH=~/dotfiles guix home reconfigure ~/dotfiles/home.scm
```

Then kill and restart your Zellij sessions so the updated plugin loads.
You may also need to clear the Zellij plugin cache if it serves a stale
copy:

```sh
rm -rf ~/.cache/zellij/
```
