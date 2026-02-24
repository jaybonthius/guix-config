use std::collections::BTreeMap;
use zellij_tile::prelude::*;

/// Zellij plugin that detects session switches and tells the Emacs daemon
/// to switch its easysession to match the active Zellij session.
///
/// Mechanism:
/// - Subscribes to `SessionUpdate` events (fired on session attach/switch).
/// - When the current session name changes, runs `emacsclient --eval` on the
///   host to call `(jb-zellij-switch-session "session-name")`.
/// - Runs as a non-selectable background plugin (no UI).
#[derive(Default)]
struct EmacsSessionSwitcher {
    /// Tracks the last session name we told Emacs about, to avoid redundant
    /// switches when `SessionUpdate` fires without an actual session change.
    current_session: Option<String>,
}

register_plugin!(EmacsSessionSwitcher);

impl ZellijPlugin for EmacsSessionSwitcher {
    fn load(&mut self, _configuration: BTreeMap<String, String>) {
        request_permission(&[
            PermissionType::RunCommands,
            PermissionType::ReadApplicationState,
        ]);
        subscribe(&[EventType::SessionUpdate]);
        set_selectable(false);
    }

    fn update(&mut self, event: Event) -> bool {
        if let Event::SessionUpdate(sessions, _resurrectable) = event {
            if let Some(current) = sessions.iter().find(|s| s.is_current_session) {
                let new_name = &current.name;
                let should_switch = self
                    .current_session
                    .as_ref()
                    .map_or(true, |old| old != new_name);

                if should_switch {
                    self.current_session = Some(new_name.clone());
                    notify_emacs(new_name);
                }
            }
        }
        false // background plugin, never renders
    }

    fn render(&mut self, _rows: usize, _cols: usize) {}
}

/// Shell out to `emacsclient --eval` to tell the Emacs daemon to switch
/// its easysession to the given name.
fn notify_emacs(session_name: &str) {
    // Escape backslashes and double quotes in the session name to produce
    // a valid Emacs Lisp string literal.
    let escaped = session_name.replace('\\', "\\\\").replace('"', "\\\"");
    let expr = format!("(jb-zellij-switch-session \"{}\")", escaped);
    run_command(&["emacsclient", "--eval", &expr], BTreeMap::new());
}
