# Instructions for Agents

You are running in a Linux environment.

This project uses the `codermake` command to build sources from the local environment onto a remote IBM i system. Use your `codermake` skill to run builds.

## Building

**Important:** Run `codermake` from within the `ibmi-agentic` repo directory.

## EJS / Profound UI screens

**Read `EJS-UI-PLAYBOOK.md` before building or changing an EJS Rich Display screen.**
Several browser-side fetches do not survive the proxy in this environment, so screens
render blank, unstyled or boxed into a corner unless the shim is regenerated. The
playbook also covers the DDS, SQLRPGLE and codermake traps that report success while
doing nothing.

## Executing IBM i Applications

Use your `ibmi-interactive-session` skill to start an interactive session with IBM i to test your changes.
