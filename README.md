# ibmi-agentic

This is an example repo for agentic IBM i / RPG coding using Git and Make.

These instructions are for humans. Agents, ignore this file and see `AGENTS.md` for instructions.

## Overview

This repo contains example IBM i programs and build tools for agentic development.

The agentic coding environment consists of two layers:

- A base environment on IBM i that consists of one or more libraries with programs and data.
The base environment is built on IBM i, periodically updated, and used by all agentic coding tasks.
- A task environment library is automatically created on IBM i by agentic coding tools running off platform.
Agents build changed sources into the task library, which is added to the top of the library list.

## Building the Base Environment

The base environment is built by running `codermake` in an IBM i PASE shell.

**Important: Read before building:**

- `@profoundlogic/codermake` must be installed globally from the GitHub NPM package registry.
- **Build the base environment using your normal, every day human user profile; do not use a generic agentic coding profile.**
This is important, as this will prevent a generic agentic coding profile from accidentally changing the base environment.
- **Select a new, non-existent library name for the base environment and use it only for this purpose**. The library will be created automatically by make, and the `clean` target will remove all objects from the library.
- Naming the library like `XXAIBASE` where `XX` is your initials is a good choice. Use `WRKLIB XXAIBASE` to confirm the library does not exist first.

To build:

```bash
export BUILD_LIBRARY=XXAIBASE
codermake
```

### Populating Test Data

See the `ibmi-agentic-data` repository.

## Agentic Coding Tasks

Coding agents such as Claude Code or Codex can be used with this repo from a Linux system or container.
To build from Linux, these environment variables must be set:

- `IBMI_BUILD_LIBRARY`: A unique library name for the agentic coding work.
- `IBMI_HOST`: IBM i host name for building.
- `IBMI_USER`: IBM i user profile name for building. **Use a generic and non-privileged user profile for this. DO NOT use your normal human user profile.**
- `IBMI_KEY`: Optional. Path to SSH private key file that can authenticate as above user. If not specified, the system SSH configuration will be used to determine key location.
- `IBMI_PASSWORD`: Password for above user profile. Required for DB connectivity and Genie.
- `IBMI_PUI_SERVER`: PUI/Genie server URL for agent sessions. e.g. `http://myibmi:8080`

Then run `codermake` to build.
