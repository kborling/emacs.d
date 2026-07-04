# Daily Workflows

## Morning

1. Open Emacs — dashboard shows today's agenda, deadlines, follow-ups, inbox count
2. Press `a` on dashboard to see full week view
3. Press `!` to check attention items across your team
4. Process inbox: `C-c o a` → view inbox items, refile with `C-c C-w`

## During a Meeting

1. `C-c o c m` — capture meeting notes (attendees, notes, action items)
2. After meeting, convert action items to TODOs:
   - `C-c o c t` on each action item → prompted for deadline
   - Or `C-c o u` to create a follow-up for someone specific

## After a 1:1

1. `C-c o 1` — log structured 1:1 notes (updates, discussion, action items)
2. `C-c o w` — log wins you observed
3. `C-c o i` — log issues to track
4. `C-c o u` — create follow-ups with deadlines

## Quick Todo from Anywhere

- `C-c o c t` — prompted for description and deadline
- `C-c o c T` — todo without a deadline (someday/maybe)
- Deadline shortcuts: `+3` (3 days), `fri` (next Friday), `7-15` (July 15)

## Code Review / PR

1. Open the file → `C-c l` → `e` (explain a confusing section)
2. Select code → `C-c l` → `r` (suggest refactoring)
3. Need to understand the full context → `C-c l` → `F` (add file) then `s` (prompt)
4. Send findings to author: capture with `C-c o c n`

## Working on a Feature

### Start
1. `C-c l` → `N` — pick project, switch branch, start agent or chat
2. Or from dashboard: click a project, then `C-c l` → `x` (start agent)

### Think Through the Approach
1. `C-c l` → `l` — open a chat, describe what you want to build
2. Add context: `C-c l` → `F` (file), `p` (project)
3. Send: `C-c l` → `s`

### Execute
1. `C-c l` → `x` — start Claude Code agent
2. `C-c l` → `X` — give it the task (copy from your chat thinking)
3. `C-c l` → `y/n` — accept or reject changes
4. Switch between sessions: `C-c ;`

### Quick Fixes While Coding
- Select code → `C-c l` → `f` (fix bugs)
- Select code → `C-c l` → `r` (refactor)
- Select code → `C-c l` → `t` (generate tests)
- Select code → `C-c l` → `d` (add documentation)
- Flymake error at point → `C-c l` → `E` (fix error via agent)

### Recall Previous Work
- `C-c l` → `o` — browse past sessions (chats, archives, desktop imports)
- `C-c l` → `/` — search across all sessions
- `C-c l` → `>` — send a past session to the agent as context

## Angular Specific

1. `C-c a TAB` — cycle between component ts/html/scss/spec
2. `C-c a a` — full transient menu
3. `C-c a g` — generate schematic (component, service, etc.)
4. `C-c a n f` — create a new feature with barrel + path alias
5. `C-c a m d` — move directory, auto-update all imports
6. `C-c a S` — convert component to standalone
7. `C-c a R s` — ng serve

## Git Workflow

1. `C-c g` — VC transient (status, diff, log, branch, stash, push)
2. Common flow: `g` (status) → mark files → `c` (commit) → `p` (push)
3. `x` — unstage, `z z` — stash, `B` — blame

## Review Prep (Performance Reviews)

1. `C-c o r` — generates a review prep buffer for a person
   - Pulls all wins, issues, feedback, goals, 1:1 notes from the current FY
   - Adds a template for overall assessment, strengths, growth areas
2. `C-c l` → `>` — send the review prep to Claude Code for help writing
3. Or `C-c l` → `<` — send to chat to discuss and refine

## Weekly Review

1. `C-c o W` — creates weekly review template with per-person prompts
2. `C-c o !` — check what needs attention
3. `C-c o V` — view any person across all fiscal years
4. Process: review wins/issues, update goals, set follow-ups for next week

## Capture from Claude Desktop (Import)

1. Export data from Claude Desktop (Settings → Export)
2. `C-c l` → `i` — point to the zip file
3. Conversations are now searchable: `C-c l` → `/`
4. Browse them: `C-c l` → `o` (shows `[desktop]` prefix)
5. Send to agent: `C-c l` → `>` (recall → agent)

## Writing Documentation

1. Write in org-mode
2. Export: `C-c o x` — choose markdown, HTML, DOCX, or PDF
3. Or `C-c o P` — pandoc convert (any format to any format)
4. Preview markdown: `C-c C-p` (in markdown-mode)

## Encryption

- `C-c s e` — encrypt a file (saves as .gpg, deletes original)
- `C-c s d` — decrypt a .gpg file
- Contacts file auto-detects `.gpg` version if it exists

## Theme Switching

- `C-c t t` — pick any installed theme
- On EXWM: `s-t` — toggle dark/light (switches Emacs + kitty + GTK + dunst)
