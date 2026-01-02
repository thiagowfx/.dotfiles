---
allowed-tools: Bash(cat:*), Bash(tail:*), Bash(jq:*), Bash(grep:*), Write
argument-hint: [branch-description]
description: Fork the conversation into a new independent thread
---

Fork this conversation into a new independent thread, allowing you to explore alternative approaches
without affecting the original conversation.

## What you'll do

1. **Summarize the current conversation context** up to this point:
   - Key decisions made
   - Current state of the codebase
   - What's been implemented or discussed

2. **Create a fork document** that captures:
   - The conversation history summary
   - The branching point description: $ARGUMENTS
   - The current project path: !`pwd`
   - Timestamp: !`date +%Y-%m-%d\ %H:%M:%S`

3. **Save the fork context** to `.claude/fork-[timestamp].md` in the current project

4. **Provide instructions** to the user:
   - How to start the new forked conversation by referencing the fork file
   - The command to use: `claude @.claude/fork-[timestamp].md`
   - Explain that they can continue both threads independently

## Fork Document Format

The fork document should include:

```markdown
# Conversation Fork - [Timestamp]

## Original Context

[Summary of what led to this point]

## Fork Reason

[Why this fork was created - use $ARGUMENTS if provided]

## Current State

**Project**: [path]
**Files Modified**: [list]
**Key Decisions**: [list]

## Next Steps

[What should be explored in this fork]

---

Use this context to continue the conversation in a new direction.
```

Make it easy for the user to start exploring a different path while keeping their original conversation intact.
