---
allowed-tools: Bash(acli jira workitem view *)
---

# Jira Issue Onboarding

Get onboarded with an existing Jira issue by viewing its details, description, and comments.

## Arguments

- `$ARGUMENTS` - The Jira issue key (e.g., `INFRA-12668`) or full Jira URL

## Instructions

1. Parse the input to extract the issue key:
   - If a full URL is provided (e.g., `https://jira.example.com/browse/INFRA-12668`), extract the issue key from the path
   - If just the key is provided (e.g., `INFRA-12668`), use it directly

2. Run the following command to fetch the issue details:

   ```bash
   acli jira workitem view <ISSUE_KEY>
   ```

3. Present a summary of the issue including:
   - Issue key and title
   - Status and priority
   - Assignee and reporter
   - Description
   - Any comments

4. Ask if the user wants to proceed with any specific task related to this issue.
