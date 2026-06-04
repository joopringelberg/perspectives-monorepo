Use the Explore agent, {{thoroughness}}.

Repository context:
- Monorepo: perspectives-monorepo
- Primary package: {{package_or_area}}
- Problem focus: {{problem_or_question}}

What to do:
- Find the smallest set of files that directly control this behavior.
- Trace the relevant call path or data flow.
- Identify the owning abstractions, not just wrappers or registrations.
- Prefer concrete anchors such as symbols, files, tests, error messages, or commands.
- Stay read-only. Do not edit files.
- Keep the search local and efficient. Do not map broad unrelated surfaces.

What I want back:
- A short summary of how the behavior works.
- The most relevant files and functions.
- The best candidate fix points, if applicable.
- Any nearby tests or validations worth running.
- Open questions only if they block a confident conclusion.

Output format:
- Summary
- Key files
- Control flow
- Fix candidates
- Validation ideas
- Open questions

Inputs:
- Anchor: {{anchor}}
- Suspected files: {{suspected_files}}
- Search terms: {{search_terms}}
- Constraints: {{constraints}}