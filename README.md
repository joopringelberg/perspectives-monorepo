## Perspectives

This is the Perspectives Monorepo. It was constructed from 12 previously independent repositories in spring 2025 and is now the sole source for the Perspectives and MyContexts programs. 


See the [**Release History**](https://github.com/joopringelberg/perspectives-monorepo/blob/master/RELEASES.md) of the monorepo.

For more information on the Perspectives Project, see its [documentation start page](https://joopringelberg.github.io/perspectives-documentation/).

We also have a document with [summary information for developers](https://github.com/joopringelberg/perspectives-monorepo/blob/master/localdevelopment.md).

## Weekly progress reports

The [weekly report workflow](.github/workflows/weekly-report.yml) creates an issue
every Tuesday (or on manual dispatch) with the last seven days of merged PRs and
commits reachable from **all pushed branches**. Shared commits are listed once;
commits belonging to that week's merged PRs, including their merge commits, are
covered by the PR section instead. Local-only commits and deleted branches cannot
be included. The time window uses GitHub's commit-date filter, not push time.

The request asks Copilot to inspect actual PR and commit diffs, group related work,
omit trivial churn, and avoid double-counting equivalent changes (including squash
merges and cherry-picks). Unmerged branch work must be distinguished from delivered
changes. Commit messages, changed-file lists, and change counts provide context,
not a significance threshold. Large activity lists continue in issue comments.
The executive summary remains limited to 500 words and is stored in `reports`.
The workflow prepares the issue and Copilot request; it does not itself run a model
or guarantee that mentioning Copilot will start a coding-agent session.

The collector's offline tests run with
`node --test .github/scripts/weekly-report.test.cjs`.