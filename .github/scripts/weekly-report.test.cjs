const assert = require('node:assert/strict');
const { test } = require('node:test');
const { collectActivity, createWeeklyReport, splitActivity } = require('./weekly-report.cjs');

const now = new Date('2026-10-07T08:00:00.000Z');
const context = { repo: { owner: 'example', repo: 'project' } };
const since = '2026-09-30T08:00:00.000Z';

function commit(sha, message = 'Fix a meaningful bug') {
  return {
    sha,
    html_url: `https://github.com/example/project/commit/${sha}`,
    author: { login: 'developer' },
    commit: { message, author: { name: 'Developer' } },
  };
}

function pr(number, merged_at = '2026-10-06T00:00:00Z') {
  return {
    number, merged_at, updated_at: '2026-10-06T00:00:00Z',
    merge_commit_sha: `merge-${number}`,
    html_url: `https://github.com/example/project/pull/${number}`,
    title: `Feature ${number}`, user: { login: 'developer' },
  };
}

function fixture({ prPages = [[]], prCommits = {}, branchCommits = {}, files = {} } = {}) {
  const calls = [];
  const pulls = { list: Symbol('pulls.list'), listCommits: Symbol('pulls.listCommits') };
  const repos = {
    listBranches: Symbol('repos.listBranches'),
    listCommits: Symbol('repos.listCommits'),
    async getCommit(params) {
      calls.push(['getCommit', params]);
      const allFiles = files[params.ref] || [{
        filename: 'src/feature.js', status: 'modified', additions: 1, deletions: 1,
      }];
      return { data: {
        stats: { additions: 1, deletions: 1 },
        files: allFiles.slice((params.page - 1) * params.per_page, params.page * params.per_page),
      } };
    },
  };
  const issues = {
    async create(params) {
      calls.push(['create', params]);
      return { data: { number: 42 } };
    },
    async createComment(params) { calls.push(['createComment', params]); },
    async update(params) { calls.push(['update', params]); },
  };
  const paginate = async (endpoint, params) => {
    calls.push(['paginate', endpoint, params]);
    if (endpoint === pulls.listCommits) return prCommits[params.pull_number] || [];
    if (endpoint === repos.listBranches) {
      return Object.keys(branchCommits).map(name => ({ name, commit: { sha: `tip-${name}` } }));
    }
    if (endpoint === repos.listCommits) {
      assert.equal(params.since, since);
      assert.equal(params.until, now.toISOString());
      return branchCommits[params.sha.slice(4)];
    }
    assert.fail('Unexpected endpoint');
  };
  paginate.iterator = async function* (endpoint, params) {
    assert.equal(endpoint, pulls.list);
    assert.equal(params.sort, 'updated');
    for (const data of prPages) {
      calls.push(['prPage']);
      yield { data };
    }
  };
  return { github: { rest: { pulls, repos, issues }, paginate }, context, now, calls };
}

test('includes direct work from every branch, deduplicates SHAs, and excludes merged PR work', async () => {
  const shared = commit('shared', 'Small fix\n\nImportant explanation');
  const args = fixture({
    prPages: [[pr(1)]],
    prCommits: { 1: [commit('covered')] },
    branchCommits: {
      master: [shared, commit('covered'), commit('merge-1')],
      feature: [shared, commit('feature-only')],
    },
  });
  const activity = await collectActivity(args);
  assert.match(activity, /#1.*Feature 1/);
  assert.equal(activity.split('](https://github.com/example/project/commit/shared)').length - 1, 1);
  assert.match(activity, /branches: master, feature/);
  assert.match(activity, /feature-only/);
  assert.match(activity, /Small fix Important explanation/);
  assert.match(activity, /src\/feature\.js \(modified, \+1\/-1\)/);
  assert.doesNotMatch(activity, /\/commit\/covered|\/commit\/merge-1/);
  assert.equal(args.calls.filter(([name]) => name === 'getCommit').length, 2);
});

test('reads later PR pages and stops after the update-date cutoff', async () => {
  const old = { ...pr(3, '2026-09-01T00:00:00Z'), updated_at: '2026-09-02T00:00:00Z' };
  const args = fixture({
    prPages: [[pr(1)], [pr(2), old], [pr(4)]],
  });
  const activity = await collectActivity(args);
  assert.match(activity, /#1/);
  assert.match(activity, /#2/);
  assert.doesNotMatch(activity, /#3|#4/);
  assert.equal(args.calls.filter(([name]) => name === 'prPage').length, 2);
});

test('excludes unmerged, old, and future PRs', async () => {
  const args = fixture({
    prPages: [[pr(1, null), pr(2, '2026-09-01T00:00:00Z'), pr(3, '2026-10-08T00:00:00Z')]],
  });
  assert.match(await collectActivity(args), /No PRs merged this week/);
});

test('includes both window boundaries regardless of timestamp precision', async () => {
  const args = fixture({
    prPages: [[pr(1, '2026-09-30T08:00:00Z'), pr(2, '2026-10-07T08:00:00Z')]],
  });
  const activity = await collectActivity(args);
  assert.match(activity, /#1/);
  assert.match(activity, /#2/);
});

test('paginates changed files and retains commits without a GitHub author', async () => {
  const direct = { ...commit('direct'), author: null };
  const files = Array.from({ length: 101 }, (_, index) => ({
    filename: `src/file-${index}.js`, status: 'modified', additions: 1, deletions: 0,
  }));
  const args = fixture({ branchCommits: { feature: [direct] }, files: { direct: files } });
  const activity = await collectActivity(args);
  assert.match(activity, /Author: Developer/);
  assert.match(activity, /101 files/);
  assert.match(activity, /src\/file-100\.js/);
  assert.equal(args.calls.filter(([name]) => name === 'getCommit').length, 2);
});

test('reports an empty activity window explicitly', async () => {
  const activity = await collectActivity(fixture());
  assert.match(activity, /No PRs merged this week/);
  assert.match(activity, /No additional commits in this window/);
});

test('splits long activity without losing content or exceeding the limit', () => {
  for (const text of ['abcdefghi', 'abc\ndef\nghi', '\nabcdefghi']) {
    const chunks = splitActivity(text, 4);
    assert.ok(chunks.every(chunk => chunk.length <= 4));
    assert.equal(chunks.join('').replace(/\n/g, ''), text.replace(/\n/g, ''));
  }
});

test('publishes all evidence before the request and stays within GitHub body limits', async () => {
  const args = fixture({
    branchCommits: { feature: [commit('large', 'x'.repeat(130000))] },
  });
  await createWeeklyReport(args);
  const writes = args.calls.filter(([name]) => ['create', 'createComment', 'update'].includes(name));
  assert.equal(writes[0][0], 'create');
  assert.equal(writes.at(-1)[0], 'update');
  assert.ok(writes.length >= 4);
  assert.ok(writes.slice(1, -1).every(([name]) => name === 'createComment'));
  for (const [name, params] of writes) {
    assert.ok(params.body.length < 65536);
    assert.equal(params.body.includes('@copilot'), name === 'update');
  }
  const body = writes.at(-1)[1].body;
  assert.match(body, /actual changed code\/diffs/);
  assert.match(body, /Group related small commits/);
  assert.match(body, /under 500 words/);
  assert.match(body, /Unmerged|unmerged branch work/);
});

test('fails explicitly on API errors without creating an incomplete report', async () => {
  const args = fixture({ branchCommits: { feature: [commit('broken')] } });
  args.github.rest.repos.getCommit = async () => { throw new Error('API unavailable'); };
  await assert.rejects(createWeeklyReport(args), /API unavailable/);
  assert.ok(!args.calls.some(([name]) => name === 'create'));
});

test('does not publish a Copilot request when an activity comment fails', async () => {
  const args = fixture({
    branchCommits: { feature: [commit('large', 'x'.repeat(70000))] },
  });
  args.github.rest.issues.createComment = async () => { throw new Error('Comment failed'); };
  await assert.rejects(createWeeklyReport(args), /Comment failed/);
  assert.ok(!args.calls.some(([name]) => name === 'update'));
});
