#!/usr/bin/env node

const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync, spawn } = require('child_process');

// Background mode: fetch the issue title and write it to the cache
// Arguments: <ref> <dir> <cacheFile>
const FETCH_FLAG = '--fetch-issue-title';
if (process.argv[2] === FETCH_FLAG) {
  fetchIssueTitle(process.argv[3], process.argv[4], process.argv[5]);
  return;
}

// Read JSON from stdin
let input = '';
process.stdin.on('data', chunk => input += chunk);
process.stdin.on('end', () => {
  try {
    const data = JSON.parse(input);

    // Extract values
    const model = data.model?.display_name || 'Unknown';
    const dir = data.workspace?.current_dir || data.cwd || '';
    const currentDir = path.basename(dir || '.');

    // Gauges
    const cwPct = Math.round(data.context_window?.used_percentage || 0);
    const fiveHourPct = Math.round(data.rate_limits?.five_hour?.used_percentage || 0);
    const sevenDayPct = Math.round(data.rate_limits?.seven_day?.used_percentage || 0);
    const gaugeDisplay = [
      ['cw', cwPct],
      ['5h', fiveHourPct],
      ['7d', sevenDayPct],
    ].map(([label, pct]) => `${label} ${formatGauge(pct)}`).join(' | ');

    // Build status line
    const sessionName = data.session_name ? ` | 💬 ${sessionLabel(data, dir)}` : '';
    const statusLine = `⚡ ${model} | 📁 ${currentDir} | ${gaugeDisplay}${sessionName}`;

    console.log(statusLine);
  } catch (error) {
    // Fallback status line on error
    console.log('[Error] 📁 .');
  }
});

const colorByPercentage = (text, pct) => {
  if (pct >= 90) return `\x1b[31m${text}\x1b[0m`;
  if (pct >= 70) return `\x1b[33m${text}\x1b[0m`;
  return `\x1b[32m${text}\x1b[0m`;
};

const pieIcon = (pct) => {
  if (pct >= 75) return '●';
  if (pct >= 50) return '◕';
  if (pct >= 25) return '◑';
  if (pct > 0)   return '◔';
  return '○';
};

const formatGauge = (pct) => {
  const icon = pieIcon(pct);
  return colorByPercentage(`${icon} ${pct}%`, pct);
};

// Shows the issue title if available, otherwise the session name
function sessionLabel(data, dir) {
  return stripControlChars(issueLabel(data, dir) || data.session_name);
}

// While a fetch is in progress or after it failed, wait this long before retrying
const RETRY_INTERVAL_MS = 5 * 60 * 1000;

// Returns "#123 <title>" or "owner/repo#123 <title>" if the session name
// contains an issue number and its title is cached for the current directory.
// Otherwise starts a background fetch and returns null. The fetch runs
// detached because Claude Code cancels a slow statusline script.
function issueLabel(data, currentDir) {
  const ref = issueRef(data.session_name);
  const sessionId = data.session_id || '';
  if (!ref || !/^[\w-]+$/.test(sessionId)) return null;

  // gh resolves the repo from the directory only when the ref has no owner/repo
  const dir = ref.startsWith('#') ? currentDir : '';
  const cacheFile = path.join(os.tmpdir(), `claude-statusline-issue-${sessionId}.json`);
  const cached = readCache(cacheFile);
  if (cached?.ref === ref && cached.dir === dir) {
    if (cached.title) return `${ref} ${cached.title}`;
    if (Date.now() - cached.startedAt < RETRY_INTERVAL_MS) return null;
  }

  try {
    // Mark as in progress so that later updates don't spawn another fetch
    writeCache(cacheFile, { ref, dir, title: null, startedAt: Date.now() });
    const child = spawn(process.execPath, [__filename, FETCH_FLAG, ref, dir, cacheFile], {
      cwd: dir || undefined,
      detached: true,
      stdio: 'ignore',
    });
    child.on('error', () => {});
    child.unref();
  } catch (error) {
    // Ignore: fall back to the session name
  }
  return null;
}

// Builds "owner/repo#123" or "#123" from the first "#123" and the first
// standalone "owner/repo" in the session name, e.g. "Issue #187 (masutaka/dotfiles)".
// Paths with two or more slashes such as "cookbooks/home/files" are not repos.
function issueRef(sessionName) {
  const number = sessionName.match(/#(\d+)/)?.[1];
  if (!number) return null;
  const repo = sessionName.match(/(?<![\w./-])[\w.-]+\/[\w.-]+(?![\w./-])/)?.[0] || '';
  return `${repo}#${number}`;
}

// Removes C0/C1 control characters so the label can't inject escape sequences
function stripControlChars(text) {
  return text.replace(/[\x00-\x1f\x7f-\x9f]/g, '');
}

function readCache(cacheFile) {
  try {
    return JSON.parse(fs.readFileSync(cacheFile, 'utf8'));
  } catch (error) {
    return null;
  }
}

// Writes atomically so the statusline never reads a partial file
function writeCache(cacheFile, value) {
  const tmpFile = `${cacheFile}.${process.pid}`;
  fs.writeFileSync(tmpFile, JSON.stringify(value), { mode: 0o600 });
  fs.renameSync(tmpFile, cacheFile);
}

function fetchIssueTitle(ref, dir, cacheFile) {
  try {
    const [repo, number] = ref.split('#');
    const args = ['issue', 'view', number, '--json', 'title', '-q', '.title'];
    if (repo) args.push('--repo', repo);
    const title = execFileSync('gh', args, {
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'ignore'],
      timeout: 10000,
    }).trim();
    if (title) writeCache(cacheFile, { ref, dir, title });
  } catch (error) {
    // Ignore: the in-progress mark stays, so the next fetch waits for the retry interval
  }
}
