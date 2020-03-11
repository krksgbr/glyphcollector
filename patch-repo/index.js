// With an update to elm-street data is serialized differently,
// so GlyphCollector can't open repos serialized with older versions.
// This script patches old repos to make them compatible with new versions
// of GlyphCollector.

const utils = require("./utils");
const fs = require("fs");

module.exports = function() {
  const repoPath = utils.repoPath();
  if (fs.existsSync(repoPath)) {
    const repo = require(repoPath);
    const newRepo = require("./v1-v2").run(repo);
    fs.copyFileSync(repoPath, `${repoPath}.${repo.version}.backup`);
    fs.writeFileSync(repoPath, JSON.stringify(newRepo));
    console.log("patched repo");
  }
};
