const os = require("os");
const path = require("path");

function appDataPath(){
  const home = os.homedir();
  return process.platform === "win32"
    ? path.resolve(home, "AppData", "Roaming")
    : path.resolve(home, ".local", "share");
}

module.exports.appDataPath = appDataPath;

module.exports.repoPath = function repoPath() {
  return path.resolve(appDataPath(), "glyphcollector", "repo.json");
};
