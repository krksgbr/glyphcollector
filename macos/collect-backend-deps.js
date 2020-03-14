const fs = require("fs");
const path = require("path");
const { execSync } = require("child_process");

const { exe, outdir } = process.argv.slice(2).reduce((acc, it) => {
  const [key, value] = it.split("=");
  return {
    ...acc,
    [`${key.replace("--", "")}`]: value
  };
}, {});

if(process.platform !== "darwin"){
  const basename = path.basename(exe);
  const newPath = `${outdir}/${basename}`;
  fs.copyFileSync(exe, newPath);
  process.exit();
}


function otool(lib) {
  try {
    const cmd = `otool -L ${lib}`;
    const s = execSync(`otool -L ${lib}`);
    return s.toString("utf8");
  } catch (e) {
    return "";
  }
}

function getLinks(lib) {
  const links = otool(lib)
    .split("\n")
    .filter(it => !it.endsWith(":"))
    .map(it => it.trim())
    .map(it => it.split(" ")[0])
    .filter(it => it !== "")
  // .filter(it => it.includes("/nix/store"));
  return links;
}

function mkInstallNameCmd(link, file) {
  return `install_name_tool -change ${link} @executable_path/${path.basename(
    link
  )} ${file}`;
}

const done = [];
function gatherLibs(lib, acc = {}) {
  if (acc[lib]) {
    return acc;
  }

  const links = getLinks(lib);
  done.push(lib);

  const basename = path.basename(lib);
  const newPath = `${outdir}/${basename}`;

  const newAcc = {
    [lib]: {
      lib,
      links,
      newPath
    }
  };

  return links.reduce((acc, link) => {
    const more = done.includes(link) ? [] : gatherLibs(link, acc);
    return {
      ...acc,
      ...more
    };
  }, newAcc);
}

const todo = Object.values(gatherLibs(exe));
 
function rename(link, file) {
  const newLink = `@executable_path/${path.basename(link)}`;
  const cmd = `install_name_tool -change ${link} ${newLink} ${file}`;
  try {
    execSync(cmd);
    // console.log(`renamed ${link} to ${newLink} in ${file}`);
  } catch (e) {
    console.log("");
    console.log(`failed to rename ${link} in ${file}`);
    console.log(`${e}`);
    console.log("");
    throw e;
  }
}

const copied = todo.reduce((acc, it) => {
  const { lib } = it;
  if (acc.includes(lib)) {
    return acc;
  }
  try {
    console.log(`copying ${lib} to ${it.newPath}`);
    fs.copyFileSync(lib, it.newPath);
    return acc.concat(lib);
  } catch (e) {
    return acc;
  }
}, []);

execSync(`chmod -Rf 755 ${outdir}`);

const renamed = todo.reduce((acc, it) => {
  if (acc.includes(it.newPath)) {
    return acc;
  }

  try {
    it.links.forEach(link => {
      rename(link, it.newPath);
    });
  } catch (e) {
    return acc;
  }

  return acc.concat(it.newPath);
}, []);

// console.log({
//   copied,
//   renamed
// });
