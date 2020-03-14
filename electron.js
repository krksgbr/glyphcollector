const { app, BrowserWindow } = require("electron");
const path = require("path");
const { spawn } = require("child_process");
const windowsInstalling = require("electron-squirrel-startup");
const patchRepo = require("./patch-repo");


// Handle creating/removing shortcuts on Windows when installing/uninstalling.
if (windowsInstalling) {
  // eslint-disable-line global-require
  return app.quit();
}

patchRepo();


const dist = (file) => path.resolve(__dirname, "dist", file);

const devConfig = {
  url: `file://${dist("index.html")}`,
};

const prodConfig = {
  url: `file://${dist("index.html")}`,
  startBackend: [
	  dist("gc-core" + (process.platform === "win32" ? ".exe" : "")),
	  "+RTS", "-N"
  ]
};

const config = app.isPackaged && typeof process.env.DEVELOPMENT === "undefined" ? prodConfig : devConfig;


// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow;
let backend;

const startBackend = (sb) => {
    console.log("starting backend:", sb);
    const [cmd, ...args] = sb;
    backend = spawn(cmd, args);
    backend.stdout.on("data", data => {
      console.log("GlyphCollector stdout: ", data.toString("utf8"));
    });

    backend.stderr.on("data", data => {
      console.log("GlyphCollector stderr: ", data.toString("utf8"));
    });

    backend.on("close", code => {
      console.log("GlyphCollector backend exited with", code);
      app.quit();
    });
};

const createWindow = () => {
  // Create the browser window.
  mainWindow = new BrowserWindow({
    show: false,
    webPreferences: {
      webSecurity: false,
      nodeIntegration: true
    }
  });
  
  mainWindow.maximize();
  mainWindow.show();

  // and load the index.html of the app.
  // mainWindow.loadURL(`file://${__dirname}/index.html`);
  mainWindow.loadURL(config.url);

  // Open the DevTools.
  // mainWindow.webContents.openDevTools();

  // Emitted when the window is closed.
  mainWindow.on("closed", () => {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    // mainWindow = null;
    stop();
  });
};

const start = () => {
    if (config.startBackend) {
      console.log(config);
      startBackend(config.startBackend);
    }
    createWindow();
}

const stop = () => {
  console.log("GlyphCollector stopping");
  if(backend){
     backend.kill();
     backend = null;
  }
  app.quit();
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on("ready", start);

// Quit when all windows are closed.
app.on("window-all-closed", () => {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== "darwin") {
    stop();
  }
});

app.on("activate", () => {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
      start();
  }
});

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and import them here.
