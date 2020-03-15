const { shell } = require("electron");
const Elm = require("./elm/Main.elm").Elm;
const manifest = require("./manifest.json");

require("./resources/whois-mono/stylesheet.css");
require("./css/spinner.css");

const { PortFunnel } = require("./js/PortFunnel");
const mkWebSocketFunnel = require("./js/PortFunnel/WebSocket");

const appSettings = localStorage.getItem("appSettings");

const app = Elm.Main.init({
  node: document.getElementById("main"),
  flags: {
    manifest,
    appSettings: appSettings && JSON.parse(appSettings)
  }
});

console.log(app);

PortFunnel.subscribe(app);
mkWebSocketFunnel(PortFunnel);

// FileDrop
function sendWindowDragStart(e) {
  if (!!e.dataTransfer.files) {
    e.preventDefault();
    app.ports.windowDragStart.send(null);
  }
}

function sendWindowDragEnd() {
  app.ports.windowDragEnd.send(null);
}

app.ports.dropHandled.subscribe(function() {
  sendWindowDragEnd();
});

window.addEventListener("dragenter", sendWindowDragStart);
window.addEventListener("dragover", sendWindowDragStart);
window.addEventListener("dragleave", sendWindowDragEnd);
window.addEventListener("drop", sendWindowDragEnd);

app.ports.openExternalUrl.subscribe(url => {
  shell.openExternalSync(url);
});

app.ports.updateAppSettings.subscribe(newAppSettings => {
  localStorage.setItem("appSettings", JSON.stringify(newAppSettings));
  app.ports.appSettingsUpdated.send(newAppSettings);
});

// Remove elm debugger this hacky way, because parcel doesn't allow for configuring this
// there's no id on the element, so select it in this unsafe way
(function r() {
  const el = document.querySelector("[data-elm-hot='true']");
  if(el){
    const toRemove = Array.from(el.children)[1].remove();
    window.r = r;
  }
})();
