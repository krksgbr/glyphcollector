const Elm = require("./elm/Main.elm").Elm;

require("./resources/whois-mono/stylesheet.css");
require("./css/spinner.css");

const { PortFunnel } = require("./js/PortFunnel");
const mkWebSocketFunnel = require("./js/PortFunnel/WebSocket");


const app = Elm.Main.init({
  node: document.getElementById('main'),
});

PortFunnel.subscribe(app);
mkWebSocketFunnel(PortFunnel);

// FileDrop
function sendWindowDragStart(e){
    if(!!e.dataTransfer.files){
       e.preventDefault();
       app.ports.windowDragStart.send(null);
    }
}

function sendWindowDragEnd(){
   app.ports.windowDragEnd.send(null);
}

app.ports.dropHandled.subscribe(function(){
    sendWindowDragEnd();
});

window.addEventListener("dragenter", sendWindowDragStart);
window.addEventListener("dragover", sendWindowDragStart);
window.addEventListener("dragleave", sendWindowDragEnd);
window.addEventListener("drop", sendWindowDragEnd);

if(process.env.NODE_ENV === "development"){
  // Remove elm debugger this hacky way, because parcel doesn't allow for configuring this
  // there's no id on the element, so select it in this unsafe way
  (function r(){
     const el = document.querySelector("[data-elm-hot='true']");
     const toRemove = Array.from(el.children)[1].remove();
     window.r = r;
  })();
}
