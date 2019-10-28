const path = require("path");

module.exports = {
  packagerConfig: {
    icon: (function() {
      switch (process.platform) {
        case "darwin":
          return "./icons/icon.icns";

        case "win32":
          return "./icons/icon.ico";

        case "linux":
          // option is not supported
          // https://github.com/electron/electron-packager/blob/master/docs/api.md#icon
          return undefined;

        default:
          return undefined;
      }
    })()
  },
  makers: [
    {
      name: "@electron-forge/maker-squirrel",
      config: {
        name: "GlyphCollector"
      }
    },
    {
      name: "@electron-forge/maker-zip",
      platforms: ["darwin"]
    },
    {
      name: "@electron-forge/maker-deb",
      config: {}
    }
  ]
};
