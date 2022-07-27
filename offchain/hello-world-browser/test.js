"use strict";

import("./output/Test.Main/index.js").then((m) => {
  console.log("app starting");
  m.main();
});