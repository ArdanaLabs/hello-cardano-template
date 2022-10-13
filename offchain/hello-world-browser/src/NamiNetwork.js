"use strict";

exports._getNetworkId = () =>
  window.cardano
    .getNetworkId()
    .then((networkId) => {
      return networkId;
    })
    .catch((e) => {
      console.log("Error in getNetworkId: ", e);
    });
