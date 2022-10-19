"use strict";

exports._getNetworkId = (conn) => conn.getNetworkId;

const getIsWalletAvailableFunctionName = (wallet) => {
  const strs = {
    nami: "isNamiWalletAvailable",
    gerowallet: "isGeroWalletAvailable",
    flint: "isFlintWalletAvailable",
    LodeWallet: "isLodeWalletAvailable",
    eternl: "isEternlWalletAvailable",
  };

  return strs[wallet] || "is?WalletAvailable";
};

const nodeEnvError = new Error(
  "`window` is not an object. Are you trying to run a Contract with" +
    " connected light wallet in NodeJS environment?"
);

const checkNotNode = () => {
  if (typeof window != "object") {
    throw nodeEnvError;
  }
};

const isWalletAvailable = (walletName) => () => {
  checkNotNode();
  return (
    typeof window.cardano != "undefined" &&
    typeof window.cardano[walletName] != "undefined" &&
    typeof window.cardano[walletName].enable == "function"
  );
};

exports._enable = (wallet) => () => {
  const isAvailable = isWalletAvailable(wallet)();
  if (isAvailable) {
    return window.cardano[wallet].enable().catch((e) => {
      throw new Error(
        "enable failed: " + (typeof e.info == "string" ? e.info : e.toString())
      );
    });
  } else {
    throw new Error(
      "Wallet is not available. Use `" +
        getIsWalletAvailableFunctionName(wallet) +
        "` before connecting."
    );
  }
};
