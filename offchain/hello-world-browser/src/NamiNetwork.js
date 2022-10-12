exports._getNetworkId = function (onError, onSuccess) {
  try {
    window.cardano.getNetworkId().then((networkId) => {
      onSuccess(networkId);
    });
  } catch (e) {
    onError(e);
  }

  return function (cancelError, onCancelerError, onCancelerSuccess) {
    onCancelerSuccess();
  };
};
