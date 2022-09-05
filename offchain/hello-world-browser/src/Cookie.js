"use strict";


exports._getCookie = function (key) {
  return function(just) {
    return function (nothing) {
      return function (onError, onSuccess) {
        try {
          const cookie = document.cookie.split('; ').find((row) => row.startsWith(`${key}=`))

          if (cookie) {
            var value = just(decodeURIComponent(cookie.split('=')[1]));
          } else {
            var value = nothing;
          }
        } catch (e) {
          onError(e);
        }

        onSuccess(value);

        return function (cancelError, onCancelerError, onCancelerSuccess) {
          onCancelerSuccess();
        };
      };
    };
  };
};
