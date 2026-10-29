// Shared language dictionary, browser side. Same rules as R/i18n.R:
// named placeholders ({n}), plurals as "<key>.uno" / "<key>.otros", and a
// missing key returns the key itself (logged once) instead of breaking the UI.
(function() {
  var warned = {};

  window.hdT = function(key, vars) {
    var dic = window.HD_I18N || {};
    var txt = dic[key];
    if (txt == null) {
      if (!warned[key]) { warned[key] = true; console.warn('[i18n] missing key:', key); }
      return key;
    }
    if (vars) {
      Object.keys(vars).forEach(function(k) {
        txt = txt.split('{' + k + '}').join(String(vars[k]));
      });
    }
    return txt;
  };

  window.hdTn = function(key, n, vars) {
    var v = Object.assign({ n: n }, vars || {});
    return window.hdT(key + (Number(n) === 1 ? '.uno' : '.otros'), v);
  };
})();
