Elm.Native.PSignalUtil = {};
Elm.Native.PSignalUtil.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.PSignalUtil = elm.Native.PSignalUtil || {};
  if (elm.Native.PSignalUtil.values) return elm.Native.PSignalUtil.values;

  var vals = {
    modFloat: F2(function(x,y){
      return x % y;
    })
  };

  return elm.Native.PSignalUtil.values = vals;
};

