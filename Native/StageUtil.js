Elm.Native.StageUtil = {};
Elm.Native.StageUtil.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.StageUtil = elm.Native.StageUtil || {};
  if (elm.Native.StageUtil.values) return elm.Native.StageUtil.values;

  var vals = {
    modFloat: F2(function(x,y){
      return x % y;
    })
  };

  return elm.Native.StageUtil.values = vals;
};

