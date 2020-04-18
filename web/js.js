function setRotation(elt, deg) {
  elt.setAttributeNS(null, 'transform', 'rotate(' + deg + ')');
}

function setAltitude(ft) {
  var hs = ft / 100.0 * 36.0 - 180.0;
  var ts = ft / 1000.0 * 36.0 - 180.0;
  var svg = document.getElementById("instruments");
  setRotation(svg.getElementById("hundreds"), hs)
  setRotation(svg.getElementById("thousands"), ts)
}

window.addEventListener("load", function() {
  setAltitude(2900);
})
