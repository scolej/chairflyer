function setRotation(elt, deg) {
  elt.setAttributeNS(null, 'transform', 'rotate(' + deg + ')');
}

function setAirspeed(knots) {
  var hs = (knots / 165) ** 2 * 360 - 180;
  var svg = document.getElementById("instruments");
  setRotation(svg.getElementById("knots"), hs)
}

function setAltitude(ft) {
  var hs = ft / 100.0 * 36.0 - 180.0;
  var ts = ft / 1000.0 * 36.0 - 180.0;
  var svg = document.getElementById("instruments");
  setRotation(svg.getElementById("hundreds"), hs)
  setRotation(svg.getElementById("thousands"), ts)
}

function trySocket() {
  var s = new WebSocket("ws://127.0.0.1:8000", "protocolOne");
  s.onmessage = function (event) {
      console.log(event.data);
      var j = JSON.parse(event.data);
      setAltitude(j.rAltitude)
      setAirspeed(j.rAirspeed)
  }
}

window.addEventListener("load", function() {
  setAltitude(2900);
  trySocket()

  var map = L.map('map', {
    zoomControl: false,
    dragging: false,
    keyboard: false,
    scrollWheelZoom: false,
    center: [-37.69844,145.36649],
    zoom: 16,
    layers: [
      L.tileLayer(
        // 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        // FIXME Check license!!
        'https://tiles.wmflabs.org/osm-no-labels/{z}/{x}/{y}.png', {
          maxZoom: 19,
        }
      )
    ]
  });
})