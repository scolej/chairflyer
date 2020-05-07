var icon = new ol.Feature({
    geometry: new ol.geom.Point(ol.proj.fromLonLat([145.365335, -37.698329])),
});

var iconStyle = new ol.style.Style({
    image: new ol.style.Icon({
        anchor: [0.5, 0.1],
        anchorXUnits: 'fraction',
        anchorYUnits: 'fraction',
        src: './plane.png'
    })
});
icon.setStyle(iconStyle);

var vectorSource = new ol.source.Vector({
  features: [icon]
});

var vectorLayer = new ol.layer.Vector({
  source: vectorSource
});

/////

var map = new ol.Map({
    layers: [
        new ol.layer.Tile({
            source: new ol.source.OSM({
                crossOrigin: null,
                // FIXME terms of use?
                url: "https://tiles.wmflabs.org/osm-no-labels/{z}/{x}/{y}.png"
            })
        }),
        vectorLayer
    ],
    target: 'map',
    view: new ol.View({
        center: ol.proj.fromLonLat([145.365335, -37.698329]),
        rotation: Math.PI / 180 * -8,
        zoom: 13.2,
    }),
    interactions: [],
    controls: []
});

/////

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

var i = 0;
window.addEventListener("load", function() {

    setAirspeed(0);
    setAltitude(0);

    var mapRot = 0;

    var s = new WebSocket("ws://127.0.0.1:8000", "protocolOne");
    s.onmessage = function (event) {
        i += 1;
        var j = JSON.parse(event.data);
        setAltitude(j.rAltitude)
        setAirspeed(j.rAirspeed)

        // FIXME would be great if these adjustments could be a bit smoother
        // maybe have a little plane which jumps
        // but the map smoothly chases the plane
        var pos = ol.proj.fromLonLat([j.rLatLon[1], j.rLatLon[0]])


        if (i % 15 == 0) {
            mapRot = -j.rHeadingRad;
            var z = 14 - 3 / 10000 * j.rAltitude;

            map.getView().setCenter(pos);
            map.getView().setZoom(z);
            map.getView().setRotation(mapRot);

            // map.getView().animate({
            //     center: pos,
            //     rotation: mapRot,
            //     zoom: z,
            //     duration: 1000
            // });
        }

        icon.setGeometry(new ol.geom.Point(pos));
        iconStyle.getImage().setRotation(mapRot + j.rHeadingRad);

        // FIXME should allow tapping on map for pitch & heading
    }

    document.addEventListener("keydown", function(event) {
        if (event.key === "ArrowLeft") {
            s.send("l5");
            i = 0;
        }
        if (event.key === "ArrowRight") {
            i = 0;
            s.send("r5");
        }
        if (event.key === "ArrowUp") {
            s.send("pd1");
        }
        if (event.key === "ArrowDown") {
            s.send("pu1");
        }
    });

    document.getElementById("inputBox").addEventListener("keyup", function(event) {
        if (event.key === "Enter") {
            s.send(this.value);
            this.value = "";
        }
    });
})
