var iconFeature = new ol.Feature({
    geometry: new ol.geom.Point(ol.proj.fromLonLat([145.365335, -37.698329])),
});

var icon = new ol.style.Icon({
    anchor: [0.5, 0.1],
    anchorXUnits: 'fraction',
    anchorYUnits: 'fraction',
    src: './plane.png'
})

var iconStyle = new ol.style.Style({
    image: icon
});
iconFeature.setStyle(iconStyle);

var vectorSource = new ol.source.Vector({
    features: [iconFeature]
});

var vectorLayer = new ol.layer.Vector({
    source: vectorSource
});

//
//
//

var map = new ol.Map({
    layers: [
        new ol.layer.Tile({
            source: new ol.source.BingMaps({
                // FIXME
                key: 'AovsTgObCx5ly8HLRLD-BGdJADMKIHGHa8Z9sAx9ld1vsgZuE4v1FH6e6OBcar8V',
                imagerySet: 'Aerial',
                maxZoom: 19
            }),
        }),
        vectorLayer,
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

//
//
//

function setRotation(elt, deg) {
    elt.setAttributeNS(null, 'transform', 'rotate(' + deg + ')');
}

function setTranslate(elt, x, y) {
    elt.setAttributeNS(null, 'transform', 'translate(' + x + ',' + y + ')');
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

function setTacho(rpm) {
    var deg = (rpm / 4000 * 0.8 + 0.1) * 360;
    var svg = document.getElementById("instruments");
    setRotation(svg.getElementById("hand"), deg)
}

function setMagneticCompass(rad) {
    var deg = rad * 180 / Math.PI;
    var x = (deg - 360) * 1.3;
    var svg = document.getElementById("instruments");
    setTranslate(svg.getElementById("compassStrip"), x, 0)
}

function showInstruments() {
    document.getElementById("instruments").style.display = 'block';
}

function hideInstruments() {
    document.getElementById("instruments").style.display = 'none';
}

var mapRot = 0;
window.addEventListener("load", function() {

    setAirspeed(0);
    setAltitude(0);

    // FIXME don't hardcode server location
    var s = new WebSocket("ws://127.0.0.1:8000", "protocolOne");
    s.onmessage = function (event) {
        var j = JSON.parse(event.data);

        setAltitude(j.rAltitude);
        setAirspeed(j.rAirspeed);
        setTacho(j.rRpm);
        setMagneticCompass(j.rHeadingMagRad);

        var pos = ol.proj.fromLonLat([j.rLatLon[1], j.rLatLon[0]]);

        var z = 15 - 3 / 10000 * j.rAltitude;

        mapRot = j.rHeadingRad;
        map.setView(new ol.View({
            center: pos,
            rotation: -mapRot,
            zoom: z,
        }));

        iconFeature.setGeometry(new ol.geom.Point(pos));
        icon.setRotation(j.rHeadingRad - mapRot)

        // FIXME should allow tapping on map for pitch & heading
    }

    document.getElementById("map").addEventListener("click", function(event) {
        var x = event.x - event.target.width / 2;
        var y = event.y - event.target.height / 2;
        var theta = Math.atan2(x, -y);
        console.log("click: " + x + " " + y + " " + theta);
        s.send(JSON.stringify({
            tag:"Turn",
            contents:theta / Math.PI * 180,
        }));
    })

    document.addEventListener("keydown", function(event) {
        var cmd;
        if (event.key === "ArrowLeft") {
            cmd = {tag:"Turn",contents:-1};
        } else if (event.key === "ArrowRight") {
            cmd = {tag:"Turn",contents:1};
        } else if (event.key === "1") {
            cmd = {tag:"AdoptConfiguration", contents:"Landed"};
        } else if (event.key === "2") {
            cmd = {tag:"AdoptConfiguration", contents:"Climb"};
        } else if (event.key === "3") {
            cmd = {tag:"AdoptConfiguration", contents:"Cruise"};
        } else if (event.key === "4") {
            cmd = {tag:"AdoptConfiguration", contents:"Descent"};
        } else if (event.key === "f") {
            cmd = {tag:"FastForward", contents: 60 * 10};
        }
        s.send(JSON.stringify(cmd));
    });
})
