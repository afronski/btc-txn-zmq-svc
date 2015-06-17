(function (io, google) {
    "use strict";

    var europe = new google.maps.LatLngBounds(
        new google.maps.LatLng(71.0, -25.0),
        new google.maps.LatLng(32.0, 40.0)
    );

    function marker(options) {
        return new google.maps.Marker(options);
    }

    function latlng(lat, lng) {
        return new google.maps.LatLng(lat, lng);
    }

    function coordinatesChanged(socket, coordinates) {
        var result = coordinates.reduce(function (object, input) {
            object[input.getAttribute("id")] = parseFloat(input.value);
            return object;
        }, {});

        socket.emit("coordinates-changed", result);
    }

    function toggleState(button, socket) {
        if (button.innerText === "Start") {
            socket.emit("start");
            button.innerText = "Stop";
        } else {
            socket.emit("stop");
            button.innerText = "Start";
        }
    }

    function init() {
        var options = {
            zoom: 3,
            center: europe.getCenter(),
            mapTypeId: google.maps.MapTypeId.TERRAIN
        };

        var map = new google.maps.Map(document.getElementById("map"), options);
        map.fitBounds(europe);

        var rectangle = new google.maps.Rectangle({
            strokeColor: "#800000",
            strokeOpacity: 0.8,
            strokeWeight: 2,

            fillColor: "#800000",
            fillOpacity: 0.15,

            map: map,
            bounds: europe
        });

        var socket = io();
        var markers = [];

        socket.on("transaction", function (data) {
            markers.push(marker({
                map: map,
                title: data.hash,
                position: latlng(data.lat, data.lng)
            }));

            if (markers.length > 100) {
                markers.shift();
            }
        });

        var button = document.getElementById("toggle-state");

        button.addEventListener("click", function () {
            toggleState.call(this, this, socket);
        });

        var coordinates = [
            document.getElementById("nw-corner-lat"),
            document.getElementById("nw-corner-lng"),
            document.getElementById("se-corner-lat"),
            document.getElementById("se-corner-lng")
        ];

        coordinates.forEach(function (input) {
            input.addEventListener("change", function () {
                coordinatesChanged.call(this, socket, coordinates);
            });
        });
    }

    google.maps.event.addDomListener(window, "load", init);
} (window.io, window.google));
