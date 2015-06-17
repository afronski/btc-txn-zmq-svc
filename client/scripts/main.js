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

    function coordinatesChanged(socket, coordinates, rectangle, map) {
        var result = coordinates.reduce(function (object, input) {
            object[input.getAttribute("id")] = parseFloat(input.value);
            return object;
        }, {});

        socket.emit("coordinates-changed", result);

        var newBounds = new google.maps.LatLngBounds(
            new google.maps.LatLng(result["nw-corner-lat"], result["nw-corner-lng"]),
            new google.maps.LatLng(result["se-corner-lat"], result["se-corner-lng"])
        );

        rectangle.setBounds(newBounds);
    }

    function toggleState(button, socket) {
        if (button.value === "Start") {
            socket.emit("start");
            button.value = "Stop";
        } else {
            socket.emit("stop");
            button.value = "Start";
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
            toggleState(this, socket);
        });

        var coordinates = [
            document.getElementById("nw-corner-lat"),
            document.getElementById("nw-corner-lng"),
            document.getElementById("se-corner-lat"),
            document.getElementById("se-corner-lng")
        ];

        coordinates.forEach(function (input) {
            input.addEventListener("change", function () {
                coordinatesChanged(socket, coordinates, rectangle, map);
            });
        });

        coordinatesChanged(socket, coordinates, rectangle, map);
    }

    google.maps.event.addDomListener(window, "load", init);
} (window.io, window.google));
