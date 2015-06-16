(function (io, google) {
    "use strict";

    var europe = new google.maps.LatLngBounds(
        new google.maps.LatLng(65.0, -12.0),
        new google.maps.LatLng(42.0, 30.0)
    );

    function marker(options) {
        return new google.maps.Marker(options);
    }

    function latlng(lat, lng) {
        return new google.maps.LatLng(lat, lng);
    }

    function init() {
        var options = {
            center: europe.getCenter(),
            mapTypeId: google.maps.MapTypeId.ROADMAP,
            zoom: 3
        };

        var map = new google.maps.Map(document.getElementById("map"), options);
        map.fitBounds(europe);

        var socket = io();
        var markers = [];

        socket.on("transaction", function (data) {
            markers.push(marker({
                position: latlng(data.lat, data.lng),
                title: data.hash,
                map: map
            }));

            if (markers.length > 100) {
                markers.shift();
            }
        });
    }

    google.maps.event.addDomListener(window, "load", init);
} (window.io, window.google));
