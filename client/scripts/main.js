(function (io) {
    "use strict";

    var socket = io();

    socket.on("socket-connected", function (data) {
        console.info(data);
    });
} (window.io));
