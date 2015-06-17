"use strict";

const path = require("path");

const zmq = require("zmq");

const subscriber = zmq.socket("sub");
const requester = zmq.socket("req");

const Hapi = require("hapi");
const web = new Hapi.Server();

web.connection({
    host: "0.0.0.0",
    port: Number(process.env.PORT || 8080)
});

const socketio = require("socket.io");
const io = socketio(web.listener);

web.route([
    { method: "GET", path: "/", handler: { view: "index.html" } },
    { method: "GET", path: "/{file*}", handler: { directory: { path: __dirname } } },
    { method: "GET", path: "/styles/{file*}", handler: { directory: { path: path.join(__dirname, "styles") } } },
    { method: "GET", path: "/scripts/{file*}", handler: { directory: { path: path.join(__dirname, "scripts") } } }
]);

web.views({
    engines: { html: require("handlebars") },
    path: path.join(__dirname, "templates")
});

//--------------------------
//-- ZeroMQ related code. --
//--------------------------

// 1. TCP based PUB-SUB.

// Subscribe on all messages.
subscriber.subscribe("");

// Handle each message individually - filtering by type.
subscriber.on("message", function (data) {
    let message = JSON.parse(data);

    if (message.type === "transaction") {
        console.info("Received transaction: %j,", message);
        io.emit("transaction", message);
    }
});

// Connecting to socket.
subscriber.connect(process.env.PUBLISHER_ADDRESS || "tcp://localhost:9002");

// 2. TCP based REQ-REP.

// Handling response individually.
requester.on("message", function (data) {
    console.info("Received response: %s", data);
});

// Connecting to socket.
requester.connect(process.env.REQUESTER_ADDRESS || "tcp://localhost:10020");

// Handling events from 'socket.io'.
io.on("connection", function (socket) {
    socket.on("start", function () {
        requester.send("toggle-state:start");
    });

    socket.on("stop", function () {
        requester.send("toggle-state:stop");
    });

    socket.on("coordinates-changed", function (result) {
        requester.send("coordinates-changed:" + JSON.stringify(result));
    });

    socket.on("disconnect", function () {
        console.info("User disconnected.");
    });
});

// Starting the web service.
web.start();

