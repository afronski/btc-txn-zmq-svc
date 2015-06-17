# `btc-txn-zmq-svc`

## What?

**This is a *pet project***.

Sample application which is a demonstration of usage for `ZeroMQ` in :

- *Erlang*
- *Node.js*

It was used as a sample project for my talk at *Berlin.js* at *18th June 2015*.

## Overview

![Screenshot of UI](docs/screenshot.png)

Basically it is a very simple `UI` which contains map, preselected, rectangular region and controls. You can redefine a region with the coordinates (first row it is a *North-West* corner and second row is a *South-East* corner) and you can toggle the simulation state (*started* / *stopped*).

Underneath it is a *server* and *client* which uses *ZeroMQ* library as a communication mechanism. *Server* is written in *Erlang*, client uses *Node.js*. This application structure can be approximated as a following diagram:

![Architecture diagram](docs/diagram.png)
