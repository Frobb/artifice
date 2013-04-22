var gamejs = require('gamejs');

var creature = require('./creature');

// Message handlers and senders ------------------------------------------------

var handlers = {};

handlers["log"] = function() { /* suppress */ };
handlers["creature_add"] = handleCreatureAdd;
handlers["creature_move"] = handleCreatureMove;
handlers["creature_remove"] = handleCreatureRemove;

function handleCreatureAdd(payload) {
    var c = new creature.Creature(payload.cid, payload.pos);
    creatures[c.cid] = c;    
}

function handleCreatureMove(payload) {
    if (payload.cid in creatures) {
        creatures[payload.cid].pos = payload.pos;
    } else {
        handleCreatureAdd(payload);
    }
}

function handleCreatureRemove(payload) {
    delete creatures[payload.cid];
}

function sendMove(x, y) {
    sendMsg('move', { 'x': x, 'y': y });
}

function sendCreatureAdd(x, y) {
    sendMsg('creature_add', {'x': x, 'y': y });
}

function sendMsg(type, payload) {
    var json = JSON.stringify({ 'type': type, 'payload': payload });
    socket.send(json);
}

// WebSocket callbacks ---------------------------------------------------------

if (!('WebSocket' in window)) {
    alert("No WebSocket support detected. Get a real browser.");
    return;
}

var socket = new WebSocket('ws://127.0.0.1:8080/');

socket.onmessage = function(evt) {
    var msg = JSON.parse(evt.data);
    var handler = handlers[msg.type];
    if (handler !== undefined) {
        handler(msg.payload);
    } else {
        alert("Unknown message type: " + msg.type + ".");
    }
}

// GameJS setup ----------------------------------------------------------------

var MAP_WIDTH = 640;
var MAP_HEIGHT = 480;
var TILE_SIZE = 32; //px

var creatures = {};
var camera = { x: 0, y: 0 };
var moveState = { up: 0, left: 0, down: 0, right: 0 };
var moveSpeed = 5.0;
var moveMult = 1.0;

gamejs.preload(['../sprites/grass.png']);

gamejs.ready(function() {
    var display = gamejs.display.setMode([MAP_WIDTH, MAP_HEIGHT]);

    gamejs.onEvent(function(event) {
        if (event.type === gamejs.event.KEY_DOWN ||
            event.type === gamejs.event.KEY_UP) {
            var change = event.type === gamejs.event.KEY_DOWN ? 1 : 0;
            if (event.key === gamejs.event.K_w) moveState.up = change;
            else if (event.key === gamejs.event.K_s) moveState.down = change;
            else if (event.key === gamejs.event.K_a) moveState.left = change;
            else if (event.key === gamejs.event.K_d) moveState.right = change;
            // Soup3r s3kr3t turb0 m0d3.
            else if (event.key === gamejs.event.K_SHIFT) {
                moveMult = event.type === gamejs.event.KEY_DOWN ? 10.0 : 1.0;
            }
        } else if (event.type === gamejs.event.MOUSE_UP) {
            var cellX = Math.floor(camera.x + event.pos[0] / TILE_SIZE);
            var cellY = Math.floor(camera.y + event.pos[1] / TILE_SIZE);
            console.log(cellX);
            sendCreatureAdd(cellX, cellY);
        }
    });

    var grass = gamejs.image.load('../sprites/grass.png');
    gamejs.onTick(function(msDuration) {
        drawDebug();
        sendMove(Math.floor(camera.x), Math.floor(camera.y));

        var dt = msDuration / 1000;
        camera.x += (-moveState.left + moveState.right) * moveSpeed * moveMult * dt;
        camera.y += (-moveState.up + moveState.down) * moveSpeed * moveMult * dt;

        var xfrac = Math.floor(camera.x) - camera.x;
        var yfrac = Math.floor(camera.y) - camera.y;
        for (var x = -1; x <= MAP_WIDTH/TILE_SIZE; x++) {
            for (var y = -1; y <= MAP_HEIGHT/TILE_SIZE; y++) {
                var r = new gamejs.Rect((x+xfrac)*TILE_SIZE, (y+yfrac)*TILE_SIZE,
                                        TILE_SIZE, TILE_SIZE);
                display.blit(grass, r);
            }
        }

        // Draw all creatures
        // TODO: Ignore off-screen creatures!
        for (var cid in creatures) {
            var c = creatures[cid];
            var rect = new gamejs.Rect((c.pos.x-camera.x)*TILE_SIZE,
                                       (c.pos.y-camera.y)*TILE_SIZE,
                                       TILE_SIZE, TILE_SIZE);
            gamejs.draw.rect(display, "rgb(255,0,0)", rect);                                       
        }
    });

    function drawDebug() {
        var gridX = Math.floor(camera.x / 128);
        var gridY = Math.floor(camera.y / 128);
        $('#debug').text(
                'world: (' + camera.x.toFixed(2) + "," + camera.y.toFixed(2) + ')' +
                ' chunk: (' + gridX + ',' + gridY + ')');
    }
});

// Support functions -----------------------------------------------------------

function writeConsole(text) {
    var elem = $('<li/>');
    elem.text(text);
    $('#console').append(elem);
}
