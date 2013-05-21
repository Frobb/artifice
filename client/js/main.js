var gamejs = require('gamejs');

var creature = require('./creature');
var food = require('./food');

// Message handlers and senders ------------------------------------------------

var handlers = {};

handlers["log"] = function() { /* suppress */ };
handlers["creature_add"] = handleCreatureAdd;
handlers["creature_move"] = handleCreatureMove;
handlers["creature_remove"] = handleCreatureRemove;
handlers["creature_die"] = handleCreatureDie;
handlers["food_add"] = handleFoodAdd;
handlers["food_remove"] = handleFoodRemove;

function handleCreatureAdd(payload) {
    writeConsole("debug", "Creature '" + payload.cid + "' added.");
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
    writeConsole("debug", "Creature '" + payload.cid + "' removed.");
    delete creatures[payload.cid];
}

function handleCreatureDie(payload) {
    writeConsole("debug", "Creature '" + payload.cid + "' died.");
}

function handleFoodAdd(payload) {
    var f = new food.Food(payload.pos, payload.type);
    foods.push(f);
    writeConsole('debug', 'Added food at ' + formatPos(payload.pos) + '.');
}

function handleFoodRemove(payload) {
    for (var i in foods) {
        if (foods[i].pos.x == payload.pos.x &&
            foods[i].pos.y == payload.pos.y) {
            writeConsole('debug', 'Removed food at ' + formatPos(payload.pos) + '.');
            delete foods[i];
        }
    }
}

function sendMove(x, y) {
    sendMsg('move', { 'x': x, 'y': y });
}

function sendCreatureAdd(x, y) {
    sendMsg('creature_add', {'x': x, 'y': y });
}

function sendFoodAdd(x, y) {
    sendMsg('food_add', {'x': x, 'y': y});
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

var socket = new WebSocket('ws://' + document.location.host + '/ws');

socket.onopen = function() {
    writeConsole("info", "Connected to server.");
}

socket.onclose = function() {
    writeConsole("info", "Disconected from server.");
}

socket.onerror = function() {
    writeConsole("error", "A WebSocket error occurred.");
}

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

var MAP_WIDTH = 1280;
var MAP_HEIGHT = 640;
var TILE_SIZE = 32; //px

var creatures = {};
var foods = []; // Name to avoid conflict with food module
var addMode = null;
var camera = { x: 0, y: 0 };
var moveState = { up: 0, left: 0, down: 0, right: 0 };
var moveSpeed = 5.0;
var moveMult = 1.0;

setAddMode('creature');

gamejs.preload(['../sprites/grass.png',
                '../sprites/food-default.png',
                '../sprites/food-carcass.png',
                '../sprites/creature.png',
               ]);

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
            // Switching between add modes (c=creature, f=food, ...)
            else if (event.key === gamejs.event.K_c &&
                     event.type === gamejs.event.KEY_UP)
                setAddMode('creature');
            else if (event.key === gamejs.event.K_f &&
                     event.type === gamejs.event.KEY_UP)
                setAddMode('food');
        } else if (event.type === gamejs.event.MOUSE_UP) {
            var cellX = Math.floor(camera.x + event.pos[0] / TILE_SIZE);
            var cellY = Math.floor(camera.y + event.pos[1] / TILE_SIZE);
            addObjectAt(cellX, cellY);
        }
    });

    var grassTile = gamejs.image.load('../sprites/grass.png');
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
                display.blit(grassTile, r);
            }
        }

        // Draw all food
        for (var i in foods) {
            var f = foods[i];
            if (!visible(camera, f.pos))
                continue;
            var rect = new gamejs.Rect((f.pos.x-camera.x)*TILE_SIZE,
                                       (f.pos.y-camera.y)*TILE_SIZE,
                                       TILE_SIZE, TILE_SIZE);
            f.draw(display, rect);
        }

        // Draw all creatures
        for (var cid in creatures) {
            var c = creatures[cid];
            if (!visible(camera, c.pos))
                continue;
            var rect = new gamejs.Rect((c.pos.x-camera.x)*TILE_SIZE,
                                       (c.pos.y-camera.y)*TILE_SIZE,
                                       TILE_SIZE, TILE_SIZE);
            c.draw(display, rect);
        }
    });

    function drawDebug() {
        var gridX = Math.floor(camera.x / 128);
        var gridY = Math.floor(camera.y / 128);
        $('#debug-world-x').text(camera.x.toFixed(2));
        $('#debug-world-y').text(camera.y.toFixed(2));
        $('#debug-chunk-x').text(gridX);
        $('#debug-chunk-y').text(gridY);
    }
});

// Support functions -----------------------------------------------------------

function visible(cameraPos, objPos) {
    return (objPos.x >= cameraPos.x &&
            objPos.y >= cameraPos.y &&
            (objPos.x < cameraPos.x + MAP_WIDTH) &&
            (objPos.y < cameraPos.y + MAP_HEIGHT));
}

function writeConsole(category, text) {
    var elem = $('<li class="' + category + '" />');
    elem.text('[' + category.toUpperCase() + '] ' + text);
    $('#console-lines').append(elem);
    var console = $('#console');
    console.scrollTop(console[0].scrollHeight - console.height());
}

function setAddMode(mode) {
    addMode = mode;
    $('#debug-add-mode').text(mode);
}

function addObjectAt(x, y) {
    if (addMode == 'creature') sendCreatureAdd(x, y);
    else if (addMode == 'food') sendFoodAdd(x, y);
    else {
        alert("Unknown add mode: " + addMode);
    }
}

function formatPos(pos) {
    return '(' + pos.x + ',' + pos.y + ')';
}
