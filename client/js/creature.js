var gamejs = require('gamejs');

exports.Creature = function(cid, pos) {
    this.cid = cid;
    this.pos = pos;

    this.draw = function(display, rect) {
        var sprite = gamejs.image.load('../sprites/creature.png');
        display.blit(sprite, rect);
    }
}
