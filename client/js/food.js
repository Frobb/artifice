var gamejs = require('gamejs');

exports.Food = function(pos, type) {
    this.pos = pos;
    this.type = type;

    this.draw = function(display, rect) {
        var sprite;
        if (this.type == 'carcass') {
            sprite = gamejs.image.load('../sprites/food-carcass.png');
        } else {
            sprite = gamejs.image.load('../sprites/food-default.png');
        }
        display.blit(sprite, rect);
    }
}
