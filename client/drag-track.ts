import templates = require('templates');
import s = require('services');
import Track = require('models/track');

export = dragTrack;
var dragTrack: s.Service<s.DragTrack> = s.service(function() {
    var tpl = templates.dragTrack()
    var $el = $(tpl.el);
    $el.appendTo('.hidden-box');

    return function apply(t: Track.Attributes, dt: DataTransfer) {
        tpl.title(t.title);
        tpl.artist(t.artist);
        dt.dropEffect = 'none';
        dt.effectAllowed = 'copy';
        dt.setData('application/x-play-track', JSON.stringify(t));
        (<any>dt).setDragImage($el[0], 0, 0);
    }
});
