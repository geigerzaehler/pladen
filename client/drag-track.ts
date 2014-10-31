import templates = require('templates');
import s = require('services');
import Track = require('models/track');

var dragTrack = s.service(function() {
    var $el = $(templates.dragTrack());
    $el.appendTo('.hidden-box');

    return function apply(t: Track.Attributes, dt: DataTransfer) {
        $el.find('.drag-release-name').text(t.title);
        $el.find('.drag-release-artist').text(t.artist);
        dt.dropEffect = 'none';
        dt.effectAllowed = 'copy';
        dt.setData('application/x-play-track', JSON.stringify(t));
        (<any>dt).setDragImage($el[0], 0, 0);
    }
});
export = dragTrack;
