import templates = require('templates');
import s = require('services');
import Track = require('models/track');

export = dragTrack;
var dragTrack: s.Service<s.DragTrack> = s.service(function() {
    var tpl = templates.dragTrack()
    var $tpl = $(tpl.el);
    $tpl.appendTo('.hidden-box');

    return function onTrackDrag($el: JQuery,
                                getTrack: (id: number) => Track.Attributes) {
        $el.on('dragstart.track', (e:any) => {
            var id = e.target.dataset.trackId
            if (!id)
                return;

            var dt = e.originalEvent.dataTransfer;
            var t = getTrack(parseInt(id))

            tpl.title(t.title);
            tpl.artist(t.artist);
            dt.dropEffect = 'none';
            dt.effectAllowed = 'copy';
            dt.setData('application/x-play-track', JSON.stringify(t));
            (<any>dt).setDragImage($tpl[0], 0, 0);

            $('html').addClass('drag-track');
        })
        .on('dragend.track', (e:any) => {
            if(e.target.dataset.trackId)
                $('html').removeClass('drag-track');
        });
    }
});
