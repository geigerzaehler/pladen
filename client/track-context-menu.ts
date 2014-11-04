import s = require('services');
import templates = require('templates');
import Track = require('models/track');

export = trackContextMenu;
var trackContextMenu = s.service<s.TrackContextMenu>(['player'], init)

function init(player: s.Player) {
    // TODO use service instead of getTrack
    return function addTrackContextMenu($target: JQuery, getTrack) {
        $target.on('click.track-context-menu', makeMenu);

        function makeMenu(e: JQueryMouseEventObject) {
            var target = $(e.target).closest('[data-track-id]')[0];
            if (!target) return;

            var id = target.dataset['trackId'];
            if (!id) return;

            e.preventDefault()
            var track = getTrack(parseInt(id));
            var rect = target.getBoundingClientRect()

            var $el = $(templates.trackContextMenu());
            $el.on('click', ['data-action=play'], function() {
                player.play(track);
            })
            $el.appendTo('body');
            $el.css({
                position: 'absolute',
                left: rect.left,
                top:  rect.bottom
            })

            setTimeout(function() {
                $(document).one('click', function() {
                    $el.remove();
                })
            }, 0);
        }
    }
}
