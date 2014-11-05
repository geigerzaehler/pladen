import s = require('services');
import templates = require('templates');
import Track = require('models/track');

export = trackContextMenu;
var trackContextMenu = s.service<s.TrackContextMenu>
    ('track-context-menu', ['player'], init);

function init(player: s.Player) {
    // TODO use service instead of getTrack
    var parent = document.body;
    return function addTrackContextMenu($target: JQuery, getTrack) {
        $target.on('click.track-context-menu', makeMenu);

        function makeMenu(e: JQueryMouseEventObject) {
            var target = $(e.target).closest('[data-track-id]')[0];
            if (!target) return;

            var id = target.dataset['trackId'];
            if (!id) return;

            e.preventDefault()

            var $el = $(templates.trackContextMenu());
            $el.appendTo(parent);

            var play = player.play.bind(player, getTrack(parseInt(id)));
            $el.on('click', '[data-action=play]', play);

            var targetRect = target.getBoundingClientRect();
            var parentRect = parent.getBoundingClientRect();
            $el.css({
                position: 'absolute',
                left: targetRect.left - parentRect.left,
                top:  targetRect.bottom - parentRect.top
            });

            setTimeout(function() {
                $(parent).one('click', function() {
                    $el.remove();
                })
            }, 0);
        }
    }
}
