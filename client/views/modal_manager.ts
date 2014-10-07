/// <reference path="../../typings/signals.d.ts" />
/// <reference path="../../typings/move.d.ts" />
import View = require('./base/view');
import DataTemplateView = require('./base/data_template');
import css = require('css_promise');
import Signal = require('signals');
import w = require('when');
import promise = w.promise;
import A = require('models/album');
import Album = A.Album;
import $ = require('jquery');

export = ModalManager;

class ModalManager extends View {

    get elementTemplate(): string
    { return '<div class="modal inactive">' }

    constructor(parent: JQuery) {
        super();
        parent.append(this.$el);
    }

    // TODO rename
    openNoAlbumDownload(album: Album) {
        this.open(new NoAlbumDownload(album, this));
    }

    openMessage(msg: string) {
        this.open(new MessageDialog(msg));
    }

    private open(d: ModalDialog) {
        var container = $('<dialog>')
        container.append(d.$el).appendTo(this.$el);

        container.addClass('run-popout');
        css.transitionShow(this.$el, '-inactive');

        d.close.add(() => {
            container.detach();
            this.close();
        });
    }

    private close() {
        css.transitionHide(this.$el, 'inactive');
    }
}

interface ModalDialog extends View {
    close: Signal<void>;
}

class NoAlbumDownload extends DataTemplateView {
    get template(): string
    { return "album-req-dialog" }

    get events(): {[index: string]: string}
    { return {
        'click button.dialog-close': 'dispatchClose',
        'click button.album-request': 'request'
    } }

    close = new Signal<void>();

    constructor(public album: Album, public mm: ModalManager) {
        super(album);
        this.render();
    }

    private dispatchClose() {
        this.close.dispatch();
    }

    private request() {
        var req = this.album.requestUpload();
        req.catch(() => {
            this.close.dispatch();
            this.app.openMessageDialog('Fehler. Konnte Album nicht vormerken.');
        })
        req.then(() => {
            this.close.dispatch();
            this.app.openMessageDialog('Album „' + this.album.name
                                      +'“ zum Download vorgemerkt.');
        })
    }
}

class MessageDialog extends DataTemplateView {
    get elementTemplate(): string
    { return '<div class=msg-dialog>' }

    get template(): string
    { return "msg-dialog" }

    get events(): {[index: string]: string}
    { return {
        'click button.dialog-close': 'dispatchClose',
    } }

    close = new Signal<void>();

    constructor(msg: string) {
        super({msg: msg});
        this.render();
    }

    private dispatchClose() {
        this.close.dispatch();
    }
}
