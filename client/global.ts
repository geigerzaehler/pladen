/// <reference path="../typings/signals.d.ts" />
import A = require('models/album');
import Signal = require('signals');

export = Global;

class Global {

    static instance = new Global();

    openNoAlbumDownload: (album: A.Album) => void;
    openMessageDialog: (msg: string) => void;

    search = new Signal<{pattern: string; download: boolean}>();
}
