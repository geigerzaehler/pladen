import B = require('backbone');
import album = require('models/album');
import Album = album.Album;

export declare function album(search: string): (a: Album) => boolean;
export declare function track(search: string): (a: B.Model) => boolean;
