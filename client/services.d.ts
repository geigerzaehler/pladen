declare module "services" {
    import Track = require('models/track');

    export interface DragTrack {
        (e: JQuery, getTrack: (id: number) => Track.Attributes);
    }

    export interface Player {
        play(track: Track.Attributes);
    }

    export interface TrackContextMenu {
        (el: JQuery, getTrack: (id: number) => Track.Attributes);
    }

    export var globalProvider: Provider;
    export interface Provider {
        player: Player;
        trackContextMenu: TrackContextMenu;
        dragTrack: DragTrack;

        get(name: "track-context-menu"): TrackContextMenu;
        get(name: "player"): Player;
        get(name: "drag-track"): DragTrack;
        get(name: string): any;
    }

    export function
    service<T>(init: (...any) => T): Service<T>;
    export function
    service<T>(deps: string[], init: (...any) => T): Service<T>;
    export function
    service<T>(name: string, init: (...any) => T): Service<T>;
    export function
    service<T>(name: string, deps: string[], init: (...any) => T): Service<T>;

    interface Service<T> {
        name?: string;
        deps: string[];
        init(...any): T;
    }
}
