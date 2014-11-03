/// <reference path="../typings/jquery/jquery.d.ts" />
/// <reference path="../typings/underscore/underscore.d.ts" />
import _ = require('underscore');
var map = _.map;

import Track = require('models/track');

export interface DragTrack {
    (e: JQuery, getTrack: (id: number) => Track.Attributes);
}

export interface Player {
    play(track: Track.Attributes);
}

export class Provider {

    provide(name: "player", s: Service<Player>);
    provide(name: "drag-track", s: Service<DragTrack>);
    provide(name: string, s: Service<any>);
    provide(name: string, s: Service<any>) {
        this.services[name] = s;
    }

    get(name: "player"): Player;
    get(name: "drag-track"): DragTrack;
    get(name: string): any;
    get(name: string): any {
        var instance = this.instances[name]
        if (typeof instance == 'undefined')
            instance = this.resolve(name);
        return instance;
    }

    private resolve(name: string) {
        var service = this.services[name]
        if (typeof service == 'undefined')
            throw Error('Service "' + name + '" not provided');
        if (this.resolving.indexOf(name) >= 0)
            throw Error('Circular dependency in requiring ' + name);

        this.resolving.push(name);
        var deps = map(service.deps, (d) => this.get(<any>d));
        var instance = this.instances[name] = service.init(deps);
        this.resolving.pop();
        return instance;
    }

    private services: {[name: string]: Service<any>} = {};
    private instances: {[name: string]: any} = {};
    private resolving: string[] = [];
}


export function service<T>(init: (...any) => T)
export function service<T>(deps: string[], init: (...any) => T)
export function service<T>(deps, init?) {
    if (typeof init == 'undefined') {
        init = deps;
        deps = [];
    }
    return new Service<T>(deps, init)
}


export class Service<T> {
    constructor(deps: string[], init: (...any) => T) {
        this.deps = deps;
        this._init = init;
    }

    deps: string[];

    init(args: any[]): T {
        return this._init.apply(undefined, args);
    }

    private _init: (...any) => T;
}
