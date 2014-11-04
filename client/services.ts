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

export interface TrackContextMenu {
    (el: JQuery, getTrack: (id: number) => Track.Attributes);
}


export class Provider {

    provide(name: string, s: Service<any>);
    provide(s: Service<any>);
    provide(name, service?) {
        if (typeof name != 'string') {
            service = name
            name = service.name;
        }
        if (!name)
            throw Error('Provided service is missing a name');
        this.services[name] = service;
    }

    get(name: "track-context-menu"): TrackContextMenu;
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
        var deps = map(service.deps, (d) => this.get(d));
        var instance = this.instances[name] = service.init(deps);
        this.resolving.pop();
        return instance;
    }

    private services: {[name: string]: Service<any>} = {};
    private instances: {[name: string]: any} = {};
    private resolving: string[] = [];
}


export function service<T>(name: string, init: (...any) => T): Service<T>;
export function service<T>(deps: string[], init: (...any) => T): Service<T>;
export function service<T>(name: string, deps: string[], init: (...any) => T): Service<T>;
export function service<T>(...args) {
    var name;
    var init = args.pop();
    if (typeof init != 'function')
        throw Error('Service must be created with an initializer function');
    var deps = args.pop();
    if (Array.isArray(deps)) {
        name = args.pop();
    } else if (typeof deps == 'string') {
        name = deps;
        deps = [];
    }
    return new Service<T>(name, deps, init)
}


export class Service<T> {
    constructor(name: string, deps: string[], init: (...any) => T) {
        this.name = name;
        this.deps = deps;
        this._init = init;
    }

    name: string;
    deps: string[];

    init(args: any[]): T {
        return this._init.apply(undefined, args);
    }

    private _init: (...any) => T;
}
