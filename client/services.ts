/// <reference path="../typings/jquery/jquery.d.ts" />
/// <reference path="../typings/underscore/underscore.d.ts" />
/// <reference path="./services.d.ts" />
import _ = require('underscore');
import I = require('services')
var map = _.map;
var each = _.each;

export class Provider implements I.Provider {

    constructor(private parent?: I.Provider) {}

    provide(name, service?) {
        if (typeof name != 'string') {
            service = name
            name = service.name;
        }
        if (!name)
            throw Error('Provided service is missing a name');
        this.services[name] = service;
        return this;
    }

    extend() {
        return new Provider(this);
    }

    get player() { return this.get('player') }
    get trackContextMenu() { return this.get('track-context-menu') }
    get dragTrack() { return this.get('drag-track') }

    get(name: string): any {
        var instance = this.instances[name]
        if (typeof instance == 'undefined')
            instance = this.resolve(name);
        return instance;
    }

    private resolve(name: string) {
        var service = this.services[name];
        if (typeof service == 'undefined') {
            if (this.parent)
                return this.parent.get(name);
            else
                throw Error('Service "' + name + '" not provided');
        }
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
export var globalProvider = new Provider();


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
    var service = new Service<T>(name, deps, init);
    if (service.name)
        globalProvider.provide(service);
    return service;
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
