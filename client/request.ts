/// <reference path="../typings/all.d.ts" />

import superagent = require('superagent');
import when = require('when');
import Promise = when.Promise;

export function create(method?: string, path?: string) {
    if (!path)
        return new Request(method);
    else
        return new Request(path, method);
}

export interface Response extends superagent.Response {};

export class Request {

    run(): Promise<Response> {
        return when.promise<Response>((resolve, reject) => {
            this.agent().end((err, res) => {
                if(err || !res.ok)
                    reject(err || res);
                else
                    resolve(res)
            })
        })
    }

    get(path?: string, query?: {}) {
        var req = this.clone();
        req._method = 'GET';
        if (query) req = req.query(query);
        if (path)  req = req.append(path);
        return req;
    }

    head(path?: string) {
        var req = this.clone();
        req._method = 'HEAD';
        if (path)  req = req.append(path);
        return req;
    }

    put(path?: string, data?: any) {
        var req = this.clone();
        req._method = 'PUT';
        if (data) req = req.data(data);
        if (path)  req = req.append(path);
        return req;
    }

    append(path: string) {
        var req = this.clone();
        req._path = req._path.replace(/\/*$/, '')
                  + path.replace(/^\/*/, '/');
        return req;
    }

    query(q: {}) {
        var req = this.clone()
        req._query.push(q);
        return req;
    }

    data(d: {}) {
        var req = this.clone()
        req._data.push(d);
        return req;
    }
    accept(type: string) {
        var req = this.clone();
        req._accept = type;
        return req;
    }

    constructor(
        private _path: string = '/',
        private _method: string = 'GET',
        private _query: {}[] = [],
        private _data: any[] = [],
        private _accept: string = undefined
    ) {}

    private agent(): superagent.Request {
        var agent = new superagent.Request(this._method, this._path);
        this._query.forEach((q) => {
            agent.query(q)
        })
        this._data.forEach((d) => {
            agent.send(this._data)
        })
        if (this._accept)
            agent.accept(this._accept)
        return agent;
    }

    private clone() {
        return new Request(
            this._path,
            this._method,
            this._query,
            this._data,
            this._accept
        )
    }
}
