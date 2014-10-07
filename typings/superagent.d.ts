declare module "superagent" {
    export function get(url: string): Request;
    export function head(url: string): Request;
    export function del(url: string): Request;
    export function patch(url: string): Request;
    export function post(url: string): Request;
    export function put(url: string): Request;

    export class Request {
        constructor(method: string, url: string);
        accept(type: string): Request;
        query(q: any): Request;
        send(q: any): Request;
        end(fn: (res: Response) => any): Request;
        end(fn: (err: any, res: Response) => any): Request;
    }

    interface Response {
        text: string;
        body: any;
        status: number;
        statusType: number;
        ok: boolean;
    }
}
